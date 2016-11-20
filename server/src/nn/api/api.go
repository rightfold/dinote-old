package api

import (
	"database/sql"
	"encoding/json"
	"net/http"

	"github.com/lib/pq"
	"github.com/Sirupsen/logrus"
	"github.com/streadway/amqp"
)

func NewHandler(amqpConn *amqp.Connection, db *sql.DB) *Handler {
	return &Handler{amqpConn: amqpConn, db: db}
}

type Handler struct {
	amqpConn *amqp.Connection
	db *sql.DB
}

func (h *Handler) ServeHTTP(res http.ResponseWriter, req *http.Request) {
	handlers := map[string]func(http.ResponseWriter, *http.Request) (interface{}, error) {
		"/api/v1/vertices": h.serveVertices,
	}
	switch req.URL.Path {
	// FIXME: This is a UI, not an API. Move it somewhere else.
	case "/", "/output/nn.js", "/output/nn.css":
		http.ServeFile(res, req, "../client" + req.URL.Path)
	default:
		handler, ok := handlers[req.URL.Path]
		if !ok {
			http.Error(res, "Not Found", 404)
			return
		}
		jres, err := handler(res, req)
		if err != nil {
			logrus.WithField("err", err).Error("api")
			http.Error(res, "Internal Server Error", 500)
			return
		}
		json.NewEncoder(res).Encode(jres)
	}
}

func (h *Handler) serveVertices(res http.ResponseWriter, req *http.Request) (interface{}, error) {
	var (
		id, note string
		children pq.StringArray
	)
	id = req.URL.Query().Get("vertexID")
	err := h.db.QueryRow(`
		SELECT vertices.note, array_remove(array_agg(edges.child_id), NULL)
		FROM vertices
		LEFT JOIN edges
			ON edges.parent_id = vertices.id
		WHERE vertices.id = $1
		GROUP BY vertices.id
	`, id).Scan(&note, &children)
	if err != nil {
		return nil, err
	}
	if children == nil {
		children = make(pq.StringArray, 0)
	}
	return map[string]interface{}{
		"note": note,
		"children": children,
	}, nil
}
