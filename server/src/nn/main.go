package main

import (
	"database/sql"
	"net/http"
	"os"

	_ "github.com/lib/pq"
	"github.com/Sirupsen/logrus"
	"github.com/streadway/amqp"

	"nn/api"
)

func main() {
	amqpURL := os.Getenv("NN_AMQP_URL")
	dbDataSourceName := os.Getenv("NN_DB_DATA_SOURCE_NAME")
	httpAddr := os.Getenv("NN_HTTP_ADDR")
	logrus.
		WithField("amqp_url", amqpURL).
		WithField("db_data_source_name", dbDataSourceName).
		WithField("http_addr", httpAddr).
		Info("boot")

	amqpConn, err := amqp.Dial(amqpURL)
	if err != nil {
		logrus.WithField("err", err).Fatal("amqp_dial")
	}
	defer amqpConn.Close()

	db, err := sql.Open("postgres", dbDataSourceName)
	if err != nil {
		logrus.WithField("err", err).Fatal("db_open")
	}
	defer db.Close()
	if err := db.Ping(); err != nil {
		logrus.WithField("err", err).Fatal("db_ping")
	}
	if err := prepareDB(db); err != nil {
		logrus.WithField("err", err).Fatal("prepare_db")
	}

	httpServer := &http.Server{
		Addr: httpAddr,
		Handler: api.NewHandler(amqpConn, db),
	}
	err = httpServer.ListenAndServe()
	logrus.WithField("err", err).Fatal("http_serve")
}

func prepareDB(db *sql.DB) error {
	_, err := db.Exec(`
		CREATE TABLE IF NOT EXISTS vertices (
			id   uuid NOT NULL,
			note text NOT NULL,
			PRIMARY KEY (id)
		);

		CREATE TABLE IF NOT EXISTS edges (
			parent_id uuid NOT NULL,
			child_id  uuid NOT NULL,
			PRIMARY KEY (parent_id, child_id),
			FOREIGN KEY (parent_id)
				REFERENCES vertices(id)
				ON DELETE CASCADE,
			FOREIGN KEY (child_id)
				REFERENCES vertices(id)
				ON DELETE CASCADE
		);
	`)
	return err
}
