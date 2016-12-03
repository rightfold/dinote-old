module NN.Server.Vertex.DB
( readVertex
, createVertex
, createEdge
) where

import Database.PostgreSQL (Connection, execute, POSTGRESQL, query)
import Data.UUID (GENUUID)
import Data.UUID as UUID
import NN.File (FileID(..))
import NN.Prelude
import NN.Vertex (Vertex(..), VertexID(..))
import NN.Vertex.Style (Style(..))

readVertex
    :: ∀ eff
     . Connection
    -> VertexID
    -> Aff (postgreSQL :: POSTGRESQL | eff) (Maybe Vertex)
readVertex conn (VertexID vertexID) = do
    query conn """
        SELECT
            v.note,
            CASE WHEN count(e.*) = 0 THEN
                ARRAY[] :: uuid[]
            ELSE
                array_agg(e.child_id ORDER BY e.index ASC)
            END,
            v.style
        FROM vertices AS v
        LEFT JOIN edges AS e
            ON e.parent_id = v.id
        WHERE v.id = $1
        GROUP BY v.id
    """ (vertexID /\ unit)
    <#> case _ of
        [note /\ children /\ style /\ (_ :: Unit)] ->
            Just $ Vertex note (map VertexID children) (parseStyle style)
        _ -> Nothing
    where
    parseStyle "normal              " = Normal
    parseStyle "dimmed              " = Dimmed
    parseStyle "grass               " = Grass
    parseStyle "ocean               " = Ocean
    parseStyle "peachpuff           " = Peachpuff
    parseStyle "hotdog_stand        " = HotdogStand
    parseStyle _ = Normal

createVertex
    :: ∀ eff
     . Connection
    -> FileID
    -> Aff (uuid :: GENUUID, postgreSQL :: POSTGRESQL | eff) VertexID
createVertex conn (FileID fileID) = do
    vertexIDStr <- liftEff $ show <$> UUID.genUUID
    execute conn """
        INSERT INTO vertices (id, note, style, file_id)
        VALUES ($1, '', 'normal', $2)
    """ (vertexIDStr /\ fileID /\ unit)
    pure $ VertexID vertexIDStr

createEdge
    :: ∀ eff
     . Connection
    -> {parentID :: VertexID, childID :: VertexID}
    -> Aff (postgreSQL :: POSTGRESQL | eff) Unit
createEdge conn {parentID: VertexID parentID, childID: VertexID childID} =
    execute conn """
        INSERT INTO edges (parent_id, child_id, index)
        SELECT $1, $2, coalesce(max(index) + 1, 0)
        FROM edges
        WHERE parent_id = $1
    """ (parentID /\ childID /\ unit)
