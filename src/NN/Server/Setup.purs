module NN.Server.Setup
( setupDB
) where

import Database.PostgreSQL (Connection, execute, POSTGRESQL, Query(..))
import NN.Prelude

setupDB :: ∀ eff. Connection -> Aff (postgreSQL :: POSTGRESQL | eff) Unit
setupDB conn = do
    execute conn (Query """
        CREATE EXTENSION IF NOT EXISTS pgcrypto
    """) unit

    execute conn (Query """
        CREATE TABLE IF NOT EXISTS sessions (
            id              uuid        NOT NULL,
            user_id         text        NOT NULL,
            description     text        NOT NULL,
            PRIMARY KEY (id)
        );
    """) unit

    execute conn (Query """
        CREATE INDEX IF NOT EXISTS sessions__user_id
            ON sessions
            (user_id)
    """) unit

    execute conn (Query """
        CREATE TABLE IF NOT EXISTS files (
            id              uuid        NOT NULL,
            name            text        NOT NULL,
            author_id       text        NOT NULL,
            root_id         uuid        NOT NULL,
            PRIMARY KEY (id)
        );
    """) unit

    execute conn (Query """
        CREATE INDEX IF NOT EXISTS files__author_id
            ON files
            (author_id)
    """) unit

    execute conn (Query """
        CREATE INDEX IF NOT EXISTS files__name
            ON files
            (name)
    """) unit

    execute conn (Query """
        CREATE TABLE IF NOT EXISTS vertices (
            id          uuid        NOT NULL,
            note        text        NOT NULL,
            style       "char"      NOT NULL,
            file_id     uuid        NOT NULL,
            PRIMARY KEY (id),
            FOREIGN KEY (file_id)
                REFERENCES files(id)
                ON DELETE CASCADE
        )
    """) unit

    execute conn (Query """
        ALTER TABLE files
        DROP CONSTRAINT IF EXISTS files_root_id_fkey,
        ADD CONSTRAINT files_root_id_fkey
            FOREIGN KEY (root_id)
            REFERENCES vertices(id)
            ON DELETE CASCADE
    """) unit

    execute conn (Query """
        CREATE TABLE IF NOT EXISTS edges (
            parent_id   uuid        NOT NULL,
            child_id    uuid        NOT NULL,
            index       int         NOT NULL,
            PRIMARY KEY (parent_id, index),
            FOREIGN KEY (parent_id)
                REFERENCES vertices(id)
                ON DELETE CASCADE,
            FOREIGN KEY (child_id)
                REFERENCES vertices(id)
                ON DELETE CASCADE
        )
    """) unit

    execute conn (Query """
        CREATE INDEX IF NOT EXISTS edges__child_id
            ON edges
            (child_id)
    """) unit

    execute conn (Query """
        CREATE SEQUENCE IF NOT EXISTS edge_index
    """) unit
