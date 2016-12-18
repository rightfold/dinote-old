BEGIN;

WITH RECURSIVE reachable(id) AS (
    SELECT root_id
    FROM files

    UNION

    SELECT edges.child_id
    FROM edges
    JOIN reachable
        ON reachable.id = edges.parent_id
)
DELETE FROM vertices
WHERE id NOT IN (TABLE reachable);

COMMIT;
