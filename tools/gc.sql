/**
 * This program deletes vertices that can no longer be accessed, because they
 * are not (indirectly) referenced by a root vertex. The implementation is a
 * simple mark-and-sweep algorithm.
 *
 * This program should be run periodically.
 */

BEGIN TRANSACTION;

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

COMMIT TRANSACTION;
