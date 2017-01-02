BEGIN;

WITH _ AS (
    INSERT INTO files (id, name, author_id, root_id)
    VALUES
        ('ab77b629-06b1-4c2e-a1e2-11ec36d778e8', 'A', 'https://api.stormpath.com/v1/accounts/1SGPcogTvU1qpx3k3iZl8Q', 'd7b77961-4b36-4d76-b1b0-db4851c9fdae'),
        ('86fb5f54-0fc3-4aee-b7ba-df4844d12f18', 'B', 'https://api.stormpath.com/v1/accounts/1SGPcogTvU1qpx3k3iZl8Q', '3a4c0d0f-28e3-42d3-a2de-430bdf569839')
)
INSERT INTO vertices (id, note, style, file_id)
VALUES
    ('d7b77961-4b36-4d76-b1b0-db4851c9fdae', 'A', 'normal', 'ab77b629-06b1-4c2e-a1e2-11ec36d778e8'),
    ('3a4c0d0f-28e3-42d3-a2de-430bdf569839', 'B', 'normal', '86fb5f54-0fc3-4aee-b7ba-df4844d12f18');

COMMIT;
