-- Schema: CREATE TABLE "gigasecond" ("moment" TEXT, "result" TEXT);

UPDATE gigasecond
-- -- apparently the old 3.41.x version used by the test runner doesn't like %T directive.
-- SET result = strftime('%FT%T', moment, '1e9 seconds');

SET result = strftime('%Y-%m-%dT%H:%M:%S', moment, '1e9 seconds');

-- or, with more datetime functionality
-- SET result = strftime('%FT%T', 1e9 + unixepoch(moment), 'unixepoch');

select * from gigasecond;
