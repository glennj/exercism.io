WITH RECURSIVE
    seq(r, i, stop) AS (
        SELECT rowid, start_bottles, start_bottles - take_down + 1 FROM "bottle-song"
        UNION
        SELECT r, i - 1, stop FROM seq WHERE i > stop
    )
    , ordinal(i, word, plural) AS (
        VALUES
            ( 0, 'No',    's')
          , ( 1, 'One',   '')
          , ( 2, 'Two',   's')
          , ( 3, 'Three', 's')
          , ( 4, 'Four',  's')
          , ( 5, 'Five',  's')
          , ( 6, 'Six',   's')
          , ( 7, 'Seven', 's')
          , ( 8, 'Eight', 's')
          , ( 9, 'Nine',  's')
          , (10, 'Ten',   's')
    )
    , verses AS (
        SELECT seq.r
             , o.word || ' green bottle' || o.plural || ' hanging on the wall,' || CHAR(10) ||
               o.word || ' green bottle' || o.plural || ' hanging on the wall,' || CHAR(10) ||
               'And if one green bottle should accidentally fall,' || CHAR(10) ||
               'There''ll be ' || LOWER(next.word) || ' green bottle' || next.plural || ' hanging on the wall.'
               AS verse
        FROM seq
        INNER JOIN ordinal AS o USING (i)
        INNER JOIN ordinal AS next ON next.i = o.i - 1
        ORDER BY seq.i DESC
    )
    , songs AS (
        SELECT r, GROUP_CONCAT(verse, CHAR(10) || CHAR(10)) AS song
        FROM verses
        GROUP BY r
    )
UPDATE "bottle-song"
SET result = song
FROM songs
WHERE "bottle-song".rowid = songs.r;
