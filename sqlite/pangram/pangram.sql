-- Schema: CREATE TABLE pangram (sentence TEXT NOT NULL, result BOOLEAN);

WITH RECURSIVE
    letters(sentence, i, ch) AS (
        SELECT sentence, 1, IIF(substr(sentence, 1, 1) GLOB '[a-zA-Z]', LOWER(SUBSTR(sentence, 1, 1)), NULL) AS ch
        FROM pangram

        UNION ALL

        SELECT sentence, i + 1, IIF(substr(sentence, i + 1, 1) GLOB '[a-zA-Z]', LOWER(SUBSTR(sentence, i + 1, 1)), NULL) AS ch
        FROM letters
        WHERE i <= LENGTH(sentence)
    )
  , counts AS (
        SELECT sentence, COUNT(DISTINCT ch) AS n
        FROM letters
        GROUP BY sentence
    )
UPDATE pangram
SET result = (counts.n = 26)
FROM counts
WHERE pangram.sentence = counts.sentence;
