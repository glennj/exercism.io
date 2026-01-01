WITH
    white(space) AS (
        -- tab, newline, vertical tab, form feed, carriage return, space
        VALUES (CHAR(9) || CHAR(10) || CHAR(11) || CHAR(12) || CHAR(13) || CHAR(32))
    )
    , input AS (
        SELECT input, RTRIM(input, white.space) AS trimmed
        FROM bob
        JOIN white
    )
    , properties AS (
        SELECT input
             , LENGTH(trimmed) == 0 AS is_silent
             , GLOB('*[A-Z]*', input) AND NOT GLOB('*[a-z]*', input) AS is_yelling
             , GLOB('*[?]', trimmed) AS is_asking
        FROM input
    )
UPDATE bob
SET reply =
    CASE
    WHEN is_yelling AND is_asking THEN 'Calm down, I know what I''m doing!'
    WHEN is_yelling               THEN 'Whoa, chill out!'
    WHEN is_asking                THEN 'Sure.'
    WHEN is_silent                THEN 'Fine. Be that way!'
    ELSE                               'Whatever.'
    END
FROM properties
WHERE bob.input = properties.input;
