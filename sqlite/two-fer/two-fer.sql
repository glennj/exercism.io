-- Schema: CREATE TABLE "twofer" ("input" TEXT, "response" TEXT);

UPDATE twofer
SET response = 'One for ' || iif(input IS NULL OR length(input) = 0, 'you', input) || ', one for me.'
;
