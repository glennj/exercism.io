-- Schema: CREATE TABLE "rna-transcription" ("dna" TEXT, "result" TEXT);

UPDATE "rna-transcription"
SET result = UPPER(REPLACE(REPLACE(REPLACE(REPLACE(dna, 'G', 'c'), 'C', 'g'), 'T', 'a'), 'A', 'u'));
