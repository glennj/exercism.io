DROP TABLE IF EXISTS "rna-transcription";

CREATE TABLE "rna-transcription" ("dna" TEXT, "result" TEXT);

.mode csv
.import ./data.csv rna-transcription
