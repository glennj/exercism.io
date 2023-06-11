       IDENTIFICATION DIVISION.
       PROGRAM-ID. rna-transcription.
       AUTHOR. glennj.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COMPLEMENT   PIC X(64).
       01 dna-nucleotides PIC X(4) VALUE "ACGT".
       01 rna-nucleotides PIC X(4) VALUE "UGCA".

       PROCEDURE DIVISION.
       RNA-TRANSCRIPTION.
           INSPECT ws-complement CONVERTING dna-nucleotides
                                         TO rna-nucleotides.
