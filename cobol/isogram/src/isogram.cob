       IDENTIFICATION DIVISION.
       PROGRAM-ID. ISOGRAM.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-Phrase     PIC X(60).
       01 WS-Result     PIC 99.
       01 idx           PIC 99.
       01 Letter-Count  PIC 99.

       01 Letters       VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
           02 Letter    PIC A OCCURS 26 TIMES.

       PROCEDURE DIVISION.
       ISOGRAM.
           MOVE 1 TO WS-Result
           MOVE FUNCTION UPPER-CASE(WS-Phrase) TO WS-Phrase

           PERFORM VARYING idx FROM 1 BY 1
                               UNTIL idx > FUNCTION LENGTH(Letters)
                                   OR WS-Result EQUAL TO ZERO
               MOVE ZEROS TO Letter-Count
               INSPECT WS-Phrase TALLYING Letter-Count FOR ALL Letter(idx)
               IF Letter-Count > 1
                   MOVE ZERO to WS-Result
               END-IF
           END-PERFORM.
