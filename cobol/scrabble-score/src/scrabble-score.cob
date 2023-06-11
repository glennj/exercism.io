       IDENTIFICATION DIVISION.
       PROGRAM-ID. SCRABBLE-SCORE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-WORD   PIC X(60).
       01 WS-RESULT PIC 99.

       01 Letter    PIC A.
           88 isOne     VALUE "A", "E", "I", "O", "U",
                              "L", "N", "R", "S", "T".
           88 isTwo     VALUE "D", "G".
           88 isThree   VALUE "B", "C", "M", "P".
           88 isFour    VALUE "F", "H", "V", "W", "Y".
           88 isFive    VALUE "K".
           88 isEight   VALUE "J", "X".
           88 isTen     VALUE "Q", "Z".

       01 idx PIC 99.

       PROCEDURE DIVISION.
       SCRABBLE-SCORE.
           MOVE ZERO TO ws-result
           MOVE FUNCTION UPPER-CASE(ws-word) TO ws-word
           PERFORM VARYING idx FROM 1 BY 1
                               UNTIL idx > FUNCTION LENGTH(ws-word)
               MOVE ws-word(idx:1) TO Letter
               EVALUATE TRUE
                   WHEN isOne    ADD  1 TO ws-result
                   WHEN isTwo    ADD  2 TO ws-result
                   WHEN isThree  ADD  3 TO ws-result
                   WHEN isFour   ADD  4 TO ws-result
                   WHEN isFive   ADD  5 TO ws-result
                   WHEN isEight  ADD  8 TO ws-result
                   WHEN isTen    ADD 10 TO ws-result
               END-EVALUATE
           END-PERFORM.

