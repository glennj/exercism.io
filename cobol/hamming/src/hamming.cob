       IDENTIFICATION DIVISION.
       PROGRAM-ID. hamming.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-DNA-1 PIC X(32).
       01 WS-DNA-2 PIC X(32).
       01 WS-HAMMING PIC 9(2).
       01 WS-ERROR PIC X(31).
         88 errmsg VALUE "Strands must be of equal length".

       01 len1 PIC 99.
       01 len2 PIC 99.
       01 idx PIC 99.

       PROCEDURE DIVISION.
       HAMMING.
           MOVE 0 TO ws-hamming
           MOVE SPACES TO ws-error
           MOVE FUNCTION LENGTH(FUNCTION TRIM(ws-dna-1)) TO len1
           MOVE FUNCTION LENGTH(FUNCTION TRIM(ws-dna-2)) TO len2

           IF len1 IS NOT EQUAL TO len2 THEN
               SET errmsg TO TRUE
           ELSE
               PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > len1
                   IF ws-dna-1(idx:1) IS NOT EQUAL TO ws-dns-2(idx:1)
                       COMPUTE ws-hamming = ws-hamming PLUS 1
                   END-IF
               END-PERFORM
           END-IF.
