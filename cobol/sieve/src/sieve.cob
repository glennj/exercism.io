       IDENTIFICATION DIVISION.
       PROGRAM-ID. SIEVE.
       AUTHOR. glennj.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-LIMIT  PIC 9999.
       01 WS-RESULT PIC 999 OCCURS 1000 TIMES. 
       01 WS-COUNT  PIC 9999.

       01 num       PIC 9(4).
       01 markers   PIC 9 OCCURS 1000 TIMES.

       01 idx       PIC 9(6).
       01 start-num PIC 9(6).
       01 increment PIC 9(4).

       PROCEDURE DIVISION.
       SIEVE.
           PERFORM sieve-initialize
           PERFORM mark-non-primes
           PERFORM extract-primes.

       SIEVE-INITIALIZE.
           PERFORM VARYING num FROM 1 BY 1
                               UNTIL num GREATER THAN ws-limit
               MOVE 1 TO markers(num)
               MOVE ZERO TO ws-result(num)
           END-PERFORM
           MOVE ZERO TO markers(1).

       MARK-NON-PRIMES.
      *    First step is to mark all even numbers > 2,
      *    then consider the odd numbers.
           MOVE 2 TO num
           PERFORM mark-multiples
           PERFORM VARYING num FROM 3 BY 2
                               UNTIL num GREATER THAN FUNCTION SQRT(ws-limit)
               IF markers(num) EQUAL TO 1 THEN
                   PERFORM mark-multiples
               END-IF
           END-PERFORM.

       EXTRACT-PRIMES.
           MOVE ZERO TO idx
           PERFORM VARYING num FROM 2 BY 1
                               UNTIL num GREATER THAN ws-limit
               IF markers(num) EQUAL TO 1 THEN
                   ADD 1 TO idx
                   MOVE num TO ws-result(idx)
               END-IF
           END-PERFORM.

       MARK-MULTIPLES.
           IF num EQUAL TO 2 THEN
               COMPUTE increment = num
           ELSE
               COMPUTE increment = num * 2
           END-IF
           COMPUTE start-num = num ** 2

           PERFORM VARYING idx FROM start-num BY increment
                               UNTIL idx GREATER THAN ws-limit
                MOVE 0 TO markers(idx)
           END-PERFORM.
