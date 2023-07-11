       IDENTIFICATION DIVISION.
       PROGRAM-ID. BOB.
       AUTHOR glennj.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-HEYBOB        PIC X(60).
       01 WS-RESULT        PIC X(40).
          88 result-silence  VALUE "Fine. Be that way!".
          88 result-yell-ask VALUE "Calm down, I know what I'm doing!".
          88 result-yelling  VALUE "Whoa, chill out!".
          88 result-asking   VALUE "Sure.".
          88 result-default  VALUE "Whatever.".

       01 len              PIC 99.
       01 idx              PIC 99.
       01 is-yelling       PIC 9.
       01 is-asking        PIC 9.
       01 is-silence       PIC 9.
       01 chr              PIC X.
          88 is-letter     VALUE "A" THROUGH "Z", "a" THROUGH "z".
          88 is-question   VALUE "?".

       PROCEDURE DIVISION.
       BOB.
           COMPUTE len = FUNCTION LENGTH(FUNCTION TRIM(WS-HEYBOB))

           PERFORM test-silence
           PERFORM test-yelling
           PERFORM test-asking

           IF      is-silence = 1 THEN SET result-silence  TO TRUE
           ELSE IF is-yelling = 1
               AND is-asking  = 1 THEN SET result-yell-ask TO TRUE
           ELSE IF is-yelling = 1 THEN SET result-yelling  TO TRUE
           ELSE IF is-asking  = 1 THEN SET result-asking   TO TRUE
           ELSE                        SET result-default  TO TRUE
           END-IF.

       test-silence.
           IF WS-HEYBOB = SPACES THEN
               MOVE 1 TO is-silence
           ELSE
               MOVE 0 TO is-silence
           END-IF.

       test-yelling.
           MOVE 0 TO is-yelling
           IF WS-HEYBOB = FUNCTION UPPER-CASE(WS-HEYBOB)
               PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > len
                   MOVE WS-HEYBOB(idx:1) TO chr
                   IF is-letter THEN
                       MOVE 1 TO is-yelling
                       EXIT PERFORM
                   END-IF
               END-PERFORM
           END-IF.

       test-asking.
           IF WS-HEYBOB(len:1) = "?" THEN
               MOVE 1 TO is-asking
           ELSE
               MOVE 0 TO is-asking
           END-IF.
