       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEAP.
       AUTHOR. glennj.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ws-year       PIC 9(4) VALUE ZERO.
       01 ws-result     PIC 9.
       01 quo           PIC 9(4).
       01 rem           PIC 9(3).

       PROCEDURE DIVISION.
       leap.
           MOVE 0 TO ws-result

           DIVIDE ws-year BY 4 GIVING quo REMAINDER rem
           IF rem = ZERO THEN
               DIVIDE ws-year BY 100 GIVING quo REMAINDER rem
               IF rem NOT = ZERO THEN
                   MOVE 1 TO ws-result
               ELSE
                   DIVIDE ws-year BY 400 GIVING quo REMAINDER rem
                   IF rem = ZERO THEN
                       MOVE 1 TO ws-result
                   END-IF
               END-IF
           END-IF.
