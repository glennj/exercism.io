       IDENTIFICATION DIVISION.
       PROGRAM-ID. QUEEN-ATTACK.
       AUTHOR. glennj.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-QUEEN          PIC X(9).
       01 WS-WHITE_QUEEN    PIC X(9).
       01 WS-BLACK_QUEEN    PIC X(9).
       01 WS-PROPERTY       PIC X(11).
       01 WS-RESULT         PIC 9.

       01 return-failure    PIC 9 VALUE ZERO.
       01 return-success    PIC 9 VALUE 1.

       01 x-pos             PIC S9.
           88 x-valid       VALUE 0 THRU 7.
       01 y-pos             PIC S9.
           88 y-valid       VALUE 0 THRU 7.

       01 x-black           PIC 9.
       01 y-black           PIC 9.
       01 dx                PIC 9.
       01 dy                PIC 9.

       PROCEDURE DIVISION.
       QUEEN-ATTACK.
           EVALUATE ws-property
               WHEN "create"    PERFORM queen-create
               WHEN "canAttack" PERFORM can-attack
           END-EVALUATE.

       QUEEN-CREATE.
           UNSTRING ws-queen DELIMITED BY "," INTO x-pos, y-pos
           IF x-valid AND y-valid THEN
               MOVE return-success TO ws-result
           ELSE
               MOVE return-failure TO ws-result
           END-IF.

       CAN-ATTACK.
           MOVE ws-black_queen TO ws-queen
           PERFORM queen-create
           IF ws-result = return-failure THEN
               GOBACK
           END-IF
           MOVE x-pos TO x-black
           MOVE y-pos TO y-black

           MOVE ws-white_queen TO ws-queen
           PERFORM queen-create
           IF ws-result = return-failure THEN
               GOBACK
           END-IF

           COMPUTE dx = FUNCTION ABS(x-black - x-pos)
           COMPUTE dy = FUNCTION ABS(y-black - y-pos)

           IF dx = 0 OR dy = 0 OR dx = dy THEN
               MOVE return-success TO ws-result
           ELSE
               MOVE return-failure TO ws-result
           END-IF.
