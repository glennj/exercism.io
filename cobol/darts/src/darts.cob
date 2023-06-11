       IDENTIFICATION DIVISION.
       PROGRAM-ID. DARTS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-X      PIC 99V9.
       01 WS-Y      PIC 99V9.
       01 WS-RESULT PIC 99.

       01 inner-circle-squared  PIC 999 VALUE 1.
       01 mid-circle-squared    PIC 999 VALUE 25.
       01 outer-circle-squared  PIC 999 VALUE 100.

       01 inner-value           PIC 99 VALUE 10.
       01 mid-value             PIC 99 VALUE 5.
       01 outer-value           PIC 99 VALUE 1.

       01 dist2                 PIC 999V9.

       PROCEDURE DIVISION.
       DARTS.
           MOVE ZERO TO ws-result
           EVALUATE ws-x ** 2 + ws-y ** 2
               WHEN 0 THRU inner-circle-squared
                   MOVE inner-value TO ws-result
               WHEN inner-circle-squared THRU mid-circle-squared
                   MOVE mid-value TO ws-result
               WHEN mid-circle-squared THRU outer-circle-squared
                   MOVE outer-value TO ws-result
           END-EVALUATE.
