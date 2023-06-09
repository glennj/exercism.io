       IDENTIFICATION DIVISION.
       PROGRAM-ID. two-fer.
       AUTHOR. glennj.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME   PIC X(18) VALUE "you".
       01 WS-RESULT PIC X(64).
       
       PROCEDURE DIVISION.
       TWO-FER.
      * -- woe betide the person with a space in their name...
           STRING
             "One for " DELIMITED BY SIZE
             WS-NAME    DELIMITED BY SPACE
             ", one for me."
           INTO WS-RESULT
           END-STRING.
