       IDENTIFICATION DIVISION.
       PROGRAM-ID. PANGRAM.
       AUTHOR. glennj.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       REPOSITORY.
           FUNCTION ALL INTRINSIC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SENTENCE   PIC X(60).
       01 WS-RESULT     PIC 9.

       01 Letters.
           02 Letter    PIC X OCCURS 26 TIMES.
       01 chr           PIC X.
           88 isLetter  VALUE "A" THRU "Z".

       01 ord-a         PIC 99.
       01 idx           PIC 99.
       01 letter-idx    PIC 99.
       01 count-it      PIC 99.

       PROCEDURE DIVISION.
       PANGRAM.
           MOVE ORD("A") TO ord-a.
           MOVE UPPER-CASE(ws-sentence) TO ws-sentence

           PERFORM pangram-iterate-over-alphabet.
      *    PERFORM pangram-iterate-over-sentence.

       PANGRAM-ITERATE-OVER-ALPHABET.
           MOVE 1 TO ws-result
           MOVE ord-a TO letter-idx

           PERFORM VARYING idx FROM 1 BY 1 UNTIL idx > 26
               MOVE ZERO TO count-it
               INSPECT ws-sentence TALLYING count-it
                                   FOR ALL CHAR(letter-idx)
               IF count-it EQUAL TO ZERO THEN
                   MOVE ZERO TO ws-result
                   EXIT PERFORM
               END-IF
               ADD 1 TO letter-idx
           END-PERFORM.

       PANGRAM-ITERATE-OVER-SENTENCE.
           MOVE ZERO TO ws-result
           MOVE SPACES TO Letters

           PERFORM VARYING idx FROM 1 BY 1
                               UNTIL idx > LENGTH(ws-sentence)
               MOVE ws-sentence(idx:1) TO chr
               IF isLetter THEN
                   COMPUTE letter-idx = ORD(chr) - ord-a + 1
                   MOVE chr TO Letter(letter-idx)
               END-IF
           END-PERFORM

           MOVE ZERO TO count-it
           INSPECT Letters TALLYING count-it FOR ALL SPACES
           IF count-it EQUAL TO ZERO THEN
               MOVE 1 TO ws-result
           END-IF.

      * Some rough benchmarks using `time bash test.sh`
      * indicate both versions take about the same amount of time, but
      * we don't have stressful tests.
