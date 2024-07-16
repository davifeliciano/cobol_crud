       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRUD.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENTS ASSIGN TO "CLIENTS.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               FILE STATUS IS CLIENTS-FILE-STATUS
               RECORD KEY IS CLIENTS-KEY.

           SELECT CLIENTS-EXPORT ASSIGN TO "CLIENTS.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CLIENTS.
       01 CLIENTS-REG.
           05 CLIENTS-KEY.
               10 CLIENTS-KEY-NUMBER   PIC 9(8).
           05 CLIENTS-NAME             PIC X(16).
           05 CLIENTS-EMAIL            PIC X(64).

       FD CLIENTS-EXPORT.
       01 EXPORT-REG.
           05 EXPORT-DATA PIC X(88).

       WORKING-STORAGE SECTION.
       77 CLIENTS-FILE-STATUS  PIC 99.
       77 WRK-TITLE            PIC X(16) VALUE "BASIC COBOL CRUD".
       77 WRK-CURRENT-SCR      PIC X(8) VALUE SPACES.
       77 WRK-MSG              PIC X(32) VALUE SPACES.
       77 WRK-OPTION           PIC X(1).
       77 WRK-CURSOR           PIC X(1).
       77 WRK-RECORDS-COUNT    PIC 9(6) VALUE ZEROES.

       SCREEN SECTION.
           01 SCR.
               05 CLEAR-SCR.
                   10 BLANK SCREEN.
                   10 LINE 01 COLUMN 01 ERASE EOL BACKGROUND-COLOR 2.
                   10 LINE 01 COLUMN 10 BACKGROUND-COLOR 2
                       FROM WRK-TITLE.
                   10 LINE 02 COLUMN 01 ERASE EOL BACKGROUND-COLOR 1.
                   10 LINE 02 COLUMN 10 BACKGROUND-COLOR 1
                       FROM WRK-CURRENT-SCR.

           01 MSG.
               05 LINE 03 COLUMN 01 ERASE EOL.
               05 LINE 03 COLUMN 30 FOREGROUND-COLOR 4 FROM WRK-MSG.
               05 LINE 24 COLUMN 01 ERASE EOL.
               05 LINE 24 COLUMN 01 VALUE
                   "PRESS Q TO QUIT, ANY OTHER KEY TO REPEAT --->".
               05 COLUMN PLUS 02 USING WRK-CURSOR AUTO-SKIP.

           01 DELETE-CONFIRM.
               05 LINE 24 COLUMN 01 VALUE
                   "PRESS Y TO CONFIRM DELETE --->".
               05 COLUMN PLUS 02 USING WRK-CURSOR AUTO-SKIP.

           01 EXPORT-LOADER.
               05 LINE 24 COLUMN 01 ERASE EOL.
               05 LINE 24 COLUMN 01 VALUE
                   "EXPORTING TO CLIENTS.txt".

           01 EXPORT-DONE.
               05 LINE 24 COLUMN 01 ERASE EOL.
               05 LINE 23 COLUMN 01 VALUE
                   "RECORDS EXPORTED TO CLIENTS.txt: ".
               05 LINE 23 COLUMN 34 FROM WRK-RECORDS-COUNT.
               05 LINE 24 COLUMN 01 VALUE
                   "PRESS ANY KEY TO DISSMIS".
               05 LINE 24 COLUMN 80 USING WRK-CURSOR AUTO-SKIP.

           01 MN.
               05 LINE 04 COLUMN 10 VALUE "1 - CREATE".
               05 LINE 05 COLUMN 10 VALUE "2 - READ".
               05 LINE 06 COLUMN 10 VALUE "3 - UPDATE".
               05 LINE 07 COLUMN 10 VALUE "4 - DELETE".
               05 LINE 08 COLUMN 10 VALUE "5 - EXPORT".
               05 LINE 09 COLUMN 10 VALUE "Q - QUIT".
               05 LINE 10 COLUMN 10 VALUE "INSERT AN OPTION:".
               05 LINE 10 COLUMN 28 USING WRK-OPTION.

           01 CREATE-SCR.
               05 KEY-INPUT FOREGROUND-COLOR 2.
                   10 LINE 04 COLUMN 10 VALUE "KEY:".
                   10 COLUMN PLUS 02 PIC 9(8) USING CLIENTS-KEY-NUMBER
                       BLANK WHEN ZEROES.
               05 DATA-FORM.
                   10 LINE 05 COLUMN 10 VALUE "NAME:".
                   10 COLUMN PLUS 02 PIC X(16) USING CLIENTS-NAME.
                   10 LINE 06 COLUMN 10 VALUE "EMAIL:".
                   10 COLUMN PLUS 02 PIC X(64) USING CLIENTS-EMAIL.

       PROCEDURE DIVISION.
           PERFORM 0000-OPEN-FILE.
           PERFORM 0100-INIT-SCR.
           PERFORM 0200-PROCESS-OPTION UNTIL WRK-OPTION = "Q"
               OR WRK-OPTION = "q".
           PERFORM 1000-CLOSE-FILE.
           STOP RUN.

           0000-OPEN-FILE.
               OPEN I-O CLIENTS

               IF CLIENTS-FILE-STATUS = 35
                   OPEN OUTPUT CLIENTS
                   CLOSE CLIENTS
                   OPEN I-O CLIENTS
               END-IF.


           1000-CLOSE-FILE.
               CLOSE CLIENTS.

           CLEAR-MSG.
               MOVE SPACES TO WRK-MSG WRK-CURSOR.

           0100-INIT-SCR.
               PERFORM CLEAR-MSG.
               MOVE SPACES TO WRK-CURRENT-SCR WRK-OPTION.
               DISPLAY SCR.
               ACCEPT MN.

           0200-PROCESS-OPTION.
               EVALUATE WRK-OPTION
                   WHEN 1
                       PERFORM CREATE-OP
                   WHEN 2
                       PERFORM READ-OP
                   WHEN 3
                       PERFORM UPDATE-OP
                   WHEN 4
                       PERFORM DELETE-OP
                   WHEN 5
                       PERFORM EXPORT-OP
                   WHEN OTHER
                       MOVE "INVALID OPTION" TO WRK-MSG
                       ACCEPT MSG

                       IF WRK-CURSOR = "Q" OR WRK-CURSOR = "q"
                           PERFORM 1000-CLOSE-FILE
                           STOP RUN
                       ELSE
                           PERFORM 0100-INIT-SCR
                       END-IF
               END-EVALUATE.

           OP-AGAIN-OR-QUIT.
               IF WRK-CURSOR = "Q" OR WRK-CURSOR = "q"
                   PERFORM 0100-INIT-SCR
               ELSE
                   EVALUATE WRK-OPTION
                       WHEN 1
                           PERFORM CREATE-OP
                       WHEN 2
                           PERFORM READ-OP
                       WHEN 3
                           PERFORM UPDATE-OP
                       WHEN 4
                           PERFORM DELETE-OP
                       WHEN 5
                           PERFORM EXPORT-OP
                       WHEN OTHER
                           PERFORM 0100-INIT-SCR
                   END-EVALUATE
               END-IF.

           CREATE-OP.
               PERFORM CLEAR-MSG.
               MOVE "CREATE" TO WRK-CURRENT-SCR.
               MOVE SPACES TO CLIENTS-REG.
               DISPLAY SCR.
               ACCEPT CREATE-SCR.

               IF CLIENTS-KEY-NUMBER = ZEROES OR
                       (CLIENTS-NAME = SPACES AND
                        CLIENTS-EMAIL = SPACES)
                   MOVE "INSERT KEY AND NAME OR EMAIL" TO WRK-MSG
               ELSE
                   WRITE CLIENTS-REG
                       INVALID KEY
                           MOVE "CONFLICT" TO WRK-MSG
                       NOT INVALID KEY
                           MOVE "CREATED" TO WRK-MSG
                   END-WRITE
               END-IF.

               ACCEPT MSG.
               PERFORM OP-AGAIN-OR-QUIT.

           READ-OP.
               PERFORM CLEAR-MSG.
               MOVE "READ" TO WRK-CURRENT-SCR.
               MOVE SPACES TO CLIENTS-REG.
               DISPLAY SCR.
               ACCEPT KEY-INPUT.

               READ CLIENTS
                   INVALID KEY
                       MOVE "NOT FOUND" TO WRK-MSG
                   NOT INVALID KEY
                       MOVE "SUCCESS" TO WRK-MSG
                       DISPLAY DATA-FORM
               END-READ.

               ACCEPT MSG.
               PERFORM OP-AGAIN-OR-QUIT.

           UPDATE-OP.
               PERFORM CLEAR-MSG.
               MOVE "UPDATE" TO WRK-CURRENT-SCR.
               MOVE SPACES TO CLIENTS-REG.
               DISPLAY SCR.
               ACCEPT KEY-INPUT.

               READ CLIENTS
                   INVALID KEY
                       MOVE "NOT FOUND" TO WRK-MSG
                   NOT INVALID KEY
                       ACCEPT DATA-FORM
                       REWRITE CLIENTS-REG

                       IF CLIENTS-FILE-STATUS = 0
                           MOVE "UPDATED" TO WRK-MSG
                       ELSE
                           MOVE "NOT UPDATED" TO WRK-MSG
                       END-IF
               END-READ.

               ACCEPT MSG.
               PERFORM OP-AGAIN-OR-QUIT.

           DELETE-OP.
               PERFORM CLEAR-MSG.
               MOVE "DELETE" TO WRK-CURRENT-SCR.
               MOVE SPACES TO CLIENTS-REG.
               DISPLAY SCR.
               ACCEPT KEY-INPUT.

               READ CLIENTS
                   INVALID KEY
                       MOVE "NOT FOUND" TO WRK-MSG
                   NOT INVALID KEY
                       DISPLAY DATA-FORM
                       ACCEPT DELETE-CONFIRM

                       IF WRK-CURSOR = "Y" OR WRK-CURSOR = "y"
                           DELETE CLIENTS
                               INVALID KEY
                                   MOVE "NOT FOUND" TO WRK-MSG
                               NOT INVALID
                                   MOVE "DELETED" TO WRK-MSG
                           END-DELETE
                       ELSE
                           MOVE "NOT DELETED" TO WRK-MSG
                       END-IF
               END-READ.

               ACCEPT MSG.
               PERFORM OP-AGAIN-OR-QUIT.



           EXPORT-OP.
               OPEN OUTPUT CLIENTS-EXPORT.
               MOVE ZEROES TO WRK-RECORDS-COUNT.
               DISPLAY EXPORT-LOADER.
               START CLIENTS FIRST.
               READ CLIENTS NEXT.

               PERFORM UNTIL CLIENTS-FILE-STATUS <> 0
                   MOVE CLIENTS-REG TO EXPORT-REG
                   WRITE EXPORT-REG
                   READ CLIENTS NEXT
                   ADD 01 TO WRK-RECORDS-COUNT
               END-PERFORM

               CLOSE CLIENTS-EXPORT.
               ACCEPT EXPORT-DONE.
               PERFORM 0100-INIT-SCR.
