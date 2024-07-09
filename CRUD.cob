       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRUD.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENTS ASSIGN TO "CLIENTS.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               FILE STATUS IS CLIENTS-FILE-STATUS
               RECORD KEY IS CLIENTS-KEY.
       DATA DIVISION.
       FILE SECTION.
       FD CLIENTS.
       01 CLIENTS-REG.
           05 CLIENTS-KEY.
               10 CLIENTS-KEY-NUMBER   PIC 9(8).
           05 CLIENTS-NAME             PIC X(16).
           05 CLIENTS-EMAIL            PIC X(64).
       WORKING-STORAGE SECTION.
       77 CLIENTS-FILE-STATUS  PIC 99.
       77 WRK-TITLE        PIC X(16) VALUE "BASIC COBOL CRUD".
       77 WRK-CURRENT-SCR  PIC X(8) VALUE SPACES.
       77 WRK-MSG          PIC X(32) VALUE SPACES.
       77 WRK-OPTION       PIC X(1).

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
                   10 LINE 03 COLUMN 01 ERASE EOL.
                   10 LINE 03 COLUMN 30 FOREGROUND-COLOR 4 FROM WRK-MSG.

           01 MN.
               05 LINE 04 COLUMN 10 VALUE "1 - CREATE".
               05 LINE 05 COLUMN 10 VALUE "2 - READ".
               05 LINE 06 COLUMN 10 VALUE "3 - UPDATE".
               05 LINE 07 COLUMN 10 VALUE "4 - DELETE".
               05 LINE 08 COLUMN 10 VALUE "5 - GENERATE REPORT".
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
           PERFORM 0200-PROCESS-OPTION UNTIL WRK-OPTION = "Q".
           PERFORM 1000-CLOSE-FILE.
           STOP RUN.

           0000-OPEN-FILE.
               OPEN I-O CLIENTS

               IF CLIENTS-FILE-STATUS = 35 THEN
                   OPEN OUTPUT CLIENTS
                   CLOSE CLIENTS
                   OPEN I-O CLIENTS
               END-IF.

           0100-INIT-SCR.
               MOVE SPACES TO WRK-CURRENT-SCR.
               DISPLAY SCR.
               ACCEPT MN.

           0200-PROCESS-OPTION.
               EVALUATE WRK-OPTION
                   WHEN 1
                      PERFORM 0210-CREATE
                   WHEN 2
                      CONTINUE
                   WHEN 3
                      CONTINUE
                   WHEN 4
                      CONTINUE
                   WHEN 5
                      CONTINUE
                   WHEN 6
                      CONTINUE
                   WHEN "X"
                       CONTINUE
                   WHEN OTHER
                      MOVE "INVALID OPTION" TO WRK-MSG
                      PERFORM 0100-INIT-SCR
               END-EVALUATE.

           0210-CREATE.
               MOVE "CREATE" TO WRK-CURRENT-SCR.
               MOVE SPACES TO WRK-MSG.
               DISPLAY SCR.
               ACCEPT CREATE-SCR.

               WRITE CLIENTS-REG
                   INVALID KEY MOVE "CONFLICT" TO WRK-MSG
               END-WRITE.

               PERFORM 0100-INIT-SCR.

           1000-CLOSE-FILE.
               CLOSE CLIENTS.
