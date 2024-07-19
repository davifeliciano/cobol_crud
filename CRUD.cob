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
           COPY WRKBOOK.

       SCREEN SECTION.
           COPY SCRBOOK.

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

           COPY OPS.
