       IDENTIFICATION DIVISION.
       PROGRAM-ID. CRUD.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 WRK-TITLE        PIC X(16) VALUE "BASIC COBOL CRUD".
       77 WRK-CURRENT-SCR  PIC X(8) VALUE SPACES.
       77 WRK-OPTION       PIC X(1).

       SCREEN SECTION.
           01 SCR.
               05 CLEAR-SCR.
                   10 BLANK SCREEN.
                   10 LINE 01 COLUMN 01 ERASE EOL
                       BACKGROUND-COLOR 2.
                   10 LINE 01 COLUMN 10 BACKGROUND-COLOR 2
                       FROM WRK-TITLE.
                   10 LINE 02 COLUMN 01 ERASE EOL
                       BACKGROUND-COLOR 1.
                   10 LINE 02 COLUMN 10 BACKGROUND-COLOR 1
                       FROM WRK-CURRENT-SCR.


           01 MN.
               05 LINE 04 COLUMN 10 VALUE "1 - CREATE".
               05 LINE 05 COLUMN 10 VALUE "2 - READ".
               05 LINE 06 COLUMN 10 VALUE "3 - UPDATE".
               05 LINE 07 COLUMN 10 VALUE "4 - DELETE".
               05 LINE 08 COLUMN 10 VALUE "5 - GENERATE REPORT".
               05 LINE 09 COLUMN 10 VALUE "Q - QUIT".
               05 LINE 10 COLUMN 10 VALUE "INSERT AN OPTION:".
               05 LINE 10 COLUMN 28 USING WRK-OPTION.
       PROCEDURE DIVISION.
           DISPLAY SCR.
           ACCEPT MN.
           STOP RUN.
