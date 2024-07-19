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
               05 LINE 03 COLUMN 30 FOREGROUND-COLOR 2 FROM WRK-MSG.
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
                       BLANK WHEN ZEROES AUTO-SKIP.
               05 DATA-FORM.
                   10 LINE 05 COLUMN 10 VALUE "NAME:".
                   10 COLUMN PLUS 02 PIC X(16) USING CLIENTS-NAME
                       AUTO-SKIP.
                   10 LINE 06 COLUMN 10 VALUE "EMAIL:".
                   10 COLUMN PLUS 02 PIC X(64) USING CLIENTS-EMAIL
                       AUTO-SKIP.
