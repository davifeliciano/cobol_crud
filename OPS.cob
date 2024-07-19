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
