       IDENTIFICATION DIVISION.
       PROGRAM-ID. brochu.
       AUTHOR. SWAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT FILEIN ASSIGN TO "S30" ORGANIZATION
             LINE SEQUENTIAL.
           SELECT FILEOUT ASSIGN TO "S35" ORGANIZATION
             LINE SEQUENTIAL.
           SELECT FILEOUT2 ASSIGN TO "S40" ORGANIZATION
             LINE SEQUENTIAL.
           SELECT HOSPFILE ASSIGN TO "S45" ORGANIZATION INDEXED
             ACCESS IS DYNAMIC RECORD KEY IS HOSP-KEY
             ALTERNATE RECORD KEY IS H-INS-KEY WITH DUPLICATES
             ALTERNATE RECORD KEY IS H-INS-NAME WITH DUPLICATES.  

       DATA DIVISION.
       
       FILE SECTION.
       
       FD  FILEIN
           RECORD CONTAINS 1 TO 300 CHARACTERS.
       01  FILEIN01 PIC X(300).

       FD  FILEOUT.
       01  FILEOUT01 PIC X(132).
          
       FD  FILEOUT2.
       01  FILEOUT201 PIC X(132).

       FD  HOSPFILE
           DATA RECORD IS HOSPFILE01.
       01  HOSPFILE01.
           02 HOSP-KEY PIC X(5).
           02 H-INS-KEY PIC XXX.
           02 H-INS-NAME PIC X(18).

       WORKING-STORAGE SECTION.
       01  FL1 PIC X(30).
       01  FL2 PIC X(30).
       01  FL3 PIC X(30).
       01  FL4 PIC X(30).
       01  FL5 PIC X(2).
       01  FL6 PIC X(10).
       01  FL7 PIC X(15).
       01  FL8 PIC X(5).
       01  FL9 PIC X(5).
       01  RIGHT-4 PIC X(4) JUST RIGHT.
       01  SIGN-DOLLAR PIC X(4).
       01  CENTS PIC XX.
       01  NUMX PIC X(7).
       01  NUM6 PIC 9(6).
       01  FO-COMMIS PIC X(6).
       01  FO-PAIDX PIC X(6).
       01  ALF6 PIC X(6).
       01  NUM-6 PIC 9(4)V99.
       01  ALF1 PIC X.
       01  WS-WORK-AREA        PIC  X(05).                           
       01  WS-SUB              PIC  9(08) BINARY.      
       01  TALLY               PIC 9.         
                                                                                          

       PROCEDURE DIVISION.

       0005-START.
           OPEN INPUT FILEIN HOSPFILE
           OPEN OUTPUT FILEOUT FILEOUT2.

       P1. 
           MOVE SPACE TO FILEIN01
           READ FILEIN 
             AT END
               GO TO P2.

           MOVE SPACE TO FILEOUT01 FL1 FL2 FL3 FL4 FL5 FL6 FL7 FL8 FL9
           UNSTRING FILEIN01 DELIMITED BY ";" INTO
             FL1 FL2 FL3 FL4 FL5 FL6 FL7 FL8 FL9

           MOVE LENGTH OF WS-WORK-AREA TO WS-SUB.                         
           MOVE WS-SUB                 TO TALLY.                         
           MOVE ZERO                   TO WS-WORK-AREA.                   
           PERFORM UNTIL TALLY < 1                                       
               IF  FL8 (TALLY:1) > SPACE                             
                   MOVE FL8 (TALLY:1)  TO WS-WORK-AREA (WS-SUB:1)         
                   SUBTRACT 1          FROM WS-SUB                       
               END-IF                                                     
               SUBTRACT 1              FROM TALLY                         
           END-PERFORM.                                                   
      *                                                                   
      *    AT THIS POINT, 'WS-WORK-AREA' CONTAINS '0000E6'               
      *                                                                   
      *     DISPLAY FL1 " " WS-WORK-AREA
      *     ACCEPT ALF1

           MOVE WS-WORK-AREA TO HOSP-KEY
           READ HOSPFILE
             INVALID
               DISPLAY FL1 " " HOSP-KEY
               STRING FL1 " " FL8 " " FL9 INTO FILEOUT01
               WRITE FILEOUT01
           END-READ    
           
           GO TO P1.

       A1.

          

       P2.
           
           CLOSE FILEIN FILEOUT FILEOUT2 HOSPFILE
           STOP RUN.

