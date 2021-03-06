      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. NEI038.
       AUTHOR. SID WAITE.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CHARCUR ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC    RECORD KEY IS CHARCUR-KEY
           ALTERNATE RECORD KEY IS CC-PAYCODE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT FILEOUT ASSIGN TO "S35" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT FILEIN ASSIGN TO "S40" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT PAPEROUT ASSIGN TO "S45" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT GARFILE ASSIGN TO "S50" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS G-GARNO
           LOCK MODE MANUAL.
           SELECT DIAGFILE ASSIGN TO "S55" ORGANIZATION IS INDEXED
           ACCESS IS RANDOM RECORD KEY IS DIAG-KEY
           ALTERNATE RECORD KEY IS DIAG-TITLE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT REFPHY ASSIGN TO "S60" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS REF-KEY
           ALTERNATE RECORD KEY IS REF-BSNUM  WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-CRNUM WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-UPIN  WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-CDNUM WITH DUPLICATES
           ALTERNATE RECORD KEY IS REF-NAME  WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT GAPFILE ASSIGN TO "S65" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC RECORD KEY IS GAPKEY
           ALTERNATE RECORD KEY IS GAP-NAME WITH DUPLICATES
           ALTERNATE RECORD KEY IS GAP-CITY WITH DUPLICATES
           ALTERNATE RECORD KEY IS GAP-STATE WITH DUPLICATES
           LOCK MODE MANUAL.
           SELECT ERRORFILE ASSIGN TO "S70" ORGANIZATION
           LINE SEQUENTIAL.
           SELECT PROCFILE ASSIGN TO "S75" ORGANIZATION IS INDEXED
           ACCESS IS DYNAMIC  RECORD KEY IS PROC-KEY
           LOCK MODE MANUAL.
           SELECT PLACEFILE ASSIGN TO "S80" ORGANIZATION 
            LINE SEQUENTIAL.
           SELECT FILEOUT2 ASSIGN TO "S85" ORGANIZATION
           LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  PLACEFILE.
       01  PLACEFILE01.
           02 DF1 PIC X.
           02 DF2 PIC X.
           02 DF3 PIC X(22).
           02 DF4 PIC X(18).
           02 DF5 PIC X(15).
           02 DF6 PIC XX.
           02 DF7 PIC X(9).
       
       FD PROCFILE
           DATA RECORD IS PROCFILE01.
       01  PROCFILE01.
           02 PROC-KEY PIC X(7).
           02 PROC-OLD PIC X(7).
           02 PROC-TYPE PIC X.
           02 PROC-BCBS PIC X(4).
           02 PROC-TITLE PIC X(28).
           02 PROC-AMOUNT PIC S9(4)V99.
           02 CARE-AMOUNT PIC S9(4)V99.
       
       FD  ERRORFILE
           DATA RECORD IS ERRORFILE01.
       01  ERRORFILE01.
           02 EF1 PIC X(12).
           02 EF2 PIC X(37).
           02 EF3 PIC X(24).
       
       FD  REFPHY
      *    BLOCK CONTAINS 5 RECORDS
           DATA RECORD IS REFPHY01.
       01  REFPHY01.
           02 REF-KEY PIC XXX.
           02 REF-BSNUM PIC X(5).
           02 REF-CRNUM PIC X(6).
           02 REF-UPIN PIC X(6).
           02 REF-CDNUM PIC X(7).
           02 REF-NAME PIC X(24).
           02 REF-NPI PIC X(10).
       FD GAPFILE.
       01 GAPFILE01.
           02 GAPKEY PIC X(7).
           02 GAP-NAME PIC X(25).
           02 GAP-ADDR PIC X(22).
           02 GAP-CITY PIC X(15).
           02 GAP-STATE PIC XX.
           02 GAP-ZIP PIC X(9).
           02 GAP-TYPE PIC X.
           02 GAP-FUTURE PIC X(40).
       FD  DIAGFILE
           BLOCK CONTAINS 15 RECORDS
           DATA RECORD IS DIAG01.
       01  DIAG01.
           02 DIAG-KEY PIC X(7).
           02 DIAG-TITLE PIC X(61).
           02 DIAG-MEDB PIC X(5).
       
       FD GARFILE
           BLOCK CONTAINS 3 RECORDS
           DATA RECORD IS G-MASTER.
       01 G-MASTER.
           02 G-GARNO PIC X(8).
           02 G-GARNAME PIC X(24).
           02 G-BILLADD PIC X(22).
           02 G-STREET PIC X(22).
           02 G-CITY PIC X(18).
           02 G-STATE PIC X(2).
           02 G-ZIP5 PIC X(5).
           02 G-ZIP4 PIC X(4).
           02 G-COLLT PIC X.
           02 G-PHONE PIC X(10).
           02 G-SEX PIC X.
           02 G-RELATE PIC X.
           02 G-MSTAT PIC X.
           02 G-DOB PIC X(8).
           02 G-DUNNING PIC X.
           02 G-ACCTSTAT PIC X.
           02 G-PR-MPLR PIC X(4).
           02 G-PRINS PIC XXX.
           02 G-PR-ASSIGN PIC X.
           02 G-PR-OFFICE PIC X(4).
           02 G-PR-GROUP PIC X(10).
           02 G-PRIPOL0.
             03 G-PRIPOL PIC X(9).
             03 G-PR-SUFX PIC XXX.
             03 G-PR-FILLER PIC X(4).
           02 G-PRNAME PIC X(24).
           02 G-PR-RELATE PIC X.
           02 G-SE-MPLR PIC X(4).
           02 G-SEINS PIC XXX.
           02 G-SE-ASSIGN PIC X.
           02 G-TRINSIND PIC X.
           02 G-TRINS PIC XXX.
           02 G-SE-GROUP PIC X(10).
           02 G-SECPOL0.
             03 G-SECPOL PIC X(9).
             03 G-SE-SUFX PIC XXX.
             03 G-SE-FILLER PIC X(4).
           02 G-SENAME PIC X(24).
           02 G-SE-RELATE PIC X.
           02 G-INSPEND PIC S9(5)V99.
           02 G-LASTBILL PIC X(8).
           02 G-ASSIGNM PIC X.
           02 G-PRIVATE PIC X.
           02 G-BILLCYCLE PIC X.
           02 G-DELETE PIC X.
           02 G-FILLER PIC XXX.
       FD  PAPEROUT.
       01  PAPEROUT01.
           02 FO-PC PIC 999.
           02 FO-PATID.
             03 FO-PATID7 PIC X(7).
             03 FO-PATID8 PIC X.
           02 FO-KEY PIC X(11).
           02 FO-DATE PIC X(8).
           02 FO-ASSIGN PIC X.
           02 FO-PLACE PIC X.
           02 FO-DOC PIC XX.
           02 FO-PAPER PIC X.
       FD  FILEIN.
       01  FILEIN01 PIC XXX.
       FD FILEOUT.
       01  FILEOUT01 PIC X(156).
       FD FILEOUT2.
       01  FILEOUT201 PIC X(156).

       FD  CHARCUR.
       01  CHARCUR01.
           02 CHARCUR-KEY.
             03 CC-KEY8 PIC X(8).
             03 CC-KEY3 PIC XXX.
           02 CC-PATID PIC X(8).
           02 CC-CLAIM PIC X(6).
           02 CC-SERVICE PIC X.
           02 CC-DIAG PIC X(7).
           02 CC-PROC.
              03 CC-PROC1 PIC X(5).
              03 CC-PROC2 PIC XX.
           02 CC-MOD2 PIC XX.
           02 CC-MOD3 PIC XX.
           02 CC-MOD4 PIC XX.
           02 CC-AMOUNT PIC S9(4)V99.
           02 CC-DOCR PIC X(3).
           02 CC-DOCP PIC X(2).
           02 CC-PAYCODE PIC XXX.
           02 CC-STUD PIC X.
           02 CC-WORK PIC XX.
           02 CC-DAT1 PIC X(8).
           02 CC-RESULT PIC X.
           02 CC-ACT PIC X.
           02 CC-SORCREF PIC X.
           02 CC-COLLT PIC X.
           02 CC-AUTH PIC X.
           02 CC-PAPER PIC X.
           02 CC-PLACE PIC X.
           02 CC-EPSDT PIC X.
           02 CC-DATE-T PIC X(8).
           02 CC-DATE-A PIC X(8).
           02 CC-DATE-P PIC X(8).
           02 CC-REC-STAT PIC X.
           02 CC-DX2 PIC X(7).
           02 CC-DX3 PIC X(7).
           02 CC-ACC-TYPE PIC X.
           02 CC-DATE-M PIC X(8).
           02 CC-ASSIGN PIC X.
           02 CC-NEIC-ASSIGN PIC X.
           02 CC-DX4 PIC X(7).
           02 CC-DX5 PIC X(7).
           02 CC-DX6 PIC X(7).
           02 CC-FUTURE PIC X(6).
       WORKING-STORAGE SECTION.
       01  PLACE-TAB01.
           02 PLACE-TAB OCCURS 29 TIMES.
             03 PL-TAB PIC X.
             03 PL-NUM PIC X.
             03 PL-NAME PIC X(22).
             03 PL-STREET PIC X(18).
             03 PL-CITY PIC X(15).
             03 PL-STATE PIC XX.
             03 PL-ZIP PIC X(9).
       01  PLINDX PIC 99 VALUE 0.
       01  DIAGFLAG PIC 9.
       01  ALF7 PIC X(7).
       01  FLAG PIC 9.
       01  Y PIC 99.
       01  CNTR PIC 99 VALUE 0.
       01  NAMEFIRST PIC X(24).
       01  NAMELAST PIC X(24).
       01  CLM-5 PIC XX.
       01  ALF7X.
           02 FILLER  PIC X(4).
           02 ALF7X5 PIC X.
       01  MD01.
           02 MD1 PIC X.
           02 MD2 PIC X.
           02 MD3 PIC X.
           02 MD4 PIC X.
           02 MD5 PIC X.
           02 MD6 PIC X.
           02 MD7 PIC X.
           02 MD8 PIC X.
           02 MD9 PIC X.
           02 MD10 PIC X.
           02 MD11 PIC X.
           
       PROCEDURE DIVISION.
       0005-START.
           OPEN INPUT PLACEFILE FILEIN CHARCUR REFPHY GARFILE 
           GAPFILE DIAGFILE PROCFILE.
           OPEN OUTPUT PAPEROUT FILEOUT FILEOUT2 ERRORFILE.
           MOVE SPACE TO ERRORFILE01
           MOVE "MEDICARE ELECTRONIC CLAIMS ERRORS" TO ERRORFILE01
           WRITE ERRORFILE01.

       P00. READ PLACEFILE AT END GO TO P0.
           ADD 1 TO PLINDX.
           MOVE DF1 TO PL-TAB(PLINDX)
           MOVE DF2 TO PL-NUM(PLINDX)
           MOVE DF3 TO PL-NAME(PLINDX)
           MOVE DF4 TO PL-STREET(PLINDX)
           MOVE DF5 TO PL-CITY(PLINDX)
           MOVE DF6 TO PL-STATE(PLINDX)
           MOVE DF7 TO PL-ZIP(PLINDX)
           GO TO P00.
       
       P0. READ FILEIN AT END GO TO P6.
           MOVE FILEIN01 TO CC-PAYCODE
           START CHARCUR KEY NOT < CC-PAYCODE INVALID GO TO P0.
       P1. READ CHARCUR NEXT AT END GO TO P0.
           IF CC-PAYCODE NOT = FILEIN01 GO TO P0.
           IF CC-REC-STAT > "1" GO TO P1.

           IF CC-SERVICE = "M" GO TO P1-2.

           IF CC-PROC < "00100  " GO TO P1.

       P1-2.
           IF (CC-PROC1 = "G0179" OR "G0180" OR "G0181" OR "G0182")
             IF CC-PLACE = "2" MOVE "1" TO CC-PLACE
             END-IF
           END-IF.
           IF (CC-PAPER = "P" OR "O") 
           OR (CC-PROC = "G016622") PERFORM PAPER-1 GO TO P1.
           MOVE SPACE TO FILEOUT01
           IF CC-PAYCODE = "003" GO TO TEST-IT.
           IF CC-PAYCODE = "062" AND CC-PAPER = " " 
           AND CC-ASSIGN = "U" GO TO P1.
           IF CC-PAYCODE = "062"
           MOVE 0 TO FLAG
           PERFORM GAP-1 THRU GAP-1-EXIT
           IF FLAG = 1 GO TO P1.
           PERFORM PAPER-1
           GO TO P1.
       PAPER-1.
           MOVE CC-PAYCODE TO FO-PC
           MOVE CC-PATID TO FO-PATID
           MOVE CHARCUR-KEY TO FO-KEY
           MOVE CC-DATE-T TO FO-DATE 
           MOVE CC-ASSIGN TO FO-ASSIGN.
           MOVE CC-PLACE TO FO-PLACE 
           MOVE CC-DOCP TO FO-DOC 
           MOVE CC-PAPER TO FO-PAPER
           WRITE PAPEROUT01.
       TEST-IT.
           IF CC-DIAG = "0000000" 
           MOVE SPACE TO EF2
           MOVE "NO DIAG" TO EF2 
           PERFORM S1
           GO TO P1.
           MOVE CC-DIAG TO DIAG-KEY
           READ DIAGFILE INVALID 
           MOVE SPACE TO EF2
           MOVE "OLD DX1 CODE" TO EF2
           MOVE CC-DIAG TO EF3 
           PERFORM S1 
           GO TO P1.
           
           MOVE 0 TO DIAGFLAG
           IF CC-DX2 NOT = "0000000" 
           MOVE CC-DX2 TO ALF7
           MOVE 0 TO DIAGFLAG
           PERFORM DIAG-CHECK.
           IF DIAGFLAG = 1 
           MOVE SPACE TO EF2
           MOVE "OLD DX2 CODE" TO EF2 
           MOVE CC-DX2 TO EF3 
           PERFORM S1
           GO TO P1.
           IF CC-DX3 NOT = "0000000" MOVE CC-DX3 TO ALF7
           MOVE 0 TO DIAGFLAG
           PERFORM DIAG-CHECK.
           IF DIAGFLAG = 1 
           MOVE SPACE TO EF2
           MOVE "OLD DX3 CODE" TO EF2 
           MOVE CC-DX3 TO EF3 
           PERFORM S1
           GO TO P1.
           IF CC-DX4 NOT = "0000000" MOVE CC-DX4 TO ALF7
           MOVE 0 TO DIAGFLAG
           PERFORM DIAG-CHECK.
           IF DIAGFLAG = 1 
           MOVE SPACE TO EF2
           MOVE "OLD DX4 CODE" TO EF2 
           MOVE CC-DX4 TO EF3 
           PERFORM S1
           GO TO P1.
           MOVE CC-KEY8 TO G-GARNO.
           READ GARFILE INVALID 
           MOVE SPACE TO EF2
           MOVE "NO GARNO" TO EF2
           PERFORM S1 
           GO TO P1.
           IF (G-BILLADD = SPACE) AND (G-STREET = SPACE)
           MOVE CHARCUR-KEY TO EF1 
           MOVE "NO ADDRESS" TO EF2
           PERFORM S1
           GO TO P1.

           IF (G-PRINS = "003")
              IF (G-SEINS = "004" OR "005" OR "062")
                 IF ((G-SECPOL = SPACE) OR (G-SENAME = SPACE)
                   OR (G-SE-RELATE = SPACE))
                   MOVE SPACE TO EF2
                   STRING G-SEINS " MISSING 2ND-DARY INSURANCE INFO" 
                   DELIMITED BY "   " INTO EF2
                   PERFORM S1 GO TO P1
                 END-IF
              END-IF
           END-IF.
           IF NOT (G-PRINS = "003" OR  G-SEINS = "003")
           MOVE SPACE TO EF2
           MOVE "NO MEDICARE INSURANCE CODE" TO EF2
           PERFORM S1 
           GO TO P1.
           IF (G-BILLADD = SPACE) AND (G-STREET = SPACE)
           MOVE CHARCUR-KEY TO EF1 
           MOVE "NO ADDRESS" TO EF2
           PERFORM S1
           GO TO P1.
           IF (G-SEINS = "003" AND CC-PAPER = "E")
            WRITE FILEOUT201 FROM CHARCUR01
            GO TO P1
           END-IF
           IF (G-PRINS NOT = "003")
            IF CC-PAPER = "O"
             PERFORM PAPER-1
             GO TO P1
            END-IF
            IF CC-PAPER = "E" OR "P"
             GO TO P1
            END-IF
           END-IF.
           IF G-PRIPOL0(1:9) NOT NUMERIC 
             MOVE 0 TO FLAG
             PERFORM MBI-CHECK
             IF FLAG = 1
              MOVE CHARCUR-KEY TO EF1 
              MOVE SPACE TO EF2
              MOVE "MEDICARE POLICY IS INVALID" TO EF2
              MOVE G-PRIPOL0(1:11) TO EF3
              
              PERFORM S1 
              GO TO P1
             END-IF
           END-IF
           IF G-ZIP5 NOT NUMERIC 
             MOVE SPACE TO EF2
             MOVE "BAD ZIP CODE" TO EF2
             PERFORM S1 
           GO TO P1.
           IF CC-DOCR = "000" GO TO T1.
           MOVE CC-DOCR TO REF-KEY.
           READ REFPHY INVALID MOVE "INVALID" TO REF-CDNUM
           MOVE SPACE TO REF-NAME.

           IF REF-CDNUM = "INVALID"
           MOVE SPACE TO EF2
           STRING CC-DOCR " IS NOT A VALID REF. PHYS. CODE"
             DELIMITED BY "**" INTO EF2
             PERFORM S1 
            GO TO P1.

           IF (REF-NPI = SPACE)
            MOVE SPACE TO EF2
            STRING CC-DOCR " / HAS NO NPI" DELIMITED BY "**" INTO EF2
            PERFORM S1 
           GO TO P1.
           IF REF-NPI NOT NUMERIC
            MOVE SPACE TO EF2
            STRING CC-DOCR " / INVALID NPI" DELIMITED BY "**" INTO EF2
            PERFORM S1 
           GO TO P1.
           IF REF-NAME NOT = SPACE
            MOVE SPACE TO NAMELAST NAMEFIRST
            UNSTRING REF-NAME DELIMITED BY 
            "; " OR ";" OR " ; " OR " ," OR ", " OR " , " OR ","  
            INTO NAMELAST NAMEFIRST
             IF NAMEFIRST = SPACE
              MOVE SPACE TO EF2
              STRING CC-DOCR  " " REF-NAME " NAME OF REFERRING ERROR" 
              DELIMITED BY "!!" INTO EF2 
              PERFORM S1
              GO TO P1
             END-IF
           END-IF.
       T1.
           IF CC-PROC1 > "99240" AND < "99280"
           MOVE SPACE TO EF2
           MOVE "CONSULT CODES INVALID FOR MEDICARE" TO EF2 PERFORM S1 
           GO TO P1.
           
           IF (CC-PROC1 = "11719" OR "11055" OR "11056" OR "11057"
           OR "G0127")
           AND ((CC-DATE-M = "00000000") OR (CC-DOCR = "000")) 
           MOVE SPACE TO EF2
           STRING CC-PROC "LAST SEEN DATE/DOC NAME" DELIMITED BY "!!"
           INTO EF2 PERFORM S1 
           GO TO P1.
           
           IF (CC-PROC1 = "11721") AND (CC-DIAG(1:1) = "E") 
           AND ((CC-DATE-M = "00000000") OR (CC-DOCR = "000")) 
           MOVE SPACE TO EF2
           STRING CC-PROC "DLS/DOC DIABETICS" DELIMITED BY "!!"
           INTO EF2 PERFORM S1 
           GO TO P1.
           
           MOVE CC-PROC TO PROC-KEY.
           READ PROCFILE INVALID
             MOVE SPACE TO EF2
             MOVE "INVALID PROCEDURE CODE" TO EF2
             PERFORM S1
             GO TO P1
           END-READ.
             MOVE 0 TO FLAG
             PERFORM DF-SEARCH2 THRU DF-SEARCH2-EXIT
             VARYING Y FROM 1 BY 1 UNTIL Y > PLINDX.
              IF FLAG = 1
                MOVE SPACE TO EF2
                STRING 
                CC-PROC1 " / " CC-PLACE " PLACE/ADMIT-DATE CONFLICT " 
                DELIMITED BY "!!"
                INTO EF2 PERFORM S1 
                GO TO P1.
           IF CC-PROC1 > "90470" AND < "90750" 
             MOVE 0 TO FLAG
             PERFORM DF-SEARCH3 VARYING Y FROM 1 BY 1 UNTIL Y > PLINDX
            IF FLAG = 1
             MOVE SPACE TO EF2
             STRING 
             CC-PROC1 " / " CC-PLACE " INVALID PLACE OF SERVICE"
             DELIMITED BY "!!"
             INTO EF2 PERFORM S1 
             GO TO P1
            END-IF
           END-IF.
           
           WRITE FILEOUT01 FROM CHARCUR01 
           GO TO P1.

       DF-SEARCH2.  
           IF CC-PLACE NOT = PL-TAB(Y) GO TO DF-SEARCH2-EXIT.
            IF CC-DATE-M NOT = "00000000" GO TO DF-SEARCH2-EXIT.
             IF ( PL-NUM(Y) = "3" OR "7") 
              MOVE 1 TO FLAG
              MOVE PLINDX TO Y
              GO TO DF-SEARCH2-EXIT.
              PERFORM PLACE-OF-SERVICE THRU POS-EXIT
             IF (CLM-5 = "21") 
              AND ( PL-NUM(Y) NOT = "3" AND NOT = "7")
              MOVE 1 TO FLAG
              MOVE PLINDX TO Y
              GO TO DF-SEARCH2-EXIT.
       DF-SEARCH2-EXIT.
           EXIT.
       PLACE-OF-SERVICE.
           MOVE "00" TO CLM-5
           IF CC-PROC1 > "99220" AND < "99224" MOVE "21" TO CLM-5.
           IF CC-PROC1 > "99230" AND < "99234" MOVE "21" TO CLM-5.
           IF CC-PROC1 = "99238" MOVE "21" TO CLM-5.
           IF CC-PROC1 > "99289" AND < "99297" MOVE "21" TO CLM-5.
       POS-EXIT. 
           EXIT.
       DF-SEARCH3.  
           IF CC-PLACE = PL-TAB(Y) 
            IF PL-NUM(Y) = "3" 
              MOVE 1 TO FLAG
              MOVE PLINDX TO Y
             END-IF
           END-IF.

       
       GAP-1.
           MOVE CC-KEY8 TO G-GARNO
           READ GARFILE INVALID MOVE 1 TO FLAG 
           MOVE SPACE TO EF2
           STRING CC-KEY8 " BAD ACCOUNT #" DELIMITED BY "//"
           INTO EF2
           PERFORM S1 
           GO TO GAP-1-EXIT.
           
           MOVE G-PR-GROUP TO GAPKEY
           READ GAPFILE INVALID 
           MOVE SPACE TO EF2
           STRING G-GARNO " INVALID MEDIGAPE CODE" DELIMITED BY "//"
           INTO EF2
           PERFORM S1 
           MOVE 1 TO FLAG GO TO GAP-1-EXIT.
           IF GAP-TYPE = "X" OR "Y"
           MOVE 1 TO FLAG.
       GAP-1-EXIT.  EXIT.
       DIAG-CHECK.
           MOVE 0 TO DIAGFLAG
           MOVE ALF7 TO DIAG-KEY
           READ DIAGFILE INVALID MOVE 1 TO DIAGFLAG.

       S1. MOVE CHARCUR-KEY TO EF1 MOVE CC-KEY8 TO G-GARNO
           READ GARFILE INVALID MOVE SPACE TO G-GARNAME.
           MOVE G-GARNAME TO EF3
           WRITE ERRORFILE01.
        MBI-CHECK.
           MOVE G-PRIPOL0(1:11) TO MD01
           MOVE 0 TO FLAG
           IF (MD1 NOT NUMERIC) OR (MD1 = "0")
              DISPLAY "1ST POSITION NOT NUMERIC  " MD1
              MOVE 1 TO FLAG
           END-IF
           IF (MD2 NUMERIC) 
             OR ((MD2 ALPHABETIC) AND 
                (MD2 = "S" OR "L" OR "O" OR "I" OR "B" OR "Z"))
             OR NOT (MD2 ALPHABETIC  OR MD2 NUMERIC)
             DISPLAY "2ND POSITION IS INVALID  " MD2
             MOVE 1 TO FLAG
           END-IF             
           
           IF NOT((MD3 NUMERIC) OR ((MD3 ALPHABETIC) AND 
                (MD3 NOT = "S" OR "L" OR "O" OR "I" OR "B" OR "Z")))
             DISPLAY "3RD POSITION IS INVALID  " MD3
             MOVE 1 TO FLAG
           END-IF           
            IF MD4 NOT NUMERIC
              DISPLAY "4TH POSITION NOT NUMERIC  " MD4
              MOVE 1 TO FLAG
           END-IF   

           
           IF (MD5 NUMERIC) 
             OR ((MD5 ALPHABETIC) AND 
                (MD5 = "S" OR "L" OR "O" OR "I" OR "B" OR "Z"))
             OR NOT (MD5 ALPHABETIC  OR MD5 NUMERIC)
             DISPLAY "5TH POSITION IS INVALID  " MD5
             MOVE 1 TO FLAG
           END-IF        
           

             IF NOT ((MD6 NUMERIC) OR ((MD6 ALPHABETIC) AND 
                (MD6 NOT = "S" OR "L" OR "O" OR "I" OR "B" OR "Z")))
             DISPLAY "6TH POSITION IS INVALID  " MD6
             MOVE 1 TO FLAG
           END-IF 
           
             IF MD7 NOT NUMERIC
              DISPLAY "7TH POSITION NOT NUMERIC  " MD7
              MOVE 1 TO FLAG
           END-IF   
           
           IF (MD8 NUMERIC) 
             OR ((MD8 ALPHABETIC) AND 
                (MD8 = "S" OR "L" OR "O" OR "I" OR "B" OR "Z"))
             OR NOT (MD8 ALPHABETIC  OR MD8 NUMERIC)
             DISPLAY "8TH POSITION IS INVALID  " MD8
             MOVE 1 TO FLAG
           END-IF  
           IF (MD9 NUMERIC) 
             OR ((MD9 ALPHABETIC) AND 
                (MD9 = "S" OR "L" OR "O" OR "I" OR "B" OR "Z"))
             OR NOT (MD9 ALPHABETIC  OR MD9 NUMERIC)
             DISPLAY "9TH POSITION IS INVALID  " MD9
             MOVE 1 TO FLAG
           END-IF  
             IF MD10 NOT NUMERIC
              DISPLAY "10TH POSITION NOT NUMERIC  " MD10
              MOVE 1 TO FLAG
           END-IF   
             IF MD11 NOT NUMERIC
              DISPLAY "11TH POSITION NOT NUMERIC  " MD11
              MOVE 1 TO FLAG
            END-IF.   

       P6. CLOSE FILEOUT FILEOUT2 PAPEROUT ERRORFILE.
           CLOSE GARFILE DIAGFILE REFPHY GAPFILE PROCFILE
           STOP RUN.
