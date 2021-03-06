      * @package cms
      * @link    http://www.cmsvt.com
      * @author  s waite <cmswest@sover.net>
      * @copyright Copyright (c) 2020 cms <cmswest@sover.net>
      * @license https://github.com/openemr/openemr/blob/master/LICENSE GNU General Public License 3
       IDENTIFICATION DIVISION.
       PROGRAM-ID. bbb816.
       AUTHOR. SID WAITE.
       DATE-COMPILED. TODAY.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT PAYCUR ASSIGN TO "S30" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC RECORD KEY IS PAYCUR-KEY
           lock mode manual.

           SELECT PAYFILE ASSIGN TO "S35" ORGANIZATION IS INDEXED
           ACCESS MODE IS DYNAMIC  RECORD KEY IS PAYFILE-KEY
           lock mode manual.

           SELECT FILEOUT ASSIGN TO "S40"
           ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.

       FILE SECTION.

       FD  FILEOUT.
       01  FILEOUT01 PIC X(80).

       FD  PAYFILE.
       01  PAYFILE01.
           02 PAYFILE-KEY.
             03 PD-KEY8 PIC X(8).
             03 PD-KEY3 PIC XXX.
           02 PD-NAME PIC X(24).
           02 PD-AMOUNT PIC S9(4)V99.
           02 PD-PAYCODE PIC XXX.
           02 PD-DENIAL PIC XX.
           02 PD-CLAIM PIC X(6).
           02 PD-DATE-T PIC X(8).
           02 PD-DATE-E PIC X(8).
           02 PD-ORDER PIC X(6).
           02 PD-BATCH PIC X(6).

       FD PAYCUR.
       01 PAYCUR01.
           02 PAYCUR-KEY.
             03 PC-KEY8 PIC X(8).
             03 PC-KEY3 PIC XXX.
           02 PC-AMOUNT PIC S9(4)V99.
           02 PC-PAYCODE PIC XXX.
           02 PC-DENIAL PIC XX.
           02 PC-CLAIM PIC X(6).
           02 PC-DATE-T PIC X(8).
           02 PC-DATE-E PIC X(8).
           02 PC-BATCH PIC X(6).

       WORKING-STORAGE SECTION.

       01  CNTR PIC 9.
       01  ANS PIC X.

       PROCEDURE DIVISION.

       P0.
           OPEN INPUT PAYFILE PAYCUR OUTPUT FILEOUT.

       P1. 
           READ PAYFILE NEXT
             AT END
               GO TO P2.

           MOVE SPACE TO PC-KEY3
           MOVE PD-KEY8 TO PC-KEY8
           START PAYCUR KEY NOT < PAYCUR-KEY
             INVALID
               PERFORM A1
               GO TO P1.

           MOVE 0 TO CNTR.

       P3. 
           READ PAYCUR NEXT
             AT END
               PERFORM A1
               GO TO P1.

           IF PC-KEY8 NOT = PD-KEY8
             PERFORM A1
             GO TO P1.

           IF PC-CLAIM = PD-CLAIM
             AND PC-AMOUNT = PD-AMOUNT
             AND PC-DENIAL = PD-DENIAL
             AND PC-PAYCODE = PD-PAYCODE  
             AND PC-DATE-T = PD-DATE-T
             WRITE FILEOUT01 FROM PAYCUR01
             GO TO P1.

           GO TO P3.

       A1.
           DISPLAY PAYFILE01.
           ACCEPT OMITTED.

       P2. 
           CLOSE FILEOUT PAYFILE PAYCUR. 
           STOP RUN.
