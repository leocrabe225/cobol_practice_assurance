       IDENTIFICATION DIVISION.
       PROGRAM-ID. assur.
       AUTHOR. BernadetteC&Leocrabe225.
       DATE-WRITTEN. 15-05-2025 (fr).
       DATE-COMPILED. null.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ASSURANCE-INPUT
               ASSIGN TO "assurances.csv"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ASSURANCE-OUTPUT
               ASSIGN TO "rapport-assurances.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ASSURANCE-ONE-OUTPUT
               ASSIGN TO "rapport-assurances-unique.dat"
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD ASSURANCE-INPUT.
       01 ASR-IN-RCD.
           05 ASR-IN-CONTRACT-CODE         PIC 9(08).
           05 FILLER                       PIC X(01).
           05 ASR-IN-CONTRACT-NAME         PIC X(14).
           05 FILLER                       PIC X(01).
           05 ASR-IN-PRODUCT-NAME          PIC X(14).
           05 FILLER                       PIC X(01).
           05 ASR-IN-CLIENT-NAME           PIC X(41).
           05 FILLER                       PIC X(01).
           05 ASR-IN-CONTRACT-STATUS       PIC X(08).
           05 FILLER                       PIC X(01).
           05 ASR-IN-START-DATE.
               10 ASR-IN-START-YEAR        PIC 9(04).
               10 ASR-IN-START-MONTH       PIC 9(02).
               10 ASR-IN-START-DAY         PIC 9(02).
           05 FILLER                       PIC X(01).
           05 ASR-IN-END-DATE.
               10 ASR-IN-END-YEAR          PIC 9(04).
               10 ASR-IN-END-MONTH         PIC 9(02).
               10 ASR-IN-END-DAY           PIC 9(02).
           05 FILLER                       PIC X(01).
           05 ASR-IN-AMOUNT                PIC X(09).
           05 FILLER                       PIC X(01).
           05 ASR-IN-CURRENCY              PIC X(03).

       FD ASSURANCE-OUTPUT.
       01 ASR-OUT-LINE-RCD.
           05 ASR-OUT-LINE                 PIC X(124).

       FD ASSURANCE-ONE-OUTPUT.
       01 ASR-ONE-OUT-LINE-RCD.
           05 ASR-ONE-OUT-LINE             PIC X(124).
     
       
       WORKING-STORAGE SECTION.
       01 WS-ASR-TBL.
           05 WS-ASR-RCD OCCURS 100 TIMES.
               10 WS-ASR-CONTRACT-CODE         PIC 9(08).
               10 WS-ASR-CONTRACT-NAME         PIC X(14).
               10 WS-ASR-PRODUCT-NAME          PIC X(14).
               10 WS-ASR-CLIENT-NAME           PIC X(41).
               10 WS-ASR-CONTRACT-STATUS       PIC X(08).
               10 WS-ASR-START-DATE.
                   15 WS-ASR-START-YEAR        PIC 9(04).
                   15 WS-ASR-START-MONTH       PIC 9(02).
                   15 WS-ASR-START-DAY         PIC 9(02).
               10 WS-ASR-END-DATE.
                   15 WS-ASR-END-YEAR          PIC 9(04).
                   15 WS-ASR-END-MONTH         PIC 9(02).
                   15 WS-ASR-END-DAY           PIC 9(02).
               10 WS-ASR-AMOUNT                PIC 9(06)V9(02).
               10 WS-ASR-CURRENCY              PIC X(03).

       77 WS-IDX                               PIC 9(03).
       
       77 WS-TBL-SIZE                          PIC 9(03).

       01 WS-EOF                               PIC 9(01).
           88 WS-EOF-TRUE                                VALUE 1.
           88 WS-EOF-FALSE                               VALUE 0.

       01 WS-DATE-OUTPUT.
           05 WS-OUT-DAY                       PIC 9(02).
           05 FILLER                           PIC X(01) VALUE "-".
           05 WS-OUT-MONTH                     PIC 9(02).
           05 FILLER                           PIC X(01) VALUE "-".
           05 WS-OUT-YEAR                      PIC 9(04).

       77 WS-OUT-LINE                          PIC X(124).

       77 WS-TOTAL-AMOUNT                      PIC 9(09)V9(02).

       01 WS-USER-INPUT                        PIC X(01).
           88 WS-USER-INPUT-YES                          VALUE "Y".
           88 WS-USER-INPUT-NO                           VALUE "N".

       01 WS-IDX-USER-INPUT                    PIC 9(03).

       77 WS-RCD-NBR                           PIC 9(03).

       01 WS-HEADER-1.
           05 FILLER                           PIC X(33) VALUE
                    "This is an insurance report with ".
           05 WS-HDR-1-RCD-AMT                 PIC 9(01).
           05 FILLER                           PIC X(21) VALUE
                    " records, totalizing ".
           05 WS-HDR-1-RCD-TTL                 PIC Z(08)9,9(02).
           05 FILLER                           PIC X(03) VALUE "€".
       
       01 WS-HEADER-2.
           05 FILLER                           PIC X(09) VALUE
                    "Code".
           05 FILLER                           PIC X(15) VALUE
                    "Contract name".
           05 FILLER                           PIC X(15) VALUE
                    "Product name".
           05 FILLER                           PIC X(42) VALUE
                    "Client name".
           05 FILLER                           PIC X(09) VALUE
                    "Status".
           05 FILLER                           PIC X(11) VALUE
                    "Start".
           05 FILLER                           PIC X(11) VALUE
                    "Stop".
           05 FILLER                           PIC X(12) VALUE
                    "Amount".

       01 WS-ASR-OUT-LINE.
           05 FILLER                           PIC X(16) VALUE 
                    "Contract code : ".
           05 WS-ASR-OUT-CONTRACT-CODE         PIC 9(08).
           05 FILLER                           PIC X(19) VALUE
                    " | Contract name : ".
           05 WS-ASR-OUT-CONTRACT-NAME         PIC X(14).
           05 FILLER                           PIC X(18) VALUE
                    " | Product name : ".
           05 WS-ASR-OUT-PRODUCT-NAME          PIC X(14).
           05 FILLER                           PIC X(17) VALUE 
                    " | Client name : ".
           05 WS-ASR-OUT-CLIENT-NAME           PIC X(41).
           05 FILLER                           PIC X(21) VALUE 
                    " | Contract status : ".
           05 WS-ASR-OUT-CONTRACT-STATUS       PIC X(08).
           05 FILLER                           PIC X(16) VALUE 
                    " | Start date : ".
           05 WS-ASR-OUT-START-DATE.
               10 WS-ASR-OUT-START-DAY         PIC 9(02).
               10 FILLER                       PIC X(01) VALUE "-".
               10 WS-ASR-OUT-START-MONTH       PIC 9(02).
               10 FILLER                       PIC X(01) VALUE "-".
               10 WS-ASR-OUT-START-YEAR        PIC 9(04).
           05 FILLER                           PIC X(14) VALUE 
                    " | End date : ".
           05 WS-ASR-OUT-END-DATE.
               10 WS-ASR-OUT-END-DAY           PIC 9(02).
               10 FILLER                       PIC X(01) VALUE "-".
               10 WS-ASR-OUT-END-MONTH         PIC 9(02).
               10 FILLER                       PIC X(01) VALUE "-".
               10 WS-ASR-OUT-END-YEAR          PIC 9(04).
           05 FILLER                           PIC X(12) VALUE 
                    " | Amount : ".
           05 WS-ASR-OUT-AMOUNT                PIC 9(07),9(02).
           05 WS-ASR-OUT-CURRENCY              PIC X(03).

       01 WS-ASR-OUT-LINE-FILE.
           05 WS-ASR-OUT-FILE-CONTRACT-CODE    PIC 9(08).
           05 FILLER                           PIC X(01) VALUE SPACE.
           05 WS-ASR-OUT-FILE-CONTRACT-NAME    PIC X(14).
           05 FILLER                           PIC X(01) VALUE SPACE.
           05 WS-ASR-OUT-FILE-PRODUCT-NAME     PIC X(14).
           05 FILLER                           PIC X(01) VALUE SPACE.
           05 WS-ASR-OUT-FILE-CLIENT-NAME      PIC X(41).
           05 FILLER                           PIC X(01) VALUE SPACE.
           05 WS-ASR-OUT-FILE-CTRACT-STATUS    PIC X(08).
           05 FILLER                           PIC X(01) VALUE SPACE.
           05 WS-ASR-OUT-FILE-START-DATE       PIC X(10).
           05 FILLER                           PIC X(01) VALUE SPACE.
           05 WS-ASR-OUT-FILE-END-DATE         PIC X(10).
           05 FILLER                           PIC X(01) VALUE SPACE.
           05 WS-ASR-OUT-FILE-AMOUNT           PIC 9(06),9(02).
           05 WS-ASR-OUT-FILE-CURRENCY         PIC X(03).
       PROCEDURE DIVISION.
           
           PERFORM 0100-READ-FILE-BEGIN
              THRU 0100-READ-FILE-END.

           PERFORM 0200-WRITE-3-7-BEGIN
              THRU 0200-WRITE-3-7-END.

           PERFORM 0400-WRITE-FILE-BEGIN
              THRU 0400-WRITE-FILE-END.

           PERFORM 0500-WRITE-ONE-RECORD-BEGIN
              THRU 0500-WRITE-ONE-RECORD-END

           STOP RUN.

       0100-READ-FILE-BEGIN.
           MOVE 0 TO WS-IDX.
           SET WS-EOF-FALSE TO TRUE.
           OPEN INPUT ASSURANCE-INPUT.
           PERFORM UNTIL WS-EOF-TRUE
               READ ASSURANCE-INPUT
                   AT END
                       SET WS-EOF-TRUE TO TRUE
                   NOT AT END
                       ADD 1 TO WS-IDX
                       MOVE ASR-IN-CONTRACT-CODE
                           TO WS-ASR-CONTRACT-CODE(WS-IDX)
                       MOVE ASR-IN-CONTRACT-NAME
                           TO WS-ASR-CONTRACT-NAME(WS-IDX)
                       MOVE ASR-IN-PRODUCT-NAME
                           TO WS-ASR-PRODUCT-NAME (WS-IDX)
                       MOVE ASR-IN-CLIENT-NAME
                           TO WS-ASR-CLIENT-NAME(WS-IDX)
                       MOVE ASR-IN-CONTRACT-STATUS
                           TO WS-ASR-CONTRACT-STATUS(WS-IDX)
                       MOVE ASR-IN-START-DATE
                           TO WS-ASR-START-DATE(WS-IDX)
                       MOVE ASR-IN-END-DATE
                           TO WS-ASR-END-DATE(WS-IDX)
                       MOVE ASR-IN-AMOUNT
                           TO WS-ASR-AMOUNT(WS-IDX)
                       MOVE ASR-IN-CURRENCY
                           TO WS-ASR-CURRENCY(WS-IDX)
               END-READ
           END-PERFORM.
           CLOSE ASSURANCE-INPUT.
           MOVE WS-IDX TO WS-TBL-SIZE.
       0100-READ-FILE-END.

       0200-WRITE-3-7-BEGIN.
           DISPLAY "Do you want to print record 3 and 7 (Y/N)?".
           ACCEPT WS-USER-INPUT.
           IF WS-USER-INPUT-YES THEN
               MOVE 3 TO WS-RCD-NBR
               PERFORM 0300-WRITE-RCD-BEGIN
                  THRU 0300-WRITE-RCD-END
               MOVE 7 TO WS-RCD-NBR
               PERFORM 0300-WRITE-RCD-BEGIN
                  THRU 0300-WRITE-RCD-END
           END-IF.
       0200-WRITE-3-7-END.

       0300-WRITE-RCD-BEGIN.
           MOVE WS-ASR-CONTRACT-CODE(WS-RCD-NBR) 
             TO WS-ASR-OUT-CONTRACT-CODE.
           MOVE WS-ASR-CONTRACT-NAME(WS-RCD-NBR)
             TO WS-ASR-OUT-CONTRACT-NAME.
           MOVE WS-ASR-PRODUCT-NAME(WS-RCD-NBR)
             TO WS-ASR-OUT-PRODUCT-NAME.
           MOVE WS-ASR-CLIENT-NAME(WS-RCD-NBR)
             TO WS-ASR-OUT-CLIENT-NAME.
           MOVE WS-ASR-CONTRACT-STATUS(WS-RCD-NBR)
             TO WS-ASR-OUT-CONTRACT-STATUS.
           MOVE WS-ASR-START-DAY(WS-RCD-NBR)
             TO WS-ASR-OUT-START-DAY.
           MOVE WS-ASR-START-MONTH(WS-RCD-NBR)
             TO WS-ASR-OUT-START-MONTH.
           MOVE WS-ASR-START-YEAR(WS-RCD-NBR)
             TO WS-ASR-OUT-START-YEAR.
           MOVE WS-ASR-END-DAY(WS-RCD-NBR)
             TO WS-ASR-OUT-END-DAY.
           MOVE WS-ASR-END-MONTH(WS-RCD-NBR)
             TO WS-ASR-OUT-END-MONTH.
           MOVE WS-ASR-END-YEAR(WS-RCD-NBR)
             TO WS-ASR-OUT-END-YEAR.
           MOVE WS-ASR-AMOUNT(WS-RCD-NBR)
             TO WS-ASR-OUT-AMOUNT.
           MOVE WS-ASR-CURRENCY(WS-RCD-NBR)
             TO WS-ASR-OUT-CURRENCY.
           DISPLAY WS-ASR-OUT-LINE.
       0300-WRITE-RCD-END.
       
       0400-WRITE-FILE-BEGIN.
           DISPLAY              "Do you want the report to be written to 
      -                        " rapport-assurance.dat (Y/N)?"
           ACCEPT WS-USER-INPUT.
           IF WS-USER-INPUT-YES THEN
               OPEN OUTPUT ASSURANCE-OUTPUT
               MOVE 0 TO WS-TOTAL-AMOUNT
               
               ADD WS-ASR-AMOUNT(3) TO WS-TOTAL-AMOUNT
               ADD WS-ASR-AMOUNT(7) TO WS-TOTAL-AMOUNT
               MOVE 2 TO WS-HDR-1-RCD-AMT
               MOVE WS-TOTAL-AMOUNT TO WS-HDR-1-RCD-TTL
               MOVE WS-HEADER-1 TO ASR-OUT-LINE-RCD
               WRITE ASR-OUT-LINE-RCD
               MOVE WS-HEADER-2 TO ASR-OUT-LINE-RCD
               WRITE ASR-OUT-LINE-RCD
               MOVE 3 TO WS-RCD-NBR
               PERFORM 0600-MOVE-RECORD-TO-LINE-BEGIN
                  THRU 0600-MOVE-RECORD-TO-LINE-END
               MOVE WS-ASR-OUT-LINE-FILE TO ASR-OUT-LINE
               WRITE ASR-OUT-LINE-RCD

               MOVE 7 TO WS-RCD-NBR
               PERFORM 0600-MOVE-RECORD-TO-LINE-BEGIN
                  THRU 0600-MOVE-RECORD-TO-LINE-END
               MOVE WS-ASR-OUT-LINE-FILE TO ASR-OUT-LINE
               WRITE ASR-OUT-LINE-RCD
               CLOSE ASSURANCE-OUTPUT
               DISPLAY "End of processing - 2 records exported"
           ELSE
               DISPLAY "Ok, the report won't be written to the file"
           END-IF.
       0400-WRITE-FILE-END.

       0500-WRITE-ONE-RECORD-BEGIN.
           DISPLAY         "Do you want an extra chosen record to go to  
      -                   "rapport-assurances-unique.dat (Y/N)?".
           ACCEPT WS-USER-INPUT.
           IF WS-USER-INPUT-YES THEN
               DISPLAY "Choose the record index : " WITH NO ADVANCING
               ACCEPT WS-IDX-USER-INPUT
               IF WS-IDX-USER-INPUT NOT EQUAL ZERO AND
                  WS-IDX-USER-INPUT <= WS-TBL-SIZE THEN
                   OPEN OUTPUT ASSURANCE-ONE-OUTPUT
                   MOVE 1 TO WS-HDR-1-RCD-AMT
                   MOVE WS-ASR-AMOUNT(WS-IDX-USER-INPUT)
                       TO WS-HDR-1-RCD-TTL
                   MOVE WS-HEADER-1 TO ASR-ONE-OUT-LINE
                   WRITE ASR-ONE-OUT-LINE-RCD
                   MOVE WS-HEADER-2 TO ASR-ONE-OUT-LINE
                   WRITE ASR-ONE-OUT-LINE-RCD

                   MOVE WS-IDX-USER-INPUT TO WS-RCD-NBR
                   PERFORM 0600-MOVE-RECORD-TO-LINE-BEGIN
                      THRU 0600-MOVE-RECORD-TO-LINE-END
                   MOVE WS-ASR-OUT-LINE-FILE TO ASR-ONE-OUT-LINE
                   WRITE ASR-ONE-OUT-LINE-RCD

                   CLOSE ASSURANCE-ONE-OUTPUT
                   DISPLAY "End of processing - 1 record exported"
               ELSE
                   DISPLAY "This is not a valid index, closing program."
               END-IF
           ELSE
               DISPLAY "Ok, no extra report done"
           END-IF.
       0500-WRITE-ONE-RECORD-END.

       0600-MOVE-RECORD-TO-LINE-BEGIN.
           MOVE WS-ASR-CONTRACT-CODE(WS-RCD-NBR)
               TO WS-ASR-OUT-FILE-CONTRACT-CODE.
           MOVE WS-ASR-CONTRACT-NAME(WS-RCD-NBR)
               TO WS-ASR-OUT-FILE-CONTRACT-NAME.
           MOVE WS-ASR-PRODUCT-NAME(WS-RCD-NBR)
               TO WS-ASR-OUT-FILE-PRODUCT-NAME.
           MOVE WS-ASR-CLIENT-NAME(WS-RCD-NBR)
               TO WS-ASR-OUT-FILE-CLIENT-NAME.
           MOVE WS-ASR-CONTRACT-STATUS(WS-RCD-NBR) 
               TO WS-ASR-OUT-FILE-CTRACT-STATUS.
           MOVE WS-ASR-START-DAY(WS-RCD-NBR) 
               TO WS-OUT-DAY.
           MOVE WS-ASR-START-MONTH(WS-RCD-NBR) 
               TO WS-OUT-MONTH.
           MOVE WS-ASR-START-YEAR(WS-RCD-NBR) 
               TO WS-OUT-YEAR.
           MOVE WS-DATE-OUTPUT 
               TO WS-ASR-OUT-FILE-START-DATE.
           MOVE WS-ASR-END-DAY(WS-RCD-NBR) 
               TO WS-OUT-DAY.
           MOVE WS-ASR-END-MONTH(WS-RCD-NBR) 
               TO WS-OUT-MONTH.
           MOVE WS-ASR-END-YEAR(WS-RCD-NBR) 
               TO WS-OUT-YEAR.
           MOVE WS-DATE-OUTPUT 
               TO WS-ASR-OUT-FILE-END-DATE.
           MOVE WS-ASR-AMOUNT(WS-RCD-NBR) 
               TO WS-ASR-OUT-FILE-AMOUNT.
           MOVE WS-ASR-CURRENCY(WS-RCD-NBR) 
               TO WS-ASR-OUT-FILE-CURRENCY.
       0600-MOVE-RECORD-TO-LINE-END.
