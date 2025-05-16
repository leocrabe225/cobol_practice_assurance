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
       01 ASR-OUT-RCD.
           05 ASR-OUT-CONTRACT-CODE        PIC 9(08).
           05 FILLER                       PIC X(01).
           05 ASR-OUT-CONTRACT-NAME        PIC X(14).
           05 FILLER                       PIC X(01).
           05 ASR-OUT-PRODUCT-NAME         PIC X(14).
           05 FILLER                       PIC X(01).
           05 ASR-OUT-CLIENT-NAME          PIC X(41).
           05 FILLER                       PIC X(01).
           05 ASR-OUT-CONTRACT-STATUS      PIC X(08).
           05 FILLER                       PIC X(01).
           05 ASR-OUT-START-DATE           PIC X(10).
           05 FILLER                       PIC X(01).
           05 ASR-OUT-END-DATE             PIC X(10).
           05 FILLER                       PIC X(01).
           05 ASR-OUT-AMOUNT               PIC 9(06),9(02).
           05 ASR-OUT-CURRENCY             PIC X(03).
       01 ASR-OUT-LINE-RCD.
           05 ASR-OUT-LINE                 PIC X(121).
     
       
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
       77 WS-IDX-2                             PIC 9(03).
       
       77 WS-TBL-SIZE                          PIC 9(03).

       01 WS-EOF                               PIC 9(01).
           88 WS-EOF-TRUE                                VALUE 1.
           88 WS-EOF-FALSE                               VALUE 0.

       01 WS-USER-INPUT                        PIC X(01).
           88 WS-USER-INPUT-YES                          VALUE "Y".
           88 WS-USER-INPUT-NO                           VALUE "N".

       77 WS-RECORD-TO-WRITE                   PIC 9(03).

       01 WS-DATE-OUTPUT.
           05 WS-OUT-DAY                       PIC 9(02).
           05 FILLER                           PIC X(01) VALUE "-".
           05 WS-OUT-MONTH                     PIC 9(02).
           05 FILLER                           PIC X(01) VALUE "-".
           05 WS-OUT-YEAR                      PIC 9(04).

       77 WS-TOTAL-AMOUNT                      PIC 9(09)V9(02).
       77 WS-TOTAL-STR  PIC Z(08)9,99.
       PROCEDURE DIVISION.
           
           PERFORM 0100-READ-FILE-BEGIN
              THRU 0100-READ-FILE-END.

           PERFORM 0200-WRITE-TABLE-BEGIN
              THRU 0200-WRITE-TABLE-END.

           PERFORM 0400-WRITE-FILE-BEGIN
              THRU 0400-WRITE-FILE-END.

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

       0200-WRITE-TABLE-BEGIN.
           MOVE 1 TO WS-IDX.
           PERFORM VARYING WS-IDX FROM 1 BY 1
                   UNTIL WS-IDX > WS-TBL-SIZE
               DISPLAY WS-ASR-RCD(WS-IDX)
           END-PERFORM.
       0200-WRITE-TABLE-END.

       0400-WRITE-FILE-BEGIN.
           DISPLAY             "Do you want the rapport to be written to 
      -                        " rapport-assurance.dat (Y/N)?"
           ACCEPT WS-USER-INPUT.
           IF WS-USER-INPUT-YES THEN
               OPEN OUTPUT ASSURANCE-OUTPUT
               MOVE 0 TO WS-TOTAL-AMOUNT
               MOVE 1 TO WS-IDX-2
               
               MOVE SPACE TO ASR-OUT-LINE
               ADD WS-ASR-AMOUNT(3) TO WS-TOTAL-AMOUNT
               ADD WS-ASR-AMOUNT(7) TO WS-TOTAL-AMOUNT
               
               STRING        "This is an insurance report with 2 records
      -            ",totalizing "
                   DELIMITED BY SIZE
                   INTO ASR-OUT-LINE
                   WITH POINTER WS-IDX-2
               END-STRING
               MOVE WS-TOTAL-AMOUNT TO WS-TOTAL-STR
               STRING WS-TOTAL-STR
                   DELIMITED BY SIZE
                   INTO ASR-OUT-LINE
                   WITH POINTER WS-IDX-2
               END-STRING
               STRING "€"
                   DELIMITED BY SIZE
                   INTO ASR-OUT-LINE
                   WITH POINTER WS-IDX-2
               END-STRING
               
               WRITE ASR-OUT-LINE-RCD
               MOVE                                           "Code     
      -                                                 "Contract name  
      -                                                 "Product name   
      -                      "Client name                               
      -                                                       "Status   
      -                                                     "Start    
      -                                                     "Stop     
      -                                                  "Amount       " 
                   TO ASR-OUT-LINE-RCD
               WRITE ASR-OUT-LINE-RCD
               MOVE 3 TO WS-RECORD-TO-WRITE
               PERFORM 0500-MOVE-RECORD-TO-LINE-BEGIN
                  THRU 0500-MOVE-RECORD-TO-LINE-END

               MOVE 7 TO WS-RECORD-TO-WRITE
               PERFORM 0500-MOVE-RECORD-TO-LINE-BEGIN
                  THRU 0500-MOVE-RECORD-TO-LINE-END
               CLOSE ASSURANCE-OUTPUT
           ELSE
               DISPLAY "Ok, the rapport won't be written to the file"
           END-IF.
       0400-WRITE-FILE-END.

       0500-MOVE-RECORD-TO-LINE-BEGIN.
           INITIALIZE ASR-OUT-LINE-RCD.
           MOVE WS-ASR-CONTRACT-CODE(WS-RECORD-TO-WRITE)
               TO ASR-OUT-CONTRACT-CODE
           MOVE WS-ASR-CONTRACT-NAME(WS-RECORD-TO-WRITE)
               TO ASR-OUT-CONTRACT-NAME
           MOVE WS-ASR-PRODUCT-NAME(WS-RECORD-TO-WRITE)
               TO ASR-OUT-PRODUCT-NAME
           MOVE WS-ASR-CLIENT-NAME(WS-RECORD-TO-WRITE)
               TO ASR-OUT-CLIENT-NAME
           MOVE WS-ASR-CONTRACT-STATUS(WS-RECORD-TO-WRITE) 
               TO ASR-OUT-CONTRACT-STATUS
           MOVE WS-ASR-START-DAY(WS-RECORD-TO-WRITE) 
               TO WS-OUT-DAY.
           MOVE WS-ASR-START-MONTH(WS-RECORD-TO-WRITE) 
               TO WS-OUT-MONTH.
           MOVE WS-ASR-START-YEAR(WS-RECORD-TO-WRITE) 
               TO WS-OUT-YEAR.
           MOVE WS-DATE-OUTPUT 
               TO ASR-OUT-START-DATE.
           MOVE WS-ASR-END-DAY(WS-RECORD-TO-WRITE) 
               TO WS-OUT-DAY.
           MOVE WS-ASR-END-MONTH(WS-RECORD-TO-WRITE) 
               TO WS-OUT-MONTH.
           MOVE WS-ASR-END-YEAR(WS-RECORD-TO-WRITE) 
               TO WS-OUT-YEAR.
           MOVE WS-DATE-OUTPUT 
               TO ASR-OUT-END-DATE.
           MOVE WS-ASR-AMOUNT(WS-RECORD-TO-WRITE) 
               TO ASR-OUT-AMOUNT.
           MOVE WS-ASR-CURRENCY(WS-RECORD-TO-WRITE) 
               TO ASR-OUT-CURRENCY.
           WRITE ASR-OUT-RCD.
       0500-MOVE-RECORD-TO-LINE-END.
