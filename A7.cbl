       identification division.
       program-id. A7-CallCenterOpReport.
       author. Kaifkhan Vakil.
       date-written. 2021-04-06.
      *Program Description:
      *    Program to create Call Center Operator Report. This program 
      *    will read the input file and output the data after processing
      *    it. We will be using tables and array in this program to 
      *    for making calculation much easier. 
      *
       environment division.
       input-output section.
       file-control.
      *Assigning input file. 
           select input-file
               assign to '../../../A7.dat'
               organization is line sequential.
      *Assigning output file and noting is output position. 
           select report-file
               assign to '../../../A7-CallCenterOpReport.out'
               organization is line sequential.
      *
       data division.
       file section.
      *Declaring input file. 
       fd input-file
           data record is emp-rec.
      *Declaring input fields. 
       01 emp-rec.
         05 emp-rec-num                pic x(3).
         05 emp-rec-name               pic x(12).
         05 emp-rec-calls              pic 9(3)    occurs 12 times.
      *Declaring output file. 
       fd report-file
           data record is print-line
           record contains 132 characters.
      *
       01 print-line pic x(132).
      *
       working-storage section.
      *Variable level for storing constants later used in the program. 
       01 ws-constants.
         05 ws-number-of-months        pic 99      value 12.
         05 ws-month-names             pic x(36)   value
                           "JULAUGSEPOCTNOVDECJANFEBMARAPRMAYJUN".
         05 ws-month-literals redefines ws-month-names
                                       pic x(3)    occurs
                              12 times.
      *Heading variable which will display report heading with my name. 
       01 ws-name-line.
         05 filler                     pic x(85)   value spaces.
         05 filler                     pic x(30)   value 
         '  Kaifkhan Vakil, Assignment 7'.
         05 filler                     pic x(18)   value spaces.
      *Report heading 
       01 ws-report-heading.
         05 filler                     pic x(37)   value spaces.
         05 filler                     pic x(40)   value
                   '  Call Centre Volumes for  July - June  '.
         05 filler                     pic x(55)   value spaces.
      *Column headings. 
       01 ws-heading-line1.
         05 filler                     pic x(40)   value
                   '  Operator  Operator      Jul   Aug   Se'.
         05 filler                     pic x(40)   value
                   'p   Oct   Nov   Dec   Jan   Feb   Mar   '.
         05 filler                     pic x(40)   value
                   'Apr   May   Jun    Total   Avg REM      '.
         05 filler                     pic x(12)   value spaces.
      *Columns headings 2
       01 ws-heading-line2.
         05 filler                     pic x(40)   value
                   "     #       Name                       ".
         05 filler                     pic x(40)   value
                   "                                        ".
         05 filler                     pic x(52)   value spaces.
      *This line will be showing the data of the file. 
       01 ws-detail-line.
         05 filler                     pic x(4)    value spaces.
         05 ws-dl-num                  pic x(3)    value spaces.
         05 filler                     pic x(6)    value spaces.
         05 ws-dl-name                 pic x(12)   value spaces.
         05 filler                     pic x(1)    value spaces.
         05 ws-dl-months-table                     occurs 12 times.
           10 ws-dl-months             pic zz9.
           10 ws-dl-filler             pic x(3).
         05 filler                     pic x(2)    value spaces.
         05 ws-dl-total                pic zzz9    value 0.
         05 filler                     pic x(2)    value spaces.
         05 ws-dl-avg                  pic zzz9    value 0.
         05 ws-dl-avg-text redefines ws-dl-avg
                                       pic x(4).
         05 filler                     pic x(2)    value spaces.
         05 ws-dl-rem                  pic z9      value 0.
         05 ws-dl-rem-text redefines ws-dl-rem
                                       pic xx.
         05 filler                     pic x(18)   value spaces.
      *This is the sumary line showing operators with no calls in all 12
      *  months
       01 ws-ops-line.
         05 filler                     pic x(4)    value spaces.
         05 filler                     pic x(10)   value "Operators ".
         05 filler                     pic x(10)   value "with calls".
         05 ws-ol-months-table                     occurs 12 times.
           10 ws-ol-months             pic zzzz9   value 0.
           10 ws-ol-filler             pic x(1).
         05 filler                     pic x(36)   value spaces.
      *This is the total line which will show the totals of total, 
      *average and remainder
       01 ws-total-line.
         05 filler                     pic x(4)    value spaces.
         05 filler                     pic x(10)   value "Totals    ".
         05 filler                     pic x(10)   value spaces.
         05 ws-tl-months-table                     occurs 12 times.
           10 ws-tl-months             pic zzzz9   value 0.
           10 ws-tl-filler             pic x(1).
         05 filler                     pic x(3)    value spaces.
         05 ws-tl-total                pic zzzz9   value 0.
         05 filler                     pic x(1)    value spaces.
         05 ws-tl-avg                  pic zzzz9   value 0.
         05 filler                     pic x(1)    value spaces.
         05 ws-tl-rem                  pic zz9     value 0.
         05 filler                     pic x(18)   value spaces.

      *This is the average line which will show the average of the calls
      *  each operator has taken in that month
       01 ws-average-line.
         05 filler                     pic x(4)    value spaces.
         05 filler                     pic x(10)   value "Averages  ".
         05 filler                     pic x(10)   value spaces.
         05 ws-al-months-table                     occurs 12 times.
           10 ws-al-months             pic zzzz9   value 0.
           10 ws-al-filler             pic x(1).
         05 filler                     pic x(36)   value spaces.

      *This is the total line showing who did not have any calls
       01 ws-total-line-no-calls.
         05 filler                     pic x(3)    value spaces.
         05 filler                     pic x(38)   value
                   'Number of Operators with No Calls:    '.
         05 filler                     pic x(21)   value spaces.
         05 ws-tl-zero-calls           pic z9      value 0.
         05 filler                     pic x(37)   value spaces.
         05 filler                     pic x(52)   value spaces.

      *This is the line which will show of how many times there was a 
      *month  where there were not calls done by operators. 
       01 ws-total-line-no-calls-months.
         05 filler                     pic x(3)    value spaces.
         05 filler                     pic x(23)   value 
         'Number of Months where '.
         05 filler                     pic x(28)   value 
         'Operators have No Calls:    '.
         05 filler                     pic x(8)    value spaces.
         05 ws-tl-zero-mths            pic z9      value 0.
         05 filler                     pic x(89)   value spaces.

      *Shows highest monthly average. 
       01 ws-total-line-highest-month-average.
         05 filler                     pic x(3)    value spaces.
         05 filler                     pic x(23)   value 
         'Month with the Highest '.
         05 filler                     pic x(28)   value 
         'Monthly Average:            '.
         05 ws-tl-h-mth-nam            pic x(3)    value "XXX".
         05 filler                     pic x(2)    value spaces.
         05 ws-tl-h-mth-avg            pic zzzz9   value 0.
         05 filler                     pic x(66)   value spaces.
      *Shows highest operator average
       01 ws-tl-highest-operator-average.
         05 filler                     pic x(3)    value spaces.
         05 filler                     pic x(23)   value 
         'Operator with the Highe'.
         05 filler                     pic x(28)   value 
         'st Monthly Average:         '.
         05 ws-tl-h-op-num             pic x(3)    value "XXX".
         05 filler                     pic x(2)    value spaces.
         05 ws-tl-h-op-avg             pic zzzz9   value 0.
         05 filler                     pic x(68)   value spaces.
      *Shows lowest operator average
       01 ws-tl-lowest-operator-average.
         05 filler                     pic x(3)    value spaces.
         05 filler                     pic x(23)   value 
         'Operator with the Lowes'.
         05 filler                     pic x(28)   value 
         't Monthly Average:          '.
         05 ws-tl-l-op-num             pic x(3)    value "XXX".
         05 filler                     pic x(2)    value spaces.
         05 ws-tl-l-op-avg             pic zzzz9   value 0.
         05 filler                     pic x(68)   value spaces.
      *Shows total calls overall done by all the employers in 12 months. 
       01 ws-total-line-overall.
         05 filler                     pic x(3)    value spaces.
         05 filler                     pic x(20)   value 
         "Overall Total Calls:".
         05 filler                     pic x(36)   value spaces.
         05 ws-tl-all-calls            pic zzzz9   value 0.

      *This is for calculating total of totals, average and remainder. 
       01 ws-call-records.
         05 ws-call-data                           occurs 12 times .
           10 ws-calc-total-calls      pic 9(5)    value 0.
           10 ws-calc-count-calls      pic 999     value 0.
           10 ws-calc-avg-calls        pic 999v99  value 0.
           10 ws-total-total-calls     pic 9(5)v99 value 0.

       77 ws-end-of-file-flag          pic x       value "N".

       77 ws-sub                       pic 99      value 0.

       77 ws-temp-calls-total          pic 9(4)    value 0. 

       77 ws-calls-avg                 pic 999     value 0.

       77 ws-calls-rem                 pic 999     value 0.

       77 ws-calc-count                pic 99      value 0.

       77 ws-calls-avg-calc            pic 999v9.

       77 ws-no-call-caounter          pic 9(4)    value 0.

       77 ws-no-call-record-count      pic 9(4)    value 0.

       77 ws-no-call-months            pic 9(4)    value 0.

       77 ws-highest-month-avg-op      pic 9(4)    value 0.

       77 ws-lowest-month-avg-op       pic 9(4)    value 100.

       77 ws-highest-month-avg         pic 9(4)    value 0.

       77 ws-total-of-total            pic 9(8)    value 0.

       77 ws-total-of-avg              pic 9(8)    value 0.

       77 ws-total-of-rem              pic 9(8)    value 0.
      *
       procedure division.
       000-Main.
      *
      *    perform 999-test-headings.
      * open files
           open output report-file.
           open input input-file

           read input-file 
           at end move "Y" to ws-end-of-file-flag.


      * output heading

           write print-line from ws-name-line.
           write print-line from ws-report-heading
           after advancing 1 lines.
           write print-line from ws-heading-line1
           after advancing 2 lines.
           write print-line from ws-heading-line2.

      * process input file & output results
           perform 20-process-lines
             until ws-end-of-file-flag equals "Y".
      * output total lines
        

           perform 60-calculate-operatores.
           write print-line from ws-ops-line
           after advancing 1 lines.
           move ws-total-of-total to ws-tl-total.
           move ws-total-of-avg to ws-tl-avg.
           move ws-total-of-rem to ws-tl-rem.
           write print-line from ws-total-line
           after advancing 2 line. 
           write print-line from ws-average-line
            after advancing 2 line.
           move ws-no-call-record-count to ws-tl-zero-calls.
           write print-line from ws-total-line-no-calls
           after advancing 2 line. 
           move ws-no-call-months to ws-tl-zero-mths
           write print-line from ws-total-line-no-calls-months
           after advancing 2 lines. 
           move ws-highest-month-avg to ws-tl-h-mth-avg.
           write print-line from ws-total-line-highest-month-average
           after advancing 2 lines.
           move ws-highest-month-avg-op to ws-tl-h-op-avg.
           write print-line from ws-tl-highest-operator-average
           after advancing 2 lines. 
           move ws-lowest-month-avg-op to ws-tl-l-op-avg.
           write print-line from ws-tl-lowest-operator-average
           after advancing 2 lines. 
           move ws-total-of-total to ws-tl-all-calls.
           write print-line from ws-total-line-overall
           after advancing 2 lines. 
      * close files
           close input-file, report-file.

           goback.

      *Processing lines from the input file and making calculations for 
      *summary line
       20-process-lines. 
           
          move 0 to ws-temp-calls-total.
           move 0 to ws-calc-count.
           move 0 to ws-no-call-caounter.
          
           PERFORM 
               varying ws-sub
               from 1 by 1
               until ws-sub > ws-number-of-months

               add emp-rec-calls(ws-sub) to ws-temp-calls-total
               
               add emp-rec-calls(ws-sub) to ws-calc-total-calls(ws-sub)

               add emp-rec-calls(ws-sub) to ws-calc-avg-calls(ws-sub)
               
               add ws-temp-calls-total to ws-total-total-calls(ws-sub)
               if emp-rec-calls(ws-sub) is not equal 0
                   add 1 to ws-calc-count
                   add 1 to ws-calc-count-calls(ws-sub)
               end-if

               if emp-rec-calls(ws-sub) is equal 0
                  add 1 to ws-no-call-caounter
                  add 1 to ws-no-call-months
               end-if

           END-PERFORM.


           add ws-temp-calls-total to ws-total-of-total.
           if(ws-no-call-caounter = 12)
               add 1 to ws-no-call-record-count
           end-if.

           divide ws-temp-calls-total by ws-calc-count giving 
           ws-calls-avg remainder ws-calls-rem.

           add ws-calls-avg to ws-total-of-avg.
           add ws-calls-rem to ws-total-of-rem.

           if(ws-calls-avg > ws-highest-month-avg-op)
               move emp-rec-num to ws-tl-h-op-num
               move ws-calls-avg to ws-highest-month-avg-op
           end-if.
           if(ws-calls-avg is not equal 0)
               if (ws-calls-avg < ws-lowest-month-avg-op)
                   move emp-rec-num to ws-tl-l-op-num
                   move ws-calls-avg to ws-lowest-month-avg-op
               end-if
           end-if.
          

           move spaces to print-line.

           move emp-rec-num to ws-dl-num.
           move emp-rec-name to ws-dl-name.
           PERFORM 
               varying ws-sub
               from 1 by 1
               until ws-sub > ws-number-of-months

               move emp-rec-calls(ws-sub) to ws-dl-months(ws-sub)
               
           END-PERFORM.
           move ws-temp-calls-total to ws-dl-total.
           if(ws-temp-calls-total is equal 0)
               move "ZERO" to ws-dl-avg-text
           else
               move ws-calls-avg to ws-dl-avg
           end-if.
           move ws-calls-rem to ws-dl-rem.

           write print-line from ws-detail-line . 

           read input-file
               at end
                   move "Y" to ws-end-of-file-flag.
           

      *    Calculating summary of totals, operators and average
        60-calculate-operatores. 
           move spaces to print-line.

           PERFORM 
               varying ws-sub
               from 1 by 1
           until ws-sub > ws-number-of-months
              move ws-calc-total-calls(ws-sub) to ws-tl-months(ws-sub)
               move ws-calc-count-calls(ws-sub) to ws-ol-months(ws-sub)
             compute ws-calls-avg-calc =  ws-calc-total-calls(ws-sub)/  
                 ws-calc-count-calls(ws-sub)
             move ws-calls-avg-calc to
                 ws-al-months(
               ws-sub)

             if(ws-calls-avg-calc > ws-highest-month-avg)
                 move ws-month-literals(ws-sub) to ws-tl-h-mth-nam
                 move ws-calls-avg-calc to ws-highest-month-avg
             end-if

           END-PERFORM.
       end program A7-CallCenterOpReport.