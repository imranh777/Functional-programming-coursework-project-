;#################################################################################################
;# 
;#  COMP1811 - CW2 Outlook Simulator in Scheme                                                        
;#             REPL based program to manage all OutlookSim operations via command-line
;#
;#  Your full name:
;#  Partner A: <Full name as appears on Moodle>, SID<student ID> Ruhal amin 001507871                             
;#  Partner B: <Full name as appears on Moodle>, SID<student ID> Imran Hussain 001223419
;# 
;#################################################################################################

(require racket/trace)

;;Mailbox
(define mb   ;mailbox
 '(;ID From To Date Subject Tag Body Flag Read
   (0  Aniket1@gre.ac.uk    Yasmine8@gre.ac.uk   (11 1 2025) "Aniket s1"  tag0 (("Aniket Yasmine8." "Mail message urgent 1 11_234.")) #f #f)
   (1  Ehsan2@gre.ac.uk     Sanyaade7@gre.ac.uk  (22 2 2025) "Ehsan s1"   tag0 (("Ehsan Sanyaade7." "Message for review 1 22_568."))  #f #t)
   (2  Margarita3@gre.ac.uk Rafael6@gre.ac.uk    (3 3 2025)  "Marg. s1"   tag3 (("Margarita Rafael6." "Please check email 33+234." "Deadline 10/4/2025.")) #t #f)
   (3  Tuan4@gre.ac.uk      Mobolaji5@gre.ac.uk  (4 4 2025)  "Tuan s1"    tag1 (("Tuan Mobolaji5." "Mail body 1 41_234.")) #f #f)
   (4  Mobolaji5@gre.ac.uk  Tuan4@gre.ac.uk      (5 5 2025)  "Mobolajis1" tag3 (("Mobolaji Tuan4.") ()) #f #f)
   (5  Rafael6@gre.ac.uk    Valdimars@gre.ac.uk  (6 6 2025)  "Rafael s1 - urgent response required!"  tag1 (("Rafael Valdimars." "Mail message urgent 1 61_234.") ("Please respond by 10/6/205." "Concerning CW2")) #f #f)
   (6  Sanyaade7@gre.ac.uk  Ehsan2@gre.ac.uk     (7 7 2025)  "Sany. s1"   tag2 (("Sanyaade Ehsan2.")) #t #f)
   (7  Yasmine8@gre.ac.uk   Aniket1@gre.ac.uk    (8 8 2025)  "Yasmines1: response needed by 12/3/2026."  tag2 (("YasmineAniket1.") ("This is a confidential message Re email sent 1/8/2025:" "can you provide the missing documentation." "Your signature is required for doc2 and doc5.") ("Please forward all the documentation by 12/3/26 at the latest." "Doc2 and doc ahouls be password protected.")) #f #t)
   (8  Aniket1@gre.ac.uk    Ehsan2@gre.ac.uk     (11 1 2025) "Anikets2"   tag3 (("Aniket Ehsan2." "SECOND REMINDER") ("re email sent 1/1/2025:" "review request for SI1529873." "Please provide a summary review by 12/1/2025.") ("Reviews should use the RD6 form and sent qr45@gre.ac.uk on completion." "Your comments will be shared with the authors.")) #t #t)
   (9  Aniket1@gre.ac.uk    Margarita3@gre.ac.uk (4 4 2025)  "Aniket s3"  tag1 (("Aniket Margarita3.") ("Mail body 3 41_234.")) #f #f)
   (10 Ehsan2@gre.ac.uk     Moeen9@gre.ac.uk     (4 4 2025)  "Ehsan s2"   tag1 (("Ehsan Moeen9.") ("1st reminder." "Mail body 2 68! 754.") ()) #f #f)
  )
)

; Replace the value at a given position (list index) with the new value in the given email list
; (replace-value 4 "New subject" (get-email 0 mb))
(define (replace-value pos new-val e-lst)
  (cond
    [(null? e-lst) '()]                       
    [(= pos 0) (cons new-val (cdr e-lst))]    
    [else (cons (car e-lst) (replace-value (- pos 1) new-val (cdr e-lst)))] ))

; replaces an enire existing email in the given mailbox
(define (replace-email old new mb-lst)
  (cond [(null? mb-lst) mb-lst]
        [(equal? (car mb-lst) old) (cons new (cdr mb-lst))]
        [else (cons (car mb-lst) (replace-email old new (cdr mb-lst)))] ))

;; PARTNER A
;; FA1
(define (get-email id mb-lst)
  ())
  
;; FA2
(define (del-email id mb-lst)
  ())
  
;; FA3
(define (filter-frm frm mb-lst)
  ())
  
;; FA4
(define (sort-by-frm mb-lst) ;sort by sender in accending order
  ())

;; FA5
(define (encrypt bdy) ; MODIFIED - now takes an email body message 
  ())                 ;            and returns a new body with the required encrypted secret code


;; PARTNER B
;; FB1
(define (mark id type mb-lst)
  (cond
     [(null? mb-lst) '()]
    (= id (car (car mb-lst))) (car mb-lst)
     (else
      (cons
       (car mb-lst)
       (mark id (cdr mb-lst))))))
      
  
;; FB2
(define (mv-email id tag mb-lst)
  (cond
    [(null? mb-lst) '()]
    
    (= id (car (car mb-lst)))
     (cons
      (append 5 tag (car mb-lst))
      (cdr mb-lst))]
    
    (else
     (cons
      (car mb-lst)
      (mv-email id tag (cdr mb-lst)))]))




;; FB3
(define (find-by-date date mb-lst)
   (filter (lambda (e) (equal? date (list-ref e 3))) mb-lst))
  

  
;; FB4
(define (sort-by-to mb-lst) ; sort by recipient in decending order
  (cond
    [(null? sorted-mb) (list email)] ; base: empty sorted list
    [(string>? (symbol->string (list-ref email 2))
               (symbol->string (list-ref (car sorted-mb) 2)))
     (cons email sorted-mb)] 
    [else
     (cons (car sorted-mb) (insert-email email (cdr sorted-mb)))]))

;; Sort the mailbox recursively by sender (From field)
(define (sort-by-frm mb-lst)
  (if (null? mb-lst)
      '()
      (insert-email (car mb-lst) (sort-by-frm (cdr mb-lst)))))


  
;; FB5
(define (add-stats bdy) ; MODIFIED - now only takes an email body message 
    (let* (
         ;; Count number of paragraphs
         (p (length bdy))

         ;; Count total number of sentences
         (s (apply + (map length bdy)))

         ;; Create statistics list
         (stats (list 'Stats:
                      (list "P count:" p)
                      (list "S count:" s)))
        )
    ;; Insert the statistics at the beginning of the body
    (cons stats bdy)))
                   ;            and returns a new body with the stats inserted


;; Partners A&B
;; FA&FB6 
(define (add-email frm to date subject tag body mb-lst)
  (let* (
         ;; Generate new ID: use length of mailbox
         (new-id (length mb-lst))

         ;; Process body based on tag
         (new-body (cond
                     [(eq? tag 'conf) (encrypt body)]
                     [(eq? tag 'prsnl) (add-stats body)]
                     [else body]))

         ;; Create new email entry
         (new-email (list new-id frm to date subject tag new-body 0 #f))
        )
    ;; Add new email to end of mailbox
    (append mb-lst (list new-email))))

;FB.6
(define (add-email frm to date subject tag body mb-lst)
  (let* (
         ;; Generate new ID
         (new-id (if (null? mb-lst)
                  0
                  (+ 1 (apply max (map car mb-lst)))))
        ;; Encrypt body if the email is confidential ('conf)
         (new-body (cond
                     [(eq? tag 'conf) (encrypt body)]
                     [(eq? tag 'prsnl) (add-stats body)]
                     [else body]))

         ;; Create new email entry
         (new-email (list new-id frm to date subject tag new-body 0 #f)))
    ;; Add new email to end of mailbox
    (append mb-lst (list new-email))))




;;
;;You should include code to execute each of your functions below.
;; Examples (test your code thoroughly using different arguments):
'A1
(get-email 1 mb)
'A2
(del-email 1 mb)
'A3
(filter-frm 'Ehsan2@gre.ac.uk mb)
'A4
(sort-by-frm mb)
'A5
(encrypt '(("Body new" "L2.") ("Conf 1811.A") ("P3" "L2" "L3" "L4")))
'B1
(mark 1 'flag mb)
'B2
(mv-email 1 'inbox mb)
'B3
(find-by-date '(3 3 2025) mb)
'B4
(sort-by-to mb)
'B5
(add-stats '(("Body 6")("Sany Ehsan2.")))

'A&B6
(add-email 'Kim9@gre.ac.uk 'Zia10@gre.ac.uk '(10 2 2026) "Reminder 1!" 'inbox '(("Body msg new.")("Sam." "Zia." "Confirmed meeting")("Teams link to follow")) mb)




