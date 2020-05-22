#lang scheme
;2017400093

(define (get List Poz)
  (if(= Poz 1) (car List) (get (cdr List) (- Poz 1)) ))

(define (create num)
  (if (> num 0) (cons 0 (create(- num 1))) '() ))

(define (size List)
  (if(null? (cdr List)) 1 (+ 1 (size (cdr List))) ))

(define (mycadr List)
  (if(null? (cdr List)) '() (cadr List) ))

(define (allzero List)
  (if (null? List) #t (and (= 0 (car List)) (allzero (cdr List))) ))

(define (same L1 L2)
  (if (null? L1) #t (and (member (car L1) L2) (same (cdr L1) L2)) ))


(define (TENTS-SOLUTION input )
  (cond ((and (null? (caddr input))(and (allzero (car input))(allzero(cadr input))) ) '())
        ((null? (caddr input)) #f)
        (else ((lambda (X input)(parser X (car X) '() (create (size (car input)))(create (size (cadr input))) (car input) (cadr input))) (listmaker(caddr input)) input  ) )
        )
  )


(define (listmaker Trees)
  (if (null? (cdr Trees)) (list (NEIGHBOR-LIST (car Trees))) (cons (NEIGHBOR-LIST (car Trees)) (listmaker (cdr Trees)))) 
  )


(define (parserSlow ListofList List Final Vhis Hhis Vlist Hlist)
  (cond ((null? ListofList) (reverse Final))
        ((null? List) #f)
        ((and  (valid Final  (car List) Vhis Hhis Vlist Hlist ) (parser (cdr ListofList)(mycadr ListofList)(append Final (list (car List)))(INC-NTH Vhis (car (car List)) ) (INC-NTH Hhis (cadr(car List)) ) Vlist Hlist)) (parser (cdr ListofList)(mycadr ListofList)(append Final (list (car List)))(INC-NTH Vhis (car(car List)) ) (INC-NTH Hhis (cadr(car List)) ) Vlist Hlist))
        (else (parser ListofList (cdr List) Final Vhis Hhis Vlist Hlist))
        )
  )

(define (parser ListofList List Final Vhis Hhis Vlist Hlist)
  (cond ((null? ListofList) (if (and (equal? Vhis Vlist)(equal? Hhis Hlist)) Final #f))
        ((null? List) #f)
        (else (if (valid Final  (car List) Vhis Hhis Vlist Hlist )((lambda (X ListofList List Final Vhis Hhis Vlist Hlist )
                (if X X (parser ListofList (cdr List) Final Vhis Hhis Vlist Hlist)  ) )
              (parser (cdr ListofList)(mycadr ListofList)(append Final (list (car List)))(INC-NTH Vhis (car(car List)) ) (INC-NTH Hhis (cadr(car List)) ) Vlist Hlist) ListofList List Final Vhis Hhis Vlist Hlist)
                  (parser ListofList (cdr List) Final Vhis Hhis Vlist Hlist)

                  )
              )
        )
  )


(define (valid Final Element Vhis Hhis Vlist Hlist)
  (cond ((or (< (car Element) 1) (< (cadr Element) 1)(> (car Element) (size Vlist) )(> (cadr Element) (size Hlist) )) #f)
        ((ADJACENT-WITH-LIST Element Final) #f )
        ((> (+ 1 (get Vhis (car Element))) (get Vlist (car Element) )) #f)
        ((> (+ 1 (get Hhis (cadr Element))) (get Hlist (cadr Element) )) #f)
        (else #t)
        )
  )




(define (INC-NTH List Poz)
  (if(= Poz 1) (cons (+ 1 (car List)) (cdr List)) (cons (car List) (INC-NTH (cdr List) (- Poz 1)))))


(define (REPLACE-NTH List Poz Num)
  (if(= Poz 1) (cons Num (cdr List)) (cons (car List) (REPLACE-NTH (cdr List) (- Poz 1) Num))))

(define (RETURN-FIRST-NOT-FALSE Func List)
  (if (null? List) #f (
        (lambda (x func list)(if x x (RETURN-FIRST-NOT-FALSE func (cdr list)) ))
           (Func (car List)) Func List ) ) )
                    

(define (ADJACENT L1 L2)
  (if ( < (+ (* (- (car L1) (car L2)) (- (car L1) (car L2)) )
         (* (- (cadr L1) (cadr L2)) (- (cadr L1) (cadr L2)) )) 3) #t #f))

(define (ADJACENT-WITH-LIST Poz List)
  (if (null? List) #f (or (ADJACENT Poz (car List)) (ADJACENT-WITH-LIST Poz (cdr List)) )))

(define (NEIGHBOR-LIST Poz)
  (list (list (car Poz) (+ (cadr Poz) 1))
        (list (car Poz) (- (cadr Poz) 1))
        (list (+ (car Poz) 1) (cadr Poz))
        (list (- (car Poz) 1) (cadr Poz)      ) ) )



