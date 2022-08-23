#lang racket/base

(provide grapheme-spans?)

;; Why `grapheme-spans?` takes only two (sub)strings: For non-empty
;; strings `a`, `b`, and `c`, it's not possible for a grapheme to span
;; all three parts in `(string-append a b c)` without a grapheme
;; spanning both parts in either `(string-append a b)` or
;; `(string-append b c)`.

(define (grapheme-spans? a-str a-start a-end  ; `a` = `(substring a-str a-start a-end)`
                         b-str b-start b-end) ; `b` = `(substring a-str a-start a-end)`
  ;; Mostly, to check whether appending `a` and `b`
  ;; creates a grapheme cluster that spans the boundary
  ;; between `a` and `b`, we can check the end of `a`
  ;; and the start of `b`. There are some cases that are
  ;; more complex, though.
  (cond
    [(= a-start a-end) #f]
    [(= b-start b-end) #f]
    [(char-extended-pictographic? (string-ref b-str b-start))
     ;; extends if preceded by ZWJ, then Extend any number, then extended pictographic
     (and (eq? 'ZWJ (char-grapheme-break-property (string-ref a-str (sub1 a-end))))
          (let loop ([i (- a-end 2)])
            (cond
              [(i . < . a-start) #f]
              [(char-extended-pictographic? (string-ref a-str i)) #t]
              [(eq? 'Extend (char-grapheme-break-property (string-ref a-str i)))
               (loop (sub1 i))])))]
    [(eq? 'Regional_Indicator (char-grapheme-break-property (string-ref b-str b-start)))
     (let loop ([i (- a-end 1)])
       (cond
         [(i . < . a-start) #f]
         [(eq? 'Regional_Indicator (char-grapheme-break-property (string-ref a-str i)))
          (let ([i (sub1 i)])
            (cond
              [(i . < . a-start) #t]
              [(eq? 'Regional_Indicator (char-grapheme-break-property (string-ref a-str i)))
               (loop (sub1 i))]
              [else #t]))]
         [else #f]))]
    [else
     ;; otherwise, pairwise works
     (define-values (consume? state) (char-grapheme-step (string-ref a-str (sub1 a-end)) 0))
     (cond
       [consume? #f]
       [else
        (define-values (consume? new-state) (char-grapheme-step (string-ref b-str b-start) state))
        (cond
          [consume? (eq? 0 new-state)]
          [else #t])])]))

(module+ test
  (define passed 0)
  (define (check* v1 v2 e)
    (unless (equal? v1 v2)
      (error "failed ~s" e))
    (set! passed (add1 passed)))
  (define-syntax-rule (check e1 e2)
    (check* e1 e2 '(check e1 e2)))

  (check #f (grapheme-spans? "a" 0 0
                             "\u300" 0 1)) ; Extend
  (check #f (grapheme-spans? "a" 0 1
                             "\u300" 0 0)) ; Extend

  (check #f (grapheme-spans? "a" 0 1
                             "b" 0 1))
  (check #t (grapheme-spans? "a\r" 0 2
                             "\nb" 0 2))
  (check #f (grapheme-spans? "a\r" 0 1
                             "\nb" 0 2))
  (check #f (grapheme-spans? "a\r" 0 2
                             "\nb" 1 2))

  (check #t (grapheme-spans? "a" 0 1
                             "\u300" 0 1)) ; Extend
  (check #f (grapheme-spans? "\u300" 0 1 ; Extend
                             "a" 0 1))
  (check #t (grapheme-spans? "a" 0 1
                             "\u200D" 0 1)) ; ZWJ
  (check #f (grapheme-spans? "\u200D" 0 1   ; ZWJ
                             "a" 0 1))

  (check #f (grapheme-spans? "a" 0 1
                             "\t" 0 1))
  (check #f (grapheme-spans? "\a" 0 1
                             "t" 0 1))

  (check #t (grapheme-spans? "\u1111" 0 1  ; L
                             "\u11A1" 0 1)) ; V

  (check #t (grapheme-spans? "\uA9" 0 1   ; extended pictographic
                             "\u300" 0 1)) ; Extend
  (check #t (grapheme-spans? "\uA9\u300" 0 2   ; extended pictographic + Extend
                             "\u301" 0 1)) ; Extend
  (check #t (grapheme-spans? "\uA9\u300\u200D" 0 3   ; extended pictographic + Extend + ZWJ
                             "\uA9" 0 1)) ; extended pictographic
  (check #f (grapheme-spans? "\uA9\u300\u200D" 0 3   ; extended pictographic + Extend + ZWJ
                             "a" 0 1))
  (check #t (grapheme-spans? "\uA9\u300\u301\u302\u200D" 0 5 ; extended pictographic + Extend* + ZWJ
                             "\uA9" 0 1))


  (check #t (grapheme-spans? "\U1F1E6" 0 1   ; RI
                             "\U1F1E6" 0 1)) ; RI
  (check #t (grapheme-spans? "a\U1F1E6" 0 2  ; RI
                             "\U1F1E6" 0 1)) ; RI
  (check #f (grapheme-spans? "\U1F1E6\U1F1E6" 0 2  ; RI+RI
                             "\U1F1E6" 0 1)) ; RI
  (check #f (grapheme-spans? "a\U1F1E6\U1F1E6" 0 3  ; RI+RI
                             "\U1F1E6" 0 1)) ; RI
  (check #f (grapheme-spans? "a\U1F1E6\U1F1E6" 0 3  ; RI+RI
                             "\U1F1E6\U1F1E6" 0 2)) ; RI+RI
  (check #f (grapheme-spans? "a\U1F1E6\U1F1E6" 0 3  ; RI+RI
                             "\U1F1E6" 0 1)) ; RI
  (check #f (grapheme-spans? "a\U1F1E6\U1F1E6" 0 3  ; RI+RI
                             "\U1F1E6x" 0 2)) ; RI

  (printf "~a passed\n" passed))
