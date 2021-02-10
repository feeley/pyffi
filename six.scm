(define-type language
  infix-unary-ops
  infix-binary-ops
  convert-identifier
  convert-literal)

(define (convert expr language)
  (define (op operator) (list-ref operator 1))
  (define (precedence operator) (list-ref operator 2))
  (define (associativity operator) (list-ref operator 3))
  (define (parens expr optional?) (if optional? expr (list "(" expr ")")))
  (define (parens-optional? pos inner-op outer-op)
    (or (< (precedence inner-op) (precedence outer-op))
        (and (= (precedence inner-op) (precedence outer-op))
             (= pos (associativity inner-op)))))
  (define (infix expr pos outer-op)
    (let ((head (car expr)))
      (case head
        ((six.identifier)
         ((language-convert-identifier language) (cadr expr)))
        ((six.literal)
         ((language-convert-literal language) (cadr expr)))
        ((six.prefix) (eval expr))
        (else
         (cond ((assq head (language-infix-unary-ops language)) =>
                (lambda (inner-op)
                  (parens
                   (list (op inner-op)
                         (infix (list-ref expr 2) 1 inner-op))
                   (parens-optional? pos inner-op outer-op))))
               ((assq head (language-infix-binary-ops language)) =>
                (lambda (inner-op)
                  (parens
                   (list (infix (list-ref expr 1) 0 inner-op)
                         (op inner-op)
                         (infix (list-ref expr 2) 1 inner-op))
                   (parens-optional? pos inner-op outer-op))))
               (else
                (error "unknown operator" head)))))))
  (infix (cadr expr) 0 '(_ _ 9999 0))) ;; pretend wrapped in infinitely low precedence operator to prevent outer level of parentheses

(define python
  (make-language
   '((six.+x    "+"    1 0)  ;; last value is associativity: 0 -> LR, 1 -> RL
     (six.-x    "-"    1 0))
   '((six.x+=y  "+="  15 1)  ;; RL associativity
     (six.x-=y  "-="  15 1)
     (six.x*=y  "*="  15 1)
     (six.x/=y  "/="  15 1)
     (six.x**=y "**=" 15 1)
     (six.x+y   "+"    4 0)
     (six.x-y   "-"    4 0)
     (six.x*y   "*"    3 0)
     (six.x/y   "/"    3 0)
     (six.x**y  "**"   2 1))  ;; RL associativity
   (lambda (ident)
     (symbol->string ident))
   (lambda (val)
     (cond ((number? val)
            (number->string val))
           ((boolean? val)
            (if val "True" "False"))
           ((string? val)
            (object->string val))
           (else
            (error "unknown literal" val))))))

(define-syntax six.infix
  (lambda (src)

    (define (tree-to-string tree)
      (call-with-output-string (lambda (p) (print port: p tree))))

    (define (else-handler sexp)
      (let* ((tree (convert sexp python))
             (code (tree-to-string tree)))
        `(py-eval ,code)))

    (define (import-handler sexp)
      ;; NOTE: convert expects (list foo bar . baz)
      ;;  and we choose six.infix as car for continuity
      (let ((module (tree-to-string (convert (list 'six.infix (cadadr sexp)) python))))
        `(py-import ,module)))

    ;; TODO: from-import-handler

    (let ((sexp (##desourcify src)))
      (case (caadr sexp)
        ((six.import)
         (import-handler sexp))
        (else
         (else-handler sexp))))))
