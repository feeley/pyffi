;;;============================================================================

;;; File: "six-convert.scm"

;;; Copyright (c) 2020-2021 by Marc Feeley, All Rights Reserved.

;;;============================================================================

(define-type conversion-ctx
  operators
  parameters
  globals
  literal
  unsupported)

(define python-operators
  (list->table
   '(
     ;;                   _____precedence
     ;;                  / ____associativity (0=LR, 1=RL, 2=not assoc.)
     ;;                 / / ___operand count (-1 for postfix unary operator)
     ;;                / / / __target operator
     ;;               / / / /
     (six.prefix     0 0)
     (six.identifier 0 0)
     (six.literal    0 0)
     (six.list       0 0)
     (six.null       0 0)

     (six.index      1 0)
     (six.call       1 0)
     (six.dot        1 0)

     (six.x**y       3 1 2 "**") ;; note: RL associative

     (six.+x         4 1 1 "+") ;; note: RL associative
     (six.-x         4 1 1 "-")
     (six.~x         4 1 1 "~")

     (six.x*y        5 0 2 "*")
     (six.x@y        5 0 2 "@")
     (six.x/y        5 0 2 "/")
     (six.x//y       5 0 2 "//")
     (six.x%y        5 0 2 "%")

     (six.x+y        6 0 2 "+")
     (six.x-y        6 0 2 "-")

     (six.x<<y       7 0 2 "<<")
     (six.x>>y       7 0 2 ">>")

     (six.x&y        8 0 2 "&")

     (six.x^y        9 0 2 "^")

     (|six.x\|y|    10 0 2 "|")

     (six.x<y       11 2 2 "<") ;; note: not associative
     (six.x<=y      11 2 2 "<=")
     (six.x>y       11 2 2 ">")
     (six.x>=y      11 2 2 ">=")
     (six.x==y      11 2 2 "==")
     (six.x!=y      11 2 2 "!=")
     (six.xiny      11 2 2 " in ")
     (six.xisy      11 2 2 " is ")

     (six.notx      12 1 1 "not ") ;; note: RL associative

     (six.xandy     13 0 2 " and ")

     (six.xory      14 0 2 " or "))))

(define javascript-operators
  (list->table
   '(
     ;;                   _____precedence
     ;;                  / ____associativity (0=LR, 1=RL, 2=not assoc.)
     ;;                 / / ___operand count (-1 for postfix unary operator)
     ;;                / / / __target operator
     ;;               / / / /
     (six.prefix     0 0)
     (six.identifier 0 0)
     (six.literal    0 0)
     (six.list       0 0)
     (six.null       0 0)

     (six.index      1 0)
     (six.call       1 0)
     (six.dot        1 0)

;;     (six.x++        3 0 -1 "++")
;;     (six.x--        3 0 -1 "--")

     (six.+x         4 1 1 "+") ;; note: RL associative
     (six.-x         4 1 1 "-")
     (six.~x         4 1 1 "~")
     (six.++x        4 1 1 "++")
     (six.--x        4 1 1 "--")

     (six.x**y       5 1 2 "**") ;; note: RL associative

     (six.x*y        6 0 2 "*")
     (six.x/y        6 0 2 "/")
     (six.x%y        6 0 2 "%")

     (six.x+y        7 0 2 "+")
     (six.x-y        7 0 2 "-")

     (six.x<<y       8 0 2 "<<")
     (six.x>>y       8 0 2 ">>")
     (six.x>>>y      8 0 2 ">>>")

     (six.x<y        9 0 2 "<")
     (six.x<=y       9 0 2 "<=")
     (six.x>y        9 0 2 ">")
     (six.x>=y       9 0 2 ">=")
     (six.xiny       9 0 2 " in ")

     (six.x==y      10 0 2 "==")
     (six.x!=y      10 0 2 "!=")
     (six.x===y     10 0 2 "===")
     (six.x!==y     10 0 2 "!==")

     (six.x&y       11 0 2 "&")

     (six.x^y       12 0 2 "^")

     (|six.x\|y|    13 0 2 "|")

     (six.x&&y      14 0 2 "&&")

     (|six.x\|\|y|  15 0 2 "||")

;;     (six.x?y:z     17 1) ;; note: RL associative

     (six.x=y       18 1 2 "=") ;; note: RL associative
     (six.x+=y      18 1 2 "+=")
     (six.x-=y      18 1 2 "-=")
     (six.x**=y     18 1 2 "**=")
     (six.x*=y      18 1 2 "*=")
     (six.x/=y      18 1 2 "/=")
     (six.x%=y      18 1 2 "%=")
     (six.x<<=y     18 1 2 "<<=")
     (six.x>>=y     18 1 2 ">>=")
     (six.x>>>=y    18 1 2 ">>>=")
     (six.x&=y      18 1 2 "&=")
     (six.x^=y      18 1 2 "^=")
     (|six.x\|=y|   18 1 2 "|=")
     (six.x&&=y     18 1 2 "&&=")
     (six.x||=y     18 1 2 "||=")

;;     (six.x,=y      20 0 2 ",")
)))

(define (six->python ast-src)

  (define (convert-literal cctx src)
    (##deconstruct-call
     src
     2
     (lambda (val-src)
       (let ((val (##source-strip val-src)))
         (cond ((number? val)
                (number->string val)) ;; TODO: use Python number syntax
               ((boolean? val)
                (if val "True" "False"))
               ((string? val)
                (object->string val)) ;; TODO: use Python string syntax
               (else
                (unsupported cctx src)))))))

  (define (unsupported cctx src)
    (##raise-expression-parsing-exception
     'ill-formed-expression
     src))

  (define cctx
    (make-conversion-ctx
     python-operators
     '()
     (make-table)
     convert-literal
     unsupported))

  (let ((target-expr (six-expression-to-infix cctx ast-src)))
    (list (reverse (conversion-ctx-parameters cctx))
          (flatten-string (list "return " target-expr)))))

(define (six->javascript ast-src)

  (define (convert-literal cctx src)
    (##deconstruct-call
     src
     2
     (lambda (val-src)
       (let ((val (##source-strip val-src)))
         (cond ((number? val)
                (number->string val)) ;; TODO: use Python number syntax
               ((boolean? val)
                (if val "true" "false"))
               ((string? val)
                (object->string val)) ;; TODO: use Python string syntax
               (else
                (unsupported cctx src)))))))

  (define (unsupported cctx src)
    (##raise-expression-parsing-exception
     'ill-formed-expression
     src))

  (define cctx
    (make-conversion-ctx
     javascript-operators
     '()
     (make-table)
     convert-literal
     unsupported))

  (let ((target-expr (six-expression-to-infix cctx ast-src)))
    (list "function ___fn("
          (append-strings (map car (conversion-ctx-parameters cctx)) ",")
          ") { return "
          target-expr
          "; }")))

(define (six-expression-to-infix cctx ast-src)

  (define (unsupported ast-src)
    ((conversion-ctx-unsupported cctx) cctx ast-src))

  (define (precedence op) (car op))
  (define (associativity op) (cadr op))

  (define (parens-optional? pos inner-op outer-op)
    (let ((inner-prec (precedence inner-op))
          (outer-prec (precedence outer-op)))
      (or (< inner-prec outer-prec)
          (and (= inner-prec outer-prec)
               (let ((inner-assoc (associativity inner-op)))
                 (and (< inner-assoc 2) ;; 2 = non associative
                      (= inner-assoc pos)))))))

  (define (infix ast-src pos outer-op)
    (let ((ast (##source-strip ast-src)))
      (if (not (pair? ast))
          (unsupported ast-src)
          (let* ((head
                  (##source-strip (car ast)))
                 (rest
                  (cdr ast))
                 (inner-op
                  (table-ref (conversion-ctx-operators cctx) head #f)))
            (if (not inner-op)
                (unsupported ast-src)
                (let* ((x
                        (cddr inner-op))
                       (expr
                        (if (not (pair? x))
                            (infix* ast-src pos inner-op)
                            (let ((operand-count (car x))
                                  (target-op (cadr x)))
                              (case (length rest)
                                ((0)
                                 target-op)
                                ((1)
                                 (list target-op
                                       (infix (car rest) 1 inner-op)))
                                ((2)
                                 (list (infix (car rest) 0 inner-op)
                                       target-op
                                       (infix (cadr rest) 1 inner-op)))
                                ((3)
                                 ...) ;; TODO: ternary operator
                                (else
                                 (unsupported ast)))))))
                  (if (parens-optional? pos inner-op outer-op)
                      expr
                      (list "(" expr ")"))))))))

  (define (infix* ast-src pos inner-op)
    (let ((ast (##source-strip ast-src)))
      (case (##source-strip (car ast))

        ((six.prefix)
         (##deconstruct-call
          ast-src
          2
          (lambda (expr-src)
            (let* ((params
                    (conversion-ctx-parameters cctx))
                   (param-id
                    (string-append "___"
                                   (number->string (+ 1 (length params))))))
              (conversion-ctx-parameters-set!
               cctx
               (cons (cons param-id expr-src)
                     params))
              param-id))))

        ((six.identifier)
         (##deconstruct-call
          ast-src
          2
          (lambda (ident-src)
            (let* ((ident-sym (##source-strip ident-src))
                   (ident (symbol->string ident-sym)))
              (table-set! (conversion-ctx-globals cctx) ident-sym ident)
              ident))))

        ((six.literal)
         ((conversion-ctx-literal cctx) cctx ast-src))

        ((six.list six.null)
         (let loop ((ast-src ast-src) (rev-elems '()))
           (let ((ast (##source-strip ast-src)))
             (case (##source-strip (car ast))
               ((six.list)
                (##deconstruct-call
                 ast-src
                 3
                 (lambda (head-src tail-src)
                   (loop tail-src (cons (cvt head-src) rev-elems)))))
               ((six.null)
                (##deconstruct-call
                 ast-src
                 0
                 (lambda ()
                   (list "["
                         (comma-separated (reverse rev-elems))
                         "]"))))
               (else
                (unsupported ast-src))))))

        ((six.index)
         (##deconstruct-call
          ast-src
          3
          (lambda (obj-src index-src)
            (list (infix obj-src 0 inner-op)
                  "["
                  (cvt index-src)
                  "]"))))

        ((six.call)
         (##deconstruct-call
          ast-src
          -2
          (lambda (fn-src . args-src)
            (let ((args (map cvt args-src)))
              (list (infix fn-src 0 inner-op)
                    "("
                    (comma-separated args)
                    ")")))))

        ((six.dot)
         (##deconstruct-call
          ast-src
          3
          (lambda (obj-src attr-src)
            ;; TODO: check that attr-src is (six.identifier ...)
            (##deconstruct-call
             attr-src
             2
             (lambda (ident-src)
               (list (infix obj-src 0 inner-op)
                     "."
                     (symbol->string (##source-strip ident-src))))))))

        (else
         (unsupported ast-src)))))

  (define (cvt ast-src)
    ;; pretend wrapped in infinitely low precedence operator to prevent
    ;; outer level of parentheses
    (infix ast-src 0 '(9999 0)))

  (cvt ast-src))

(define (comma-separated lst)
  (if (pair? lst)
      (cons (car lst)
            (map (lambda (x) (list "," x)) (cdr lst)))
      ""))

(define (flatten-string x)
  (call-with-output-string (lambda (p) (print port: p x))))

