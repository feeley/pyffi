(define-syntax six.infix
  (lambda (src)

    (include "six-convert.scm")

    ;; TODO: from-import-handler

    (##deconstruct-call
     src
     2
     (lambda (ast-src)
       (let ((ast (##source-strip ast-src)))
         (if (and (pair? ast)
                  (eq? 'six.import (##source-strip (car ast)))
                  (pair? (cdr ast))
                  (null? (cddr ast)))
             (let ((ident (##source-strip (cadr ast))))
               (if (and (pair? ident)
                        (eq? 'six.identifier (##source-strip (car ident)))
                        (pair? (cdr ident))
                        (null? (cddr ident)))
                   `(begin
                      (py-import ,(symbol->string (##source-strip (cadr ident))))
                      (void))
                   (error "invalid import")))

             (let* ((x (six->python ast-src))
                    (params (car x))
                    (body (cadr x))
                    (def
                     (string-append "def ___0("
                                    (flatten-string
                                     (comma-separated (map car params)))
                                    "):\n "
                                    body)))
               `((begin
                   (py-exec ,def)
                   (PyObject*->object (py-eval "___0")))
                 ,@(map cdr params)))))))))
