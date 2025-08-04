#lang racket/base

(require roos)
(require "main.rkt")

(provide ini
         (all-from-out roos))

(define-syntax i-set!
  (syntax-rules ()
    ((_ a b)
     (set! a b))))

(def-roos (ini . _file) this (supers)
  (file* (if (null? _file) #f (get-ini-file (car _file))))
  (content (if (not (eq? file* #f))
               (if (file-exists? file*)
                   (file->ini file*)
                   (make-ini))
               (make-ini)))
  (fail #f)

  ((file) file*)
  ((file! f) (i-set! file* f) (-> this reload))

  ((reload) (i-set! content
                  (if (not (eq? file* #f))
                      (if (file-exists? file*)
                          (file->ini file*)
                          (make-ini))
                      (make-ini))))
  
  ((set! s k v)
   (begin
     (ini-set! content s k v)
     (if (eq? file* #f)
         (if fail
             (error "ini: No filename set, cannot write ini file after set!")
             #f)
         (ini->file content file*))
     this))
  
  ((get s k . def)
   (let ((def-val (if (null? def) '@@no-value@@ (car def))))
     (let ((r (ini-get content s k def-val)))
       (if fail
           (if (eq? r '@@no-value@@)
               (error (string-append
                       "ini: No default value set and no value found for section '"
                       (symbol->string s)
                       "' and key '"
                       (symbol->string k)
                       "'"))
               r)
           (if (eq? r '@@no-value@@)
               #f
               r)))))
  )
