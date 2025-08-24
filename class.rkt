#lang racket/base

(require racket/class)
(require "main.rkt")

(provide ini%
         (all-from-out racket/class))

(define-syntax i-set!
  (syntax-rules ()
    ((_ a b)
     (set! a b))))



(define ini%
  (class object%
    (init-field [file #f] [fail #f])
    
    (define content #f)

    (define/public (get-file)
      file)

    (define/public (set-file! f)
      (i-set! file (get-ini-file f))
      this)

    (define/public (set-fail! f)
      (i-set! fail f)
      this)

    (define/public (get-fail)
      fail)

    (define/public (get-contents)
      content)

    (define/public (contents)
      content)

    (define/public (reload)
      (i-set! content (if (eq? file #f)
                        (make-ini)
                        (if (file-exists? file)
                            (file->ini file)
                            (make-ini))))
      this)

    (define/public (set! section key value)
      (begin
        (ini-set! content section key value)
        (if (eq? file #f)
            (when fail
                (error "ini: No filename set, cannot write ini after set!"))
            (ini->file content file))
        this))

    (define/public (get section key . default-value)
      (let* ((rnd (string->symbol (format "@@no-value-~a-@@" (random 400000000))))
             (def-val (if (null? default-value) rnd (car (default-value)))))
        (let ((r (ini-get content section key def-val)))
          (when fail
            (when (eq? r rnd)
              (error (string-append
                       "ini: No default value set and no value found for section '"
                       (symbol->string section)
                       "' and key '"
                       (symbol->string key)
                       "'"))
              )
            )
          (if (eq? r rnd)
              #f
              r))))

    (super-new)
    
    (begin
      (if (eq? file #f)
          (i-set! content (make-ini))
          (begin
            (i-set! file (get-ini-file file))
            (if (file-exists? file)
                (i-set! content (file->ini file))
                (i-set! content (make-ini)))))
      )

    )
  )
              
          