#lang racket/base

(require racket/string)
(require racket/file)

(provide file->ini
         ini->file
         ini-get
         ini-set!
         make-ini
         )

(define (ini->file ini file)
  (let ((out (open-output-file file #:exists 'replace)))
    (let ((last-is-newline #f))
      (for-each (lambda (section)
                  (let ((section-name (car section)))
                    (if (eq? section-name 'nil)
                        #t
                        (begin
                          (unless last-is-newline (newline out))
                          (display "[" out)
                          (display section-name out)
                          (display "]" out)
                          (newline out)))
                    (let ((lines (cdr section)))
                      (for-each (lambda (line)
                                  (if (eq? (car line) 'comment)
                                      (begin
                                        (set! last-is-newline #f)
                                        (display "; " out)
                                        (display (cadr line) out)
                                        (newline out))
                                      (if (eq? (car line) 'empty)
                                          (begin
                                            (newline out)
                                            (set! last-is-newline #t))
                                          (if (eq? (car line) 'keyval)
                                              (begin
                                                (set! last-is-newline #f)
                                                (display (cadr line) out)
                                                (display "=" out)
                                                (display (caddr line) out)
                                                (newline out))
                                              (error "Unknown line format")))))
                              lines))))
                (mcdr ini)))
    (close-output-port out)))

(define (make-ini)
  (mcons 'ini (list)))

(define (interpret s)
  (let ((re-num #px"^([+]|[-])?([0-9]+([.]([0-9]+))?)$")
        (re-bool #px"^(#f|#t|true|false)$"))
    (let ((ss (string-downcase (string-trim s))))
      (let ((m-num (regexp-match re-num ss)))
        (if m-num
            (string->number (car m-num))
            (let ((m-b (regexp-match re-bool ss)))
              (if m-b
                  (if (or (string=? ss "#t") (string=? ss "true"))
                      #t
                      #f)
                  s)))))))

(define (file->ini file)
  (let* ((lines (file->lines file))
         (re-section #px"^\\[([a-zA-Z0-9_-]+)\\]$")
         (re-keyval #px"^([a-zA-Z0-9_-]+)[=](.*)$")
         (re-comment #px"^[;](.*)$"))
    (letrec ((f (lambda (sections section lines)
                  (if (null? lines)
                      (append sections (list section))
                      (let* ((line (string-trim (car lines)))
                             (empty (string=? line ""))
                             (m-comment (regexp-match re-comment line))
                             (m-keyval (regexp-match re-keyval line))
                             (m-section (regexp-match re-section line)))
                        (if empty
                            (f sections (append section (list (list 'empty))) (cdr lines))
                            (if m-comment
                                (f sections (append section (list (list 'comment (cadr m-comment)))) (cdr lines))
                                (if m-keyval
                                    (f sections (append section
                                                        (list
                                                         (list 'keyval (string->symbol
                                                                        (string-trim (string-downcase
                                                                                      (cadr m-keyval))))
                                                               (interpret (string-trim (caddr m-keyval)))))) (cdr lines))
                                    (if m-section
                                        (f (append sections (list section)) (list (string->symbol (string-trim (cadr m-section)))) (cdr lines))
                                        (error "Unknown INI line"))))))))))
      (mcons 'ini (f '() (list 'nil) lines)))))

(define (ini-get ini section key def-val)
  (letrec ((g (lambda (ini)
                (if (null? ini)
                    def-val
                    (if (eq? section (caar ini))
                        (letrec ((f (lambda (l)
                                      (if (null? l)
                                          def-val
                                          (let ((entry (car l)))
                                            (if (eq? (car entry) 'keyval)
                                                (if (eq? (cadr entry) key)
                                                    (caddr entry)
                                                    (f (cdr l)))
                                                (f (cdr l))))))))
                          (f (cdar ini)))
                        (g (cdr ini)))))))
    (g (mcdr ini))))

(define (ini-set! ini section key val)
  (let ((found #f))
    (letrec ((for-sect (lambda (sect)
                         (if (null? sect)
                             (if found
                                 '()
                                 (begin
                                   (set! found #t)
                                   (list (list 'keyval key val))))
                             (let ((entry (car sect)))
                               (if (eq? (car entry) 'keyval)
                                   (if (eq? (cadr entry) key)
                                       (begin
                                         (set! found #t)
                                         (cons (list 'keyval key val) (for-sect (cdr sect))))
                                       (cons entry (for-sect (cdr sect))))
                                   (cons entry (for-sect (cdr sect)))))))))
      (letrec ((for-ini (lambda (ini)
                          (if (null? ini)
                              (if found
                                  '()
                                  (list (list section (list 'keyval key val))))
                              (let* ((ini-section (car ini))
                                     (section-key (car ini-section)))
                                (if (eq? section-key section)
                                    (cons (cons section (for-sect (cdr ini-section))) (for-ini (cdr ini)))
                                    (cons ini-section (for-ini (cdr ini)))))))))
        (let ((new-ini (for-ini (mcdr ini))))
          (set-mcdr! ini new-ini)
          ini)))))



