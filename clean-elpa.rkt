#!/usr/bin/env racket
#lang racket/base

;;;; The purpose of this script is to clean ELPA directories from Emacs. Note
;;;; how when package manager sometimes does not fully remove old package upon
;;;; upgradeing, but instead leaves some directories with byte-compiled files.
;;;; This is supposed to detect such directories and remove them.

(define source-pattern #rx"\\.el$")
(define byte-compiled-pattern #rx"\\.elc$")

(define excluded-dirs (list "archives"))
(define home-dir-pattern
  (regexp (string-append "^" (path->string (find-system-path 'home-dir)))))

(define (is-elpa-leftover a-dir)
  (call-with-escape-continuation
   (λ (return)
      (define has-elc #f)
      (for ([p (in-directory a-dir)] #:when (file-exists? p))
        (when (regexp-match? source-pattern (path->string p))
          (return #f))
        (when (and (not has-elc)
                   (regexp-match? byte-compiled-pattern (path->string p)))
          (set! has-elc #t)))
      has-elc)))

(define (get-elpa-leftovers [elpa-dir (build-path (find-system-path 'home-dir) ".emacs.d" "elpa")])
  (define exclusions (map (λ (d) (path->string (build-path elpa-dir d))) excluded-dirs))
  (filter (λ (p)
             (and (directory-exists? p)
                  (not (member (path->string p) exclusions))
                  (is-elpa-leftover p)))
          (directory-list elpa-dir #:build? #t)))

(define (rm-r dir)
  (for ([f (in-list (directory-list dir #:build? #t))])
    (cond [(file-exists? f) (delete-file f)]
          [(directory-exists? f) (rm-r f)]))
  (delete-directory dir))

(define (dir->pretty-string dir)
  (regexp-replace home-dir-pattern (path->string dir) "~/"))

(define (ask-and-maybe-remove-dirs dirs)
  (printf "Directories to be removed:~n~n")
  (for ([d (in-list dirs)])
    (printf "  ~a~n" (dir->pretty-string d)))
  (printf "~nAre you sure? (y/n) ")
  (let ([answer (string-downcase (read-line))])
    (cond [(regexp-match? #rx"^y" answer)
           (for ([d (in-list dirs)])
             (rm-r d))]
          [(regexp-match? #rx"^n" answer)
           (printf "Aborting on user request.~n")]
          [else
           (printf "I did not understand your response. Aborting.~n")])))

(define (clean-elpa)
  (define leftovers (get-elpa-leftovers))
  (if (null? leftovers)
      (printf "Nothing to clean.~n")
      (ask-and-maybe-remove-dirs leftovers)))

(module+ main
  (clean-elpa))
