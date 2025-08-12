;;;; Minimal Lisp harness for the portable C test runner
;; Usage: sbcl --script lisp_test_harness.lsp input.txt output.txt error.txt
;; Parses a list of tokens from input.txt and writes the result to output.txt

(load "portable/lisp/boot/parser.lsp")
(if (null (find-package "PARSER"))
    (defpackage "PARSER"
      (:use :common-lisp)
      (:export parse))
  (format t "Parser package handled"))

(defun main (argv)
  (let ((input     (second argv))
        (output    (third argv))
        (error-log (fourth argv)))
    (handler-case
        (with-open-file (in input :direction :input)
          (with-open-file (out output :direction :output :if-exists :supersede)
            (let* ((raw (read-line in))
                   (tokens (read-from-string (format nil "(~a)" raw)))
                   (res (parser:parse tokens)))
              (write-line (write-to-string res) out))))
      (error (c)
        (with-open-file (err error-log :direction :output :if-exists :supersede)
          (format err "~a" c))
        (sb-ext:exit :code 1)))))

(main sb-ext:*posix-argv*)