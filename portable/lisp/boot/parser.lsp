;;;; -------------------------------------------------------------------------
;;;; Universal parser, adapted for os-pi project tests
;;;;
;;;; Package: LISP-PARSER
;;;; Exports:
;;;;   - parse: Main function for parsing a list of tokens.
;;;;
;;;; Features:
;;;;   - Works with a list of tokens in '(:TYPE value)' format, e.g., '(:LPAREN)'.
;;;;   - Supports lists, dotted pairs, arrays, strings, numbers, symbols.
;;;;   - Implements all reader macros: ', `, ,, ,@, #', #\.
;;;; -------------------------------------------------------------------------

(defpackage #:lisp-parser
  (:use #:common-lisp)
  (:export #:parse))

(in-package #:lisp-parser)

;;; --- Parser Response Structure ---
;;; Parsers return a list of successful parses.
;;; Each success is a (cons <parsed-object> <remaining-tokens>).
;;; Failure is an empty list (nil).

(defun p-success (result remaining-input)
  (list (cons result remaining-input)))

(defun p-failure () nil)


;;; --- Basic Combinators ---

(defun p-app (parser func)
  "Applies `func` to the result of a successful `parser` parse."
  (lambda (input)
    (mapcar (lambda (res) (cons (funcall func (car res)) (cdr res)))
            (funcall parser input))))

(defun p-and (&rest parsers)
  "Applies parsers sequentially. Succeeds if all are successful."
  (if (null parsers)
      (lambda (input) (p-success nil input))
      (lambda (input)
        (let* ((p1 (car parsers))
               (ps (cdr parsers))
               (res1 (funcall p1 input)))
          (loop for r1 in res1
                nconc (let ((sub-res (funcall (apply #'p-and ps) (cdr r1))))
                        (mapcar (lambda (sr)
                                  (cons (cons (car r1) (car sr)) (cdr sr)))
                                sub-res)))))))

(defun p-or (&rest parsers)
  "Returns the result of the first successful parser."
  (lambda (input)
    (loop for p in parsers
          do (let ((res (funcall p input)))
               (when res (return res))))
    (p-failure)))

(defun p-many (parser)
  "0 or more repetitions of `parser`."
  (p-or (p-app (p-and parser (p-many parser))
               #'(lambda (res) (cons (car res) (cadr res))))
        (lambda (input) (p-success nil input))))

;;; --- Primitives for Working with Tokens ---

(defun p-token-type (type)
  "A parser that expects a token of a given type."
  (lambda (input)
    (if (and input (eq (caar input) type))
        (p-success (car input) (cdr input))
        (p-failure))))

(defun token-value (token) (second token))

;;; --- Lisp Grammar Description ---

;; Helper function to build lists (including dotted pairs)
(defun build-list (exprs optional-dot-expr)
  (if optional-dot-expr
      (let ((last-expr (car optional-dot-expr)))
        (if (null exprs)
            last-expr ; Case for (. 2) -> 2, although this is invalid syntax
            (let ((rev-exprs (reverse exprs)))
              (reduce #'cons (cdr rev-exprs)
                      :initial-value (cons (car rev-exprs) last-expr)
                      :from-end t))))
      exprs))

(labels (
    ;; -- Recursive declarations --
    (p-expr (input) (funcall (p-or #'p-quoted-form #'p-atom #'p-list) input))
    
    ;; -- Atomic types --
    (p-atom ()
      (p-or (p-app (p-token-type :T_NUMBER) #'token-value)
            (p-app (p-token-type :T_FLOAT)  #'token-value)
            (p-app (p-token-type :T_STRING) #'token-value)
            (p-app (p-token-type :T_CHAR)   (lambda (tok) (code-char (token-value tok))))
            (p-app (p-token-type :T_SYMBOL) (lambda (tok) (intern (string-upcase (token-value tok)))))))
            
    ;; -- Lists and dotted pairs ( ... ) --
    (p-list ()
      (p-app (p-and (p-token-type :LPAREN)
                    (p-many #'p-expr)
                    (p-or (p-app (p-and (p-token-type :DOT) #'p-expr) #'second)
                          (lambda (input) (p-success nil input))) ; Optional dotted part
                    (p-token-type :RPAREN))
             (lambda (res)
               (let ((exprs (second res))
                     (dot-expr (third res)))
                 (build-list exprs dot-expr)))))
                 
    ;; -- # macros --
    (p-sharp-macro ()
      (p-and (p-token-type :SHARP)
             (p-or
              ;; #(...) -> Vector
              (p-app (p-and (p-token-type :LPAREN) (p-many #'p-expr) (p-token-type :RPAREN))
                     (lambda (res) (coerce (second res) 'vector)))
              ;; #'foo -> (FUNCTION foo)
              (p-app (p-and (p-token-type :T_FUNCTION) #'p-expr)
                     (lambda (res) `(FUNCTION ,(second res)))))))

    ;; -- Quoted constructs --
    (p-quoted-form ()
      (p-or
       (p-sharp-macro) ; #(...) and #'...
       (p-app (p-and (p-token-type :QUOTE) #'p-expr)
              (lambda (res) `(QUOTE ,(second res))))
       (p-app (p-and (p-token-type :BACKQUOTE) #'p-expr)
              (lambda (res) `(BACKQUOTE ,(second res))))
       (p-app (p-and (p-token-type :COMMA) #'p-expr)
              (lambda (res) `(COMMA ,(second res))))
       (p-app (p-and (p-token-type :COMMA_AT) #'p-expr)
              (lambda (res) `(COMMA-AT ,(second res))))
       (p-app (p-and (p-token-type :T_FUNCTION) (p-token-type :T_SYMBOL))
              (lambda (res) `(FUNCTION ,(intern (string-upcase (token-value (second res)))))))))

  ;; -- Main parser --
  (defun final-parser (input)
    (p-expr input))
)

;;; --- Main public interface ---

(defun parse (token-stream)
  "Parses a list of tokens.
  Returns the parsed object or raises an error."
  (if (null token-stream)
      (return-from parse :no-value)) ; Corresponds to NOVALUE in C tests
      
  (let* ((results (final-parser token-stream))
         (first-good-result (car results)))
    (cond
      ((null results)
       (error "Parse error: invalid syntax."))
      ((not (null (cdr first-good-result)))
       (error "Parse error: unparsed tokens remaining: ~s" (cdr first-good-result)))
      (t
       (car first-good-result)))))