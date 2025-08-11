;; Загружаем ваш парсер. Этот путь должен быть правильным относительно места запуска.
(load "portable/lisp/parser.lsp") 

(in-package #:lisp-parser)

;;; --- Главная логика обертки ---
(defun main ()
  (let ((token-string (read-line *standard-input* nil nil)))
    (when token-string
      (handler-case
          (let* ((token-list (read-from-string (format nil "(~a)" token-string)))
                 (result (parse token-list)))
            (princ result)) ; Печатаем результат в stdout
        (error (c)
          (format *error-output* "LISP_PARSE_ERROR: ~a" c) ; Пишем ошибку в stderr
          (uiop:quit 1)))))) ; Выходим с ненулевым кодом

(main)