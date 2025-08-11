;; --- LISP TEST HARNESS (FINAL VERSION) ---
;; Этот скрипт принимает имена файлов как аргументы командной строки.

(load (merge-pathnames "boot/parser.lsp" *load-truename*))
(in-package #:lisp-parser)

(defun main ()
  ;; Получаем имена файлов из аргументов командной строки
  (let* ((args sb-ext:*posix-argv*)
         (input-file (second args))
         (output-file (third args))
         (error-file (fourth args)))

    (unless (and input-file output-file error-file)
      (format *error-output* "Usage: sbcl --script harness.lsp <input> <output> <error>")
      (sb-ext:quit :unix-status 1))
      
    (handler-case
        ;; Открываем файлы и привязываем их к стандартным потокам
        (with-open-file (input-stream input-file :direction :input)
          (with-open-file (output-stream output-file :direction :output :if-exists :supersede)
            (let* ((*standard-input* input-stream)
                   (*standard-output* output-stream)
                   (token-string (read-line nil nil nil))
                   (token-list (read-from-string (format nil "(~a)" token-string)))
                   (result (parse token-list)))
              (princ result))))
      ;; В случае ошибки, пишем в файл ошибок
      (error (c)
        (with-open-file (error-stream error-file :direction :output :if-exists :supersede)
          (format error-stream "LISP_PARSE_ERROR: ~a" c))
        (sb-ext:quit :unix-status 1)))))

(main)