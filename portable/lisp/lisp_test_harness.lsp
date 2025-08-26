;;;; Minimal Lisp harness for the portable C test runner
;;; Usage: sbcl --script lisp_test_harness.lsp input.txt output.txt error.txt
;;; Parses a list of tokens from input.txt and writes the result to output.txt

;; Шаг 1: Загружаем файл с реализацией парсера.
(load "../boot/parser.lsp")

(defun main (argv)
  (let ((input-file     (second argv))
        (output-file    (third argv))
        (error-log-file (fourth argv)))
    (handler-case
        (with-open-file (in input-file :direction :input)
          (with-open-file (out output-file :direction :output :if-exists :supersede)
            (let* (;; Читаем всю строку с токенами из входного файла
                   (raw-text (read-line in nil :eof))
                   ;; Оборачиваем строку в скобки, чтобы read-from-string
                   ;; воспринял её как единый список
                   (token-string (format nil "(~a)" raw-text))
                   ;; Преобразуем строку в список токенов
                   (tokens (read-from-string token-string))
                   ;; Шаг 2: Вызываем функцию 'parse'.
                   (result (parse tokens)))
              ;; Записываем результат (разобранное S-выражение) в выходной файл
              (write-line (write-to-string result) out))))
      ;; В случае любой ошибки парсинга или чтения...
      (error (c)
        ;; ...записываем сообщение об ошибке в файл ошибок...
        (with-open-file (err error-log-file :direction :output :if-exists :supersede)
          (format err "~a" c))
        ;; ...и завершаем процесс с кодом ошибки 1.
        (sb-ext:exit :code 1)))))

;; Запускаем главную функцию, передавая ей аргументы командной строки
(main sb-ext:*posix-argv*)