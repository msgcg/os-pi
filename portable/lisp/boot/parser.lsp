;;;; -------------------------------------------------------------------------
;;;; Универсальный парсер, адаптированный под тесты проекта os-pi
;;;;
;;;; Пакет: LISP-PARSER
;;;; Экспортирует:
;;;;   - parse: Главная функция для разбора списка токенов.
;;;;
;;;; Особенности:
;;;;   - Работает со списком токенов формата '(:TYPE value)', например '(:LPAREN)'.
;;;;   - Поддерживает списки, точечные пары, массивы, строки, числа, символы.
;;;;   - Реализует все читательские макросы: ', `, ,, ,@, #', #\.
;;;; -------------------------------------------------------------------------

(defpackage #:lisp-parser
  (:use #:common-lisp)
  (:export #:parse))

(in-package #:lisp-parser)

;;; --- Структура ответа парсера ---
;;; Парсеры возвращают список успешных разборов.
;;; Каждый успех - это (cons <разобранный-объект> <остаток-токенов>).
;;; Неудача - это пустой список (nil).

(defun p-success (result remaining-input)
  (list (cons result remaining-input)))

(defun p-failure () nil)


;;; --- Базовые комбинаторы ---

(defun p-app (parser func)
  "Применяет `func` к результату успешного разбора `parser`."
  (lambda (input)
    (mapcar (lambda (res) (cons (funcall func (car res)) (cdr res)))
            (funcall parser input))))

(defun p-and (&rest parsers)
  "Применяет парсеры последовательно. Успешен, если все успешны."
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
  "Возвращает результат первого успешного парсера."
  (lambda (input)
    (loop for p in parsers
          do (let ((res (funcall p input)))
               (when res (return res))))
    (p-failure)))

(defun p-many (parser)
  "0 или более повторений `parser`."
  (p-or (p-app (p-and parser (p-many parser))
               #'(lambda (res) (cons (car res) (cadr res))))
        (lambda (input) (p-success nil input))))

;;; --- Примитивы для работы с токенами ---

(defun p-token-type (type)
  "Парсер, ожидающий токен заданного типа."
  (lambda (input)
    (if (and input (eq (caar input) type))
        (p-success (car input) (cdr input))
        (p-failure))))

(defun token-value (token) (second token))

;;; --- Описание грамматики Лиспа ---

;; Вспомогательная функция для построения списков (в т.ч. точечных)
(defun build-list (exprs optional-dot-expr)
  (if optional-dot-expr
      (let ((last-expr (car optional-dot-expr)))
        (if (null exprs)
            last-expr ; Случай (. 2) -> 2, хотя это невалидный синтаксис
            (let ((rev-exprs (reverse exprs)))
              (reduce #'cons (cdr rev-exprs)
                      :initial-value (cons (car rev-exprs) last-expr)
                      :from-end t))))
      exprs))

(labels (
    ;; -- Рекурсивные объявления --
    (p-expr (input) (funcall (p-or #'p-quoted-form #'p-atom #'p-list) input))
    
    ;; -- Атомарные типы --
    (p-atom ()
      (p-or (p-app (p-token-type :T_NUMBER) #'token-value)
            (p-app (p-token-type :T_FLOAT)  #'token-value)
            (p-app (p-token-type :T_STRING) #'token-value)
            (p-app (p-token-type :T_CHAR)   (lambda (tok) (code-char (token-value tok))))
            (p-app (p-token-type :T_SYMBOL) (lambda (tok) (intern (string-upcase (token-value tok)))))))
            
    ;; -- Списки и точечные пары ( ... ) --
    (p-list ()
      (p-app (p-and (p-token-type :LPAREN)
                    (p-many #'p-expr)
                    (p-or (p-app (p-and (p-token-type :DOT) #'p-expr) #'second)
                          (lambda (input) (p-success nil input))) ; Опциональная точечная часть
                    (p-token-type :RPAREN))
             (lambda (res)
               (let ((exprs (second res))
                     (dot-expr (third res)))
                 (build-list exprs dot-expr)))))
                 
    ;; -- Макросы # --
    (p-sharp-macro ()
      (p-and (p-token-type :SHARP)
             (p-or
              ;; #(...) -> Вектор
              (p-app (p-and (p-token-type :LPAREN) (p-many #'p-expr) (p-token-type :RPAREN))
                     (lambda (res) (coerce (second res) 'vector)))
              ;; #'foo -> (FUNCTION foo)
              (p-app (p-and (p-token-type :T_FUNCTION) #'p-expr)
                     (lambda (res) `(FUNCTION ,(second res)))))))

    ;; -- Конструкции с цитированием --
    (p-quoted-form ()
      (p-or
       (p-sharp-macro) ; #(...) и #'...
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

  ;; -- Главный парсер --
  (defun final-parser (input)
    (p-expr input))
)

;;; --- Главный публичный интерфейс ---

(defun parse (token-stream)
  "Разбирает список токенов.
  Возвращает разобранный объект или возбуждает ошибку."
  (if (null token-stream)
      (return-from parse :no-value)) ; Соответствует NOVALUE в C тестах
      
  (let* ((results (final-parser token-stream))
         (first-good-result (car results)))
    (cond
      ((null results)
       (error "Ошибка парсинга: невалидный синтаксис."))
      ((not (null (cdr first-good-result)))
       (error "Ошибка парсинга: остались неразобранные токены: ~s" (cdr first-good-result)))
      (t
       (car first-good-result)))))