// =================================================================
//      ФАЙЛ: portable/lisp/lexer.h (ИСПРАВЛЕННАЯ ВЕРСИЯ)
// =================================================================

#ifndef LEXER_H_
#define LEXER_H_

// --- Определения констант и типов ---

#define MAX_STR 500
#define MAX_SYMBOL 25

typedef enum {
    T_NUMBER, // целое число
    T_FLOAT, // вещественное число
    T_SYMBOL, // строка +a4s6d7fd23
    T_CHAR,  // одиночный символ #\<символ>
    LPAREN, //левая скобка (
    RPAREN, // правая скобка )
    END, // конец потока
    QUOTE, // символ '
    BACKQUOTE, // символ `
    COMMA, // символ ,
    COMMA_AT, // символ ,@
    T_STRING, // строка в кавычках
    SHARP, // символ #
    T_FUNCTION, // символ #'
    DOT,// точка .
} tokentype_t; //тип ликсемы 

typedef struct {
    tokentype_t type; // тип ликсемы
    int value; // значение числа
    char str[MAX_STR]; //значение строки 
} token_t; // ликсема


// --- Глобальные переменные, используемые в проекте ---

extern char *boot_code;
extern int boot_load;


// --- Объявления функций, доступных извне ---

/**
 * @brief Инициализирует лексер новой строкой для разбора.
 * ЭТО ТА САМАЯ ФУНКЦИЯ, КОТОРОЙ НЕ ХВАТАЛО.
 */
void init_lexer(char *string);

/**
 * @brief Возвращает следующий токен из потока.
 */
token_t *get_token();

/**
 * @brief Вспомогательная функция для печати токена (для отладки).
 */
void print_token(token_t *token);

/**
 * @brief Сбрасывает внутренний буфер лексера.
 */
void reset_buffer();

#endif // LEXER_H_