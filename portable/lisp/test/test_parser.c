/**
 * ===================================================================================
 *  Гибридный тестовый файл для проверки внешнего Lisp-парсера.
 *  ----------------------------------------------------------------------------------
 *  Этот C-файл компилируется в исполняемый файл, который:
 *  1. Готовит наборы токенов для тестов.
 *  2. Запускает Lisp-интерпретатор (SBCL) с вашим парсером ('lisp_test_harness.lsp').
 *  3. Передает ему токены через временный файл.
 *  4. Читает результат из другого временного файла и сравнивает с ожидаемым.
 * ===================================================================================
 */

// Стандартные библиотеки, необходимые для работы тестов
#include <stdio.h>
#include <stdlib.h> // Необходимо для system()
#include <string.h>

// Заголовочные файлы вашего проекта
#include "test.h"
#include "objects.h" // Для init_objects()
#include "alloc.h"   // Для init_regions()
#include "lexer.h"   // Для определения структуры token_t

/**
 * @brief Запускает Lisp-парсер как внешний процесс и проверяет его вывод,
 *        используя временные файлы для надежной работы на Windows и Linux.
 * 
 * @param test_name Имя теста для вывода.
 * @param lisp_executable Команда для запуска Lisp (например, "sbcl --script").
 * @param script_path Путь к lisp_test_harness.lsp.
 * @param tokens Массив токенов для отправки.
 * @param token_count Количество токенов в массиве.
 * @param expected_output Ожидаемая строка от Lisp-парсера.
 */
void run_lisp_test(const char* test_name, const char* lisp_executable, const char* script_path, 
                   token_t tokens[], int token_count, const char* expected_output)
{
    printf("%s: ", test_name);

    // 1. Записать токены во временный файл ввода
    FILE *input_file = fopen("temp_lisp_input.txt", "w");
    if (!input_file) {
        printf("Failed to create temp input file.\n");
        FAIL;
        return;
    }
    for (int i = 0; i < token_count; i++) {
        switch(tokens[i].type) {
            case T_NUMBER: fprintf(input_file, "(:T_NUMBER %d) ", tokens[i].value); break;
            case T_FLOAT:  fprintf(input_file, "(:T_FLOAT %f) ", *(float*)&tokens[i].value); break;
            case T_STRING: fprintf(input_file, "(:T_STRING \\\"%s\\\") ", tokens[i].str); break;
            case T_SYMBOL: fprintf(input_file, "(:T_SYMBOL \"%s\") ", tokens[i].str); break;
            case T_CHAR:   fprintf(input_file, "(:T_CHAR %d) ", tokens[i].value); break;
            case LPAREN:   fprintf(input_file, "(:LPAREN) "); break;
            case RPAREN:   fprintf(input_file, "(:RPAREN) "); break;
            case DOT:      fprintf(input_file, "(:DOT) "); break;
            case QUOTE:    fprintf(input_file, "(:QUOTE) "); break;
            case BACKQUOTE:fprintf(input_file, "(:BACKQUOTE) "); break;
            case COMMA:    fprintf(input_file, "(:COMMA) "); break;
            case COMMA_AT: fprintf(input_file, "(:COMMA_AT) "); break;
            case SHARP:    fprintf(input_file, "(:SHARP) "); break;
            // Добавьте здесь другие типы токенов, если они понадобятся
        }
    }
    fclose(input_file);

    // 2. Сформировать команду для запуска с перенаправлением ввода-вывода
    char command[1024];
    sprintf(command, "%s %s < temp_lisp_input.txt > temp_lisp_output.txt 2> temp_lisp_error.txt", 
            lisp_executable, script_path);

    // 3. Выполнить команду в операционной системе
    int exit_code = system(command);

    // 4. Прочитать результат из файла вывода
    char result_buffer[512] = {0};
    FILE *output_file = fopen("temp_lisp_output.txt", "r");
    if (output_file) {
        if (fgets(result_buffer, sizeof(result_buffer) - 1, output_file) != NULL) {
            // Удаляем символы новой строки (\n или \r\n) для корректного сравнения
            result_buffer[strcspn(result_buffer, "\n\r")] = 0;
        }
        fclose(output_file);
    }
    
    // 5. Проверить результат
    if (exit_code == 0) { // Lisp-процесс завершился успешно (код 0)
        ASSERT(strcmp(result_buffer, expected_output), 0);
    } else { // Lisp-процесс завершился с ошибкой
        if (strcmp(expected_output, "LISP_PARSE_ERROR") == 0) {
            OK; // Мы ожидали ошибку, и она произошла - тест пройден
        } else {
            printf("Lisp process failed unexpectedly. Expected '%s', got error code %d.\n", 
                   expected_output, exit_code);
            FAIL;
        }
    }

    // 6. Очистить временные файлы
    remove("temp_lisp_input.txt");
    remove("temp_lisp_output.txt");
    remove("temp_lisp_error.txt");
}

// --- Определение гибридных тестов ---

void test_lisp_list_atoms() {
    token_t tokens[] = {
        {LPAREN}, {T_NUMBER, 45}, {T_SYMBOL, 0, "A"}, {T_STRING, 0, "StrB"}, {RPAREN}
    };
    const char* expected = "(45 A \"StrB\")"; 
    run_lisp_test("test_lisp_list_atoms", "sbcl --script", "portable/lisp/lisp_test_harness.lsp", tokens, 5, expected);
}

void test_lisp_dotted_pair() {
    token_t tokens[] = { {LPAREN}, {T_NUMBER, 1}, {DOT}, {T_NUMBER, 2}, {RPAREN} };
    const char* expected = "(1 . 2)";
    run_lisp_test("test_lisp_dotted_pair", "sbcl --script", "portable/lisp/lisp_test_harness.lsp", tokens, 5, expected);
}

void test_lisp_no_rparen() {
    token_t tokens[] = { {LPAREN}, {T_NUMBER, 1} };
    const char* expected = "LISP_PARSE_ERROR"; // Ожидаем, что Lisp выдаст ошибку
    run_lisp_test("test_lisp_no_rparen", "sbcl --script", "portable/lisp/lisp_test_harness.lsp", tokens, 2, expected);
}

void test_lisp_array() {
    token_t tokens[] = { {SHARP}, {LPAREN}, {T_NUMBER, 1}, {T_NUMBER, 2}, {RPAREN} };
    const char* expected = "#(1 2)";
    run_lisp_test("test_lisp_array", "sbcl --script", "portable/lisp/lisp_test_harness.lsp", tokens, 5, expected);
}

void test_lisp_quote() {
    token_t tokens[] = { {QUOTE}, {T_SYMBOL, 0, "MY-SYMBOL"} };
    const char* expected = "(QUOTE MY-SYMBOL)";
    run_lisp_test("test_lisp_quote", "sbcl --script", "portable/lisp/lisp_test_harness.lsp", tokens, 2, expected);
}


// --- Точка входа в программу ---

int main()
{
    // Инициализация подсистем вашего проекта
    init_regions();
    init_objects();

    // Запуск тестов, проверяющих Lisp-парсер
    printf("\n------------ Testing Lisp parser implementation -----------\n");
    
    test_lisp_list_atoms();
    test_lisp_dotted_pair();
    test_lisp_no_rparen();
    test_lisp_array();
    test_lisp_quote();
    // Добавьте сюда вызовы других ваших test_lisp_... функций

    printf("\n-------------------- All tests finished --------------------\n");
    
    return 0;
}