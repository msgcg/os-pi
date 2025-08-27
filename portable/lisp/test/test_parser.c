#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "test.h"
#include "objects.h"
#include "alloc.h"
#include "lexer.h"

void run_lisp_test(const char* test_name, const char* lisp_executable, const char* script_path, 
                   token_t tokens[], int token_count, const char* expected_output)
{
    printf("%s: ", test_name);
    
    // Определяем имена временных файлов
    const char* input_filename = "temp_lisp_input.txt";
    const char* output_filename = "temp_lisp_output.txt";
    const char* error_filename = "temp_lisp_error.txt";

    FILE *input_file = fopen(input_filename, "w");
    if (!input_file) {
        printf("Failed to create temp input file.\n");
        FAIL;
        return;
    }
    for (int i = 0; i < token_count; i++) {
        switch(tokens[i].type) {
            case T_NUMBER: fprintf(input_file, "(:T_NUMBER %d) ", tokens[i].value); break;
            case T_FLOAT:  fprintf(input_file, "(:T_FLOAT %f) ", *(float*)&tokens[i].value); break;
            case T_STRING: fprintf(input_file, "(:T_STRING \"%s\") ", tokens[i].str); break;
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
            case T_FUNCTION: fprintf(input_file, "(:T_FUNCTION) "); break;
        }
    }
    fclose(input_file);

    char command[1024];
    // ФОРМИРУЕМ КОМАНДУ, ПЕРЕДАВАЯ ИМЕНА ФАЙЛОВ КАК АРГУМЕНТЫ
    sprintf(command, "%s %s %s %s %s", 
            lisp_executable, script_path, input_filename, output_filename, error_filename);
    
    int exit_code = system(command);

    char result_buffer[512] = {0};
    FILE *output_file = fopen(output_filename, "r");
    if (output_file) {
        if (fgets(result_buffer, sizeof(result_buffer) - 1, output_file) != NULL) {
            result_buffer[strcspn(result_buffer, "\n\r")] = 0;
        }
        fclose(output_file);
    }
    
    if (exit_code == 0) {
        if (strcmp(result_buffer, expected_output) != 0) {
            printf("Expected: '%s', Got: '%s'\n", expected_output, result_buffer);
        }
        ASSERT(strcmp(result_buffer, expected_output), 0);
    } else {
        if (strcmp(expected_output, "LISP_PARSE_ERROR") == 0) {
            OK;
        } else {
            printf("Lisp process failed unexpectedly. Expected '%s', got error code %d.\n", 
                   expected_output, exit_code);
            // Попробуем прочитать файл с ошибками, если он есть
            FILE* err_file = fopen(error_filename, "r");
            if (err_file) {
                char err_buf[256];
                fgets(err_buf, sizeof(err_buf), err_file);
                printf("  Lisp error: %s\n", err_buf);
                fclose(err_file);
            }
            FAIL;
        }
    }

    remove(input_filename);
    remove(output_filename);
    remove(error_filename);
}

// --- Определение гибридных тестов ---

const char* sbcl_path = "sbcl --script";
const char* harness_path = "../lisp_test_harness.lsp";

void test_lisp_list_atoms() {
    token_t tokens[] = {
        {LPAREN}, {T_NUMBER, 45}, {T_SYMBOL, 0, "A"}, {T_STRING, 0, "StrB"}, {RPAREN}
    };
    const char* expected = "(45 A \"StrB\")"; 
    run_lisp_test("test_lisp_list_atoms", sbcl_path, harness_path, tokens, 5, expected);
}

void test_lisp_dotted_pair() {
    token_t tokens[] = { {LPAREN}, {T_NUMBER, 1}, {DOT}, {T_NUMBER, 2}, {RPAREN} };
    const char* expected = "(1 . 2)";
    run_lisp_test("test_lisp_dotted_pair", sbcl_path, harness_path, tokens, 5, expected);
}

void test_lisp_no_rparen() {
    token_t tokens[] = { {LPAREN}, {T_NUMBER, 1} };
    const char* expected = "LISP_PARSE_ERROR";
    run_lisp_test("test_lisp_no_rparen", sbcl_path, harness_path, tokens, 2, expected);
}

void test_lisp_array() {
    token_t tokens[] = { {SHARP}, {LPAREN}, {T_NUMBER, 1}, {T_NUMBER, 2}, {RPAREN} };
    const char* expected = "#(1 2)";
    run_lisp_test("test_lisp_array", sbcl_path, harness_path, tokens, 5, expected);
}

void test_lisp_quote() {
    token_t tokens[] = { {QUOTE}, {T_SYMBOL, 0, "MY-SYMBOL"} };
    const char* expected = "(QUOTE MY-SYMBOL)";
    run_lisp_test("test_lisp_quote", sbcl_path, harness_path, tokens, 2, expected);
}

void test_lisp_list_numbers_sep() {
    token_t tokens[] = {
        {LPAREN}, {T_NUMBER, 1}, {COMMA}, {T_NUMBER, 2}, {COMMA}, {T_NUMBER, 3}, {RPAREN}
    };
    const char* expected = "(1 2 3)";
    run_lisp_test("test_lisp_list_numbers_sep", sbcl_path, harness_path, tokens, 7, expected);
}

// --- Точка входа в программу ---
int main()
{
    init_regions();
    init_objects();
    printf("\n------------ Testing Lisp parser implementation via SBCL -----------\n");
    test_lisp_list_atoms();
    test_lisp_dotted_pair();
    test_lisp_no_rparen();
    test_lisp_array();
    test_lisp_quote();
    test_lisp_list_numbers_sep();
    printf("\n-------------------- All tests finished --------------------\n");
    return 0;
}