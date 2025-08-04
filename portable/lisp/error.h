#ifndef _ERROR_H_
#define _ERROR_H_

#include <setjmp.h>

extern jmp_buf jmp_env;

void error(char *str, ...);
void parser_error(char *str, ...);

#endif