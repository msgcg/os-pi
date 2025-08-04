CFLAGS=-O3 -I. -I../ -I../../../include -g -Wno-pointer-to-int-cast -Wno-int-to-pointer-cast -Wno-int-conversion
LDFLAGS=-lm
TESTS=test_lexer.exe test_symbols.exe test_objects.exe test_parser.exe test_eval_int.exe test_arrays.exe test_arith.exe test_str_int.exe test_pair.exe test_mpa.exe
PROFILE=baselisp.lsp

all: $(TESTS)

test_lexer.exe: test_lexer.c ../lexer.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^

test_eval_int.exe: test_eval_int.c ../eval.c ../symbols.c ../objects.c ../pair.c ../alloc.c ../bind.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^

test_pair.exe: test_pair.c ../symbols.c ../objects.c ../pair.c ../alloc.c ../eval.c ../bind.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^

test_arith.exe: test_arith.c ../eval.c ../symbols.c ../objects.c ../arith.c ../alloc.c ../bind.c
	gcc $^ $(CFLAGS) $(LDFLAGS) -o $@

test_symbols.exe: test_symbols.c ../symbols.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^

test_objects.exe: test_objects.c ../objects.c ../alloc.c ../bind.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^

test_parser.exe: test_parser.c ../parser.c ../objects.c ../alloc.c ../symbols.c ../bind.c ../eval.c ../error.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^

test_arrays.exe: test_arrays.c ../symbols.c ../objects.c ../array.c ../alloc.c ../bind.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^

test_str_int.exe: test_str_int.c ../symbols.c ../objects.c ../str.c ../alloc.c ../bind.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^

test_sys.exe: test_sys.c ../eval.c ../symbols.c ../objects.c ../parser.c ../lexer.c ../arith.c ../str.c ../array.c ../pair.c ../predicates.c ../alloc.c ../error.c ../bind.c
	gcc $^ $(CFLAGS) -D REPL $(LDFLAGS) -o $@

test_comp_d.exe: test_comp.c ../eval.c ../objects.c ../parser.c ../lexer.c ../arith.c ../str.c ../array.c ../pair.c ../predicates.c ../alloc.c ../error.c ../vm.c ../bind.c ../predicates.c ../symbols.c
	gcc $^ $(CFLAGS) -D REPL -D DEBUG -D VM $(LDFLAGS) -o $@

test_comp.exe: test_comp.c ../eval.c ../objects.c ../parser.c ../lexer.c ../arith.c ../str.c ../array.c ../pair.c ../predicates.c ../alloc.c ../error.c ../vm.c ../bind.c ../predicates.c ../symbols.c
	gcc $^ $(CFLAGS) -D REPL -D VM $(LDFLAGS) -o $@

lisp.exe: test_sys.c ../eval.c ../symbols.c ../objects.c ../parser.c ../lexer.c ../arith.c ../str.c ../array.c ../pair.c ../predicates.c ../alloc.c ../error.c ../bind.c
	gcc $^ $(CFLAGS) -DDEBUG $(LDFLAGS) -o $@

repl_lisp.exe: test_sys.c ../eval.c ../symbols.c ../objects.c ../parser.c ../lexer.c ../arith.c ../str.c ../array.c ../pair.c ../predicates.c ../alloc.c ../error.c ../bind.c
	gcc $^ $(CFLAGS) -DREPL -DDEBUG $(LDFLAGS) -o $@

test_mpa.exe: test_mpa.c ../mpa.c
	gcc $^ $(CFLAGS) $(LDFLAGS) -o $@

test_libc.exe: ../mpa.c ../../float.c test_libc.c
	gcc $^ $(CFLAGS) $(LDFLAGS) -o $@

test_vm.exe: ../vm.c test_vm.c
	gcc $^ $(CFLAGS) $(LDFLAGS) -o $@

test: $(TESTS) test_sys.exe
	for %%f in ($(TESTS)) do %%f


coverage: $(TESTS) /tmp/test_sys
	lcov -c -i -d ../ -o /tmp/base.info
	for test in $(TESTS); do rm $$test; make $$test; bash -c $$test ; lcov -c -d ../ -o $$test.info ;	done
	./make_report

profile:
	gcc test_sys.c ../eval.c ../symbols.c ../objects.c ../parser.c ../lexer.c ../arith.c ../str.c ../array.c ../pair.c ../predicates.c ../alloc.c ../error.c $(CFLAGS) $(LDFLAGS) -g -pg -o /tmp/profile
	make -C .. $(PROFILE)
	for i in `seq 1 10`; do /tmp/profile <$(PROFILE)  ; mv gmon.out gmon.out.$$i ; done
	gprof -s /tmp/profile gmon.out.*
	gprof /tmp/profile gmon.sum >/tmp/report
