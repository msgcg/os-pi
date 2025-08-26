CFLAGS=-O3 -I. -I../ -I../../../include -g -Wno-pointer-to-int-cast -Wno-int-to-pointer-cast -Wno-int-conversion
LDFLAGS=-lm
TESTS=test_lexer test_symbols test_objects test_parser test_eval_int test_arrays test_arith test_str_int test_pair test_mpa
PROFILE=baselisp.lsp

all: $(TESTS)

test_lexer: test_lexer.c ../lexer.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^

test_eval_int: test_eval_int.c ../eval.c ../symbols.c ../objects.c ../pair.c ../alloc.c ../bind.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^

test_pair: test_pair.c ../symbols.c ../objects.c ../pair.c ../alloc.c ../eval.c ../bind.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^

test_arith: test_arith.c ../eval.c ../symbols.c ../objects.c ../arith.c ../alloc.c ../bind.c
	gcc $^ $(CFLAGS) $(LDFLAGS) -o $@

test_symbols: test_symbols.c ../symbols.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^

test_objects: test_objects.c ../objects.c ../alloc.c ../bind.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^

test_parser: test_parser.c ../parser.c ../objects.c ../alloc.c ../symbols.c ../bind.c ../eval.c ../error.c ../lexer.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^

test_arrays: test_arrays.c ../symbols.c ../objects.c ../array.c ../alloc.c ../bind.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^

test_str_int: test_str_int.c ../symbols.c ../objects.c ../str.c ../alloc.c ../bind.c
	gcc $(CFLAGS) $(LDFLAGS) -o $@ $^

test_sys: test_sys.c ../eval.c ../symbols.c ../objects.c ../parser.c ../lexer.c ../arith.c ../str.c ../array.c ../pair.c ../predicates.c ../alloc.c ../error.c ../bind.c
	gcc $^ $(CFLAGS) -D REPL $(LDFLAGS) -o $@

test_comp_d: test_comp.c ../eval.c ../objects.c ../parser.c ../lexer.c ../arith.c ../str.c ../array.c ../pair.c ../predicates.c ../alloc.c ../error.c ../vm.c ../bind.c ../predicates.c ../symbols.c
	gcc $^ $(CFLAGS) -D REPL -D DEBUG -D VM $(LDFLAGS) -o $@

test_comp: test_comp.c ../eval.c ../objects.c ../parser.c ../lexer.c ../arith.c ../str.c ../array.c ../pair.c ../predicates.c ../alloc.c ../error.c ../vm.c ../bind.c ../predicates.c ../symbols.c
	gcc $^ $(CFLAGS) -D REPL -D VM $(LDFLAGS) -o $@

lisp: test_sys.c ../eval.c ../symbols.c ../objects.c ../parser.c ../lexer.c ../arith.c ../str.c ../array.c ../pair.c ../predicates.c ../alloc.c ../error.c ../bind.c
	gcc $^ $(CFLAGS) -DDEBUG $(LDFLAGS) -o $@

repl_lisp: test_sys.c ../eval.c ../symbols.c ../objects.c ../parser.c ../lexer.c ../arith.c ../str.c ../array.c ../pair.c ../predicates.c ../alloc.c ../error.c ../bind.c
	gcc $^ $(CFLAGS) -DREPL -DDEBUG $(LDFLAGS) -o $@

test_mpa: test_mpa.c ../mpa.c
	gcc $^ $(CFLAGS) $(LDFLAGS) -o $@

test_libc: ../mpa.c ../../float.c test_libc.c
	gcc $^ $(CFLAGS) $(LDFLAGS) -o $@

test_vm: ../vm.c test_vm.c
	gcc $^ $(CFLAGS) $(LDFLAGS) -o $@

test: $(TESTS) test_sys
	for f in $(TESTS); do ./$$f; done


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