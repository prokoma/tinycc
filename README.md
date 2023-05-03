# Modular Compiler for the TinyC Language

This is the source code repository for a modular tinyC compiler targeting the tiny86 VM implemented in Scala 2. It is a part of masters thesis defended by Martin Prokopič at FIT CTU in 2023.

## Building

A prerequisite for running tests and compiled tiny86 programs is a working tiny86 interpreter.

### Building Tiny86

This repository includes a modified version of the tiny86 VM with some bug fixes and support for a text-based assembly format in the `t86` subfolder. The parser and assembly syntax was designed by Filip Gregor as part of his work on an interactive tiny86 debugger.

Tiny86 is a standard CMake project and can be compiled by running the `t86/scripts/build.sh` script. The main CLI binary is then located at `t86/build/t86-cli/t86-cli`.

### Building the Compiler

This project uses `sbt`. We provide `scripts/setup_env.sh`, which installs [asdf](https://asdf-vm.com/) to the home directory and uses it to grab all the required Java, Scala and sbt versions. If you already have asdf installed, you can do the same using `asdf install`.

To build the project, run `scripts/build.sh` or `sbt assembly` in the root directory of the repository. Unit and integration tests can be run with `sbt test`.

We have tested the instructions above on Ubuntu 22.04 with `build-essential`, `cmake`, `git` and `curl` packages.

## Usage

The compiler is controlled through the CLI exposed as the `tinycc` script, which contains a built-in help available through `tinycc help`.

### Compiling and Running TinyC Source

```bash
# create hello.c with an example program
cat >hello.c <<EOF
int main() {
	char *str = "Hello, world!\n";
	while(*str) {
		print(*(str++));
	}
	return 0;
}
EOF

# compile hello.c w/o optimizations
./tinycc compile -o hello.t86 hello.c

# compile hello.c w/ optimizations
./tinycc compile -O -o hello.t86 hello.c

# run hello.t86 on tiny86
./t86/build/t86-cli/t86-cli run hello.t86
```

Instead of directly compiling to assembly, we can run the frontend, middleend and backend separately:

```bash
# compile to IR
./tinycc compile-to-ir -o hello.ir hello.c

# optimize the IR
./tinycc optimize -o hello.opt.ir hello.ir

# compile the optimized IR to assembly
./tinycc codegen -o hello.t86 hello.opt.ir
```

The IR grammar is described in the PDF version of the thesis.

## Additional Options

Most subcommands accept `--verbose` and `--profile` options. The former enables additional logging to the standard error output, the latter displays statistics about time spent in individual components of the compiler.

The number of available integer and float registers is 4, resp. 5 and it is configurable with the `--register-cnt=M` and `--float-register-cnt=M` options passed to `compile` or `codegen` actions. 

Don't forget to configure tiny86 with the corresponding register count via `-registerCnt=N` and `-floatRegisterCnt=M` passed to `t86-cli` (default is 10 and 5). The size of RAM can be configured with `-ram=S` (default is 1024 words). We have also added `-stats` option to `t86-cli`, which prints number of elapsed ticks and executed instructions by the VM.

```bash
# compile with 2 integer and 2 float registers
./tinycc compile --register-cnt=2 --float-register-cnt=2 -O -o hello.t86 hello.c

# run with 2 integer, 2 float registers and 128 words of RAM
./t86/build/t86-cli/t86-cli run -registerCnt=2 -floatRegisterCnt=2 -ram=128 -stats hello.t86
```

## Transpiling TinyC to C

For debugging purposes, the compiler contains a builtin tinyC to C transpiler, which can be used to execute tinyC programs directly on the host machine. A small runtime defining the builtin functions is included in `src/test/resources/gcc_runtime.{c,h}`. This is used by the `run_tests.sh` script to verify correctness of the reference output.

```bash
# compile the runtime
gcc --std=c99 -fsigned-char -c src/test/resources/gcc_runtime.c -o gcc_runtime.o

# transpile the tinyC program to C
./tinycc transpile-to-c --prefix='#include "gcc_runtime.h"' -o hello.transpiled.c hello.c

# compile the generated C program and link it with the runtime
gcc --std=c99 -I src/test/resources -fno-builtin -fsigned-char hello.transpiled.c gcc_runtime.o -o hello

# run the binary on the host system
./hello
```

## TinyC Features

The compiler supports all tinyC features from the [language reference](https://gitlab.fit.cvut.cz/NI-GEN/ni-gen-23/-/blob/main/LANGUAGE.md):

- `void`, `char`, `int`, `double`, pointers (including pointers to functions), static 1D arrays and structs
- basic arithmetic operators
- function calls, including recursion
- reading and writing a single character from stdin/stdout via `scan()` and `print(c)` builtins (`scan()` returns -1 on EOF)

In addition, the compiler supports the `printnum(n)` builtin, which prints an integer terminated by newline.

The file `examples/stdlib.c` contains some useful functions that you can copy into your programs (there is no preprocessor).
