# Modular Compiler for the TinyC Language

This is the source code repository for a modular tinyC compiler targeting the tiny86 VM implemented in Scala 2. It is a part of masters thesis defended by Martin Prokopič at FIT CTU in 2023.

## Building

A prerequisite for running tests and compiled tiny86 program is a working tiny86 interpreter.

### Building Tiny86

This repository includes a modified version of the tiny86 VM with some bug fixes and support for a text-based assembly format in the `t86` subfolder. The parser and assembly syntax was designed by Filip Gregor as part of his work on an interactive tiny86 debugger.

Tiny86 is a standard CMake project and can be compiled by running the `t86/scripts/build.sh` script. The main CLI binary is then located at `t86/build/t86-cli/t86-cli`.

### Building the Compiler

This project uses `sbt`. We provide `scripts/setup_env.sh`, which installs [asdf](https://asdf-vm.com/) to the home directory and uses it to grab all the required Java, Scala and sbt versions. If you already have asdf installed, you can do the same using `asdf install`.

To build the project, run `sbt assembly` in the root directory of the repository. Unit and integration tests can be run with `sbt test`.

We've tested the instructions above on Ubuntu 22.04 with `build-essential` and `cmake` packages.

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

## TinyC Features

The compiler supports all tinyC features from the [language reference](https://gitlab.fit.cvut.cz/NI-GEN/ni-gen-23/-/blob/main/LANGUAGE.md):

- `void`, `int`, `double`, pointers (including pointers to functions), static 1D arrays and structs
- basic arithmetic operators
- function calls, including recursion
- reading and writing a single character from stdin/stdout via `scan()` and `print(c)` builtins
	+ `scan()` returns -1 on EOF

In addition, we've added the `printnum(n)` builtin, which prints an integer terminated by newline.

`examples/stdlib.c` contains some useful functions that you can copy into your programs.
