# KMCC: The Kiel Monadic Curry Compiler

This repository contains the implementation of a compiler
and run-time system to compile [Curry](http://www.curry-lang.org)
programs to [Haskell](http://www.haskell.org) programs
via a monadic transformation.

The ideas of this compiler are described in:

> Hanus, M., Prott, K.-O., Teegen, F.:
> A Monadic Implementation of Functional Logic Programs.
> Proc. of the 24th International Symposium on Principles and Practice
> of Declarative Programming (PPDP 2022), ACM Press, pp. 1:1-1:15, 2024
> DOI: [10.1145/3551357.3551370](https://dx.doi.org/10.1145/3551357.3551370)

Installation:
-------------

To install the compiler and interactive compilation environment,
download this repository and build it. This can be done by the
following shell commands:

    > git clone git@github.com:Ziharrk/kmcc.git
    > cd kmcc
    > git submodule update --init
    > make

This installs in the subdirectory `bin` the executables and
interactive environment of KMCC. The latter can be invoked
by the script `bin/kmcc`. If you put the directory `.../kmcc/bin`
into your `PATH` variable, KMCC can be invoked as follows:

    > kmcc
    > ----------------------------------------------------------
    > KMCC Interactive Environment (Version 0.3.0 of 2025-02-17)
    > ----------------------------------------------------------
    Prelude> 

Now one can use the command `:help` to get an overview of all
available commands in this environment.

A distinguishing feature of KMCC is its operational completeness
by offering a fair search strategy in the default mode.
For instance, KMCC computes a value to the following
non-deterministic choice between three expressions,
where the leftmost and rightmost are non-terminating: 

    Prelude> length [1 ..] ? 42 ? length [1 ..]
    42
