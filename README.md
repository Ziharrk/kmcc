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

Since the front end and compiler of KMCC are implemented in Haskell,
[Haskell Stack](https://www.haskellstack.org/) implementation is required
to install KMCC.
Thus, the executable `stack` should be in the load path.
For instance, if you run Ubuntu Linux, you can easily install it by

    > sudo apt-get install haskell-stack

Furthermore, the interactive environment ("REPL") of KMCC is
implemented in Curry so that the
[Curry Package Manager](https://curry-lang.org/tools/cpm/)
together with a Curry system,
like [PAKCS](https://www.curry-lang.org/pakcs/) or
[KiCS2](https://www.curry-lang.org/kics2/),
must be already installed.
Thus, the executable `cypm` should be in the load path.

If these prerequisites are satisfied, the compiler and
interactive compilation environment of KMCC can be installed
by downloading this repository and building it.
This can be done by the following shell commands:

    > git clone git@github.com:Ziharrk/kmcc.git
    > cd kmcc
    > git submodule update --init
    > make

This installs in the subdirectory `bin` the executables and
interactive environment of KMCC. The latter can be invoked
by the script `bin/kmcc`. If you put the directory `.../kmcc/bin`
into your `PATH` variable, KMCC can be invoked as follows:

    > kmcc
    ----------------------------------------------------------
    KMCC Interactive Environment (Version 0.3.0 of 2025-02-24)
    ----------------------------------------------------------
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
