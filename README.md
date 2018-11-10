
Modelyze toolchain, version 1.15
------------------------------------
Modelyze toolchain
Copyright (C) 2010-2017 David Broman


This project contains a simple interpreter for the 
Modelyze language. For a detailed overview of the
Modelyze language, please see the technical report:

David Broman and Jeremy G. Siek. "Modelyze: a Gradually Typed Host Language 
for Embedding Equation-Based Modeling Languages", Technical Report No. 
UCB/EECS-2012-173, University of California, Berkeley, June 30, 2012.
Available at: http://www.eecs.berkeley.edu/Pubs/TechRpts/2012/EECS-2012-173.html

A comprehensive description of Modelyze's predecessor, called MKL, is available
in [David Broman's PhD Thesis](http://www.bromans.com/david/publ/thesis-2010-david-broman.pdf)

If you have any comments or questions, please send an email to
[dbro@kth.se](mailto:dbro@kth.se).

<!--
 INSTALLATION (Mac OS)
 --------------------
 The following has been tested on OS X 10.12.6, but should work on 
 other Mac OS versions as well.

 1. Install [Homebrew](https://brew.sh/).

 2. Install the Objective Caml compiler, Gnuplot, and Sundials via Homebew.  
  
    `>> brew install ocaml opam homebrew/science/sundials`
  
    `>> brew install gnuplot --qt`
 3. If you have not done it already, clone the Modelyze project from GitHub:
  
    `>> git clone https://github.com/david-broman/modelyze.git`
 5. Compile the code:
  
    `>> cd modelyze`
  
    `>> make`
-->
INSTALLATION (Linux)
---------------------

The following has been tested on a `ubuntu 18.04.1 LTS` minimal installation and
version `4.05.0` of the `ocaml` compiler, but should work on other Linux
distributions as well.

We install [Ocaml](https://ocaml.org/),
[Ocamlbuild](https://ocaml.org/learn/tutorials/ocamlbuild/) and
[SundialsML](https://inria-parkas.github.io/sundialsml/) using
[opam](https://opam.ocaml.org/).

1. Install `opam` along with some dependencies and the
[Sundials](https://computation.llnl.gov/projects/sundials) libraries:

```console
sudo apt-get install opam m4 libsundials-dev
```

This will install version `4.05.0`  of the `ocaml` compiler as a dependency.

2. Initialize `opam` by issuing:

```console
opam init
eval `opam config env`
```

Preferably, update your `~/.profile` according to the instructions from
`opam init` to set the correct environment at each login session.

3. Install the *Ocaml* bindings for the *Sundials* solver suite together with
`ocamlbuild`.

```console
opam install sundialsml ocamlbuild
```

If you would like to switch the version of the `ocaml` compiler, see the
[documentation](https://opam.ocaml.org/doc/man/opam-switch.html) for `opam switch`

4. Install *GNUPlot* if you would like to plot:

```console
sudo apt-get install gnuplot
```

5. If you have not done it already, clone the *Modelyze* project from *GitHub*:

```console
git clone https://github.com/david-broman/modelyze.git
```

6. Finally, compile the code:

```console
cd modelyze
make
```

EXECUTING EXAMPLES
--------------------

Under folder `library`, a simple standard library for Modelyze is provided.
Folder `demo` contains a number of demo files that use these libraries.
To execute a demo example, got to folder `demo` and run for instance:

```console
./moz lotkavolterra.moz
```

This prints the plot data to the standard output. If GNUPlot is installed,
plotting can be done as follows:

```console
./mozplot hybrid-ball-stairs.moz
```

DISPLAYING ANIMATIONS FOR THE 2D-MECHANICS LIBRARY WITH MATLAB
---------------------------------------------------------------

In the folder [`env/matlab`](env/matlab) there's scripts that can visualize the output from
provided by the mechanical2d library. In order to use these follow these steps:

1. Install MATLAB
2. Open MATLAB, navigate to the [`env/matlab`](env/matlab) folder and open the `armAnim.m` script
3. Change the file path and filename to your liking and run the script

There is also another MATLAB script, `dataplotter.m`, for plotting the output of
any Modelyze script that can be used instead of GNUplot.

NOTE: currently the animation script uses the angles of each joint as input
and can thus not extract the length of the links so the script assumes all
lengths are 0.5 meters. This will be fixed later when an initialization bug
is fixed so the script can use the position of each joint instead.

DILL
----

DILL is a DSL for modeling hybrid systems and is part of the evaluation of a
semantics described in the master thesis:

*Hybrid Semantics in Equation-Based Modelling - Oscar Eriksson ([oerikss@kth.se](mailto:oerikss@kth.se)) (pending upload to the public domain)*

The implementation can be found in [`library/dill.moz`](library/dill.moz),
[`library/dillmodels.moz`](library/dillmodels.moz) and
[`library/graphelaboration.moz`](library/graphelaboration.moz).  Examples using
this implementation can be found in [`demo/dill`](demo/dill).

LICENSE
-------
All files in the Modelyze toolchain project, excepts for files in the folders
`library/` and `ext/` are under the GNU General Public Licence according
to file COPYING. Files under folder `ext/` have specific licenses given
in each sub-folder. Files under folder `library/` are under the GNU Lesser
General Public License according to file `library/COPYING.LESSER`.





REVISIONS
---------

* Version 1.15
  * Use [SundialsML](https://inria-parkas.github.io/sundialsml/)
  * Added primitive functions for calling nonlinear equation solver (KINSOL)
  * Included DILL as an alternative for modeling hybrid systems
  * Added syntax definitions for VIM
  * Updated regression tests
  * Updated Makefile and build instructions

* Version 1.14 
  - Improved error messages.
  - Added support for function overloading.
  - Fixed a bug when using Sundials via Macports.

* Version 1.13 January 13, 2015
  - Created demo, test, and library directories
  - Added library-path parameter
  - Solved many minor issues

* Version 1.12 November 22, 2013
  - Added new regression testing system
  - Added a number of new DSLs

* Version 1.11 September 24, 2013
  - Minor bugfixes and cleanup.

* Version 1.1 December 17, 2012.
  - Changed the name of the project from "MKL" to "Modelyze". The rationale
    for the change is to emphasize that Modelyze is a host language for
    embedding DSLs.
  - Full gradual typing is now implemented according to the technical report
    http://www.eecs.berkeley.edu/Pubs/TechRpts/2012/EECS-2012-173.html. The
    type system and the symbol lifting analysis correspond to the description.
    Cast insertion has not been implemented.  
  - Pattern matching on symbols do not need to have the ~ anymore. Instead, the
    compiler recognized if the pattern variable equal to a globally defined 
    symbol and in such a case it is treated as a symbol matching instead of
    a pattern variable 
  - Simplified the way of creating symbolic data types. Now these are always
    symbolic types. Defintions of equations and nodes are now simpler.
  - Add def-syntax, which is closer to Java or C. { } are used as
    grouping of expressions, not as defining arrays.
  - Added new syntax for derivatives, e.g., der(x) can be written x' and
    der(der(y)) can be written y''
  - Changed the syntax for polymorphic equality from == to <==>. Added 
    operators == and ==. meaning real and integer equality test respectively.
    The rationale is that == can now be used as part of a model.
  - Added two new keywords "begin" and "end", which have the same 
    functionality as parentheses.
  - Renamed initialization keyword from "Init" to "init". Also, the 
    init operator "<-" can be used instead of "init".
  - Renamed the "when" guard on patterns to use keyword "if". In this
    way, "when" can be used in the DSLs.
  - It is now possible to plot a probe that does not exist during the
    whole simulation, e.g., a probe that exists in a specific state.
  - A function can be called either by using space between paranthesis, e.g.
      foo p1 p2
    or to use paranthesis and a comma separated list (as in C or Java)
      foo(p1,p2)
    For the latter, it is important that it there are no white space
    between foo and the left paranhesis. In this case, this is
    translated into a curried call. If foo was supposed to take a tuple
    as argument, it can be called as follows (note the space)
      foo (p1,p2)

* Version 1.0.0 - October 1, 2010
  - First version corresponding to David Broman's PhD Thesis.
  - Changed operators for Real and Int, so that e.g., -. and +. are
    for integer operations instead of Real. The rationale is that most
    expressions are written using Reals.
