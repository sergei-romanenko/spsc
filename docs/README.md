# A Small Positive Supercompiler in Scala, Haskell, Python, Ruby & JavaScript

> This site is associated with the project <https://github.com/sergei-romanenko/spsc>.

A [supercompiler](http://sites.google.com/site/keldyshscp/Home/supercompilerconcept)
is a program transformer that traces the possible generalized histories
of computation by the source program, and builds an equivalent target
program, whose structure is, in a sense, "simpler" than the structure of
the source program. The simplification is achieved by removing certain
"redundant" actions from the source program.

The goal of the project is to implement in
[Scala](http://www.scala-lang.org) a
[positive supercompiler](http://sites.google.com/site/keldyshscp/Home/positive-supercompilers)
that is very close to the one described in papers by M.H.Sørensen,
R.Glück, and N.D.Jones.

The current version of spsc can be run as a command-line application or
accessed as a [web-application](http://spsc.appspot.com/).

SPSC deals with programs written in [SLL](SimpleLazyFirstOrderLanguage.md), a 
simple lazy first-order functional language. The intended operational semantics 
of the language is normal-order graph reduction to weak head normal form.

## SPSC Lite

SPSC Lite is a minimalistic version of SPSC, which may be useful for
educational purposes and as a starting point for experimenting with
supercompilation.

Currently, there are 4 versions of SPSC Lite written in: Scala, Haskell, Python 
2.6 and Ruby 1.8.

## News

**2009, September** A version of SPSC Lite written in Ruby 1.8.

**2009, August** A version of SPSC Lite written in Python 2.6.

**2009, August** A version of SPSC Lite written in Haskell.

**2009, June** The paper
[SPSC: Суперкомпилятор на языке Scala](https://storage.googleapis.com/google-code-archive-downloads/v2/code.google.com/spsc/Klyuchnikov,Romanenko-2009--SPSC-.Superkompilyator.na.yazyke.Scala.pdf).

**2009, May** The paper
[SPSC: a Simple Supercompiler in Scala](https://storage.googleapis.com/google-code-archive-downloads/v2/code.google.com/spsc/Klyuchnikov__Romanenko__SPSC_a_Simple_Supercompiler_in_Scala.pdf)
is accepted for
[PU’09](http://psi.nsc.ru/psi09/p_understanding/index).

## Documentation

* Concepts
    - The concept of a supercompiler
      <http://sites.google.com/site/keldyshscp/Home/supercompilerconcept>
    - Positive supercompilers
      <http://sites.google.com/site/keldyshscp/Home/positive-supercompilers>
    - [Input language](SimpleLazyFirstOrderLanguage.md) (SLL)
* Getting started with SPSC [command line application](GettingStarted.md)
* Getting started with SPSC [web application (obsolete)](GettingStartedWeb.md)
* SPSC web interface (new) <http://spsc.appspot.com/>
* [Sample tasks](SampleTasks.md)
