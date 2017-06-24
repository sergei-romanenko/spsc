# Simple Positive SuperCompiler in Scala

### Working web demonstration

<http://spsc.appspot.com/all>

### Documentation at googlecode

<https://code.google.com/p/spsc/>

### Hacking

__Prerequisites__: Install google appengine Java SDK (see [sbt-appengine](https://github.com/sbt/sbt-appengine) for details).

Projects:

* `spcs` (Scala) - the main (core) project
* `spcs_back` (Scala) - simple SPSC backend (targeted for Google app engine)
* `spcs_front` (Python) - simple SPSC frontend deployed at Google app engine (it uses `spcs_back` for supercompilation)


Running:

    $ export APPENGINE_SDK_HOME=/Users/lambdamix/apps/appengine-java-sdk-1.9.5;
    $ sbt
    > test
    > project spsc
    > runMain spsc.Samples

