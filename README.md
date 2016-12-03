# Advent of Code in Scala

Some of the developers at
[@DetroitLabs](http://github.com/detroitlabs) thought it would fun to
do the exercises from [Advent of Code](http://adventofcode.com)
together, so here are my humble submissions, probably all in
[Scala](http://scala-lang.org/)

## Prerequisites

You'll need the
[simple build tool](http://www.scala-sbt.org/download.html). Installing
this should be all you need to run the project, as it should pull down
the appropriate version of Scala as needed.

## Running

- To run the test suite

    `sbt test`

- To get an answer for a specific day, append the appropriate day to
  the package name `com.sleepynate.adventofcode.`, for example, to
  print Day 1's answer, you'd use:

    `sbt "run-main com.sleepynate.adventofcode.Day1"`

- Or, you could just be wild and run `sbt run` and see what happens


Happy holiday hacking!

- [@sleepynate](http://github.com/sleepynate)
