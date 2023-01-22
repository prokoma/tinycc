version := "0.1.0-SNAPSHOT"

scalaVersion := "2.13.10"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"

libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test

testFrameworks += new TestFramework("munit.Framework")

assembly / assemblyJarName := "tinycc.jar"