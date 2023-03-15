version := "0.1.0-SNAPSHOT"

scalaVersion := "2.13.10"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % "test"

Compile / mainClass := Some("tinycc.cli.Main")

assembly / assemblyJarName := "tinycc.jar"
