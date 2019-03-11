name := "MonadsInScala"

version := "0.1"

scalaVersion := "2.12.8"

scalacOptions += "-Ypartial-unification"


mainClass in (Compile, run) := Some("CatsExercises.ShowTypeclass")

libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.0"
