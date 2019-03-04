name := "MonadsInScala"

version := "0.1"

scalaVersion := "2.12.8"

// Currently testing State monad code
mainClass in (Compile, run) := Some("myState.DriveStateMonad")