name := "formaggio"

organization := "com.github.wrwills"

scalaVersion := "2.9.0-1"

version := "0.2.2"

//publishTo := Option(Resolver.file("gitpages-local", Path.userHome / "git" / "repository"))

publishTo := Option(Resolver.file("gitpages-local", Path.userHome / "projects" / "scala" / "formaggio" / "repository"))

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "6.0.3",
  "org.specs2" %% "specs2" % "1.4" % "test"
)

initialCommands := """
import scalaz._
import Scalaz._
import formaggio._
import Formlets._
import SampleData._
"""