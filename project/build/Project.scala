import sbt._

class Project(info: ProjectInfo) extends DefaultProject(info)
{
  val scalaToolsSnapshots = "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
  val scalazCore = "com.googlecode.scalaz" %% "scalaz-core" % "5.1-SNAPSHOT"

  val specs = "org.scala-tools.testing" %% "specs" % "1.6.6" % "test"
  val scalaCheck = "org.scala-tools.testing" %% "scalacheck" % "1.7" % "test"  
}
