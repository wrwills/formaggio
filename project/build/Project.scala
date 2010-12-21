import sbt._

class Project(info: ProjectInfo) extends ParentProject(info)
{
  val scalazVersion = "5.1-SNAPSHOT"

  val scalaToolsSnapshots = "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"

  val sonatypeNexusSnapshots = "Sonatype Nexus Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
  // For Scalate
  val fuseSourceSnapshots = "FuseSource Snapshot Repository" at "http://repo.fusesource.com/nexus/content/repositories/snapshots"

  lazy val core = 
    project(
      "core", 
      "core", 
      new DefaultProject(_) {
	val scalazCore = "com.googlecode.scalaz" %% "scalaz-core" % scalazVersion

	val specs = "org.scala-tools.testing" %% "specs" % "1.6.6" % "test"
	val scalaCheck = "org.scala-tools.testing" %% "scalacheck" % "1.7" % "test"  

	override def consoleInit =
"""
import scalaz._
import Scalaz._
import scormlets.Formlets._
"""

      })

  lazy val scalatra_example =
    project(
      "scalatra_example", 
      "scalatra_example", 
      new DefaultWebProject(_) {
	  val scalatraVersion = "2.0.0.M2"
	val scalatra = "org.scalatra" %% "scalatra" % scalatraVersion
	val scalate = "org.scalatra" %% "scalatra-scalate" % scalatraVersion
	val servletApi = "org.mortbay.jetty" % "servlet-api" % "2.5-20081211" % "provided"

	// Alternatively, you could use scalatra-specs
	val scalatest = "org.scalatra" %% "scalatra-scalatest" % scalatraVersion % "test"

	// Pick your favorite slf4j binding
	val slf4jBinding = "ch.qos.logback" % "logback-classic" % "0.9.25" % "runtime"

	// http://groups.google.com/group/simple-build-tool/msg/1f17b43807d06cda
	override def testClasspath = super.testClasspath +++ buildCompilerJar

      }, core)



}
