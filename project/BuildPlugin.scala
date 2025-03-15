import sbt.*, Keys.*
import sbt.plugins.{JvmPlugin, SbtPlugin}

object BuildPlugin extends AutoPlugin {
  override def trigger = allRequirements

  override def requires = JvmPlugin

  override lazy val projectSettings =
    Seq(
      scalaVersion := "3.6.2",
      scalacOptions ++= Seq(
        "-source",
        "future",
        "-Xkind-projector:underscores",
        "-deprecation",
        "-experimental",
        "-Yexplicit-nulls"
      ),
      libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
    )
}
