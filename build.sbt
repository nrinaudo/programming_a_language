scalaVersion := "3.6.2"
scalacOptions ++= Seq("-source", "future", "-Xkind-projector:underscores", "-deprecation", "-experimental", "-Yexplicit-nulls")


libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
