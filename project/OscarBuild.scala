package oscar

import sbt.Keys._
import sbt._



object OscarBuild {

  lazy val PerfTest = config("perf") extend (Test)

  lazy val buildSettings = Seq(
    organization := "oscar",
    version := "4.1.0-SNAPSHOT",
    scalaVersion := "2.12.4",
    sbtVersion := "1.1.4"
  )

  lazy val commonSettings = buildSettings ++ Defaults.coreDefaultSettings ++ Seq(
    scalacOptions in Compile ++= Seq("-encoding", "UTF-8", "-deprecation", "-feature",
      "-unchecked", "-Xdisable-assertions", "-language:implicitConversions",
      "-language:postfixOps"),
    scalacOptions in Test := Seq("-optimise"),
    testOptions in Test += ((target in Test) map {
      t => Tests.Argument(TestFrameworks.ScalaTest, "-u","<%s>" format (t / "streams/test"))
    }).value,
    parallelExecution in Test := false,
    fork in Test := true,
    javaOptions in Test += "-Djava.library.path=../lib:../lib/",
    javacOptions ++= Seq("-encoding", "UTF-8"),
    unmanagedSourceDirectories in Test += baseDirectory.value / "src" / "main" / "examples",
    publishTo := {
      val artifactoryName = "Artifactory Realm"
      val artifactoryUrl = "http://130.104.230.89/artifactory/"
      if (isSnapshot.value)
        Some(artifactoryName at artifactoryUrl + "libs-snapshot-local;build.timestamp=" + new java.util.Date().getTime)
      else
        Some(artifactoryName at artifactoryUrl + "libs-release-local")
    },
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
    testOptions in PerfTest += ((target in PerfTest) map {
      t => Tests.Argument(TestFrameworks.ScalaTest, "-u","<%s>" format (t / "streams/test"))
    }).value,
    fork in PerfTest := true,
    parallelExecution in PerfTest := false
  )

  object Resolvers {
    val xypron = "Xypron Release" at "http://rsync.xypron.de/repository/"
    val leadoperations = "AWS S3 Release Repository" at "http://maven.leadoperations.co/release"
    val cogcomp = "Cognitive Computation Group" at "http://cogcomp.cs.illinois.edu/m2repo/"
    val ingi = "INGI Snapshots" at "http://artifactory.info.ucl.ac.be/artifactory/libs-snapshot-local/"
  }

  object Dependencies {
    // Regular libraries
    val antlr4Runtime = "org.antlr" % "antlr4-runtime" % "latest.milestone"
    val jcommon = "org.jfree" % "jcommon" % "latest.milestone"
    val jfreechart = "org.jfree" % "jfreechart" % "latest.milestone"
    val jsci = "net.sf.jsci" % "jsci" % "latest.milestone"
    val scalaParserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "latest.milestone"
    val scalaXml = "org.scala-lang.modules" %% "scala-xml" % "1.1.0"
    val scalaSwing = "org.scala-lang.modules" %% "scala-swing" % "latest.milestone"
    val swingx = "org.swinglabs" % "swingx" % "latest.milestone"//"1.0"
    val swingxWs = "org.swinglabs" % "swingx-ws" % "1.0"
    val xmlApisExt = "xml-apis" % "xml-apis-ext" % "latest.milestone"
    //    val xcsp3 = "xcsp3"  % "xcsp3" % "1.0.0-SNAPSHOT"
    val xcsp3 = "org.xcsp" % "xcsp3-tools" % "1.0.0"
    val graphStreamCore = "org.graphstream" % "gs-core" % "1.3"
    val graphStreamAlgo = "org.graphstream" % "gs-algo" % "1.3"
    val graphStreamUI = "org.graphstream" % "gs-ui" % "1.3"
    val scallop = "org.rogach" % "scallop_2.11" % "1.0.0"

    // Akka
    val akkaActor = "com.typesafe.akka" %% "akka-actor" % "2.5.12"
    val akkaRemote = "com.typesafe.akka" %% "akka-remote" % "2.5.12"

    // Test libraries
    val junit = "junit" % "junit" % "latest.milestone" % Test
    val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.14.0" % Test
    val scalaTest = "org.scalatest" %% "scalatest" % "3.2.0-SNAP10" % Test

    val junit2 = "junit" % "junit" % "latest.milestone" % PerfTest
    val scalaCheck2 = "org.scalacheck" %% "scalacheck" % "1.14.0" % PerfTest
    val scalaTest2 = "org.scalatest" %% "scalatest" % "3.2.0-SNAP10" % PerfTest

    val testDeps = Seq(junit, scalaCheck, scalaTest)
  }


}
