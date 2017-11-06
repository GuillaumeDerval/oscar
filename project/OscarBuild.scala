package oscar

import sbt._
import sbt.Keys._
import de.johoop.jacoco4sbt.JacocoPlugin._
import xerial.sbt.Pack._
import sbtunidoc.Plugin._
import java.util.Date


object OscarBuild extends Build {

  object BuildSettings {
    val buildOrganization = "oscar"
    val buildVersion = "4.0.0-SNAPSHOT"
    val buildScalaVersion = "2.11.0"
    val buildSbtVersion= "0.13.12"

    val osNativeLibDir = (sys.props("os.name"), sys.props("os.arch")) match {
      case (os, arch) if os.contains("Mac") && arch.endsWith("64") => "macos64"
      case (os, arch) if os.contains("Linux") && arch.endsWith("64") => "linux64"
      case (os, arch) if os.contains("Windows") && arch.endsWith("32") => "windows32"
      case (os, arch) if os.contains("Windows") && arch.endsWith("64") => "windows64"
      case (os, arch) => sys.error("Unsupported OS [${os}] Architecture [${arch}] combo, OscaR currently supports macos64, linux64, windows32, windows64")
    }

    lazy val commonSettings = Defaults.defaultSettings ++  jacoco.settings ++ Seq(
      organization := buildOrganization,
      version := buildVersion,
      scalacOptions in Compile ++= Seq("-encoding", "UTF-8", "-deprecation", "-feature", "-unchecked", "-Xdisable-assertions"),
      scalacOptions in Test := Seq("-optimise"),
      testOptions in Test <+= (target in Test) map {
        t => Tests.Argument(TestFrameworks.ScalaTest, "junitxml(directory=\"%s\")" format (t / "test-reports") ) },
      parallelExecution in Test := false,
      fork in Test := true,
      javaOptions in Test += "-Djava.library.path=../lib:../lib/" + osNativeLibDir,
      javacOptions ++= Seq("-encoding", "UTF-8"),
      scalaVersion := buildScalaVersion,
      unmanagedSourceDirectories in Test += baseDirectory.value / "src" / "main" / "examples",
      publishTo := {
        val artifactoryName = "Artifactory Realm"
        val artifactoryUrl = "http://130.104.230.89/artifactory/"
        if (isSnapshot.value)
          Some(artifactoryName at artifactoryUrl + "libs-snapshot-local;build.timestamp=" + new java.util.Date().getTime)
        else
          Some(artifactoryName at artifactoryUrl + "libs-release-local")
      },
      credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")
    )
  }

  object Resolvers {
    val xypron = "Xypron Release" at "http://rsync.xypron.de/repository/"
    val leadoperations = "AWS S3 Release Repository" at "http://maven.leadoperations.co/release"
    val cogcomp = "Cognitive Computation Group" at "http://cogcomp.cs.illinois.edu/m2repo/"
    val ingi = "INGI Snapshots" at "http://artifactory.info.ucl.ac.be/artifactory/libs-snapshot-local/"
    val mvnrepository = "Maven Repository" at "https://mvnrepository.com/artifact/"
  }

  object Dependencies {

    // Regular libraries
    val antlr4Runtime = "org.antlr" % "antlr4-runtime" % "latest.milestone"
    val glpk = "org.gnu.glpk" % "glpk-java" % "1.0.16"
    val gurobi = "gurobi" % "gurobi" % "5.0.1"
    val lpsolve = "lpsolve" % "lpsolve" % "5.5.2"
    val jcommon = "org.jfree" % "jcommon" % "latest.milestone"
    val jfreechart = "org.jfree" % "jfreechart" % "latest.milestone"
    val jsci = "net.sf.jsci" % "jsci" % "latest.milestone"
    val scalaParserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "latest.milestone"
    val scalaXml = "org.scala-lang.modules" %% "scala-xml" % "latest.milestone"
    val scalaSwing = "org.scala-lang.modules" %% "scala-swing" % "latest.milestone"
    //val swingx = "org.swinglabs" % "swingx" % "latest.milestone"
    // val swingxWs = "org.swinglabs" % "swingx-ws" % "latest.milestone"
    val swingx = "org.swinglabs" % "swingx" % "1.0"
    val swingxWs = "org.swinglabs" % "swingx-ws" % "1.0"
    val xmlApisExt = "xml-apis" % "xml-apis-ext" % "latest.milestone"
    val xcsp3 = "xcsp3"  % "xcsp3_2.11" % "1.0.0-SNAPSHOT"
    val jxmapviewer2 = "org.jxmapviewer" % "jxmapviewer2" % "2.2"
    // Test libraries
    val junit = "junit" % "junit" % "latest.milestone" % Test
    val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.11.+" % Test
    val scalaTest = "org.scalatest" %% "scalatest" % "2.2.+" % Test

    val testDeps = Seq(junit, scalaCheck, scalaTest)
  }

  import BuildSettings._
  import Dependencies._
  import Resolvers._
  import UnidocKeys._


  def ceticSpecificSettings = {
    if(Option(System.getProperty("cetic")).isDefined) Seq(
      publishTo := {
        val artifactory = "http://maven.oscar.ext.cetic.be:8081/artifactory/"
        if (isSnapshot.value)
          Some("Artifactory Realm" at artifactory + "libs-snapshot-local;build.timestamp=" + new java.util.Date().getTime)
        else
          Some("Artifactory Realm" at artifactory + "libs-release-local")
      },
      packageOptions += Package.ManifestAttributes(
        ("REVISION_ID", System.getProperty("REVISION_ID")),
        ("REVISION_URL", ("https://bitbucket.org/oscarlib/oscar/commits/"+System.getProperty("REVISION_ID")) ),
        ("JENKINS_BUILD_ID", System.getProperty("BUILD_ID")),
        ("BUILD_DATE", new Date().toString())
      )
    )
    else Seq()
  }

  lazy val oscar = Project(
    id = "oscar",
    base = file("."),
    settings =
      commonSettings ++
        packSettings ++
        unidocSettings ++
        Seq(libraryDependencies ++= testDeps) :+
        (unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(oscarFzn, oscarFznCbls)),
    aggregate = Seq(oscarAlgebra, oscarAlgo, oscarCbls, oscarCp, oscarCPXcsp3, oscarDfo, oscarLinprog, oscarUtil, oscarVisual, oscarFzn, oscarFznCbls, oscarDes, oscarInvariants)

  )

  lazy val oscarAlgebra = Project(
    id = "oscar-algebra",
    base = file("oscar-algebra"),
    settings =
      commonSettings ++
        Seq(libraryDependencies ++= testDeps)
  )

  lazy val oscarAlgo = Project(
    id = "oscar-algo",
    base = file("oscar-algo"),
    settings =
      commonSettings ++
        Seq(libraryDependencies ++= testDeps),
    dependencies = Seq(oscarUtil, oscarVisual)
  )

  lazy val oscarCbls = Project(
    id = "oscar-cbls",
    base = file("oscar-cbls"),
    settings =
      commonSettings ++
        packAutoSettings ++
        ceticSpecificSettings ++
        Seq(
          resolvers ++= Seq(mvnrepository),
          libraryDependencies ++= testDeps :+ scalaSwing :+ jxmapviewer2,
          packGenerateWindowsBatFile := false
        ),
    dependencies = Seq(oscarVisual)
  )

  lazy val oscarCp = Project(
    id = "oscar-cp",
    base = file("oscar-cp"),
    settings =
      commonSettings ++
        Seq(libraryDependencies ++= testDeps :+ scalaParserCombinators),
    dependencies = Seq(oscarAlgo, oscarVisual)
  )

  lazy val oscarCPXcsp3 = Project(
    id = "oscar-cp-xcsp3",
    base = file("oscar-cp-xcsp3"),
    settings =
      commonSettings ++
        Seq(
          resolvers ++= Seq(ingi),
          libraryDependencies ++= testDeps :+ xcsp3),
    dependencies = Seq(oscarCp)
  )

  // Not included in the root build
  lazy val oscarDes = Project(
    id = "oscar-des",
    base = file("oscar-des"),
    settings =
      commonSettings ++
        Seq(libraryDependencies ++= testDeps :+ jsci),
    dependencies = Seq(oscarInvariants)
  )

  lazy val oscarDfo = Project(
    id = "oscar-dfo",
    base = file("oscar-dfo"),
    settings =
      commonSettings ++
        Seq(libraryDependencies ++= testDeps :+ jcommon :+ jfreechart),
    dependencies = Seq(oscarAlgebra, oscarAlgo, oscarVisual)
  )

  // Not included in the default build
  lazy val oscarFzn = Project(
    id = "oscar-fzn",
    base = file("oscar-fzn"),
    settings =
      commonSettings ++
        Seq(libraryDependencies ++= testDeps :+ antlr4Runtime)
  )

  lazy val oscarFznCbls = Project(
    id = "oscar-fzn-cbls",
    base = file("oscar-fzn-cbls"),
    settings =
      commonSettings ++
        Seq(libraryDependencies ++= testDeps),
    dependencies = Seq(oscarCbls,oscarFzn)
  )

  // Not included in the build
  lazy val oscarInvariants = Project(
    id = "oscar-invariants",
    base = file("oscar-invariants"),
    settings =
      commonSettings ++
        Seq(libraryDependencies ++= testDeps)
  )

  lazy val oscarLinprog = Project(
    id = "oscar-linprog",
    base = file("oscar-linprog"),
    settings =
      commonSettings ++
        Seq(
          resolvers ++= Seq(xypron, leadoperations, cogcomp),
          libraryDependencies ++= testDeps :+ glpk :+ gurobi :+ lpsolve :+ scalaXml
        ),
    dependencies = Seq(oscarAlgebra, oscarVisual)
  )

  lazy val oscarUtil = Project(
    id = "oscar-util",
    base = file("oscar-util"),
    settings =
      commonSettings ++
        Seq(libraryDependencies ++= testDeps :+ scalaXml)
  )

  lazy val oscarVisual = Project(
    id = "oscar-visual",
    base = file("oscar-visual"),
    settings =
      commonSettings ++
        Seq(libraryDependencies ++= testDeps :+ jfreechart :+ swingx :+ swingxWs),
    dependencies = Seq(oscarUtil)
  )
}
