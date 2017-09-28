import sbt._
import Keys._
import java.util.Date
import sbt.Package.ManifestAttributes

val ceticSpecificsBuild = Def.taskDyn {
  if (System.getProperty("is_cetic")) Def.task {
    lazy val manifestSettings = Seq(
    packageOptions in (Compile, packageBin) += 
         Package.ManifestAttributes( "REVISION_ID" -> System.getProperty("REVISION_ID") ),
         Package.ManifestAttributes( "REVISION_URL" -> "https://bitbucket.org/oscarlib/oscar/commits/"+System.getProperty("REVISION_ID") ),
         Package.ManifestAttributes( "JENKINS_BUILD_ID" -> System.getProperty("BUILD_ID") ),
         Package.ManifestAttributes( "BUILD_DATE" -> new Date().toString() )
    )
  
    publishTo := {
      val artifactory = "http://maven.oscar.ext.cetic.be:8081/artifactory/"
      if (isSnapshot.value)
        Some("Artifactory Realm" at artifactory + "sbt-dev;build.timestamp=" + new java.util.Date().getTime) 
      else
        Some("Artifactory Realm" at artifactory + "sbt-release")
    }
  } 
}