if(System.setProperty("is_cetic")){
  publishTo := {
    val artifactory = "http://maven.oscar.ext.cetic.be:8081/artifactory/"
    if (isSnapshot.value)
      Some("Artifactory Realm" at artifactory + "sbt-dev;build.timestamp=" + new java.util.Date().getTime) 
    else
      Some("Artifactory Realm" at artifactory + "sbt-release")
  }
}