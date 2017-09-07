publishTo := {
  val artifactory = "http://maven.oscar.ext.cetic.be:8081/artifactory/"
  if (isSnapshot.value)
    Some("Artifactory Realm" at artifactory + "sbt-dev") 
  else
    Some("Artifactory Realm" at artifactory + "sbt-release")
}
