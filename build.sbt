name := "lunatech-expenses"

version := "1.0-SNAPSHOT"

resolvers += "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  "org.reactivemongo" %% "play2-reactivemongo" % "0.10.0-SNAPSHOT",
  "org.ocpsoft.prettytime" % "prettytime" % "3.2.1.Final",
  "com.typesafe" %% "play-plugins-mailer" % "2.2.0" 
)     

play.Project.playScalaSettings
