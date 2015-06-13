name := "lunatech-expenses"

version := "1.0-SNAPSHOT"

resolvers += "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

resolvers += "Lunatech Artifactory" at "http://artifactory.lunatech.com/artifactory/releases-public"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  "org.reactivemongo" %% "play2-reactivemongo" % "0.10.2",
  "org.ocpsoft.prettytime" % "prettytime" % "3.2.1.Final",
  "com.typesafe" %% "play-plugins-mailer" % "2.2.0",
  "com.lunatech" %% "play-googleopenconnect" % "1.1"
)

play.Project.playScalaSettings
