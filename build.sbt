name := "lunatech-expenses"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

resolvers += "Sonatype Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"

resolvers += "Lunatech Artifactory" at "http://artifactory.lunatech.com/artifactory/releases-public"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  "org.reactivemongo" % "play2-reactivemongo_2.10" % "0.11.7.play23",
  "org.ocpsoft.prettytime" % "prettytime" % "3.2.1.Final",
  "com.typesafe.play" %% "play-mailer" % "2.4.1",
  "com.google.gdata" % "core" % "1.47.1",
  "com.lunatech" %% "play-googleopenconnect" % "1.1"
)
