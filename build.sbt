ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    organization := "come.codepowered.avro-based",
    name := "avro-based",
    publishArtifact := false,
  )

lazy val `avsc-gen-scala` = (project in file("avsc-gen-scala"))
  .settings(
    libraryDependencies += "com.eed3si9n" %% "treehugger" % "0.4.4",
    libraryDependencies += "org.apache.avro" % "avro" % "1.11.3",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.17",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % Test,
    licenses := Seq("Apache 2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    homepage := Some(url("https://github.com/LeonPoon/gen-from-avro")),
    pomExtra := (
      <scm>
        <url>git://github.com/LeonPoon/gen-from-avro.git</url>
        <connection>scm:git://github.com/LeonPoon/gen-from-avro.git</connection>
      </scm>
        <developers>
          <developer>
            <id>szeleung.poon</id>
            <name>Leon poon</name>
            <url>https://github.com/LeonPoon</url>
          </developer>
        </developers>)
  )
