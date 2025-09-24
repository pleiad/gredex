import scala.sys.process._

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.5"

lazy val root = (project in file("."))
  .settings(
    name := "Gredex"
  )

// Versions
val PekkoVersion = "1.0.2"
val PekkoHttpVersion = "1.0.1"

libraryDependencies ++= Seq(
  // core
  "org.apache.pekko" %% "pekko-actor-typed" % PekkoVersion,
  "org.apache.pekko" %% "pekko-stream" % PekkoVersion,

  // HTTP + JSON
  "org.apache.pekko" %% "pekko-http" % PekkoHttpVersion,
  "org.apache.pekko" %% "pekko-http-spray-json" % PekkoHttpVersion
)

libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"

libraryDependencies += "io.github.cdimascio" % "java-dotenv" % "3.2.0"

lazy val buildFrontend =
  taskKey[Unit]("Build frontend with npm and copy to public")

buildFrontend := {
  val log = streams.value.log
  val base = baseDirectory.value
  val frontendDir = base / "frontend"
  val outputDir = frontendDir / "dist"
  val targetDir = base / "src" / "main" / "resources" / "public"

  log.info("🔧 Building frontend...")
  Process("npm install", frontendDir).!
  Process("npm run build", frontendDir).!

  log.info(s"📁 Copying frontend dist to $targetDir...")
  IO.delete(targetDir)
  IO.copyDirectory(outputDir, targetDir)

  log.info("✅ Frontend build complete.")
}
