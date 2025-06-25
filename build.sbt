import scala.sys.process._

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.5"

lazy val root = (project in file("."))
  .settings(
    name := "Gredex"
  )

resolvers += "Akka library repository".at("https://repo.akka.io/maven")

val AkkaVersion = "2.9.3"
val AkkaHttpVersion = "10.6.3"
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion,
  "com.typesafe.akka" %% "akka-stream" % AkkaVersion,
  "com.typesafe.akka" %% "akka-http" % AkkaHttpVersion,
  "com.typesafe.akka" %% "akka-http-spray-json" % AkkaHttpVersion
)

libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"

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
