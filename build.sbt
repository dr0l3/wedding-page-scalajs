organization := "droletours"
name := "Scalajsplayground"
version := "0.1.0"

scalaVersion := "2.12.6"
val circeVersion = "0.10.0"

libraryDependencies ++= Seq(
  "com.raquo" %%% "laminar" % "0.5",
  "org.typelevel" %%% "cats-effect" % "1.1.0",
  "com.propensive" %%% "magnolia" % "0.10.0",
  "io.surfkit" %%% "scalajs-google-maps" % "0.0.3-SNAPSHOT",
  "org.scalatest" %%% "scalatest" % "3.0.5" % Test,
  "io.scalajs.npm" %%% "aws-s3" % "0.4.2"

) ++ Seq(
  "io.circe" %%% "circe-core",
  "io.circe" %%% "circe-generic",
  "io.circe" %%% "circe-parser"
).map(_ % circeVersion)

enablePlugins(ScalaJSBundlerPlugin)
scalacOptions += "-P:scalajs:sjsDefinedByDefault"
useYarn := true // makes scalajs-bundler use yarn instead of npm
requiresDOM in Test := true
scalaJSUseMainModuleInitializer := true
scalaJSModuleKind := ModuleKind.CommonJSModule // configure Scala.js to emit a JavaScript module instead of a top-level script

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
resolvers += Resolver.sonatypeRepo("releases")


scalacOptions ++=
  "-encoding" :: "UTF-8" ::
  "-unchecked" ::
  "-deprecation" ::
  "-explaintypes" ::
  "-feature" ::
  "-language:_" ::
  "-Xfuture" ::
  "-Xlint" ::
  "-Ypartial-unification" ::
  "-Yno-adapted-args" ::
  "-Ywarn-extra-implicit" ::
  "-Ywarn-infer-any" ::
  "-Ywarn-value-discard" ::
  "-Ywarn-nullary-override" ::
  "-Ywarn-nullary-unit" ::
  Nil


npmDependencies in Compile += "uuid" -> "3.1.0"
npmDependencies in Compile += "leaflet" -> "1.3.4"
npmDependencies in Compile += "aws-sdk" -> "2.28.0"


// hot reloading configuration:
// https://github.com/scalacenter/scalajs-bundler/issues/180
addCommandAlias("dev", "; compile; fastOptJS::startWebpackDevServer; devwatch; fastOptJS::stopWebpackDevServer")
addCommandAlias("devwatch", "~; fastOptJS; copyFastOptJS")
addCommandAlias("wp", "fastOptJS::webpack")
addCommandAlias("copy", "copyFastOptJS")
addCommandAlias("prod", "fullOptJS")

version in webpack := "4.16.1"
version in startWebpackDevServer := "3.1.4"
webpackDevServerExtraArgs := Seq("--progress", "--color")
webpackConfigFile in fastOptJS := Some(baseDirectory.value/ "webpack.config.dev.js")

webpackBundlingMode in fastOptJS := BundlingMode.LibraryOnly() // https://scalacenter.github.io/scalajs-bundler/cookbook.html#performance
webpackMonitoredDirectories += (crossTarget in (Compile, fastOptJS)).value / "facades"

// when running the "dev" alias, after every fastOptJS compile all artifacts are copied into
// a folder which is served and watched by the webpack devserver.
// this is a workaround for: https://github.com/scalacenter/scalajs-bundler/issues/180
lazy val copyFastOptJS = TaskKey[Unit]("copyFastOptJS", "Copy javascript files to target directory")
copyFastOptJS := {
  val inDir = (crossTarget in (Compile, fastOptJS)).value
  val outDir = (crossTarget in (Compile, fastOptJS)).value / "dev"
  
  val files = Seq(name.value.toLowerCase + "-fastopt-loader.js", name.value.toLowerCase + "-fastopt.js") map { p => (inDir / p, outDir / p) }
  IO.copy(files, overwrite = true, preserveLastModified = true, preserveExecutable = true)
}
