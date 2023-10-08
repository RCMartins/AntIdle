ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root =
  (project in file("."))
    .enablePlugins(ScalaJSPlugin)
    .settings(
      name := "AntIdle",
      scalacOptions ++= ScalacOptions.allScalacOptions,
      scalaJSUseMainModuleInitializer := true,
      libraryDependencies ++= Seq(
        "com.raquo"                  %%% "laminar"     % "16.0.0",
        "dev.zio"                    %%% "zio"         % "2.0.17",
        "dev.zio"                    %%% "zio-json"    % "0.6.2",
        "dev.zio"                    %%% "zio-prelude" % "1.0.0-RC20",
        "com.softwaremill.quicklens" %%% "quicklens"   % "1.9.6",
        // java.time library support for Scala.js
        "io.github.cquiroz" %%% "scala-java-time-tzdb" % "2.5.0",
      )
    )

Global / onChangedBuildSource := ReloadOnSourceChanges

addCommandAlias("c", "fastOptJS")
