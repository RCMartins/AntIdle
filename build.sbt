ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.14"

lazy val root =
  (project in file("."))
    .enablePlugins(ScalaJSPlugin)
    .settings(
      name := "AntIdle",
      scalacOptions ++= ScalacOptions.allScalacOptions,
      scalaJSUseMainModuleInitializer := true,
      libraryDependencies ++= Seq(
        "com.raquo"                  %%% "laminar"                 % "17.1.0",
        "com.raquo"                  %%% "laminar-shoelace_sjs" % "0.1.0",
        "dev.zio"                    %%% "zio"                     % "2.1.9",
        "dev.zio"                    %%% "zio-json"                % "0.7.3",
        "dev.zio"                    %%% "zio-prelude"             % "1.0.0-RC31",
        "com.softwaremill.quicklens" %%% "quicklens"               % "1.9.8",
        // java.time library support for Scala.js
        "io.github.cquiroz" %%% "scala-java-time"      % "2.6.0",
        "io.github.cquiroz" %%% "scala-java-time-tzdb" % "2.6.0",
      )
    )

Global / onChangedBuildSource := ReloadOnSourceChanges

addCommandAlias("c", "fastOptJS")
