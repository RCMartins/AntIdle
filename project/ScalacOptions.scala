object ScalacOptions {

  val allScalacOptions: Seq[String] =
    Seq(
      "-encoding",
      "UTF-8",
      "-feature",
      "-deprecation",
      "-unchecked",
      "-language:implicitConversions",
      "-language:existentials",
      "-language:dynamics",
      "-Xlint:-unused,_",
      "-Ybackend-parallelism",
      "4",
      "-Ycache-plugin-class-loader:last-modified",
      "-Ycache-macro-class-loader:last-modified",
      "-Xnon-strict-patmat-analysis",
      "-Xlint:-strict-unsealed-patmat"
    )

}
