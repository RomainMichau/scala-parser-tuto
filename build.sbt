val scala3Version = "3.6.4"

lazy val root = project
  .in(file("."))
  .enablePlugins(ScalafmtPlugin, ScalafixPlugin)
  .settings(
    name         := "jsonParser",
    version      := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    scalacOptions ++= Seq(
      "-Xfatal-warnings", // Treat warnings as errors
      "-Wunused:all",
      "-deprecation",   // Warn on deprecated APIs
      "-feature",       // Warn when using advanced language features
      "-unchecked",     // Enable additional warnings (pattern match exhaustiveness, etc.)
      "-Xcheck-macros", // Check that macros expand correctly (when using macros)
      "-Wvalue-discard",
      "-Yretain-trees"
    ),
    libraryDependencies += "org.scalameta" %% "munit"     % "1.0.0" % Test,
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.13.0",
    Compile / compile                      := (Compile / compile).dependsOn(Compile / scalafmtCheckAll).value,
    Test / compile                         := (Test / compile).dependsOn(Test / scalafmt).value,
    ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always,
    semanticdbEnabled                                                := true,
    semanticdbVersion                                                := scalafixSemanticdb.revision,
    coverageEnabled                                                  := true,
    coverageMinimumBranchTotal                                       := 100,
    coverageMinimumStmtTotal                                         := 100,
    coverageFailOnMinimum                                            := true, // Enforce the threshold
    coverageHighlighting                                             := true
  )
