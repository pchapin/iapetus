ThisBuild / scalaVersion := "3.3.4"

lazy val common = project.in(file("common"))
  .settings(
    name := "common"
  )

lazy val backend = project.in(file("backend"))
  .dependsOn(common)
  .settings(
    name := "backend",
    Compile / run / fork := true,
    Compile / run / javaOptions += s"-Diapetus.root=${(ThisBuild / baseDirectory).value}",
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-ember-server" % "0.23.33",
      "org.http4s" %% "http4s-dsl" % "0.23.33",
      "org.typelevel" %% "cats-effect" % "3.7.0",
      "ch.qos.logback" % "logback-classic" % "1.5.32"
    )
  )

lazy val frontend = project.in(file("frontend"))
  .enablePlugins(ScalaJSPlugin)  // Enable Scala.js only for frontend
  .dependsOn(common)
  .settings(
    name := "frontend",
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies += "com.raquo" %%% "laminar" % "15.0.0"
  )

lazy val root = project.in(file("."))
  .aggregate(backend, frontend, common)
  .settings(
    name := "iapetus"
  )
