ThisBuild / scalaVersion := "3.3.4"

lazy val common = project.in(file("common"))
  .settings(
    name := "common"
  )

lazy val backend = project.in(file("backend"))
  .dependsOn(common)
  .settings(
    name := "backend",
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-ember-server" % "0.23.26",
      "org.http4s" %% "http4s-dsl" % "0.23.26"
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
