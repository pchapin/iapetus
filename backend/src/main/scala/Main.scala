
import cats.effect.{IO, IOApp}
import com.comcast.ip4s.*
import org.http4s.*
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.headers.`Content-Type`

object Main extends IOApp.Simple {

  val routes: HttpRoutes[IO] = HttpRoutes.of[IO] {

    case GET -> Root =>
      Ok(indexHtml, `Content-Type`(MediaType.text.html))

    case GET -> Root / "main.js" =>
      val projectRoot = sys.props.getOrElse("iapetus.root", ".")
      val jsFile = java.nio.file.Paths.get(projectRoot,
        "frontend/target/scala-3.3.4/frontend-fastopt/main.js")
      if java.nio.file.Files.exists(jsFile) then
        Ok(java.nio.file.Files.readString(jsFile),
          `Content-Type`(MediaType.application.javascript))
      else
        NotFound("main.js not found -- did you run frontend/fastOptJS?")
  }

  val indexHtml: String = """<!DOCTYPE html>
                            |<html>
                            |  <head><title>Iapetus</title></head>
                            |  <body>
                            |    <script src="/main.js"></script>
                            |  </body>
                            |</html>""".stripMargin

  val run: IO[Unit] =
    EmberServerBuilder
      .default[IO]
      .withHost(ipv4"0.0.0.0")
      .withPort(port"8080")
      .withHttpApp(routes.orNotFound)
      .build
      .useForever
}
