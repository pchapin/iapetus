import org.scalajs.dom
import org.scalajs.dom.document

object Main {
  def main(args: Array[String]): Unit = {
    appendParagraph("Hello, Scala.js!")
  }

  def appendParagraph(text: String): Unit = {
    val p = document.createElement("p")
    p.textContent = text
    document.body.appendChild(p)
  }
}
