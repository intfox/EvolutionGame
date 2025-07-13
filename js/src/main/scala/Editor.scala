
import org.scalajs.dom
import io.circe._
import io.circe.generic.auto._ // Для автоматического вывода Encoder/Decoder
import io.circe.syntax._
import io.circe.parser._
import org.scalajs.dom.html
import scala.scalajs.js.annotation._
import scala.scalajs.js

@JSExportTopLevel("Editor")
object Editor {
  def getFromStorage(): List[GenerationScript] = decode[List[GenerationScript]](dom.window.localStorage.getItem("editorStorage")).toOption.get

  case class GenerationScript(fileName: String, script: String)

  @JSExport
  def validateCode(code: String) = {
    try{
      Interpreter.parse(code)
      js.Dynamic.literal(success = true)
    } catch {
      case err: Throwable =>
        js.Dynamic.literal(success = false, error = err.getMessage)
    }
  }
}
