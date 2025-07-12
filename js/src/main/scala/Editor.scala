
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
  case class Test(str: String)
  @JSExport
  def run(): Unit = {
    println("Run editor")
    if(dom.window.localStorage.getItem("editorStorage") == null) {
      dom.window.localStorage.setItem("editorStorage", List.empty[(String, String)].asJson.toString)
    }
    val selectCode = dom.document.getElementById("selectCode").asInstanceOf[html.Select]
    for((genId, _) <- getFromStorage()) {
      val option = dom.document.createElement("option").asInstanceOf[html.Option]
      option.value = genId
      option.textContent = genId
      selectCode.appendChild(option)
    }

  }

  @JSExport
  def onSelect(): Unit = {
    val selectCode = dom.document.getElementById("selectCode").asInstanceOf[html.Select]
    val codeTextarea = dom.document.getElementById("codeInput").asInstanceOf[html.TextArea]
    val genealogyIdInput = dom.document.getElementById("genealogyIdInput").asInstanceOf[html.Input]
    codeTextarea.value = getFromStorage().find((genId, _) => genId == selectCode.value).get._2
    genealogyIdInput.value = selectCode.value
    js.Dynamic.global.updateLineNumbers()
//    println(selectCode.value)
  }

  @JSExport
  def save(): Unit = {

//    val enterButton = dom.document.getElementById("enterBtn").asInstanceOf[html.Button]
    val codeTextarea = dom.document.getElementById("codeInput").asInstanceOf[html.TextArea]
    val genealogyIdInput = dom.document.getElementById("genealogyIdInput").asInstanceOf[html.Input]
    val code = try {
      Interpreter.parse(codeTextarea.value)
      val noEmptyName = genealogyIdInput.value match {
        case s if s.isBlank => "Empty name"
        case s => s
      }
      saveToStorage(noEmptyName, codeTextarea.value)
      dom.document.getElementById("error").asInstanceOf[html.Heading].textContent = ""
      dom.document.getElementById("success").asInstanceOf[html.Heading].textContent = "Program saved"
    } catch {
      case err: Throwable =>
        val errorText = dom.document.getElementById("error").asInstanceOf[html.Heading]
        errorText.textContent = s"${err.getMessage}"
    }
  }

  def saveToStorage(generationId: String, code: String) = {
    val store = getFromStorage()
    dom.window.localStorage.setItem("editorStorage", (store.filter((genId, _) => genId != generationId) :+ (generationId, code)).asJson.toString)
  }

  def getFromStorage(): List[(String, String)] = decode[List[(String, String)]](dom.window.localStorage.getItem("editorStorage")).toOption.get
}
