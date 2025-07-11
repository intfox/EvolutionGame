
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
      println(s"create option: ${genId}")
      println(s"select: ${selectCode}")
      val option = dom.document.createElement("option").asInstanceOf[html.Option]
      option.value = genId
      option.textContent = genId
      selectCode.appendChild(option)
    }
    //    Interpreter.Command.fromOrdinal()
//    val test: Interpreter.ArrayAddress | Int = Interpreter.ArrayAddress(123)
//    println(s"Test: ${decode[Interpreter.ArrayAddress](test.asJson.toString)}")
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
      saveToStorage(genealogyIdInput.value, codeTextarea.value)
      dom.document.getElementById("error").asInstanceOf[html.Heading].textContent = ""
      dom.document.getElementById("success").asInstanceOf[html.Heading].textContent = "Program saved"
    } catch {
      case err: Throwable =>
        val errorText = dom.document.getElementById("error").asInstanceOf[html.Heading]
        errorText.textContent = s"${err.getMessage}"
    }

//    println(codeTextarea.value)
    // Add a click event listener to the button
//    enterButton.onclick = { _ =>
      // Print the text from the textarea to the console

      //    world = World(UnitObject("test", 0, 0, Program(Code(code), State(Array.empty, 0)), 200), 10)
//      world = World(PlayerUnit("test", Code(code)) +: anotherGenealogy.toList.zipWithIndex.map((c, i) => PlayerUnit(s"test${i}", c)), 10)
//      observedUnitId = world.units(0).id
//      WorldCanvas.observedUnitId = observedUnitId
      //    world = World(List(PlayerUnit("test", Code(code))), 10)
//      anotherGenealogy.addOne(Code(code))

//    }
  }

  def saveToStorage(generationId: String, code: String) = {
    val store = getFromStorage()
    dom.window.localStorage.setItem("editorStorage", (store.filter((genId, _) => genId != generationId) :+ (generationId, code)).asJson.toString)
  }

  def getFromStorage(): List[(String, String)] = decode[List[(String, String)]](dom.window.localStorage.getItem("editorStorage")).toOption.get
}
