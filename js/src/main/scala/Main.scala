import Interpreter.Command.{GoTo, Label, Move}
import Interpreter.Direction.Up
import org.scalajs.dom
import org.scalajs.dom.{HTMLCanvasElement, html}

import scala.scalajs.js.timers.{ setInterval, clearInterval }

@main def main(): Unit = {
  println(s"JS: ${Test.hi()}")

  var world: World | Null = null

  var speed = 1

  def startInterval() =
    setInterval(1000 / speed) {
      if (world != null && world.units.nonEmpty) {
        println(s"Units count: ${world.units.length}")
        world.tick()
        WorldCanvas.render(world)
        if (world.units.nonEmpty) {
          updateTable(world)
          updateEnergy(world.units(0).energy)
        }
      }
    }

  var intervalHandler = startInterval()

  val enterButton = dom.document.getElementById("enterBtn").asInstanceOf[html.Button]
  val codeTextarea = dom.document.getElementById("code").asInstanceOf[html.TextArea]

  // Add a click event listener to the button
  enterButton.onclick = { _ =>
    // Print the text from the textarea to the console
    val code = Interpreter.parse(codeTextarea.value)
    world = World(UnitObject("test", 0, 0, Program(Code(code), State(Array.empty, 0)), 200), 10)
    println(codeTextarea.value)
  }

  dom.document.getElementById("speed1").asInstanceOf[html.Button].onclick = (_) =>
    if(speed != 1) {
      speed = 1
      clearInterval(intervalHandler)
      intervalHandler = startInterval()
    }
  dom.document.getElementById("speed2").asInstanceOf[html.Button].onclick = (_) => if(speed != 2) {
    speed = 2
    clearInterval(intervalHandler)
    intervalHandler = startInterval()
  }
  dom.document.getElementById("speed4").asInstanceOf[html.Button].onclick = (_) => if(speed != 4) {
    speed = 4
    clearInterval(intervalHandler)
    intervalHandler = startInterval()
  }

}

def updateTable(world: World): Unit = {
  for((value, i) <- world.units(0).program.state.array.zipWithIndex) {
    updateCell(s"A$i", value.toString)
  }
}

def updateCell(id: String, text: String): Unit = {
  val cell = dom.document.getElementById(id).asInstanceOf[html.TableCell]
  cell.textContent = text
}

def updateEnergy(energy: Int): Unit = {
  val doc = dom.document.getElementById("energy").asInstanceOf[html.Heading]
  doc.textContent = s"Energy: $energy"
}

