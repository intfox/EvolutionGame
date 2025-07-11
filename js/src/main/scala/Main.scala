import Interpreter.Command.{GoTo, Label, Move}
import Interpreter.Direction.Up
import org.scalajs.dom
import org.scalajs.dom.{HTMLCanvasElement, HTMLElement, SVGElement, html}
import scala.scalajs.js.annotation._

import scala.scalajs.js.timers.{clearInterval, setInterval}

@JSExportTopLevel("MainPage")
object MainPage {
  @JSExport
  def run(): Unit = {
    println(s"JS: ${Test.hi()}")

    var world: World | Null = null

    var speed = 1
    var observedUnitId = 0

    checkBoxView()
    def startInterval() =
      setInterval(1000 / speed) {
        if (world != null && world.units.nonEmpty) {
          world.tick()
          WorldCanvas.render(world)
          updateStateView()
          updateStatisticsView(world.statistics.toMap)
        }
      }

    def updateStateView(): Unit = {
      world.units.find(_.id == observedUnitId) match {
        case Some(unit) =>
          updateTable(unit)
          updateEnergy(unit.energy)
        case None => ()
      }
    }


    WorldCanvas.init((x, y) => {
      if (world != null) {
        for (unit <- world.units) {
          if (unit.x == x && unit.y == y) {
            observedUnitId = unit.id
            WorldCanvas.observedUnitId = observedUnitId
          }
        }
      }
      updateStateView()
      WorldCanvas.render(world)
    })
    var intervalHandler = startInterval()

    val anotherGenealogy = scala.collection.mutable.ListBuffer.empty[Code]

    dom.document.getElementById("start").asInstanceOf[html.Button].onclick = (_) =>
      world = World(for {
        (genealogyId, code) <- Editor.getFromStorage()
        checkbox = dom.document.getElementById(s"check$genealogyId").asInstanceOf[html.Input]
        parsedCode = Interpreter.parse(code) if checkbox.checked
      } yield PlayerUnit(genealogyId, Code(parsedCode)), 10)

    dom.document.getElementById("speed1").asInstanceOf[html.Button].onclick = (_) =>
      if (speed != 1) {
        speed = 1
        clearInterval(intervalHandler)
        intervalHandler = startInterval()
      }
    dom.document.getElementById("speed2").asInstanceOf[html.Button].onclick = (_) => if (speed != 2) {
      speed = 2
      clearInterval(intervalHandler)
      intervalHandler = startInterval()
    }
    dom.document.getElementById("speed4").asInstanceOf[html.Button].onclick = (_) => if (speed != 4) {
      speed = 4
      clearInterval(intervalHandler)
      intervalHandler = startInterval()
    }
  }
}

def updateTable(unit: UnitObject): Unit = {
  for ((value, i) <- unit.program.state.array.zipWithIndex) {
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

def updateStatisticsView(map: Map[String, Statistic]): Unit = {
  val tableBody = dom.document.getElementById("statisticsTableBody").asInstanceOf[html.TableSection]

  // Clear existing rows to avoid duplicates or outdated information
  // Alternatively, you could iterate and update existing rows if they match,
  // but clearing and re-adding is often simpler for dynamic tables.
  while (tableBody.hasChildNodes()) {
    tableBody.removeChild(tableBody.firstChild)
  }

  for ((genealogyId, stat) <- map) {
    val row = tableBody.insertRow().asInstanceOf[html.TableRow]

    val idCell = row.insertCell().asInstanceOf[html.TableCell]
    idCell.textContent = genealogyId

    val countCell = row.insertCell().asInstanceOf[html.TableCell]
    countCell.textContent = stat.unitCount.toString
  }
}


def checkBoxView(): Unit = {
  val gCheckboxes = dom.document.getElementById("gCheckboxes").asInstanceOf[html.Div]

  for((genealogyId, code) <- Editor.getFromStorage()) {
//    println()
    val input = dom.document.createElement("input").asInstanceOf[html.Input]
    input.`type` = "checkbox"
    input.id = s"check$genealogyId"
    val label = dom.document.createElement("label").asInstanceOf[html.Label]
    label.textContent = genealogyId
    label.htmlFor = genealogyId
    gCheckboxes.appendChild(input)
    gCheckboxes.appendChild(label)
  }
}