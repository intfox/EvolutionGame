import Interpreter.Command.{GoTo, Label, Move}
import Interpreter.Direction.Up
import org.scalajs.dom
import org.scalajs.dom.{HTMLCanvasElement, HTMLElement, SVGElement, html}

import scala.scalajs.js.timers.{clearInterval, setInterval}

@main def main(): Unit = {
  println(s"JS: ${Test.hi()}")

  var world: World | Null = null

  var speed = 1
  var observedUnitId = 0

  def startInterval() =
    setInterval(1000 / speed) {
      if (world != null && world.units.nonEmpty) {
        println(s"Units count: ${world.units.length}")
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

  val enterButton = dom.document.getElementById("enterBtn").asInstanceOf[html.Button]
  val codeTextarea = dom.document.getElementById("code").asInstanceOf[html.TextArea]

  val anotherGenealogy = scala.collection.mutable.ListBuffer.empty[Code]

  // Add a click event listener to the button
  enterButton.onclick = { _ =>
    // Print the text from the textarea to the console
    val code = Interpreter.parse(codeTextarea.value)
    //    world = World(UnitObject("test", 0, 0, Program(Code(code), State(Array.empty, 0)), 200), 10)
    world = World(PlayerUnit("test", Code(code)) +: anotherGenealogy.toList.zipWithIndex.map((c, i) => PlayerUnit(s"test${i}", c)), 10)
    observedUnitId = world.units(0).id
    WorldCanvas.observedUnitId = observedUnitId
    //    world = World(List(PlayerUnit("test", Code(code))), 10)
    anotherGenealogy.addOne(Code(code))
    println(codeTextarea.value)
  }

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

  for((genealogyId, stat) <- map) {
    val row = tableBody.insertRow().asInstanceOf[html.TableRow]

    val idCell = row.insertCell().asInstanceOf[html.TableCell]
    idCell.textContent = genealogyId

    val countCell = row.insertCell().asInstanceOf[html.TableCell]
    countCell.textContent = stat.unitCount.toString
  }
}

