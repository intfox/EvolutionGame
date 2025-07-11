import Interpreter.Command.{Move, Reproduce, Wait}
import Interpreter.{Command, Direction, ObjectType, arraySize}

import scala.collection.mutable
import scala.util.{Failure, Random, Success}

class World(playerUnits: List[PlayerUnit], seed: Int) {
  val rand = Random(seed)
  var worldTick = 0
  val countFoodEveryTick = 5
  val width = 100
  val height = 50
  val units = mutable.ListBuffer.empty[UnitObject]
  val foods = mutable.ListBuffer.empty[Food]

  for (playerUnit <- playerUnits) {
    units.addOne(unitObject(playerUnit))
  }

  def tick(): Unit = {
    worldTick += 1
    for (i <- 0 to countFoodEveryTick) {
      val food = Food(energy = 100, x = rand.nextInt(width), y = rand.nextInt(height))
      if (!foods.exists(f => f.x == food.x && f.y == food.y)) foods.addOne(food)
    }

    for ((unit, i) <- units.zipWithIndex) {
      val scan = (objType: ObjectType) => {
        val x = unit.x
        val y = unit.y
        val listCells = List(
          (x - 1, y - 4), (x, y - 4), (x + 1, y - 4),
          (x - 3, y - 3), (x - 2, y - 3), (x - 1, y - 3), (x, y - 3), (x + 1, y - 3), (x + 2, y - 3), (x + 3, y - 3),
          (x - 3, y - 2), (x - 2, y - 2), (x - 1, y - 2), (x, y - 2), (x + 1, y - 2), (x + 2, y - 2), (x + 3, y - 2),
          (x - 4, y - 1), (x - 3, y - 1), (x - 2, y - 1), (x - 1, y - 1), (x, y - 1), (x + 1, y - 1), (x + 2, y - 1), (x + 3, y - 1), (x + 4, y - 1),
          (x - 4, y), (x - 3, y), (x - 2, y), (x - 1, y), (x, y), (x + 1, y), (x + 2, y), (x + 3, y), (x + 4, y),
          (x - 4, y + 1), (x - 3, y + 1), (x - 2, y + 1), (x - 1, y + 1), (x, y + 1), (x + 1, y + 1), (x + 2, y + 1), (x + 3, y + 1), (x + 4, y + 1),
          (x - 3, y + 2), (x - 2, y + 2), (x - 1, y + 2), (x, y + 2), (x + 1, y + 2), (x + 2, y + 2), (x + 3, y + 2),
          (x - 3, y + 3), (x - 2, y + 3), (x - 1, y + 3), (x, y + 3), (x + 1, y + 3), (x + 2, y + 3), (x + 3, y + 3),
          (x - 1, y + 4), (x, y + 4), (x + 1, y + 4),
        )
        val scannedObjects = mutable.ListBuffer.empty[(Int, Int)]
        if (objType == ObjectType.All || objType == ObjectType.Food) {
          for (food <- foods) {
            if (listCells.exists((x, y) => food.x == x && food.y == y)) {
              scannedObjects.addOne((food.x, food.y))
            }
          }
        }
        if (objType == ObjectType.All || objType == ObjectType.Unit) {
          for (anotherUnit <- units) {
            if (listCells.exists((x, y) => anotherUnit.x == x && anotherUnit.y == y)) {
              scannedObjects.addOne((anotherUnit.x, anotherUnit.y))
            }
          }
        }

        scannedObjects.toList.map((x, y) => (x - unit.x, y - unit.y)).sortBy((x, y) => math.abs(x) + math.abs(y))
      }
      val event = scala.util.Try(Interpreter.eval(unit, rand, scan))
      if (unit.energy <= 0) {
        println(s"Die: energy ${unit.energy}")
        units.remove(i)
      } else {
        event match {
          case Success(Move(direction)) => direction match {
            case Direction.Up =>
              if (unit.y > 0 && !units.exists((a) => a.x == unit.x && a.y == unit.y - 1)) unit.y -= 1
            case Direction.Down =>
              if (unit.y < height - 1 && !units.exists((a) => a.x == unit.x && a.y == unit.y + 1)) unit.y += 1
            case Direction.Left =>
              if (unit.x > 0 && !units.exists((a) => a.x == unit.x - 1 && a.y == unit.y)) unit.x -= 1
            case Direction.Right =>
              if (unit.x < width - 1 && !units.exists((a) => a.x == unit.x + 1 && a.y == unit.y)) unit.x += 1
          }
          case Success(Wait()) => ()
          case Success(Reproduce(givingEnergy)) => {
            val possibleForNewUnit = List((unit.x - 1, unit.y), (unit.x, unit.y - 1), (unit.x + 1, unit.y), (unit.x, unit.y + 1))
              .filter((x, y) => !units.exists(aUnit => aUnit.x == x && aUnit.y == y))

            if (possibleForNewUnit.nonEmpty) {
              val (newX, newY) = possibleForNewUnit(rand.nextInt(possibleForNewUnit.length))
              unit.energy -= givingEnergy
              units.addOne(unit.copy(energy = givingEnergy, x = newX, y = newY, program = unit.program.copy(
                state = State(array = Array.fill(arraySize)(0), currentLine = -1)
              )))
            }
          }
          case Failure(err) =>
            println(s"Die: ${err.getMessage}")
            units.remove(i)
        }
      }
    }

    for (unit <- units) {
      val foundI = foods.indexWhere(f => f.x == unit.x && f.y == unit.y)
      if (foundI != -1) {
        val energy = foods(foundI).energy
        foods.remove(foundI)
        unit.energy += energy
      }
    }
  }

  private def unitObject(userUnit: PlayerUnit): UnitObject =
    UnitObject(
      genealogyId = userUnit.genealogyId,
      x = rand.nextInt(width),
      y = rand.nextInt(height),
      program = Program(
        code = userUnit.code,
        state = State(
          array = Array.fill(arraySize)(0),
          currentLine = -1
        )
      ),
      energy = 200
    )
}

type Event = Move | Wait | Reproduce

case class UnitObject(val genealogyId: String, var x: Int, var y: Int, val program: Program, var energy: Int)

case class Program(val code: Code, val state: State)

case class Code(val commands: Array[Option[Command]])

case class State(val array: Array[Int], var currentLine: Int)

case class Food(val energy: Int, val x: Int, val y: Int)

case class PlayerUnit(val genealogyId: String, val code: Code)