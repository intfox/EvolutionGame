import Interpreter.Command.{GoTo, Move}
import Interpreter.Direction

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

object Interpreter {

  val arraySize = 30

  def parse(source: String): Array[Option[Command]] = {
    val labelPattern = "([A-Z]+):".r
    val movePattern = "MOVE (UP|DOWN|RIGHT|LEFT)".r
    val setPattern = "SET A([0-9]+), (A[0-9]+|-?[0-9]+)".r
    val randPattern = "RAND A([0-9]+), ([0-9]+)".r
    val gotoPatternNumber = "GOTO ([0-9]+)".r
    val gotoPatternLabel = "GOTO ([A-Z]+)".r
    val ifPattern = "IF (A[0-9]+|-?[0-9]+) ([<>=]) (A[0-9]+|-?[0-9]+) (GOTO (?:[0-9]+|[A-Z]+))".r
    val addPattern = "ADD A([0-9]+), (A[0-9]+|-?[0-9]+)".r
    val subPattern = "SUB A([0-9]+), (A[0-9]+|-?[0-9]+)".r
    val scanPattern = "SCAN A([0-9]+), (UNIT|FOOD|ALL)".r
    val reproducePattern = "REPRODUCE ([0-9]+)".r
    import Command._
    val commands = ListBuffer.empty[Option[Command]]
    for (line <- source.toUpperCase.split('\n')) {
      val command = if (line.strip().nonEmpty) Some(line.strip() match {
        case labelPattern(labelName) => Label(labelName)
        case movePattern(direction) => direction match {
          case "UP" => Move(Direction.Up)
          case "DOWN" => Move(Direction.Down)
          case "RIGHT" => Move(Direction.Right)
          case "LEFT" => Move(Direction.Left)
        }
        case setPattern(address, addressOrNumber) =>
          Set(
            ArrayAddress(address.toInt),
            unboxArr(addressOrNumber)
          )
        case randPattern(address, maxNumber) => Rand(ArrayAddress(address.toInt), maxNumber.toInt)
        case gotoPatternNumber(lineNumber) => GoTo(lineNumber.toInt)
        case gotoPatternLabel(labelName) => GoTo(labelName)
        case ifPattern(condLeft, operator, condRight, gotoStr) => {
          val gt: GoTo = gotoStr match {
            case gotoPatternNumber(lineNumber) => GoTo(lineNumber.toInt)
            case gotoPatternLabel(labelName) => GoTo(labelName)
          }
          val condition = Condition(
            leftOperand = unboxArr(condLeft),
            rightOperand = unboxArr(condRight),
            operator = operator match {
              case ">" => Operator.Greater
              case "<" => Operator.Less
              case "=" => Operator.Equal
            }
          )
          If(condition, gt)
        }
        case "WAIT" => Wait()
        case addPattern(leftOperand, rightOperand) => Add(ArrayAddress(leftOperand.toInt), unboxArr(rightOperand))
        case subPattern(leftOperand, rightOperand) => Sub(ArrayAddress(leftOperand.toInt), unboxArr(rightOperand))
        case scanPattern(arrayAddress, objectType) => Scan(ArrayAddress(arrayAddress.length), objectType match {
          case "UNIT" => ObjectType.Unit
          case "FOOD" => ObjectType.Food
          case "ALL" => ObjectType.All
        })
        case reproducePattern(givingEnergy) => Reproduce(givingEnergy.toInt)
        case com => throw Error(s"command $com not supported")
      }) else None
      commands.addOne(command)
    }

    def unboxArr(indexOrInt: String): ArrayAddress | Int =
      if (indexOrInt(0) == 'A') ArrayAddress(indexOrInt.drop(1).toInt) else indexOrInt.toInt

    commands.toArray
  }

  def eval(unit: UnitObject, rand: Random, scan: (objectType: ObjectType) => List[(Int, Int)]): Event = {
    if(unit.program.state.currentLine == -1) {
      unit.program.state.currentLine = unit.program.code.commands.indexWhere {
        case Some(Command.Label("START")) => true
        case _ => false
      }
      if(unit.program.state.currentLine == -1) throw EvalError("Label start not found")
    }
    var exit = false
    var event: Event | Null = null
    while (!exit) {
      val curCommand = unit.program.code.commands(unit.program.state.currentLine)
//      println(curCommand)
      curCommand match {
        case None =>
          unit.program.state.currentLine += 1
        case Some(command) => if (unit.energy - commandEnergyCost(command) < 0) {
          unit.energy = 0
        } else {
          unit.energy = unit.energy - commandEnergyCost(command)
          command match {
            case c: Command.Move =>
              event = c
              unit.program.state.currentLine += 1
              exit = true
            case Command.Set(address, ArrayAddress(i)) =>
              unit.program.state.array(address.index) = unit.program.state.array(i)
              unit.program.state.currentLine += 1
            case Command.Set(address, int: Int) =>
              unit.program.state.array(address.index) = int
              unit.program.state.currentLine += 1
            case Command.Scan(address, objectType) =>
              val objects = scan(objectType)
              unit.program.state.array(address.index) = objects.length
              var index = address.index + 1
              for((x, y) <- objects) {
                if(index + 2 < unit.program.state.array.length) {
                  unit.program.state.array(index) = x
                  index += 1
                  unit.program.state.array(index) = y
                  index += 1
                }
              }
              unit.program.state.currentLine += 1
            case Command.Add(operand1, operand2) =>
              unit.program.state.array(operand1.index) = unboxAddr(operand1) + unboxAddr(operand2)
              unit.program.state.currentLine += 1
            case Command.Sub(operand1, operand2) =>
              unit.program.state.array(operand1.index) = unboxAddr(operand1) - unboxAddr(operand2)
              unit.program.state.currentLine += 1
            case Command.Rand(address, maxValue) =>
              unit.program.state.array(address.index) = rand.nextInt(maxValue)
              unit.program.state.currentLine += 1
            case c: Command.GoTo => goToCommand(c)
            case Command.Label(name) =>
              unit.program.state.currentLine += 1
            case Command.If(condition, goTo) =>
              val leftOperand = unboxAddr(condition.leftOperand)
              val rightOperand = unboxAddr(condition.rightOperand)
              val calc = condition.operator match {
                case Operator.Less => leftOperand < rightOperand
                case Operator.Greater => leftOperand > rightOperand
                case Operator.Equal => leftOperand == rightOperand
              }
              if (calc) goToCommand(goTo)
              else unit.program.state.currentLine += 1
            case c@ Command.Reproduce(givingEnergy) =>
              event = c
              unit.program.state.currentLine += 1
              exit = true
            case c: Command.Wait =>
              event = c
              unit.program.state.currentLine += 1
              exit = true
          }
        }
      }
      if (unit.energy <= 0) exit = true
    }

    def goToCommand(command: GoTo): Unit = command match {
      case Command.GoTo(to: String) =>
        val index = unit.program.code.commands.indexWhere {
          case Some(Command.Label(label)) => label == to
          case _ => false
        }
        if (index != -1) {
          unit.program.state.currentLine = index
        } else {
          throw EvalError(s"Not found label $to")
        }
      case Command.GoTo(to: Int) =>
        if (unit.program.code.commands.array.length > to) {
          unit.program.state.currentLine = to
        } else {
          throw EvalError(s"Not found line $to")
        }
    }

    def unboxAddr(a: ArrayAddress | Int): Int = a match {
      case ArrayAddress(index) => unit.program.state.array(index)
      case number: Int => number
    }

    event match {
      case null => throw EvalError("Not have events")
      case event: Event => event
    }
  }

  case class EvalError(val msg: String) extends Throwable(msg)

  private def commandEnergyCost(command: Command): Int = command match {
    case Command.Move(_) => 10
    case Command.Set(_, _) => 1
    case Command.Scan(_, _) => 1
    case Command.Add(_, _) => 1
    case Command.Sub(_, _) => 1
    case Command.Rand(_, _) => 1
    case Command.GoTo(_) => 1
    case Command.Label(_) => 0
    case Command.If(_, _) => 2
    case Command.Reproduce(_) => 50
    case Command.Wait() => 0
  }

  enum Command {
    case Move(direction: Direction)
    //    case Get(address: ArrayAddress, readFrom: ArrayAddress | Int)
    case Set(address: ArrayAddress, value: ArrayAddress | Int)
    case Scan(address: ArrayAddress, objectType: ObjectType)
    case Add(operand1: ArrayAddress, operand2: ArrayAddress | Int)
    case Sub(operand1: ArrayAddress, operand2: ArrayAddress | Int)
    case Rand(address: ArrayAddress, maxValue: Int)
    case GoTo(to: String | Int)
    case Label(name: String)
    case If(condition: Condition, goTo: GoTo)
    case Reproduce(givingEnergy: Int)
    case Wait()
  }

  case class Condition(leftOperand: ArrayAddress | Int, rightOperand: ArrayAddress | Int, operator: Operator)

  enum Operator {
    case Less
    case Greater
    case Equal
  }

  enum Direction {
    case Up
    case Down
    case Left
    case Right
  }

  enum ObjectType {
    case Unit
    case Food
    case All
  }

  case class ArrayAddress(index: Int)
}

/*
* Движение:

    MOVE [Направление/Координаты]: Перемещение в указанном направлении (например, Север, Юг, Восток, Запад, Северо-Запад и т.д.) или к конкретным координатам. Тратит энергию.

Чтение данных:

    GET [Адрес памяти] [Источник данных]: Чтение данных из источника (например, текущая энергия, радиус обзора, тип клетки под юнитом, содержимое соседней клетки). Источником может быть массив юнитов в радиусе.

    SCAN [Адрес памяти] [Тип объекта]: Сканирование окружающего пространства в заданном радиусе на предмет точек пропитания, других юнитов. Результат (например, ближайшее направление или расстояние) записывается в регистр.

Манипуляции с данными:

    SET [Адрес памяти] [Значение/Адрес памяти]: Запись значения в регистр или ячейку памяти.

    ADD/SUB/MUL/DIV [Адрес памяти] [Значение/Адрес памяти]: Арифметические операции.

    RAND [Адрес памяти] [МаксЗначение]: Генерация случайного числа.

Управление потоком:

    GOTO [Адрес/Метка]: Безусловный переход к указанной строке кода.

    IF [Условие] GOTO [Адрес/Метка]: Условный переход (например, IF [Энергия > 100] GOTO [Размножение]).

Размножение:

    REPRODUCE [Количество энергии для потомка]: Создание копии юнита.*/