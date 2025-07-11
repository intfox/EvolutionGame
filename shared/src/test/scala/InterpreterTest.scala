import Interpreter.{Command, Direction}
import Interpreter.Command.{GoTo, Label, Move}
import Interpreter.Direction.Up

import scala.util.Random

class InterpreterTest extends munit.FunSuite {
  test("When move command eval should return move, currentLine should be next line, energy should be 100 - 10") {
    val code = Code(Array(
      None,
      Some(Label("START")),
      Some(Move(Up)),
      Some(GoTo("START"))
    ))
    val unitObject = UnitObject("test", 20, 20, Program(code = code, state = State(Array.fill(30)(0), currentLine = 1)), 100)

    val result = Interpreter.eval(unitObject, Random(10)).toOption.get

    assertEquals(result, Move(Up))
    assertEquals(unitObject.program.state.currentLine, 3)
    assertEquals(unitObject.energy, 100 - 10)
  }

  test("When goto command eval should return move, currentLine should be next line, energy should be 100 - 10 - 1 - 10") {
    val code = Code(Array(
      None,
      Some(Label("START")),
      Some(Move(Up)),
      Some(GoTo("START"))
    ))
    val unitObject = UnitObject("test", 20, 20, Program(code = code, state = State(Array.fill(30)(0), currentLine = 1)), 100)

    Interpreter.eval(unitObject, Random(10))
    val result = Interpreter.eval(unitObject, Random(10)).toOption.get

    assertEquals(result, Move(Up))
    assertEquals(unitObject.program.state.currentLine, 3)
    assertEquals(unitObject.energy, 100 - 10 - 1 - 10)
  }

  test("When empty eval should return event, empty line should be skipped") {
    val code = Code(Array(
      None,
      Some(Label("START")),
      None,
      Some(Move(Up)),
      Some(GoTo("START"))
    ))
    val unitObject = UnitObject("test", 20, 20, Program(code = code, state = State(Array.fill(30)(0), currentLine = 1)), 100)

    val result = Interpreter.eval(unitObject, Random(10)).toOption.get

    assertEquals(result, Move(Up))
    assertEquals(unitObject.program.state.currentLine, 4)
    assertEquals(unitObject.energy, 100 - 10)
  }

  test("Parse correct code should work") {
    val code =
      """START:
        |  MOVE UP
        |  GOTO START
        |
        |""".stripMargin

    println(code)
    val program = Interpreter.parse(code)
    println(program.toList)

    assertEquals(program(0).get, Label("START"))
    assertEquals(program(1).get, Move(Direction.Up))
    assertEquals(program(2).get, GoTo("START"))
    assertEquals(program(3), None)
  }
}
