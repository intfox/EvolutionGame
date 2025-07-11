import org.scalajs.dom
import org.scalajs.dom.html

object WorldCanvas {
  val canvas = dom.document.getElementById("worldCanvas").asInstanceOf[html.Canvas]
  val myGenealogyId = "test"
  var observedUnitId = 0
  def render(world: World): Unit = {
    val ctx = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]
    ctx.clearRect(0, 0, canvas.width, canvas.height)

    for(unit <- world.units) {
      drawCircle(unit.x, unit.y, if(unit.genealogyId == myGenealogyId) "green" else "red", highlighted = unit.id == observedUnitId)
    }
    for(food <- world.foods) {
      drawCircle(food.x, food.y, "white")
    }
  }

  def init(onClick: (x: Int, y: Int) => Unit): Unit = {
    canvas.onclick = (e: dom.MouseEvent) => {
      val rect = canvas.getBoundingClientRect()
      val x = e.clientX - rect.left // X-координата клика относительно canvas
      val y = e.clientY - rect.top // Y-координата клика относительно canvas
      val resX = (x / 10).toInt
      val resY = (y / 10).toInt
      onClick(resX, resY)
    }
  }

  def drawGrid(): Unit = {
    val ctx = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]
    val gridSize = 10; // Ширина и высота каждой ячейки

    // Определяем ширину и высоту холста
    val canvasWidth = canvas.width;
    val canvasHeight = canvas.height;

    // Устанавливаем цвет и толщину линий сетки
    ctx.strokeStyle = "#d3d3d3"; // Цвет линий (светло-серый)
    ctx.lineWidth = 0.5; // Толщина линий

    // Рисуем вертикальные линии
    for (x <- 0 to canvasWidth if x % gridSize == 0) {
      ctx.beginPath(); // Начинаем новый путь рисования
      ctx.moveTo(x, 0); // Перемещаем перо в начальную точку (верх холста)
      ctx.lineTo(x, canvasHeight); // Рисуем линию до нижней части холста
      ctx.stroke(); // Выполняем рисование линии
    }

    // Рисуем горизонтальные линии
    for (y <- 0 to canvasHeight if y % gridSize == 0) {
      ctx.beginPath(); // Начинаем новый путь рисования
      ctx.moveTo(0, y); // Перемещаем перо в начальную точку (левый край холста)
      ctx.lineTo(canvasWidth, y); // Рисуем линию до правого края холста
      ctx.stroke(); // Выполняем рисование линии
    }
  }

  def drawCircle(x: Int, y: Int, color: String, highlighted: Boolean = false): Unit = {
    val ctx = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]

    ctx.beginPath()
    ctx.arc(x * 10 + 5, y * 10 + 5, 5, 0, 2 * Math.PI)
    ctx.fillStyle = color
    ctx.fill()
    ctx.lineWidth = if(!highlighted) 1 else 2
    ctx.stroke()
  }
}
