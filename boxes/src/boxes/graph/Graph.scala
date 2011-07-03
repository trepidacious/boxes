package boxes.graph

import javax.swing.JPanel
import boxes._
import java.awt.{RenderingHints, Graphics2D, Color, Graphics}
import java.text.DecimalFormat
import java.awt.image.BufferedImage
import java.awt.event.{ComponentEvent, ComponentListener}
import list.ListVal

object Axis extends Enumeration {
   type Axis = Value
   val X, Y = Value
}
import Axis._


//Immutable
case class Vec2(x:Double = 0, y:Double = 0) {
  def +(b:Vec2) = Vec2(x + b.x, y + b.y)
  def -(b:Vec2) = Vec2(x - b.x, y - b.y)
  def *(b:Vec2) = Vec2(x * b.x, y * b.y)
  def /(b:Vec2) = Vec2(x / b.x, y / b.y)
  def *(d:Double) = Vec2(d * x, d * y)
  def dot(b:Vec2) = x * b.x + y * b.y
  def transpose = Vec2(y, x)
}

object Vec2 {
  val zero = Vec2(0, 0)
}

case class Borders(top:Double = 0, left:Double = 0, bottom:Double = 0, right:Double = 0)
case class Series(curve:List[Vec2], color:Color)

case class Area(origin:Vec2 = Vec2(), size:Vec2 = Vec2(1, 1)) {
  def toUnit(v:Vec2) = (v - origin)/size
  def fromUnit(v:Vec2) = (v * size) + origin
  def axisBounds(a:Axis) = a match {
    case X => (origin.x, origin.x + size.x)
    case Y => (origin.y, origin.y + size.y)
  }
  def axisSize(a:Axis) = a match {
    case X => size.x
    case Y => size.y
  }
  def axisPosition(a:Axis, p:Double) = a match {
    case X => Vec2(p, origin.y)
    case Y => Vec2(origin.y, p)
  }
  def axisVec2(a:Axis) = a match {
    case X => Vec2(size.x, 0)
    case Y => Vec2(0, size.y)
  }
  def axisPerpVec2(a:Axis) = a match {
    case X => Vec2(0, size.y)
    case Y => Vec2(size.x, 0)
  }
}

trait Graph {
  def layers:RefGeneral[List[GraphLayer], _]
  def dataArea:RefGeneral[Area, _]
  def borders:RefGeneral[Borders, _]
}

case class GraphSpaces(val dataArea:Area, val pixelArea:Area, val componentArea:Area) {
  def toPixel(dataPos:Vec2) = pixelArea.fromUnit(dataArea.toUnit(dataPos))
  def toData(pixelPos:Vec2) = dataArea.fromUnit(pixelArea.toUnit(pixelPos))
}

trait GraphLayer {
  def paint(canvas:GraphCanvas)
}

object Ticks {
  def apply(range:(Double,Double), pixels:Double, pixelsPerTick:Double) = {
    val absPixels = math.abs(pixels)

    val dataPerPixel = math.abs(range._2 - range._1) / absPixels
    val dataPerTick = pixelsPerTick * dataPerPixel

    //Find a power of ten value for data per tick
    val dataPerTickR = math.pow(10, math.round(math.log10(dataPerTick)).asInstanceOf[Int])

    //See if we would be better using a multiple of the power of ten
    val dataPerTickBest = List(0.1, 0.5, 5, 10).foldLeft(dataPerTickR)((dataPerTickBest, scale) => {
      val dataPerTickScale = dataPerTickR * scale

      val pixelsPerTickScale = dataPerTickScale / dataPerPixel
      val pixelsPerTickBest = dataPerTickBest / dataPerPixel
      if (math.abs(pixelsPerTickScale - pixelsPerTick) < math.abs(pixelsPerTickBest - pixelsPerTick)) {
        dataPerTickScale
      } else {
        dataPerTickBest
      }
    })

    //Minor ticks are major ticks divided by 5.
    val dataPerMinorTick = dataPerTickBest / 5

    val firstTick = math.floor(range._1 / dataPerMinorTick).asInstanceOf[Int]
    val lastTick = math.ceil(range._2 / dataPerMinorTick).asInstanceOf[Int]

    Range.inclusive(firstTick, lastTick).map(x => (x * dataPerMinorTick, x % 5 == 0)).filter(xMajor => xMajor._1 >= range._1 && xMajor._1 <= range._2)
  }
}

class GraphBGAndShadow(val bg:Color, val dataBG:Color) extends GraphLayer {
  def paint(canvas:GraphCanvas) {
    canvas.color = bg
    canvas.fillRect(canvas.spaces.componentArea.origin, canvas.spaces.componentArea.size)

    canvas.color = dataBG
    canvas.fillRect(canvas.spaces.pixelArea.origin, canvas.spaces.pixelArea.size)

    canvas.color = SwingView.dividingColor.brighter
    canvas.line(canvas.spaces.pixelArea.origin + Vec2(-1, 1), canvas.spaces.pixelArea.origin + Vec2(canvas.spaces.pixelArea.size.x, 1))
    canvas.line(canvas.spaces.pixelArea.origin + Vec2(-1, 0), canvas.spaces.pixelArea.origin + Vec2(-1, canvas.spaces.pixelArea.size.y))
  }
}

class GraphOutline extends GraphLayer {
  def paint(canvas:GraphCanvas) {
    canvas.color = SwingView.dividingColor
    canvas.drawRect(canvas.spaces.pixelArea.origin, canvas.spaces.pixelArea.size)
  }
}

class GraphAxis(val axis:Axis, val pixelsPerMajor:Int = 100, val format:DecimalFormat = new DecimalFormat("0.###")) extends GraphLayer {

  def paint(canvas:GraphCanvas) {
    val dataArea = canvas.spaces.dataArea

    val ticks = Ticks(dataArea.axisBounds(axis), canvas.spaces.pixelArea.axisSize(axis), pixelsPerMajor)

    ticks.foreach(t => {
      val (p, major) = t
      val start = canvas.spaces.toPixel(dataArea.axisPosition(axis, p))

      canvas.color = SwingView.dividingColor
      axis match {
        case X => canvas.line(start, start + Vec2(0, if (major) 8 else 4))
        case Y => canvas.line(start, start + Vec2(if (major) -8 else -4, 0))
      }

      if (major) {
        canvas.color = SwingView.dividingColor.darker
        axis match {
          case X => canvas.string(format.format(p), start + Vec2(0, 10), 0.5, 1)
          case Y => canvas.string(format.format(p), start + Vec2(-10, 0), 1, 0.5)
        }
        canvas.color = new Color(230, 230, 230)
      } else {
        canvas.color = new Color(245, 245, 245)
      }
      canvas.line(start, start + canvas.spaces.pixelArea.axisPerpVec2(axis))
    })
  }
}

class GraphSeries(series:RefGeneral[List[Series], _]) extends GraphLayer {
  def paint(canvas:GraphCanvas) {
    canvas.clipToData
    for {
      s <- series()
    } {
      canvas.color = s.color
      s.curve.foldLeft(None:Option[Vec2]){(last, current) => {
        last.foreach(someLast => canvas.dataLine(someLast, current))
        Some(current)
      }}
    }
    canvas.clipToAll
  }
}

case class GraphBasic(layers:RefGeneral[List[GraphLayer], _], dataArea:RefGeneral[Area, _] = Val(Area()), borders:RefGeneral[Borders, _] = Val(Borders(16, 55, 55, 16))) extends Graph

object GraphBasic {
  def withSeries(series:RefGeneral[List[Series], _], dataArea:RefGeneral[Area, _] = Val(Area()), borders:RefGeneral[Borders, _] = Val(Borders(16, 55, 55, 16))) = {
    new GraphBasic(
      ListVal(
        new GraphBGAndShadow(SwingView.alternateBackgroundColor, Color.white),
        new GraphAxis(X),
        new GraphAxis(Y),
        new GraphSeries(series),
        new GraphOutline()
      ),
      dataArea,
      borders
    )
  }
}

trait GraphCanvas {
  def spaces:GraphSpaces
  def color_=(color:Color)
  def color:Color
  def dataLine(a:Vec2, b:Vec2)
  def line(a:Vec2, b:Vec2)
  def string(s:String, v:Vec2, xAlign:Double = 0, yAlign:Double = 0)
  def rect(origin:Vec2, size:Vec2, fill:Boolean)
  def fillRect(origin:Vec2, size:Vec2)
  def drawRect(origin:Vec2, size:Vec2)
  def clipToData()
  def clipToAll()
}

class GraphCanvasFromGraphics2D(g:Graphics2D, val spaces:GraphSpaces) extends GraphCanvas {

  val defaultClip = g.getClip

  var c = Color.black

  def color_=(color:Color) {
    g.setColor(color)
    c = color
  }
  def color = c

  def dataLine(a:Vec2, b:Vec2) {
    line(spaces.toPixel(a), spaces.toPixel(b))
  }

  def line(a:Vec2, b:Vec2) {
    g.drawLine(a.x.asInstanceOf[Int], a.y.asInstanceOf[Int], b.x.asInstanceOf[Int], b.y.asInstanceOf[Int])
  }

  def string(s:String, v:Vec2, xAlign:Double = 0, yAlign:Double = 0) {
    val d = g.getFontMetrics.getStringBounds(s, g)
    val w = d.getWidth
    val h = d.getHeight
    val vo = v + Vec2(-w * xAlign, h * yAlign - g.getFontMetrics.getDescent)

    g.drawString(s, vo.x.asInstanceOf[Int], vo.y.asInstanceOf[Int])
  }

  def toDumbFormat(origin:Vec2, size:Vec2) = {
    var x = origin.x.asInstanceOf[Int]
    var y = origin.y.asInstanceOf[Int]
    var w = size.x.asInstanceOf[Int]
    var h = size.y.asInstanceOf[Int]
    if (h < 0) {
      y = y + h
      h = -h
    }
    if (w < 0) {
      x = x + w
      w = -w
    }
    (x, y, w, h)
  }

  def rect(origin:Vec2, size:Vec2, fill:Boolean) {
    val df = toDumbFormat(origin, size)
    if (fill) {
      g.fillRect(df._1, df._2, df._3, df._4)
    } else {
      g.drawRect(df._1, df._2, df._3, df._4)
    }
  }

  def fillRect(origin:Vec2, size:Vec2) {
    rect(origin, size, true)
  }

  def drawRect(origin:Vec2, size:Vec2) {
    rect(origin, size, false)
  }

  def clipToRect(origin:Vec2, size:Vec2) {
    val df = toDumbFormat(origin, size)
    g.setClip(df._1, df._2, df._3, df._4)
  }

  def clipToData() {
    clipToRect(spaces.pixelArea.origin, spaces.pixelArea.size)
  }

  def clipToAll() {
    g.setClip(defaultClip)
  }

}

class GraphSwingView(graph:Ref[_ <: Graph]) extends SwingView {

  val componentSize = Var(Vec2(10, 10))

  var offBuffer = new BufferedImage(10, 10, BufferedImage.TYPE_INT_RGB)
  var onBuffer = new BufferedImage(10, 10, BufferedImage.TYPE_INT_RGB)
  var bufferLock = new Object()

  val component = new JPanel() {

    override def paintComponent(gr:Graphics) {
      bufferLock.synchronized{
        gr.drawImage(onBuffer, 0, 0, null)
      }
    }

    def updateSize() {
      componentSize() = Vec2(this.getWidth, this.getHeight)
    }

    this.addComponentListener(new ComponentListener(){
      override def componentResized(e:ComponentEvent) {
        updateSize
      }
      override def componentMoved(e:ComponentEvent) {}
      override def componentShown(e:ComponentEvent) {}
      override def componentHidden(e:ComponentEvent) {}
    })
  }

  val v = View {
    drawBuffer
    replaceUpdate {
      component.repaint()
    }
  }

  def drawBuffer() {

    val size = componentSize()
    val area = graph().dataArea()
    val borders = graph().borders()
    val layers = graph().layers()

    val w = size.x.asInstanceOf[Int]
    val h = size.y.asInstanceOf[Int]

    if (offBuffer.getWidth != w || offBuffer.getHeight != h) {
      offBuffer = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    }

    val buffer = offBuffer

    val g = buffer.getGraphics.asInstanceOf[Graphics2D]
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    val l = borders.left.asInstanceOf[Int]
    val r = borders.right.asInstanceOf[Int]
    val t = borders.top.asInstanceOf[Int]
    val b = borders.bottom.asInstanceOf[Int]
    val dw = w - l - r
    val dh = h - t - b

    val spaces = GraphSpaces(area, Area(Vec2(l, t+dh), Vec2(dw, -dh)), Area(Vec2.zero, size))

    val canvas = new GraphCanvasFromGraphics2D(g, spaces)

    layers.foreach(layer => layer.paint(canvas))

    g.dispose

    bufferLock.synchronized{
      val t = onBuffer
      onBuffer = offBuffer
      offBuffer = t
    }
  }

}
