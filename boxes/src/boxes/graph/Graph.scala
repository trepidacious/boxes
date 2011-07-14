package boxes.graph

import boxes._
import java.text.DecimalFormat
import java.awt.image.BufferedImage
import list.ListVal
import javax.swing.{ImageIcon, JPanel}
import java.awt.{Shape, BasicStroke, RenderingHints, Graphics2D, Image, Color, Graphics}
import java.awt.geom.{AffineTransform, Path2D, PathIterator}
import java.awt.event.{MouseMotionListener, MouseEvent, MouseListener, ComponentEvent, ComponentListener}

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
  def withX(newX:Double) = Vec2(newX, y)
  def withY(newY:Double) = Vec2(x, newY)
}

object Vec2 {
  val zero = Vec2(0, 0)
}

case class Borders(top:Double = 0, left:Double = 0, bottom:Double = 0, right:Double = 0)
case class Series(curve:List[Vec2], color:Color = Color.black, width:Double = 1)

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
    case Y => Vec2(origin.x, p)
  }
  def axisVec2(a:Axis) = a match {
    case X => Vec2(size.x, 0)
    case Y => Vec2(0, size.y)
  }
  def axisPerpVec2(a:Axis) = a match {
    case X => Vec2(0, size.y)
    case Y => Vec2(size.x, 0)
  }
  def normalise = {
    var x = origin.x
    var y = origin.y
    var w = size.x
    var h = size.y
    if (h < 0) {
      y = y + h
      h = -h
    }
    if (w < 0) {
      x = x + w
      w = -w
    }
    Area(Vec2(x, y), Vec2(w, h))
  }
}

trait Graph {
  def layers:RefGeneral[List[GraphLayer], _]
  def dataArea:RefGeneral[Area, _]
  def borders:RefGeneral[Borders, _]
}

case class GraphSpaces(val dataArea:Area, val pixelArea:Area, val componentArea:Area) {
  def toPixel(dataPos:Vec2):Vec2 = pixelArea.fromUnit(dataArea.toUnit(dataPos))
  def toData(pixelPos:Vec2):Vec2 = dataArea.fromUnit(pixelArea.toUnit(pixelPos))
  def toPixel(area:Area):Area = {
    val o = toPixel(area.origin)
    val s = area.size / dataArea.size * pixelArea.size
    Area(o, s)
  }
  def toData(area:Area):Area = {
    val o = toData(area.origin)
    val s = area.size / pixelArea.size * dataArea.size
    Area(o, s)
  }
}

object GraphMouseEventType extends Enumeration {
   type GraphMouseEventType = Value
   val PRESS, RELEASE, DRAG, MOVE, CLICK = Value
}
import GraphMouseEventType._

object GraphMouseButton extends Enumeration {
   type GraphMouseButton = Value
   val LEFT, MIDDLE, RIGHT, NONE = Value
}
import GraphMouseButton._

case class GraphMouseEvent (spaces:GraphSpaces, dataPoint:Vec2, eventType:GraphMouseEventType, button:GraphMouseButton)

trait GraphLayer {
  def paint(canvas:GraphCanvas)
  def onMouse(event:GraphMouseEvent)
}

trait GraphDisplayLayer extends GraphLayer {
  def onMouse(event:GraphMouseEvent) {}
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

class GraphBG(val bg:Color, val dataBG:Color) extends GraphDisplayLayer {
  def paint(canvas:GraphCanvas) {
    canvas.color = bg
    canvas.fillRect(canvas.spaces.componentArea.origin, canvas.spaces.componentArea.size)

    canvas.color = dataBG
    canvas.fillRect(canvas.spaces.pixelArea.origin, canvas.spaces.pixelArea.size)
  }
}

class GraphOutline extends GraphDisplayLayer {
  def paint(canvas:GraphCanvas) {
    canvas.color = SwingView.dividingColor.brighter
    canvas.drawRect(canvas.spaces.pixelArea.origin, canvas.spaces.pixelArea.size)
  }
}
class GraphHighlight extends GraphDisplayLayer {
  def paint(canvas:GraphCanvas) {
    canvas.color = SwingView.alternateBackgroundColor.brighter
    canvas.drawRect(canvas.spaces.pixelArea.origin + Vec2(-1, 1), canvas.spaces.pixelArea.size + Vec2(2, -2))
  }
}

object GraphShadow {
  val topLeft = new ImageIcon(classOf[GraphShadow].getResource("/boxes/swing/GraphShadowTopLeft.png")).getImage
  val top = new ImageIcon(classOf[GraphShadow].getResource("/boxes/swing/GraphShadowTop.png")).getImage
  val left = new ImageIcon(classOf[GraphShadow].getResource("/boxes/swing/GraphShadowLeft.png")).getImage
}

class GraphShadow extends GraphDisplayLayer {
  def paint(canvas:GraphCanvas) {
    canvas.clipToData
    val w = GraphShadow.topLeft.getWidth(null)
    val h = GraphShadow.topLeft.getHeight(null)

    val a = canvas.spaces.pixelArea
    val tl = a.origin + a.size.withX(0)
    val bl = a.origin
    val br = a.origin + a.size.withY(0)

    canvas.image(GraphShadow.top, tl, Vec2(a.size.x, h))
    canvas.image(GraphShadow.left, tl, Vec2(w, -a.size.y))
    canvas.image(GraphShadow.top, bl + Vec2(0, 2), Vec2(a.size.x, -h))
    canvas.image(GraphShadow.left, br + Vec2(2, 0), Vec2(-w, a.size.y))
  }
}

class GraphAxis(val axis:Axis, val pixelsPerMajor:Int = 100, val format:DecimalFormat = new DecimalFormat("0.###")) extends GraphDisplayLayer {

  def paint(canvas:GraphCanvas) {
    val dataArea = canvas.spaces.dataArea

    val ticks = Ticks(dataArea.axisBounds(axis), canvas.spaces.pixelArea.axisSize(axis), pixelsPerMajor)

    ticks.foreach(t => {
      val (p, major) = t
      val start = canvas.spaces.toPixel(dataArea.axisPosition(axis, p))

      canvas.color = SwingView.dividingColor.brighter
      axis match {
        case X => canvas.line(start, start + Vec2(0, if (major) 8 else 4))
        case Y => canvas.line(start, start + Vec2(if (major) -8 else -4, 0))
      }
      canvas.color = SwingView.alternateBackgroundColor.brighter
      axis match {
        case X => canvas.line(start + Vec2(1, 0), start + Vec2(1, if (major) 8 else 4))
        case Y => canvas.line(start + Vec2(0, 1), start + Vec2(if (major) -8 else -4, 1))
      }


      if (major) {
        canvas.color = SwingView.dividingColor.darker
        canvas.fontSize = 9
        axis match {
          case X => canvas.string(format.format(p), start + Vec2(0, 10), Vec2(0.5, 1))
          case Y => canvas.string(format.format(p), start + Vec2(-10, 0), Vec2(1, 0.5))
        }
        canvas.color = new Color(0f, 0f, 0f, 0.1f)
      } else {
        canvas.color = new Color(0f, 0f, 0f, 0.05f)
      }
      canvas.line(start, start + canvas.spaces.pixelArea.axisPerpVec2(axis))
    })
  }
}

class GraphAxisTitle(val axis:Axis, name:RefGeneral[String, _]) extends GraphDisplayLayer {

  def paint(canvas:GraphCanvas) {

    val a = canvas.spaces.pixelArea
    val tl = a.origin + a.size.withX(0)
    val bl = a.origin
    val br = a.origin + a.size.withY(0)

    canvas.color = SwingView.dividingColor.darker
    canvas.fontSize = 12
    axis match {
      case X => canvas.string(name(), br + Vec2(-10, 28), Vec2(1, 1))
      case Y => canvas.string(name(), tl + Vec2(-52, 10 ), Vec2(1, 0), -1)
    }
  }
}

class GraphSeries(series:RefGeneral[List[Series], _], shadow:Boolean = false) extends GraphDisplayLayer {
  def paint(canvas:GraphCanvas) {
    canvas.clipToData
    if (shadow) canvas.color = new Color(220, 220, 220)
    val shadowOffset = Vec2(1, 1)
    for {
      s <- series()
    } {
      if (!shadow) {
        canvas.color = s.color
        canvas.lineWidth = s.width
        canvas.dataPath(s.curve)
      } else {
        canvas.lineWidth = s.width + 1
        canvas.path(s.curve.map(p => canvas.spaces.toPixel(p) + shadowOffset))
      }
    }

  }
}

class GraphBox(c:RefGeneral[Color, _], areaOut:VarGeneral[Area, _]) extends GraphLayer {
  private val area:Var[Option[Area]] = Var(None)

  def paint(canvas:GraphCanvas) {
    area().foreach(a => {
      canvas.color = c()
      canvas.clipToData
      canvas.fillRect(canvas.spaces.toPixel(a))
    })
  }

  def onMouse(e:GraphMouseEvent) {
    e.eventType match {
      case PRESS => area() = Some(Area(e.dataPoint, Vec2(0, 0)))
      case DRAG => area().foreach(a => {
        area() = Some(Area(a.origin, e.dataPoint - a.origin))
      })
      case RELEASE => area().foreach(a => {
        area() = None
        areaOut() = Area(a.origin, e.dataPoint - a.origin).normalise
      })
      case _ => {}
    }
  }
}

case class GraphBasic(layers:RefGeneral[List[GraphLayer], _], dataArea:RefGeneral[Area, _], borders:RefGeneral[Borders, _]) extends Graph

object GraphBasic {
  def withSeries(
      series:RefGeneral[List[Series], _],
      dataArea:VarGeneral[Area, _] = Var(Area()),
      xName:RefGeneral[String, _] = Val("x"),
      yName:RefGeneral[String, _] = Val("y"),
      borders:RefGeneral[Borders, _] = Val(Borders(16, 74, 53, 16))) = {
    new GraphBasic(
      ListVal(
        new GraphBG(SwingView.alternateBackgroundColor, Color.white),
        new GraphHighlight(),
        new GraphSeries(series, true),
        new GraphAxis(Y, 50),
        new GraphAxis(X),
        new GraphShadow(),
        new GraphSeries(series),
        new GraphBox(Val(new Color(0, 0, 200, 100)), dataArea),
        new GraphOutline(),
        new GraphAxisTitle(X, xName),
        new GraphAxisTitle(Y, yName)
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
  def lineWidth_=(w:Double)
  def lineWidth:Double
  def fontSize_=(w:Double)
  def fontSize:Double
  def dataLine(a:Vec2, b:Vec2)
  def line(a:Vec2, b:Vec2)
  def string(s:String, v:Vec2, align:Vec2 = Vec2.zero, rotateQuadrants:Int = 0)
  def rect(origin:Vec2, size:Vec2, fill:Boolean)
  def fillRect(origin:Vec2, size:Vec2)
  def drawRect(origin:Vec2, size:Vec2)
  def fillRect(area:Area)
  def drawRect(area:Area)
  def rect(area:Area, fill:Boolean)

  def clipToData()
  def clipToAll()
  def image(i:Image, origin:Vec2, size:Vec2)
  def image(i:Image, origin:Vec2)
  def path(path:List[Vec2])
  def dataPath(path:List[Vec2])
}

class VecListPathIterator(list:List[Vec2]) extends PathIterator {
  var remaining = list
  var first = true

  def getWindingRule = PathIterator.WIND_NON_ZERO
  def isDone = remaining.isEmpty
  def next() {
    remaining = remaining.tail
  }
  def currentSegment(coords:Array[Float]) = {
    coords.update(0, remaining.head.x.asInstanceOf[Float])
    coords.update(1, remaining.head.y.asInstanceOf[Float])
    if (first) {
      first = false
      PathIterator.SEG_MOVETO
    } else {
      PathIterator.SEG_LINETO
    }
  }
  def currentSegment(coords:Array[Double]) = {
    coords.update(0, remaining.head.x)
    coords.update(1, remaining.head.y)
    if (first) {
      first = false
      PathIterator.SEG_MOVETO
    } else {
      PathIterator.SEG_LINETO
    }
  }
}

class GraphCanvasFromGraphics2D(g:Graphics2D, val spaces:GraphSpaces) extends GraphCanvas {

  val defaultClip = g.getClip
  val defaultFont = g.getFont

  var c = Color.black
  var w = 1d
  var fs = 10d

  def color_=(color:Color) {
    g.setColor(color)
    c = color
  }
  def color = c

  def lineWidth_=(lineWidth:Double) {
    g.setStroke(new BasicStroke(lineWidth.asInstanceOf[Float], BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER))
    w = lineWidth
  }
  def lineWidth = w

  def fontSize_=(fontSize:Double) {
    g.setFont(defaultFont.deriveFont(fontSize.asInstanceOf[Float]))
    fs = fontSize
  }
  def fontSize = fs

  def dataLine(a:Vec2, b:Vec2) {
    g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE)
    line(spaces.toPixel(a), spaces.toPixel(b))
    g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_DEFAULT)
  }

  def line(a:Vec2, b:Vec2) {
    g.drawLine(a.x.asInstanceOf[Int], a.y.asInstanceOf[Int], b.x.asInstanceOf[Int], b.y.asInstanceOf[Int])
  }

  def string(s:String, v:Vec2, align:Vec2 = Vec2.zero, rotateQuadrants:Int = 0) {

    val d = g.getFontMetrics.getStringBounds(s, g)
    val w = d.getWidth
    val h = d.getHeight

    val x = v.x.asInstanceOf[Int]
    val y = v.y.asInstanceOf[Int]

    val oldxForm = g.getTransform
    val t = g.getTransform
    t.concatenate(AffineTransform.getQuadrantRotateInstance(rotateQuadrants, x, y))
    g.setTransform(t)

    val vo = v + Vec2(-w * align.x, h * align.y)

    val ox = vo.x.asInstanceOf[Int]
    val oy = vo.y.asInstanceOf[Int]

    g.drawString(s, ox, oy)

    g.setTransform(oldxForm)
  }

  //Convert from a pair of vecs that may draw a "backwards"
  //rect with negative size(s) to the dumb format needed by Java2D,
  //with individual int values that must have a positive width and height
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

  def fillRect(area:Area) {
    rect(area, true)
  }
  def drawRect(area:Area) {
    rect(area, false)
  }
  def rect(area:Area, fill:Boolean) {
    rect(area.origin, area.size, fill)
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

  def image(i:Image, origin:Vec2, size:Vec2) {
    val df = toDumbFormat(origin, size)
    g.drawImage(i, origin.x.asInstanceOf[Int], origin.y.asInstanceOf[Int], size.x.asInstanceOf[Int], size.y.asInstanceOf[Int], null)
  }

  def image(i:Image, origin:Vec2) {
    image(i, origin, Vec2(i.getWidth(null), i.getHeight(null)))
  }

  def path(path:List[Vec2]) {
    val path2D = new Path2D.Double()
    path2D.append(new VecListPathIterator(path), false)
    g.draw(path2D)
  }

  def dataPath(dataPath:List[Vec2]) {
    g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE)
    path(dataPath.map(p => spaces.toPixel(p)))
    g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_DEFAULT)
  }

}

object GraphSwingView {
  val zoom = new ImageIcon(classOf[GraphSwingView].getResource("/boxes/swing/Zoom.png"))
  val zoomIn = new ImageIcon(classOf[GraphSwingView].getResource("/boxes/swing/ZoomIn.png"))
  val zoomOut = new ImageIcon(classOf[GraphSwingView].getResource("/boxes/swing/ZoomOut.png"))
  val zoomSelect = new ImageIcon(classOf[GraphSwingView].getResource("/boxes/swing/ZoomSelect.png"))

  def apply(graph:Ref[_ <: Graph]) = new GraphSwingView(graph)
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

    def fireMouse(e:MouseEvent, eventType:GraphMouseEventType) {
      val s = buildSpaces
      val p = e.getPoint
      val b = e.getButton match {
        case MouseEvent.BUTTON1 => LEFT
        case MouseEvent.BUTTON2 => MIDDLE
        case MouseEvent.BUTTON3 => RIGHT
        case _ => NONE
      }
      val dataPoint = s.toData(Vec2(p.getX, p.getY))
      val gme = GraphMouseEvent(s, dataPoint, eventType, b)
      graph().layers().foreach(layer => layer.onMouse(gme))
    }

    this.addMouseMotionListener(new MouseMotionListener() {
      def mouseDragged(e: MouseEvent) {
        fireMouse(e, DRAG)
      }
      def mouseMoved(e: MouseEvent) {
        fireMouse(e, MOVE)
      }
    })

    this.addMouseListener(new MouseListener(){
      def mouseClicked(e: MouseEvent) {
        fireMouse(e, CLICK)
      }
      def mousePressed(e: MouseEvent) {
        fireMouse(e, PRESS)
      }
      def mouseReleased(e: MouseEvent) {
        fireMouse(e, RELEASE)
      }
      def mouseEntered(e: MouseEvent) {}
      def mouseExited(e: MouseEvent) {}
    })
  }

  val v = View {
    //Note, view will not be called from multiple threads concurrently,
    //so we don't need to synchronize our own use of the off-buffer.
    //We just sync on swapping buffers, so we don't do it while the
    //component is drawing the on-buffer.
    drawBuffer
    replaceUpdate {
      component.repaint()
    }
  }

  def buildSpaces = {

    val size = componentSize()
    val area = graph().dataArea()
    val borders = graph().borders()

    val w = size.x.asInstanceOf[Int]
    val h = size.y.asInstanceOf[Int]

    val l = borders.left.asInstanceOf[Int]
    val r = borders.right.asInstanceOf[Int]
    val t = borders.top.asInstanceOf[Int]
    val b = borders.bottom.asInstanceOf[Int]
    val dw = w - l - r
    val dh = h - t - b

    GraphSpaces(area, Area(Vec2(l, t+dh), Vec2(dw, -dh)), Area(Vec2.zero, size))
  }

  def drawBuffer() {
    val layers = graph().layers()
    val spaces = buildSpaces

    val w = spaces.componentArea.size.x.asInstanceOf[Int]
    val h = spaces.componentArea.size.y.asInstanceOf[Int]

    if (offBuffer.getWidth != w || offBuffer.getHeight != h) {
      offBuffer = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
    }

    val buffer = offBuffer

    val g = buffer.getGraphics.asInstanceOf[Graphics2D]
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    //Each layer paints on a fresh canvas, to avoid side effects from one affecting the next
    layers.foreach(layer => {
      layer.paint(new GraphCanvasFromGraphics2D(g.create().asInstanceOf[Graphics2D], spaces))
    })

    g.dispose

    bufferLock.synchronized{
      val t = onBuffer
      onBuffer = offBuffer
      offBuffer = t
    }
  }

}
