package boxes.graph

import boxes._
import boxes.list._
import boxes.general._
import java.text.DecimalFormat
import javax.swing.{ImageIcon}
import list.ListVal
import java.awt.geom.Rectangle2D
import com.explodingpixels.widgets.ImageUtils
import java.awt.{Color, Image}
import boxes.swing.icons.IconFactory
import boxes.swing.SwingView

object Axis extends Enumeration {
  type Axis = Value
  val X, Y = Value

  def other(axis:Axis) = if (axis == X) Y else X
}
import Axis._


//Immutable
case class Vec2(x:Double = 0, y:Double = 0) {
  def +(b:Vec2) = Vec2(x + b.x, y + b.y)
  def -(b:Vec2) = Vec2(x - b.x, y - b.y)
  def *(b:Vec2) = Vec2(x * b.x, y * b.y)
  def /(b:Vec2) = Vec2(x / b.x, y / b.y)
  def *(d:Double) = Vec2(d * x, d * y)
  def /(d:Double) = Vec2(x / d, y / d)
  def dot(b:Vec2) = x * b.x + y * b.y
  def transpose = Vec2(y, x)
  def withX(newX:Double) = Vec2(newX, y)
  def withY(newY:Double) = Vec2(x, newY)

  //The Vec2 that has the minimum value in each component that is in either
  //this Vec2, or b.
  //Geometrically, the bottom left corner of a rectangle
  //containing this Vec2 and b
  def componentMinima(b:Vec2) = Vec2(math.min(x, b.x), math.min(y, b.y))

  //The Vec2 that has the maximum value in each component that is in either
  //this Vec2, or b.
  //Geometrically, The top right corner of a rectangle
  //containing this Vec2 and b
  def componentMaxima(b:Vec2) = Vec2(math.max(x, b.x), math.max(y, b.y))
  def onAxis(axis:Axis) = axis match {
    case X => x
    case Y => y
  }
  def squaredLength = x * x + y * y
  def length = math.sqrt(squaredLength)
  
  //The union of the intervals defined by this Vec2 and b. Each interval is from
  //a minimum at the Vec2 x value to at maximum at the Vec2 y value.
  def intervalUnion(b: Vec2) = Vec2(math.min(x, b.x), math.max(y, b.y))
  
  def round() = Vec2(math.round(x), math.round(y))
}

object Vec2 {
  val zero = Vec2(0, 0)
  def x(v:Double = 1) = Vec2(v, 0)
  def y(v:Double = 1) = Vec2(0, v)
}

case class Borders(top:Double = 0, left:Double = 0, bottom:Double = 0, right:Double = 0)

trait SeriesPainter{
  def paint(canvas:GraphCanvas, series:Series[_], shadow:Boolean = false)
  def linesDrawn(series:Series[_]):Boolean
}

case class Series[K](key:K, curve:List[Vec2], color:Color = Color.black, width:Double = 1, painter:SeriesPainter = SeriesStyles.line) {
  def pixelCurve(spaces:GraphSpaces) = curve.map(p => spaces.toPixel(p))
}

class LineSeriesPainter extends SeriesPainter {
  def paint(canvas:GraphCanvas, s:Series[_], shadow:Boolean = false) {
    if (shadow) {
      canvas.color = GraphSeries.shadowColor
      canvas.lineWidth = s.width + 1
      canvas.path(s.curve.map(p => canvas.spaces.toPixel(p) + GraphSeries.shadowOffset))
    } else {
      canvas.color = s.color
      canvas.lineWidth = s.width
      canvas.dataPath(s.curve)
    }
  }
  def linesDrawn(series:Series[_]) = true
}

trait PointPainter {
  def paint(canvas:GraphCanvas, p:Vec2)
}

class CrossPointPainter extends PointPainter {
  def paint(canvas:GraphCanvas, p:Vec2) {
    canvas.line(p - Vec2(2, 2), p + Vec2(2, 2))
    canvas.line(p - Vec2(-2, 2), p + Vec2(-2, 2))
  }
}

class PlusPointPainter extends PointPainter {
  def paint(canvas:GraphCanvas, p:Vec2) {
    canvas.line(p - Vec2(0, 2), p + Vec2(0, 2))
    canvas.line(p - Vec2(2, 0), p + Vec2(2, 0))
  }
}

class SquarePointPainter extends PointPainter {
  def paint(canvas:GraphCanvas, p:Vec2) {
    canvas.drawRect(p - Vec2(2,2), Vec2(4, 4))
  }
}

object SeriesStyles {
  val cross = new PointSeriesPainter(new CrossPointPainter())
  val plus = new PointSeriesPainter(new PlusPointPainter())
  val square = new PointSeriesPainter(new SquarePointPainter())
  val line = new LineSeriesPainter()
}

class PointSeriesPainter(val pointPainter:PointPainter = new CrossPointPainter()) extends SeriesPainter {

  def paint(canvas:GraphCanvas, s:Series[_], shadow:Boolean = false) {
    if (shadow) {
      canvas.color = GraphSeries.shadowColor
      canvas.lineWidth = s.width + 1
      for (p <- s.curve.map(p => canvas.spaces.toPixel(p) + GraphSeries.shadowOffset)) {
        pointPainter.paint(canvas, p)
      }
    } else {
      canvas.color = s.color
      canvas.lineWidth = s.width
      for (p <- s.curve.map(p => canvas.spaces.toPixel(p))) {
        pointPainter.paint(canvas, p)
      }
    }
  }
  def linesDrawn(series:Series[_]) = false
}

object GraphSeries {
  val shadowColor = new Color(220, 220, 220)
  val shadowOffset = Vec2(1, 1)
  val barShadowColor = new Color(215, 215, 215)
  val barOutlineColor = SwingView.dividingColor
  val barShadowOffset = Vec2(3, 3)
}

class GraphSeries[K](series:Box[List[Series[K]], _], shadow:Boolean = false) extends GraphLayer {

  def paint() = {
    val currentSeries = series()
    (canvas:GraphCanvas) => {
      canvas.clipToData
      for (s <- currentSeries) {
        s.painter.paint(canvas, s, shadow)
      }
    }
  }

  val dataBounds = Cal{
    series().foldLeft(None:Option[Area]){(seriesArea, series) => series.curve.foldLeft(seriesArea){
      (area, v) => area match {
        case None => Some(Area(v, Vec2.zero))
        case Some(a) => Some(a.extendToContain(v))
      }
    }}
  }

  def onMouse(event:GraphMouseEvent) = false

}

case class Area(origin:Vec2 = Vec2(), size:Vec2 = Vec2(1, 1)) {
  def toUnit(v:Vec2) = (v - origin)/size
  def fromUnit(v:Vec2) = (v * size) + origin
  def axisBounds(a:Axis) = a match {
    case X => (origin.x, origin.x + size.x)
    case Y => (origin.y, origin.y + size.y)
  }
  def axisContains(a:Axis, p: Double) = a match {
    case X => (p >= origin.x && p<= origin.x + size.x)
    case Y => (p >= origin.y && p<= origin.y + size.y)
  }
  def axisSize(a:Axis) = a match {
    case X => size.x
    case Y => size.y
  }
  def axisRelativePosition(a:Axis, v:Vec2) = a match {
    case X => (v - origin).x
    case Y => (v - origin).y
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
  def round() = {
    val o = origin.round()
    val c = (origin + size).round()
    Area(o, c-o)
  }
  def contains(v:Vec2) = normalise.rawContains(v)
  private def rawContains(v:Vec2) = (v.x >= origin.x && v.y >= origin.y && v.x <= origin.x + size.x && v.y <= origin.y + size.y)

  def contains(a:Area) = normalise.rawContains(a)
  private def rawContains(a:Area) = {
    val area = a.normalise
    (area.origin.x >= origin.x && area.origin.y >= origin.y && area.origin.x + area.size.x <= origin.x + size.x && area.origin.y + area.size.y <= origin.y + size.y)
  }

  def normalise = {
    var w = size.x
    var h = size.y
    if (w >= 0 && h >= 0 ) {
      this
    } else {
      var x = origin.x
      var y = origin.y
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
  def translate(v: Vec2) = Area(origin + v, size)
  def extendToContain(v:Vec2) = {
    if (contains(v)) {
      this
    } else {
      normalise.rawExtendToContain(v)
    }
  }
  private def rawExtendToContain(v:Vec2) = {
    //TODO can probably be done more concisely using corners
    var x = origin.x
    var y = origin.y
    var w = size.x
    var h = size.y
    if (v.x < x) {
      w += x-v.x
      x = v.x
    } else if (v.x > x + w) {
      w = v.x - x
    }
    if (v.y < y) {
      h += y-v.y
      y = v.y
    } else if (v.y > y + h) {
      h = v.y - y
    }
    Area(Vec2(x, y), Vec2(w, h))
  }

  def extendToContain(a:Area) = {
    if (contains(a)) {
      this
    } else {
      normalise.rawExtendToContain(a.normalise)
    }
  }
  private def rawExtendToContain(a:Area) = {

    //TODO can probably be done more concisely using corners
    var x = origin.x
    var y = origin.y
    var w = size.x
    var h = size.y
    if (a.origin.x < x) {
      w += x - a.origin.x
      x = a.origin.x
    } else if (a.origin.x + a.size.x > x + w) {
      w = a.origin.x + a.size.x - x
    }
    if (a.origin.y < y) {
      h += y - a.origin.y
      y = a.origin.y
    } else if (a.origin.y + a.size.y > y + h) {
      h = a.origin.y + a.size.y - y
    }
    Area(Vec2(x, y), Vec2(w, h))
  }

  def intersection(a:Area) = normalise.rawIntersection(a.normalise)

  def rawIntersection(a:Area) = {
    //Find corners of the intersection
    val bottomLeft = origin.componentMaxima(a.origin)
    val topRight = (origin + size).componentMinima(a.origin + a.size)

    //Produce area from corners
    Area(bottomLeft, topRight - bottomLeft)
  }

  def pad(v:Vec2): Area = pad(v, v)
  def pad(before: Vec2, after: Vec2): Area = normalise.rawPad(before, after)
  def rawPad(before: Vec2, after: Vec2) = {
    val beforePadding = size * before
    val o = origin - beforePadding
    val s = size + beforePadding + size * after
    Area(o, s)
  }

  def inset(r: Double, u: Double, l: Double, d: Double): Area = normalise.rawInset(r, u, l, d)
  def rawInset(r: Double, u: Double, l: Double, d: Double) = {
    val o = origin + Vec2(l, d)
    val s = size + Vec2(-l-r, -u-d)
    Area(o, s)
  }

  def sizeAtLeast(minSize:Vec2) = normalise.rawSizeAtLeast(minSize)
  def rawSizeAtLeast(minSize:Vec2) = {
    if (size.x >= minSize.x && size.y >= minSize.y) {
      this
    } else {
      val newSize = Vec2(math.max(size.x, minSize.x), math.max(size.y, minSize.y))
      val newOrigin = origin - (newSize - size) / 2
      Area(newOrigin, newSize)
    }
  }
}

trait Graph {
  def layers:Box[List[GraphLayer], _]
  def overlayers:Box[List[GraphLayer], _]
  def dataArea:Box[Area, _]
  def borders:Box[Borders, _]
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
   val PRESS, RELEASE, DRAG, MOVE, CLICK, ENTER, EXIT, CONSUMED = Value
}
import GraphMouseEventType._

object GraphMouseButton extends Enumeration {
   type GraphMouseButton = Value
   val LEFT, MIDDLE, RIGHT, NONE = Value
}
import GraphMouseButton._

case class GraphMouseEvent (spaces:GraphSpaces, dataPoint:Vec2, eventType:GraphMouseEventType, button:GraphMouseButton)

trait GraphLayer {
  //When called, reads Box state and returns a method that will draw this state to a canvas
  def paint():(GraphCanvas => Unit)
  //Handle an event, returns false to allow it to reach other layers, or true to consume it
  def onMouse(event:GraphMouseEvent):Boolean
  def dataBounds:Box[Option[Area], _]
}

trait GraphDisplayLayer extends GraphLayer {
  def onMouse(event:GraphMouseEvent) = false
}

trait UnboundedGraphDisplayLayer extends GraphDisplayLayer {
  val dataBounds = Val(None:Option[Area])
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

class GraphBG(val bg:Color, val dataBG:Color) extends UnboundedGraphDisplayLayer {
  def paint() = {
    (canvas:GraphCanvas) => {
      canvas.color = bg
      canvas.fillRect(canvas.spaces.componentArea.origin, canvas.spaces.componentArea.size)

      canvas.color = dataBG
      canvas.fillRect(canvas.spaces.pixelArea.origin, canvas.spaces.pixelArea.size)
    }
  }
}

class GraphOutline extends UnboundedGraphDisplayLayer {
  def paint() = {
    (canvas:GraphCanvas) => {
      canvas.color = GraphAxis.axisColor
      canvas.drawRect(canvas.spaces.pixelArea.origin, canvas.spaces.pixelArea.size)
    }
  }
}

class GraphHighlight extends UnboundedGraphDisplayLayer {
  def paint() = {
    (canvas:GraphCanvas) => {
      canvas.color = SwingView.alternateBackgroundColor.brighter
      canvas.drawRect(canvas.spaces.pixelArea.origin + Vec2(-1, 1), canvas.spaces.pixelArea.size + Vec2(1, -1))
    }
  }
}

object GraphBusy {
  val pencil = IconFactory.image("GraphPencil")
}

class GraphBusy(val alpha:Var[Double]) extends UnboundedGraphDisplayLayer {
  def paint() = {
    val a = alpha()
    (canvas:GraphCanvas) => {
      canvas.color = SwingView.transparentColor(SwingView.selectionColor, a)
      val pa = canvas.spaces.pixelArea
      if (a > 0.5) {
        canvas.image(GraphBusy.pencil, pa.origin + pa.size + Vec2(-43, 10))
      }
    }
  }
}

object GraphShadow {
  val topLeft = IconFactory.image("GraphShadowTopLeft")
  val top = IconFactory.image("GraphShadowTop")
  val left = IconFactory.image("GraphShadowLeft")
}

class GraphShadow extends UnboundedGraphDisplayLayer {
  def paint() = {
    (canvas:GraphCanvas) => {
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
}

object GraphAxis {
  val fontSize = 10
  val titleFontSize = 12
  val fontColor = SwingView.textColor
  val axisColor = SwingView.dividingColor
  val axisHighlightColor = SwingView.alternateBackgroundColor.brighter
  val gridMajorColor = new Color(0f, 0f, 0f, 0.08f)
  val gridMinorColor = new Color(0f, 0f, 0f, 0.03f)
  val defaultFormat = new DecimalFormat("0.###")

  def apply(axis:Axis, pixelsPerMajor:Int = 100, format:DecimalFormat = GraphAxis.defaultFormat) = new GraphAxis(axis, pixelsPerMajor, format)
}

class GraphAxis(val axis:Axis, val pixelsPerMajor:Int = 100, val format:DecimalFormat = GraphAxis.defaultFormat, val gridlines: Boolean = true) extends UnboundedGraphDisplayLayer {

  def paint() = {
    (canvas:GraphCanvas) => {
      val dataArea = canvas.spaces.dataArea

      val ticks = Ticks(dataArea.axisBounds(axis), canvas.spaces.pixelArea.axisSize(axis), pixelsPerMajor)

      ticks.foreach(t => {
        val (p, major) = t
        val start = canvas.spaces.toPixel(dataArea.axisPosition(axis, p))

        canvas.color = GraphAxis.axisColor
        axis match {
          case X => canvas.line(start, start + Vec2(0, if (major) 8 else 4))
          case Y => canvas.line(start, start + Vec2(if (major) -8 else -4, 0))
        }
        canvas.color = GraphAxis.axisHighlightColor
        axis match {
          case X => canvas.line(start + Vec2(1, 0), start + Vec2(1, if (major) 8 else 4))
          case Y => canvas.line(start + Vec2(0, 1), start + Vec2(if (major) -8 else -4, 1))
        }


        if (major) {
          canvas.color = GraphAxis.fontColor
          canvas.fontSize = GraphAxis.fontSize
          axis match {
            case X => canvas.string(format.format(p), start + Vec2(0, 10), Vec2(0.5, 1))
            case Y => canvas.string(format.format(p), start + Vec2(-10, 0), Vec2(1, 0.5))
          }
          if (gridlines) {
            canvas.color = GraphAxis.gridMajorColor
            canvas.line(start, start + canvas.spaces.pixelArea.axisPerpVec2(axis))
          }
        }
//        } else {
//          canvas.color = GraphAxis.gridMinorColor
//          canvas.line(start, start + canvas.spaces.pixelArea.axisPerpVec2(axis))
//        }
      })
    }
  }
}

class GraphAxisTitle(val axis:Axis, name:Box[String, _]) extends UnboundedGraphDisplayLayer {
  def paint() = {
    val currentName = name()

    (canvas:GraphCanvas) => {
      val a = canvas.spaces.pixelArea
      val tl = a.origin + a.size.withX(0)
      val br = a.origin + a.size.withY(0)

      canvas.color = GraphAxis.fontColor
      canvas.fontSize = GraphAxis.titleFontSize
      axis match {
        case X => canvas.string(currentName, br + Vec2(-10, 28), Vec2(1, 1))
        case Y => canvas.string(currentName, tl + Vec2(-52, 10 ), Vec2(1, 0), -1)
      }
    }
  }
}

object GraphSelectBox {

  def curvePointInArea(curve:List[Vec2], area:Area) = {
    curve.foldLeft(false) {
      (contains, p) => contains || area.contains(p)
    }
  }

  def curveIntersectsArea(curve:List[Vec2], area:Area) = {
    val rect = new Rectangle2D.Double(area.origin.x, area.origin.y, area.size.x, area.size.y)

    //TODO we should finish this early if possible - there is some way to do this
    val result = curve.foldLeft((false, None:Option[Vec2])){
      (result, current) => {
        val intersects = result._1
        val previous = result._2
        if (intersects) {
          (intersects, Some(current))
        } else {
          previous match {
            case None => (false, Some(current))
            case Some(p) => {
              if (rect.intersectsLine(p.x, p.y, current.x, current.y)) {
                (true, Some(current))
              } else {
                (false, Some(current))
              }
            }
          }
        }
      }
    }

    result._1
  }

  def seriesSelected(series:Series[_], area:Area) = {
    if (series.painter.linesDrawn(series)) {
      curveIntersectsArea(series.curve, area)
    } else {
      curvePointInArea(series.curve, area)
    }
  }


  def apply[K](series:Box[List[Series[K]], _], fill:Box[Color, _], outline:Box[Color, _], selectionOut:VarBox[Set[K], _], enabled:Box[Boolean, _] = Val(true)) = {
    new GraphBox(fill, outline, enabled, (area:Area, spaces:GraphSpaces) => {
      val areaN = area.normalise
      val selected = series().collect{
        case s if (seriesSelected(s, areaN)) => s.key
      }
      selectionOut() = selected.toSet
    })
  }
}

object GraphZoomBox {
  def apply(fill:Box[Color, _], outline:Box[Color, _], areaOut:VarBox[Option[Area], _], enabled:Box[Boolean, _] = Val(true)) = {
    new GraphBox(fill, outline, enabled, (zoomArea:Area, spaces:GraphSpaces) => {
      //Zoom out for second quadrant drag (x negative, y positive)
      if (zoomArea.size.x < 0 && zoomArea.size.y > 0) {
        areaOut() = None
      } else {
        areaOut() = Some(zoomArea.normalise)
      }
    })
  }
}

object GraphClick{
  def apply[K](enabled:Box[Boolean, _] = Val(true), selectionOut:VarBox[Set[K], _]) = new GraphClick(enabled, selectionOut)
}

class GraphClick[K](enabled:Box[Boolean, _] = Val(true), selectionOut:VarBox[Set[K], _]) extends GraphLayer {

  def paint() = (canvas:GraphCanvas) => {}

  def onMouse(current:GraphMouseEvent) = {
    if (enabled()) {
      Box.transact{
        current.eventType match {
          case RELEASE => {
            //TODO Find closest series, and select it
            println("Got a release!")
            true
          }
          case _ => false
        }
      }
    } else {
      false
    }
  }

  val dataBounds = Val(None:Option[Area])

}


object GraphGrab{
  def apply(enabled:Box[Boolean, _] = Val(true), manualDataArea:VarBox[Option[Area], _], displayedDataArea:Box[Area, _]) = new GraphGrab(enabled, manualDataArea, displayedDataArea)
}

class GraphGrab(enabled:Box[Boolean, _] = Val(true), manualDataArea:VarBox[Option[Area], _], displayedDataArea:Box[Area, _]) extends GraphLayer {

  private var maybeInitial:Option[GraphMouseEvent] = None

  def paint() = (canvas:GraphCanvas) => {}

  def onMouse(current:GraphMouseEvent) = {
    if (enabled()) {
      Box.transact{
        current.eventType match {
          case PRESS => {
            maybeInitial = Some(current)
            true
          }
          case DRAG => {
            maybeInitial.foreach(initial => {
              //If there is no manual zoom area, set it to the displayed area
              if (manualDataArea() == None) {
                manualDataArea() = Some(displayedDataArea())
              }
              manualDataArea().foreach(a => {
                val initialArea = initial.spaces.dataArea
                val currentPixelOnInitialArea = initial.spaces.toData(current.spaces.toPixel(current.dataPoint))

                val dataDrag = initial.dataPoint - currentPixelOnInitialArea
                manualDataArea() = Some(Area(initialArea.origin + dataDrag, initialArea.size))
              })
            })
            true
          }
          case RELEASE => {
            maybeInitial = None
            true
          }
          case _ => false
        }
      }
    } else {
      false
    }
  }

  val dataBounds = Val(None:Option[Area])

}

//Draw a horizontal component made up of a left, middle and right portion. Portions are
//taken from the thirds of an image, and middle is stretched horizontally to fit.
class GraphThreePartPainter(image:Image) {
  val pieceWidth = image.getWidth(null)/3
  val pieceHeight = image.getHeight(null)
  val parts = (
    ImageUtils.getSubImage(image, 0, 0, pieceWidth, pieceHeight),
    ImageUtils.getSubImage(image, pieceWidth, 0, pieceWidth, pieceHeight),
    ImageUtils.getSubImage(image, pieceWidth * 2, 0, pieceWidth, pieceHeight)
  )

  def paint(canvas:GraphCanvas, p:Vec2, s:Vec2) {
    val middle = s.x - pieceWidth * 2
    canvas.image(parts._1, p)
    canvas.image(parts._3, p + Vec2(s.x - pieceWidth))
    if (middle > 0) {
      canvas.image(parts._2, p + Vec2(pieceWidth, 0), Vec2(middle, pieceHeight))
    }
  }
}

class GraphThreePartPainterVertical(image:Image) {
  val pieceHeight = image.getHeight(null)/3
  val pieceWidth = image.getWidth(null)
  val parts = (
    ImageUtils.getSubImage(image, 0, 0,               pieceWidth, pieceHeight),
    ImageUtils.getSubImage(image, 0, pieceHeight,     pieceWidth, pieceHeight),
    ImageUtils.getSubImage(image, 0, pieceHeight * 2, pieceWidth, pieceHeight)
  )

  def paint(canvas:GraphCanvas, p:Vec2, s:Vec2) {
    val middle = s.y - pieceHeight * 2
    canvas.image(parts._1, p)
    canvas.image(parts._3, p + Vec2(0, s.y - pieceHeight))
    if (middle > 0) {
      canvas.image(parts._2, p + Vec2(0, pieceHeight), Vec2(pieceWidth, middle))
    }
  }
}


object AxisTooltip {
  val format = new DecimalFormat("0.0000")
  def apply(axis:Axis, enabled:Box[Boolean, _] = Val(true)) = new AxisTooltip(axis, enabled)
  val horizTabPainter = new GraphThreePartPainter(IconFactory.image("HorizontalLineLabel"))
  val vertTabPainter = new GraphThreePartPainterVertical(IconFactory.image("VerticalLineLabel"))
  val lineColor = SwingView.shadedBoxColor

  def drawAxisLine(canvas:GraphCanvas, v:Double, a:Axis, label:String, color:Option[Color]) = {
    canvas.clipToData()
    val dataArea = canvas.spaces.dataArea
    val start = canvas.spaces.toPixel(dataArea.axisPosition(a, v))
    val end = start + canvas.spaces.pixelArea.axisPerpVec2(a)

    canvas.lineWidth = 1
    canvas.color = color.getOrElse(AxisTooltip.lineColor)
    canvas.line(start, end)

    canvas.color = GraphAxis.fontColor
    canvas.fontSize = GraphAxis.fontSize

    val size = canvas.stringSize(label)

    val colorOffset = if (color == None) 0 else 12;

    //TODO combine code
    a match {
      case X => {
        AxisTooltip.vertTabPainter.paint(canvas, start + Vec2(-16, -4 - 23 - size.x - colorOffset), Vec2(16, size.x + 23 + colorOffset))
        canvas.color = SwingView.selectedTextColor
        canvas.string(label, start + Vec2(-3, -15 - colorOffset), Vec2(0, 0), -1)
        color.foreach(c => {
          val swatch = Area(start + Vec2(-11, -21), Vec2(7, 7))
          canvas.color = c
          canvas.fillRect(swatch)
          canvas.color = SwingView.selectedTextColor
          canvas.drawRect(swatch)
        })
      }
      case Y => {
        AxisTooltip.horizTabPainter.paint(canvas, start + Vec2(4, -16), Vec2(size.x + 23 + colorOffset, 16))
        canvas.color = SwingView.selectedTextColor
        canvas.string(label, start + Vec2(15 + colorOffset, -3), Vec2(0, 0), 0)
        color.foreach(c => {
          val swatch = Area(start + Vec2(14, -11), Vec2(7, 7))
          canvas.color = c
          canvas.fillRect(swatch)
          canvas.color = SwingView.selectedTextColor
          canvas.drawRect(swatch)
        })
      }
    }

    size.x + 23 + 4 + colorOffset

  }

}

class AxisTooltip(axis:Axis, enabled:Box[Boolean, _] = Val(true)) extends GraphLayer {

  private val value:Var[Option[Double]] = Var(None)

  def paint() = {

    val a = axis
    val maybeV = value()
    val e = enabled()

    (canvas:GraphCanvas) => {
      if (e) {
        maybeV.foreach(v => {
          val label = AxisTooltip.format.format(v)
          AxisTooltip.drawAxisLine(canvas, v, a, label, None)
        })
      }
    }
  }

  def onMouse(e:GraphMouseEvent) = {
    if (enabled()) {
      e.eventType match {
        case MOVE => {
          val axisPosition = e.spaces.pixelArea.axisRelativePosition(Axis.other(axis), e.spaces.toPixel(e.dataPoint)) * (if (axis == X) -1 else 1)
          if (axisPosition <= 0 && axisPosition > -32) {
            value() = Some(e.dataPoint.onAxis(axis))
          } else {
            value() = None
          }
        }
        case _ => value() = None
      }
      false
    } else {
      false
    }
  }

  val dataBounds = Val(None:Option[Area])

}


class GraphBox(fill:Box[Color, _], outline:Box[Color, _], enabled:Box[Boolean, _] = Val(true), action:(Area, GraphSpaces) => Unit, val minSize:Int = 5) extends GraphLayer {
  private val area:Var[Option[Area]] = Var(None)

  def bigEnough(a:Area) = (math.abs(a.size.x) > minSize || math.abs(a.size.y) > minSize)

  def paint() = {
    val cFill = fill()
    val cOutline = outline()
    val cEnabled = enabled()
    val cArea = area()

    (canvas:GraphCanvas) => {
      if (cEnabled) {
        cArea.foreach(a => {
          val pixelArea = canvas.spaces.toPixel(a)
          if (bigEnough(pixelArea)) {
            canvas.color = cFill
            canvas.fillRect(canvas.spaces.toPixel(a))
            canvas.color = cOutline
            canvas.drawRect(canvas.spaces.toPixel(a))
          }
        })
      }
    }
  }

  def onMouse(e:GraphMouseEvent) = {
    if (enabled()) {
      e.eventType match {
        case PRESS => {
          area() = Some(Area(e.dataPoint, Vec2(0, 0)))
          true
        }
        case DRAG => {
          area().foreach(a => {
            area() = Some(Area(a.origin, e.dataPoint - a.origin))
          })
          true
        }
        
//        case RELEASE => area() match {
//          case None => false
//          case Some(a) => {
//            area() = None
//            val dragArea = Area(a.origin, e.dataPoint - a.origin)
//            val pixelDragArea = e.spaces.toPixel(dragArea)
//            if (bigEnough(pixelDragArea)) {
//              action.apply(dragArea, e.spaces)
//              true
//            } else {
//              false
//            }
//          }
//        }
        
        case RELEASE => {
          area().foreach(a => {
            area() = None
            val dragArea = Area(a.origin, e.dataPoint - a.origin)
            val pixelDragArea = e.spaces.toPixel(dragArea)
            if (bigEnough(pixelDragArea)) {
              action.apply(dragArea, e.spaces)
            }
          })
          true
        }
        case _ => false
      }
    } else {
      false
    }
  }

  val dataBounds = Val(None:Option[Area])

}

case class GraphZoomerAxis(
    requiredRange:Ref[Option[(Double, Double)]] = Var(None),
    paddingBefore:Ref[Double] = Var(0.05),
    paddingAfter:Ref[Double] = Var(0.05),
    minSize:Ref[Double] = Var(0.01)
)

class GraphZoomer(
    val dataBounds:Box[Option[Area], _],
    val manualBounds:Box[Option[Area], _] = Val(None),
    val xAxis:Ref[GraphZoomerAxis] = Val(GraphZoomerAxis()),
    val yAxis:Ref[GraphZoomerAxis] = Val(GraphZoomerAxis())) {

  def autoArea = {
    dataBounds() match {
      case None => {
        //We have no data bounds, so use the axes required ranges,
        //or 0 to 1 in each axis if there are none.
        val xRange = xAxis().requiredRange().getOrElse((0d, 1d))
        val yRange = yAxis().requiredRange().getOrElse((0d, 1d))
        Area(Vec2(xRange._1, yRange._1), Vec2(xRange._2, yRange._2)).normalise
      }
      case Some(area) => {
        //We have a data bounds area, so pad it appropriately
        val auto = area.pad(Vec2(xAxis().paddingBefore(), yAxis().paddingBefore()), Vec2(xAxis().paddingAfter(), yAxis().paddingAfter()))

        val padX = xAxis().requiredRange().foldLeft(auto){(area, range) => area.extendToContain(Vec2(range._1, auto.origin.y)).extendToContain(Vec2(range._2, auto.origin.y))}
        val padY = yAxis().requiredRange().foldLeft(padX){(area, range) => area.extendToContain(Vec2(auto.origin.x, range._1)).extendToContain(Vec2(auto.origin.x, range._2))}

        padY
      }
    }
  }

  val dataArea = Cal{
    //Use manual bounds if specified, automatic area from data bounds etc.
    //Make sure that size is at least the minimum for each axis
    val a = manualBounds().getOrElse(autoArea)
    a.sizeAtLeast(Vec2(xAxis().minSize(), yAxis().minSize()))
  }
}

case class GraphBasic(layers:ListRef[GraphLayer], overlayers:ListRef[GraphLayer], dataArea:Ref[Area], borders:Ref[Borders]) extends Graph {}

object GraphBasic {
  
  def withSeries[K](
      series:ListRef[Series[K]],
      xName:Ref[String] = Val("x"),
      yName:Ref[String] = Val("y"),
      borders:Ref[Borders] = Val(Borders(16, 74, 53, 16)),
      zoomEnabled:Ref[Boolean] = Val(true),
      manualBounds:Var[Option[Area]] = Var(None),
      xAxis:Ref[GraphZoomerAxis] = Val(GraphZoomerAxis()),
      yAxis:Ref[GraphZoomerAxis] = Val(GraphZoomerAxis()),
      selectEnabled:Ref[Boolean] = Val(false),
      selection:Var[Set[K]] = Var(Set[K]()),
      grabEnabled:Ref[Boolean] = Val(false),
      seriesTooltipsEnabled:Ref[Boolean] = Val(true),
      seriesTooltipsPrint:(K=>String) = (k:K) => k.toString(),
      axisTooltipsEnabled:Ref[Boolean] = Val(true),
      extraMainLayers:List[GraphLayer] = List[GraphLayer](),
      extraOverLayers:List[GraphLayer] = List[GraphLayer]()
      ) = {

    val layers = ListVal[GraphLayer](
      extraMainLayers ::: List(
        new GraphBG(SwingView.background, Color.white),
        new GraphHighlight(),
        new GraphSeries(series, true),
        new GraphAxis(Y, 50),
        new GraphAxis(X),
        new GraphShadow(),
        new GraphSeries[K](series),
        new GraphOutline(),
        new GraphAxisTitle(X, xName),
        new GraphAxisTitle(Y, yName)
      )
    )

    val dataBounds = Cal{
      layers().foldLeft(None:Option[Area]){
        (areaOption, layer) => areaOption match {
          case None => layer.dataBounds()

          case Some(area) => layer.dataBounds() match {
            case None => Some(area)
            case Some(layerArea) => Some(area.extendToContain(layerArea))
          }
        }
      }
    }

    val zoomer = new GraphZoomer(dataBounds, manualBounds, xAxis, yAxis)

    val overlayers = ListVal[GraphLayer](
      List(SeriesTooltips.highlight(series, seriesTooltipsEnabled)) ::: extraOverLayers ::: List(
        GraphZoomBox(Val(new Color(0, 0, 200, 50)), Val(new Color(100, 100, 200)), manualBounds, zoomEnabled),
        GraphSelectBox(series, Val(new Color(0, 200, 0, 50)), Val(new Color(100, 200, 100)), selection, selectEnabled),
        GraphGrab(grabEnabled, manualBounds, zoomer.dataArea),
        AxisTooltip(X, axisTooltipsEnabled),
        AxisTooltip(Y, axisTooltipsEnabled),
        SeriesTooltips.string(series, seriesTooltipsEnabled, seriesTooltipsPrint)
                      //TODO is this in the right place?
        //GraphClick(Val(true), selection)
      )
    )

    new GraphBasic(
      layers,
      overlayers,
      zoomer.dataArea,
      borders
    )
  }
  
  
  def withBars[C1, C2](
      data: Ref[Map[(C1, C2), Bar]],
      barWidth: Ref[Double] = Val(1.0), catPadding: Ref[Double] = Val(1.0), barPadding: Ref[Double] = Val(0.4),
      yName:Ref[String] = Val("y"),
      borders:Ref[Borders] = Val(Borders(16, 74, 53, 16)),
      zoomEnabled:Ref[Boolean] = Val(true),
      manualBounds:Var[Option[Area]] = Var(None),
      xAxis:Ref[GraphZoomerAxis] = Val(GraphZoomerAxis()),
      yAxis:Ref[GraphZoomerAxis] = Val(GraphZoomerAxis()),
      //selectEnabled:Ref[Boolean] = Val(false),
      //selection:Var[Set[K]] = Var(Set[K]()),
      grabEnabled:Ref[Boolean] = Val(false),
      //seriesTooltipsEnabled:Ref[Boolean] = Val(true),
      //seriesTooltipsPrint:(K=>String) = (k:K) => k.toString(),
      axisTooltipsEnabled:Ref[Boolean] = Val(true),
      extraMainLayers:List[GraphLayer] = List[GraphLayer](),
      extraOverLayers:List[GraphLayer] = List[GraphLayer]()
      )(implicit ord1: Ordering[C1], ord2: Ordering[C2]) = {

    val layers = ListVal[GraphLayer](
      extraMainLayers ::: List(
        new GraphBG(SwingView.background, Color.white),
        new GraphHighlight(),
        new GraphBars(data, barWidth, catPadding, barPadding, true)(ord1, ord2),  //Shadows
        new GraphAxis(Y, 50),
        new GraphBarAxis(data, barWidth, catPadding, barPadding, X)(ord1, ord2),
        new GraphShadow(),
        new GraphBars(data, barWidth, catPadding, barPadding, false)(ord1, ord2), //Data
        new GraphOutline(),
        new GraphAxisTitle(Y, yName)
      )
    )

    val dataBounds = Cal{
      layers().foldLeft(None:Option[Area]){
        (areaOption, layer) => areaOption match {
          case None => layer.dataBounds()

          case Some(area) => layer.dataBounds() match {
            case None => Some(area)
            case Some(layerArea) => Some(area.extendToContain(layerArea))
          }
        }
      }
    }

    val zoomer = new GraphZoomer(dataBounds, manualBounds, xAxis, yAxis)

    val overlayers = ListVal[GraphLayer](
//      List(SeriesTooltips.highlight(series, seriesTooltipsEnabled)) ::: 
        extraOverLayers ::: List(
        GraphZoomBox(Val(new Color(0, 0, 200, 50)), Val(new Color(100, 100, 200)), manualBounds, zoomEnabled),
//        GraphSelectBox(series, Val(new Color(0, 200, 0, 50)), Val(new Color(100, 200, 100)), selection, selectEnabled),
        GraphGrab(grabEnabled, manualBounds, zoomer.dataArea),
        AxisTooltip(Y, axisTooltipsEnabled)
//        SeriesTooltips.string(series, seriesTooltipsEnabled, seriesTooltipsPrint)
                      //TODO is this in the right place?
        //GraphClick(Val(true), selection)
      )
    )

    new GraphBasic(
      layers,
      overlayers,
      zoomer.dataArea,
      borders
    )
  }
  
}

object ColorSeriesBySelection {
  def apply[K](series:Box[List[Series[K]], _], indices:Box[Set[K],_], unselectedColor:Color = new Color(230, 230, 230), unselectedWidth:Option[Double] = Some(1d)) = ListCal{
    val unselected = series().collect{
      case s:Series[K] if !indices().contains(s.key) => s.copy(color = unselectedColor, width = unselectedWidth.getOrElse(s.width))
    }
    val selected = series().filter(s => indices().contains(s.key))

    unselected ::: selected
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
  def stringSize(s:String):Vec2
  def rect(origin:Vec2, size:Vec2, fill:Boolean)
  def drawRect(origin:Vec2, size:Vec2)
  def fillRect(origin:Vec2, size:Vec2)
  def fillRect(area:Area)
  def drawRect(area:Area)
  def rect(area:Area, fill:Boolean)

  def fillRoundRect(origin: Vec2, size: Vec2, radius: Double)
  def fillRoundRect(area: Area, radius: Double)
  def drawRoundRect(origin: Vec2, size: Vec2, radius: Double)
  def drawRoundRect(area: Area, radius: Double)
  def roundRect(area: Area, fill: Boolean, radius: Double)
  def roundRect(origin: Vec2, size: Vec2, fill: Boolean, radius: Double)

  def clipToData()
  def clipToAll()
  def image(i:Image, origin:Vec2, size:Vec2)
  def image(i:Image, origin:Vec2)
  def path(path:List[Vec2])
  def dataPath(path:List[Vec2])

}


