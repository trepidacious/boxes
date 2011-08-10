package boxes.graph

import boxes._
import java.text.DecimalFormat
import javax.swing.{ImageIcon}
import java.awt.{Image, Color}
import list.ListVal
import java.awt.geom.Rectangle2D

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
}

object Vec2 {
  val zero = Vec2(0, 0)
}

case class Borders(top:Double = 0, left:Double = 0, bottom:Double = 0, right:Double = 0)

case class Series[K](key:K, curve:List[Vec2], color:Color = Color.black, width:Double = 1)

class GraphSeries[K](series:RefGeneral[List[Series[K]], _], shadow:Boolean = false) extends GraphLayer {

  def gather() = series()

  def paint() = {
    val currentSeries = series()
    (canvas:GraphCanvas) => {
      canvas.clipToData
      if (shadow) canvas.color = new Color(220, 220, 220)
      val shadowOffset = Vec2(1, 1)
      for {
        s <- currentSeries
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

  val dataBounds = Cal{
    series().foldLeft(None:Option[Area]){(seriesArea, series) => series.curve.foldLeft(seriesArea){
      (area, v) => area match {
        case None => Some(Area(v, Vec2.zero))
        case Some(a) => Some(a.extendToContain(v))
      }
    }}
  }

  def onMouse(event:GraphMouseEvent) {}

}

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

  def pad(v:Vec2) = normalise.rawPad(v)
  def rawPad(v:Vec2) = {
    val padding = size * v
    val o = origin - padding
    val s = size + padding * (Vec2(2,2))
    Area(o, s)
  }

}

trait Graph {
  def layers:RefGeneral[List[GraphLayer], _]
  def overlayers:RefGeneral[List[GraphLayer], _]
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
  //When called, reads Box state and returns a method that will draw this state to a canvas
  def paint():(GraphCanvas => Unit)
  def onMouse(event:GraphMouseEvent)
  def dataBounds:RefGeneral[Option[Area], _]
}

trait GraphDisplayLayer extends GraphLayer {
  def onMouse(event:GraphMouseEvent) {}
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
      canvas.color = SwingView.dividingColor.brighter
      canvas.drawRect(canvas.spaces.pixelArea.origin, canvas.spaces.pixelArea.size)
    }
  }
}

class GraphHighlight extends UnboundedGraphDisplayLayer {
  def paint() = {
    (canvas:GraphCanvas) => {
      canvas.color = SwingView.alternateBackgroundColor.brighter
      canvas.drawRect(canvas.spaces.pixelArea.origin + Vec2(-1, 1), canvas.spaces.pixelArea.size + Vec2(2, -2))
    }
  }
}

object GraphShadow {
  val topLeft = new ImageIcon(classOf[GraphShadow].getResource("/boxes/swing/GraphShadowTopLeft.png")).getImage
  val top = new ImageIcon(classOf[GraphShadow].getResource("/boxes/swing/GraphShadowTop.png")).getImage
  val left = new ImageIcon(classOf[GraphShadow].getResource("/boxes/swing/GraphShadowLeft.png")).getImage
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

class GraphAxis(val axis:Axis, val pixelsPerMajor:Int = 100, val format:DecimalFormat = new DecimalFormat("0.###")) extends UnboundedGraphDisplayLayer {

  def paint() = {
    (canvas:GraphCanvas) => {
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
}

class GraphAxisTitle(val axis:Axis, name:RefGeneral[String, _]) extends UnboundedGraphDisplayLayer {
  def paint() = {
    val currentName = name()

    (canvas:GraphCanvas) => {
      val a = canvas.spaces.pixelArea
      val tl = a.origin + a.size.withX(0)
      val br = a.origin + a.size.withY(0)

      canvas.color = SwingView.dividingColor.darker
      canvas.fontSize = 12
      axis match {
        case X => canvas.string(currentName, br + Vec2(-10, 28), Vec2(1, 1))
        case Y => canvas.string(currentName, tl + Vec2(-52, 10 ), Vec2(1, 0), -1)
      }
    }
  }
}

object GraphSelectBox {

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

  def apply[K](series:RefGeneral[List[Series[K]], _], fill:RefGeneral[Color, _], outline:RefGeneral[Color, _], selectionOut:VarGeneral[Set[K], _], enabled:RefGeneral[Boolean, _] = Val(true)) = {
    new GraphBox(fill, outline, enabled, (area:Area, spaces:GraphSpaces) => {
      val areaN = area.normalise
      val selected = series().collect{
        case series if (curveIntersectsArea(series.curve, areaN)) => series.key
      }
      selectionOut() = selected.toSet
    })
  }
}

object GraphZoomBox {
  def apply(fill:RefGeneral[Color, _], outline:RefGeneral[Color, _], areaOut:VarGeneral[Option[Area], _], enabled:RefGeneral[Boolean, _] = Val(true)) = {
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


class GraphBox(fill:RefGeneral[Color, _], outline:RefGeneral[Color, _], enabled:RefGeneral[Boolean, _] = Val(true), action:(Area, GraphSpaces) => Unit, val minSize:Int = 5) extends GraphLayer {
  private val area:Var[Option[Area]] = Var(None)

  def bigEnough(a:Area) = (math.abs(a.size.x) > minSize || math.abs(a.size.y) > minSize)

  def gather = (fill(), outline(), enabled(), area())


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

  def onMouse(e:GraphMouseEvent) {
    if (enabled()) {
      e.eventType match {
        case PRESS => area() = Some(Area(e.dataPoint, Vec2(0, 0)))
        case DRAG => area().foreach(a => {
          area() = Some(Area(a.origin, e.dataPoint - a.origin))
        })
        case RELEASE => area().foreach(a => {
          area() = None
          val dragArea = Area(a.origin, e.dataPoint - a.origin)
          val pixelDragArea = e.spaces.toPixel(dragArea)
          if (bigEnough(pixelDragArea)) {
            action.apply(dragArea, e.spaces)
          }
        })
        case _ => {}
      }
    }
  }

  val dataBounds = Val(None:Option[Area])

}

case class GraphZoomerAxis(
    requiredRange:Ref[Option[(Int, Int)]] = Var(None),
    padding:Ref[Double] = Var(0.05)
)

class GraphZoomer(
    val dataBounds:RefGeneral[Option[Area], _],
    val manualBounds:RefGeneral[Option[Area], _] = Val(None),
    val xAxis:Ref[GraphZoomerAxis] = Val(GraphZoomerAxis()),
    val yAxis:Ref[GraphZoomerAxis] = Val(GraphZoomerAxis())) {

  def autoArea = {
    dataBounds() match {
      case None => {
        //We have no data bounds, so use the axes required ranges,
        //or 0 to 1 in each axis if there are none.
        val xRange = xAxis().requiredRange().getOrElse((0, 1))
        val yRange = yAxis().requiredRange().getOrElse((0, 1))
        Area(Vec2(xRange._1, yRange._1), Vec2(xRange._2, yRange._2)).normalise
      }
      case Some(area) => {
        //We have a data bounds area, so pad it appropriately
        val auto = area.pad(Vec2(xAxis().padding(), yAxis().padding()))

        val padX = xAxis().requiredRange().foldLeft(auto){(area, range) => area.extendToContain(Vec2(range._1, auto.origin.y)).extendToContain(Vec2(range._2, auto.origin.y))}
        val padY = yAxis().requiredRange().foldLeft(padX){(area, range) => area.extendToContain(Vec2(auto.origin.x, range._1)).extendToContain(Vec2(auto.origin.x, range._2))}

        padY
      }
    }
  }

  val dataArea = Cal{
    //Use manual bounds if specified, automatic area from data bounds etc.
    manualBounds().getOrElse(autoArea)
  }
}

case class GraphBasic(layers:RefGeneral[List[GraphLayer], _], overlayers:RefGeneral[List[GraphLayer], _], dataArea:RefGeneral[Area, _], borders:RefGeneral[Borders, _]) extends Graph {}

object GraphBasic {
  def withSeries[K](
      series:RefGeneral[List[Series[K]], _],
      xName:RefGeneral[String, _] = Val("x"),
      yName:RefGeneral[String, _] = Val("y"),
      borders:RefGeneral[Borders, _] = Val(Borders(16, 74, 53, 16)),
      zoomEnabled:RefGeneral[Boolean, _] = Val(true),
      manualBounds:VarGeneral[Option[Area], _] = Var(None),
      xAxis:Ref[GraphZoomerAxis] = Val(GraphZoomerAxis()),
      yAxis:Ref[GraphZoomerAxis] = Val(GraphZoomerAxis()),
      selectEnabled:RefGeneral[Boolean, _] = Val(false),
      selection:VarGeneral[Set[K], _] = Var(Set[K]())
      ) = {

    val layers = ListVal[GraphLayer](
        new GraphBG(SwingView.alternateBackgroundColor, Color.white),
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
      GraphZoomBox(Val(new Color(0, 0, 200, 50)), Val(new Color(100, 100, 200)), manualBounds, zoomEnabled),
      GraphSelectBox(series, Val(new Color(0, 200, 0, 50)), Val(new Color(100, 200, 100)), selection, selectEnabled)
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
  def apply[K](series:RefGeneral[List[Series[K]], _], indices:RefGeneral[Set[K],_], unselectedColor:Color = new Color(230, 230, 230), unselectedWidth:Option[Double] = Some(1d)) = Cal{
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


