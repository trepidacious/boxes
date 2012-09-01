package boxes.graph

import boxes.graph.GraphMouseEventType._
import boxes.{Var, Val, Box}
import boxes.swing.SwingView
import java.awt.geom.{Line2D}

trait SeriesTooltipRenderer[K] {
  def paint(canvas:GraphCanvas, series:Series[K], pixelPos:Vec2)
}

class StringSeriesTooltipRenderer[K] extends SeriesTooltipRenderer[K]{
  def paint(canvas:GraphCanvas, series:Series[K], pixelPos:Vec2) {
    val s = series.key.toString
    val size = canvas.stringSize(s)

    canvas.color = SwingView.shadedBoxColor
    canvas.fillRoundRect(pixelPos - Vec2(-8, 4 + size.y + 8), size + Vec2(8, 8), 6)

    canvas.color = SwingView.selectedTextColor
    canvas.string(s, pixelPos + Vec2(12, -12))

  }
}

class HighlightSeriesTooltipRenderer[K] extends SeriesTooltipRenderer[K]{
  def paint(canvas:GraphCanvas, series:Series[K], pixelPos:Vec2) {
    canvas.clipToData()
    series.painter.paint(canvas, series.copy(width = series.width + 3))
    canvas.clipToAll()
  }
}

object SeriesTooltips {
  val maxRadius = 10

  def apply[K](series:Box[List[Series[K]], _], enabled:Box[Boolean, _] = Val(true), renderer:SeriesTooltipRenderer[K] = new StringSeriesTooltipRenderer[K]()) = new SeriesTooltips[K](enabled, series, renderer)

  def string[K](series:Box[List[Series[K]], _], enabled:Box[Boolean, _] = Val(true)) = new SeriesTooltips[K](enabled, series, new StringSeriesTooltipRenderer[K]())
  def highlight[K](series:Box[List[Series[K]], _], enabled:Box[Boolean, _] = Val(true)) = new SeriesTooltips[K](enabled, series, new HighlightSeriesTooltipRenderer[K]())
}

class SeriesTooltips[K](enabled:Box[Boolean, _], series:Box[List[Series[K]], _], renderer:SeriesTooltipRenderer[K]) extends GraphLayer {

  private val toPaint:Var[Option[(Series[K], Vec2)]] = Var(None)

  def paint() = {
    val e = enabled()
    val tp = toPaint()
    (canvas:GraphCanvas) => {
      if (e) {
        tp.foreach(pair => renderer.paint(canvas, pair._1, pair._2))
      }
    }
  }

  def curveNearestPoint(curve:List[Vec2], p:Vec2) = {
    val squared = curve.foldLeft(Double.PositiveInfinity) {
      (previousSquaredLength, curvePoint) => math.min(previousSquaredLength, (p-curvePoint).squaredLength)
    }

    math.sqrt(squared)
  }

  def curveNearestApproach(curve:List[Vec2], p:Vec2) = {
    val squared = curve.foldLeft((Double.PositiveInfinity, None:Option[Vec2])){
      (result, current) => {
        val previousDistance = result._1
        val maybePrevious = result._2
        maybePrevious match {
          case None => (previousDistance, Some(current))
          case Some(previous) => {
            val distance = Line2D.ptSegDistSq(previous.x, previous.y, current.x, current.y, p.x, p.y)
            (math.min(previousDistance, distance), Some(current))
          }
        }
      }
    }
    math.sqrt(squared._1)
  }

  def seriesDistance(spaces:GraphSpaces, series:Series[_], p:Vec2) = {
    //Use the curve in PIXEL coords, to make sense to user
    if (series.painter.linesDrawn(series)) {
      curveNearestApproach(series.pixelCurve(spaces), p)
    } else {
      curveNearestPoint(series.pixelCurve(spaces), p)
    }
  }


  def onMouse(e:GraphMouseEvent) = {

    if (enabled()) {
      val pixelPoint = e.spaces.toPixel(e.dataPoint)
      val dataPoint = e.dataPoint

      e.eventType match {
        case MOVE => {
          //Search for nearest series line/point, and if it is within maxRadius, and mouse is in data area,
          //set its key and our pixel point to toPaint
          if (e.spaces.dataArea.contains(dataPoint)) {
            val currentSeries = series()
            val distances = currentSeries.map(s => seriesDistance(e.spaces, s, pixelPoint)).zipWithIndex
            val index = distances.minBy(pair => pair._1)._2
            val d = distances(index)._1

            if (d < SeriesTooltips.maxRadius) {
              toPaint() = Some(currentSeries(index), pixelPoint)
            } else {
              toPaint() = None
            }
          } else {
            toPaint() = None
          }
        }
        case _ => {
          toPaint() = None
        }
      }
    }

    false
  }

  val dataBounds = Val(None:Option[Area])

}