package boxes.graph

import boxes.graph.GraphMouseEventType._
import boxes.{Var, Val, Box}
import boxes.swing.SwingView
import java.awt.geom.{Line2D}

trait SeriesTooltipRenderer[K] {
  def paint(canvas:GraphCanvas, series:Series[K], pixelPos:Vec2)
}

class StringSeriesTooltipRenderer[K](print:(K=>String) = (k:K) => k.toString()) extends SeriesTooltipRenderer[K]{
  def paint(canvas:GraphCanvas, series:Series[K], pixelPos:Vec2) {
    val s = print(series.key)
    canvas.drawTooltip(s, pixelPos)
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

  def string[K](series:Box[List[Series[K]], _], enabled:Box[Boolean, _] = Val(true), print:(K=>String) = (k:K) => k.toString()) = new SeriesTooltips[K](enabled, series, new StringSeriesTooltipRenderer[K](print))
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

  def onMouse(e:GraphMouseEvent) = {

    if (enabled()) {
      //If the mouse position is ner enough to a series to "select" it, show tooltip with that series, at current mouse pixel point
      toPaint() = e.eventType match {
        case MOVE => SeriesSelection.selectedSeries(series(), e).map((_, e.spaces.toPixel(e.dataPoint)))  //Stick the pixel point in with selected series
        case _ => None
      }
    }

    false
  }

  val dataBounds = Val(None:Option[Area])

}