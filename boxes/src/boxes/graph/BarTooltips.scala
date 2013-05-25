package boxes.graph

import boxes.graph.GraphMouseEventType._
import boxes.{Var, Val, Box}
import boxes.swing.SwingView
import java.awt.geom.{Line2D}
import java.text.DecimalFormat

trait BarTooltipRenderer[C1, C2, K] {
  def paint(canvas:GraphCanvas, cat1: C1, cat2: C2, bar: Bar[K], pixelPos:Vec2)
}

class StringBarTooltipRenderer[C1, C2, K](
    print:((C1, C2, Bar[K])=>String) = BarTooltips.defaultPrint)
    extends BarTooltipRenderer[C1, C2, K]{
  
  //TODO refactor this method, maybe into GraphCanvas - very similar in SeriesToolTip stuff.
  def paint(canvas:GraphCanvas, cat1: C1, cat2: C2, bar: Bar[K], pixelPos:Vec2) {
    val s = print(cat1, cat2, bar)
    canvas.drawTooltip(s, pixelPos)
  }
}

object BarTooltips {

  val format = new DecimalFormat("0.###")

  def printRange(bar: Bar[_]) = {
    val l = List(bar.rangeMin, bar.rangeMax).flatten.map(format.format _)
    if (l.isEmpty) {
      ""
    } else {
      "(" + l.mkString(" to ") + ")"
    }
  }
  
  def printValueAndRange(bar: Bar[_]) = format.format(bar.value) + " " + printRange(bar)
    
  def defaultPrint[C1, C2, K] = (cat1: C1, cat2: C2, bar: Bar[K]) => cat1.toString() + ", " + cat2.toString() + " = " + printValueAndRange(bar)
  
  def apply[C1, C2, K](enabled:Box[Boolean, _], 
    data: Box[Map[(C1, C2), Bar[K]], _],
    barWidth: Box[Double, _], catPadding: Box[Double, _], 
    barPadding: Box[Double, _],
    renderer:BarTooltipRenderer[C1, C2, K])
    (implicit ord1: Ordering[C1], ord2: Ordering[C2]) = new BarTooltips(enabled, data, barWidth, catPadding, barPadding, renderer)(ord1, ord2)

  def string[C1, C2, K](enabled:Box[Boolean, _], 
    data: Box[Map[(C1, C2), Bar[K]], _],
    barWidth: Box[Double, _], catPadding: Box[Double, _], 
    barPadding: Box[Double, _], print:((C1, C2, Bar[K])=>String) = BarTooltips.defaultPrint)
    (implicit ord1: Ordering[C1], ord2: Ordering[C2]) = new BarTooltips(enabled, data, barWidth, catPadding, barPadding, new StringBarTooltipRenderer(print))(ord1, ord2)
}

class BarTooltips[C1, C2, K](enabled:Box[Boolean, _], 
    data: Box[Map[(C1, C2), Bar[K]], _],
    barWidth: Box[Double, _], catPadding: Box[Double, _], 
    barPadding: Box[Double, _],
    renderer:BarTooltipRenderer[C1, C2, K])
    (implicit ord1: Ordering[C1], ord2: Ordering[C2]) extends GraphLayer {

  private val toPaint:Var[Option[(C1, C2, Bar[K], Vec2)]] = Var(None)

  def paint() = {
    val e = enabled()
    val tp = toPaint()
    (canvas:GraphCanvas) => {
      if (e) {
        tp.foreach(pair => renderer.paint(canvas, pair._1, pair._2, pair._3, pair._4))
      }
    }
  }

  def onMouse(e:GraphMouseEvent) = {

    if (enabled()) {
      //If the mouse position is ner enough to a series to "select" it, show tooltip with that series, at current mouse pixel point
      toPaint() = e.eventType match {
        case MOVE => {
          val s = BarSelection.selectedBar(data(), barWidth(), catPadding(), barPadding(), e)
          s.map(cat => (cat._1, cat._2, cat._3, e.spaces.toPixel(e.dataPoint)))
        }
        case _ => None
      }
    }

    false
  }

  val dataBounds = Val(None:Option[Area])

}