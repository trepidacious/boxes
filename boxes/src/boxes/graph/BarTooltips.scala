package boxes.graph

import boxes.graph.GraphMouseEventType._
import boxes.{Var, Val, Box}
import boxes.swing.SwingView
import java.awt.geom.{Line2D}

trait BarTooltipRenderer[C1, C2] {
  def paint(canvas:GraphCanvas, cat1: C1, cat2: C2, bar: Bar, pixelPos:Vec2)
}

class StringBarTooltipRenderer[C1, C2](
    print:((C1, C2, Bar)=>String) = BarTooltips.defaultPrint)
    extends BarTooltipRenderer[C1, C2]{
  
  //TODO refactor this method, maybe into GraphCanvas - very similar in SeriesToolTip stuff.
  def paint(canvas:GraphCanvas, cat1: C1, cat2: C2, bar: Bar, pixelPos:Vec2) {
    val s = print(cat1, cat2, bar)
    val size = canvas.stringSize(s)

    canvas.color = SwingView.shadedBoxColor
    canvas.fillRoundRect(pixelPos - Vec2(-8, 4 + size.y + 8), size + Vec2(8, 8), 6)

    canvas.color = SwingView.selectedTextColor
    canvas.string(s, pixelPos + Vec2(12, -12))
  }
}

object BarTooltips {

  def defaultPrint[C1, C2] = (cat1: C1, cat2: C2, bar: Bar) => cat1.toString() + ", " + cat2.toString() + " = " + bar.value
  
  def apply[C1, C2](enabled:Box[Boolean, _], 
    data: Box[Map[(C1, C2), Bar], _],
    barWidth: Box[Double, _], catPadding: Box[Double, _], 
    barPadding: Box[Double, _],
    renderer:BarTooltipRenderer[C1, C2])
    (implicit ord1: Ordering[C1], ord2: Ordering[C2]) = new BarTooltips(enabled, data, barWidth, catPadding, barPadding, renderer)(ord1, ord2)

  def string[C1, C2](enabled:Box[Boolean, _], 
    data: Box[Map[(C1, C2), Bar], _],
    barWidth: Box[Double, _], catPadding: Box[Double, _], 
    barPadding: Box[Double, _], print:((C1, C2, Bar)=>String) = BarTooltips.defaultPrint)
    (implicit ord1: Ordering[C1], ord2: Ordering[C2]) = new BarTooltips(enabled, data, barWidth, catPadding, barPadding, new StringBarTooltipRenderer(print))(ord1, ord2)
}

class BarTooltips[C1, C2](enabled:Box[Boolean, _], 
    data: Box[Map[(C1, C2), Bar], _],
    barWidth: Box[Double, _], catPadding: Box[Double, _], 
    barPadding: Box[Double, _],
    renderer:BarTooltipRenderer[C1, C2])
    (implicit ord1: Ordering[C1], ord2: Ordering[C2]) extends GraphLayer {

  private val toPaint:Var[Option[(C1, C2, Bar, Vec2)]] = Var(None)

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