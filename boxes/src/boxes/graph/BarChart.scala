package boxes.graph

import boxes._
import boxes.list._
import boxes.general._
import boxes.graph.Axis._
import boxes.graph.GraphMouseEventType._
import java.awt.Color
import java.text.DecimalFormat
import boxes.{VarBox, Val, Box}
import java.util.concurrent.atomic.AtomicReference
import scala.collection.immutable.SortedSet

trait BarPainter{
  def paint(canvas:GraphCanvas, 
      x: Double, barWidth: Double,
      bar: Bar,
      shadow:Boolean = false)
}

class PlainBarPainter extends BarPainter {
  def paint(canvas:GraphCanvas,
      x: Double, barWidth: Double,
      bar: Bar,
      shadow:Boolean = false) {
    
    val area = Area(Vec2(x, 0), Vec2(barWidth, bar.value))
    val pixelArea = canvas.spaces.toPixel(area);
    
    if (shadow) {
      canvas.color = GraphSeries.barShadowColor
      canvas.fillRect(pixelArea.translate(GraphSeries.barShadowOffset))
    } else {
      bar.fill.foreach((c:Color) => {
        canvas.color = c
        canvas.fillRect(pixelArea)
      })
      bar.outline.foreach((c:Color) => {
        canvas.color = c
        canvas.lineWidth = bar.width
        canvas.drawRect(pixelArea)
      })
    }
    
    //TODO draw the range bars
  }
}

object BarStyles {
  val plain = new PlainBarPainter()
}

case class Bar(value: Double, rangeMin: Double, rangeMax: Double, fill: Option[Color], outline: Option[Color] = Some(GraphSeries.barOutlineColor), width: Double = 1, painter: BarPainter = BarStyles.plain) {
  def min = math.min(math.min(value, rangeMin), rangeMax)
  def max = math.max(math.max(value, rangeMin), rangeMax)  
}

class GraphBars[C1, C2](
    data: Box[Map[(C1, C2), Bar], _],
    barWidth: Box[Double, _], catPadding: Box[Double, _], 
    barPadding: Box[Double, _],
    shadow: Boolean = false)
    (implicit ord1: Ordering[C1], ord2: Ordering[C2]) extends GraphLayer {

  def paint() = {
    val d = data()
    val bw = barWidth()
    val cp = catPadding()
    val bp = barPadding()
    (canvas:GraphCanvas) => {
      val cat1s = SortedSet(d.keySet.map(_._1).toSeq:_*)(ord1)
      val cat2s = SortedSet(d.keySet.map(_._2).toSeq:_*)(ord2)
      
      canvas.clipToData
      
      var x = 0.0;
      for (cat1 <- cat1s) {
        for (cat2 <- cat2s) {
          d.get((cat1, cat2)).foreach((bar: Bar) => {
            bar.painter.paint(canvas, x + bp/2, bw-bp, bar, shadow)
            x+=bw;
          })
        }
        x+=cp;
      }
    }
  }
  
  val dataBounds = Cal{
    val d = data();
    if (d.isEmpty) {
      None
    } else {
      //Convert Bars to Vec2 of min/max, then find the overall range min/max
      val range = d.values.toList.map((v) => Vec2(v.min, v.max)).reduceLeft((a, b) => Vec2(math.min(a.x, b.x), math.max(a.y, b.y)))
      val cat1s = SortedSet(d.keySet.map(_._1).toSeq:_*)(ord1)
      val barCount = d.size
      //We display with specified width per Bar. We pad on between eachcat1 group with
      //an additional specified padding unit
      val width = (math.max(0, cat1s.size - 1)) * catPadding() + barCount * barWidth();

      Some(Area(Vec2(0, range.x), Vec2(width, range.y-range.x)))
    }
  }

  def onMouse(event:GraphMouseEvent) = false

}
