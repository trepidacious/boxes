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
import scala.collection._

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
    
    //Round before drawing, so that inset works exactly on pixels.
    val pixelArea = canvas.spaces.toPixel(area).round();
    
    if (shadow) {
      canvas.color = GraphSeries.barShadowColor
      canvas.fillRect(pixelArea.translate(GraphSeries.barShadowOffset))
    } else {
      bar.fill.foreach((c:Color) => {
        canvas.color = c
        canvas.fillRect(pixelArea)
        canvas.color = new Color(1f, 1f, 1f, 0.4f)
        canvas.drawRect(pixelArea.inset(1, 1, 1, 1))
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

case class Bar(value: Double, rangeMin: Option[Double] = None, rangeMax: Option[Double] = None, fill: Option[Color], outline: Option[Color] = Some(GraphSeries.barOutlineColor), width: Double = 1, painter: BarPainter = BarStyles.plain) {
  def min = math.min(math.min(value, rangeMin.getOrElse(value)), rangeMax.getOrElse(value))
  def max = math.max(math.max(value, rangeMin.getOrElse(value)), rangeMax.getOrElse(value))  
  def interval = Vec2(min, max)
}

case class GraphBarPosition[C1, C2](cat1: C1, cat2: C2, x: Double)
case class GraphBarLayout[C1, C2](positions: List[GraphBarPosition[C1, C2]], cat1Positions: Map[C1, Vec2])

object GraphBars {
  def layout[C1, C2](d: Map[(C1, C2), Bar], barWidth: Double, catPadding: Double, ord1: Ordering[C1], ord2: Ordering[C2]) = {
    val cat1s = SortedSet(d.keySet.map(_._1).toSeq:_*)(ord1)
    val cat2s = SortedSet(d.keySet.map(_._2).toSeq:_*)(ord2)

    val xs = mutable.ListBuffer[GraphBarPosition[C1, C2]]()
    val cat1Positions = mutable.Map[C1, Vec2]()
    
    var x = 0.0;
    for (cat1 <- cat1s) {
      val xStart = x
      for (cat2 <- cat2s) {
        d.get((cat1, cat2)).foreach((bar: Bar) => {
          xs.append(GraphBarPosition(cat1, cat2, x))
          x+=barWidth;
        })
      }
      cat1Positions.put(cat1, Vec2(xStart, x))
      x+=catPadding;
    }
    
    GraphBarLayout(List(xs: _*), cat1Positions.toMap)
  }
  
  def integerTicks(range:(Double,Double)) = {
    val firstTick = math.floor(range._1).asInstanceOf[Int]
    val lastTick = math.ceil(range._2).asInstanceOf[Int]

    Range.inclusive(firstTick, lastTick).filter(x => x >= range._1 && x <= range._2)
  }

}


class GraphBarAxis[C1, C2](data: Box[Map[(C1, C2), Bar], _], barWidth: Box[Double, _], catPadding: Box[Double, _], 
    barPadding: Box[Double, _], val axis:Axis, 
    cat1Print: (C1 => String) = (c: C1)=>c.toString, 
    cat2Print: (C2 => String) = (c: C2)=>c.toString)
    (implicit ord1: Ordering[C1], ord2: Ordering[C2]) extends UnboundedGraphDisplayLayer {

  def paint() = {
    val d = data()
    val bw = barWidth()
    val cp = catPadding()
    val bp = barPadding()
    
    (canvas:GraphCanvas) => {
      val dataArea = canvas.spaces.dataArea
      
      val ticks = GraphBars.integerTicks(dataArea.axisBounds(axis))

      val layout = GraphBars.layout(d, bw, cp, ord1, ord2)
      
      //Secondary category labels
      for (pos <- layout.positions; bar <- d.get(pos.cat1, pos.cat2)) {
        
        val p = pos.x + bw/2
        if (canvas.spaces.dataArea.axisContains(axis, p)) {
          val start = canvas.spaces.toPixel(dataArea.axisPosition(axis, p))
          val text = cat2Print(pos.cat2)
          canvas.color = GraphAxis.fontColor
          canvas.fontSize = GraphAxis.fontSize
          axis match {
            case X => canvas.string(text, start + Vec2(0, 10), Vec2(0.5, 1))
            case Y => canvas.string(text, start + Vec2(-10, 0), Vec2(1, 0.5))
          }
        }
      }

      //Now primary category labels
      for (cat1 <- layout.cat1Positions.keySet; pos <- layout.cat1Positions.get(cat1)) {
        
        val p = (pos.x + pos.y)/2
        if (canvas.spaces.dataArea.axisContains(axis, p)) {
          val start = canvas.spaces.toPixel(dataArea.axisPosition(axis, p))
          val text = cat1Print(cat1)
          canvas.color = GraphAxis.fontColor
          canvas.fontSize = GraphAxis.titleFontSize
          axis match {
            case X => canvas.string(text, start + Vec2(0, 28), Vec2(0.5, 1))
            case Y => canvas.string(text, start + Vec2(-28, 0), Vec2(1, 0.5))
          }
        }
      }
      
      ticks.foreach(p => {

        val start = canvas.spaces.toPixel(dataArea.axisPosition(axis, p))

        canvas.color = GraphAxis.axisColor
        axis match {
          case X => canvas.line(start, start + Vec2(0, 8))
          case Y => canvas.line(start, start + Vec2(-8, 0))
        }
        canvas.color = GraphAxis.axisHighlightColor
        axis match {
          case X => canvas.line(start + Vec2(1, 0), start + Vec2(1, 8))
          case Y => canvas.line(start + Vec2(0, 1), start + Vec2(-8, 1))
        }

      })
    }
  }
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
      canvas.clipToData

      for (pos <- GraphBars.layout(d, bw, cp, ord1, ord2).positions; bar <- d.get(pos.cat1, pos.cat2)) {
        bar.painter.paint(canvas, pos.x + bp/2, bw-bp, bar, shadow)
      }
      
    }
  }
  
  val dataBounds = Cal{
    val d = data();
    if (d.isEmpty) {
      None
    } else {
      //Convert Bars to Vec2 of min/max, then find the overall range min/max
      val range = d.values.toList.map(_.interval).reduceLeft(_ intervalUnion _)
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
