package boxes.graph

import boxes.graph.GraphMouseEventType._
import boxes.{Var, Val, Box}
import boxes.swing.SwingView
import java.awt.geom.{Line2D}

object BarSelection {

  def selectedBar[C1, C2](data: Map[(C1, C2), Bar],
    barWidth: Double, catPadding: Double, 
    barPadding: Double, e:GraphMouseEvent)(implicit ord1: Ordering[C1], ord2: Ordering[C2]): Option[(C1, C2, Bar)] = {
    val pixelPoint = e.spaces.toPixel(e.dataPoint)
    val dataPoint = e.dataPoint
    
    val d = data
    val bw = barWidth
    val cp = catPadding
    val bp = barPadding

    //Look for any bar containing the data point
    val layout = GraphBars.layout(d, bw, cp, ord1, ord2)
    
    def contained(x: Double, bar: Bar) = {
      dataPoint.x >= x + bp/2 && dataPoint.x < x + bw - bp/2 && (Vec2(0, bar.value).intervalContains(dataPoint.y))
    }
    
    val o = for {pos <- layout.positions; bar <- data.get(pos.cat1, pos.cat2) if contained(pos.x, bar)} yield (pos.cat1, pos.cat2, bar)
    o.headOption
  }
  
}