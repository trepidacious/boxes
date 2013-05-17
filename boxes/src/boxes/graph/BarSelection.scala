package boxes.graph

import boxes.graph.GraphMouseEventType._
import boxes.{Var, Val, Box}
import boxes.swing.SwingView
import java.awt.geom.{Line2D}
import boxes.Cal
import boxes.VarBox

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

object ColorBarBySelection {
  def apply[C1, C2](data: Box[Map[(C1, C2), Bar], _], selection:Box[Set[(C1, C2)],_], barToUnselected: (Bar=>Bar) = (bar) => bar.copy(outline = bar.outline.map(_ => GraphSeries.barOutlineUnselectedColor), fill = bar.fill.map(GraphSeries.blendColors(_, GraphSeries.unselectedColor, 0.8)))) = Cal {
    val d = data()
    val s = selection()
    d.map{case (cats, bar) => (cats, if (s.contains(cats)) bar else barToUnselected(bar))}
  }
}

object GraphClickToSelectBar{
  def apply[C1, C2](data: Box[Map[(C1, C2), Bar], _], selection:VarBox[Set[(C1, C2)],_], barWidth: Box[Double, _], catPadding: Box[Double, _], 
    barPadding: Box[Double, _], enabled:Box[Boolean, _])(implicit ord1: Ordering[C1], ord2: Ordering[C2]) = new GraphClickToSelectBar[C1, C2](data, selection, barWidth, catPadding, 
    barPadding, enabled)(ord1, ord2)
}

class GraphClickToSelectBar[C1, C2](data: Box[Map[(C1, C2), Bar], _], selection:VarBox[Set[(C1, C2)],_], barWidth: Box[Double, _], catPadding: Box[Double, _], 
    barPadding: Box[Double, _], enabled:Box[Boolean, _])(implicit ord1: Ordering[C1], ord2: Ordering[C2]) extends GraphLayer {

  def paint() = (canvas:GraphCanvas) => {}

  def onMouse(e:GraphMouseEvent) = {
    if (enabled()) {
      Box.transact{
        e.eventType match {
          case CLICK => {
            val selectedSeries = BarSelection.selectedBar(data(), barWidth(), catPadding(), barPadding(), e)(ord1, ord2)
            selectedSeries.foreach((ss) => selection() = Set((ss._1, ss._2)))
            selectedSeries.isDefined
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