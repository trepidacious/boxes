package boxes.graph

import boxes.graph.GraphMouseEventType._
import boxes.{Var, Val, Box}
import boxes.swing.SwingView
import java.awt.geom.{Line2D}
import boxes.Cal
import boxes.VarBox
import java.awt.Color

object BarSelection {

  def selectedBar[C1, C2, K](data: Map[(C1, C2), Bar[K]],
    barWidth: Double, catPadding: Double, 
    barPadding: Double, e:GraphMouseEvent)(implicit ord1: Ordering[C1], ord2: Ordering[C2]): Option[(C1, C2, Bar[K])] = {
    val pixelPoint = e.spaces.toPixel(e.dataPoint)
    val dataPoint = e.dataPoint
    
    val d = data
    val bw = barWidth
    val cp = catPadding
    val bp = barPadding

    //Look for any bar containing the data point
    val layout = GraphBars.layout(d, bw, cp, ord1, ord2)
    
    def contained(x: Double, bar: Bar[_]) = {
      dataPoint.x >= x + bp/2 && dataPoint.x < x + bw - bp/2 && (Vec2(bar.min, bar.max).intervalContains(dataPoint.y))
    }
    
    val o = for {pos <- layout.positions; bar <- data.get(pos.cat1, pos.cat2) if contained(pos.x, bar)} yield (pos.cat1, pos.cat2, bar)
    o.headOption
  }
  
  def selectedBar[C1, C2, K](data: Map[(C1, C2), Bar[K]],
    barWidth: Double, catPadding: Double, 
    barPadding: Double, dataArea: Area)(implicit ord1: Ordering[C1], ord2: Ordering[C2]): List[(C1, C2, Bar[K])] = {
    
    val d = data
    val bw = barWidth
    val cp = catPadding
    val bp = barPadding

    //Look for any bar containing the data point
    val layout = GraphBars.layout(d, bw, cp, ord1, ord2)
    
    def contained(x: Double, bar: Bar[_]) = {
      val barArea = Area(Vec2(x + bp/2, bar.min), Vec2(bw - bp, bar.max))
      barArea.intersects(dataArea)
    }
    
    for {pos <- layout.positions; bar <- data.get(pos.cat1, pos.cat2) if contained(pos.x, bar)} yield (pos.cat1, pos.cat2, bar)
  }
}

object ColorBarByCatSelection {
  def apply[C1, C2, K](data: Box[Map[(C1, C2), Bar[K]], _], selection:Box[Set[(C1, C2)],_], barToUnselected: (Bar[K]=>Bar[K]) = (bar: Bar[K]) => bar.copy(outline = bar.outline.map(_ => GraphSeries.barOutlineUnselectedColor), fill = bar.fill.map(GraphSeries.blendColors(_, GraphSeries.unselectedColor, 0.8)))) = Cal {
    val d = data()
    val s = selection()
    d.map{case (cats, bar) => (cats, if (s.contains(cats)) bar else barToUnselected(bar))}
  }
}

object ColorBarByKeySelection {
  def apply[C1, C2, K](data: Box[Map[(C1, C2), Bar[K]], _], selection:Box[Set[K],_], barToUnselected: (Bar[K]=>Bar[K]) = (bar: Bar[K]) => bar.copy(outline = bar.outline.map(_ => GraphSeries.barOutlineUnselectedColor), fill = bar.fill.map(GraphSeries.blendColors(_, GraphSeries.unselectedColor, 0.8)))) = Cal {
    val d = data()
    val s = selection()
    d.map{case (cats, bar) => (cats, if (s.contains(bar.key)) bar else barToUnselected(bar))}
  }
}

object GraphClickToSelectBarByCat{
  def apply[C1, C2, K](data: Box[Map[(C1, C2), Bar[K]], _], selection:VarBox[Set[(C1, C2)],_], barWidth: Box[Double, _], catPadding: Box[Double, _], 
    barPadding: Box[Double, _], enabled:Box[Boolean, _])(implicit ord1: Ordering[C1], ord2: Ordering[C2]) = new GraphClickToSelectBarByCat[C1, C2, K](data, selection, barWidth, catPadding, 
    barPadding, enabled)(ord1, ord2)
}

class GraphClickToSelectBarByCat[C1, C2, K](data: Box[Map[(C1, C2), Bar[K]], _], selection:VarBox[Set[(C1, C2)],_], barWidth: Box[Double, _], catPadding: Box[Double, _], 
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

object GraphSelectBarsByCatWithBox {
  def apply[C1, C2, K](data: Box[Map[(C1, C2), Bar[K]], _], selection:VarBox[Set[(C1, C2)],_], barWidth: Box[Double, _], catPadding: Box[Double, _], 
    barPadding: Box[Double, _], enabled:Box[Boolean, _], fill:Box[Color, _], outline:Box[Color, _])
    (implicit ord1: Ordering[C1], ord2: Ordering[C2]) = 
      new GraphBox(fill, outline, enabled, (area:Area, spaces:GraphSpaces) => {
        val bs = BarSelection.selectedBar(data(), barWidth(), catPadding(), barPadding(), area);
        selection() = Set(bs.map(b => (b._1, b._2)): _*)
    })
}


object GraphClickToSelectBarByKey{
  def apply[C1, C2, K](data: Box[Map[(C1, C2), Bar[K]], _], selection:VarBox[Set[K],_], barWidth: Box[Double, _], catPadding: Box[Double, _], 
    barPadding: Box[Double, _], enabled:Box[Boolean, _])(implicit ord1: Ordering[C1], ord2: Ordering[C2]) = new GraphClickToSelectBarByKey[C1, C2, K](data, selection, barWidth, catPadding, 
    barPadding, enabled)(ord1, ord2)
}

class GraphClickToSelectBarByKey[C1, C2, K](data: Box[Map[(C1, C2), Bar[K]], _], selection:VarBox[Set[K],_], barWidth: Box[Double, _], catPadding: Box[Double, _], 
    barPadding: Box[Double, _], enabled:Box[Boolean, _])(implicit ord1: Ordering[C1], ord2: Ordering[C2]) extends GraphLayer {

  def paint() = (canvas:GraphCanvas) => {}

  def onMouse(e:GraphMouseEvent) = {
    if (enabled()) {
      Box.transact{
        e.eventType match {
          case CLICK => {
            val selectedSeries = BarSelection.selectedBar(data(), barWidth(), catPadding(), barPadding(), e)(ord1, ord2)
            selectedSeries.foreach((ss) => selection() = Set(ss._3.key))
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

object GraphSelectBarsByKeyWithBox {
  def apply[C1, C2, K](data: Box[Map[(C1, C2), Bar[K]], _], selection:VarBox[Set[K],_], barWidth: Box[Double, _], catPadding: Box[Double, _], 
    barPadding: Box[Double, _], enabled:Box[Boolean, _], fill:Box[Color, _], outline:Box[Color, _])
    (implicit ord1: Ordering[C1], ord2: Ordering[C2]) = 
      new GraphBox(fill, outline, enabled, (area:Area, spaces:GraphSpaces) => {
        val bs = BarSelection.selectedBar(data(), barWidth(), catPadding(), barPadding(), area);
        selection() = Set(bs.map(b => b._3.key): _*)
    })
}