package boxes.graph

import boxes.graph.GraphMouseEventType._
import boxes.{Var, Val, Box}
import boxes.swing.SwingView
import java.awt.geom.{Line2D}
import java.awt.Color
import boxes.list.ListCal


object SeriesSelection {
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
  
  def selectedSeriesIndex[K](currentSeries: List[Series[K]], e:GraphMouseEvent): Option[Int] = {
    val pixelPoint = e.spaces.toPixel(e.dataPoint)
    val dataPoint = e.dataPoint
    
    //Search for nearest series line/point, and if it is within maxRadius, and mouse is in data area,
    //set its key and our pixel point to toPaint
    if (e.spaces.dataArea.contains(dataPoint)) {
      val distances = currentSeries.map(s => SeriesSelection.seriesDistance(e.spaces, s, pixelPoint)).zipWithIndex
      val index = distances.minBy(pair => pair._1)._2
      val d = distances(index)._1

      if (d < SeriesTooltips.maxRadius) {
        Some(index)
      } else {
        None
      }
    } else {
      None
    }
  }
    
  def selectedSeries[K](currentSeries: List[Series[K]], e:GraphMouseEvent): Option[Series[K]] = selectedSeriesIndex(currentSeries, e).map(currentSeries(_))
}

object ColorSeriesBySelection {
  def defaultSeriesToUnselected[K](s: Series[K]) = s.copy(color = GraphSeries.blendColors(s.color, GraphSeries.unselectedColor, 0.4), width = 1, shadow = false)
    
  def apply[K](series:Box[List[Series[K]], _], indices:Box[Set[K],_], seriesToUnselected: (Series[K] => Series[K]) = defaultSeriesToUnselected[K] _) = ListCal{
    val unselected = series().collect{
      case s:Series[K] if !indices().contains(s.key) => seriesToUnselected(s)
    }
    val selected = series().filter(s => indices().contains(s.key))

    unselected ::: selected
  }
}

//object ColorSeriesBySelection {
//  def apply[K](series:Box[List[Series[K]], _], indices:Box[Set[K],_], unselectedColor:Color = GraphSeries.unselectedColor, unselectedWidth:Option[Double] = Some(1d)) = ListCal{
//    val unselected = series().collect{
//      case s:Series[K] if !indices().contains(s.key) => s.copy(color = unselectedColor, width = unselectedWidth.getOrElse(s.width))
//    }
//    val selected = series().filter(s => indices().contains(s.key))
//
//    unselected ::: selected
//  }
//}