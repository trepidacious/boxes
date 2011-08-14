package boxes.graph

import boxes.graph.Axis._
import boxes.graph.GraphMouseEventType._
import java.awt.Color
import boxes.{VarGeneral, SwingView, Val, RefGeneral}

object GraphThreshold {
  val handleRadius = 4

  def apply(axis:RefGeneral[Axis, _], value:VarGeneral[Double, _], color:RefGeneral[Color, _], name:RefGeneral[String, _], enabled:RefGeneral[Boolean, _]) = new GraphThreshold(axis, value, color, name, enabled)
}

class GraphThreshold(axis:RefGeneral[Axis, _], value:VarGeneral[Double, _], color:RefGeneral[Color, _], name:RefGeneral[String, _], enabled:RefGeneral[Boolean, _]) extends GraphLayer {

  def paint() = {
    val c = color()
    val a = axis()
    val v = value()
    val e = enabled()
    val n = name()

    (canvas:GraphCanvas) => {
      canvas.clipToData()
      val dataArea = canvas.spaces.dataArea
      val start = canvas.spaces.toPixel(dataArea.axisPosition(a, v))
      val end = start + canvas.spaces.pixelArea.axisPerpVec2(a)

      if (e) {
        canvas.color = SwingView.transparentColor(c, 0.1)
        canvas.lineWidth = GraphThreshold.handleRadius * 2 + 1
        canvas.line(start, end)
      }

      canvas.color = c
      canvas.lineWidth = 1
      canvas.line(start, end)

      a match {
        case X => canvas.string(n, start + Vec2(-6, -6), Vec2(0, 0), -1)
        case Y => canvas.string(n, start + Vec2(6, -6), Vec2(0, 0), 0)
      }
    }
  }

  private var pressOffset:Option[Double] = None

  def dataPointValue(point:Vec2) = axis() match {
    case X => point.x
    case Y => point.y
  }

  def onMouse(e:GraphMouseEvent) = {
    if (enabled()) {
      val pixelPoint = e.spaces.toPixel(e.dataPoint)
      val valuePoint = e.spaces.toPixel(e.spaces.dataArea.axisPosition(axis(), value()))
      val pixelDistance = math.abs((valuePoint - pixelPoint).onAxis(axis()))
      e.eventType match {
        case PRESS => {
          if (pixelDistance < GraphThreshold.handleRadius + 3) {
            pressOffset = Some(value() - dataPointValue(e.dataPoint))
            true
          } else {
            false
          }
        }
        case DRAG => pressOffset match {
          case Some(offset) => {
            value() = dataPointValue(e.dataPoint) + offset
            true
          }
          case None => false
        }
        case RELEASE => pressOffset match {
          case Some(offset) => {
            value() = dataPointValue(e.dataPoint) + offset
            pressOffset = None
            true
          }
          case None => false
        }
        case _ => false
      }
    } else {
      false
    }
  }

  val dataBounds = Val(None:Option[Area])

}