package boxes.graph

import boxes.graph.Axis._
import boxes.graph.GraphMouseEventType._
import java.awt.Color
import java.text.DecimalFormat
import boxes.{Var, VarGeneral, SwingView, Val, RefGeneral}

object GraphThreshold {
  val format = new DecimalFormat("0.00")
  val handleRadius = 3

  def apply(axis:RefGeneral[Axis, _], value:VarGeneral[Double, _], color:RefGeneral[Color, _], name:RefGeneral[String, _], enabled:RefGeneral[Boolean, _]) = new GraphThreshold(axis, value, color, name, enabled)
}

class GraphThreshold(axis:RefGeneral[Axis, _], value:VarGeneral[Double, _], color:RefGeneral[Color, _], name:RefGeneral[String, _], enabled:RefGeneral[Boolean, _]) extends GraphLayer {

  private var labelWidth = 0d

  def paint() = {
    val c = color()
    val a = axis()
    val v = value()
    val n = name()

    (canvas:GraphCanvas) => {
      val label = n + ": " + GraphThreshold.format.format(v)


      labelWidth = AxisTooltip.drawAxisLine(canvas, v, a, label, Some(c))

//      canvas.clipToData()
//      val dataArea = canvas.spaces.dataArea
//      val start = canvas.spaces.toPixel(dataArea.axisPosition(a, v))
//      val end = start + canvas.spaces.pixelArea.axisPerpVec2(a)
//
//      if (e) {
//        canvas.color = SwingView.transparentColor(c, 0.1)
//        canvas.lineWidth = GraphThreshold.handleRadius * 2 + 1
//        canvas.line(start, end)
//      }
//
//      canvas.color = c
//      canvas.lineWidth = 1
//      canvas.line(start, end)
//
//      val label = n + " = " + GraphThreshold.format.format(v)
//
//      a match {
//        case X => canvas.string(label, start + Vec2(-6, -6), Vec2(0, 0), -1)
//        case Y => canvas.string(label, start + Vec2(6, -6), Vec2(0, 0), 0)
//      }
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
      val pixelPerpDistance = (valuePoint - pixelPoint).onAxis(axis())
      val pixelDistance = (valuePoint - pixelPoint).onAxis(Axis.other(axis())) * (if (axis() == X) 1 else -1)
      e.eventType match {
        case PRESS => {
          if ((pixelPerpDistance > -2 && pixelPerpDistance < 18 && pixelDistance > 0 && pixelDistance < labelWidth) || math.abs(pixelPerpDistance) < GraphThreshold.handleRadius) {
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