package boxes.graph

import boxes.graph.Axis._
import boxes.graph.GraphMouseEventType._
import java.awt.Color
import java.text.DecimalFormat
import boxes.{VarGeneral, Val, RefGeneral}
import java.util.concurrent.atomic.AtomicReference

object GraphThreshold {
  val format = new DecimalFormat("0.00")
  val handleRadius = 3

  def apply(axis:RefGeneral[Axis, _], value:VarGeneral[Double, _], color:RefGeneral[Color, _], name:RefGeneral[String, _], enabled:RefGeneral[Boolean, _]) = new GraphThreshold(axis, value, color, name, enabled)
}

class GraphThreshold(axis:RefGeneral[Axis, _], value:VarGeneral[Double, _], color:RefGeneral[Color, _], name:RefGeneral[String, _], enabled:RefGeneral[Boolean, _]) extends GraphLayer {

  private val labelWidth = new AtomicReference[Double](0d)

  def paint() = {
    val c = color()
    val a = axis()
    val v = value()
    val n = name()

    (canvas:GraphCanvas) => {
      val label = n + ": " + GraphThreshold.format.format(v)
      labelWidth.set(AxisTooltip.drawAxisLine(canvas, v, a, label, Some(c)))

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
          if ((pixelPerpDistance > -2 && pixelPerpDistance < 18 && pixelDistance > 0 && pixelDistance < labelWidth.get()) || math.abs(pixelPerpDistance) < GraphThreshold.handleRadius) {
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