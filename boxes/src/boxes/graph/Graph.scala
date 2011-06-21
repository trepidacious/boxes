package boxes.graph

import javax.swing.JPanel
import java.awt.{Graphics2D, Color, Graphics}
import boxes._
import java.awt.geom.AffineTransform

//Immutable
case class Vec2D(x:Double = 0, y:Double = 0) {
  def +(b:Vec2D) = Vec2D(x + b.x, y + b.y)
  def -(b:Vec2D) = Vec2D(x - b.x, y - b.y)
  def *(b:Vec2D) = Vec2D(x * b.x, y * b.y)
  def /(b:Vec2D) = Vec2D(x / b.x, y / b.y)
  def *(d:Double) = Vec2D(d*x, d*y)
}

case class Borders(top:Double = 0, left:Double = 0, bottom:Double = 0, right:Double = 0)
case class Series(curve:List[Vec2D], color:Color)

case class Area(origin:Vec2D = Vec2D(), size:Vec2D = Vec2D(1, 1)) {
  def toUnit(v:Vec2D) = (v - origin)/size
  def fromUnit(v:Vec2D) = (v * size) + origin
}


trait Graph {
  def series:RefGeneral[List[Series], _]
  def dataArea:RefGeneral[Area, _]
  def borders:RefGeneral[Borders, _]
}

case class GraphSpaces(dataArea:Area, pixelArea:Area) {
  def toPixel(dataPos:Vec2D) = pixelArea.fromUnit(dataArea.toUnit(dataPos))
  def toData(pixelPos:Vec2D) = dataArea.fromUnit(pixelArea.toUnit(pixelPos))
}

case class GraphBasic(series:RefGeneral[List[Series], _], dataArea:RefGeneral[Area, _] = Val(Area()), borders:RefGeneral[Borders, _] = Val(Borders(8, 64, 32, 8))) extends Graph


class Graphics2DVector(g:Graphics2D) {
  def line(a:Vec2D, b:Vec2D) {
    g.drawLine(a.x.asInstanceOf[Int], a.y.asInstanceOf[Int], b.x.asInstanceOf[Int], b.y.asInstanceOf[Int])
  }
}

class GraphSwingView(graph:Ref[_ <: Graph]) extends SwingView {

  implicit def graphics2DtoVector(g:Graphics2D) = new Graphics2DVector(g)

  val component = new JPanel() {

    var series = List[Series]()
    var dataArea = Area()
    var borders = Borders()

    def repaint(series:List[Series], area:Area, borders:Borders) {
      this.series  = series
      this.dataArea = area
      this.borders = borders
      repaint()
    }

    override def paintComponent(gr:Graphics) {
      val g = gr.create.asInstanceOf[Graphics2D]

      g.setColor(Color.black)
      g.drawString("Hello World", 10, 10)

      val w = getWidth
      val h = getHeight

      val l = borders.left.asInstanceOf[Int]
      val r = borders.right.asInstanceOf[Int]
      val t = borders.top.asInstanceOf[Int]
      val b = borders.bottom.asInstanceOf[Int]
      val dw = w - l - r
      val dh = h - t - b
      g.drawRect(l, t, dw, dh)

      val spaces = GraphSpaces(dataArea, Area(Vec2D(l, t+dh), Vec2D(dw, -dh)))

      for {
        s <- series
      } {
        s.curve.foldLeft(None:Option[Vec2D]){(last, current) => {
          last.foreach(someLast => g.line(spaces.toPixel(someLast), spaces.toPixel(current)))
          Some(current)
        }}
      }

      g.dispose
    }
  }

  val v = View {
    val area = graph().dataArea()
    val borders = graph().borders()
    val series = graph().series()
    replaceUpdate {
      component.repaint(series, area, borders)
    }
  }

}
