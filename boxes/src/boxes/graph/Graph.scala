package boxes.graph

import javax.swing.JPanel
import boxes._
import java.awt.geom.AffineTransform
import java.awt.{RenderingHints, Graphics2D, Color, Graphics}
import java.text.DecimalFormat

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

case class GraphBasic(series:RefGeneral[List[Series], _], dataArea:RefGeneral[Area, _] = Val(Area()), borders:RefGeneral[Borders, _] = Val(Borders(16, 48, 48, 16))) extends Graph


class Graphics2DVector(g:Graphics2D) {
  def line(a:Vec2D, b:Vec2D) {
    g.drawLine(a.x.asInstanceOf[Int], a.y.asInstanceOf[Int], b.x.asInstanceOf[Int], b.y.asInstanceOf[Int])
  }
  def string(s:String, v:Vec2D) {
    val w = g.getFontMetrics.getStringBounds(s, g).getWidth
    val vo = v + Vec2D(-w/2d, 0)

    g.drawString(s, vo.x.asInstanceOf[Int], vo.y.asInstanceOf[Int])
  }
}

object Axis {
  def ticks(range:(Double,Double), pixels:Double, pixelsPerTick:Double) = {
    val dataPerPixel = math.abs(range._2 - range._1) / pixels
    val dataPerTick = pixelsPerTick * dataPerPixel

    //Find a power of ten value for data per tick
    val dataPerTickR = math.pow(10, math.round(math.log10(dataPerTick)).asInstanceOf[Int])

    //See if we would be better using half or 5 times the power of ten
    val dataPerTickBest = List(0.1, 0.5, 2, 5, 10).foldLeft(dataPerTickR)((dataPerTickBest, scale) => {
      val dataPerTickScale = dataPerTickR * scale

      val pixelsPerTickScale = dataPerTickScale / dataPerPixel
      val pixelsPerTickBest = dataPerTickBest / dataPerPixel
      if (math.abs(pixelsPerTickScale - pixelsPerTick) < math.abs(pixelsPerTickBest - pixelsPerTick)) {
        dataPerTickScale
      } else {
        dataPerTickBest
      }
    })

    //Minor ticks are major ticks divided by 5.
    val dataPerMinorTick = dataPerTickBest / 5

    val firstTick = math.floor(range._1 / dataPerMinorTick).asInstanceOf[Int]
    val lastTick = math.ceil(range._2 / dataPerMinorTick).asInstanceOf[Int]

    Range.inclusive(firstTick, lastTick).map(x => (x * dataPerMinorTick, x % 5 == 0)).filter(xMajor => xMajor._1 >= range._1 && xMajor._1 <= range._2)
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
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

      val w = getWidth
      val h = getHeight

      val l = borders.left.asInstanceOf[Int]
      val r = borders.right.asInstanceOf[Int]
      val t = borders.top.asInstanceOf[Int]
      val b = borders.bottom.asInstanceOf[Int]
      val dw = w - l - r
      val dh = h - t - b

      //TODO each section below should be an element in a list,
      //which gets drawn in order using GraphSpaces, g, etc.
      // Best to make a nice class with drawing methods that
      // make sense for graphs, AND also can be backed with
      //Java2D or OpenGL, etc.
      // We need to
      //know when to redraw them though, so they need to be boxes?. Best thing would be to allow
      //them to say when they want to be redrawn, and maybe even cache them
      //to transparent buffers. We can decide how to buffer them - e.g.
      //series in one buffer, everything else in another. "Everything else"
      //gets redrawn when any individual element of it changes, with everything
      //redrawing. This means we don't have a huge buffer overhead.
      //Or do we make them generate their own buffers, then we just become a view
      //of those buffers? Or they return immutable objects that can paint to a
      //GraphSpaces, and they are just Ref[Painter]... this is probably best.
      //At this point, the only thing we do is provide a GraphCanvas to draw to,
      //and everything else handles its own crap.
      //In fact, the GraphCanvas should be a Ref[GraphCanvas], then we are a
      //View of the Ref[GC] and a ListRef[Painter], and we react to either one
      //changing by redrawing as appropriate.

      //data bg
      g.setColor(Color.white)
      g.fillRect(l, t, dw, dh)

      //series
      val oldClip = g.getClip
      g.clipRect(l, t, dw, dh)
      val spaces = GraphSpaces(dataArea, Area(Vec2D(l, t+dh), Vec2D(dw, -dh)))
      for {
        s <- series
      } {
        g.setColor(s.color)
        s.curve.foldLeft(None:Option[Vec2D]){(last, current) => {
          last.foreach(someLast => g.line(spaces.toPixel(someLast), spaces.toPixel(current)))
          Some(current)
        }}
      }
      g.setClip(oldClip)

      //border
      g.setColor(SwingView.dividingColor)
      g.drawRect(l, t, dw, dh)
      g.setColor(SwingView.dividingColor.brighter)
      g.drawLine(l-1, t + dh + 1, l + dw, t + dh + 1)
      g.drawLine(l-1, t, l -1, t + dh + 1)

      //ticks
      val format = new DecimalFormat("0.###");
      val xTicks = Axis.ticks((dataArea.origin.x, dataArea.origin.x + dataArea.size.x), dw, 50)
      xTicks.foreach(t => {
        val (x, major) = t
        val start = spaces.toPixel(Vec2D(x, dataArea.origin.y))
        g.setColor(SwingView.dividingColor)
        g.line(start, start + Vec2D(0, if (major) 8 else 4))
        if (major) {
          g.setColor(SwingView.dividingColor.darker)
          g.string(format.format(x), start + Vec2D(0, 20))
        }
      })


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
