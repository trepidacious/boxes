package boxes.swing

import javax.swing.{JPanel, ImageIcon}
import java.awt.image.BufferedImage
import java.awt.event.{MouseListener, MouseMotionListener, MouseEvent, ComponentListener, ComponentEvent}
import boxes.graph._
import boxes.graph.GraphMouseEventType._
import boxes.graph.GraphMouseButton._
import java.awt.geom.{Rectangle2D, PathIterator, Path2D, AffineTransform}
import java.awt.{AlphaComposite, Graphics, Image, Color, RenderingHints, BasicStroke, Graphics2D, BorderLayout}
import boxes._
import boxes.list._
import boxes.general._
import java.util.concurrent.atomic.AtomicBoolean
import boxes.swing.icons.IconFactory
import boxes.BoxImplicits._


class GraphCanvasFromGraphics2D(g:Graphics2D, val spaces:GraphSpaces) extends GraphCanvas {

  val defaultClip = g.getClip
  val defaultFont = g.getFont

  var c = Color.black
  var w = 1d
  var fs = 10d

  {
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
  }

  def color_=(color:Color) {
    g.setColor(color)
    c = color
  }
  def color = c

  def lineWidth_=(lineWidth:Double) {
    g.setStroke(new BasicStroke(lineWidth.asInstanceOf[Float], BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER))
    w = lineWidth
  }
  def lineWidth = w

  def fontSize_=(fontSize:Double) {
    g.setFont(defaultFont.deriveFont(fontSize.asInstanceOf[Float]))
    fs = fontSize
  }
  def fontSize = fs

  def dataLine(a:Vec2, b:Vec2) {
    g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE)
    line(spaces.toPixel(a), spaces.toPixel(b))
    g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_DEFAULT)
  }

  def line(a:Vec2, b:Vec2) {
    g.drawLine(a.x.asInstanceOf[Int], a.y.asInstanceOf[Int], b.x.asInstanceOf[Int], b.y.asInstanceOf[Int])
  }

  def stringSize(s:String) = {
    val d = g.getFontMetrics.getStringBounds(s, g)
    Vec2(d.getWidth, d.getHeight)
  }

  def string(s:String, v:Vec2, align:Vec2 = Vec2.zero, rotateQuadrants:Int = 0) {

    val d = g.getFontMetrics.getStringBounds(s, g)
    val w = d.getWidth
    val h = d.getHeight

    val x = v.x.asInstanceOf[Int]
    val y = v.y.asInstanceOf[Int]

    val oldxForm = g.getTransform
    val t = g.getTransform
    t.concatenate(AffineTransform.getQuadrantRotateInstance(rotateQuadrants, x, y))
    g.setTransform(t)

    val vo = v + Vec2(-w * align.x, h * align.y)

    val ox = vo.x.asInstanceOf[Int]
    val oy = vo.y.asInstanceOf[Int]

    g.drawString(s, ox, oy)

    g.setTransform(oldxForm)
  }

  //Convert from a pair of vecs that may draw a "backwards"
  //rect with negative size(s) to the dumb format needed by Java2D,
  //with individual int values that must have a positive width and height
  def toDumbFormat(origin:Vec2, size:Vec2) = {
    var x = origin.x.asInstanceOf[Int]
    var y = origin.y.asInstanceOf[Int]
    var w = size.x.asInstanceOf[Int]
    var h = size.y.asInstanceOf[Int]
    if (h < 0) {
      y = y + h
      h = -h
    }
    if (w < 0) {
      x = x + w
      w = -w
    }
    (x, y, w, h)
  }

  def rect(origin:Vec2, size:Vec2, fill:Boolean) {
    val df = toDumbFormat(origin, size)
    if (fill) {
      g.fillRect(df._1, df._2, df._3, df._4)
    } else {
      g.drawRect(df._1, df._2, df._3, df._4)
    }
  }

  def fillRect(area:Area) {
    rect(area, true)
  }
  def drawRect(area:Area) {
    rect(area, false)
  }
  def rect(area:Area, fill:Boolean) {
    rect(area.origin, area.size, fill)
  }

  def fillRect(origin:Vec2, size:Vec2) {
    rect(origin, size, true)
  }

  def drawRect(origin:Vec2, size:Vec2) {
    rect(origin, size, false)
  }


  def roundRect(origin: Vec2, size: Vec2, fill: Boolean, radius: Double) {
    val df = toDumbFormat(origin, size)
    val r = radius.asInstanceOf[Int]
    if (fill) {
      g.fillRoundRect(df._1, df._2, df._3, df._4, r, r)
    } else {
      g.drawRoundRect(df._1, df._2, df._3, df._4, r, r)
    }
  }

  def roundRect(area: Area, fill: Boolean, radius: Double) {
    roundRect(area.origin, area.size, fill, radius)
  }

  def fillRoundRect(origin: Vec2, size: Vec2, radius: Double) {
    roundRect(origin, size, true, radius)
  }
  def fillRoundRect(area: Area, radius: Double) {
    roundRect(area, true, radius)
  }
  def drawRoundRect(origin: Vec2, size: Vec2, radius: Double){
    roundRect(origin, size, false, radius)
  }
  def drawRoundRect(area: Area, radius: Double){
    roundRect(area, false, radius)
  }


  def clipToRect(origin:Vec2, size:Vec2) {
    val df = toDumbFormat(origin, size)
    g.setClip(df._1, df._2, df._3, df._4)
  }

  def clipToData() {
    clipToRect(spaces.pixelArea.origin, spaces.pixelArea.size)
  }

  def clipToAll() {
    g.setClip(defaultClip)
  }

  def image(i:Image, origin:Vec2, size:Vec2) {
    // TODO should we use dumb format, or leave potentially mirrored image? Check whether mirrored image actually draws.
//    val df = toDumbFormat(origin, size)
    g.drawImage(i, origin.x.asInstanceOf[Int], origin.y.asInstanceOf[Int], size.x.asInstanceOf[Int], size.y.asInstanceOf[Int], null)
  }

  def image(i:Image, origin:Vec2) {
    image(i, origin, Vec2(i.getWidth(null), i.getHeight(null)))
  }

  def path(path:List[Vec2]) {
    val path2D = new Path2D.Double()
    path2D.append(new VecListPathIterator(path), false)
    g.draw(path2D)
  }

  def dataPath(dataPath:List[Vec2]) {
    g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE)
    path(dataPath.map(p => spaces.toPixel(p)))
    g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_DEFAULT)
  }
  
  def drawTooltip(s: String, v: Vec2) {
    val size = stringSize(s)

    val offRight = (v.round + size).x - spaces.pixelArea.axisBounds(Axis.X)._2 + 8;
    val pp = if (offRight > 0) {
      v.round - (Vec2(offRight, 0))
    } else {
      v.round
    }
    
    color = SwingView.shadedBoxColor
    fillRoundRect(pp - Vec2(-8, 4 + size.y + 8), size + Vec2(8, 8), 6)

    color = SwingView.selectedTextColor
    string(s, pp + Vec2(12, -12))
  }
}


object VecListPathIterator {
  def apply(path:List[Vec2]) = {
    val path2D = new Path2D.Double()
    path2D.append(new VecListPathIterator(path), false)
    path2D
  }
}

class VecListPathIterator(list:List[Vec2]) extends PathIterator {
  var remaining = list
  var first = true

  def getWindingRule = PathIterator.WIND_NON_ZERO
  def isDone = remaining.isEmpty
  def next() {
    remaining = remaining.tail
  }
  def currentSegment(coords:Array[Float]) = {
    coords.update(0, remaining.head.x.asInstanceOf[Float])
    coords.update(1, remaining.head.y.asInstanceOf[Float])
    if (first) {
      first = false
      PathIterator.SEG_MOVETO
    } else {
      PathIterator.SEG_LINETO
    }
  }
  def currentSegment(coords:Array[Double]) = {
    coords.update(0, remaining.head.x)
    coords.update(1, remaining.head.y)
    if (first) {
      first = false
      PathIterator.SEG_MOVETO
    } else {
      PathIterator.SEG_LINETO
    }
  }
}

class GraphBuffer(var image:BufferedImage = new BufferedImage(10, 10, BufferedImage.TYPE_INT_ARGB)) {
  val clearComposite = AlphaComposite.getInstance(AlphaComposite.CLEAR, 0.0f)
  val lock = new Object()
  def ensureSize(area:Vec2) {
    val w = math.max(area.x.asInstanceOf[Int], 1)
    val h = math.max(area.y.asInstanceOf[Int], 1)

    if (image.getWidth != w || image.getHeight != h) {
      image = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
    }
  }
  def clear() {
    val g = image.getGraphics.asInstanceOf[Graphics2D]
    g.setComposite(clearComposite)
    g.fill(new Rectangle2D.Double(0,0,image.getWidth,image.getHeight))
    g.dispose()
  }
}




object GraphSwingView {

  def icon(name:String) = IconFactory.icon(name)

  val zoom = icon("Zoom")
  val zoomIn = icon("ZoomIn")
  val zoomOut = icon("ZoomOut")
  val zoomSelect = icon("ZoomSelect")
  val boxSelect = icon("BoxSelect")
  val grabHand = icon("GrabHand")
  val move = icon("Move")

  def apply(graph:Ref[_ <: Graph]) = new GraphSwingView(graph)

  //TODO can this be done with currying?
  //Make a panel with series, using normal view
  def panelWithSeries[K](
    series:ListRef[Series[K]],
    selection:Var[Set[K]] = Var(Set[K]()),

    xName:Ref[String] = Val("x"),
    yName:Ref[String] = Val("y"),
    
    xAxis:Ref[GraphZoomerAxis] = Val(GraphZoomerAxis()),
    yAxis:Ref[GraphZoomerAxis] = Val(GraphZoomerAxis()),

    graphName: Ref[String] = Val(""),
    
    zoom:Boolean = true,
    select:Boolean = false,
    grab:Boolean = false,
    
    seriesTooltips:Boolean = false,
    axisTooltips:Boolean = true,
    
    borders:Ref[Borders] = Val(Borders(16, 74, 53, 16)),
    extraMainLayers:List[GraphLayer] = List[GraphLayer](),
    extraOverLayers:List[GraphLayer] = List[GraphLayer]()
  ) = GraphSwing.panelWithSeries(g => GraphSwingView(g))(series, selection, xName, yName, xAxis, yAxis, graphName, zoom, select, grab, seriesTooltips, axisTooltips, borders, extraMainLayers, extraOverLayers)

}


//TODO remove shared code from GraphSwingView and GraphSwingBGView
class GraphSwingView(graph:Ref[_ <: Graph]) extends SwingView {

  val componentSize = Var(Vec2(10, 10))

  val mainBuffer = new GraphBuffer()
  val overBuffer = new GraphBuffer()

  val concBuffer = new GraphBuffer()
  val concBackBuffer = new GraphBuffer()

  val component = new JPanel() {

    override def paintComponent(gr:Graphics) {
      mainBuffer.lock.synchronized{
        gr.drawImage(mainBuffer.image, 0, 0, null)
      }
      concBuffer.lock.synchronized{
        gr.drawImage(concBuffer.image, 0, 0, null)
      }
      overBuffer.lock.synchronized{
        gr.drawImage(overBuffer.image, 0, 0, null)
      }
    }

    def updateSize() {
      componentSize() = Vec2(this.getWidth, this.getHeight)
    }

    this.addComponentListener(new ComponentListener(){
      override def componentResized(e:ComponentEvent) {
        updateSize()
      }
      override def componentMoved(e:ComponentEvent) {}
      override def componentShown(e:ComponentEvent) {}
      override def componentHidden(e:ComponentEvent) {}
    })

    def fireMouse(e:MouseEvent, eventType:GraphMouseEventType) {
      val s = buildSpaces
      val p = e.getPoint
      val b = e.getButton match {
        case MouseEvent.BUTTON1 => LEFT
        case MouseEvent.BUTTON2 => MIDDLE
        case MouseEvent.BUTTON3 => RIGHT
        case _ => NONE
      }
      var x = p.x
      var y = p.y
      val w = getWidth
      val h = getHeight
      if (x < 0) {
        x = 0
      } else if (x > w) {
        x = w
      }
      if (y < 0) {
        y = 0
      } else if (y > h) {
        y = h
      }

      val dataPoint = s.toData(Vec2(x, y))
      val gme = GraphMouseEvent(s, dataPoint, eventType, b)
      val consumedGME = GraphMouseEvent(s, dataPoint, CONSUMED, b)

      val consumed = graph().overlayers().foldLeft(false)((consumed, layer) => if(!consumed) layer.onMouse(gme) else {layer.onMouse(consumedGME); true})
      graph().layers().foldLeft(consumed)((consumed, layer) => if(!consumed) layer.onMouse(gme) else {layer.onMouse(consumedGME); true})

    }

    this.addMouseMotionListener(new MouseMotionListener() {
      def mouseDragged(e: MouseEvent) {
        fireMouse(e, DRAG)
      }
      def mouseMoved(e: MouseEvent) {
        fireMouse(e, MOVE)
      }
    })

    this.addMouseListener(new MouseListener(){
      def mouseClicked(e: MouseEvent) {
        fireMouse(e, CLICK)
      }
      def mousePressed(e: MouseEvent) {
        fireMouse(e, PRESS)
      }
      def mouseReleased(e: MouseEvent) {
        fireMouse(e, RELEASE)
      }
      def mouseEntered(e: MouseEvent) {
        fireMouse(e, ENTER)
      }
      def mouseExited(e: MouseEvent) {
        fireMouse(e, EXIT)
      }
    })
  }

  val mainView = View {
    drawBuffer(mainBuffer, graph().layers())
    replaceUpdate {
      component.repaint()
    }
  }

  val overView = View {
    drawBuffer(overBuffer, graph().overlayers())
    replaceUpdate {
      component.repaint()
    }
  }

  def buildSpaces = {

    val size = componentSize()
    val area = graph().dataArea()
    val borders = graph().borders()

    val w = size.x.asInstanceOf[Int]
    val h = size.y.asInstanceOf[Int]

    val l = borders.left.asInstanceOf[Int]
    val r = borders.right.asInstanceOf[Int]
    val t = borders.top.asInstanceOf[Int]
    val b = borders.bottom.asInstanceOf[Int]
    val dw = w - l - r
    val dh = h - t - b

    GraphSpaces(area, Area(Vec2(l, t+dh), Vec2(dw, -dh)), Area(Vec2.zero, size))
  }

  def drawBuffer(buffer:GraphBuffer, layers:List[GraphLayer]) {
    buffer.lock.synchronized{
      val spaces = buildSpaces

      val w = spaces.componentArea.size.x.asInstanceOf[Int]
      val h = spaces.componentArea.size.y.asInstanceOf[Int]

      buffer.ensureSize(spaces.componentArea.size)

      val g = buffer.image.getGraphics.asInstanceOf[Graphics2D]
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

      val oldComposite = g.getComposite
      g.setComposite(AlphaComposite.getInstance(AlphaComposite.CLEAR, 0.0f))
      g.fill(new Rectangle2D.Double(0,0,w,h))
      g.setComposite(oldComposite)

      //Each layer paints on a fresh canvas, to avoid side effects from one affecting the next
      layers.foreach(layer => {
        val paint = layer.paint()
        Box.withoutReading {
          paint.apply(new GraphCanvasFromGraphics2D(g.create().asInstanceOf[Graphics2D], spaces))
        }
      })

      g.dispose
    }
  }
}


object GraphSwingBGView {
  def apply(graph:Ref[_ <: Graph]) = new GraphSwingBGView(graph)
  
  //Make a panel with series, using bg view
  def panelWithSeries[K](
    series:ListRef[Series[K]],
    selection:Var[Set[K]] = Var(Set[K]()),

    xName:Ref[String] = Val("x"),
    yName:Ref[String] = Val("y"),
    
    xAxis:Ref[GraphZoomerAxis] = Val(GraphZoomerAxis()),
    yAxis:Ref[GraphZoomerAxis] = Val(GraphZoomerAxis()),

    graphName: Ref[String] = Val(""),
    
    zoom:Boolean = true,
    select:Boolean = false,
    grab:Boolean = false,
    
    seriesTooltips:Boolean = false,
    axisTooltips:Boolean = true,
    
    borders:Ref[Borders] = Val(Borders(16, 74, 53, 16)),
    extraMainLayers:List[GraphLayer] = List[GraphLayer](),
    extraOverLayers:List[GraphLayer] = List[GraphLayer]()
  ) = GraphSwing.panelWithSeries(g => GraphSwingBGView(g))(series, selection, xName, yName, xAxis, yAxis, graphName, zoom, select, grab, seriesTooltips, axisTooltips, borders, extraMainLayers, extraOverLayers)
}

object GraphSwing {
  
  def panelWithSeries[K](makeView: Ref[_ <: Graph] => SwingView)(
    
    series:ListRef[Series[K]],
    selection:Var[Set[K]] = Var(Set[K]()),

    xName:Ref[String] = Val("x"),
    yName:Ref[String] = Val("y"),
    
    xAxis:Ref[GraphZoomerAxis] = Val(GraphZoomerAxis()),
    yAxis:Ref[GraphZoomerAxis] = Val(GraphZoomerAxis()),

    graphName: Ref[String] = Val(""),
    
    zoom:Boolean = true,
    select:Boolean = false,
    grab:Boolean = false,
    
    seriesTooltips:Boolean = false,
    axisTooltips:Boolean = true,
    
    borders:Ref[Borders] = Val(Borders(16, 74, 53, 16)),
    extraMainLayers:List[GraphLayer] = List[GraphLayer](),
    extraOverLayers:List[GraphLayer] = List[GraphLayer]()
  ) = {
        
    val zoomEnabled = if (zoom) Some(Var(true)) else None
    val selectEnabled = if (select) Some(Var(false)) else None
    val grabEnabled = if (grab) Some(Var(false)) else None

    val modes = List(zoomEnabled, selectEnabled, grabEnabled).collect{case Some(v) => v}
    if (!modes.isEmpty) RadioReaction(modes:_*)
    
    val axisTooltipsEnabled = if (axisTooltips) Some(Var(true)) else None
    val seriesTooltipsEnabled = if (seriesTooltips) Some(Var(true)) else None

    val manualBounds = Var(None:Option[Area])

    import boxes.graph.Axis._

    val graph = Var (
      GraphBasic.withSeries (
        ColorSeriesBySelection(series, selection),
        xName = xName,
        yName = yName,
        zoomEnabled = zoomEnabled.getOrElse(Val(false)),
        manualBounds = manualBounds,
        xAxis = xAxis,
        yAxis = yAxis,
        selectEnabled = selectEnabled.getOrElse(Val(false)),
        selection = selection,
        grabEnabled = grabEnabled.getOrElse(Val(false)),
        seriesTooltipsEnabled = seriesTooltipsEnabled.getOrElse(Val(false)),
        axisTooltipsEnabled = axisTooltipsEnabled.getOrElse(Val(false))
       // extraOverLayers = List(xThreshold, yThreshold)
      )
    )

    val v = makeView(graph)

    //Zoom out by clearing manual bounds to None
    val zoomOutButton = if(zoom) Some(SwingBarButton(SwingOp("", Some(GraphSwingView.zoomOut), SetOp(manualBounds, None:Option[Area])))) else None

    val zoomEnabledView = zoomEnabled.map(BooleanView(_, "", BooleanControlType.TOOLBARBUTTON, Some(GraphSwingView.zoomSelect), false))

    val selectEnabledView = selectEnabled.map(BooleanView(_, "", BooleanControlType.TOOLBARBUTTON, Some(GraphSwingView.boxSelect), false))

    val grabEnabledView = grabEnabled.map(BooleanView(_, "", BooleanControlType.TOOLBARBUTTON, Some(GraphSwingView.move), false))

    val properties = List(("Axis Tooltips", axisTooltipsEnabled), ("Series Tooltips", seriesTooltipsEnabled)).collect{case (k,Some(v)) => (k, v)}
    
    val graphProperties = if (properties.isEmpty) None else {
      Some(properties.foldLeft(SheetBuilder().blankTop()){case (b, (s, v)) => b.view(s, BooleanView(v))}.panel())
    } 

    val settingsPopup = graphProperties.map(panel => BoxesPopupView(icon = Some(SwingView.wrench), popupContents = panel))

    val buttons = SwingButtonBar()
                    .addSwingView(selectEnabledView)
                    .addSwingView(grabEnabledView)
                    .addSwingView(zoomEnabledView)
                    .addComponent(zoomOutButton)
                    .addSwingView(settingsPopup)
                  .buildWithListStyleComponent(EmbossedLabelView(graphName).component())

    val panel = new JPanel(new BorderLayout())
    panel.add(v.component, BorderLayout.CENTER)

    panel.add(buttons, BorderLayout.SOUTH)

    panel
  }
}


class GraphSwingBGView(graph:Ref[_ <: Graph]) extends SwingView {

  val componentSize = Var(Vec2(10, 10))

  val overBuffer = new GraphBuffer()

  val concBuffer = new GraphBuffer()
  val concBackBuffer = new GraphBuffer()

  val busyAlpha = Var(0d)
  val busyLayer = new GraphBusy(busyAlpha)

  val component = new JPanel() {

    override def paintComponent(gr:Graphics) {
      concBuffer.lock.synchronized{
        gr.drawImage(concBuffer.image, 0, 0, null)
      }
      overBuffer.lock.synchronized{
        gr.drawImage(overBuffer.image, 0, 0, null)
      }
    }

    def updateSize() {
      componentSize() = Vec2(this.getWidth, this.getHeight)
    }

    this.addComponentListener(new ComponentListener(){
      override def componentResized(e:ComponentEvent) {
        updateSize()
      }
      override def componentMoved(e:ComponentEvent) {}
      override def componentShown(e:ComponentEvent) {}
      override def componentHidden(e:ComponentEvent) {}
    })

    def fireMouse(e:MouseEvent, eventType:GraphMouseEventType) {
      val s = buildSpaces
      val p = e.getPoint
      val b = e.getButton match {
        case MouseEvent.BUTTON1 => LEFT
        case MouseEvent.BUTTON2 => MIDDLE
        case MouseEvent.BUTTON3 => RIGHT
        case _ => NONE
      }
      var x = p.x
      var y = p.y
      val w = getWidth
      val h = getHeight
      if (x < 0) {
        x = 0
      } else if (x > w) {
        x = w
      }
      if (y < 0) {
        y = 0
      } else if (y > h) {
        y = h
      }

      val dataPoint = s.toData(Vec2(x, y))
      val gme = GraphMouseEvent(s, dataPoint, eventType, b)
      val consumedGME = GraphMouseEvent(s, dataPoint, CONSUMED, b)

      val consumed = graph().overlayers().foldLeft(false)((consumed, layer) => if(!consumed) layer.onMouse(gme) else {layer.onMouse(consumedGME); true})
      graph().layers().foldLeft(consumed)((consumed, layer) => if(!consumed) layer.onMouse(gme) else {layer.onMouse(consumedGME); true})
    }

    this.addMouseMotionListener(new MouseMotionListener() {
      def mouseDragged(e: MouseEvent) {
        fireMouse(e, DRAG)
      }
      def mouseMoved(e: MouseEvent) {
        fireMouse(e, MOVE)
      }
    })

    this.addMouseListener(new MouseListener(){
      def mouseClicked(e: MouseEvent) {
        fireMouse(e, CLICK)
      }
      def mousePressed(e: MouseEvent) {
        fireMouse(e, PRESS)
      }
      def mouseReleased(e: MouseEvent) {
        fireMouse(e, RELEASE)
      }
      def mouseEntered(e: MouseEvent) {
        fireMouse(e, ENTER)
      }
      def mouseExited(e: MouseEvent) {
        fireMouse(e, EXIT)
      }
    })
  }

  val bg = BackgroundReaction{

    val paints = graph().layers().map(layer => layer.paint())
    val spaces = buildSpaces

    (cancel:AtomicBoolean) => {
      Box.withoutReading {
        //TODO Would be nice to fade the busy icon in after a short delay, to avoid flickering the icon, or flashing it
        //for trivial redraws, then (perhaps) fade out later. This could then allow for an animated progress indicator.
        //Requires a Timer or similar, so a little fiddly
        busyAlpha() = 1
        concBackBuffer.ensureSize(spaces.componentArea.size)
        val g = concBackBuffer.image.getGraphics.asInstanceOf[Graphics2D]
        concBackBuffer.clear()
        //TODO progress indicator, would need to pass a Var[Double] to each paint method, then estimate proportion
        //of total time taken by each layer, or perhaps have a two-level progress indicator to show layers complete out
        //of total, and progress on current layer, might look quite cool.
        paints.foreach(paint => paint.apply(new GraphCanvasFromGraphics2D(g.create().asInstanceOf[Graphics2D], spaces)))
        g.dispose

        concBuffer.lock.synchronized{
          concBuffer.ensureSize(spaces.componentArea.size)
          concBuffer.clear()
          concBuffer.image.getGraphics.asInstanceOf[Graphics2D].drawImage(concBackBuffer.image, 0, 0, null)
        }
        busyAlpha() = 0

        replaceUpdate{
          component.repaint();
        }
      }

    }
  }

  val overView = View {
    drawBuffer(overBuffer, graph().overlayers() ::: List(busyLayer))
    replaceUpdate {
      component.repaint()
    }
  }

  def buildSpaces = {
    val size = componentSize()
    val area = graph().dataArea()
    val borders = graph().borders()

    val w = size.x.asInstanceOf[Int]
    val h = size.y.asInstanceOf[Int]

    val l = borders.left.asInstanceOf[Int]
    val r = borders.right.asInstanceOf[Int]
    val t = borders.top.asInstanceOf[Int]
    val b = borders.bottom.asInstanceOf[Int]
    val dw = w - l - r
    val dh = h - t - b

    GraphSpaces(area, Area(Vec2(l, t+dh), Vec2(dw, -dh)), Area(Vec2.zero, size))
  }

  def drawBuffer(buffer:GraphBuffer, layers:List[GraphLayer]) {
    buffer.lock.synchronized{
      val spaces = buildSpaces

      buffer.ensureSize(spaces.componentArea.size)
      buffer.clear()

      val g = buffer.image.getGraphics.asInstanceOf[Graphics2D]

      //Each layer paints on a fresh canvas, to avoid side effects from one affecting the next
      layers.foreach(layer => {
        val paint = layer.paint()
        Box.withoutReading {
          paint.apply(new GraphCanvasFromGraphics2D(g.create().asInstanceOf[Graphics2D], spaces))
        }
      })

      g.dispose
    }
  }
}


