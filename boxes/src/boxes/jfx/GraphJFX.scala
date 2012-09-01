package boxes.jfx

import javafx.scene.Group
import javafx.scene.Node
import javafx.scene.shape.Line
import boxes.graph.Vec2
import java.awt.Color
import boxes.graph.GraphSpaces
import boxes.graph.GraphCanvas
import javafx.scene.shape.Rectangle
import boxes.graph.Area
import java.awt.Image
import javafx.scene.shape.Path
import javafx.scene.shape.MoveTo
import javafx.scene.shape.LineTo
import javafx.scene.text.Text
import javafx.scene.text.Font
import boxes.Ref
import boxes.graph.Graph
import boxes.list.ListRef
import boxes.Var
import boxes.Val
import boxes.graph.GraphZoomerAxis
import boxes.graph.Series
import boxes.graph.Borders
import boxes.graph.GraphLayer
import javafx.scene.layout.StackPane
import javafx.beans.value.ChangeListener
import javafx.beans.value.ObservableValue
import boxes.general.RadioReaction
import boxes.graph.GraphMouseEventType
import boxes.graph.GraphMouseEvent
import boxes.graph.GraphMouseEventType._
import boxes.graph.GraphMouseButton._
import boxes.jfx.JFXImplicits._
import javafx.scene.input.MouseEvent
import javafx.scene.input.MouseButton
import boxes.View
import boxes.Box
import boxes.graph.GraphBasic
import boxes.graph.ColorSeriesBySelection
import javafx.scene.canvas.Canvas
import boxes.graph.GraphSpaces

class GraphCanvasJFX(val canvas: Canvas, val spaces:GraphSpaces) extends GraphCanvas {

  val gc = canvas.getGraphicsContext2D;

  var c = new javafx.scene.paint.Color(0.0, 0.0, 0.0, 1.0)
  var w = 1d
  var fs = 10d
  
  def color_=(color:Color) {
    c = new javafx.scene.paint.Color(color.getRed.asInstanceOf[Double]/255d, color.getGreen.asInstanceOf[Double]/255d, color.getBlue.asInstanceOf[Double]/255d, color.getAlpha.asInstanceOf[Double]/255d)
  }
  def color = new Color(c.getRed.asInstanceOf[Float], c.getGreen.asInstanceOf[Float], c.getBlue.asInstanceOf[Float], c.getOpacity.asInstanceOf[Float])

  def lineWidth_=(lineWidth:Double) {
    w = lineWidth
  }
  def lineWidth = w

  def fontSize_=(fontSize:Double) {
    fs = fontSize
  }
  def fontSize = fs

  def dataLine(a:Vec2, b:Vec2) {
    line(spaces.toPixel(a), spaces.toPixel(b))
  }

  def line(a:Vec2, b:Vec2) {
    val l = new Line(a.x, a.y, b.x, b.y)
    gc.setFill(c)
    gc.setLineWidth(w)
    gc.moveTo(a.x, a.y)
    gc.lineTo(b.x, b.y)
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
    roundRect(origin, size, fill, 0)
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

    if (fill) {
      gc.setFill(c)
      gc.fillRoundRect(df._1, df._2, df._3, df._4, radius, radius)
    } else {
      gc.setStroke(c)
      gc.setLineWidth(w)
      gc.strokeRoundRect(df._1, df._2, df._3, df._4, radius, radius)
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
  
  def image(i:Image, origin:Vec2, size:Vec2) {
    // TODO draw image - hard to convert from Image, so should we actually request a resource that can be loaded as an image,
    //and possibly cache it? Or just get rid of images, which would be neater.
    // TODO should we use dumb format, or leave potentially mirrored image? Check whether mirrored image actually draws.
  }

  def image(i:Image, origin:Vec2) {
    image(i, origin, Vec2(i.getWidth(null), i.getHeight(null)))
  }

  def path(path:List[Vec2]) {
    gc.setLineWidth(w)
    gc.setStroke(c)

    gc.beginPath
    gc.moveTo(path.head.x, path.head.y)
    path.tail.foreach(v => gc.lineTo(v.x, v.y))
    
    gc.stroke()
  }

  def dataPath(dataPath:List[Vec2]) {
    path(dataPath.map(p => spaces.toPixel(p)))
  }

  def stringSize(s:String) = {
//    val d = g.getFontMetrics.getStringBounds(s, g)
//    Vec2(d.getWidth, d.getHeight)
    
    //TODO implement
    Vec2(s.size * 8, 20);
  }

  def string(s:String, v:Vec2, align:Vec2 = Vec2.zero, rotateQuadrants:Int = 0) {

    gc.setStroke(c)
    gc.setFill(c)
    gc.setFont(new Font(fs))
    gc.fillText(s, v.x, v.y)
    
    //TODO rotation, alignment
    
//    val d = g.getFontMetrics.getStringBounds(s, g)
//    val w = d.getWidth
//    val h = d.getHeight
//
//    val x = v.x.asInstanceOf[Int]
//    val y = v.y.asInstanceOf[Int]
//
//    val oldxForm = g.getTransform
//    val t = g.getTransform
//    t.concatenate(AffineTransform.getQuadrantRotateInstance(rotateQuadrants, x, y))
//    g.setTransform(t)
//
//    val vo = v + Vec2(-w * align.x, h * align.y)
//
//    val ox = vo.x.asInstanceOf[Int]
//    val oy = vo.y.asInstanceOf[Int]
//
//    g.drawString(s, ox, oy)
//
//    g.setTransform(oldxForm)
  }
  
  def clipToRect(origin:Vec2, size:Vec2) {
//    val df = toDumbFormat(origin, size)
//    g.setClip(df._1, df._2, df._3, df._4)
    //TODO do
  }

  def clipToData() {
    clipToRect(spaces.pixelArea.origin, spaces.pixelArea.size)
  }

  def clipToAll() {
//    g.setClip(defaultClip)
    //TODO do
  }
}


object GraphJFXView {

  def apply(graph:Ref[_ <: Graph]) = new GraphJFXView(graph)

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
  ) = GraphJFX.panelWithSeries(g => GraphJFXView(g))(series, selection, xName, yName, xAxis, yAxis, graphName, zoom, select, grab, seriesTooltips, axisTooltips, borders, extraMainLayers, extraOverLayers)

}


class GraphJFXView(graph:Ref[_ <: Graph]) extends JFXView {

  val componentSize = Var(Vec2(500, 500))

  val node = new StackPane()
  val mainCanvas = new Canvas(500, 500)
  mainCanvas.setMouseTransparent(true)
  val overlayCanvas = new Canvas(500, 500)
  overlayCanvas.setMouseTransparent(true)
  node.getChildren.addAll(mainCanvas, overlayCanvas)
  
//  val updateSize = new ChangeListener[Number](){
//    def changed(observable: ObservableValue[_ <: Number] , oldValue: Number, newValue: Number) {
//      componentSize() = Vec2(node.getWidth, node.getHeight)
//    }
//  }
//  node.widthProperty.addListener(updateSize)
//  node.heightProperty.addListener(updateSize)
  
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
  
  def clip(d: Double, m: Double) = if (d < 0) 0 else if (d > m) m else d
  
  def fireMouse(e: MouseEvent, eventType: GraphMouseEventType) {
    val s = buildSpaces
    val w = node.getWidth
    val h = node.getHeight
    val x = clip(e.getX, node.getWidth)
    val y = clip(e.getY, node.getHeight)

    val dataPoint = s.toData(Vec2(x, y))
    
    val b = e.getButton match {
      case MouseButton.PRIMARY => LEFT
      case MouseButton.MIDDLE => MIDDLE
      case MouseButton.SECONDARY => RIGHT
      case _ => NONE
    }
    
    val gme = GraphMouseEvent(s, dataPoint, eventType, b)
    val consumedGME = GraphMouseEvent(s, dataPoint, CONSUMED, b)

    val consumed = graph().overlayers().foldLeft(false)((consumed, layer) => if(!consumed) layer.onMouse(gme) else {layer.onMouse(consumedGME); true})
    graph().layers().foldLeft(consumed)((consumed, layer) => if(!consumed) layer.onMouse(gme) else {layer.onMouse(consumedGME); true})

  }
  
  node.setOnMousePressed((r: MouseEvent) => fireMouse(r, PRESS))
  node.setOnMouseReleased((r: MouseEvent) => fireMouse(r, RELEASE))
  node.setOnMouseDragged((r: MouseEvent) => fireMouse(r, DRAG))
  node.setOnMouseMoved((r: MouseEvent) => fireMouse(r, MOVE))
  node.setOnMouseClicked((r: MouseEvent) => fireMouse(r, CLICK))
  node.setOnMouseEntered((r: MouseEvent) => fireMouse(r, ENTER))
  node.setOnMouseExited((r: MouseEvent) => fireMouse(r, EXIT))

  val view = View {
    val spaces = buildSpaces
    val overlayPaints = graph().overlayers().map(_.paint)
    val mainPaints = graph().layers().map(_.paint)
    replaceUpdate {
      drawLayersToCanvas(overlayPaints, overlayCanvas, spaces)
      drawLayersToCanvas(mainPaints, mainCanvas, spaces)
    }
  }

  def drawLayersToCanvas(paints:List[GraphCanvas => Unit], canvas: Canvas, spaces: GraphSpaces) = {
    canvas.getGraphicsContext2D.clearRect(0, 0, 500, 500)
    val gcjfx = new GraphCanvasJFX(canvas, spaces)
    gcjfx.color = Color.BLACK
    gcjfx.line(Vec2(0,0), Vec2(100,100))

    paints.foreach(paint => {
      Box.withoutReading {
        paint.apply(gcjfx)
      }
    })
  }
}


object GraphJFX {
  
  def panelWithSeries[K](makeView: Ref[_ <: Graph] => JFXView)(
    
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

//    //Zoom out by clearing manual bounds to None
//    val zoomOutButton = if(zoom) Some(SwingBarButton(SwingOp("", Some(GraphSwingView.zoomOut), SetOp(manualBounds, None:Option[Area])))) else None
//
//    val zoomEnabledView = zoomEnabled.map(BooleanView(_, "", BooleanControlType.TOOLBARBUTTON, Some(GraphSwingView.zoomSelect), false))
//
//    val selectEnabledView = selectEnabled.map(BooleanView(_, "", BooleanControlType.TOOLBARBUTTON, Some(GraphSwingView.boxSelect), false))
//
//    val grabEnabledView = grabEnabled.map(BooleanView(_, "", BooleanControlType.TOOLBARBUTTON, Some(GraphSwingView.move), false))
//
//    val properties = List(("Axis Tooltips", axisTooltipsEnabled), ("Series Tooltips", seriesTooltipsEnabled)).collect{case (k,Some(v)) => (k, v)}
//    
//    val graphProperties = if (properties.isEmpty) None else {
//      Some(properties.foldLeft(SheetBuilder().blankTop()){case (b, (s, v)) => b.view(s, BooleanView(v))}.panel())
//    } 
//
//    val settingsPopup = graphProperties.map(panel => BoxesPopupView(icon = Some(SwingView.wrench), popupContents = panel))
//
//    val buttons = SwingButtonBar()
//                    .addSwingView(selectEnabledView)
//                    .addSwingView(grabEnabledView)
//                    .addSwingView(zoomEnabledView)
//                    .addComponent(zoomOutButton)
//                    .addSwingView(settingsPopup)
//                  .buildWithListStyleComponent(EmbossedLabelView(graphName).component())
//
//    val panel = new JPanel(new BorderLayout())
//    panel.add(v.component, BorderLayout.CENTER)
//
//    panel.add(buttons, BorderLayout.SOUTH)
//
//    panel
    
    v.node
  }
}
