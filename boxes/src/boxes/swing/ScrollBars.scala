package boxes.swing

import com.explodingpixels.painter.ImagePainter
import com.explodingpixels.painter.Painter
import com.explodingpixels.swingx.EPPanel
import com.explodingpixels.widgets.plaf.SkinnableScrollBarUI.ScrollBarSkinProvider
import com.explodingpixels.macwidgets.IAppWidgetFactory
import javax.swing._
import java.awt.event.MouseListener
import com.explodingpixels.widgets.plaf.{ScrollThumbImagePainter, ScrollBarOrientation, ScrollBarSkin, SkinnableScrollBarUI}
import com.explodingpixels.macwidgets.plaf.{ArtworkUtils, IAppScrollBarArtworkUtils}
import com.explodingpixels.widgets.{ImageUtils, ImageBasedJComponent}
import java.awt.{Graphics2D, Component, Dimension, Image, Rectangle, Point, Color}
import boxes.swing.icons.IconFactory

class BoxesScrollBarUI(dotModel:DotModel, plain:Boolean = false) extends SkinnableScrollBarUI(BoxesScrollBarUI.createScrollBarSkinProvider(dotModel, plain))

class WhitePainter extends Painter[Component] {
  def paint(g:Graphics2D, t:Component, w:Int, h:Int) {
    g.setColor(Color.white)
    g.fillRect(0, 0, w, h)
  }
}

object BoxesScrollBarUI {

  def horizontalScrollBarPieces(image:Image) = {
    (ImageUtils.getSubImage(image, 0, 0, 8, 13),
    ImageUtils.getSubImage(image, 8, 0, 8, 13),
    ImageUtils.getSubImage(image, 16, 0, 8, 13))
  }

  def verticalScrollBarPieces(image:Image) = {
    (ImageUtils.getSubImage(image, 0, 0, 13, 8),
    ImageUtils.getSubImage(image, 0, 8, 13, 8),
    ImageUtils.getSubImage(image, 0, 16, 13, 8))
  }

  def createHorizontalScrollerThumb():ScrollThumbImagePainter = {
    val images = ArtworkUtils.getImageSet(classOf[BoxesScrollBarUI].getResource("/boxes/swing/icons/HorizontalThumb.png"))

    val disabled = horizontalScrollBarPieces(images.getInactiveImage.getImage)
    val inactive = horizontalScrollBarPieces(images.getActiveImage.getImage)
    val active = horizontalScrollBarPieces(images.getPressedImage.getImage)

    ScrollThumbImagePainter.createHorizontalScrollThumbImagePainter(
            disabled._1, disabled._2, disabled._3,
            inactive._1, inactive._2, inactive._3,
            active._1, active._2, active._3)
  }

  def createVerticalScrollerThumb():ScrollThumbImagePainter = {
    val images = ArtworkUtils.getImageSet(classOf[BoxesScrollBarUI].getResource("/boxes/swing/icons/VerticalThumb.png"))

    val disabled = verticalScrollBarPieces(images.getInactiveImage.getImage)
    val inactive = verticalScrollBarPieces(images.getActiveImage.getImage)
    val active = verticalScrollBarPieces(images.getPressedImage.getImage)

    ScrollThumbImagePainter.createVerticalScrollThumbImagePainter(
            disabled._1, disabled._2, disabled._3,
            inactive._1, inactive._2, inactive._3,
            active._1, active._2, active._3)
  }


  def defaultThickness = 15

  def applyTo(scrollPane:JScrollPane, horizontalDotModel:DotModel = new DotModel(), verticalDotModel:DotModel = new DotModel(), horizontal:Boolean = true, vertical:Boolean = true, plain:Boolean = false) = {
    scrollPane.setBorder(BorderFactory.createEmptyBorder())
    scrollPane.getVerticalScrollBar().setUI(new BoxesScrollBarUI(verticalDotModel, plain))
    scrollPane.getHorizontalScrollBar().setUI(new BoxesScrollBarUI(horizontalDotModel, plain))
    scrollPane.setCorner(ScrollPaneConstants.UPPER_RIGHT_CORNER, topRightCorner)
    scrollPane.setCorner(ScrollPaneConstants.LOWER_RIGHT_CORNER, corner)
    scrollPane.setVerticalScrollBarPolicy(if (vertical) ScrollPaneConstants.VERTICAL_SCROLLBAR_ALWAYS else ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER)
    scrollPane.setHorizontalScrollBarPolicy(if (horizontal) ScrollPaneConstants.HORIZONTAL_SCROLLBAR_ALWAYS else ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER)
    horizontalDotModel.scrollBar = Some(scrollPane.getHorizontalScrollBar)
    verticalDotModel.scrollBar = Some(scrollPane.getVerticalScrollBar)
    scrollPane
  }

  def corner()= new ImageBasedJComponent(new ImageIcon(classOf[IAppWidgetFactory].getResource(
              "/com/explodingpixels/macwidgets/images/iapp_scrollpane_corner.png")).getImage)

  def topRightCorner() = {
    val p = new EPPanel()
    p.setBackgroundPainter(BoxesTableCellHeaderRenderer.cornerBGPainter)
    p
  }

  def createScrollBarSkinProvider(dotModel:DotModel, plain:Boolean):ScrollBarSkinProvider = {
    new ScrollBarSkinProvider() {
      override def provideSkin(orientation:ScrollBarOrientation) = {
        if (orientation == ScrollBarOrientation.HORIZONTAL) {
          horizontalSkin(dotModel, plain)
        } else {
          verticalSkin(dotModel, plain)
        }
      }
    }
  }

  def horizontalSkin(dotModel:DotModel, plain:Boolean):ScrollBarSkin = {
    val minimumThumbSize = IAppScrollBarArtworkUtils.getHorizontalScrollBarMinimumSize()
    val trackPainter = if(plain) new WhitePainter() else new ImagePainter(IconFactory.image("HorizontalTrack"))
    val scrollerThumb = createHorizontalScrollerThumb
    val preferredSize = new Dimension(100, defaultThickness)

    new BoxesScrollBarSkin(trackPainter, scrollerThumb, minimumThumbSize, preferredSize, dotModel)
  }

  def verticalSkin(dotModel:DotModel, plain:Boolean):ScrollBarSkin = {
    val minimumThumbSize = IAppScrollBarArtworkUtils.getVerticalScrollBarMinimumSize()
    val trackPainter = if (plain) new WhitePainter() else new ImagePainter(IconFactory.image("VerticalTrack"))
    val scrollerThumb = createVerticalScrollerThumb()
    val preferredSize = new Dimension(defaultThickness, 100)

    new BoxesScrollBarSkin(trackPainter, scrollerThumb, minimumThumbSize, preferredSize, dotModel)
  }
}

class DotModel {
  private var _positions = Set[(Double, Double)]()

  def positions = _positions

  def positions_=(newPositions:Set[(Double, Double)]) = {
    _positions = newPositions
    scrollBar.foreach(sb => sb.repaint())
  }

  private[swing] var scrollBar:Option[JScrollBar] = None
}

class DotPainter(val model:DotModel) extends Painter[Component] {

  val dot = IconFactory.image("Dot")
  val dotCenter = IconFactory.image("DotCenter")
  val dotColour = new Color(222, 222, 222)
  val dotCenterColour = new Color(148, 167, 178)

  override def paint(g:Graphics2D, t:Component, w:Int, h:Int) {
    model.positions.foreach(p => draw(g, dot, dotColour, p, h, 6, 4))
    model.positions.foreach(p => draw(g, dotCenter, dotCenterColour, p, h, 7, 2))
  }

  private def draw(g:Graphics2D, image:Image, color:Color, p:(Double, Double), h:Int, x:Int, w:Int) {
    val startOffset = 1
    val endOffset = 11
    val range = h - startOffset - endOffset

    val startY = (p._1*range).asInstanceOf[Int] + startOffset + 2
    val endY = (p._2*range).asInstanceOf[Int] + startOffset - 1

    g.drawImage(image, 3, startY, null)
    if (endY - startY > 1) {
      g.drawImage(image, 3, endY, null)
      g.setColor(color)
      g.fillRect(x, startY + 5, w, endY - startY - 1)
    }
  }
}

class BoxesScrollBarSkin(
  trackPainter:Painter[Component],
  scrollThumbPainter:Painter[Component],
  val minimumThumbSize:Dimension,
  val preferredSize:Dimension,
  val dotModel:DotModel) extends ScrollBarSkin {

  val thumbContainer = new JPanel()
  thumbContainer.setLayout(null)
  thumbContainer.setOpaque(false)

  val thumb = new EPPanel()
  thumb.setBackgroundPainter(scrollThumbPainter);
  thumb.setOpaque(false);

  val track = new EPPanel()
  track.setBackgroundPainter(trackPainter);

  val dots = new DotPainter(dotModel)

  val dotPanel = new EPPanel()
  dotPanel.setBackgroundPainter(dots)

  val emptyBounds = new Rectangle(0, 0, 0, 0)

  override def getMinimumThumbSize() = minimumThumbSize
  override def getPreferredSize() = preferredSize
  override def getScrollThumbBounds() = thumb.getBounds()
  override def getTrackBounds() = thumbContainer.getBounds()

  override def installComponents(scrollBar:JScrollBar) {
    // add the components to the scrollbar. order matters here - components added first, are
    // drawn last (on top).
    scrollBar.add(dotPanel)
    scrollBar.add(thumbContainer)
    scrollBar.add(track)

    // add the actual scroller thumb (the component that will be painted) to the scroller thumb
    // container.
    thumbContainer.add(thumb)
  }

  override def layoutTrackOnly(scrollBar:JScrollBar, orientation:ScrollBarOrientation) {
      thumbContainer.setBounds(emptyBounds)

      val r = scrollBar.getBounds()
      track.setBounds(0, 0, r.width, r.height)
      dotPanel.setBounds(0, 0, r.width, r.height)
  }

  override def layoutEverything(scrollBar:JScrollBar, orientation:ScrollBarOrientation) {
    val r = scrollBar.getBounds()
    track.setBounds(0, 0, r.width, r.height)
    dotPanel.setBounds(0, 0, r.width, r.height)
    thumbContainer.setBounds(0, 0, r.width, r.height)
  }

  override def installMouseListenersOnButtons(d:MouseListener, i:MouseListener) {}

  override def setScrollThumbBounds(bounds:Rectangle) {
      thumb.setBounds(bounds)
  }
}
