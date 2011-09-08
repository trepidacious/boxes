package boxes.swing

import javax.swing.plaf.basic.BasicSliderUI
import javax.swing.{JComponent, ImageIcon, JSlider}
import boxes.SwingView
import java.awt.{Graphics2D, Graphics, Dimension}

object BoxesSliderUI {
  val trackImage = new ImageIcon(classOf[BoxesSliderUI].getResource("/boxes/swing/Slider.png")).getImage
  val trackHeight = trackImage.getHeight(null)
  val trackPainter = new ThreePartPainter(trackImage)
  val handleImage = new ImageIcon(classOf[BoxesSliderUI].getResource("/boxes/swing/SliderHandle.png")).getImage
  val handleSelectImage = new ImageIcon(classOf[BoxesSliderUI].getResource("/boxes/swing/SliderHandleSelect.png")).getImage
}

class BoxesSliderUI(b:JSlider) extends BasicSliderUI(b) {

  override def installDefaults(slider:JSlider) {
    super.installDefaults(slider)
    slider.setOpaque(false)
  }

  override val getThumbSize = new Dimension(BoxesSliderUI.handleImage.getWidth(null), BoxesSliderUI.handleImage.getHeight(null))

  override def paint(g:Graphics, c:JComponent) {
    SwingView.graphicsForEnabledState(g.asInstanceOf[Graphics2D], c.isEnabled)
    super.paint(g, c);
  }

  override def paintThumb(g:Graphics) {
    g.drawImage(BoxesSliderUI.handleImage, thumbRect.x, thumbRect.y, null)
    if (slider.hasFocus) g.drawImage(BoxesSliderUI.handleSelectImage, thumbRect.x, thumbRect.y, null)
  }

  override def paintFocus(g:Graphics) {
  }

  override def paintTrack(g:Graphics) {
    val y = (slider.getHeight - BoxesSliderUI.trackHeight)/2

    val g2d = g.asInstanceOf[Graphics2D]
    val oldTrans = g2d.getTransform
    g2d.translate(0, y)
    BoxesSliderUI.trackPainter.paint(g2d, slider.getWidth, BoxesSliderUI.trackHeight)
    g2d.setTransform(oldTrans)
  }

  override def getTickLength = 5

//    override def calculateThumbLocation() {
//      super.calculateThumbLocation();
//    }
//    override def calculateTickRect() {
//      super.calculateTickRect();
//    }
//    override def paintMajorTickForHorizSlider(g:Graphics, tickBounds:Rectangle, x:Int) {
//        g.setColor(Color.WHITE)
//        super.paintMajorTickForHorizSlider(g, tickBounds, x)
//    }

  override def setThumbLocation(x:Int, y:Int) {
      super.setThumbLocation(x, y)
      slider.repaint()
  }

}
