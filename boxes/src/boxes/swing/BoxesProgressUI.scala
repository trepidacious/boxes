package boxes.swing

import javax.swing.plaf.basic.BasicProgressBarUI
import javax.swing.{ImageIcon, JComponent}
import sun.swing.SwingUtilities2
import java.awt.{Color, Rectangle, Graphics, Graphics2D}
import boxes.SwingView
import boxes.swing.icons.IconFactory

object BoxesProgressUI {
  val barPainter = new ThreePartPainter(IconFactory.image("Progress"))
}

class BoxesProgressUI extends BasicProgressBarUI {

  override def paintDeterminate(g: Graphics, c: JComponent) {

    val w = progressBar.getWidth
    val barTotalWidth = w - 8
    val h = progressBar.getHeight
    //Note "percent" complete isn't percent, it is from 0 to 1
    val barFilledWidth = (barTotalWidth * progressBar.getPercentComplete).asInstanceOf[Int]

    val fillEndX = 4 + barFilledWidth - 1

    val g2 = g.asInstanceOf[Graphics2D]

    TextComponentPainter.instance.paint(g2, c, w, h)

    g.translate(4, 0)
    BoxesProgressUI.barPainter.paint(g2, barFilledWidth, h)
    g.translate(-4, 0)

    paintString(g, w, h, fillEndX)

  }

  private def paintString(g: Graphics, width: Int, height: Int, fillEndX:Int): Unit = {
    val g2 = g.asInstanceOf[Graphics2D]
    val s = progressBar.getString
    g2.setFont(progressBar.getFont)

    val renderLocation = getStringPlacement(g2, s, 0, 0, width, height)

    val oldClip: Rectangle = g2.getClipBounds

    g2.clipRect(fillEndX, 0, width - fillEndX, height)
    g2.setColor(SwingView.textColor)
    SwingUtilities2.drawString(progressBar, g2, s, renderLocation.x, renderLocation.y)

    g2.setClip(oldClip)
    g2.clipRect(0, 0, fillEndX, height)
    g2.setColor(SwingView.selectedTextColor)
    SwingUtilities2.drawString(progressBar, g2, s, renderLocation.x, renderLocation.y)

    g2.setClip(oldClip)
  }

}