package boxes.swing

import javax.swing.plaf.basic.BasicTextFieldUI
import javax.swing.text.JTextComponent
import com.explodingpixels.macwidgets.plaf.HudPaintingUtils
import com.explodingpixels.painter.Painter
import javax.swing.{AbstractButton, ImageIcon, BorderFactory, JComponent}
import com.explodingpixels.widgets.ImageUtils
import java.awt.{Insets, Component, Image, Graphics2D, RenderingHints, Graphics, Color}
import javax.swing.border.{EmptyBorder, AbstractBorder}
import boxes.SwingView


object BarStylePainter {
  val dividerColor = new Color(0, 0, 0, 51)
  val pressedColor = new Color(0, 0, 0, 25)
  val dividerBright = new Color(1f, 1f, 1f, 0.4f)
  val topColor = new Color(0xaaaaaa)
  val image = new ImageIcon(classOf[SwingOpAction].getResource("/boxes/swing/ListButton.png")).getImage

  def apply[T](paintLeft:Boolean = false, paintRight:Boolean = true) = new BarStylePainter[T](paintLeft, paintRight)
}

class BarStylePainter[T](paintLeft:Boolean = false, paintRight:Boolean = true) extends Painter[T] {
  override def paint(g:Graphics2D, t:T, w:Int, h:Int) {
    g.drawImage(BarStylePainter.image, 0, 0, w, h, null)

    g.setColor(BarStylePainter.dividerColor)
    if (paintLeft) {
      g.drawLine(0, 0, 0, h-1)
    }
    if (paintRight) {
      g.drawLine(w-1, 0, w-1, h-1)
    }

    g.setColor(BarStylePainter.dividerBright)
    if (paintLeft) {
      g.drawLine(1, 0, 1, h-1)
    } else {
      g.drawLine(0, 0, 0, h-1)
    }
    if (paintRight) {
      g.drawLine(w-2, 0, w-2, h-1)
    } else {
      g.drawLine(w-1, 0, w-1, h-1)
    }

    g.setColor(BarStylePainter.topColor)
    g.drawLine(0, 0, w-1, 0)
  }
}

class BarStyleButtonPainter(paintLeft:Boolean = false, paintRight:Boolean = true) extends BarStylePainter[AbstractButton] {
  override def paint(g:Graphics2D, t:AbstractButton, w:Int, h:Int) {
    super.paint(g, t, w, h)
    if (t.getModel.isPressed) {
      g.setColor(BarStylePainter.pressedColor)
      g.fillRect(0, 0, w, h)
    }
  }
}

class BarStyleToggleButtonPainter(paintLeft:Boolean = false, paintRight:Boolean = true) extends BarStylePainter[AbstractButton] {
  override def paint(g:Graphics2D, t:AbstractButton, w:Int, h:Int) {
    super.paint(g, t, w, h)
    if (t.getModel.isSelected || t.getModel.isPressed) {
      g.setColor(BarStylePainter.pressedColor)
      g.fillRect(0, 0, w, h)
    }
  }
}

//Draw a horizontal component made up of a left, middle and right portion. Portions are
//taken from the thirds of an image, and middle is stretched horizontally to fit.
class ThreePartPainter(image:Image) {
  val pieceWidth = image.getWidth(null)/3
  val pieceHeight = image.getHeight(null)
  val parts = (
    ImageUtils.getSubImage(image, 0, 0, pieceWidth, pieceHeight),
    ImageUtils.getSubImage(image, pieceWidth, 0, pieceWidth, pieceHeight),
    ImageUtils.getSubImage(image, pieceWidth * 2, 0, pieceWidth, pieceHeight)
  )

  def paint(g:Graphics2D, w:Int, h:Int) {
    val middle = w - pieceWidth * 2
    g.drawImage(parts._1, 0, 0, pieceWidth, pieceHeight, null)
    g.drawImage(parts._3, w - pieceWidth, 0, pieceWidth, pieceHeight, null)
    if (middle > 0) {
      g.drawImage(parts._2, pieceWidth, 0, middle, pieceHeight, null)
    }
  }

  //Paint only left and middle parts
  def paintWithoutRight(g:Graphics2D, w:Int, h:Int) {
    val middle = w - pieceWidth
    g.drawImage(parts._1, 0, 0, pieceWidth, pieceHeight, null)
    if (middle > 0) {
      g.drawImage(parts._2, pieceWidth, 0, middle, pieceHeight, null)
    }
  }
}

//Draw a horizontal component made up of 9 portions (corners, edges, middle). Portions are
//taken from an image, which is assumed to be a multiple of 3 in width, with custom sizes
//in the vertical axis. Edges and middle are stretched to fit, etc.
class ThreePartVariableHeightPainter(image:Image, t:Int = 10, m:Int = 8, b:Int = 10) {
  val pieceWidth = image.getWidth(null)/3
  val pieceHeight = image.getHeight(null)

  def strip(y:Int, h:Int) = (
    ImageUtils.getSubImage(image, 0, y, pieceWidth, h),
    ImageUtils.getSubImage(image, pieceWidth, y, pieceWidth, h),
    ImageUtils.getSubImage(image, pieceWidth * 2, y, pieceWidth, h)
  )

  def topStrip = strip(0, t)
  def middleStrip = strip(t, m)
  def bottomStrip = strip(t + m, b)

  def paintStrip(g:Graphics2D, w:Int, h:Int, y:Int, stripHeight:Int, parts:(Image, Image, Image)) {
    val middle = w - pieceWidth * 2
    g.drawImage(parts._1, 0, y, pieceWidth, stripHeight, null)
    g.drawImage(parts._3, w - pieceWidth, y, pieceWidth, stripHeight, null)
    if (middle > 0) {
      g.drawImage(parts._2, pieceWidth, y, middle, stripHeight, null)
    }
  }

  def paint(g:Graphics2D, w:Int, h:Int) {
    val middle = h - t - b
    paintStrip(g, w, h, 0, t, topStrip)
    paintStrip(g, w, h, h - b, b, bottomStrip)
    if (middle > 0) {
      paintStrip(g, w, h, t, middle, middleStrip)
    }
  }
}

object ButtonPainter {
  def threePartPainter(resource:String) = new ThreePartPainter(new ImageIcon(classOf[ButtonPainter].getResource(resource)).getImage)
  val plain = threePartPainter("/boxes/swing/StandaloneButton.png")
  val disabled = threePartPainter("/boxes/swing/StandaloneButtonDisabled.png")
  val focus = threePartPainter("/boxes/swing/StandaloneButtonFocusOverlay.png")
  val pressed = threePartPainter("/boxes/swing/StandaloneButtonPressed.png")
}

class ButtonPainter() extends Painter[AbstractButton] {
  override def paint(g:Graphics2D, b:AbstractButton, w:Int, h:Int) {
    if (b.getModel.isSelected || b.getModel.isPressed) {
      ButtonPainter.pressed.paint(g, w, h)
    } else if (b.getModel.isEnabled) {
      ButtonPainter.plain.paint(g, w, h)
    } else {
      ButtonPainter.disabled.paint(g, w, h)
    }

    if (b.hasFocus) {
      ButtonPainter.focus.paint(g, w, h)
    }
  }
}

object TextComponentPainter {
  val instance = new TextComponentPainter()
  def painter(resource:String) = new ThreePartVariableHeightPainter(new ImageIcon(classOf[ButtonPainter].getResource(resource)).getImage)
  val plain = painter("/boxes/swing/Text.png")
  val disabled = painter("/boxes/swing/TextDisabled.png")
  val focus = painter("/boxes/swing/TextFocusOverlay.png")
}

class TextComponentPainter() extends Painter[Component] {
  override def paint(g:Graphics2D, t:Component, w:Int, h:Int) {

    if (t.isEnabled) {
      TextComponentPainter.plain.paint(g, w, h)
    } else {
      TextComponentPainter.disabled.paint(g, w, h)
    }

    if (t.hasFocus) {
      TextComponentPainter.focus.paint(g, w, h)
    }
  }
}

class TextComponentBorder() extends AbstractBorder {
  override def paintBorder(c:Component, g:Graphics, x:Int, y:Int, width:Int, height:Int) {
    TextComponentPainter.instance.paint(g.asInstanceOf[Graphics2D], c, width, height)
  }

  override def getBorderInsets(c: Component) = new Insets(2, 2, 2, 2)

  override def getBorderInsets(c: Component, insets: Insets): Insets = {
    insets.left = 2
    insets.top = 2
    insets.right = 2
    insets.bottom = 2
    insets
  }

  override def isBorderOpaque = false
}

object BoxesTextFieldUI {
  val ui = new BoxesTextFieldUI()
  def apply(c:JComponent) = ui.installUI(c)
}

class BoxesTextFieldUI extends BasicTextFieldUI {

    override def installUI(c:JComponent) {
        super.installUI(c)

        val textComponent = c.asInstanceOf[JTextComponent]

        textComponent.setOpaque(false)
        textComponent.setBorder(new EmptyBorder(7, 8, 6, 8))
        textComponent.setBackground(new Color(0, 0, 0, 0))
        textComponent.setForeground(SwingView.textColor)
//        textComponent.setFont(HudPaintingUtils.getHudFont)
        textComponent.setSelectedTextColor(SwingView.selectedTextColor)
        textComponent.setSelectionColor(SwingView.selectionColor)
        textComponent.setCaretColor(SwingView.textColor)

    }

    override def paintSafely(graphics:Graphics) {
        val g2d = graphics.asInstanceOf[Graphics2D]
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

        super.paintSafely(graphics)
    }

}
