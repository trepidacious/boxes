package boxes.swing

import javax.swing.text.JTextComponent
import com.explodingpixels.macwidgets.plaf.HudPaintingUtils
import com.explodingpixels.painter.Painter
import com.explodingpixels.widgets.ImageUtils
import java.awt.{Insets, Component, Image, Graphics2D, RenderingHints, Graphics, Color}
import javax.swing.border.{EmptyBorder, AbstractBorder}
import boxes.SwingView
import javax.swing.{JTextArea, JTextField, AbstractButton, ImageIcon, BorderFactory, JComponent}
import javax.swing.plaf.basic.{BasicFormattedTextFieldUI, BasicTextAreaUI, BasicTextFieldUI}
import java.beans.PropertyChangeEvent

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

  def paintStripWithoutRight(g:Graphics2D, w:Int, h:Int, y:Int, stripHeight:Int, parts:(Image, Image, Image)) {
    val middle = w - pieceWidth
    g.drawImage(parts._1, 0, y, pieceWidth, stripHeight, null)
    if (middle > 0) {
      g.drawImage(parts._2, pieceWidth, y, middle, stripHeight, null)
    }
  }

  def paintWithoutRight(g:Graphics2D, w:Int, h:Int) {
    val middle = h - t - b
    paintStripWithoutRight(g, w, h, 0, t, topStrip)
    paintStripWithoutRight(g, w, h, h - b, b, bottomStrip)
    if (middle > 0) {
      paintStripWithoutRight(g, w, h, t, middle, middleStrip)
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

object SpinnerTextComponentPainter {
  val instance = new SpinnerTextComponentPainter()
}

class SpinnerTextComponentPainter() extends Painter[Component] {
  override def paint(g:Graphics2D, t:Component, w:Int, h:Int) {

    if (t.isEnabled) {
      TextComponentPainter.plain.paintWithoutRight(g, w, h)
    } else {
      TextComponentPainter.disabled.paintWithoutRight(g, w, h)
    }

    if (t.hasFocus) {
      TextComponentPainter.focus.paintWithoutRight(g, w, h)
    }
  }
}

object BoxesTextComponentUI {
  def adjustComponent(c:JTextComponent) {
    c.setOpaque(false)
    c.setBorder(new EmptyBorder(7, 8, 6, 8))
    c.setBackground(new Color(0, 0, 0, 0))
    c.setForeground(SwingView.textColor)
//      c.setFont(HudPaintingUtils.getHudFont)
    c.setSelectedTextColor(SwingView.selectedTextColor)
    c.setSelectionColor(SwingView.selectionColor)
    c.setCaretColor(SwingView.textColor)
  }
}

object BoxesTextFieldUI {
  def apply(c:JTextField) = c.setUI(new BoxesTextFieldUI())
}
class BoxesTextFieldUI extends BasicTextFieldUI {
  override def installUI(c:JComponent) {
    super.installUI(c)
    BoxesTextComponentUI.adjustComponent(c.asInstanceOf[JTextComponent])
  }

  override def paintSafely(g:Graphics) {
    val c = getComponent
    TextComponentPainter.instance.paint(g.asInstanceOf[Graphics2D], c, c.getWidth, c.getHeight)
    super.paintSafely(g)
  }
}

object BoxesTextAreaUI {
  def apply(c:JTextArea) = c.setUI(new BoxesTextAreaUI())
}
class BoxesTextAreaUI extends BasicTextAreaUI {
  override def installUI(c:JComponent) {
    super.installUI(c)
    BoxesTextComponentUI.adjustComponent(c.asInstanceOf[JTextComponent])
  }

  override def paintSafely(g:Graphics) {
    val c = getComponent
    TextComponentPainter.instance.paint(g.asInstanceOf[Graphics2D], c, c.getWidth, c.getHeight)
    super.paintSafely(g)
  }
}

object SpinnerTextFieldUI {
  def apply(c:JTextField) = c.setUI(new SpinnerTextFieldUI())
}
class SpinnerTextFieldUI extends BasicFormattedTextFieldUI {
  override def installUI(c:JComponent) {
    super.installUI(c)
    BoxesTextComponentUI.adjustComponent(c.asInstanceOf[JTextComponent])
  }

  override def paintSafely(g:Graphics) {
    val c = getComponent
    SpinnerTextComponentPainter.instance.paint(g.asInstanceOf[Graphics2D], c, c.getWidth, c.getHeight)
    super.paintSafely(g)
  }

}

