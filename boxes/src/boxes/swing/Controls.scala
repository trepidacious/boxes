package boxes.swing

import com.explodingpixels.widgets.ImageUtils
import javax.swing.border.EmptyBorder
import java.awt.event.{FocusEvent, FocusListener}
import java.beans.{PropertyChangeListener, PropertyChangeEvent}
import com.explodingpixels.painter.Painter
import boxes.{Op, SwingView}
import com.explodingpixels.swingx.{EPPanel, EPToggleButton, EPButton}
import sun.swing.SwingUtilities2
import javax.swing.text.{View, JTextComponent}
import javax.swing.plaf.basic.{BasicHTML, BasicButtonUI, BasicLabelUI, BasicGraphicsUtils, BasicCheckBoxUI, BasicFormattedTextFieldUI, BasicTextAreaUI, BasicTextFieldUI}
import java.awt.{FontMetrics, Rectangle, Dimension, Component, Image, Graphics2D, RenderingHints, Graphics, Color}
import javax.swing.{JToggleButton, ButtonModel, SwingConstants, JLabel, ImageIcon, Action, Icon, JCheckBox, JTextArea, JTextField, AbstractButton, JComponent}

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

class TabPainter(image:Image, top:Int = 4, waist:Int = 3, indicator:Int = 24, bottom:Int = 4, left:Int = 4, middle:Int = 4, right:Int = 16) {

  //Extract left, middle and right parts of a horizontal image strip
  def strip(y:Int, h:Int) = (
    ImageUtils.getSubImage(image, 0, y, left, h),
    ImageUtils.getSubImage(image, left, y, middle, h),
    ImageUtils.getSubImage(image, image.getWidth(null) - right, y, right, h)
  )

  def topStrip = strip(0, top)
  def waistStrip = strip(top, waist)
  def indicatorStrip = strip(top + waist, indicator)
  def bottomStrip = strip(image.getHeight(null) - bottom, bottom)

  def paintStrip(g:Graphics2D, w:Int, h:Int, y:Int, stripHeight:Int, parts:(Image, Image, Image)) {
    val middleStretched = w - left - right

    g.drawImage(parts._1, 0, y, left, stripHeight, null)
    g.drawImage(parts._3, w - right, y, right, stripHeight, null)
    if (middleStretched > 0) {
      g.drawImage(parts._2, left, y, middleStretched, stripHeight, null)
    }
  }

  def paint(g:Graphics2D, w:Int, h:Int, paintBottom:Boolean) {
    val bottomUsed = if (paintBottom) bottom else 0

    val waistStretched = h - top - bottomUsed

    paintStrip(g, w, h, 0, top, topStrip)

    if (paintBottom) {
      paintStrip(g, w, h, h - bottom, bottom, bottomStrip)
    }

    //TODO paint the waist in two sections, above and below indicator, so that transparent images will work
    if (waistStretched > 0) {
      paintStrip(g, w, h, top, waistStretched, waistStrip)
    }

    val indicatorY = (h - indicator)/2
    paintStrip(g, w, h, indicatorY, indicator, indicatorStrip)

  }
}

object TabSpacer {
  def apply() = new TabSpacer()
}

class TabSpacer extends EPPanel {
  {
    setBackgroundPainter(new TabSpacerPainter(false))
  }
}

//class TabButton(paintBottom:Boolean = true) extends EPToggleButton {
//  {
//    setBorder(null)
//    setContentAreaFilled(false)
//    setBackgroundPainter(new TabButtonPainter(paintBottom))
//    setPreferredSize(new Dimension(39, 38))
//    setMinimumSize(new Dimension(39, 38))
//    setForeground(SwingView.selectedTextColor)
//    setHorizontalTextPosition(SwingConstants.CENTER)
//    setVerticalTextPosition(SwingConstants.BOTTOM)
//    //TODO add borders to avoid indicator overpaint, set foreground/background etc.
//  }
//}

class TabButton(paintBottom:Boolean = true) extends JToggleButton {
  {
    setUI(new TabButtonUI())
  }
}


object TabButtonPainter {
  def painter(resource:String) = new TabPainter(new ImageIcon(classOf[SlideCheckPainter].getResource(resource)).getImage)
  val off = painter("/boxes/swing/Tab.png")
  val on = painter("/boxes/swing/TabPressed.png")
//  val focus = painter("/boxes/swing/SlideCheckFocusOverlay.png")
}

class TabButtonPainter(paintBottom:Boolean = true) extends Painter[AbstractButton] {
  override def paint(g:Graphics2D, b:AbstractButton, w:Int, h:Int) {

    val oldComposite = g.getComposite
    SwingView.graphicsForEnabledState(g, b.getModel.isEnabled)

    if (b.getModel.isSelected) {
      TabButtonPainter.on.paint(g, w, h, paintBottom)
    } else {
      TabButtonPainter.off.paint(g, w, h, paintBottom)
    }

//    if (b.hasFocus) {
//      g.drawImage(SlideCheckPainter.focus, 0, 0, null)
//    }

    g.setComposite(oldComposite)
  }
}

class TabSpacerPainter(paintBottom:Boolean = true) extends Painter[Component] {
  override def paint(g:Graphics2D, b:Component, w:Int, h:Int) {
    TabButtonPainter.off.paint(g, w, h, paintBottom)
  }
}

class TabButtonUI extends BasicButtonUI {
  override def installUI(c:JComponent) {
    super.installUI(c)
    val b = c.asInstanceOf[AbstractButton]
    b.setBorder(null)
    b.setContentAreaFilled(false)
    b.setForeground(SwingView.selectedTextColor)
    b.setHorizontalTextPosition(SwingConstants.CENTER)
    b.setVerticalTextPosition(SwingConstants.BOTTOM)
  }

  override def paint(graphics: Graphics, c: JComponent) {
    var b = c.asInstanceOf[AbstractButton]

    val g = graphics.asInstanceOf[Graphics2D]
    val w = c.getWidth
    val h = c.getHeight

    if (b.getModel.isSelected) {
      TabButtonPainter.on.paint(g, w, h, true)
    } else {
      TabButtonPainter.off.paint(g, w, h, true)
    }

    val oldComposite = g.getComposite
    SwingView.graphicsForEnabledState(g, b.getModel.isSelected)

    super.paint(graphics, c)

    g.setComposite(oldComposite)
  }

  override def paintText(g: Graphics, b: AbstractButton, textRect: Rectangle, text: String) {
    var fm = SwingUtilities2.getFontMetrics(b, g)
    var mnemonicIndex = b.getDisplayedMnemonicIndex

    //TODO get shadow color from SwingViews
    g.setColor(new Color(0,0,0,140))
    SwingUtilities2.drawStringUnderlineCharAt(b, g, text, mnemonicIndex, textRect.x, textRect.y + fm.getAscent + 1)

    g.setColor(b.getForeground)
    SwingUtilities2.drawStringUnderlineCharAt(b, g, text, mnemonicIndex, textRect.x, textRect.y + fm.getAscent)
  }

}

class SlideCheckButton extends EPToggleButton{
  {
    setBorder(null)
    setContentAreaFilled(false)
    setBackgroundPainter(new SlideCheckPainter())
    setPreferredSize(new Dimension(60, 28))
    setMinimumSize(new Dimension(60, 28))
    setMaximumSize(new Dimension(60, 28))
  }
}

object SlideCheckPainter {
  def painter(resource:String) = new ImageIcon(classOf[SlideCheckPainter].getResource(resource)).getImage
  val off = painter("/boxes/swing/SlideCheckOff.png")
  val on = painter("/boxes/swing/SlideCheckOn.png")
  val focus = painter("/boxes/swing/SlideCheckFocusOverlay.png")
}

class SlideCheckPainter() extends Painter[AbstractButton] {
  override def paint(g:Graphics2D, b:AbstractButton, w:Int, h:Int) {

    val oldComposite = g.getComposite
    SwingView.graphicsForEnabledState(g, b.getModel.isEnabled)

    if (b.getModel.isSelected) {
      g.drawImage(SlideCheckPainter.on, 0, 0, null)
    } else {
      g.drawImage(SlideCheckPainter.off, 0, 0, null)
    }

    if (b.hasFocus) {
      g.drawImage(SlideCheckPainter.focus, 0, 0, null)
    }

    g.setComposite(oldComposite)
  }
}


object ButtonPainter {
  def threePartPainter(resource:String) = new ThreePartPainter(new ImageIcon(classOf[ButtonPainter].getResource(resource)).getImage)
  val plain = threePartPainter("/boxes/swing/StandaloneButton.png")
  val focus = threePartPainter("/boxes/swing/StandaloneButtonFocusOverlay.png")
  val pressed = threePartPainter("/boxes/swing/StandaloneButtonPressed.png")
  val comboArrow = new ImageIcon(classOf[ButtonPainter].getResource("/boxes/swing/ComboArrow.png")).getImage
  val comboArrowWidth = comboArrow.getWidth(null)
}

class ButtonPainter(combo:Boolean = false) extends Painter[AbstractButton] {
  override def paint(g:Graphics2D, b:AbstractButton, w:Int, h:Int) {

    val oldComp = g.getComposite
    SwingView.graphicsForEnabledState(g, b.getModel.isEnabled)

    if (b.getModel.isSelected || b.getModel.isPressed) {
      ButtonPainter.pressed.paint(g, w, h)
    } else {
      ButtonPainter.plain.paint(g, w, h)
    }

    if (combo) {
      g.drawImage(ButtonPainter.comboArrow, w - ButtonPainter.comboArrowWidth, 0, null)
    }
    g.setComposite(oldComp)

    if (b.hasFocus || (combo && b.getModel.isSelected)) {
      ButtonPainter.focus.paint(g, w, h)
    }
  }
}

object SwingBarButton {
  def apply(name:String, icon:Option[Icon] = None, op:Op):EPButton = {
    apply(SwingOp(name, icon, op))
  }
  def apply(op:Op):EPButton = {
    val s = SwingOp(op)
    apply(s)
  }

  def apply(a:Action) = {
    new SwingBarButton(a)
  }
}

class SwingBarButton(a:Action) extends EPButton(a:Action) {
  {
    setBorder(new EmptyBorder(4,2,3,2))
    setContentAreaFilled(false)
    setBackgroundPainter(new BarStyleButtonPainter())
  }
}

class SwingBarToggleButton extends EPToggleButton{
  {
    setBorder(new EmptyBorder(4,2,3,2))
    setContentAreaFilled(false)
    setBackgroundPainter(new BarStyleToggleButtonPainter())
  }
}

class SwingToggleButton extends EPToggleButton{
  {
    setBorder(new EmptyBorder(7,12,6,12))
    setContentAreaFilled(false)
    setBackgroundPainter(new ButtonPainter())
  }
}

class DropdownButton extends EPToggleButton {
  {
    setBorder(new EmptyBorder(7,8,6,ButtonPainter.comboArrowWidth + 4))
    setContentAreaFilled(false)
    setBackgroundPainter(new ButtonPainter(true))
  }
}

object SwingButton {
  def apply(name:String, icon:Option[Icon] = None, op:Op):EPButton = new SwingButton(SwingOp(name, icon, op))
  def apply(op:Op):EPButton = new SwingButton(SwingOp(op))
  def apply(op:SwingOpAction):EPButton = new SwingButton(op)
}

class SwingButton(a:Action) extends EPButton(a) {
  {
    setBorder(new EmptyBorder(7,12,6,12))
    setContentAreaFilled(false)
    setBackgroundPainter(new ButtonPainter())
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
  val textBorder = new EmptyBorder(7, 8, 6, 8)

  def adjustComponent(c:JTextComponent) {
    c.setOpaque(false)
    c.setBorder(textBorder)
    c.setBackground(new Color(0, 0, 0, 0))
    c.setForeground(SwingView.textColor)
//      c.setFont(HudPaintingUtils.getHudFont)
    c.setSelectedTextColor(SwingView.selectedTextColor)
    c.setSelectionColor(SwingView.selectionColor)
    c.setCaretColor(SwingView.textColor)

    //TODO is there a better way of doing this? Otherwise
    //we don't get to repaint in paintSafely when focus changes
    c.addFocusListener(new FocusListener {
      def focusGained(e: FocusEvent) {
        c.repaint()
      }

      def focusLost(e: FocusEvent) {
        c.repaint()
      }
    })
    c.addPropertyChangeListener("enabled", new PropertyChangeListener {
      def propertyChange(evt: PropertyChangeEvent) {
        c.repaint()
      }
    })
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
//  def apply(c:JTextArea) = c.setUI(new BoxesTextAreaUI())
  def apply(c:JTextArea) {
    c.setOpaque(true)
    c.setBorder(null)
    c.setBackground(Color.white)
    c.setForeground(SwingView.textColor)
//      c.setFont(HudPaintingUtils.getHudFont)
    c.setSelectedTextColor(SwingView.selectedTextColor)
    c.setSelectionColor(SwingView.selectionColor)
    c.setCaretColor(SwingView.textColor)
  }


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
    c.setBorder(new EmptyBorder(5, 7, 6, 4))
  }

  override def paintSafely(g:Graphics) {
    val c = getComponent
    SpinnerTextComponentPainter.instance.paint(g.asInstanceOf[Graphics2D], c, c.getWidth, c.getHeight)
    super.paintSafely(g)
  }

}

object BoxesCheckBox {
  val checkboxIcons = (
    new ImageIcon(classOf[BoxesCheckBox].getResource("/boxes/swing/Checkbox.png")),
    new ImageIcon(classOf[BoxesCheckBox].getResource("/boxes/swing/CheckboxPressed.png")),
    new ImageIcon(classOf[BoxesCheckBox].getResource("/boxes/swing/CheckboxFocusOverlay.png"))
  )
  val radioIcons = (
    new ImageIcon(classOf[BoxesCheckBox].getResource("/boxes/swing/Radio.png")),
    new ImageIcon(classOf[BoxesCheckBox].getResource("/boxes/swing/RadioPressed.png")),
    new ImageIcon(classOf[BoxesCheckBox].getResource("/boxes/swing/RadioFocusOverlay.png"))
  )
}

class BoxesCheckBoxUI(val radio:Boolean = false) extends BasicCheckBoxUI {

  val icons = if (radio) BoxesCheckBox.radioIcons else BoxesCheckBox.checkboxIcons

  override def installDefaults(b:AbstractButton) {
    super.installDefaults(b)
    icon = new BoxesCheckIcon()
  }

  override def paint(graphics:Graphics, c:JComponent) {
    val g = graphics.asInstanceOf[Graphics2D]
    val oldComposite = g.getComposite
    SwingView.graphicsForEnabledState(g, c.isEnabled)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    super.paint(g, c);
    g.setComposite(oldComposite)
  }

  override def paintText(g:Graphics, c:JComponent, textRect:Rectangle, text:String) {
    val button = c.asInstanceOf[AbstractButton]
    val fontMetrics = g.getFontMetrics(button.getFont());
    val mnemonicIndex = button.getDisplayedMnemonicIndex();

    g.setColor(button.getForeground)
    BasicGraphicsUtils.drawStringUnderlineCharAt(g, text, mnemonicIndex,
      textRect.x + getTextShiftOffset(),
      textRect.y + fontMetrics.getAscent() + getTextShiftOffset())
  }

  class BoxesCheckIcon extends Icon {

    def paintIcon(c:Component, g:Graphics, x:Int, y:Int) {

      val button = c.asInstanceOf[AbstractButton]
      val model = button.getModel

      val graphics = g.create().asInstanceOf[Graphics2D]
      graphics.translate(x, y);

      if (model.isSelected()) {
        icons._2.paintIcon(c, graphics, 0, 0)
      } else {
        icons._1.paintIcon(c, graphics, 0, 0)
      }

      if (c.hasFocus) {
        icons._3.paintIcon(c, graphics, 0, 0)
      }

      graphics.dispose();
    }

    def getIconWidth = BoxesCheckBox.checkboxIcons._1.getIconWidth
    def getIconHeight = BoxesCheckBox.checkboxIcons._1.getIconHeight
  }

}


class BoxesCheckBox extends JCheckBox {
  setUI(new BoxesCheckBoxUI())
}

class BoxesRadioButton extends JCheckBox {
  setUI(new BoxesCheckBoxUI(true))
}

class HeaderLabelUI extends BasicLabelUI {
  override def installUI(c:JComponent) {
      super.installUI(c)
      c.setOpaque(false)
  }

  override def uninstallUI(c:JComponent) {
      super.uninstallUI(c)
  }

  override def paint(g:Graphics, c:JComponent) {
    g.drawImage(HeaderLabel.shadow, 0, HeaderLabel.height, c.getWidth, 12, null)
    BoxesTableCellHeaderRenderer.lastBGPainter.paint(g.asInstanceOf[Graphics2D], c, c.getWidth, HeaderLabel.height)
    super.paint(g, c)
  }

  override def paintEnabledText(label:JLabel, graphics:Graphics, s:String, textX:Int, textY:Int) {
    val g = graphics.asInstanceOf[Graphics2D]
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setFont(label.getFont());
    g.setColor(SwingView.textUnderlightColor)
    BasicGraphicsUtils.drawStringUnderlineCharAt(g, s, -1, textX, textY + 1);
//    g.setColor(WindowUtils.isParentWindowFocused(label)
    g.setColor(label.getForeground)
    BasicGraphicsUtils.drawStringUnderlineCharAt(g, s, -1, textX, textY);
  }

}

object HeaderLabel {
  val height = 22
  val shadow = new ImageIcon(classOf[HeaderLabelUI].getResource("/boxes/swing/Shadow.png")).getImage
  def apply(text:String, icon:Option[Icon] = None, horizontalAlignment:Int = SwingConstants.LEFT) = {
    val label = new JLabel(text, icon.getOrElse(null), horizontalAlignment)
    label.setUI(new HeaderLabelUI())
    label.setBorder(new EmptyBorder(7, 8, 16, 8))
    label.setPreferredSize(new Dimension(26, height + 10))
    label
  }
}