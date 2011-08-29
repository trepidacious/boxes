package boxes.swing

import collection._
import javax.swing.plaf.basic.BasicSpinnerUI
import com.explodingpixels.painter.Painter
import java.awt.{Image, Graphics2D, Dimension, Container, Component, LayoutManager}
import com.explodingpixels.widgets.ImageUtils
import javax.swing.{SwingConstants, AbstractButton, ImageIcon, JPanel, JSpinner, JComponent}
import javax.swing.JComponent._
import javax.swing.border.EmptyBorder
import javax.swing.AbstractButton._
import com.explodingpixels.swingx.EPButton

object BoxesSpinnerLayout {
  val spinnerButtonWidth = 19
  val height = 28
}

class BoxesSpinnerLayout extends LayoutManager {

  val components = mutable.HashMap[String, Component]()
  val acceptedNames = Set("Next", "Previous", "Editor")

  override def addLayoutComponent(name:String, c:Component) = if (acceptedNames.contains(name)) components.put(name, c)

  override def removeLayoutComponent(c:Component) {
    components.find(entry => entry._2 == c).foreach(entry => components.remove(entry._1))
  }

  override def preferredLayoutSize(parent:Container) = {
    components.get("Editor") match {
      case None => new Dimension(BoxesSpinnerLayout.spinnerButtonWidth * 2, BoxesSpinnerLayout.height)
      case Some(editor) => new Dimension(editor.getWidth + BoxesSpinnerLayout.spinnerButtonWidth, BoxesSpinnerLayout.height)
    }
  }

  override def minimumLayoutSize(parent:Container) = preferredLayoutSize(parent)

  override def layoutContainer(parent:Container) {
    val width = parent.getWidth
    val height = parent.getHeight

    val editorWidth = width - BoxesSpinnerLayout.spinnerButtonWidth;

    val buttonHeight = BoxesSpinnerLayout.height / 2
    val buttonWidth = BoxesSpinnerLayout.spinnerButtonWidth

    components.get("Editor").foreach(editor => editor.setBounds(0, 0, editorWidth, height))
    components.get("Next").foreach(next => next.setBounds(editorWidth, 0, buttonWidth, buttonHeight))
    components.get("Previous").foreach(editor => editor.setBounds(editorWidth, buttonHeight, buttonWidth, buttonHeight))
  }
}

class SpinnerButton(up:Boolean) extends EPButton {
  {
    setBorder(null)
    setContentAreaFilled(false)
    setBackgroundPainter(new SpinnerButtonPainter(up))
    setName(if (up) "Spinner.previousButton" else "Spinner.nextButton")
    setInheritsPopupMenu(true)
  }
  override def isFocusTraversable = false
}

class BoxesSpinnerUI extends BasicSpinnerUI {

  override def createLayout() = new BoxesSpinnerLayout()

  override def createPreviousButton() = {
    val b = new SpinnerButton(true)
    installPreviousButtonListeners(b)
    b
  }

  override def createNextButton() = {
    val b = new SpinnerButton(false)
    installNextButtonListeners(b)
    b
  }

  override def createEditor() = {
    val editor = spinner.getEditor()
    configureEditorBorder(editor)
    editor
  }

  override def replaceEditor(oldEditor:JComponent, newEditor:JComponent) {
    spinner.remove(oldEditor)
    configureEditorBorder(newEditor)
    spinner.add(newEditor, "Editor")
  }

  def configureEditorBorder(editor:JComponent) {
    editor match {
      case defaultEditor:JSpinner.DefaultEditor => {
        val editorField = defaultEditor.getTextField()
        SpinnerTextFieldUI(editorField)
      }

//      case editor:JPanel if (editor.getBorder() != null && editor.getComponentCount() > 0) => {
//        val editorField = editor.getComponent(0).asInstanceOf[JComponent]
//        BoxesTextFieldUI(editorField)
//      }
    }
  }

}

class ArrowPartPainter(image:Image, up:Boolean) {
  val part = {
    if (up) {
      ImageUtils.getSubImage(image, 0, BoxesSpinnerLayout.height/2, BoxesSpinnerLayout.spinnerButtonWidth, BoxesSpinnerLayout.height/2)
    } else {
      ImageUtils.getSubImage(image, 0, 0, BoxesSpinnerLayout.spinnerButtonWidth, BoxesSpinnerLayout.height/2)
    }
  }

  def paint(g:Graphics2D, w:Int, h:Int) {
    g.drawImage(part, 0, 0, null)
  }
}

class ArrowPartPainters(up:Boolean) {
  def partPainter(resource:String, up:Boolean) = new ArrowPartPainter(new ImageIcon(classOf[ButtonPainter].getResource(resource)).getImage, up)
  val plain = partPainter("/boxes/swing/SpinnerButtons.png", up)
  val disabled = partPainter("/boxes/swing/SpinnerButtonsDisabled.png", up)
  val focus = partPainter("/boxes/swing/SpinnerButtonsFocusOverlay.png", up)
  val pressed = partPainter("/boxes/swing/SpinnerButtonsPressed.png", up)
}

object ArrowPainter {
  val up = new ArrowPartPainters(true)
  val down = new ArrowPartPainters(false)
  def direction(isUp:Boolean) = if (isUp) up else down
}

class SpinnerButtonPainter(val up:Boolean) extends Painter[AbstractButton] {

  val painter = ArrowPainter.direction(up)

  override def paint(g:Graphics2D, b:AbstractButton, w:Int, h:Int) {
    if (b.getModel.isSelected || b.getModel.isPressed) {
      painter.pressed.paint(g, w, h)
    } else if (b.getModel.isEnabled) {
      painter.plain.paint(g, w, h)
    } else {
      painter.disabled.paint(g, w, h)
    }

    if (b.hasFocus) {
      painter.focus.paint(g, w, h)
    }
  }
}

