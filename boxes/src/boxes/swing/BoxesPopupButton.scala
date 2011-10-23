package boxes.swing

import java.awt.{Toolkit, BorderLayout, Component}
import javax.swing.event.{PopupMenuEvent, PopupMenuListener}
import javax.swing.border.{MatteBorder}
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.{Icon, JComponent, JToggleButton, SwingUtilities, JPopupMenu}
import boxes.BooleanControlType._
import boxes.{LinkingToolbarToggleButton, LinkingJToggleButton, Val, RefGeneral, SwingView}
import boxes.View

private class BoxesPopupButtonHandler(popupComponent:Component, focusComponent:Component, invoker:Component) extends PopupMenuListener {

  private val xOffset = -1
  private val yOffsetSize = -1

  val popup = new JPopupMenu();
  popup.removeAll()
  popup.setBorder(new MatteBorder(1, 1, 1, 1, SwingView.dividingColor))
  popup.setLayout(new BorderLayout())
  popup.add(popupComponent, BorderLayout.CENTER)
  popup.addPopupMenuListener(this)

  popup.pack()


  def hide() {
    popup.setVisible(false)
  }

  def show() {
    popup.pack();

    //Find position relative to invoker - if we would appear (partially) off screen top, display below
    //instead of above
    var y = - popup.getHeight - yOffsetSize;
    if (invoker.getLocationOnScreen.getY + y < 0) {
      y = invoker.getHeight + yOffsetSize;
    }
    popup.setLocation(xOffset, y)
    popup.show(invoker, xOffset, y);

    //Start with correct component focused
    SwingUtilities.invokeLater(new Runnable() {
      override def run() {
        if (focusComponent != null) {
          focusComponent.requestFocus()
        }
      }
    })
  }

  override def popupMenuWillBecomeVisible(e:PopupMenuEvent) {}

  override def popupMenuWillBecomeInvisible(e:PopupMenuEvent) {
    invoker match {
      case button:JToggleButton => button.setSelected(false)
    }
  }

  override def popupMenuCanceled(e:PopupMenuEvent) {}
}

object BoxesPopupView {
  def apply(n:RefGeneral[String,_] = Val(""), controlType:BooleanControlType = TOGGLEBUTTON, icon:RefGeneral[Option[Icon], _] = Val(None), popupContents:JComponent) = {
    new BoxesPopupView(n, controlType, icon, popupContents)
  }
}

class BoxesPopupView(n:RefGeneral[String,_] = Val(""), controlType:BooleanControlType = TOGGLEBUTTON, icon:RefGeneral[Option[Icon], _] = Val(None), popupContents:JComponent) extends SwingView {

  //Only support types that make sense, otherwise default to a plain toggle button
  val component = controlType match {
    case TOOLBARBUTTON => new LinkingToolbarToggleButton(this)
    case _ => new LinkingJToggleButton(this)
  }

  val view = View {
    //Store the values for later use on Swing Thread
    val newN = n()
    val newIcon = icon()
    //This will be called from Swing Thread
    replaceUpdate { display(newN, newIcon) }
  }

  //Update display if necessary
  private def display(newN:String, newIcon:Option[Icon]) {
    if (newN != component.getText) {
      component.setText(newN)
    }
    val iconOrNull = newIcon.getOrElse(null)
    if (iconOrNull != component.getIcon) {
      component.setIcon(iconOrNull)
    }
  }

  private val handler = new BoxesPopupButtonHandler(popupContents, popupContents, component)

  component.addActionListener(new ActionListener {
    def actionPerformed(e: ActionEvent) {
      if (component.isSelected) handler.show()
    }
  })

}
