package boxes.swing

import java.awt.{Toolkit, Dimension, Color, BorderLayout, Component}
import javax.swing.event.{PopupMenuEvent, PopupMenuListener}
import boxes._
import javax.swing.{JComponent, SwingConstants, KeyStroke, JInternalFrame, AbstractAction, JToggleButton, SwingUtilities, JPopupMenu, JComboBox}
import javax.swing.border.{EmptyBorder, MatteBorder}
import java.awt.event.{FocusEvent, FocusAdapter, MouseEvent, MouseAdapter, KeyEvent, KeyAdapter, ActionEvent, ActionListener}

class BoxesDropdownView(v:LedgerVar, i:VarBox[Option[Int], _], sorting:Boolean = false, minWidth:Int = 250, maxHeight:Int = 300, displayHeader:Boolean = false) extends SwingView {

  val component = new DropdownButton()

  private val button = component
  
//  val preventHide = new JComboBox().getClientProperty("doNotCancelPopup");
//  button.putClientProperty("doNotCancelPopup", preventHide);

  private val ledgerScrollView = LedgerView.singleSelectionScroll(v, i, sorting)

  //TODO should we accept field index as parameter, or accept a list and build our own ledger so we can display list element,
  //or attempt to display the entire ledger row? In most cases, we really do expect to have a ledger with a single column.
  //In any case, in the long term, when we have a LAbelView that accepts an arbitrary data type and associated renderer,
  //we can get rid of the to toString.
  private val selectedString = Cal{
    val str = for (index <- i() if (index >= 0 && index < v().recordCount)) yield v().apply(index, 0).toString
    str.getOrElse("No Selection")
  }

  private val labelView = LabelView(selectedString)
  labelView.component.setBorder(new EmptyBorder(0, 0, 0, 0))

  button.add(labelView.component)

  //Set popup height before displaying - it will handle width itself
  private val preparePopup = {() => {
    val height = math.min(ledgerScrollView.ledgerView.component.getPreferredSize.height, maxHeight)
    val size = new Dimension(10, height)
    ledgerScrollView.component.setPreferredSize(size)
    ledgerScrollView.component.setMaximumSize(size)
    ledgerScrollView.component.setMinimumSize(size)
  }}

  private val handler = new PopupHandler(ledgerScrollView.component, ledgerScrollView.ledgerView.component, button, minWidth, preparePopup)

  button.addActionListener(new ActionListener {
    def actionPerformed(e: ActionEvent) {
      if (button.isSelected) handler.show()
    }
  })

  private val table = ledgerScrollView.ledgerView.component

  table.addKeyListener(new KeyAdapter() {
    override def keyTyped(e:KeyEvent) {
      SwingUtilities.invokeLater(new Runnable() {
        override def run() = handler.hide()
      })
    }
  })

  table.addMouseListener(new MouseAdapter() {
    override def mouseClicked(e:MouseEvent) {
      if (e.getButton() == MouseEvent.BUTTON1) {
        SwingUtilities.invokeLater(new Runnable() {
          override def run() = handler.hide()
        })
      }
    }
  })

  table.addFocusListener(new FocusAdapter() {
    override def focusLost(e:FocusEvent) {
      SwingUtilities.invokeLater(new Runnable() {
        override def run() = handler.hide()
      })
    }
  })

  //Do nothing on pressing enter - otherwise it moves selection down one row
  val jpropellerNullAction = new AbstractAction() {
    override def actionPerformed(e:ActionEvent) {}
  }

  table.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT)
    .put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), "jpropellerNullAction");
  table.getInputMap(JComponent.WHEN_FOCUSED)
    .put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), "jpropellerNullAction");
  table.getActionMap().put("jpropellerNullAction", jpropellerNullAction);

  if (!displayHeader) {
    table.setTableHeader(null)
  }

}

class PopupHandler(popupComponent:Component, focusComponent:Component, invoker:Component, minWidth:Int, preparePopup: () => Unit) extends PopupMenuListener {

  private val xOffset = 2
  private val yOffsetSize = 2

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
    val width = math.max(invoker.getWidth, minWidth)
    preparePopup.apply()
    popupComponent.setPreferredSize(new Dimension(width, popupComponent.getPreferredSize.height));
    popupComponent.setMinimumSize(new Dimension(width, popupComponent.getMinimumSize.height));
    popupComponent.setMaximumSize(new Dimension(width, popupComponent.getMaximumSize.height));
    popup.pack();

    //Find position relative to invoker - if we would appear (partially) off screen bottom, display above
    //instead of below
    var y = invoker.getHeight + yOffsetSize;
    if (invoker.getLocationOnScreen.getY + y + popup.getHeight > Toolkit.getDefaultToolkit.getScreenSize.height) {
      y = - popup.getHeight - yOffsetSize;
    }
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