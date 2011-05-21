package boxes.swing



import java.text.ParseException;

import javax.swing.text.DefaultFormatterFactory;
import javax.swing.text.NumberFormatter
import boxes.util.NumericClass
import javax.swing._
import math.ScalaNumber
import java.awt.{Graphics2D, Graphics, Color, Component}
import table.{DefaultTableCellRenderer, TableCellEditor}
import java.awt.event.{MouseEvent, MouseAdapter, ActionEvent, KeyEvent}
import java.util.EventObject

/**
 * Implements a {@link CellEditor} that uses a {@link JFormattedTextField}
 * to edit {@link Number} values.
 */
class NumberCellEditor[N](val ftf:JFormattedTextField, val requiredContentsDescription:String)(implicit val numericClass:NumericClass[N]) extends DefaultCellEditor(ftf) {

  ftf.setFormatterFactory(new DefaultFormatterFactory(new NumberFormatter(numericClass.formatInstance)))
  ftf.setHorizontalAlignment(SwingConstants.TRAILING)
  ftf.setFocusLostBehavior(JFormattedTextField.PERSIST)

  // React when the user presses Enter while the editor is
  // active. (Tab is handled as specified by
  // JFormattedTextField's focusLostBehavior property.)
  ftf.getInputMap().put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), "check")
  ftf.getActionMap().put("check", new AbstractAction() {
    def actionPerformed(e:ActionEvent) {
      if (!ftf.isEditValid()) { // The text is invalid.
        if (userSaysRevert()) { // reverted
          ftf.postActionEvent(); // inform the editor
        }
        //If user didn't want to revert, do nothing and editing continues
      } else
        try { // The text is valid,
          ftf.commitEdit(); // so use it.
          ftf.postActionEvent(); // stop editing
        } catch {
        //TODO logger
          case exc:java.text.ParseException => println("actionPerformed: can't parse: " + exc);
        }
    }
  })

	override def getTableCellEditorComponent(table:JTable, value:Object, isSelected:Boolean, row:Int, column:Int) = {
		val newField = super.getTableCellEditorComponent(table, value, isSelected, row, column).asInstanceOf[JFormattedTextField]
		newField.setValue(value)

		//Select all text for sane editing behaviour - contents are replaced if
		//the user starts an edit by typing, editing proceeds as normal if they
		//double click
		newField.selectAll()
		newField
	}

	// Override to ensure that the value remains valid.
	override def getCellEditorValue() = {
		val o = getComponent().asInstanceOf[JFormattedTextField].getValue()
		try {
      numericClass.parse(o.toString).asInstanceOf[AnyRef]
		} catch {
      case e:ParseException => {
        //TODO logger
			  println("getCellEditorValue: can't parse o: " + o)
			  null
      }
		  case (e:ClassCastException) => {
        //TODO logger
			  println("getCellEditorValue: non-Number from numberFormat on " + o)
			  null
      }
		}
	}

	// Override to check whether the edit is valid,
	// setting the value if it is and complaining if
	// it isn't. If it's OK for the editor to go
	// away, we need to invoke the superclass's version
	// of this method so that everything gets cleaned up.
	override def stopCellEditing() = {
		val newField = getComponent().asInstanceOf[JFormattedTextField]
		if (newField.isEditValid()) {
			try {
				newField.commitEdit();
			} catch {
        case _ => {}
			}
		} else { // text is invalid
			if (!userSaysRevert()) { // user wants to edit
				false // don't let the editor go away
			}
		}
		super.stopCellEditing()
	}

	/**
	 * Lets the user know that the text they entered is bad. Returns true if the
	 * user elects to revert to the last good value. Otherwise, returns false,
	 * indicating that the user wants to continue editing.
	 */
	private def userSaysRevert() = {
		ftf.selectAll();

		val prompt = "The value must be " + requiredContentsDescription + ".\nYou can either continue editing or revert to the last valid value."

		val options = Array("Edit":Object, "Revert":Object)

		val answer = JOptionPane.showOptionDialog(
      SwingUtilities.getWindowAncestor(ftf),
		  prompt,
			"Invalid Text Entered",
      JOptionPane.YES_NO_OPTION,
			JOptionPane.ERROR_MESSAGE,
      null,
      options,
      options(1)
    )

		if (answer == 1) { // Revert!
			ftf.setValue(ftf.getValue());
			true
		} else {
		  false
    }
	}

}

object NumberCellEditor {
  def apply[N](c:Class[N])(implicit numericClass:NumericClass[N]) = new NumberCellEditor(
    new JFormattedTextField(),
    if (numericClass.isWhole) "a whole number" else "a number"
  )(numericClass)
}

class SelectingTextCellEditor extends DefaultCellEditor(new JTextField()) {
	override def getTableCellEditorComponent(table:JTable, value:Object, isSelected:Boolean, row:Int, column:Int) = {
		super.getTableCellEditorComponent(table, value, isSelected, row, column) match {
      case tf:JTextField => {
        tf.selectAll
        tf
      }
      case c => c
    }
	}
}

object SelectingTextCellEditor {
  def apply() = new SelectingTextCellEditor
}

object BooleanCellRenderer {
  //TODO get these from SwingView icon factory, so they can be replaced easily
  val tick = IconFactory.icon(classOf[BooleanCellRenderer], "tick.png")
  val untick = IconFactory.icon(classOf[BooleanCellRenderer], "untick.png")
  val opaque = new BooleanCellRenderer(true)
  val transparent = new BooleanCellRenderer(false)
}

class BooleanCellRenderer(val opaque:Boolean) extends DefaultTableCellRenderer {

  setOpaque(opaque)
  setHorizontalAlignment(SwingConstants.CENTER)

	override def setValue(value:Any) {
    value match {
      case b:Boolean => setIcon(if (b) BooleanCellRenderer.tick else BooleanCellRenderer.untick)
      case _ => setIcon(null)
    }
	}
}

object NumberCellRenderer {
  def apply[N](c:Class[N])(implicit nc:NumericClass[N]) = new NumberCellRenderer[N](nc)
}

class NumberCellRenderer[N](nc:NumericClass[N]) extends DefaultTableCellRenderer {

  setHorizontalAlignment(SwingConstants.RIGHT)

	override def setValue(value:Any) {
    value match {
      case n:N => setText(nc.format(n))
      case v => setText(v.toString)
    }
	}
}


object BooleanCellEditor {
  def apply() = new BooleanCellEditor
}

class BooleanCellEditor extends AbstractCellEditor with TableCellEditor {

	var completeImmediately = false
	var toggled = false
	var originalValue = false
	var inComponent = false

	val renderer = new BooleanCellRenderer(true)
	var rendererComponent:Option[Component] = None

	val highlightColor = new Color(255, 255, 255, 50)

  //Our editing component
	val label = new JLabel() {
		override def paintComponent(g:Graphics) {
			rendererComponent.foreach(c => {
				c.setBounds(getBounds())
				c.paint(g)
			})

      //Highlight on mouseover
			if (inComponent) {
				val g2 = g.asInstanceOf[Graphics2D]
				g2.setColor(highlightColor)
				g.fillRect(0, 0, getWidth(), getHeight())
			}
		}
	}

  label.setOpaque(true);
  label.addMouseListener(new MouseAdapter() {
    override def mouseReleased(e:MouseEvent) {
      super.mouseReleased(e)
      if (label.contains(e.getPoint())) {
        toggled = true
      }
      fireEditingStopped()
    }
    override def mouseEntered(e:MouseEvent) {
      super.mouseEntered(e)
      inComponent = true
      label.repaint()
    }
    override def mouseExited(e:MouseEvent) {
      super.mouseExited(e)
      inComponent = false
      label.repaint()
    }
  })

	//Implement the one CellEditor method that AbstractCellEditor doesn't.
	override def getCellEditorValue = (if (toggled) !originalValue else originalValue).asInstanceOf[AnyRef]

	//Implement the one method defined by TableCellEditor.
  override def getTableCellEditorComponent(
    table:JTable,
	  value:Any,
	  isSelected:Boolean,
	  row:Int,
		column:Int) = {

    value match {
      case b:Boolean => {
        originalValue = b

        //Normally we start out doing nothing
        toggled = false;

        //If we will complete immediately, then invoke
        //an event so the editor will essentially never appear,
        //and also set toggled so we will edit.
        if (completeImmediately) {
          toggled = true;
          SwingUtilities.invokeLater(new Runnable() {
            override def run() {
              fireEditingStopped()
            }
          })
        }
        rendererComponent = Some(renderer.getTableCellRendererComponent(table, value, true, true, row, column))
        label
      }
      case _ => null
    }
	}

	override def isCellEditable(anEvent:EventObject) = {
    anEvent match {

      //Only single clicks start editing using mouse, and
      //in this case we want to wait for the mouse to be released
      case mouseEvent:MouseEvent => {
        inComponent = true
        completeImmediately = false
        mouseEvent.getClickCount() >= 1
      }

      //Other events start editing, but complete immediately -
      //this will just toggle the contents
      case _ => {
        completeImmediately = true
        true
      }
    }
	}
}
