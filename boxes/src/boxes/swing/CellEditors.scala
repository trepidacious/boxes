package boxes.swing



import java.text.ParseException;

import javax.swing.text.DefaultFormatterFactory;
import javax.swing.text.NumberFormatter
import boxes.util.NumericClass
import javax.swing._
import border.{CompoundBorder, EmptyBorder}
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

object CellEditorBorders {
  val textBorder = new CompoundBorder(UIManager.getBorder("Table.focusCellHighlightBorder"), new EmptyBorder(1,3,0,0))
  val rendererBorder = new EmptyBorder(1,4,0,1)
  val selectedRendererBorder = new CompoundBorder(UIManager.getBorder("Table.focusCellHighlightBorder"), new EmptyBorder(1,3,0,0))
}

class CellEditorJTFormattedTextField extends JFormattedTextField {
  setBorder(CellEditorBorders.textBorder)
}

object NumberCellEditor {
  def apply[N](c:Class[N])(implicit numericClass:NumericClass[N]) = new NumberCellEditor(
    new CellEditorJTFormattedTextField(),
    if (numericClass.isWhole) "a whole number" else "a number"
  )(numericClass)
}

class CellEditorJTextField extends JTextField {
  setBorder(CellEditorBorders.textBorder)
}

class SelectingTextCellEditor extends DefaultCellEditor(new CellEditorJTextField()) {
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

class BoxesTableCellRenderer extends DefaultTableCellRenderer {
  override def getTableCellRendererComponent(table:JTable, value:Object, isSelected:Boolean, hasFocus:Boolean, row:Int, column:Int) = {
    if (isSelected) {
      super.setForeground(table.getSelectionForeground())
      super.setBackground(table.getSelectionBackground())
	  } else {
      var background = table.getBackground()
      val alternateColor = UIManager.getColor("Table.alternateRowColor")
      if (alternateColor != null && row % 2 == 0) background = alternateColor
      super.setForeground(table.getForeground())
      super.setBackground(background);
	  }

	  setFont(table.getFont())

//	if (hasFocus) {
//            Border border = null;
//            if (isSelected) {
//                border = DefaultLookup.getBorder(this, ui, "Table.focusSelectedCellHighlightBorder");
//            }
//            if (border == null) {
//                border = DefaultLookup.getBorder(this, ui, "Table.focusCellHighlightBorder");
//            }
//            setBorder(border);
//
//	    if (!isSelected && table.isCellEditable(row, column)) {
//                Color col;
//                col = DefaultLookup.getColor(this, ui, "Table.focusCellForeground");
//                if (col != null) {
//                    super.setForeground(col);
//                }
//                col = DefaultLookup.getColor(this, ui, "Table.focusCellBackground");
//                if (col != null) {
//                    super.setBackground(col);
//                }
//	    }
//	} else {
//            setBorder(getNoFocusBorder());
//	}

    if (hasFocus && isSelected) {
      setBorder(CellEditorBorders.selectedRendererBorder)
    } else {
      setBorder(CellEditorBorders.rendererBorder)
    }
    setValue(value)
    this
  }
//  setBorder(CellEditorBorders.rendererBorder)
}

object BooleanCellRenderer {
  val tick = BoxesCheckBox.pressedIcon
  val untick = BoxesCheckBox.disabledIcon
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

class NumberCellRenderer[N](nc:NumericClass[N]) extends BoxesTableCellRenderer {

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

      //Only space key presses edit
      case keyEvent:KeyEvent => {
        if (keyEvent.getKeyCode == KeyEvent.VK_SPACE) {
          completeImmediately = true
          true
        } else {
          false
        }
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
