package boxes.swing

import javax.swing.DefaultListSelectionModel;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener
import boxes.{VarGeneral, View}

/**
 * View/edit a Var[Int] as the selected index of a ListSelectionModel
 * used with a JTable.
 * We use JTable specifically since ListModel is awful, and Lists can be replaced
 * by a one column JTable to avoid exposure to more Swing. JCombos are even worse,
 * and are replaced by a custom component that also uses JTable.
 *
 * TODO Check this handles filtering of the table, it should react
 * to selection of rows that have been filtered out by clearing selection.
 */
class ListSelectionIndexModel(v:VarGeneral[Option[Int],_], setFilter: =>Boolean, table:JTable) extends ListSelectionModel {

	val delegate = new DefaultListSelectionModel()
  delegate.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)

  //Update Var from delegate
  delegate.addListSelectionListener(new ListSelectionListener() {
    override def valueChanged(e:ListSelectionEvent) {
        handleDelegateChange();
    }
  })

  //Update delegate from Var
  val view = View {
    val newIndex = v()
    SwingView.replaceUpdate (this, handleVarChange(newIndex))
  }

  private def indexToView(i:Int):Option[Int] = {
    try {
      val converted = table.convertRowIndexToView(i)
      if (converted >= 0) Some(converted) else None
    } catch {
      //If index is out of bounds, treat as no selection
      case _ => None
    }
  }

  private def indexToModel(i:Int):Option[Int] = {
    try {
      val converted = table.convertRowIndexToModel(i)
      if (converted >= 0) Some(converted) else None
    } catch {
      //If index is out of bounds, treat as no selection
      case _ => None
    }
  }

	private def handleDelegateChange() {
		//If we can set selection, update the Var to match the delegate
		if (setFilter) {
      //Note that we delay this change to var, so that any pending swing
      //actions occur BEFORE we change the var. This gives other swing code
      //a chance to execute BEFORE it receives any additional changes.
      //For example, anything editing an object selected (in a Cal) by
      //the current index will have a chance to respond to losing focus
      //by applying pending edits, BEFORE the selected object changes to
      //a new instance.
      SwingView.addUpdate(this,
			  v() = indexToModel(delegate.getMinSelectionIndex())
      )

		//If we are ignoring changes to the delegate, then we need to
		//revert the delegate back to mirroring the Var
		} else {
			handleVarChange(v())
		}
	}

	private def handleVarChange(optionI:Option[Int]) {
		var index = optionI flatMap {indexToView(_)}

    index match {
      case None => if (!delegate.isSelectionEmpty) delegate.clearSelection
      case Some(i) => if (delegate.getMinSelectionIndex() != i) delegate.setSelectionInterval(i, i)
    }
	}

	//Delegated ListSelectionModel methods
  override def addListSelectionListener(l:ListSelectionListener)  = delegate.addListSelectionListener(l)
	override def getAnchorSelectionIndex() = delegate.getAnchorSelectionIndex()
	override def getLeadSelectionIndex() = delegate.getLeadSelectionIndex()
	override def getMaxSelectionIndex() = delegate.getMaxSelectionIndex()
	override def getMinSelectionIndex() = delegate.getMinSelectionIndex()
	override def getSelectionMode() = delegate.getSelectionMode()
	override def getValueIsAdjusting() = delegate.getValueIsAdjusting()
  override def isSelectedIndex(index:Int) = delegate.isSelectedIndex(index)
  override def isSelectionEmpty() = delegate.isSelectionEmpty()
  override def removeListSelectionListener(l:ListSelectionListener) = delegate.removeListSelectionListener(l)

  //Delegated ListSelectionModel methods that change the selection, and so are filtered by setFilter
	override def addSelectionInterval(index0:Int, index1:Int) {
    if (setFilter) delegate.addSelectionInterval(index0, index1)
  }
	override def clearSelection() {
		if (setFilter) delegate.clearSelection()
	}
	override def insertIndexInterval(index:Int, length:Int, before:Boolean) {
		if (setFilter) delegate.insertIndexInterval(index, length, before)
	}
	override def removeIndexInterval(index0:Int, index1:Int) {
		if (!setFilter) delegate.removeIndexInterval(index0, index1)
	}
	override def removeSelectionInterval(index0:Int, index1:Int) {
		if (setFilter) delegate.removeSelectionInterval(index0, index1)
	}
	override def setAnchorSelectionIndex(anchorIndex:Int) {
		if (setFilter) delegate.setAnchorSelectionIndex(anchorIndex)
	}
	override def setLeadSelectionIndex(leadIndex:Int) {
		if (setFilter) delegate.setLeadSelectionIndex(leadIndex)
	}
	override def setSelectionInterval(index0:Int, index1:Int) {
		if (setFilter) delegate.setSelectionInterval(index0, index1)
	}
	override def setSelectionMode(selectionMode:Int) {
		if (setFilter) delegate.setSelectionMode(selectionMode)
	}
	override def setValueIsAdjusting(isAdjusting:Boolean) {
		if (setFilter) delegate.setValueIsAdjusting(isAdjusting)
	}

}
