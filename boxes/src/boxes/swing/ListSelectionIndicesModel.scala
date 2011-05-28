package boxes.swing

import javax.swing.DefaultListSelectionModel;
import javax.swing.JTable;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener
import boxes.{VarGeneral, SwingView, View}

/**
* View/edit a Var[Set[Int]] as the selected indices of a ListSelectionModel
* used with a JTable.
* We use JTable specifically since ListModel is awful, and Lists can be replaced
* by a one column JTable to avoid exposure to more Swing. JCombos are even worse,
* and are replaced by a custom component that also uses JTable.
*
* TODO Check this handles filtering of the table, it should react
* to selection of rows that have been filtered out by removing those indices.
*/
class ListSelectionIndicesModel(v:VarGeneral[Set[Int],_], setFilter: =>Boolean, table:JTable) extends ListSelectionModel {

	val delegate = new DefaultListSelectionModel()
  delegate.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION)

  var selection = Set[Int]()

  var adjustingDelegate = false

  //Update Var from delegate
  delegate.addListSelectionListener(new ListSelectionListener() {
    override def valueChanged(e:ListSelectionEvent) {
        handleDelegateChange();
    }
  })

  //Update delegate from Var
  val view = View {
    val newIndices = v()
    SwingView.replaceUpdate (this, handleVarChange(newIndices))
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

  private def delegateSelectionSet() = {
    if (delegate.isSelectionEmpty) {
      Set[Int]()
    } else {
      //Filter selection range to get actual indices, then map from view indices to model indices, and make the set
      Set[Int]((delegate.getMinSelectionIndex until delegate.getMaxSelectionIndex + 1).filter(delegate.isSelectedIndex(_)).flatMap(indexToModel(_)):_*)
    }
  }

	private def handleDelegateChange() {

		//Skip when we are actually adjusting delegate in this class, so we
		//don't respond to our own changes.
		if (adjustingDelegate) return;

		//Update our cached selection
		selection = delegateSelectionSet

		//If we can set selection, update the Var to match the delegate
		if (setFilter) {
      v() = selection
		//If we are ignoring changes to the delegate, then we need to
		//revert the delegate back to mirroring the Var
		} else {
			handleVarChange(v())
		}
	}

  //TODO update this
	private def handleVarChange(indices:Set[Int]) {

    try {
		  adjustingDelegate = true

			//Only act if selection changes
			if (!indices.sameElements(selection)) {

        //"Adjusting" is best we can do, since there doesn't seem to be a way
        //to make a batch change to the delegate
				delegate.setValueIsAdjusting(true)

				val originalAnchor = delegate.getAnchorSelectionIndex

				delegate.clearSelection

				indices.foreach(i => {
					try {
						indexToView(i).foreach(row => delegate.addSelectionInterval(row, row))
					} catch {
            case ioobe:IndexOutOfBoundsException => println("Invalid model row index " + i)
					}
				})

				//Restore the original anchor if it is still selected
				if (delegate.isSelectedIndex(originalAnchor)) {
					delegate.setAnchorSelectionIndex(originalAnchor)
				}

				delegate.setValueIsAdjusting(false);

      }

    } finally {
      adjustingDelegate = false
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
