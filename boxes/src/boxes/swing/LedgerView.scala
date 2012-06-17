package boxes.swing

import java.util.concurrent.atomic.AtomicBoolean

import scala.collection.immutable

import boxes._
import boxes.list._
import javax.swing.event.ChangeEvent
import javax.swing.event.TableColumnModelEvent
import javax.swing.event.TableModelEvent
import javax.swing.table.AbstractTableModel
import javax.swing.table.TableCellEditor
import javax.swing.table.TableCellRenderer
import javax.swing.table.TableModel
import javax.swing.{JTable, JPanel, JComponent}
import java.awt.BorderLayout

object LedgerView {
  def apply(v:LedgerVar, sorting:Boolean = false) = {
    val lv = new LedgerView(v)
    if (sorting) lv.component.setAutoCreateRowSorter(true)
    lv
  }

  def singleSelection(v:LedgerVar, i:VarBox[Option[Int], _], sorting:Boolean = false) = {
    val lv = new LedgerView(v)
    //Only allow the selection to be set when the table is NOT responding
    //to a model change.
    //This is somewhat messy, but is necessary to wrest control of updating the selection
    //away from the JTable - we already update the selection ourself in a more intelligent
    //way, so we only want the selection changes that are NOT in response to a table model
    //change, but in response to a user selection action
    lv.component.setSelectionModel(new ListSelectionIndexModel(i, !lv.component.isRespondingToChange, lv.component))
    if (sorting) lv.component.setAutoCreateRowSorter(true)
    lv
  }
  def multiSelection(v:LedgerVar, i:VarBox[immutable.Set[Int], _], sorting:Boolean = false) = {
    val lv = new LedgerView(v)
    lv.component.setSelectionModel(new ListSelectionIndicesModel(i, !lv.component.isRespondingToChange, lv.component))
    if (sorting) lv.component.setAutoCreateRowSorter(true)
    lv
  }

  def singleSelectionScroll(v:LedgerVar, i:VarBox[Option[Int], _], sorting:Boolean = false) = {
    val lv = new LedgerView(v)
    lv.component.setSelectionModel(new ListSelectionIndexModel(i, !lv.component.isRespondingToChange, lv.component))
    //TODO is there a better way to do the match?
    val lsv = new LedgerScrollView(lv, v, Cal{
      i().map(immutable.Set(_)).getOrElse(immutable.Set[Int]())
    })
      
//      i() match {
//        case None => immutable.Set[Int]()
//        case Some(index) => immutable.Set(index)
//      }
//    })
    if (sorting) lv.component.setAutoCreateRowSorter(true)
    lsv
  }

  def multiSelectionScroll(v:LedgerVar, i:VarBox[immutable.Set[Int], _], sorting:Boolean = false) = {
    val lv = new LedgerView(v)
    lv.component.setSelectionModel(new ListSelectionIndicesModel(i, !lv.component.isRespondingToChange, lv.component))
    val lsv = new LedgerScrollView(lv, v, i)
    if (sorting) lv.component.setAutoCreateRowSorter(true)
    lsv
  }

  def list[T](list:ListVar[T], view:Box[RecordView[T], _], i:VarBox[Option[Int], _], sorting:Boolean, source: => Option[T], target:T => Unit, component:JComponent, additionalViews:SwingView*) = {
    
    val ledger = ListLedgerVar(list, view)
    val ledgerView = singleSelectionScroll(ledger, i, sorting)

    val add = new ListAddOp(list, i, source)
    val delete = new ListDeleteOp(list, i, target)

    val up = new ListMoveOp(list, i, true)
    val down = new ListMoveOp(list, i, false)

    val buttons = SwingButtonBar().add(add).add(delete).add(up).add(down);
    val panel = additionalViews.foldLeft(buttons){case (b, a) => b.add(a)}.buildWithListStyleComponent(component)

    val mainPanel = new JPanel(new BorderLayout())
    mainPanel.add(ledgerView.component, BorderLayout.CENTER)
    mainPanel.add(panel, BorderLayout.SOUTH)

    mainPanel
  }
}


class LedgerScrollView(val ledgerView:LedgerView, val ledger:LedgerVar, val indices:Box[immutable.Set[Int], _]) extends SwingView {
  val component = new LinkingJScrollPane(this, ledgerView.component)
  val dotModel = new DotModel()
  BoxesScrollBarUI.applyTo(component, new DotModel(), dotModel, false, true)
  val table = ledgerView.component

  val view = View {
    val scale = (ledger().recordCount).asInstanceOf[Double]
    val is = indices()

    addUpdate{
      val viewIndices = is.map(i => indexToView(i)).filter(i=>i>=0).toList.sorted
      val viewRuns = encodeDirect(viewIndices)
      val viewRunsScaled = viewRuns.map(run => (run._1/scale, (run._1 + run._2)/scale))

      dotModel.positions = viewRunsScaled.toSet
    }
  }

  //Convert a sorted list of ints to a list of starts and lengths of runs of ints
  def encodeDirect(list:List[Int]) : List[(Int,Int)] = {

    def encode(result:List[(Int,Int)], n:Int, rl:List[Int]) : List[(Int,Int)] = {
      rl match {
        //If the first two elements are a run, continue any current run
        case head::next::tail if head == next-1 => encode(result,n+1,next::tail)
        //Ending a run, with more to encode
        case head::next::tail => encode(result:::List((head-n, n+1)),0,next::tail)
        //Ending a run and no more to encode
        case head::Nil => result:::List((head-n, n+1))
        //Single stage of encoding an empty list
        case Nil => result
      }
    }

    encode(Nil,0,list)
  }

  def indexToView(i:Int):Int = {
    try {
      table.convertRowIndexToView(i)
    } catch {
      //If index is out of bounds, treat as no selection
      case _ => -1
    }
  }
}

class LedgerView(v:LedgerVar) extends SwingView{

  class LedgerTableModel extends AbstractTableModel {
    override def getColumnClass(columnIndex:Int) = v().fieldClass(columnIndex)
    override def getColumnName(columnIndex:Int) = v().fieldName(columnIndex)
    override def getColumnCount() = v().fieldCount
    override def getRowCount() = v().recordCount
    override def getValueAt(rowIndex:Int, columnIndex:Int) = v().apply(rowIndex, columnIndex).asInstanceOf[AnyRef]
    override def isCellEditable(rowIndex:Int, columnIndex:Int) = v().editable(rowIndex, columnIndex)
    override def setValueAt(aValue:Object, rowIndex:Int, columnIndex:Int) = v()=v().update(rowIndex, columnIndex, aValue)
  }

  val model = new LedgerTableModel()

  val component = new LinkingJTable(this, model)

  val view = View {
    //Read the entire ledger - not neat, but means we receive updates. Might be best just to cache it all
    //but probably doesn't make much difference
    val ledger = v()
    for (f <- 0 until ledger.fieldCount) {
      ledger.fieldName(f)
      ledger.fieldClass(f)
    }
    for (r <- 0 until ledger.recordCount) {
      for (f <- 0 until ledger.fieldCount) {
        ledger(r, f)
        ledger.editable(r, f)
      }
    }

    //First look for changes in current ledger
    var rowCountChanged = false
    var columnsChanged = false
    for {
      queue <- v.changes
      (_, change) <- queue
    } {
      rowCountChanged |= change.rowCountChanged
      columnsChanged |= change.columnsChanged
    }

    //Completely new ledger means row count may have changed
    if (!v.changes.isEmpty) {
      rowCountChanged = true
    }

    val rowCount = ledger.recordCount()

    //This will be called from Swing Thread
    addUpdate {
      if (columnsChanged) {
        model.fireTableStructureChanged()
      } else if (rowCountChanged) {
        model.fireTableDataChanged()
      } else {
        model.fireTableRowsUpdated(0, rowCount-1)
      }
    }
  }

  def defaultEditor(columnClass:Class[_]) = component.getDefaultEditor(columnClass)
  def defaultRenderer(columnClass:Class[_]) = component.getDefaultRenderer(columnClass)

  def defaultEditor(columnClass:Class[_], editor:TableCellEditor) {
    component.setDefaultEditor(columnClass, editor);
  }

  def defaultRenderer(columnClass:Class[_], renderer:TableCellRenderer) {
    component.setDefaultRenderer(columnClass, renderer);
  }

  def rowHeight = component.getRowHeight
  def rowHeight_=(rowHeight:Int) = component.setRowHeight(rowHeight)

  def removeHeader = component.setTableHeader(null)
}

class LinkingJTable(val sv:SwingView, m:TableModel) extends JTable(m) {

  getTableHeader().setDefaultRenderer(new BoxesTableCellHeaderRenderer())

  val defaultRenderer = new BoxesTableCellRenderer()

  //Apologies for null, super constructor calls lots of
  //methods, leading to use of responding before it can be
  //initialised. This is why I hate subclassing, but necessary
  //to make a JTable. We initialise responding wherever it first
  //happens to get used.
  private var responding:AtomicBoolean = null

  setDefaultRenderer(classOf[Boolean],  BooleanCellRenderer.opaque)
  setDefaultRenderer(classOf[Char],     defaultRenderer)
  setDefaultRenderer(classOf[String],     defaultRenderer)

  //We want to use implicits, so we can't use a list of classes, unfortunately
  setDefaultEditor(classOf[Byte],       NumberCellEditor(classOf[Byte]))
  setDefaultEditor(classOf[Double],     NumberCellEditor(classOf[Double]))
  setDefaultEditor(classOf[Long],       NumberCellEditor(classOf[Long]))
  setDefaultEditor(classOf[Float],      NumberCellEditor(classOf[Float]))
  setDefaultEditor(classOf[Int],        NumberCellEditor(classOf[Int]))
  setDefaultEditor(classOf[Short],      NumberCellEditor(classOf[Short]))
  setDefaultEditor(classOf[BigInt],     NumberCellEditor(classOf[BigInt]))
  setDefaultEditor(classOf[BigDecimal], NumberCellEditor(classOf[BigDecimal]))

  setDefaultRenderer(classOf[Byte],       NumberCellRenderer(classOf[Byte]))
  setDefaultRenderer(classOf[Double],     NumberCellRenderer(classOf[Double]))
  setDefaultRenderer(classOf[Long],       NumberCellRenderer(classOf[Long]))
  setDefaultRenderer(classOf[Float],      NumberCellRenderer(classOf[Float]))
  setDefaultRenderer(classOf[Int],        NumberCellRenderer(classOf[Int]))
  setDefaultRenderer(classOf[Short],      NumberCellRenderer(classOf[Short]))
  setDefaultRenderer(classOf[BigInt],     NumberCellRenderer(classOf[BigInt]))
  setDefaultRenderer(classOf[BigDecimal], NumberCellRenderer(classOf[BigDecimal]))


  setDefaultEditor(classOf[String],     SelectingTextCellEditor())
  setDefaultEditor(classOf[Boolean],    boxes.swing.BooleanCellEditor())

  //TODO add default editor/renderer for Color

  setRowHeight(24)

  //Workarounds for when not using Nimbus
//  setShowGrid(false)
//  setIntercellSpacing(new Dimension(0, 0))
//  setShowHorizontalLines(false)
//  setShowVerticalLines(false)

  //See Java bug 4709394
  putClientProperty("terminateEditOnFocusLost", true);

  //Workaround for bug 4330950, stops editing before starting to move column
  override def columnMoved(e:TableColumnModelEvent) {
      if (isEditing()) cellEditor.stopCellEditing()
      super.columnMoved(e);
  }

  //Workaround for bug 4330950, stops editing before starting to change column
  override def columnMarginChanged(e:ChangeEvent) {
      if (isEditing()) cellEditor.stopCellEditing()
      super.columnMarginChanged(e);
  }

  override def tableChanged(e:TableModelEvent) {
    //See note on declaration of responding
    if (responding == null) {
       responding = new AtomicBoolean(false);
    }
    responding.set(true);
    super.tableChanged(e);
    responding.set(false);
  }

  def isRespondingToChange = {
    //See note on declaration of responding
    if (responding == null) {
       responding = new AtomicBoolean(false);
    }
    responding.get
  }

}