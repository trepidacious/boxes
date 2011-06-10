package boxes

import scala.collection._
import java.awt.event.{FocusEvent, FocusListener, ActionEvent, ActionListener}
import javax.swing._
import border.MatteBorder
import event.{TableModelEvent, ChangeEvent, TableColumnModelEvent}
import javax.swing.JToggleButton.ToggleButtonModel
import math.Numeric
import plaf.basic.BasicButtonUI
import plaf.metal.MetalLookAndFeel
import swing._
import table._
import util._
import java.util.concurrent.atomic.AtomicBoolean
import com.explodingpixels.macwidgets.ITunesTableHeaderRenderer
import com.explodingpixels.widgets.TableHeaderUtils
import java.awt.{Dimension, Color, Component}
import com.explodingpixels.macwidgets.plaf.{UnifiedToolbarButtonUI, ITunesTableUI}

//TODO implement rate-limiting of updates? But then we need to know that views don't rely on all updates being called, just the most recent
//Should be easy enough to do, just make the views store some Atomic style stuff they need to use to update, and fiddle
//with this from View methods, so that when update DOES get called, it clears out any updates that are needed. Only really
//fiddly for tables, which need to remember the size of the change (columns and/or row count) and make sure that this
//is synchronized between View handling and swing updates - a simple lock should do it.
object SwingView {

  val viewToUpdates = new mutable.WeakHashMap[Any, mutable.ListBuffer[() => Unit]]()
  val responder = new CoalescingResponder(respond)
  val lock = new Object()

  def addUpdate(v:Any, update: => Unit) = {
    lock.synchronized{
      viewToUpdates.get(v) match {
        case None => viewToUpdates.put(v, mutable.ListBuffer(() => update))
        case Some(list) => list.append(() => update)
      }
      responder.request
    }
  }

  def replaceUpdate(v:Any, update: => Unit) = {
    lock.synchronized{
      viewToUpdates.put(v, mutable.ListBuffer(() => update))
      responder.request
    }
  }

  private def respond = {
    SwingUtilities.invokeLater(new Runnable() {
      override def run = {
        while(
          popUpdates match {
            case Some(updates) => {
              for {
                update <- updates
              } update.apply
              true
            }
            case None => false
          }
        ) {}
      }
    })
  }

  /**
   * If there are any updates stored in map, get a list of updates
   * for some key, remove them from the map, and return them.
   * If there are no updates left (no keys), then return None
   * This is synchronized, so updates can't be added as they are being
   * retrieved
   */
  private def popUpdates = {
    lock.synchronized{
      val keysIt = viewToUpdates.keysIterator;
      if (keysIt.hasNext) {
        val r = viewToUpdates.remove(keysIt.next)
        if (r == None) throw new RuntimeException("Got None for a key in viewToUpdates")
        r
      } else {
        None
      }
    }
  }

  def nimbus() {
    try {
      for (info <- UIManager.getInstalledLookAndFeels()) {
        if ("Nimbus".equals(info.getName())) {
          UIManager.setLookAndFeel(info.getClassName())
          UIManager.put("nimbusSelectionBackground", new Color(120, 144, 161));
          UIManager.put("Table.alternateRowColor", new Color(240, 240, 240))
          UIManager.put("Table.backgroundColor", new Color(255, 255, 255))
          UIManager.put("Table.selectionForeground", new Color(255, 255, 255))
          UIManager.put("Table.selectionBackground", new Color(120, 144, 161))
          UIManager.put("Table.focusCellHighlightBorder", new MatteBorder(1, 1, 1, 1, new Color(120, 144, 161).darker.darker))
        }
      }
    } catch {
      case _ => {}
    }
  }

  def nimbox() {
    try {
      UIManager.setLookAndFeel( new MetalLookAndFeel() )
      UIManager.put("Table.alternateRowColor", new Color(240, 240, 240))
      UIManager.put("Table.backgroundColor", new Color(255, 255, 255))
      UIManager.put("Table.selectionForeground", new Color(255, 255, 255))
      UIManager.put("Table.selectionBackground", new Color(120, 144, 161))
      UIManager.put("Table.focusCellHighlightBorder", new MatteBorder(1, 1, 1, 1, new Color(120, 144, 161).darker.darker))
    }
    catch {
      case _ => {}
    }
  }

  //TODO add this
//  val iconFactory = new ResourceIconFactory()
}

trait SwingView {
  def component():JComponent

  private[boxes] def addUpdate(update: => Unit) = SwingView.addUpdate(this, update)
  private[boxes] def replaceUpdate(update: => Unit) = SwingView.replaceUpdate(this, update)
}


object LabelView {
  def apply(v:RefGeneral[String,_]) = new LabelOptionView(v, new TConverter[String]).asInstanceOf[SwingView]
}

object LabelOptionView {
  def apply(v:RefGeneral[Option[String],_]) = new LabelOptionView(v, new OptionTConverter[String]).asInstanceOf[SwingView]
}

//TODO use a renderer to customise display
private class LabelOptionView[G](v:RefGeneral[G,_], c:GConverter[G, String]) extends SwingView {

  val component = new LinkingJLabel(this)

  val view = View{
    //Store the value for later use on Swing Thread
    val newV = v()
    //This will be called from Swing Thread
    replaceUpdate {display(newV)}
  }

  //Update display if necessary
  private def display(s:G) {
    val text = c.toOption(s) match {
      case None => ""
      case Some(string) => string
    }
    if (!component.getText.equals(text)) {
      component.setText(text)
    }
  }
}

//Special versions of components that link back to the SwingView using them,
//so that if users only retain the component, they still also retain the SwingView.
class LinkingJLabel(val sv:SwingView) extends JLabel {}

object StringView {
  def apply(v:VarGeneral[String,_], multiline:Boolean = false) = new StringOptionView(v, new TConverter[String], multiline).asInstanceOf[SwingView]
}

object StringOptionView {
  def apply(v:VarGeneral[Option[String],_], multiline:Boolean = false) = new StringOptionView(v, new OptionTConverter[String], multiline).asInstanceOf[SwingView]
}

private class StringOptionView[G](v:VarGeneral[G,_], c:GConverter[G, String], multiline:Boolean) extends SwingView {

  val text = if (multiline) new JTextArea(10, 20) else new LinkingJTextField(this)
  val component = if (multiline) new LinkingJScrollPane(this, text) else text

  {
    if (!multiline) {
      text.asInstanceOf[JTextField].addActionListener(new ActionListener() {
				override def actionPerformed(e:ActionEvent) = commit
			})
    }

    text.addFocusListener(new FocusListener() {
      override def focusLost(e:FocusEvent) = commit
      override def focusGained(e:FocusEvent) = display(v())
    })
  }

  val view = View{
    //Store the value for later use on Swing Thread
    val newV = v()
    //This will be called from Swing Thread
    replaceUpdate {display(newV)}
  }

  private def commit = {
    v() = c.toG(text.getText)
  }

  //Update display if necessary
  private def display(s:G) {
    val enableAndText = c.toOption(s) match {
      case None => (false, "")
      case Some(string) => (true, string)
    }
    text.setEnabled(enableAndText._1)
    if (!text.getText.equals(enableAndText._2)) {
      text.setText(enableAndText._2)
    }
  }
}

//Special versions of components that link back to the SwingView using them,
//so that if users only retain the component, they still also retain the SwingView.
class LinkingJScrollPane(val sv:SwingView, contents:Component) extends JScrollPane(contents) {}
class LinkingJTextField(val sv:SwingView) extends JTextField {}


object BooleanView {
  def apply(v:VarGeneral[Boolean,_], n:RefGeneral[String,_], button:Boolean = false) = new BooleanOptionView(v, n, new TConverter[Boolean], button).asInstanceOf[SwingView]
}

object BooleanOptionView {
  def apply(v:VarGeneral[Option[Boolean],_], n:RefGeneral[String,_], button:Boolean = false) = new BooleanOptionView(v, n, new OptionTConverter[Boolean], button).asInstanceOf[SwingView]
}

private class BooleanOptionView[G](v:VarGeneral[G,_], n:RefGeneral[String,_], c:GConverter[G, Boolean], button:Boolean) extends SwingView {

  val component = if (!button) new LinkingJCheckBox(this) else new LinkingJToggleButton(this)
  private val model = new AutoButtonModel()

  {
    component.setModel(model)
    component.addActionListener(new ActionListener(){
      //On action, toggle value if it is not None
      override def actionPerformed(e:ActionEvent) = {
        c.toOption(v()) match {
          case None => None
          case Some(b) => v() = c.toG(!b)
        }
      }
    })
  }

  val view = View{
    //Store the values for later use on Swing Thread
    val newV = v()
    val newN = n()
    //This will be called from Swing Thread
    replaceUpdate { display(newV, newN) }
  }

  //Update display if necessary
  private def display(newV:G, newN:String) {
    c.toOption(newV) match {
      case None => {
        model.enabled = false
        model.selected = false
      }
      case Some(b) => {
        model.enabled = true
        model.selected = b
      }
    }
    model.fire
    if (newN != component.getText) {
      component.setText(newN)
    }
  }

  private class AutoButtonModel extends ToggleButtonModel {
    var enabled = true
    var selected = true
    def fire() = fireStateChanged()
    override def isSelected = selected
    override def isEnabled = enabled
  }

}

class LinkingJCheckBox(val sv:SwingView) extends JCheckBox {}
class LinkingJToggleButton(val sv:SwingView) extends JToggleButton {}



object RangeView {
  def apply(v:VarGeneral[Int,_], min:Int, max:Int, progress:Boolean = false) = new RangeOptionView(v, min, max, new TConverter[Int], progress).asInstanceOf[SwingView]
}

object RangeOptionView {
  def apply(v:VarGeneral[Option[Int],_], min:Int, max:Int, progress:Boolean = false) = new RangeOptionView(v, min, max, new OptionTConverter[Int], progress).asInstanceOf[SwingView]
}

private class RangeOptionView[G](v:VarGeneral[G,_], min:Int, max:Int, c:GConverter[G, Int], progress:Boolean) extends SwingView {

  private val model = new AutoBoundedRangeModel(min, max)
  val component = if (!progress) new LinkingJSlider(this, model) else new LinkingJProgressBar(this, model)

  val view = View{
    //Store the values for later use on Swing Thread
    val newV = v()
    //This will be called from Swing Thread
    replaceUpdate {
      c.toOption(newV) match {
        case None => {
          component.setEnabled(false)
          model.fireNewValue(model.getMinimum)
        }
        case Some(i) => {
          component.setEnabled(true)
          model.fireNewValue(i)
        }
      }
    }
  }

  private class AutoBoundedRangeModel(min:Int, max:Int) extends DefaultBoundedRangeModel(min, 0, min, max) {

    private var currentValue = 0

		def fireNewValue(i:Int) = {
      //If necessary, extend range to cover value we have seen
      if (i < getMinimum) setMinimum(i)
      if (i > getMaximum) setMaximum(i)
      currentValue = i

			fireStateChanged
		}

		override def getValue = currentValue

		override def getExtent = 0

		override def setValue(n:Int) = currentValue = n

		override def setValueIsAdjusting(b:Boolean) = {
			super.setValueIsAdjusting(b)
      c.toOption(v()) match {
        case None => None
        case Some(_) => v() = c.toG(currentValue)
      }
		}

	}

}

class LinkingJSlider(val sv:SwingView, brm:BoundedRangeModel) extends JSlider(brm) {}
class LinkingJProgressBar(val sv:SwingView, brm:BoundedRangeModel) extends JProgressBar(brm) {}

object NumberView {
  def apply[N](v:VarGeneral[N,_], s:Sequence[N] = LogStep(10))(implicit n:Numeric[N], nc:NumericClass[N]) = new NumberOptionView(v, s, new TConverter[N], n, nc).asInstanceOf[SwingView]
}

object NumberOptionView {
  def apply[N](v:VarGeneral[Option[N],_], s:Sequence[N] = LogStep(10))(implicit n:Numeric[N], nc:NumericClass[N]) = new NumberOptionView(v, s, new OptionTConverter[N], n, nc).asInstanceOf[SwingView]
}

private class NumberOptionView[G, N](v:VarGeneral[G,_], s:Sequence[N], c:GConverter[G, N], n:Numeric[N], nc:NumericClass[N]) extends SwingView {

  private val model = new AutoSpinnerModel()
  val component = new LinkingJSpinner(this, model)

  val view = View{
    //Store the values for later use on Swing Thread
    val newV = v()
    //This will be called from Swing Thread
    replaceUpdate {
      c.toOption(newV) match {
        case None => {
          component.setEnabled(false)
          model.fireNewValue(n.zero)
        }
        case Some(someNewV) => {
          component.setEnabled(true)
          model.fireNewValue(someNewV)
        }
      }
    }
  }

  private class AutoSpinnerModel extends SpinnerNumberModel {
		private var firing = false
    var currentValue = n.zero

		def fireNewValue(newValue:N) = {
      currentValue = newValue

      //TODO - why DOES fireStateChanged end up calling setValue? can we stop it
      //and avoid the need for firing variable?
			firing = true
			fireStateChanged
			firing = false
		}

    //These three are nasty - but SpinnerNumberModel expects an Object, and we
    //stupidly have a much nicer instance of N
		override def getNextValue = s.next(currentValue).asInstanceOf[Object]
		override def getPreviousValue = s.previous(currentValue).asInstanceOf[Object]
    override def getValue = currentValue.asInstanceOf[Object]

		override def setValue(spinnerValue:Object) {
      //Don't respond to our own changes, or incorrect classes
			if (!firing && nc.javaWrapperClass.isAssignableFrom(spinnerValue.getClass)) {
					v() = c.toG(spinnerValue.asInstanceOf[N]);
			}
		}
	}

}

class LinkingJSpinner(val sv:SwingView, m:SpinnerModel) extends JSpinner(m) {}



object LedgerView {
  def apply(v:RefGeneral[_<:Ledger,_], sorting:Boolean = false) = {
    val lv = new LedgerView(v)
    if (sorting) lv.component.setAutoCreateRowSorter(true)
    lv
  }

  def singleSelection(v:RefGeneral[_<:Ledger,_], i:VarGeneral[Option[Int], _], sorting:Boolean = false) = {
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
  def multiSelection(v:RefGeneral[_<:Ledger,_], i:VarGeneral[immutable.Set[Int], _], sorting:Boolean = false) = {
    val lv = new LedgerView(v)
    lv.component.setSelectionModel(new ListSelectionIndicesModel(i, !lv.component.isRespondingToChange, lv.component))
    if (sorting) lv.component.setAutoCreateRowSorter(true)
    lv
  }

  def multiSelectionScroll(v:RefGeneral[_<:Ledger,_], i:VarGeneral[immutable.Set[Int], _], sorting:Boolean = false) = {
    val lv = new LedgerView(v)
    lv.component.setSelectionModel(new ListSelectionIndicesModel(i, !lv.component.isRespondingToChange, lv.component))
    val lsv = new LedgerScrollView(lv, v, i)
    if (sorting) lv.component.setAutoCreateRowSorter(true)
    lsv
  }

}

class LedgerScrollView(val ledgerView:LedgerView, val ledger:RefGeneral[_<:Ledger,_], val indices:VarGeneral[immutable.Set[Int], _]) extends SwingView {
  val component = new LinkingJScrollPane(this, ledgerView.component)
  val dotModel = new DotModel
  BoxesScrollBarUI.applyTo(component, new DotModel, dotModel, false, true)
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

object BoxesScrollPane {
  def apply(component:JComponent) = {
    val scroll = new JScrollPane(component)
    BoxesScrollBarUI.applyTo(scroll)
    scroll
  }
  def horizontal(component:JComponent) = {
    val scroll = new JScrollPane(component)
    BoxesScrollBarUI.applyTo(scroll, new DotModel, new DotModel, true, false)
    scroll
  }
  def vertical(component:JComponent) = {
    val scroll = new JScrollPane(component)
    BoxesScrollBarUI.applyTo(scroll, new DotModel, new DotModel, false, true)
    scroll
  }
}

class LedgerView(v:RefGeneral[_<:Ledger,_]) extends SwingView{

  class LedgerTableModel extends AbstractTableModel {
    override def getColumnClass(columnIndex:Int) = v().fieldClass(columnIndex)
    override def getColumnName(columnIndex:Int) = v().fieldName(columnIndex)
    override def getColumnCount() = v().fieldCount
    override def getRowCount() = v().recordCount
    override def getValueAt(rowIndex:Int, columnIndex:Int) = v().apply(rowIndex, columnIndex).asInstanceOf[AnyRef]
    override def isCellEditable(rowIndex:Int, columnIndex:Int) = v().editable(rowIndex, columnIndex)
    override def setValueAt(aValue:Object, rowIndex:Int, columnIndex:Int) = v().update(rowIndex, columnIndex, aValue)
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
      queue <- ledger.changes
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
  //to make a JTable.
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

  def isRespondingToChange() = {
    //See note on declaration of responding
    if (responding == null) {
       responding = new AtomicBoolean(false);
    }
    responding.get
  }

}
