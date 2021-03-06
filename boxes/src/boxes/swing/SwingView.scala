package boxes.swing

import scala.collection._
import java.awt.event.{FocusEvent, FocusListener, ActionEvent, ActionListener}
import javax.swing.JToggleButton.ToggleButtonModel
import math.Numeric
import java.util.concurrent.atomic.AtomicBoolean
import com.explodingpixels.painter.MacWidgetsPainter
import java.awt.geom.Arc2D
import javax.swing.JSpinner.DefaultEditor
import java.text.ParseException
import javax.swing.plaf.metal.MetalLookAndFeel
import javax.swing.border.{EmptyBorder, MatteBorder}
import javax.swing.table.{TableModel, TableCellRenderer, TableCellEditor, AbstractTableModel}
import javax.swing.event.{TableModelEvent, ChangeEvent, TableColumnModelEvent}
import boxes.util.{NumericClass, GConverter, OptionTConverter, TConverter, CoalescingResponder, Sequence}
import javax.swing.{ScrollPaneConstants, JTable, JSpinner, SpinnerModel, SpinnerNumberModel, JProgressBar, JSlider, BoundedRangeModel, DefaultBoundedRangeModel, SwingConstants, Icon, JTextArea, JScrollPane, JTextField, JLabel, JComponent, ImageIcon, UIManager, SwingUtilities}
import com.explodingpixels.swingx.EPPanel
import java.awt.{BorderLayout, AlphaComposite, Dimension, BasicStroke, RenderingHints, Graphics2D, Color, Component}
import boxes.swing.icons.IconFactory
import boxes._
import java.text.DecimalFormat
import boxes.BooleanControlType._

object SwingViewImplicits {
  implicit def varToBooleanView(v : Var[Boolean]) = BooleanView(v)
  implicit def varToNumberView[N](v : Var[N])(implicit n:Numeric[N], nc:NumericClass[N]) = NumberView[N](v)
  implicit def varToStringView(v : Var[String]) = StringView(v)

  implicit def optionVarToBooleanView(v : Var[Option[Boolean]]) = BooleanOptionView(v)
  implicit def optionVarToNumberView[N](v : Var[Option[N]])(implicit n:Numeric[N], nc:NumericClass[N]) = NumberOptionView[N](v)
  implicit def optionVarToStringView(v : Var[Option[String]]) = StringOptionView(v)
}

object SwingView {

  val defaultDecimalFormat = new DecimalFormat("0.#")
  val viewToUpdates = new mutable.WeakHashMap[Any, mutable.ArrayBuffer[() => Unit]]()
  //TODO consider using smaller intervals than default. 5, 10 makes graph use very smooth on a good PC. 
  val responder = new CoalescingResponder(respond)
  val lock = new Object()
  
  def format(d:Double) = defaultDecimalFormat.format(d)

  def icon(name:String) = IconFactory.icon(name)

  val wrench = icon("Wrench")

  def addUpdate(v:Any, update: => Unit) = {
    lock.synchronized{
      viewToUpdates.get(v) match {
        case None => viewToUpdates.put(v, mutable.ArrayBuffer(() => update))
        case Some(list) => list.append(() => update)
      }
      responder.request
    }
  }

  def replaceUpdate(v:Any, update: => Unit) = {
    lock.synchronized{
      viewToUpdates.put(v, mutable.ArrayBuffer(() => update))
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
          UIManager.put("control", background)
          UIManager.put("nimbusSelectionBackground", selectionColor);
          UIManager.put("Table.alternateRowColor", alternateBackgroundColor)
          UIManager.put("Table.backgroundColor", selectedTextColor)
          UIManager.put("Table.selectionForeground", selectedTextColor)
          UIManager.put("Table.selectionBackground", selectionColor)
          UIManager.put("Table.focusCellHighlightBorder", new MatteBorder(1, 1, 1, 1, selectionColor.darker.darker))
          UIManager.put("CheckBox.icon", IconFactory.icon("Checkbox"))
        }
      }
    } catch {
      case _ => {}
    }
  }
  
  def swingRun(r : => Unit) {
    SwingUtilities.invokeLater(new Runnable(){
      override def run = r
    })
  }


  def nimbox() {
    try {
      UIManager.setLookAndFeel( new MetalLookAndFeel() )
      UIManager.put("Table.alternateRowColor", alternateBackgroundColor)
      UIManager.put("Table.backgroundColor", selectedTextColor)
      UIManager.put("Table.selectionForeground", selectedTextColor)
      UIManager.put("Table.selectionBackground", selectionColor)
      UIManager.put("Table.focusCellHighlightBorder", new MatteBorder(1, 1, 1, 1, selectionColor.darker.darker))
    }
    catch {
      case _ => {}
    }
  }

  val background = new Color(240, 240, 240)
  val dividingColor = new Color(150, 150, 150)
  val unimportantTextColor = new Color(150, 150, 150)
  val alternateBackgroundColor = new Color(240, 240, 240)
  val selectionColor = new Color(120, 144, 161)
  val selectedTextColor = Color.white
  val textColor = new Color(50, 50, 50)
  val textUnderlightColor = new Color(255, 255, 255, 160)
  val shadedBoxColor = new Color(0,0,0,0.6f)

  def clip(value:Int, min:Int, max:Int) = {
    if (value < min) min
    else if (value > max) max
    else value
  }

  def transparentColor(c:Color, factor:Double) = {
    new Color(	c.getRed,
    						c.getGreen,
    						c.getBlue,
    						clip((c.getAlpha() * factor).asInstanceOf[Int], 0, 255))
  }

  def graphicsForEnabledState(g:Graphics2D, e:Boolean) {
    if (!e) g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.5f))
  }

  //TODO add this
//  val iconFactory = new ResourceIconFactory()
}

trait SwingView {
  def component():JComponent

  private[boxes] def addUpdate(update: => Unit) = SwingView.addUpdate(this, update)
  private[boxes] def replaceUpdate(update: => Unit) = SwingView.replaceUpdate(this, update)
}


object EmbossedLabelView {
  def apply(v:Box[String,_]) = {
    val view = new LabelOptionView(v, new TConverter[String])
    view.component.setUI(new EmbossedLabelUI())
    view.asInstanceOf[SwingView]
  }
}

object EmbossedLabelOptionView {
  def apply(v:Box[Option[String],_]) = {
    val view = new LabelOptionView(v, new OptionTConverter[String]) 
    view.component.setUI(new EmbossedLabelUI())
    view.asInstanceOf[SwingView]
  }
}

object LabelView {
  def apply(v:Box[String,_]) = new LabelOptionView(v, new TConverter[String]).asInstanceOf[SwingView]
}

object LabelOptionView {
  def apply(v:Box[Option[String],_]) = new LabelOptionView(v, new OptionTConverter[String]).asInstanceOf[SwingView]
}

//TODO use a renderer to customise display
private class LabelOptionView[G](v:Box[G,_], c:GConverter[G, String]) extends SwingView {

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
class LinkingJLabel(val sv:SwingView) extends Label {}

object StringView {
  def apply(v:VarBox[String,_], multiline:Boolean = false) = new StringOptionView(v, new TConverter[String], multiline).asInstanceOf[SwingView]
}

object StringOptionView {
  def apply(v:VarBox[Option[String],_], multiline:Boolean = false) = new StringOptionView(v, new OptionTConverter[String], multiline).asInstanceOf[SwingView]
}

private class StringOptionView[G](v:VarBox[G,_], c:GConverter[G, String], multiline:Boolean) extends SwingView {

  val text = if (multiline) new BoxesJTextArea(1, 1) else new LinkingJTextField(this)
  //TODO need a nice scrollable text area with the minimal scrollbars from ledger view, inside the text area.
  val component = if (multiline) new LinkingTextEPPanel(this, new LinkingTextJScrollPane(this, text)) else text

  {
    if (multiline) {
      component.setMinimumSize(new Dimension(50, 100))
      component.setPreferredSize(new Dimension(50, 100))
    } else {
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

class LinkingTextEPPanel(val sv:SwingView, contents:Component) extends EPPanel {
  setBackgroundPainter(new TextComponentPainter())
  setBorder(new EmptyBorder(7,8,4,4))
  setLayout(new BorderLayout())
  add(contents)
}

//Special versions of components that link back to the SwingView using them,
//so that if users only retain the component, they still also retain the SwingView.
class LinkingJScrollPane(val sv:SwingView, contents:Component) extends JScrollPane(contents)

class LinkingTextJScrollPane(val sv:SwingView, contents:Component) extends JScrollPane(contents) {
  BoxesScrollBarUI.applyTo(this, plain = true)
  val lowerRightCorner = new EPPanel()
  lowerRightCorner.setBackgroundPainter(new WhitePainter())
  setCorner(ScrollPaneConstants.LOWER_RIGHT_CORNER, lowerRightCorner)
  setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED)
  setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED)
}


class LinkingJTextField(val sv:SwingView) extends JTextField {
  BoxesTextFieldUI(this)
}

class BoxesJTextArea(r:Int, c:Int) extends JTextArea(r, c) {
  BoxesTextAreaUI(this)
}


object BooleanView {
  def apply(v:VarBox[Boolean,_], n:Box[String,_] = Val(""), controlType:BooleanControlType = SLIDECHECK, icon:Box[Option[Icon], _] = Val(None), toggle:Boolean = true) = new BooleanOptionView(v, n, new TConverter[Boolean], controlType, icon, toggle).asInstanceOf[SwingView]
}

object BooleanOptionView {
  def apply(v:VarBox[Option[Boolean],_], n:Box[String,_] = Val(""), controlType:BooleanControlType = SLIDECHECK, icon:Box[Option[Icon], _] = Val(None), toggle:Boolean = true) = new BooleanOptionView(v, n, new OptionTConverter[Boolean], controlType, icon, toggle).asInstanceOf[SwingView]
}

object Label {
  def apply(text:String, icon:Option[Icon] = None, horizontalAlignment:Int = SwingConstants.LEFT) = new Label(text, icon, horizontalAlignment)
}

class Label(text:String="", icon:Option[Icon] = None, horizontalAlignment:Int = SwingConstants.LEFT) extends JLabel(text, icon.getOrElse(null), horizontalAlignment) {
  {
    setBorder(new EmptyBorder(7, 2, 6, 2))
  }
}

private class BooleanOptionView[G](v:VarBox[G,_], n:Box[String,_], c:GConverter[G, Boolean], controlType:BooleanControlType, icon:Box[Option[Icon], _], toggle:Boolean = true) extends SwingView {

  val component = controlType match {
    case CHECKBOX => new LinkingJCheckBox(this)
    case RADIO => new LinkingJRadioButton(this)
    case TOGGLEBUTTON => new LinkingJToggleButton(this)
    case TOOLBARBUTTON => new LinkingToolbarToggleButton(this)
    case SLIDECHECK => new LinkingSlideCheckButton(this)
    case TAB => new LinkingTabButton(this)
  }

  private val model = new AutoButtonModel()

  {
    component.setModel(model)
    component.addActionListener(new ActionListener(){
      //On action, toggle value if it is not None
      override def actionPerformed(e:ActionEvent) = {
        c.toOption(v()) match {
          case None => None
          case Some(b) => v() = if (toggle) c.toG(!b) else c.toG(true)
        }
      }
    })
  }

  val view = View{
    //Store the values for later use on Swing Thread
    val newV = v()
    val newN = n()
    val newIcon = icon()
    //This will be called from Swing Thread
    replaceUpdate { display(newV, newN, newIcon) }
  }

  //Update display if necessary
  private def display(newV:G, newN:String, newIcon:Option[Icon]) {
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
    val iconOrNull = newIcon.getOrElse(null)
    if (iconOrNull != component.getIcon) {
      component.setIcon(iconOrNull)
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

class LinkingSlideCheckButton(val sv:SwingView) extends SlideCheckButton

class LinkingTabButton(val sv:SwingView) extends TabButton

class LinkingJCheckBox(val sv:SwingView) extends BoxesCheckBox

class LinkingJRadioButton(val sv:SwingView) extends BoxesRadioButton

class LinkingJToggleButton(val sv:SwingView) extends SwingToggleButton

class LinkingToolbarToggleButton(val sv:SwingView) extends SwingBarToggleButton

object RangeView {
  def apply(v:VarBox[Int,_], min:Int, max:Int, progress:Boolean = false) = new RangeOptionView(v, min, max, new TConverter[Int], progress).asInstanceOf[SwingView]
}

object RangeOptionView {
  def apply(v:VarBox[Option[Int],_], min:Int, max:Int, progress:Boolean = false) = new RangeOptionView(v, min, max, new OptionTConverter[Int], progress).asInstanceOf[SwingView]
}

private class RangeOptionView[G](v:VarBox[G,_], min:Int, max:Int, c:GConverter[G, Int], progress:Boolean) extends SwingView {

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

class LinkingJSlider(val sv:SwingView, brm:BoundedRangeModel) extends JSlider(brm) {
  {
    setUI(new BoxesSliderUI(this))
  }
}
class LinkingJProgressBar(val sv:SwingView, brm:BoundedRangeModel) extends JProgressBar(brm) {
  {
    setUI(new BoxesProgressUI())
  }
}

object PiePainter {

  val defaultFill = SwingView.selectionColor //new Color(70, 153, 70)
	val defaultOutline = Color.white// Color(200, 200, 200)

  def apply(border:Int = 3, dotRadius:Int = 2, fill:Color = defaultFill, outline:Color = defaultOutline, justDot:Boolean = false) = new PiePainter(border, dotRadius, fill, outline, justDot)
}

class PiePainter(val border:Int, val dotRadius:Int, val fill:Color, val outline:Color, val justDot:Boolean) {

  def paint(g:Graphics2D, n:Double, w:Int, h:Int, alpha:Double = 1) {

		val oldAA = g.getRenderingHint(RenderingHints.KEY_ANTIALIASING)
    val oldFM = g.getRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS)

    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON)

    val arcAngle = - (n * 360).asInstanceOf[Int]

    val size = math.min(w, h)

		val circleDiameter = size - 2 * (dotRadius + border)

		g.setStroke(new BasicStroke(dotRadius * 2 + 3))
		g.setPaint(SwingView.transparentColor(outline, alpha))
		g.drawOval(border + dotRadius, border + dotRadius, circleDiameter, circleDiameter)

    if (justDot) {
      g.setPaint(SwingView.transparentColor(fill, alpha))
      val x = (size/2 + math.cos(-(arcAngle+90)/360d * math.Pi * 2) * circleDiameter/2 + 1).asInstanceOf[Int]
      val y = (size/2 + math.sin(-(arcAngle+90)/360d * math.Pi * 2) * circleDiameter/2 + 1).asInstanceOf[Int]

      g.fillOval(x - dotRadius, y - dotRadius, dotRadius*2, dotRadius*2)

    } else {
      val arc = new Arc2D.Double(0, 0, size, size, 90, arcAngle, Arc2D.PIE)

      val clip = g.getClip()
      g.setPaint(SwingView.transparentColor(fill, alpha))
      g.setClip(arc)
      g.setStroke(new BasicStroke(dotRadius * 2))
      g.drawOval(border + dotRadius, border + dotRadius, circleDiameter, circleDiameter)
      g.setClip(clip)
    }

		g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, oldAA)
    g.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, oldFM)
  }
}

object PieView {
  def apply(n:Box[Double,_], a:Box[Double,_] = Val(1d)) = new PieOptionView(n, new TConverter[Double], a, new TConverter[Double]).asInstanceOf[SwingView]
}

object PieOptionView {
  def apply(n:Box[Option[Double],_], a:Box[Option[Double],_] = Val(Some(1d))) = new PieOptionView(n, new OptionTConverter[Double], a, new OptionTConverter[Double]).asInstanceOf[SwingView]
}

private class PieOptionView[G, H](n:Box[G,_], c:GConverter[G, Double], a:Box[H,_], d:GConverter[H, Double]) extends SwingView {

  val pie = PiePainter()

  val component:LinkingEPPanel = new LinkingEPPanel(this);

  {
    component.setBackgroundPainter(new MacWidgetsPainter[Component] {
      override def paint(g:Graphics2D, t:Component, w:Int, h:Int) {
        pie.paint(g, nDisplay, w, h, aDisplay)
      }
    })
    component.setPreferredSize(new Dimension(24, 24))
    component.setMinimumSize(new Dimension(24, 24))
  }
  var nDisplay = 0d
  var aDisplay = 0d

  val view = View{
    //Store the values for later use on Swing Thread
    val newN = n()
    val newA = a()
    //This will be called from Swing Thread
    replaceUpdate {
      nDisplay = c.toOption(newN).getOrElse(0d)
      aDisplay = d.toOption(newA).getOrElse(0d)
      component.repaint()
    }
  }
}

class LinkingEPPanel(val sv:SwingView) extends EPPanel {}

object NumberView {
  def apply[N](v:VarBox[N,_])(implicit n:Numeric[N], nc:NumericClass[N]):SwingView = apply(v, nc.defaultSequence)
  def apply[N](v:VarBox[N,_], s:Sequence[N])(implicit n:Numeric[N], nc:NumericClass[N]) = new NumberOptionView(v, s, new TConverter[N], n, nc).asInstanceOf[SwingView]
}

object NumberOptionView {
  def apply[N](v:VarBox[Option[N],_])(implicit n:Numeric[N], nc:NumericClass[N]):SwingView = apply(v, nc.defaultSequence)
  def apply[N](v:VarBox[Option[N],_], s:Sequence[N])(implicit n:Numeric[N], nc:NumericClass[N]):SwingView = new NumberOptionView(v, s, new OptionTConverter[N], n, nc).asInstanceOf[SwingView]
}

private class NumberOptionView[G, N](v:VarBox[G,_], s:Sequence[N], c:GConverter[G, N], n:Numeric[N], nc:NumericClass[N]) extends SwingView {

  private val model = new AutoSpinnerModel()
  val component = new LinkingJSpinner(this, model)

  //If the editor is a default editor, we can work around
  //failure to commit when selecting a menu (and possibly other
  //problems) by monitoring the editor, and committing its edits when it loses
  //focus. This should happen automatically, and sometimes does by some
  //means I have not yet located, but fails when immediately selecting a menu
  //after editing, etc.
  component.getEditor() match {
    case dEditor:DefaultEditor => {
      dEditor.getTextField().addFocusListener(new FocusListener() {

        override def focusLost(e:FocusEvent) {
          try {
            component.commitEdit()
          } catch {
            case e:ParseException => {
              update()
            }
          }
        }

        override def focusGained(e:FocusEvent) {}

      })
    }
  }

  def update() = {
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

  val view = View {
     update()
  }

  //FIXME there is an issue with JSpinner where it can end up on a value like 0.21000000000000002,
  //when we decrease this the text component first commits itself as 0.21, then we decrement and hit 0.20.
  //This generates two changes to the viewed Var, which is slightly annoying. 
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
        currentValue = spinnerValue.asInstanceOf[N]
        v() = c.toG(currentValue)
			}
		}
	}

}

class LinkingJSpinner(val sv:SwingView, m:SpinnerModel) extends JSpinner(m) {
  {
    this.setUI(new BoxesSpinnerUI())
    this.setMinimumSize(new Dimension(60, 28))
    this.setPreferredSize(new Dimension(60, 28))
  }
}

object BoxesScrollPane {
  def apply(component:JComponent, horizontal:Boolean = false, vertical:Boolean = true) = {
    val scroll = new JScrollPane(component)
    BoxesScrollBarUI.applyTo(scroll, horizontal = horizontal, vertical = vertical)
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




