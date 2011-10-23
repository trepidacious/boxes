package boxes.swing

import com.jgoodies.forms.builder.DefaultFormBuilder
import com.jgoodies.forms.layout.{ConstantSize, CellConstraints, FormLayout}
import java.awt.{Dimension}
import boxes.{BoxesScrollPane, Label, SwingView}
import javax.swing.{JLabel, SwingConstants, JComponent}

object SheetBuilder {
  def apply() = new SheetBuilder()
}

class SheetBuilder {

  val layout = new FormLayout("4px, 0px, right:100px:grow(0.25), 6px, fill:100px:grow(0.75), 8px", "")
  val cc = new CellConstraints()
  val builder = new DefaultFormBuilder(layout)

  builder.setLineGapSize(new ConstantSize(2, ConstantSize.PIXEL))

  def separator(text:String) = {
    val label = HeaderLabel(text)
    builder.append(label, 6)
    this
  }

  def blankTop() = {
    val label = new JLabel("")
    label.setPreferredSize(new Dimension(10, 9))
    builder.append(label, 6)
    this
  }

  def component(label:String, component:JComponent, grow:Boolean = false) = {
    if (grow) {
      builder.appendRow("fill:pref:grow")
    } else {
      builder.appendRow("top:pref")
    }
    builder.append(new JLabel(""))    //TODO get rid of this - nextColumn just doesn't seem to work
    val l = Label(label)
    l.setVerticalAlignment(SwingConstants.TOP)
    builder.append(l)
    builder.append(component)
    builder.nextLine();
    builder.appendRow(builder.getLineGapSpec());
    builder.nextLine();
    this
  }

  def view(label:String, view:SwingView, grow:Boolean = false) = component(label, view.component, grow)

  def panel = {
    //TODO find better way to get bottom gap
    val label = new JLabel(" ")
    label.setPreferredSize(new Dimension(10, 5))
    builder.append(label, 6)
    builder.getPanel
  }

  def scrollingPanel = BoxesScrollPane(panel)
}

