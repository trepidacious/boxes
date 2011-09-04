package boxes.swing

import com.jgoodies.forms.builder.DefaultFormBuilder
import boxes.SwingView
import com.jgoodies.forms.layout.{ConstantSize, CellConstraints, FormLayout}
import javax.swing.border.EmptyBorder
import javax.swing.{JLabel, JComponent}
import com.explodingpixels.swingx.EPPanel
import java.awt.Dimension

object SheetBuilder {
  def apply() = new SheetBuilder()
}

class SheetBuilder {

  val layout = new FormLayout("4px, 0px, right:60px:grow, 6px, fill:60px:grow, 4px", "")
  val cc = new CellConstraints()
  val builder = new DefaultFormBuilder(layout)
  builder.setLineGapSize(new ConstantSize(2, ConstantSize.PIXEL))

  def separator(text:String) = {
    val label = HeaderLabel(text)
    builder.append(label, 6)
    this
  }

  def component(label:String, component:JComponent) = {
    builder.append(new JLabel(""))    //TODO get rid of this - nextColumn just doesn't seem to work
    builder.append(label, component)
    this
  }

  def view(label:String, view:SwingView) = component(label, view.component)

  def panel = {
    //TODO find better way to get bottom gap
    val label = new JLabel(" ")
    label.setPreferredSize(new Dimension(10, 10))
    builder.append(label, 6)
    builder.getPanel
  }

}

