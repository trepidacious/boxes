package boxes.swing

import javax.swing.{AbstractAction, Icon}
import java.awt.event.ActionEvent
import boxes.{SwingView, View, Op}
import boxes.swing.{ListMultiDeleteOp, ListDeleteOp, ListMultiAddOp, ListAddOp}

object SwingOpAction {
  def apply(name:String, icon:Icon, op:Op) = new SwingOpAction(name, icon, op)
}

class SwingOpAction(name:String, icon:Icon, op:Op) extends AbstractAction(name, icon) {
  def actionPerformed(e:ActionEvent) {
    op()
  }
  View {
    val enabled = op.canApply()
    SwingView.replaceUpdate(
      this,
      setEnabled(enabled)
    )
  }
}

object SwingOps {

  def apply(op:Op):SwingOpAction = {
    op match {
      case o:ListAddOp[_] => SwingOpAction("Add", null, op)
      case o:ListMultiAddOp[_] => SwingOpAction("Add", null, op)
      case o:ListDeleteOp[_] => SwingOpAction("Delete", null, op)
      case o:ListMultiDeleteOp[_] => SwingOpAction("Delete", null, op)
      //FIXME use implicits
      case _ => throw new IllegalArgumentException("Unknown op")
    }
  }

}