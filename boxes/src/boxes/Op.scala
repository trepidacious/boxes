package boxes
import java.awt.Image

trait Op {
  def apply()
  def canApply:Box[Boolean, _]
}

trait ViewOp extends Op {
  def icon:Box[Option[Image], _]
  def label:Box[String, _]
}

class OpDefault(action: => Unit, val canApply:Box[Boolean, _]) extends Op {
  def apply() = action
}

object Op {
  def apply(action: => Unit, canApply:Box[Boolean, _] = Val(true)) = new OpDefault(action, canApply)
}

class ViewOpDefault(action: => Unit, val canApply:Box[Boolean, _], val icon:Box[Option[Image], _], val label:Box[String, _]) extends Op {
  def apply() = action
}

object ViewOp {
  def apply(action: => Unit, canApply:Box[Boolean, _] = Val(true), icon:Box[Option[Image], _] = Val(None), label:Box[String, _] = Val("")) = new ViewOpDefault(action, canApply, icon, label)
}

