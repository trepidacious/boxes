package boxes
import java.awt.Image

trait Op {
  def apply()
  def canApply:RefGeneral[Boolean, _]
}

trait ViewOp extends Op {
  def icon:RefGeneral[Option[Image], _]
  def label:RefGeneral[String, _]
}

class OpDefault(action: => Unit, val canApply:RefGeneral[Boolean, _]) extends Op {
  def apply() = action
}

object Op {
  def apply(action: => Unit, canApply:RefGeneral[Boolean, _] = Val(true)) = new OpDefault(action, canApply)
}

class ViewOpDefault(action: => Unit, val canApply:RefGeneral[Boolean, _], val icon:RefGeneral[Option[Image], _], val label:RefGeneral[String, _]) extends Op {
  def apply() = action
}

object ViewOp {
  def apply(action: => Unit, canApply:RefGeneral[Boolean, _] = Val(true), icon:RefGeneral[Option[Image], _] = Val(None), label:RefGeneral[String, _] = Val("")) = new ViewOpDefault(action, canApply, icon, label)
}