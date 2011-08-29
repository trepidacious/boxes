package boxes

trait Op {
  def apply()
  def canApply:RefGeneral[Boolean, _]
}

class OpDefault(action: => Unit, val canApply:RefGeneral[Boolean, _]) extends Op {
  def apply() = action
}

object Op {
  def apply(action: => Unit, canApply:RefGeneral[Boolean, _] = Val(true)) = new OpDefault(action, canApply)
}