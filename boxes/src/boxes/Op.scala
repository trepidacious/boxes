package boxes

trait Op {
  def apply()
  def canApply:RefGeneral[Boolean, _]
}