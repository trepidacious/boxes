package boxes

object View {
  def apply(view: => Unit) = {
    val r = new Reaction {
      def respond = {
        view
        (()=>())
      }

      def isView = true
      def react {respond.apply()}
    }
    Box.registerReaction(r)
    r
  }
}

class View {

}

object BooleanControlType extends Enumeration {
   type BooleanControlType = Value
   val CHECKBOX, TOGGLEBUTTON, TOOLBARBUTTON, SLIDECHECK, RADIO, TAB = Value
}
import BooleanControlType._