package boxes.javafx

import javafx.beans.value.ChangeListener
import javafx.beans.value.ObservableValue

object JFXImplicits {

  implicit def function2jfxChangeListener[T](op: (T) => Unit) =
    new ChangeListener[T] {
      def changed(observable: ObservableValue[_ <: T], oldValue: T, newValue: T) {
        op(newValue)
      }
    }
  
}