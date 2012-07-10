package boxes.jfx

import javafx.beans.value.ChangeListener
import javafx.beans.value.ObservableValue

object JFXImplicits {

  implicit def function2jfxChangeListener[T](op: (T) => Unit) =
    new ChangeListener[T] {
      def changed(observable: ObservableValue[_ <: T], oldValue: T, newValue: T) {
        op(newValue)
      }
    }
  
    /**
   * Generates a new [[javafx.event.EventHandler]] from a simple function that neither receives
   * parameter either return value (just [[scala.Unit]]).
   */
  implicit def function2jfxEventHandler[E <: javafx.event.Event, H <: javafx.event.EventHandler[E]](op: => Unit) =
    new javafx.event.EventHandler[E] {
      def handle(event: E) {
        op
      }
    }

  /**
   * Generates a new [[javafx.event.EventHandler]] from a simple function that neither receives
   * parameter either return value (just [[scala.Unit]]).
   */
  implicit def function2jfxEventHandlerWithParam[E <: javafx.event.Event, H <: javafx.event.EventHandler[E]](op: (E) => Unit) =
    new javafx.event.EventHandler[E] {
      def handle(event: E) {
        op(event)
      }
    }
}