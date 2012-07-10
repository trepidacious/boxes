package boxes.jfx

import java.math.BigDecimal
import java.text.NumberFormat
import javafx.beans.binding.NumberBinding
import javafx.beans.property.ObjectProperty
import javafx.beans.property.SimpleObjectProperty
import javafx.event.ActionEvent
import javafx.event.EventHandler
import javafx.geometry.Pos
import javafx.scene.control.Button
import javafx.scene.input.KeyCode
import javafx.scene.input.KeyEvent
import javafx.scene.layout.HBox
import javafx.scene.layout.StackPane
import javafx.scene.layout.VBox
import javafx.scene.shape.LineTo
import javafx.scene.shape.MoveTo
import javafx.scene.shape.Path
import javax.swing.JSpinner
import javafx.scene.layout.HBox
import boxes.util.NumericClass
import boxes.util.GConverter
import boxes.VarBox
import boxes.util.Sequence
import JFXImplicits._
import boxes.View
import javafx.scene.control.TextField

object NumberOptionSpinnerView {
    val ARROW = "NumberSpinnerArrow"
    val NUMBER_FIELD = "NumberField"
    val NUMBER_SPINNER = "NumberSpinner"
    val SPINNER_BUTTON_UP = "SpinnerButtonUp"
    val SPINNER_BUTTON_DOWN = "SpinnerButtonDown"
    val BUTTONS_BOX = "ButtonsBox"
    val ARROW_SIZE = 4
}

class NumberOptionSpinnerView[G, N](v:VarBox[G,_], c:GConverter[G, N], s:Sequence[N], n:Numeric[N], nc:NumericClass[N]) extends JFXView {

  val textField = new TextField();
  {
    textField.setOnAction((event: ActionEvent) => commit)
    textField.focusedProperty.addListener((focused: java.lang.Boolean) => if (focused) display(v()) else commit)
    textField.setId(NumberOptionSpinnerView.NUMBER_FIELD)
    textField.addEventFilter(KeyEvent.KEY_PRESSED, (keyEvent: KeyEvent) => {
        if (keyEvent.getCode() == KeyCode.DOWN) {
            decrement()
            keyEvent.consume()
        }
        if (keyEvent.getCode() == KeyCode.UP) {
            increment()
            keyEvent.consume()
        }
    })

  }

  val view = View{
    //Store the value for later use on Swing Thread
    val newV = v()
    //This will be called from Swing Thread
    replaceUpdate {display(newV)}
  }

  private def commit = {
    nc.parseOption(textField.getText) match {
      case Some(n) => v() = c.toG(n)
      case None => display(v())
    }
  }

  //Update display if necessary
  private def display(s:G) {
    val (e, t) = c.toOption(s).map((n: N) => (true, nc.format(n))).getOrElse(false, "")
    node.setDisable(!e)
    if (!textField.getText.equals(t)) textField.setText(t)
  }
  
  private def increment(){
    
    s.next(t)
  }
  private def decrement(){}
  
  val node = new HBox() {
    //Reference view, so that as long as this node is retained, the view will be retained
    val view = NumberOptionSpinnerView.this
    
    setId(NumberOptionSpinnerView.NUMBER_SPINNER)

    // the spinner buttons scale with the textfield size
    // TODO: the following approach leads to the desired result, but it is 
    // not fully understood why and obviously it is not quite elegant
    val buttonHeight = textField.heightProperty().subtract(3).divide(2)
    // give unused space in the buttons VBox to the incrementBUtton
    val spacing = textField.heightProperty().subtract(2).subtract(buttonHeight.multiply(2))

        // inc/dec buttons
    val buttons = new VBox()
    buttons.setId(NumberOptionSpinnerView.BUTTONS_BOX)
    val incrementButton = new Button()
    incrementButton.setId(NumberOptionSpinnerView.SPINNER_BUTTON_UP)
    incrementButton.prefWidthProperty().bind(textField.heightProperty())
    incrementButton.minWidthProperty().bind(textField.heightProperty())
    incrementButton.maxHeightProperty().bind(buttonHeight.add(spacing))
    incrementButton.prefHeightProperty().bind(buttonHeight.add(spacing))
    incrementButton.minHeightProperty().bind(buttonHeight.add(spacing))
    incrementButton.setFocusTraversable(false)
    incrementButton.setOnAction((ae: ActionEvent) => {
        increment()
        ae.consume()
    })


    val decrementButton = new Button()
    decrementButton.setId(NumberOptionSpinnerView.SPINNER_BUTTON_DOWN)
    decrementButton.prefWidthProperty().bind(textField.heightProperty())
    decrementButton.minWidthProperty().bind(textField.heightProperty())
    decrementButton.maxHeightProperty().bind(buttonHeight)
    decrementButton.prefHeightProperty().bind(buttonHeight)
    decrementButton.minHeightProperty().bind(buttonHeight)

    decrementButton.setFocusTraversable(false)
    decrementButton.setOnAction((ae: ActionEvent) => {
        decrement()
        ae.consume()
    })
    
    buttons.getChildren().addAll(incrementButton, decrementButton)
    getChildren().addAll(textField, buttons)
  }

}