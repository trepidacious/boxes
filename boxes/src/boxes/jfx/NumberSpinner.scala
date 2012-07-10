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
  
  private def increment(){}
  private def decrement(){}
  
  val component = new HBox() {
    setId(NumberOptionSpinnerView.NUMBER_SPINNER)
  }
    
  private final Button incrementButton
  private final Button decrementButton
  private final NumberBinding buttonHeight
  private final NumberBinding spacing


        // Painting the up and down arrows
        Path arrowUp = new Path()
        arrowUp.setId(ARROW)
        arrowUp.getElements().addAll(new MoveTo(-ARROW_SIZE, 0), new LineTo(ARROW_SIZE, 0),
                new LineTo(0, -ARROW_SIZE), new LineTo(-ARROW_SIZE, 0))
        // mouse clicks should be forwarded to the underlying button
        arrowUp.setMouseTransparent(true)

        Path arrowDown = new Path()
        arrowDown.setId(ARROW)
        arrowDown.getElements().addAll(new MoveTo(-ARROW_SIZE, 0), new LineTo(ARROW_SIZE, 0),
                new LineTo(0, ARROW_SIZE), new LineTo(-ARROW_SIZE, 0))
        arrowDown.setMouseTransparent(true)

        // the spinner buttons scale with the textfield size
        // TODO: the following approach leads to the desired result, but it is 
        // not fully understood why and obviously it is not quite elegant
        buttonHeight = numberField.heightProperty().subtract(3).divide(2)
        // give unused space in the buttons VBox to the incrementBUtton
        spacing = numberField.heightProperty().subtract(2).subtract(buttonHeight.multiply(2))

        // inc/dec buttons
        VBox buttons = new VBox()
        buttons.setId(BUTTONS_BOX)
        incrementButton = new Button()
        incrementButton.setId(SPINNER_BUTTON_UP)
        incrementButton.prefWidthProperty().bind(numberField.heightProperty())
        incrementButton.minWidthProperty().bind(numberField.heightProperty())
        incrementButton.maxHeightProperty().bind(buttonHeight.add(spacing))
        incrementButton.prefHeightProperty().bind(buttonHeight.add(spacing))
        incrementButton.minHeightProperty().bind(buttonHeight.add(spacing))
        incrementButton.setFocusTraversable(false)
        incrementButton.setOnAction(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent ae) {
                increment()
                ae.consume()
            }
        })

        // Paint arrow path on button using a StackPane
        StackPane incPane = new StackPane()
        incPane.getChildren().addAll(incrementButton, arrowUp)
        incPane.setAlignment(Pos.CENTER)

        decrementButton = new Button()
        decrementButton.setId(SPINNER_BUTTON_DOWN)
        decrementButton.prefWidthProperty().bind(numberField.heightProperty())
        decrementButton.minWidthProperty().bind(numberField.heightProperty())
        decrementButton.maxHeightProperty().bind(buttonHeight)
        decrementButton.prefHeightProperty().bind(buttonHeight)
        decrementButton.minHeightProperty().bind(buttonHeight)

        decrementButton.setFocusTraversable(false)
        decrementButton.setOnAction(new EventHandler<ActionEvent>() {

            @Override
            public void handle(ActionEvent ae) {
                decrement()
                ae.consume()
            }
        })

        StackPane decPane = new StackPane()
        decPane.getChildren().addAll(decrementButton, arrowDown)
        decPane.setAlignment(Pos.CENTER)

        buttons.getChildren().addAll(incPane, decPane)
        this.getChildren().addAll(numberField, buttons)
    }

    /**
     * increment number value by stepWidth
     */
    private void increment() {
        BigDecimal value = numberField.getNumber()
        value = value.add(stepWitdhProperty.get())
        numberField.setNumber(value)
    }

    /**
     * decrement number value by stepWidth
     */
    private void decrement() {
        BigDecimal value = numberField.getNumber()
        value = value.subtract(stepWitdhProperty.get())
        numberField.setNumber(value)
    }

    public final void setNumber(BigDecimal value) {
        numberField.setNumber(value)
    }

    public ObjectProperty<BigDecimal> numberProperty() {
        return numberField.numberProperty()
    }

    public final BigDecimal getNumber() {
        return numberField.getNumber()
    }

    // debugging layout bounds
    public void dumpSizes() {
        System.out.println("numberField (layout)=" + numberField.getLayoutBounds())
        System.out.println("buttonInc (layout)=" + incrementButton.getLayoutBounds())
        System.out.println("buttonDec (layout)=" + decrementButton.getLayoutBounds())
        System.out.println("binding=" + buttonHeight.toString())
        System.out.println("spacing=" + spacing.toString())
    }
}