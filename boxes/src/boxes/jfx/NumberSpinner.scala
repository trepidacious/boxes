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
import boxes.util.TConverter
import boxes.util.OptionTConverter
import javafx.scene.layout.GridPane
import javafx.scene.shape.Rectangle
import javafx.beans.property.DoubleProperty
import javafx.beans.value.ObservableDoubleValue
import javafx.scene.input.MouseEvent
import javafx.scene.control.Tooltip
import javafx.stage.Popup
import javafx.scene.control.Label
import javafx.scene.Parent
import javafx.animation.FadeTransition
import javafx.util.Duration
import javafx.animation.Timeline

object NumberOptionSpinnerView {
  val ARROW = "NumberSpinnerArrow"
  val NUMBER_FIELD = "NumberField"
  val NUMBER_SPINNER = "NumberSpinner"
  val SPINNER_BUTTON_UP = "SpinnerButtonUp"
  val SPINNER_BUTTON_DOWN = "SpinnerButtonDown"
  val SPINNER_BUTTON_UP_ARROW = "SpinnerButtonUpArrow"
  val SPINNER_BUTTON_DOWN_ARROW = "SpinnerButtonDownArrow"
  val BUTTONS_BOX = "ButtonsBox"
  val ARROW_SIZE = 4

  def apply[N](v:VarBox[Option[N],_])(implicit n:Numeric[N], nc:NumericClass[N]):JFXView = apply(v, nc.defaultSequence)
  def apply[N](v:VarBox[Option[N],_], s:Sequence[N])(implicit n:Numeric[N], nc:NumericClass[N]):JFXView = new NumberOptionSpinnerView(v, new OptionTConverter[N], s, n, nc): JFXView
}

object NumberSpinnerView {
  def apply[N](v:VarBox[N,_])(implicit n:Numeric[N], nc:NumericClass[N]):JFXView = apply(v, nc.defaultSequence)
  def apply[N](v:VarBox[N,_], s:Sequence[N])(implicit n:Numeric[N], nc:NumericClass[N]) = new NumberOptionSpinnerView(v, new TConverter[N], s, n, nc): JFXView
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
  
  private def increment() = c.toOption(v()).foreach(n => v() = c.toG(s.next(n)))
  private def decrement() = c.toOption(v()).foreach(n => v() = c.toG(s.previous(n)))
  
  def button(inc: Boolean, height: ObservableDoubleValue, width: ObservableDoubleValue) = {
    val btn = new Button()
    btn.setId(if (inc) NumberOptionSpinnerView.SPINNER_BUTTON_UP else NumberOptionSpinnerView.SPINNER_BUTTON_DOWN)
    btn.minHeightProperty().set(2)
    btn.prefHeightProperty().bind(height)
    btn.minWidthProperty().bind(width)
    btn.maxWidthProperty().bind(width)
    btn.prefWidthProperty().bind(width) 
    
    btn.setFocusTraversable(false)
    btn.setOnAction((ae: ActionEvent) => {
        if (inc) increment() else decrement()
        ae.consume()
    })

    val popup = new Popup
    val stack = new StackPane
    val label = new Label
    stack.setId("PopupStack") //TODO move to JFXView constant
    stack.getChildren.add(label)
    stack.setOpacity(0)
    label.setText("Drag to adjust")
    label.setId("PopupText")
    popup.getContent().add(stack);
    popup.setAutoHide(true);
    popup.setHideOnEscape(true);
    popup.setAutoFix(true);

    val ft = new FadeTransition(Duration.millis(250), stack)

    //A little state machine for dragging
    var drag: Option[(N, Double)] = None
    btn.setOnMousePressed((me: MouseEvent) => {
      drag = c.toOption(v()).map(n => (n, me.getY))
      
      ft.setFromValue(stack.getOpacity);
      ft.setToValue(1.0);
      ft.setDelay(Duration.millis(150))
      ft.playFromStart();
      ft.setOnFinished((ae: ActionEvent)=>{})

      popup.show(btn, me.getScreenX + 15, me.getScreenY-9);
    })
    btn.setOnMouseReleased((me: MouseEvent) => {
      drag = None
      ft.setFromValue(stack.getOpacity);
      ft.setToValue(0.0);
      ft.setDelay(Duration.millis(0))
      ft.playFromStart();
      ft.setOnFinished((ae: ActionEvent)=>popup.hide())
    })
    
    btn.setOnMouseDragged((me: MouseEvent) => {
      drag.foreach{case (n, startY) => {
        val ticks = ((me.getY - startY) / 5).asInstanceOf[Int]
        val newN = if (ticks > 0) {
          Range(0, ticks).foldLeft(n){case (n, _) => s.previous(n)}
        } else if (ticks < 0) {
          Range(0, -ticks).foldLeft(n){case (n, _) => s.next(n)}          
        } else {
          n
        }
        popup.setX(me.getScreenX + 15)
        popup.setY(me.getScreenY-9)
        label.setText("Drag to adjust: " + nc.format(newN))

        v() = c.toG(newN)
      }}
    })

    val arrow = new StackPane()
    //TODO Use a class instead of id
    arrow.setId(if (inc) NumberOptionSpinnerView.SPINNER_BUTTON_UP_ARROW else NumberOptionSpinnerView.SPINNER_BUTTON_DOWN_ARROW)
    arrow.setMouseTransparent(true);
    
    val pane = new StackPane();
    pane.getChildren().addAll(btn, arrow);
    pane.setAlignment(Pos.CENTER);
    pane
  }
  
  val node = new HBox() {
    //Reference view, so that as long as this node is retained, the view will be retained
    val view = NumberOptionSpinnerView.this
    
    setId(NumberOptionSpinnerView.NUMBER_SPINNER)

    val firstHeight = textField.heightProperty().divide(2)
    val secondHeight = textField.heightProperty().subtract(firstHeight)
    val width = textField.heightProperty().multiply(0.75)

    // inc/dec buttons
    val buttons = new GridPane()
    buttons.setHgap(0)
    buttons.setVgap(0)
  
    buttons.setId(NumberOptionSpinnerView.BUTTONS_BOX)

    buttons.add(button(true, firstHeight, width), 0, 0)
    buttons.add(button(false, secondHeight, width), 0, 1)
    getChildren().addAll(textField, buttons)
  }

}