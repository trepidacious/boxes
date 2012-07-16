package javafxtest

import javafx.application.Application
import javafx.event.ActionEvent
import javafx.event.EventHandler
import javafx.geometry.Insets
import javafx.geometry.Pos
import javafx.scene.Scene
import javafx.scene.control.Button
import javafx.scene.layout.GridPane
import javafx.stage.Stage
import javafx.scene.control.Tab
import javafx.scene.control.TabPane
import javafx.scene.shape.Rectangle
import javafx.scene.paint.Color
import scalafx.geometry.Side
import boxes.Var
import boxes.jfx._
import boxes.Cal
import boxes.general.RadioReaction
import boxes.Val

class JavaFXDemo extends Application {
    
  override def start(primaryStage: Stage) {
    primaryStage.setTitle("Hello World!")
        
    val grid = new GridPane()
    grid.setAlignment(Pos.CENTER)
    grid.setHgap(10)
    grid.setVgap(10)
    grid.setPadding(new Insets(25, 25, 25, 25))
    grid.setStyle("-fx-background-color: #f0f0f0;");

    val tabPane = new TabPane()
    tabPane.setSide(Side.TOP)
    tabPane.setTabClosingPolicy(TabPane.TabClosingPolicy.UNAVAILABLE)
    val tab = new Tab()
    tab.setText("Tab one");
    tab.setContent(grid);
    tabPane.getTabs().add(tab);
    
    val tab2 = new Tab()
    tab2.setText("Tab two");
    tabPane.getTabs().add(tab2);

    val scene = new Scene(tabPane, 300, 275)
    scene.getStylesheets.clear()
    scene.getStylesheets.add(JFXView.css)
    primaryStage.setScene(scene)

    val text = Var("Text")
    
    val btn = new Button()
    btn.setText("Say 'Hello World'")
    btn.setOnAction(new EventHandler[ActionEvent]() {
        override def handle(event: ActionEvent) {
            text() = text() + " p"
        }
    })
    grid.add(btn, 0, 0)
    
    val b = Var(true)
    
    val bString = Cal{if (b()) "True!" else "FALSE!" }
    
    grid.add(StringView(text).node, 1, 0)
    grid.add(LabelView(text).node, 2, 0)
    grid.add(BooleanView(b).node, 3, 0)
    grid.add(LabelView(bString).node, 4, 0)
    grid.add(BooleanView(b).node, 5, 0)
    
    val x = Var(false)
    val y = Var(true)
    val z = Var(false)
    RadioReaction(x, y, z)

    //Why do we need all the parameters here?
    val xv = BooleanView(x, Val("X Label"), boxes.BooleanControlType.CHECKBOX, Val(None), false)
    val yv = BooleanView(y, Val("Y Label"), boxes.BooleanControlType.CHECKBOX, Val(None), false)
    val zv = BooleanView(z, Val("Z Label"), boxes.BooleanControlType.CHECKBOX, Val(None), false)

    grid.add(xv.node, 0, 1)
    grid.add(yv.node, 1, 1)
    grid.add(zv.node, 2, 1)
    
    val p = Var(10d)
    val q = Var(20d)
    
    p << q() + 10
    q << p() - 10
    
    grid.add(NumberSpinnerView(p).node, 0, 2)
    grid.add(NumberSpinnerView(q).node, 1, 2)


    val dis = new Button()
    dis.setText("Disabled")
    dis.disableProperty.set(true)
    grid.add(dis, 3, 2)

    grid.add(SlideCheck(x, false).node, 0, 3)
    grid.add(SlideCheck(y, false).node, 0, 4)
    grid.add(SlideCheck(z, false).node, 0, 5)
    grid.add(SlideCheck(b, true).node, 3, 3)

    primaryStage.show()
  }
}

object JavaFXDemo {
  def main(args: Array[String]) = Application.launch(classOf[JavaFXDemo], args:_*)
}
