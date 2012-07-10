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
import javafx.scene.control.ToggleButton
import boxes.jfx.SlideCheck

class ToggleDemo extends Application {
    
  override def start(primaryStage: Stage) {
    primaryStage.setTitle("Hello World!")
        
    val grid = new GridPane()
    grid.setAlignment(Pos.CENTER)
    grid.setHgap(10)
    grid.setVgap(10)
    grid.setPadding(new Insets(25, 25, 25, 25))
    grid.setStyle("-fx-background-color: #f0f0f0;")

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
    scene.getStylesheets.add(classOf[ToggleDemo].getResource("Buttons.css").toExternalForm())
    primaryStage.setScene(scene)

    val text = Var("Text")
    
    val btn = new SlideCheck()
    btn.setText("Say 'Hello World'")
//    btn.setId("lion")
    btn.setOnAction(new EventHandler[ActionEvent]() {
        override def handle(event: ActionEvent) {
            text() = text() + " p"
        }
    })
    grid.add(btn, 0, 0)
    
    val r = new Rectangle
    r.setWidth(10)
    r.setHeight(10)
    r.setStyle("-fx-background-color: pink;");
//    btn.

    
    primaryStage.show()
  }
}

object ToggleDemo {
  def main(args: Array[String]) = Application.launch(classOf[ToggleDemo], args:_*)
}