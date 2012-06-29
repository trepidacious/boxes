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
 
class HelloWorld extends Application {
    
    override def start(primaryStage: Stage) {
        primaryStage.setTitle("Hello World!")
        val btn = new Button()
        btn.setText("Say 'Hello World'")
        btn.setOnAction(new EventHandler[ActionEvent]() {
            override def handle(event: ActionEvent) {
                println("Hello World!")
            }
        })
        
        val grid = new GridPane()
        grid.setAlignment(Pos.CENTER)
        grid.setHgap(10)
        grid.setVgap(10)
        grid.setPadding(new Insets(25, 25, 25, 25))

        val scene = new Scene(grid, 300, 275)
        primaryStage.setScene(scene)

        grid.add(btn, 0, 0)
        grid.add(new Button(), 1, 0)

        primaryStage.show()
    }
}

object HelloWorld {
  def main(args: Array[String]) = Application.launch(classOf[HelloWorld], args:_*)
}