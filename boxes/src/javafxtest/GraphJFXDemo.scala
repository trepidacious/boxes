package javafxtest

import boxes._
import general.{RadioReaction, SetOp}
import graph._
import list._
import java.awt.{Dimension, BorderLayout, GridLayout, Color}
import boxes.VarLens.apply
import swing.{BoxesPopupView, EmbossedLabel, TabBuilder, SheetBuilder, GraphSwingBGView, GraphSwingView, SwingButtonBar, SwingOp, SwingBarButton}
import boxes.BoxImplicits._
import javax.swing._
import com.explodingpixels.macwidgets.{SourceList, SourceListItem, SourceListCategory, SourceListModel}
import boxes.swing.icons.IconFactory
import boxes.swing._
import boxes.jfx.GraphJFXView
import javafx.application.Application
import javafx.stage.Stage
import javafx.scene.layout.GridPane
import javafx.geometry.Pos
import javafx.geometry.Insets

class GraphJFXDemo extends Application {
  
  def buildGraphPanel(sines: ListVar[Sine], indices:Var[Set[Int]]) = {

    val selectEnabled = Var(false)
    val zoomEnabled = Var(true)
    val grabEnabled = Var(false)
    val axisTooltipsEnabled = Var(true)
    val seriesTooltipsEnabled = Var(true)
    val manualBounds = Var(None:Option[Area])
    RadioReaction(selectEnabled, zoomEnabled, grabEnabled)

    val series = Cal{
      sines().zipWithIndex.map{case (s, i) => 
        Series(i,
          if (s.enabled()) Range(0, 100).map(x => x/100d).map(x => Vec2(x, math.sin((x + s.phase()) * 2 * 3.1415) * s.amplitude())).toList else List[Vec2](),
          Color.getHSBColor((9-i)/14f, 1f, 1f),
          2,
          if (s.points()) SeriesStyles.cross else SeriesStyles.line
        )
      }
    }

    import boxes.graph.Axis._

    val x = Var(0.5d)
    val xThreshold = GraphThreshold(X, x, Color.blue, "X Threshold", true)

    val y = Var(0.5d)
    val yThreshold = GraphThreshold(Y, y, Color.red, "Y Threshold", true)

    val graph = Var (
      GraphBasic.withSeries (
        ColorSeriesBySelection(series, indices),
        xName = "X (Time)",
        yName = "Y (Intensity)",
        zoomEnabled = zoomEnabled,
        manualBounds = manualBounds,
        selectEnabled = selectEnabled,
        selection = indices,
        grabEnabled = grabEnabled,
        seriesTooltipsEnabled = seriesTooltipsEnabled,
        axisTooltipsEnabled = axisTooltipsEnabled,
        extraOverLayers = List(xThreshold, yThreshold)
      )
    )

    val v = GraphJFXView(graph)

    v.node
  }
    
  override def start(primaryStage: Stage) {
    primaryStage.setTitle("Hello World!")
        
    val grid = new GridPane()
    grid.setAlignment(Pos.CENTER)
    grid.setHgap(10)
    grid.setVgap(10)
    grid.setPadding(new Insets(25, 25, 25, 25))
    grid.setStyle("-fx-background-color: #f0f0f0;");

//    val tabPane = new TabPane()
//    tabPane.setSide(Side.TOP)
//    tabPane.setTabClosingPolicy(TabPane.TabClosingPolicy.UNAVAILABLE)
//    val tab = new Tab()
//    tab.setText("Tab one");
//    tab.setContent(grid);
//    tabPane.getTabs().add(tab);
//    
//    val tab2 = new Tab()
//    tab2.setText("Tab two");
//    tabPane.getTabs().add(tab2);
//
//    val scene = new Scene(tabPane, 300, 275)
//    scene.getStylesheets.clear()
//    scene.getStylesheets.add(JFXView.css)
//    primaryStage.setScene(scene)
//
//    val text = Var("Text")
//    
//    val btn = new Button()
//    btn.setText("Say 'Hello World'")
//    btn.setOnAction(new EventHandler[ActionEvent]() {
//        override def handle(event: ActionEvent) {
//            text() = text() + " p"
//        }
//    })
//    grid.add(btn, 0, 0)
//    
//    val b = Var(true)
//    
//    val bString = Cal{if (b()) "True!" else "FALSE!" }
//    
//    grid.add(StringView(text).node, 1, 0)
//    grid.add(LabelView(text).node, 2, 0)
//    grid.add(BooleanView(b).node, 3, 0)
//    grid.add(LabelView(bString).node, 4, 0)
//    grid.add(BooleanView(b).node, 5, 0)
//    
//    val x = Var(false)
//    val y = Var(true)
//    val z = Var(false)
//    RadioReaction(x, y, z)
//
//    //Why do we need all the parameters here?
//    val xv = BooleanView(x, Val("X Label"), boxes.BooleanControlType.CHECKBOX, Val(None), false)
//    val yv = BooleanView(y, Val("Y Label"), boxes.BooleanControlType.CHECKBOX, Val(None), false)
//    val zv = BooleanView(z, Val("Z Label"), boxes.BooleanControlType.CHECKBOX, Val(None), false)
//
//    grid.add(xv.node, 0, 1)
//    grid.add(yv.node, 1, 1)
//    grid.add(zv.node, 2, 1)
//    
//    val p = Var(10d)
//    val q = Var(20d)
//    
//    p << q() + 10
//    q << p() - 10
//    
//    grid.add(NumberSpinnerView(p).node, 0, 2)
//    grid.add(NumberSpinnerView(q).node, 1, 2)
//
//
//    val dis = new Button()
//    dis.setText("Disabled")
//    dis.disableProperty.set(true)
//    grid.add(dis, 3, 2)
//
//    grid.add(SlideCheck(x, false).node, 0, 3)
//    grid.add(SlideCheck(y, false).node, 0, 4)
//    grid.add(SlideCheck(z, false).node, 0, 5)
//    grid.add(SlideCheck(b, true).node, 3, 3)

    val sines = ListVar(Range(0, 10).map(i=>{
      val s = new Sine
      s.name() = "Sine " + i
      s.phase() = i/40d
      s.amplitude() = 1
      s
    }):_*)

    val indices = ListIndices(sines, defaultSelection = DefaultSelection.AllIndices)

    grid.add(buildGraphPanel(sines, indices), 0, 0)
    
    primaryStage.show()
  }
}

class Sine {
  val name = Var("Sine")
  val phase = Var(0d)
  val amplitude = Var(1d)
  val enabled = Var(true)
  val points = Var(false)
  val description = Var("Default Description\nCan have multiple lines")
}

object GraphJFXDemo {
  def main(args: Array[String]) = Application.launch(classOf[GraphJFXDemo], args:_*)
}