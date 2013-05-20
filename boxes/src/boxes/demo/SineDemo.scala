package boxes.demo

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

object SineDemo {

  class Sine {
    val name = Var("Sine")
    val phase = Var(0d)
    val amplitude = Var(1d)
    val enabled = Var(true)
    val points = Var(false)
    val description = Var("Default Description\nCan have multiple lines")
    override def toString = name() + ": " + description()
  }

  def buildLedgerMulti() = {

    val list = ListVar(Range(0, 10).map(i=>{
      val s = new Sine
      s.name() = "Sine " + i
      s.phase() = i/40d
      s.amplitude() = 1
      s
    }):_*)

    val view = LensRecordView[Sine](
      VarLens("Name", _.name),
      VarLens("Phase", _.phase),
      VarLens("Amplitude", _.amplitude),
      VarLens("Enabled", _.enabled),
      VarLens("Points", _.points)
    )

    val indices = ListIndices(list, defaultSelection = DefaultSelection.FirstIndex)

    val ledger = ListLedgerVar(list, view)

    val ledgerView = LedgerView.multiSelectionScroll(ledger, indices, sorting=true)

    val add = new ListMultiAddOp(list, indices, Some(new Sine()))

    val delete = new ListMultiDeleteOp[Sine](list, indices, t=>Unit)

    val up = new ListMultiMoveOp[Sine](list, indices, true)

    val down = new ListMultiMoveOp[Sine](list, indices, false)

    val firstSelected = Cal{
      val is = indices()
      if (is.isEmpty) {
        None
      } else {
        val min = is.min
        if (min >= 0 && min < list().size) {
          Some(list(min))
        } else {
          None
        }
      }
    }

    val popup = BoxesPopupView(icon = Some(GraphSwingView.zoom), popupContents = properties(firstSelected))

    val buttons = SwingButtonBar().add(add).add(delete).add(up).add(down).add(popup).buildWithListStyleComponent(EmbossedLabel("Sine Table"))

    val mainPanel = new JPanel(new BorderLayout())
    mainPanel.add(ledgerView.component, BorderLayout.CENTER)
    mainPanel.add(buttons, BorderLayout.SOUTH)

    (mainPanel, list, indices, firstSelected)
  }

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
        seriesTooltipsPrint = (i:Int) => sines(i).toString(),
        axisTooltipsEnabled = axisTooltipsEnabled,
        extraOverLayers = List(xThreshold, yThreshold)
      )
    )

    val v = GraphSwingBGView(graph)

    //Zoom out by clearing manual bounds to None
    val zoomOutButton = SwingBarButton(SwingOp("", Some(GraphSwingView.zoomOut), SetOp(manualBounds, None:Option[Area])))

    val zoomEnabledView = BooleanView(zoomEnabled, "", BooleanControlType.TOOLBARBUTTON, Some(GraphSwingView.zoomSelect), false)
    val selectEnabledView = BooleanView(selectEnabled, "", BooleanControlType.TOOLBARBUTTON, Some(GraphSwingView.boxSelect), false)

    val grabEnabledView = BooleanView(grabEnabled, "", BooleanControlType.TOOLBARBUTTON, Some(GraphSwingView.move), false)

    val graphProperties = SheetBuilder()
      .blankTop()
      .view("Axis Tooltips", BooleanView(axisTooltipsEnabled))
      .view("Series Tooltips", BooleanView(seriesTooltipsEnabled))
    .panel()

    val settingsPopup = BoxesPopupView(icon = Some(SwingView.wrench), popupContents = graphProperties)

    val buttons = SwingButtonBar()
                    .add(selectEnabledView)
                    .add(grabEnabledView)
                    .add(zoomEnabledView)
                    .add(zoomOutButton)
                    .add(settingsPopup)
                  .buildWithListStyleComponent(EmbossedLabel("Demo Graph"))

    val panel = new JPanel(new BorderLayout())
    panel.add(v.component, BorderLayout.CENTER)

    panel.add(buttons, BorderLayout.SOUTH)

    panel
  }

  def buildBarChartPanel(sines: ListVar[Sine], indices:Var[Set[Int]]) = {

    val selectEnabled = Var(false)
    val zoomEnabled = Var(true)
    val grabEnabled = Var(false)
    val axisTooltipsEnabled = Var(true)
    val seriesTooltipsEnabled = Var(true)
    val manualBounds = Var(None:Option[Area])
    RadioReaction(selectEnabled, zoomEnabled, grabEnabled)

    val data = Cal {
      val bars = sines().zipWithIndex.map{case (s, i) => 
        (("Group " + i/3, s.name()), Bar(s.phase(), None, None, Some(Color.getHSBColor((9-i)/14f, 1f, 1f))))
      }
      Map(bars:_*);
    }
    
    import boxes.graph.Axis._

    val y = Var(0.5d)
    val yThreshold = GraphThreshold(Y, y, Color.red, "Y Threshold", true)

//    val selection = Cal{
//      val s = sines()
//      indices().map(i => ("Group " + i/3, s(i).name()))
//    }

    //Just for demonstration purposes we set up a truly terrifying (but working) bidirectional reaction.
    //This is NOT meant to be a robust means of mapping - in a genuine example, the barchart
    //categories would have genuine meaning and so translating between categories and selected
    //indices would be easier.
    //The interesting thing here is that selection still works when more than one Sine has the same
    //name - it just causes the same-named Sines to be selected together.
    //The simple bit is that when indices() changes, we change selection to contain the
    //categories we generate for the selected sines.
    val selection = Var(Set[(String, String)]())
    selection << {
      val s = sines()
      indices().map(i => ("Group " + i/3, s(i).name()))
    }
    //The terrifying bit is that when selection (or names of sines) change, we
    //update the selected indices to select the indices of those sines with
    //names matching any selected secondary categories.
    indices << {
      val selNames = selection().map(_._2)
      Set(sines().zipWithIndex.flatMap{
        case (s, i) if selNames.contains(s.name()) => Some(i)
        case _ => None
      }: _*)
    }
    
    val graph = Var (
      GraphBasic.withBars (
        ColorBarBySelection(data, selection),
        yName = "Phase",
        zoomEnabled = zoomEnabled,
        manualBounds = manualBounds,
        selectEnabled = selectEnabled,
        selection = selection,
        grabEnabled = grabEnabled,
        yAxis = Val(GraphZoomerAxis(paddingBefore = 0.0, paddingAfter = 0.05)),
        barTooltipsEnabled = seriesTooltipsEnabled,
        axisTooltipsEnabled = axisTooltipsEnabled,
        extraOverLayers = List(yThreshold)
      )
    )

    val v = GraphSwingBGView(graph)

    //Zoom out by clearing manual bounds to None
    val zoomOutButton = SwingBarButton(SwingOp("", Some(GraphSwingView.zoomOut), SetOp(manualBounds, None:Option[Area])))

    val selectEnabledView = BooleanView(selectEnabled, "", BooleanControlType.TOOLBARBUTTON, Some(GraphSwingView.boxSelect), false)

    val zoomEnabledView = BooleanView(zoomEnabled, "", BooleanControlType.TOOLBARBUTTON, Some(GraphSwingView.zoomSelect), false)

    val grabEnabledView = BooleanView(grabEnabled, "", BooleanControlType.TOOLBARBUTTON, Some(GraphSwingView.move), false)

    val graphProperties = SheetBuilder()
      .blankTop()
      .view("Axis Tooltips", BooleanView(axisTooltipsEnabled))
      .view("Series Tooltips", BooleanView(seriesTooltipsEnabled))
    .panel()

    val settingsPopup = BoxesPopupView(icon = Some(SwingView.wrench), popupContents = graphProperties)

    val buttons = SwingButtonBar()
                    .add(selectEnabledView)
                    .add(grabEnabledView)
                    .add(zoomEnabledView)
                    .add(zoomOutButton)
                    .add(settingsPopup)
                  .buildWithListStyleComponent(EmbossedLabel("Demo Graph"))

    val panel = new JPanel(new BorderLayout())
    panel.add(v.component, BorderLayout.CENTER)

    panel.add(buttons, BorderLayout.SOUTH)

    panel
  }
  
  def properties(sine:Ref[Option[Sine]]) = {
    val nameView = StringOptionView(for (s <- sine()) yield s.name)
    val amplitudeView = NumberOptionView(for (s <- sine()) yield s.amplitude)
    val phaseView = NumberOptionView(for (s <- sine()) yield s.phase)

    SheetBuilder()
      .blankTop()
      .view("Name", nameView)
      .view("Amplitude", amplitudeView)
      .view("Phase", phaseView)
    .panel()
  }

  def tabs() {

    val graphIcon = IconFactory.icon("GraphTab")
    val tableIcon = IconFactory.icon("TableTab")
    val propertiesIcon = IconFactory.icon("PropertiesTab")

    val frame = new JFrame("Boxes UI Sine Demo")

    val stuff = buildLedgerMulti()
    val table = stuff._1
    val graph = buildGraphPanel(stuff._2, stuff._3)
    val barchart = buildBarChartPanel(stuff._2, stuff._3)

    val sine = stuff._4

    val nameView = StringOptionView(for (s <- sine()) yield s.name)
    val amplitudeView = NumberOptionView(for (s <- sine()) yield s.amplitude)
    val phaseView = NumberOptionView(for (s <- sine()) yield s.phase)
    val enabledView = BooleanOptionView(for (s <- sine()) yield s.enabled)
    val descriptionView = StringOptionView(for (s <- sine()) yield s.description, true)
    val pointsView = BooleanOptionView(for (s <- sine()) yield s.points)

    val sheet = SheetBuilder()
    val properties = sheet
                      .separator("Edit Sine")
                      .view("Name", nameView)
                      .view("Amplitude", amplitudeView)
                      .view("Phase", phaseView)
                      .view("Enabled", enabledView)
                      .view("Points", pointsView)
                      .view("Description", descriptionView, true)
                     .panel()

    val tabs =
      TabBuilder()
        .add(graph,       "Graph",  Some(graphIcon))
        .add(barchart,    "Bar Chart",  Some(graphIcon))
        .add(table,       "Table",  Some(tableIcon))
        .add(properties,  "Edit",   Some(propertiesIcon))
      .panel()

    frame.add(tabs)

    frame.pack
    frame.setMinimumSize(new Dimension(650, 550))
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setVisible(true)

  }

  def source() {
    val model = new SourceListModel()
    val category = new SourceListCategory("Category")
    model.addCategory(category)
    model.addItemToCategory(new SourceListItem("Item"), category)
    val sourceList = new SourceList(model)

    val frame = new JFrame()

    frame.add(sourceList.getComponent)

    frame.pack
    frame.setMinimumSize(new Dimension(50, 50))
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    frame.setVisible(true)

  }

  def main(args: Array[String]) {
    SwingView.swingRun{
      SwingView.nimbus()
      tabs
//      source
    }
  }

}