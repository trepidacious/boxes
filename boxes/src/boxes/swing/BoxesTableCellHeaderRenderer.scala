package boxes.swing

import javax.swing.plaf.UIResource
import javax.swing.table.{DefaultTableCellRenderer}
import javax.swing._
import border.{EmptyBorder, EtchedBorder, Border}
import com.explodingpixels.macwidgets.MacColorUtils
import java.awt._
import com.explodingpixels.macwidgets.plaf.EmphasizedLabelUI
import com.explodingpixels.painter.Painter

object HeaderPainter {
  val dividerColor = new Color(0, 0, 0, 51)
  val bottomColor = new Color(0x555555)
  val image = new ImageIcon(classOf[BoxesTableCellHeaderRenderer].getResource("/boxes/swing/Header.png")).getImage
}

class HeaderPainter(paintLeft:Boolean = false, paintRight:Boolean = true) extends Painter[Component] {
  override def paint(g:Graphics2D, t:Component, w:Int, h:Int) {
    g.drawImage(HeaderPainter.image, 0, 0, w, h, null)

    g.setColor(HeaderPainter.dividerColor)
    if (paintLeft) {
      g.drawLine(0, 0, 0, h-1)
    }
    if (paintRight) {
      g.drawLine(w-1, 0, w-1, h-1)
    }

    g.setColor(HeaderPainter.bottomColor)
    g.drawLine(0, h-1, w-1, h-1)
  }
}

object BoxesTableCellHeaderRenderer {
  val border = BorderFactory.createEmptyBorder(1, 5, 1, 6);
  val bgPainter = new HeaderPainter
  val lastBGPainter = new HeaderPainter(false, false)
  val cornerBGPainter = new HeaderPainter(true, false)
}

class BoxesTableCellHeaderRenderer() extends DefaultTableCellRenderer with UIResource {

  setHorizontalAlignment(SwingConstants.CENTER)
  setBorder(BoxesTableCellHeaderRenderer.border)
  setPreferredSize(new Dimension(getPreferredSize.width, 22))

  var lastColumn = false

  override def getTableCellRendererComponent(table:JTable, value:Object,
            isSelected:Boolean, hasFocus:Boolean, row:Int, column:Int):Component = {

    var sortIcon:Icon = null
    var isPaintingForPrint = false
    lastColumn = (column == table.getColumnCount-1)

    if (table != null) {
      val header = table.getTableHeader();
      if (header != null) {
        var fgColor:Color = null;
        var bgColor:Color = null;
        if (hasFocus) {
          fgColor = UIManager.getColor("TableHeader.focusCellForeground")
          bgColor = UIManager.getColor("TableHeader.focusCellBackground")
        }
        if (fgColor == null) {
          fgColor = header.getForeground()
        }
        if (bgColor == null) {
          bgColor = header.getBackground()
        }
        setForeground(fgColor);
        setBackground(bgColor);

        setFont(header.getFont())

        isPaintingForPrint = header.isPaintingForPrint()
      }

      if (!isPaintingForPrint && table.getRowSorter() != null) {
        setHorizontalTextPosition(SwingConstants.LEADING)
        getColumnSortOrder(table, column) match {
          case null => {}
          case SortOrder.ASCENDING => sortIcon = UIManager.getIcon("Table.ascendingSortIcon")
          case SortOrder.DESCENDING => sortIcon = UIManager.getIcon("Table.descendingSortIcon")
          case SortOrder.UNSORTED => sortIcon = UIManager.getIcon("Table.naturalSortIcon")
        }
      }
    }

    setText(if (value == null) "" else value.toString())
    setIcon(sortIcon);

    this
  }

  override def paintComponent(g:Graphics) {
    val g2d = g.create.asInstanceOf[Graphics2D]
    if (lastColumn) {
      BoxesTableCellHeaderRenderer.lastBGPainter.paint(g2d, this, this.getWidth, this.getHeight)
    } else {
      BoxesTableCellHeaderRenderer.bgPainter.paint(g2d, this, this.getWidth, this.getHeight)
    }
    super.paintComponent(g);
    g2d.dispose();
  }


  def getColumnSortOrder(table:JTable, column:Int) = {
    var rv:SortOrder = null
    if (table.getRowSorter() == null) {
      rv
    }
    val sortKeys = table.getRowSorter().getSortKeys();
    if (sortKeys.size() > 0 && sortKeys.get(0).getColumn() ==
      table.convertColumnIndexToModel(column)) {
      rv = sortKeys.get(0).getSortOrder();
    }
    rv
  }
}