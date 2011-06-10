package boxes.demo;

import java.awt.*;
  import javax.swing.*;
  import javax.swing.plaf.*;
  import javax.swing.plaf.metal.*;

  public class CornerButtonUI
      extends MetalButtonUI {
    private final static CornerButtonUI
      cornerButtonUI = new CornerButtonUI();

    public static ComponentUI createUI(JComponent c) {
      return cornerButtonUI;
    }

    public void paint(Graphics g, JComponent c) {
      super.paint(g, c);
      g.setColor(c.getForeground());
      int width = c.getWidth();
      int height = c.getHeight();
      int drawWidth = (int)(width * .15);
      int drawHeight = (int)(height * .10);
      // top left
      g.fillRect(0, 0, drawWidth, drawHeight);
      // top right
      g.fillRect(width-drawWidth, 0, width,
        drawHeight);
      // bottom left
      g.fillRect(0, height-drawHeight, drawWidth,
        height);
      // bottom right
      g.fillRect(width-drawWidth, height-drawHeight,
        width, height);
   }

   public static void main(String args[]) {
       try {
        UIManager.setLookAndFeel( new MetalLookAndFeel() );
       } catch (UnsupportedLookAndFeelException ulafe) {

       }
      JFrame frame = new JFrame("Corners");
      frame.setDefaultCloseOperation(
        JFrame.EXIT_ON_CLOSE);
      Container c = frame.getContentPane();

      UIManager.put("ButtonUI", "CornerButtonUI");
      JButton corners = new JButton("Corners");
      c.add(corners, BorderLayout.CENTER);
      frame.setSize(200, 100);
      frame.setVisible(true);
   }
  }