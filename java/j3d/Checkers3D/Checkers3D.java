
// Checkers3D.java
// Andrew Davison, April 2005, ad@fivedots.coe.psu.ac.th

/* A simple basic world consisting of a checkboard floor, 
   with a red center square, and labelled XZ axes.

   A floating, shiny blue sphere is placed at the center.
*/

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;


public class Checkers3D extends JFrame
{
  public Checkers3D() 
  {
    super("Checkers3D");

    GraphicsEnvironment graphicsEnvironment = 
      GraphicsEnvironment.getLocalGraphicsEnvironment();
    GraphicsDevice gd = graphicsEnvironment.getDefaultScreenDevice();

    if (gd.isFullScreenSupported()) {
      System.out.println("Setting to full screen mode.");
      setUndecorated(true);
      setResizable(false);
      gd.setFullScreenWindow(this);
      validate();
    } else {
      System.out.println("Bugger... no full-screen supported.");
    }

    Container c = getContentPane();
    c.setLayout( new BorderLayout() );
    WrapCheckers3D w3d = new WrapCheckers3D();     // panel holding the 3D canvas
    c.add(w3d, BorderLayout.CENTER);

    setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
    pack();

    setResizable(false);    // fixed size display
    setVisible(true);
  } // end of Checkers3D()


// -----------------------------------------

  public static void main(String[] args)
  { new Checkers3D(); }

} // end of Checkers3D class
