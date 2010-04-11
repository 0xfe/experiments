
// Loader3D.java
// Andrew Davison, April 2005, ad@fivedots.coe.psu.ac.th

/* Load an object into the checkboard world, with its
   bounding sphere scaled with radius 1. A 3ds model is
   also rotated -90 around the x-axis.
   The Portfolio loaders are used.

   This class handles the moves, rotations, and scaling
   GUI interface, and a save button with stores the 
   placement info in a 'coords' file.

   When an object is loaded it can optionally be loaded
   with its coords file, created in an earlier execution
   of Loader.
*/

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.text.DecimalFormat;
import java.io.*;

import javax.vecmath.*;


public class Loader3D extends JFrame
						implements ActionListener
{
  // for specifying moves and rotations
  private static final int X_AXIS = 0;
  private static final int Y_AXIS = 1;
  private static final int Z_AXIS = 2;
  private static final int INCR = 0;
  private static final int DECR = 1;

  private WrapLoader3D w3d;    // the J3D canvas for the loader

  // the GUI elements
  private JButton xPosLeftBut, xPosRightBut, yPosLeftBut, yPosRightBut, 
											zPosLeftBut, zPosRightBut;
  private JButton xRotLeftBut, xRotRightBut, yRotLeftBut, yRotRightBut, 
											zRotLeftBut, zRotRightBut;
  private JTextField scaleTF;
  private JTextField xyzTF, rotTF, scaleTotTF;
  private JButton saveBut;

  private DecimalFormat df;    // for textfield output



  public Loader3D(String args[]) 
  {
    super("3D Loader");

    boolean hasCoordsInfo = false;
    String filename = null;
    if ((args.length == 2) && (args[0].equals("-c"))) {
      hasCoordsInfo = true;
      filename = args[1];
    }
    else if (args.length == 1) 
       filename = args[0];
    else {
      System.out.println( "Usage: java Loader3D [-c] <file>");
      System.exit(0);
    }

    w3d = new WrapLoader3D(filename, hasCoordsInfo);
    initGUI();

    setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
    pack();
    setResizable(false);    // fixed size display
    setVisible(true);
  } // end of Loader3D()



  private void initGUI()
  /* The GUI consists of the 3D canvas in the center, and a column
     of buttons, textfields, etc., down the right hand side (the 
     control panel).
     The top half of the control panel are for inputs: a series of
     buttons for carrying out moves and rotations, and a textfield
     for entering scaling info.
     The bottom half of the panel is for displaying the current
     position, rotation, and scaling details.
  */
  {
    ImageIcon upIcon = new ImageIcon("icons/up.gif");
    ImageIcon downIcon = new ImageIcon("icons/down.gif");
    ImageIcon leftIcon = new ImageIcon("icons/left.gif");
    ImageIcon rightIcon = new ImageIcon("icons/right.gif");
    ImageIcon inIcon = new ImageIcon("icons/in.gif");
    ImageIcon outIcon = new ImageIcon("icons/out.gif");

    df = new DecimalFormat("0.###");  // 3 dp

    Container c = getContentPane();
    c.setLayout( new BorderLayout() );
    c.add(w3d, BorderLayout.CENTER);

    // build input controls

    JPanel p1 = new JPanel();
    JLabel xPosLabel = new JLabel("X incr:");
    xPosLeftBut = new JButton(leftIcon);
    xPosLeftBut.addActionListener(this);
    xPosRightBut = new JButton(rightIcon);
    xPosRightBut.addActionListener(this);
    p1.add(xPosLabel); p1.add(xPosLeftBut); p1.add(xPosRightBut);

    JPanel p2 = new JPanel();
    JLabel yPosLabel = new JLabel("Y incr:");
    yPosLeftBut = new JButton(downIcon);
    yPosLeftBut.addActionListener(this);
    yPosRightBut = new JButton(upIcon);
    yPosRightBut.addActionListener(this);
    p2.add(yPosLabel); p2.add(yPosLeftBut); p2.add(yPosRightBut);

    JPanel p3 = new JPanel();
    JLabel zPosLabel = new JLabel("Z incr:");
    zPosLeftBut = new JButton(inIcon);
    zPosLeftBut.addActionListener(this);
    zPosRightBut = new JButton(outIcon);
    zPosRightBut.addActionListener(this);
    p3.add(zPosLabel); p3.add(zPosLeftBut); p3.add(zPosRightBut);

    JPanel p4 = new JPanel();
    JLabel xRotLabel = new JLabel("X rot:");
    xRotLeftBut = new JButton(leftIcon);
    xRotLeftBut.addActionListener(this);
    xRotRightBut = new JButton(rightIcon);
    xRotRightBut.addActionListener(this);
    p4.add(xRotLabel); p4.add(xRotLeftBut); p4.add(xRotRightBut);

    JPanel p5 = new JPanel();
    JLabel yRotLabel = new JLabel("Y rot:");
    yRotLeftBut = new JButton(leftIcon);
    yRotLeftBut.addActionListener(this);
    yRotRightBut = new JButton(rightIcon);
    yRotRightBut.addActionListener(this);
    p5.add(yRotLabel); p5.add(yRotLeftBut); p5.add(yRotRightBut);


    JPanel p6 = new JPanel();
    JLabel zRotLabel = new JLabel("Z rot:");
    zRotLeftBut = new JButton(leftIcon);
    zRotLeftBut.addActionListener(this);
    zRotRightBut = new JButton(rightIcon);
    zRotRightBut.addActionListener(this);
    p6.add(zRotLabel); p6.add(zRotLeftBut); p6.add(zRotRightBut);

    JPanel p7 = new JPanel();
    JLabel scaleLabel = new JLabel("Scale mult:");
    scaleTF = new JTextField("1.1", 4);
    scaleTF.addActionListener(this);
    p7.add(scaleLabel); p7.add(scaleTF);

    JPanel p8 = new JPanel();
    saveBut = new JButton("Save Coords");
    saveBut.addActionListener(this);
    p8.add(saveBut);

    // build info. reporting controls

    JLabel xyzLabel = new JLabel("Pos (x,y,z):");
    xyzTF = new JTextField(10);
    xyzTF.setEditable(false);

    JLabel rotLabel = new JLabel("Rot (x,y,z):");
    rotTF = new JTextField(10);
    rotTF.setEditable(false);

    JPanel pScale = new JPanel();
    JLabel scaleTotLabel = new JLabel("Tot Scale:");
    scaleTotTF = new JTextField(4);
    scaleTotTF.setEditable(false);
    pScale.add(scaleTotLabel); pScale.add(scaleTotTF);

    // main control panel
    JPanel ctrlPanel = new JPanel();
    ctrlPanel.setLayout(
			new BoxLayout(ctrlPanel, BoxLayout.Y_AXIS));
    // add input controls
    ctrlPanel.add(p1); ctrlPanel.add(p2);
    ctrlPanel.add(p3); ctrlPanel.add(p4);
    ctrlPanel.add(p5); ctrlPanel.add(p6);
    ctrlPanel.add(p7); ctrlPanel.add(p8);

    // add a bit of space between the inputs and reporters
    ctrlPanel.add( javax.swing.Box.createVerticalStrut(15) );

    // add info. reporting controls
    ctrlPanel.add(xyzLabel);
    ctrlPanel.add(xyzTF);
    ctrlPanel.add(rotLabel);
    ctrlPanel.add(rotTF);
    ctrlPanel.add(pScale);

    JPanel ctrlP = new JPanel();
    ctrlP.add(ctrlPanel);

    c.add(ctrlP, BorderLayout.EAST);

    showPosInfo();   // update on-screen display
    showRotInfo();
    showScale();
  }  // end of initGUI()



  public void actionPerformed(ActionEvent e)
  {
    if (e.getSource() == saveBut)   // save coord info
      w3d.saveCoordFile();
    else if (e.getSource() == xPosLeftBut)   // X move
      w3d.movePos(X_AXIS, DECR);
    else if (e.getSource() == xPosRightBut)
      w3d.movePos(X_AXIS, INCR);
    else if (e.getSource() == yPosLeftBut)   // Y move
      w3d.movePos(Y_AXIS, DECR);
    else if (e.getSource() == yPosRightBut)
      w3d.movePos(Y_AXIS, INCR);
    else if (e.getSource() == zPosLeftBut)   // Z move
      w3d.movePos(Z_AXIS, DECR);
    else if (e.getSource() == zPosRightBut)
      w3d.movePos(Z_AXIS, INCR);
    else 
    if (e.getSource() == xRotLeftBut)        // X rotation
      w3d.rotate(X_AXIS, DECR);
    else if (e.getSource() == xRotRightBut)
      w3d.rotate(X_AXIS, INCR);
    else if (e.getSource() == yRotLeftBut)   // Y rotation
      w3d.rotate(Y_AXIS, INCR);
    else if (e.getSource() == yRotRightBut)
      w3d.rotate(Y_AXIS, DECR);
    else if (e.getSource() == zRotLeftBut)   // Z rotation
      w3d.rotate(Z_AXIS, INCR);
    else if (e.getSource() == zRotRightBut)
      w3d.rotate(Z_AXIS, DECR);
    else if (e.getSource() == scaleTF) {   // scale
      try {
        double d = Double.parseDouble( e.getActionCommand() );
        w3d.scale(d);
      }
      catch(NumberFormatException ex) {
        System.out.println("Scale input was not a number");
      }
    }
    showPosInfo();   // update on-screen display
    showRotInfo();
    showScale();
  }  // end of actionPerformed()


  private void showPosInfo()
  {  
    Vector3d loc = w3d.getLoc();
    xyzTF.setText("( " + df.format(loc.x) + ", " +
				df.format(loc.y) + ", " + df.format(loc.z) + " )");  
  }


  private void showRotInfo()
  {  
     Point3d rots = w3d.getRotations();
     rotTF.setText("( " + df.format(rots.x) + ", " +
				df.format(rots.y) + ", " + df.format(rots.z) + " )");  
  }


  private void showScale()
  { 
    double scale = w3d.getScale();
    scaleTotTF.setText( df.format(scale) );  
  }


// ----------------------------------------

  public static void main(String[] args)
  { new Loader3D(args); }

} // end of Loader3D class

