
// LoaderInfo3D.java
// Andrew Davison, April 2005, ad@fivedots.coe.psu.ac.th

/*  Checkers3D scene with a model loaded using a Portfolio loader.
    The model is examined, and its shapes can be changed in various ways.

    Compilation done with CompileLI.bat:
     javac -classpath %CLASSPATH%;ncsa\portfolio.jar *.java
    in compileLI.bat

   Example LoaderInfo3D.bat batch file usage:
     LoaderInfo3D  dolphins.3ds 0

   Changes =======================

   November 2nd, 2004
   show() --> setVisible(true);

*/

import javax.swing.*;
import java.awt.*;


public class LoaderInfo3D extends JFrame
{

  public LoaderInfo3D(String fn, int adaptNo) 
  {
    super("LoaderInfo3D");

    Container c = getContentPane();
    c.setLayout( new BorderLayout() );
    WrapLoaderInfo3D w3d = new WrapLoaderInfo3D(fn, adaptNo);
    c.add(w3d, BorderLayout.CENTER);

    setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
    pack();
    setResizable(false);    // fixed size display
    // show();
    setVisible(true);
    w3d.requestFocus();
  } // end of LoaderInfo3D()


// -----------------------------------------

  public static void main(String[] args)
  { 
    if (args.length == 2) {
      int adaptNo = 0;
      try 
      { adaptNo = Integer.parseInt( args[1] ); }
      catch(NumberFormatException e)
      { System.out.println("Illegal adaptNo, using 0"); } 

      new LoaderInfo3D("models/" + args[0], adaptNo);   // assume in models/
    }
    else
      System.out.println(
       "Usage: java -cp %CLASSPATH%;ncsa\\portfolio.jar LoaderInfo3D  <file> <adaptNo>");
  }

} // end of LoaderInfo3D class

