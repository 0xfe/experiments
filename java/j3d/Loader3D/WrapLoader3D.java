
// WrapLoader3D.java
// Andrew Davison, April 2005, ad@fivedots.coe.psu.ac.th

/* The checkboard world with the loaded object (the 'prop')
   initially located at its center.
*/

import javax.swing.*;
import java.awt.*;
import com.sun.j3d.utils.universe.*;
import com.sun.j3d.utils.geometry.*;
import javax.media.j3d.*;
import javax.vecmath.*;
import com.sun.j3d.utils.behaviors.vp.*;

// import com.tornadolabs.j3dtree.*;    // for displaying the scene graph



public class WrapLoader3D extends JPanel
// Holds the 3D canvas where the loaded image is displayed
{
  private static final int PWIDTH = 512;   // size of panel
  private static final int PHEIGHT = 512; 

  private static final int BOUNDSIZE = 100;  // larger than world
  private static final Point3d USERPOSN = new Point3d(0,5,10);
    // initial user position


  private SimpleUniverse su;
  private BranchGroup sceneBG;
  private BoundingSphere bounds;      // for environment nodes

  private TransformGroup objectTG;    // TG which the loaded object hangs off
  private PropManager propMan;        // manages the manipulation of the 
                                      // loaded object

  // private Java3dTree j3dTree;   // frame to hold tree display



  public WrapLoader3D(String filename, boolean hasCoordsInfo)
  // construct the 3D canvas
  {
    setLayout( new BorderLayout() );
    setOpaque( false );
    setPreferredSize( new Dimension(PWIDTH, PHEIGHT));

    // load the object (the prop) and optional placement info
    propMan = new PropManager(filename, hasCoordsInfo);

    GraphicsConfiguration config =
					SimpleUniverse.getPreferredConfiguration();
    Canvas3D canvas3D = new Canvas3D(config);
    add("Center", canvas3D);

    su = new SimpleUniverse(canvas3D);

    // j3dTree = new Java3dTree();   // create a display tree for the SG

    createSceneGraph(filename);
    initUserPosition();        // set user's viewpoint
    orbitControls(canvas3D);   // controls for moving the viewpoint

    su.addBranchGraph( sceneBG );

	// j3dTree.updateNodes( su );    // build the tree display window

  } // end of WrapLoader3D()


  private void createSceneGraph(String filename) 
  // initilise the scene
  { 
    sceneBG = new BranchGroup();
    bounds = new BoundingSphere(new Point3d(0,0,0), BOUNDSIZE); 

    lightScene();         // add the lights
    addBackground();      // add the sky
    sceneBG.addChild( new CheckerFloor().getBG() );  // add the floor
    
    sceneBG.addChild( propMan.getTG() );    
                    // add the loaded object (the 'prop') to the scene

	// j3dTree.recursiveApplyCapability( sceneBG );   // set capabilities for tree display

    sceneBG.compile();   // fix the scene
  } // end of createScene()


  private void lightScene()
  // One ambient light, 2 directional lights
  {
    Color3f white = new Color3f(1.0f, 1.0f, 1.0f);

    // Set up the ambient light
    AmbientLight ambientLightNode = new AmbientLight(white);
    ambientLightNode.setInfluencingBounds(bounds);
    sceneBG.addChild(ambientLightNode);

    // Set up the directional lights
    Vector3f light1Direction  = new Vector3f(-1.0f, -1.0f, -1.0f);
       // left, down, backwards 
    Vector3f light2Direction  = new Vector3f(1.0f, -1.0f, 1.0f);
       // right, down, forwards

    DirectionalLight light1 = 
            new DirectionalLight(white, light1Direction);
    light1.setInfluencingBounds(bounds);
    sceneBG.addChild(light1);

    DirectionalLight light2 = 
        new DirectionalLight(white, light2Direction);
    light2.setInfluencingBounds(bounds);
    sceneBG.addChild(light2);
  }  // end of lightScene()



  private void addBackground()
  // A blue sky
  { Background back = new Background();
    back.setApplicationBounds( bounds );
    back.setColor(0.17f, 0.65f, 0.92f);    // sky colour
    sceneBG.addChild( back );
  }  // end of addBackground()


  private void orbitControls(Canvas3D c)
  /* OrbitBehaviour allows the user to rotate around the scene, and to
     zoom in and out. */
  { OrbitBehavior orbit = 
		new OrbitBehavior(c, OrbitBehavior.REVERSE_ALL);
    orbit.setSchedulingBounds(bounds);
    ViewingPlatform vp = su.getViewingPlatform();
    vp.setViewPlatformBehavior(orbit);	    
  }  // end of orbitControls()



  private void initUserPosition()
  // Set the user's initial viewpoint using lookAt()
  {
    ViewingPlatform vp = su.getViewingPlatform();
    TransformGroup steerTG = vp.getViewPlatformTransform();

    Transform3D t3d = new Transform3D();
    steerTG.getTransform(t3d);

    // args are: viewer posn, where looking, up direction
    t3d.lookAt( USERPOSN, new Point3d(0,0,0), new Vector3d(0,1,0));
    t3d.invert();

    steerTG.setTransform(t3d);
  }  // end of initUserPosition()



  // ---------------------- handle prop ---------------------------
  // The follwing methods are used by the GUI in Loader3D to
  // access the PropManager.

  public void movePos(int axis, int change)
  {  propMan.move(axis, change);  }

  public void rotate(int axis, int change)
  {  propMan.rotate(axis, change);  }

  public void scale(double d)
  {  propMan.scale(d);  }

  public Vector3d getLoc()
  {  return propMan.getLoc();  }

  public Point3d getRotations()
  {  return propMan.getRotations();  }

  public double getScale()
  {  return propMan.getScale();  }

  public void saveCoordFile()
  {  propMan.saveCoordFile();  }


} // end of WrapLoader3D class