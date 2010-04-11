
// WrapLoaderInfo3D.java
// Andrew Davison, April 2005, ad@fivedots.coe.psu.ac.th

/* A checkboard floor, with a red center square, and labelled
   XZ axes.

   Load the model stored in filename fn using a NCSA Portfolio
   loader. The model is rotated and scaled so it is easy to see.

   Carry out the following operations on the loaded model:
     * display its named objects
     * traverse its scene graph, saving info to the EXAMINE_FN file
     * adjust the model's component shapes in various ways
         - there are 4 different variations; see adjustShape3D()

*/

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;
import java.text.DecimalFormat;

import com.sun.j3d.utils.universe.*;
import com.sun.j3d.utils.geometry.*;
import com.sun.j3d.utils.image.*;
import javax.media.j3d.*;
import javax.vecmath.*;
import com.sun.j3d.utils.behaviors.vp.*;

import ncsa.j3d.loaders.*;     // Portfolio loaders
import com.sun.j3d.loaders.Scene;

// import com.tornadolabs.j3dtree.*;    // for displaying the scene graph



public class WrapLoaderInfo3D extends JPanel
// Holds the 3D canvas where the loaded image is displayed
{
  private static final int PWIDTH = 512;   // size of panel
  private static final int PHEIGHT = 512; 

  private static final int BOUNDSIZE = 100;  // larger than world
  private static final Point3d USERPOSN = new Point3d(0,5,20);
    // initial user position

  private static final Color3f white = new Color3f(1.0f, 1.0f, 1.0f);
  private static final Color3f black = new Color3f(0.0f, 0.0f, 0.0f);
  private static final Color3f blue = new Color3f(0.6f,0.6f, 1.0f);

  private static final String EXAMINE_FN = "examObj.txt";
  private static final String TEXTURE_FN = "models/stone.jpg";


  private SimpleUniverse su;
  private BranchGroup sceneBG;
  private BoundingSphere bounds;   // for environment nodes

  private FileWriter ofw;     // for writing out model info
  private DecimalFormat df;   // for simpler output

  // references to the loading model
  private Scene loadedScene = null;
  private BranchGroup loadedBG = null;

  private int adaptNo;    
        // used to choose the shape adaption method in adjustShape3D()
  private Texture2D texture = null;   // used when changing a shape's texture

  // private Java3dTree j3dTree;   // frame to hold tree display


  public WrapLoaderInfo3D(String fn, int adaptNo)
  // construct the 3D canvas
  {
    this.adaptNo = adaptNo;
    setLayout( new BorderLayout() );
    setOpaque( false );
    setPreferredSize( new Dimension(PWIDTH, PHEIGHT));

    GraphicsConfiguration config =
					SimpleUniverse.getPreferredConfiguration();
    Canvas3D canvas3D = new Canvas3D(config);
    add("Center", canvas3D);
    canvas3D.setFocusable(true);     // give focus to the canvas 
    canvas3D.requestFocus();

    su = new SimpleUniverse(canvas3D);

    // j3dTree = new Java3dTree();   // create a display tree for the SG

    createSceneGraph(fn);
    initUserPosition();        // set user's viewpoint
    orbitControls(canvas3D);   // controls for moving the viewpoint
    
    su.addBranchGraph( sceneBG );

	// j3dTree.updateNodes( su );    // build the tree display window

  } // end of WrapLoaderInfo3D()



  private void createSceneGraph(String fn) 
  // initilise the scene
  { 
    sceneBG = new BranchGroup();
    bounds = new BoundingSphere(new Point3d(0,0,0), BOUNDSIZE);
    df = new DecimalFormat("0.###");    // 3 dp
   
    lightScene();         // add the lights
    addBackground();      // add the sky
    sceneBG.addChild( new CheckerFloor().getBG() );  // add the floor

    loadModel(fn);    // load the model stored in fn
                      // loadedScene and loadedBG should now have values
    // examine the model
    if (loadedScene != null) {
      showNamedObject(loadedScene);   // display its named objects
      storeGraphInfo(loadedBG);       // traverse model's graph; save info to file

      // adjust the model's component shapes in various ways
      adjustShapes(loadedBG);      
    }
	// j3dTree.recursiveApplyCapability( sceneBG );   // set capabilities for tree display

    sceneBG.compile();   // fix the scene
  } // end of createSceneGraph()



  private void lightScene()
  /* One ambient light, 2 directional lights */
  {
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
     zoom in and out.  */
  {
    OrbitBehavior orbit = 
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



// -------------------------- model loading --------------


    private void loadModel(String fn)
    /* Load the model from fn into the scene graph using a NCSA 
       Portfolio loader. Rotate and scale it to make it easier to see.
       Store the loaded model's scene in the global loadedScene, 
       and its branch group in loadedBG.
    */
    {
      FileWriter ofw = null;
      System.out.println( "Loading: " + fn );

      try {
        ModelLoader loader = new ModelLoader();    // the NCSA portfolio loader
        // System.out.println("Loader flags: " + loader.getFlags());
        loadedScene = loader.load(fn);             // the loaded scene

        // Rotate and scale the model
        if(loadedScene != null ) {
          loadedBG = loadedScene.getSceneGroup();    // the model's BG
 
          Transform3D t3d = new Transform3D();
          t3d.rotX( -Math.PI/2.0 );    // models are often on their face; fix that
          Vector3d scaleVec = calcScaleFactor(loadedBG, fn);   // scale the model
          t3d.setScale( scaleVec );

          TransformGroup tg = new TransformGroup(t3d);
          tg.addChild(loadedBG);

          sceneBG.addChild(tg);   // add (tg->loadedBG) to scene
        }
        else
          System.out.println("Load error with: " + fn);
      }
      catch( IOException ioe )
      { System.err.println("Could not find object file: " + fn); }
    } // end of loadModel()


  private Vector3d calcScaleFactor(BranchGroup loadedBG, String fn)
  // Scale the model based on its original bounding box size
  {
     BoundingBox boundbox = new BoundingBox( loadedBG.getBounds() );
     // System.out.println(boundbox);

     // obtain the upper and lower coordinates of the box
     Point3d lower = new Point3d();
     boundbox.getLower( lower );
     Point3d upper = new Point3d();
     boundbox.getUpper( upper );

     // store the largest X, Y, or Z dimension and calculate a scale factor
     double max = 0.0;
     if( (upper.x - lower.x ) > max )
       max = (upper.x - lower.x );

     if( (upper.y - lower.y ) > max )
       max = (upper.y - lower.y );

     if( (upper.z - lower.z ) > max )
       max = (upper.z - lower.z );

     double scaleFactor = 10.0/max;    // 10 is half the width of the floor
     System.out.println("max dimension: " + df.format(max) + 
                        "; scaleFactor: " + df.format(scaleFactor) );

     // limit the scaling so that a big model isn't scaled too much
     if( scaleFactor < 0.0005 )
         scaleFactor = 0.0005;

     return new Vector3d(scaleFactor, scaleFactor, scaleFactor);
  }  // end of calcScaleFactor()



 // ---------- model examination --------------------------------


  private void showNamedObject(Scene loadedScene)
  /* Display the named objects, if any. 
     The naming scheme is file-type dependent, but includes 
     VRML DEF names and Lightwave 3D object filenames.
  */
  {
    String name;
    Hashtable namedObjects = loadedScene.getNamedObjects();
    Enumeration e = namedObjects.keys();
    if (namedObjects.isEmpty())
      System.out.println("No Named Objects");
    else {
      System.out.println("Named Objects");
      while(e.hasMoreElements()) {
        name = (String) e.nextElement(); 
        System.out.println(name);
      }
    }
  }  // end of showNamedObject()



  
  private void storeGraphInfo(BranchGroup bg)
  /* Traverse the model's scene graph and store 
     information in EXAMINE_FN via the ofw FileWriter.
  */
  {
    System.out.println("Writing model details to " + EXAMINE_FN);
    try {
      ofw = new FileWriter( EXAMINE_FN );
      examineNode(0, bg);
      ofw.close();
    }
    catch( IOException ioe )
    { System.err.println("Cannot write to " + EXAMINE_FN); }
  }  // end of storeGraphInfo()


  private void examineNode(int level, Node node) throws IOException
  /* A Node can be a Group or a Leaf. Depending on the type of
     Group or Leaf, report different things.
     Recursively call examineNode() on the children of a Group,
     incrementing level so the output can be indented correctly.
  */
  {
    if(node instanceof Group) {     // the Node is a Group
      Group g = (Group) node;
      levelPrint(level, "Group: " + g.getClass());

      if(g instanceof TransformGroup) {    // consider subclass
        Transform3D t3d = new Transform3D();
	    ((TransformGroup) g).getTransform(t3d);
        levelPrint(level, t3d.toString() );   // show Transform3D info for TG
      }

      levelPrint(level, g.numChildren() + " children");
      Enumeration enumKids = g.getAllChildren();
      while(enumKids.hasMoreElements())    // visit Group children
        examineNode(level+1, (Node) enumKids.nextElement());
    }
    else if (node instanceof Leaf) {     // the Node is a Leaf
      levelPrint(level, "Leaf: " + node.getClass());
      if (node instanceof Shape3D)
        examineShape3D(level, (Shape3D) node);   // treat Shape3D specially
    }
    else    // the Node is something other than a Group or Leaf
      levelPrint(level, "Node: " + node.getClass());

  }  // end of examineNode()


  private void examineShape3D(int level, Shape3D shape)
									      throws IOException
  /* A Shape3D is a container for Appearance and Geometry components.
     Show appearance info with printAppearance().
     A shape may contain many geometries: examine each one with
     examineGeometry()
  */
  {
    Appearance app = shape.getAppearance();    // consider appearance
    if (app == null)
      levelPrint(level+1, "No Appearance Component");
    else
      printAppearance(level, app);

    int numGeoms = shape.numGeometries();    // consider geometries
    if (numGeoms == 0)
      levelPrint(level+1, "No Geometry Components");
    else if (numGeoms == 1) {
      Geometry g = shape.getGeometry();
      examineGeometry(level+1, 1, g);
    }
    else {   // more than one geometry in the shape
      levelPrint(level+1, "No. of Geometries: " + numGeoms);
      Enumeration enumGeoms = shape.getAllGeometries();
      int i = 1;
      while(enumGeoms.hasMoreElements()) {
        examineGeometry(level+1, i, (Geometry) enumGeoms.nextElement() );
        i++;
      }
    }
    levelPrint(level, "");  // put in a newline in the output
  }  // end of examineShape3D()


  private void printAppearance(int level, Appearance app)
										 throws IOException
  /* Lots of Appearance information could be printed here. We only
     display colour related stuff.
  */
  {
     ColoringAttributes ca = app.getColoringAttributes();
     if (ca != null)
       levelPrint(level, ca.toString() );
     Material mat = app.getMaterial();
     if (mat != null)
       levelPrint(level, mat.toString() );
  }  // end of printAppearance()


  private void examineGeometry(int level, int index, Geometry geo)
										 throws IOException
  // Display geometry info found in a shape
  {
    levelPrint(level, "Geometry: " + geo.getClass());
    if(geo instanceof GeometryArray)
      // geometryArray is frequently used in models, so give some extra info
      levelPrint(level, "Vertex count: " + 
               ((GeometryArray)geo).getVertexCount()); 
  }  // end of examineGeometry()



  private void levelPrint(int level, String s) throws IOException
  // the s string is indented according to level
  {
    for(int i=0; i<level; i++) {
      // System.out.print("  ");
      ofw.write("  ");
    }
    // System.out.println(s);
    ofw.write(s + "\n");
  }  // end of levelPrint()




  // ----------------------- adjust the model's Shape3Ds ------------------


  private void adjustShapes(Node node)
  {
    System.out.println("Adjusting shapes...");
    if ((adaptNo == 3) || (adaptNo == 4))   // will add a texture to the shapes
      loadTexture(TEXTURE_FN);
    visitNode(node);
  }  // end of adjustShapes()


  private void loadTexture(String fn)
  // load image from file fn as a texture
  {
    TextureLoader texLoader = new TextureLoader(fn, null);
    texture = (Texture2D) texLoader.getTexture();
    if (texture == null)
      System.out.println("Cannot load texture from " + fn);
    else {
      System.out.println("Loaded texture from " + fn);
      texture.setEnable(true);
    }
  }  // end of loadTexture()


  private void visitNode(Node node)
  /*  If the node is a Group then recursively visit its children.
      Otherwise if the node is a Shape3D then so the changes.
  */
  { if(node instanceof Group) {
      Group g = (Group) node;
      Enumeration enumKids = g.getAllChildren();
      while(enumKids.hasMoreElements()) {    // visit children
        SceneGraphObject obj = (SceneGraphObject) enumKids.nextElement();
        if (obj instanceof Node)
          visitNode((Node) obj);
      }
    }
    else if (node instanceof Shape3D)
      adjustShape3D((Shape3D) node);
  }  // end of visitNode()



  private void adjustShape3D(Shape3D shape)
  /* Shape adjusting examples:
        * change the colour of a shape to blue
        * draw the shape in outline (i.e. as a wireframe)
        * make the shape transparent
        * add a texture to the shape (this can be combined with the
          colour changing method)
     The choice of which one depends on adaptNo: 0-3, and
     4 does makeBlue() and addTexture()
  */
  { switch(adaptNo) {
      case 0: makeBlue(shape); break;
      case 1: drawOutline(shape); break;
      case 2: makeAlmostTransparent(shape); break;
      case 3: addTexture(shape); break;
      case 4: makeBlue(shape); addTexture(shape); break;
      default: break;   // say nothing
    }
  }  // end of adjustShape3D()


  private void makeBlue(Shape3D shape)
  // change the shape's colour to blue
  {
    Appearance app = shape.getAppearance();
    Material blueMat = new Material(black, black, blue, white, 20.0f);
        // the black ambient means that unlit surfaces are pitch black
    blueMat.setLightingEnable(true);
    app.setMaterial( blueMat );
    shape.setAppearance(app);
  }  // end of makeBlue()


  private void drawOutline(Shape3D shape)
  // draw only the shape's outline (i.e. as a wireframe)
  {
    Appearance app = shape.getAppearance();
    PolygonAttributes pa = new PolygonAttributes();
    pa.setCullFace( PolygonAttributes.CULL_NONE );
    pa.setPolygonMode( PolygonAttributes.POLYGON_LINE );

    app.setPolygonAttributes( pa );
    shape.setAppearance(app);
  }  // end of drawOutline()


  private void makeAlmostTransparent(Shape3D shape)
  // make the shape almost transparent
  {
    Appearance app = shape.getAppearance();

    TransparencyAttributes ta = new TransparencyAttributes();
    ta.setTransparencyMode( TransparencyAttributes.BLENDED );
    ta.setTransparency(0.8f);     // 1.0f is totally transparent

    app.setTransparencyAttributes( ta );
    shape.setAppearance(app);
  } // end of makeAlmostTransparent()


  private void addTexture(Shape3D shape)
  /* Add a texture to the shape, but only if it is represented by a
     single GeometryArray.
  */
  { if (shape.numGeometries() == 1) {
      Geometry g = shape.getGeometry();
      if (g instanceof GeometryArray) 
        addTextureGA(shape);
      else
        System.out.println("Shape geometry is not a GeometryArray");
    }
    else
      System.out.println("Shape has too many geometries");
  }  // end of addTexture()



  private void addTextureGA(Shape3D shape)
  /* Add a texture to a GeometryArray. 
     Add the texture to all faces of the model, 'stretch' the texture 
     over the model, modulate the texture with the existing colour
     and lighting components. The texture comes from TEXTURE_FN, and
     was loaded before the traversal of the model began.
  */
  {
    Appearance app = shape.getAppearance();

    // make shape two-sided, so texture appears on both sides
    PolygonAttributes pa = new PolygonAttributes();
    pa.setCullFace( PolygonAttributes.CULL_NONE );
    app.setPolygonAttributes( pa );

    // generate texture coords that 'stretch' the texture over the model
    app.setTexCoordGeneration( stampTexCoords(shape) );

    // combine texture with colour and lighting of underlying surface
    TextureAttributes ta = new TextureAttributes();
    ta.setTextureMode( TextureAttributes.MODULATE );
    app.setTextureAttributes( ta );

    // apply texture to shape
    if (texture != null) {    // loaded at start, from adjustShapes()
      app.setTexture(texture);
      shape.setAppearance(app);
    }
  }  // end of addTextureGA()


  private TexCoordGeneration stampTexCoords(Shape3D shape)
  /* Specify how the texture is mapped to the model by 
     adjusting the genration planes so that a single texture is
     'stetched' over the entire model rather than repeatedly 'tiled'.

     This makes a big difference for models that are very large
     since the default tiling approach tends to lose texture
     detail and the display 'shimmers' since the texture is 
     mapped to only a few pixels on the model.
  */
  {
    // get the bounds of the shape
    BoundingBox boundBox = new BoundingBox( shape.getBounds() );
    Point3d lower = new Point3d();
    Point3d upper = new Point3d();
    boundBox.getLower(lower); boundBox.getUpper(upper);
    // System.out.println("lower: " + lower + "\nupper: " + upper );

    double width = upper.x - lower.x;
    double height = upper.y - lower.y;
    // System.out.println("width: " + df.format(width) + 
    //                 "; height: " + df.format(height) );

    // adjust generation planes so shape box is mapped to texture
    // coordinates [0,0] and [1,1].
    Vector4f planeS = 
       new Vector4f( (float)(1.0/width), 0.0f, 0.0f, (float)(-lower.x/width));
    Vector4f planeT = 
       new Vector4f( 0.0f, (float)(1.0/height), 0.0f, (float)(-lower.y/height));

    // generate new texture coordinates for GeometryArray
    TexCoordGeneration texGen = new TexCoordGeneration();
    texGen.setPlaneS(planeS);
    texGen.setPlaneT(planeT);

    return texGen;
  } // end of stampTexCoords()


} // end of WrapLoaderInfo3D class
