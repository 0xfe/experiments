import com.sun.j3d.utils.universe.*;
import com.sun.j3d.utils.geometry.*;
import com.sun.j3d.utils.behaviors.vp.*;
import javax.media.j3d.*;
import javax.vecmath.*;

import javax.swing.*;
import java.awt.*;

class WrapPosition extends JPanel {
  private int rotation = -1;

  public static void LOG(String msg) {
    System.out.println(msg);
  }

  public WrapPosition() {
    setLayout( new BorderLayout() );
    setOpaque( false );
    setPreferredSize( new Dimension(1024, 768));

    GraphicsConfiguration config =
					SimpleUniverse.getPreferredConfiguration();
    Canvas3D canvas3D = new Canvas3D(config);
    add("Center", canvas3D);
    canvas3D.setFocusable(true);     // give focus to the canvas 
    canvas3D.requestFocus();

    SimpleUniverse universe = new SimpleUniverse(canvas3D);
    BranchGroup group = new BranchGroup();

    Color3f red = new Color3f(1.4f, .1f, .1f);
    Color3f black = new Color3f(.0f, .0f, .0f);

    for (float x = -1.0f; x <= 1.0f; x = x + 0.1f) {
      Appearance ap = new Appearance();
      Color3f random = new Color3f((float) Math.random() * 2, 
                                   (float) Math.random() * 2, 
                                   (float) Math.random() * 2);

      ap.setMaterial(new Material(random, black, random, black, 1.0f));

      int primflags = Primitive.GENERATE_NORMALS +
        Primitive.GENERATE_TEXTURE_COORDS;

      Sphere sphere = new Sphere(0.05f, primflags, ap);

      TransformGroup tg = new TransformGroup();
      Transform3D transform = new Transform3D();
      Vector3f vector = new Vector3f(x, .0f, .0f);

      transform.setTranslation(vector);
      tg.setTransform(transform);
      tg.addChild(sphere);
      group.addChild(tg);
    }

    Color3f light1Color = new Color3f(1.4f, 1.4f, 1.4f);
    BoundingSphere bounds = 
      new BoundingSphere(new Point3d(0.0, 0.0, 0.0), 100.0);

    Vector3f light1Direction = new Vector3f(4.0f, -7.0f, -12.0f);
    DirectionalLight light1 = new DirectionalLight(light1Color, light1Direction);
    light1.setInfluencingBounds(bounds);
    group.addChild(light1);

    OrbitBehavior orbit = new OrbitBehavior(canvas3D, OrbitBehavior.REVERSE_ALL);
    orbit.setSchedulingBounds(bounds);

    ViewingPlatform vp = universe.getViewingPlatform();
    vp.setViewPlatformBehavior(orbit);

    vp.setNominalViewingTransform();
    universe.addBranchGraph(group);
  }
}

public class Position extends JFrame {
  public Position() {
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
    WrapPosition w3d = new WrapPosition();     // panel holding the 3D canvas
    c.add(w3d, BorderLayout.CENTER);

    setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
    pack();

    setResizable(false);    // fixed size display
    setVisible(true);
  }

  public static void main(String[] args) { new Position(); }
}
