import com.sun.j3d.utils.universe.SimpleUniverse;
import com.sun.j3d.utils.geometry.*;
import javax.media.j3d.*;

public class Test3d {

  private int rotation = -1;

  public static void LOG(String msg) {
    System.out.println(msg);
  }

  public Test3d(int rot) {
    rotation = rot;

    SimpleUniverse universe = new SimpleUniverse();

    Transform3D rotate = new Transform3D();
    Transform3D tempRotate = new Transform3D();

    rotate.rotX(Math.PI/4.0d);
    tempRotate.rotY(Math.PI/5.0d);
    rotate.mul(tempRotate);

    TransformGroup objSpin = new TransformGroup(rotate);
    objSpin.setCapability(TransformGroup.ALLOW_TRANSFORM_WRITE);

    BranchGroup group = new BranchGroup();
    objSpin.addChild(new ColorCube(0.4));
    group.addChild(objSpin);

    Alpha rotationAlpha = new Alpha(-1, rotation);

    RotationInterpolator rotator = 
      new RotationInterpolator(rotationAlpha, objSpin);

    BoundingSphere bounds = new BoundingSphere();
    rotator.setSchedulingBounds(bounds);

    objSpin.addChild(rotator);

    universe.getViewingPlatform().setNominalViewingTransform();
    universe.addBranchGraph(group);
  }

  public static void main( String[] args ) {
    if (args.length < 1) {
      LOG("Usage: test [args]");
      return;
    }

    new Test3d(Integer.parseInt(args[0]));
  }
} // end of class Hello3d

