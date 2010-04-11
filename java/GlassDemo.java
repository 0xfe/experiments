import java.lang.reflect.*;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.font.*;
import java.awt.peer.*;
import java.util.logging.*;

class WindowUtilities {
  public static DisplayMode originalDisplayMode = null;
  public static GraphicsDevice gd;
  public static Logger logger = Logger.global;

  public static void setWindowAlpha(Window w, float alpha) {
    ComponentPeer peer = w.getPeer();
    if (peer == null) {
      return;
    }
    Class< ? extends ComponentPeer> peerClass = peer.getClass();

    //noinspection EmptyCatchBlock
    try {
      Class< ?> nativeClass = Class.forName("apple.awt.CWindow");
      if (nativeClass.isAssignableFrom(peerClass)) {
          Method setAlpha = nativeClass.getMethod(
                  "setAlpha", float.class);
          setAlpha.invoke(peer, Math.max(0.0f, Math.min(alpha, 1.0f)));
      }
    } catch (ClassNotFoundException e) {
    } catch (NoSuchMethodException e) {
    } catch (IllegalAccessException e) {
    } catch (InvocationTargetException e) {
    }
  }

  public static int getTextWidth(Graphics2D g2d, Font f, String text) {
    Font font = new Font("Arial", Font.PLAIN, 24);

    FontRenderContext frc = g2d.getFontRenderContext();
    int textWidth = (int) font.getStringBounds(text, frc).getWidth();

    return textWidth;
  }

  public static void renderText(Graphics2D g2d, String text, int x, int y) {
    Font font = new Font("Arial", Font.PLAIN, 24);
    g2d.setFont(font);
    g2d.drawString(text, x, y);
  }

  // Set to full screen mode. This must be done before calling
  // setVisible on the frame.
  public static void setFullScreenMode(JFrame frame) {
    GraphicsEnvironment graphicsEnvironment =
      GraphicsEnvironment.getLocalGraphicsEnvironment();

    GraphicsDevice[] devices = graphicsEnvironment.getScreenDevices();

    for (GraphicsDevice device : devices) {
      logger.info("Detected device: " + device);
    }

    gd = graphicsEnvironment.getDefaultScreenDevice();
    originalDisplayMode = gd.getDisplayMode();

    if (gd.isFullScreenSupported()) {
      frame.setUndecorated(true);
      frame.setResizable(false);
      gd.setFullScreenWindow(frame);
      frame.validate();
    } else {
      logger.warning("Bugger... no full-screen supported.");
    }
  }

  public static void revertFullScreenMode() {
    gd.setDisplayMode(originalDisplayMode);
  }
}

class MyGlassPane extends JComponent {
  public void paintComponent(Graphics g) {
    Graphics2D g2d = (Graphics2D) g;

    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON);

    WindowUtilities.renderText(g2d, "Press any key to continue", 10, 10);
  }
};
  
public class GlassDemo {
  JFrame frame;

  public GlassDemo() {
    frame = new JFrame("Transparent Window");
    frame.setUndecorated(true);

    frame.getContentPane().add(new MyGlassPane());
    frame.setLocation(200, 200);
    frame.setSize(200,200);
    frame.setAlwaysOnTop(true);

    frame.addWindowListener(new WindowAdapter() {
      public void windowClosed(ActionEvent event) {
        System.exit(0);
      }
    });

    // OS X needs the frame to be visible BEFORE setting Alpha.
    if (!"Mac OS X".equals(System.getProperty("os.name"))) {
      frame.setVisible(true);
    }

    frame.setVisible(true);
    WindowUtilities.setWindowAlpha(frame, 0.5f);
    frame.setVisible(true);
    frame.getGlassPane().setVisible(true);
  }

  public static void main(String[] args) {
    System.setProperty("apple.laf.useScreenMenuBar", "true");
    System.setProperty("apple.awt.brushMetalRounded", "true");
    System.setProperty("com.apple.mrj.application.apple.menu.about.name", "Glass");
    new GlassDemo();
  }
}
