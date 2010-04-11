import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;
import java.net.*;

class AdFace extends JComponent {
  private Image image;
  private String url;

  public AdFace(String location) {
    setURL(location);
    downloadAd();
  }

  public synchronized void setURL(String location) {
    url = location;
  } 

  public synchronized void downloadAd() {
    try {
      Toolkit toolkit = Toolkit.getDefaultToolkit();
      URL imageURL = new URL(url);
      image = toolkit.getImage(imageURL);
      repaint();
    } catch (MalformedURLException ex) {
      System.out.println("Malformed URL: " + url);
    }
  }

  public synchronized void renderAd(Graphics2D g2d) {
    g2d.drawImage(image, 10, 10, this);
  }

  public void paint(Graphics g) {
    Graphics2D g2d = (Graphics2D) g;

    int width = getSize().width;
    int height = getSize().height;

    g2d.setPaint(Color.black);
    g2d.fillRect(0, 0, width, height);

    g2d.setPaint(Color.white);

    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
      RenderingHints.VALUE_ANTIALIAS_ON);

    g2d.setFont(new Font("Arial", Font.PLAIN, 36));
    g2d.drawString("Touch screen to continue", (width / 2) - 200, height - 100);

    renderAd(g2d);
  }

  protected void clear(Graphics g) {
    super.paintComponent(g);
  }
}

public class AdSaver implements Runnable {
  private JFrame frame;
  private GraphicsDevice gd;
  private DisplayMode originalDisplayMode;
  private AdFace adface;
  private boolean running = true;

  public void quit() {
    gd.setDisplayMode(originalDisplayMode);
    running = false;
    System.exit(0);
   }

  public void run() {
    int counter = 0;

    while (running) {
      try {
        Thread.sleep(1000);
      } catch (InterruptedException ex) {}

      counter++;

      if (counter == 30) {
        System.out.println("Downloading ad.");
        adface.downloadAd();
        counter = 0;
      }
    }
  }

  public void go(String location) {
    GraphicsEnvironment graphicsEnvironment =
      GraphicsEnvironment.getLocalGraphicsEnvironment();

    GraphicsDevice[] devices = graphicsEnvironment.getScreenDevices();

    System.out.println("Detected devices: ");
    for (GraphicsDevice device : devices) {
      System.out.println(device);
    }

    gd = graphicsEnvironment.getDefaultScreenDevice();
    originalDisplayMode = gd.getDisplayMode();

    JFrame frame = new JFrame("Ad Saver");
    Container c = frame.getContentPane();
    c.setLayout(new BorderLayout());

    adface = new AdFace(location);
    c.add(adface, BorderLayout.CENTER);
    frame.setSize(400, 400);
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.setLocation(100, 100);

    frame.addWindowListener(new WindowAdapter() {
      public void windowClosed(ActionEvent event) {
        quit();
      }
    });

    MouseMotionAdapter mouseMoveHandler = new MouseMotionAdapter() {
      public void mouseMoved(MouseEvent e) {
        quit();
      }
    };

    MouseAdapter mouseHandler = new MouseAdapter() {
      public void mousePressed(MouseEvent e) {
        quit();
      }
      public void mouseReleased(MouseEvent e) {
        quit();
      }
      public void mouseClicked(MouseEvent e) {
        quit();
      }
    };

    KeyAdapter keyHandler = new KeyAdapter() {
      public void keyPressed(KeyEvent e) {
        quit();
      }
    };

    frame.addMouseMotionListener(mouseMoveHandler);
    frame.addKeyListener(keyHandler);
    frame.addMouseListener(mouseHandler);

    // Set to full screen mode. This must be done before calling
    // setVisible on the frame.

    if (gd.isFullScreenSupported()) {
      System.out.println("Setting to full screen mode.");
      frame.setUndecorated(true);
      frame.setResizable(false);
      gd.setFullScreenWindow(frame);
      frame.validate();
    } else {
      System.out.println("Bugger... no full-screen supported.");
    }

    frame.setVisible(true);
    new Thread(this).start();
  }

  public static void main(String args[]) {
    String location = args[0];
    System.out.println("Getting ads from: " + location);
    new AdSaver().go(location);
  }
}
