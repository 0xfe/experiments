import javax.swing.*;
import java.awt.*;
import java.awt.geom.*;

class AdImage {
  private String image;

  public AdImage() {
    image = "No Image";
  }
}

class Advertiser {
  private String url;

  public Advertiser(String u) {
    url = u;
  }

  public AdImage getAdvertisement() {
    return new AdImage();
  }
}

public class ShapeExample extends JPanel {
  private Ellipse2D.Double circle = 
    new Ellipse2D.Double(10, 10, 350, 350);
  private Rectangle2D.Double square =
    new Rectangle2D.Double(10, 10, 350, 350);

  private JFrame frame;

  public ShapeExample() {
    frame = new JFrame("Ad Saver");
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

    
  }

  public void paintComponent(Graphics g) {
    clear(g);
    Graphics2D g2d = (Graphics2D) g;

    g2d.fill(circle);
    g2d.draw(square);
  }

  protected void clear(Graphics g) {
    super.paintComponent(g);
  }

  protected Ellipse2D.Double getCircle() {
    return(circle);
  }

  public static void main(String[] args) {
    JFrame frame = new JFrame("Hello");
    frame.setSize(400, 400);
    frame.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );

    frame.setLocation(100, 100);
    frame.setContentPane(new ShapeExample());
    // frame.pack();
    frame.setVisible(true);
  }
}
