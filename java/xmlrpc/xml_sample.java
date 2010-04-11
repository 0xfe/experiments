package com.fossilix.ads;

import org.w3c.dom.*;
import org.xml.sax.InputSource;
import javax.xml.xpath.*;
import javax.xml.transform.*;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.font.*;
import java.awt.geom.*;
import java.io.*;
import java.net.*;
import java.util.logging.*;

class ConfigReader {
  private String location;
  private XPath xpath;
  private InputSource inputSource;
  private static Logger logger = Logger.global;

  public static final String CONFIGFILE = "fossilix_adsaver_config.xml";

  public ConfigReader() {
    location = System.getProperty("launch4j.exedir") + 
               System.getProperty("file.separator") + CONFIGFILE;

    xpath = XPathFactory.newInstance().newXPath();
    inputSource = new InputSource(location);
    logger.info("Reading configuration from: " + location);
  }

  public String getValue(String path) throws XPathExpressionException {
    String value = xpath.evaluate(path, inputSource);
    logger.info("Configuration: " + path + " = " + value);
    return value;
  }
    
  public NodeList getNodes(String path) throws XPathExpressionException {
    NodeList nodes = null;

    XPathExpression expr = xpath.compile(path);
    Object result = expr.evaluate(inputSource, XPathConstants.NODESET);

    nodes = (NodeList) result;

    return nodes;
  }

  public void writeToFile(String key) throws IOException {
    PrintWriter writer = new PrintWriter(new FileWriter(location));
    writer.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
    writer.println("<AdSaver>");
    writer.println("  <Kiosk id=\"demo\">");
    writer.println("    <Key>" + key + "</Key>");
    writer.println("  </Kiosk>");
    writer.println("</AdSaver>");
    writer.close();
  }
}

class ConfigDialog {
  private JFrame frame;
  private ConfigReader config;
  private static Logger logger = Logger.global;
  private JTextField keyField;

  public ConfigDialog() {
    config = new ConfigReader();
    frame = new JFrame("Fossilix AdServer");
    frame.setSize(400,400);
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

    JPanel bottomPanel = new JPanel();

    JButton okButton = new JButton("Save");
    JButton cancelButton = new JButton("Cancel");
    cancelButton.setMnemonic(KeyEvent.VK_ESCAPE);
    bottomPanel.add(okButton);
    bottomPanel.add(cancelButton);

    frame.getContentPane().add(BorderLayout.SOUTH, bottomPanel);

    JPanel topPanel = new JPanel();
    frame.getContentPane().add(BorderLayout.CENTER, topPanel);
    topPanel.setLayout(new BoxLayout(topPanel, BoxLayout.PAGE_AXIS));

    JPanel keyPanel = new JPanel();
    keyPanel.setLayout(new FlowLayout());

    String key = "NO KEY";

    try {
      key = config.getValue("//AdSaver/Kiosk/Key");
    } catch (XPathExpressionException ex) {
      logger.warning("No key available.");
    }
      
    keyField = new JTextField(20);
    keyField.setText(key);
    keyPanel.add(new JLabel("Kiosk Key"));
    keyPanel.add(keyField);

    topPanel.add(keyPanel);

    okButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        try {
          logger.info("Writing configuration.");
          config.writeToFile(keyField.getText());
          System.exit(0);
        } catch (IOException ex) {
          JOptionPane.showMessageDialog(null, "Error writing configuration.");
          logger.warning("Error writing configuration: " + ex);
        }
      }
    });

    cancelButton.addActionListener(new ActionListener() {
      public void actionPerformed(ActionEvent e) {
        System.exit(0);
      }
    });
        
    frame.pack();
    frame.getRootPane().setDefaultButton(okButton);
    frame.setVisible(true);
  }
}

class AdFace extends JComponent {
  private Image image;
  private String url;
  private String message = "Touch screen to continue";
  public static final String LOCATION = "http://ads.fossilix.com/ads/server/%s/image";
  private static Logger logger = Logger.global;

  public AdFace(String key) {
    setURL(String.format(LOCATION, key));
    downloadAd();
  }

  public synchronized void setURL(String location) {
    url = location;
    logger.config("Getting ads from: " + location);
  } 

  public void setMessage(String m) {
    message = m;
    repaint();
  }

  public void downloadAd() {
    Image tempImage;

    try {
      // Get image path url
      logger.info("Getting image path from: " + url);
      URL imageURL = new URL(url);
      BufferedReader bin = new BufferedReader(
        new InputStreamReader(imageURL.openStream()));

      // Read image path
      String imagePath = bin.readLine();
      logger.info("Downloading image from: " + imagePath);

      Toolkit toolkit = Toolkit.getDefaultToolkit();
      imageURL = new URL(imagePath);
      tempImage = toolkit.getImage(imageURL);

      synchronized (this) {
       image = tempImage;
      }
    } catch (MalformedURLException ex) {
      logger.warning("Malformed URL: " + url);
    } catch (IOException ex) {
      logger.warning("Network error: " + ex);
    }

    repaint();
  }

  public synchronized void renderImage(Graphics2D g2d) {
    int width = getSize().width;
    int imageWidth = image.getWidth(this);
    int imageHeight = image.getHeight(this);

    int centerWidth = width / 2;
    int imageCenterWidth = imageWidth / 2;

    g2d.drawImage(image, centerWidth - imageCenterWidth, 10, this);
  }

  public synchronized void renderText(Graphics2D g2d, String text) {
    Font font = new Font("Arial", Font.PLAIN, 24);
    g2d.setFont(font);

    FontRenderContext frc = g2d.getFontRenderContext();
    int textWidth = (int) font.getStringBounds(text, frc).getWidth();
    int textHeight = (int) font.getStringBounds(text, frc).getHeight();
    int width = getSize().width;
    int height = getSize().height;

    int centerWidth = width / 2;
    int textCenterWidth = textWidth / 2;

    g2d.drawString(message, centerWidth - textCenterWidth, height - textHeight);
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

    if (image != null) {
      renderImage(g2d);
    }
    renderText(g2d, message);
  }
}

public class AdSaver implements Runnable {
  private JFrame frame;
  private GraphicsDevice gd;
  private DisplayMode originalDisplayMode = null;
  private AdFace adface;
  private ConfigReader config;
  private int refresh = 60;
  private boolean running = true;

  private static Logger logger = Logger.global;

  public void loadConfiguration() {
    config = new ConfigReader();

    Logger.global.setLevel(
      Level.parse(getConfigValue("//AdSaver/Configuration/Logging/@level", "WARNING")));
    refresh = Integer.parseInt(getConfigValue("//AdSaver/Configuration/Ads/@refresh", "60"));
  }

  public String getConfigValue(String path) {
    String value = "";

    try {
       value = config.getValue(path);
    } catch (XPathExpressionException ex) {
      logger.severe("Configuration read error (" + path + "): " + ex);
      ex.printStackTrace(System.err);
      quit();
    }

    return value;
  }

  public String getConfigValue(String path, String defaultValue) {
    String value = defaultValue;

    try {
      value = config.getValue(path);
      if (value.length() == 0) {
        value = defaultValue;
        logger.info("Using default: " + path + " = " + defaultValue);
      }
    } catch (XPathExpressionException ex) {
      logger.info("Using default: " + path + " = " + defaultValue);
    }

    return value;
  }

  public void initInterface() {
    GraphicsEnvironment graphicsEnvironment =
      GraphicsEnvironment.getLocalGraphicsEnvironment();

    GraphicsDevice[] devices = graphicsEnvironment.getScreenDevices();

    for (GraphicsDevice device : devices) {
      logger.info("Detected device: " + device);
    }

    gd = graphicsEnvironment.getDefaultScreenDevice();
    originalDisplayMode = gd.getDisplayMode();

    // Initialize AdSaver Frame
    frame = new JFrame("Ad Saver");
    Container c = frame.getContentPane();
    c.setLayout(new BorderLayout());

    // Add the AdFace Component
    adface = new AdFace(getConfigValue("//AdSaver/Kiosk/Key", "NOKEY"));
    adface.setMessage(getConfigValue("//AdSaver/Configuration/Display/@message",
      "Touch screen to continue"));
    c.add(adface, BorderLayout.CENTER);

    frame.setSize(800, 600);
    frame.setAlwaysOnTop(true);

    // Set to full screen mode. This must be done before calling
    // setVisible on the frame.

    if (gd.isFullScreenSupported() &&
        getConfigValue("//AdSaver/Configuration/Display/@fullscreen", "true").
        equalsIgnoreCase("true"))
    {
      frame.setUndecorated(true);
      frame.setResizable(false);
      gd.setFullScreenWindow(frame);
      frame.validate();
    } else {
      logger.warning("Bugger... no full-screen supported.");
    }

    frame.setVisible(true);
  }

  public void installHandlers() {
    WindowAdapter windowHandler = new WindowAdapter() {
      public void windowClosed(ActionEvent event) {
        quit();
      }
    };

    MouseMotionAdapter mouseMoveHandler = new MouseMotionAdapter() {
      public void mouseMoved(MouseEvent e) {
        quit();
      }
    };

    MouseAdapter mouseHandler = new MouseAdapter() {
      public void mouseClicked(MouseEvent e) {
        quit();
      }
    };

    KeyAdapter keyHandler = new KeyAdapter() {
      public void keyPressed(KeyEvent e) {
        quit();
      }
    };

    if (getConfigValue("//AdSaver/Configuration/ExitOn/@mousemove", "false").
        equalsIgnoreCase("true")) {
      frame.addMouseMotionListener(mouseMoveHandler);
    }

    if (getConfigValue("//AdSaver/Configuration/ExitOn/@windowclose", "true").
        equalsIgnoreCase("true")) {
      frame.addWindowListener(windowHandler);
    }

    if (getConfigValue("//AdSaver/Configuration/ExitOn/@keypress", "true").
        equalsIgnoreCase("true")) {
      frame.addKeyListener(keyHandler);
    }

    if (getConfigValue("//AdSaver/Configuration/ExitOn/@mouseclick", "true").
        equalsIgnoreCase("true")) {
      frame.addMouseListener(mouseHandler);
    }
  }
    
  public void quit() {
    if (originalDisplayMode != null) {
      gd.setDisplayMode(originalDisplayMode);
    }

    running = false;
    System.exit(0);
  }

  public void run() {
    int counter = 0;
    logger.info("Started AdSaver thread.");

    while (running) {
      try {
        Thread.sleep(1000);
      } catch (InterruptedException ex) {}

      counter++;

      if (counter > refresh) {
        adface.downloadAd();
        counter = 0;
      }
    }
  }

  public void go() {
    loadConfiguration();
    javax.swing.SwingUtilities.invokeLater(new Runnable() {
      public void run() {
        initInterface();
        installHandlers();
      }
    });
    new Thread(this).start();
  }

  public static void main(String args[]) {
    if (args.length > 0) {
      String flag = args[0];
      if (flag.equalsIgnoreCase("/c")) {
        javax.swing.SwingUtilities.invokeLater(new Runnable() {
          public void run() {
            new ConfigDialog();
          }
        });
      } else if (flag.equalsIgnoreCase("/s")) {
        new AdSaver().go();
      } else {
        logger.severe("Invalid argument: " + flag);
      }
    } else {
      new AdSaver().go();
    }
  }
}
