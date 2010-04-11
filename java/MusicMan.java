import javax.sound.midi.*;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

class MyDrawPanel extends JPanel {
  public void paintComponent(Graphics g) {
    int red = (int) (Math.random() * 255);
    int blue = (int) (Math.random() * 255);
    int green = (int) (Math.random() * 255);

    Color randomColor = new Color(red, blue, green);
    g.setColor(randomColor);
    g.fillOval(70, 70, 100, 100);
  }
}

class MusicGui {
  private JFrame frame;
  private JPanel panel;
  private JTextField field;

  private GraphicsDevice gd;
  private DisplayMode origDisplayMode;

  public void go() {
    GraphicsEnvironment graphicsEnvironment = 
      GraphicsEnvironment.getLocalGraphicsEnvironment();

    GraphicsDevice[] devices = 
      graphicsEnvironment.getScreenDevices();

    System.out.println("Detected devices: ");
    for (GraphicsDevice device : devices) {
      System.out.println(device);
    }

    gd = graphicsEnvironment.getDefaultScreenDevice();
    origDisplayMode = gd.getDisplayMode();

    // Setup the Frame

    frame = new JFrame();
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    frame.setSize(300, 300);

    // Setup the bottom panel

    panel = new JPanel();

    JButton button = new JButton("Change colors");
    button.addActionListener(new ChangeListener());

    JButton quit = new JButton("Quit");
    quit.addActionListener(new QuitListener());

    panel.add(button);
    panel.add(quit);

    frame.getContentPane().add(BorderLayout.SOUTH, panel);

    // Setup the top panel

    JPanel top_panel = new JPanel();

    JLabel label = new JLabel("System Status");
    field = new JTextField(20);

    top_panel.add(label);
    top_panel.add(field);

    frame.getContentPane().add(BorderLayout.NORTH, top_panel);

    // Setup the center panel

    MyDrawPanel drawPanel = new MyDrawPanel();
    frame.getContentPane().add(BorderLayout.CENTER, drawPanel);

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

    // Set focus to text field

    field.requestFocus();
    frame.setVisible(true);
  }


  class ChangeListener implements ActionListener {
    public void actionPerformed(ActionEvent event) {
      frame.repaint();
    }
  }

  class QuitListener implements ActionListener {
    public void actionPerformed(ActionEvent event) {
      System.out.println("Trying to quit... help help!");
      System.out.println(field.getText());
      gd.setDisplayMode(origDisplayMode);
      System.exit(0);
    }
  }
}

public class MusicMan {
  public void play() {
    try {
      Sequencer player = MidiSystem.getSequencer();
      player.open();

      Sequence seq = new Sequence(Sequence.PPQ, 4);
      Track track = seq.createTrack();

      ShortMessage a = new ShortMessage();
      a.setMessage(144, 1, 44, 100);
      MidiEvent noteOn = new MidiEvent(a, 1);

      track.add(noteOn);

      ShortMessage b = new ShortMessage();
      b.setMessage(128, 1, 44, 100);
      MidiEvent noteOff = new MidiEvent(b, 16);

      track.add(noteOff);

      player.setSequence(seq);
      player.start();
    } catch(Exception ex) {
      ex.printStackTrace();
    }
  }

  public static void main(String[] args) {
    MusicMan mm = new MusicMan();

    MusicGui gui = new MusicGui();
    gui.go();
  }
}
