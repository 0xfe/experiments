import java.net.*;
import java.io.*;
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class EasyChatClient {
  JTextField outgoing;
  PrintWriter writer;
  Socket sock;

  public void go() {
    JFrame frame = new JFrame("Easy Chat");
    JPanel panel = new JPanel();

    outgoing = new JTextField(20);

    JButton sendButton = new JButton("Send");
    sendButton.addActionListener(new SendButtonListener());

    panel.add(outgoing);
    panel.add(sendButton);

    frame.getContentPane().add(BorderLayout.CENTER, panel);
    setupNetworking();
    frame.setSize(400, 500);
    frame.setVisible(true);
  }

  private void setupNetworking() {
    try {
      sock = new Socket("127.0.0.1", 5000);
      writer = new PrintWriter(sock.getOutputStream());
      System.out.println("Connected to 127.0.0.1:5000");
    } catch (IOException ex) {
      ex.printStackTrace();
    }
  }

  public class SendButtonListener implements ActionListener {
    public void actionPerformed(ActionEvent ev) {
      try {
        writer.println(outgoing.getText());
        writer.flush();
      } catch (Exception ex) {
        ex.printStackTrace();
      }

      outgoing.setText("");
      outgoing.requestFocus();
    }
  }

  public static void main(String[] args) {
    new EasyChatClient().go();
  }
}
