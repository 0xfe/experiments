import java.io.*;
import java.net.*;

public class AdviceGuy {
  String[] adviceList = {
    "Take smaller bites",
    "One word: inappropriate",
    "Just for today, be honest. Lie tomorrow",
    "You might want to rethink that haircut"
  };

  boolean running = true;

  public void go() {
    try {
      ServerSocket s = new ServerSocket(4242);

      while (running) {
        Socket sock = s.accept();
        new Thread(new AdvisorThread(sock)).start();
      }
    } catch (IOException ex) {
      ex.printStackTrace();
    }
  }

  public class AdvisorThread implements Runnable {
    Socket sock;

    public AdvisorThread(Socket s) {
      sock = s;
    }

    public void run() {
      try {
        PrintWriter writer = new PrintWriter(sock.getOutputStream());
        writer.println(adviceList[(int) (Math.random() * adviceList.length)]);
        writer.close();
      } catch (IOException ex) {
        ex.printStackTrace();
      }
    }
  }
      

  public static void main(String[] args) {
    new AdviceGuy().go();
  }
}
