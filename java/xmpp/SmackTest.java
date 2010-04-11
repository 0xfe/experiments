import org.jivesoftware.smack.*;
import org.jivesoftware.smack.packet.*;
import java.io.*;
import java.util.logging.*;

class SmackTest {
  private static Logger logger = Logger.global;

  public static void main(String[] args) {
    logger.setLevel(Level.INFO);
    ConnectionConfiguration config = 
      new ConnectionConfiguration("talk.google.com", 5222, "gmail.com");
    
    XMPPConnection connection = new XMPPConnection(config);

    BufferedReader in = new BufferedReader(new InputStreamReader(System.in));

    try {
      System.out.print("Username: ");
      String username = in.readLine();

      System.out.print("Password: " );
      String password = in.readLine();

      connection.connect();
      connection.login(username, password);

      Presence presence = new Presence(Presence.Type.available);
      connection.sendPacket(presence);
    } catch (XMPPException ex) {
      logger.severe("Connection failed: " + ex);
    } catch (IOException ex) {
      logger.severe("Failed to get credentials: " + ex);
    }
  }
}
