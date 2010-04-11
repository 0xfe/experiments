import java.io.*;
import java.util.logging.Logger;
import java.net.*;

import org.apache.xmlrpc.*;
import org.apache.xmlrpc.client.*;

public class XmlRpcClientTest {
  private static Logger logger = Logger.global;

  public static void main(String [] args) throws Exception {
    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
    config.setServerURL(new URL("http://127.0.0.1:8999/xmlrpc"));

    XmlRpcClient client = new XmlRpcClient();
    client.setConfig(config);

    logger.info("Sending RPC to server."); 
    Object[] params = new Object[] { new String("123456FFF") };
    String result = (String) client.execute("DeliveryServlet.getAdImageURL", params);
    logger.info("Got result: " + result); 

    logger.info("Shutting down RPC server.");
    result = (String) client.execute("ControlServlet.shutdownServer", params);
    logger.info("Got result: " + result); 
  }
}
