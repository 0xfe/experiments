import java.io.*;

class WriteFile {
  public static void main(String[] args) {
    try {
      FileWriter fw = new FileWriter("model.xml");

      fw.write("<xml>\n");
      fw.write("  <important>XML Sucks Balls</xml>\n");
      fw.write("</xml>\n");

      fw.close();
    } catch(IOException ex) {
      ex.printStackTrace();
    }
  }
}

