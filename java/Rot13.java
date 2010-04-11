/* 
Rot13.java - Deep military-grade encryption classes.
NSA - 2007
*/

// Our abstract crypto class. This should really be an
// interface.
abstract class Crypto {
  protected String data = "";

  String set_data(String text) {
    return data = text;
  }

  String get_data() {
    return data;
  }

  abstract void encrypt();
  abstract void decrypt();
}

// RotCrypto: Implements a simp^H^H^H^Hcomplicated elliptical rotational
// cryptography algorighm.
class RotCrypto extends Crypto {
  static private final int CH_A = (int) 'A';
  static private final int CH_Z = (int) 'Z';

  static private final int CH_a = (int) 'a';
  static private final int CH_z = (int) 'z';

  private int rotation = 13;

  RotCrypto(int rot) {
    rotation = rot;
  }

  static RotCrypto getRot13CryptoInstance() {
    return new RotCrypto(13);
  }

  void encrypt() {
    data = do_encrypt(rotation, data);
  }

  void decrypt() {
    data = do_encrypt(-rotation, data);
  }

  private String do_encrypt(int rot, String text) {
    StringBuffer result = new StringBuffer(text.length());

    for (char c : text.toCharArray()) {
      int CH_c = (int) c;
      int new_char = CH_c + rot;

      if (Character.isUpperCase(c)) {
        if (new_char > CH_Z) {
          new_char = CH_A + (new_char - CH_Z);
        } else if (new_char < CH_A) {
          new_char = CH_Z - (CH_A - new_char);
        }
      } else if (Character.isLowerCase(c)) {
        if (new_char > CH_z) {
          new_char = CH_a + (new_char - CH_z);
        } else if (new_char < CH_a) {
          new_char = CH_z - (CH_a - new_char);
        }
      } else {
        new_char = CH_c;
      }

      result.append((char) new_char);
    }

    return result.toString();
  }
}

public class Rot13 {
  public static void main(String[] args) {
    RotCrypto crypto = RotCrypto.getRot13CryptoInstance();

    if (args.length == 0) {
      System.out.println("Usage: Rot13 'string_to_encrypt'");
      return;
    }

    crypto.set_data(args[0]);

    crypto.encrypt();
    System.out.println(crypto.get_data());

    crypto.decrypt();
    System.out.println(crypto.get_data());
  }
}
