public class PhraseOMatic {
  public static void main(String[] args) {
    String[] wordlist1 = { "Say Goodnight",
                           "To",
                           "The",
                           "Bad Guy" };

    int random = (int) (Math.random() * wordlist1.length);

    System.out.println("Random = " + random);
    System.out.println("Length = " + wordlist1.length);
    System.out.println(wordlist1[random]);
  }
}
