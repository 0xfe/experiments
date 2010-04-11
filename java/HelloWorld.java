class HelloWorld {
  public static void main(String[] args) {
    System.out.println("Hello World!");
    System.out.println("I rule the world!");

    int x = 0;

    while (x < 10) {
      System.out.println("Why not? " + x);
      x++;

      if (x == 3) {
        System.out.println("Because x is 3");
      }
    }
  }
}
