class Dog {
  int vigour;
  String name;

  public Dog() {
    name = "Nobody";
    vigour = 1;
  }

  public void bark() {
    String bark = "";
    int x = 0;

    for (x = 0; x < vigour; x++) {
      bark += "Ruff!! ";
    }

    System.out.println(name + ": " + bark);
  }
}
