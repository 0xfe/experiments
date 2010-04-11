public class DogTester {
  public static void main(String[] args) {
    Dog dog = new Dog();
    dog.vigour = 4;
    dog.name = "Rufus";

    dog.bark();

    Dog nodog = new Dog();
    nodog.bark();
  }
}
