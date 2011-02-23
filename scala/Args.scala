/*
  $ scalac-2.8 Args.scala
  $ scala-2.8 Args a b c
  A
  B
  C
*/

object Args {
  def main(args: Array[String]) {
    args.foreach(x => println(x toUpperCase));
  }
}
