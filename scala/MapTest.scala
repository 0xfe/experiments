package com.muthanna.playground.scala

object MapTest {
  def main(args: Array[String]) {
    var capital = Map("US" -> "DC", "India" -> "Delhi")
    capital += ("Canada" -> "Ottawa")
    println(capital("India"))
  }
}
