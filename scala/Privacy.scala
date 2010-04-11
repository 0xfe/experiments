package com.muthanna.playground.scala

class PrivacyT(visible: Int)
  

object Privacy {
  def main(args: Array[String]) {
    var p = new PrivacyT(5)
    println(p.visible)
  }
}
