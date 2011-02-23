package com.muthanna.playground.scala

object Timer {
  def oncePerSecond(callback: () => Unit) {
    while (true) { callback(); Thread sleep 1000 }
  }

  def main(args: Array[String]) {
    oncePerSecond(() =>
      println("Hello, world!"));
  }
}
