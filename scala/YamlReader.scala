package com.muthanna.playground.scala

import org.yaml.snakeyaml._
import java.util.{Map => JavaMap}
import java.util.{HashMap => JavaHashMap}

// To build and run:
//
// $ fsc -cp support/snakeyaml/* YamlReader.scala
// $ scala -cp support/snakeyaml/*:. com.muthanna.playground.YamlReader

object Converter {
  def toJavaHashMap[T, U](map: Map[T,U]): JavaHashMap[T,U] = {
    var javamap = new JavaHashMap[T,U]
    for ((k, v) <- map) {
      javamap.put(k, v)
    }

    javamap
  }

  def toJavaMap[M[K, V] <: JavaMap[K, V], K, V](
    map: Map[K,V], javamap: M[K, V]): M[K, V] = {
    for ((k, v) <- map) {
      javamap.put(k, v)
    }

    javamap
  }
}

object YamlReader {
  def main(args: Array[String]) {
    var yaml = new Yaml
    var data = yaml.load("a: 1\nb: 2\n")

    println(data)

    var map: Map[String, Int] = Map("a" -> 1, "b" -> 5)
    println(yaml.dump(Converter.toJavaHashMap(map)))

    // var javamap: JavaHashMap[String, Int] = new JavaHashMap
    // println(yaml.dump(Converter.toJavaMap(map, javamap)))
  }
}
