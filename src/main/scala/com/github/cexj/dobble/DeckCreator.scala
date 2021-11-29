package com.github.cexj.dobble

import com.github.cexj.dobble.Dobble.symbols
import com.github.cexj.dobble.Mod7.{intersectionMod7, normalForm}

object Dobble {

  val symbols =  {
    val all = for {
      x <- 0 to 6
      y <- 0 to 6
      z <- 0 to 6
    } yield Vector3(x,y,z)

    all.foldLeft(Seq[Vector3]()){
      case (deck, vector) => normalForm(vector) match {
        case Some(value) => value +: deck
        case None => deck
      }
    }.toSet
  }

  val cards = symbols.zip(symbols).map{ case (x,y) => intersectionMod7(x,y) }

}

case class Vector3(x: Int, y: Int, z: Int) {
  def **(v2: Vector3): Int = (v2.x * x) + (v2.y * y) + (v2.z * z)
}

object Mod7 {

  def intersectionMod7(v1: Vector3, v2: Vector3): Set[Vector3] = {
    symbols.filter { card =>
      v1 ** card % 7 == 0 && v2 ** card % 7 == 0
    }.map(normalForm(_).get)
  }


  def normalForm(projectivePoint: Vector3): Option[Vector3] = projectivePoint  match {
    case Vector3(0,0,0) => None
    case Vector3(0,0,_) => Some(Vector3(0, 0, 1))
    case Vector3(0,_,0) => Some(Vector3(0, 1, 0))
    case Vector3(0,n,m) =>
      val inverse = (0 to 6).find(x => (x * n) % 7 == 1).get
      Some(Vector3(0, 1, inverse*m %7))
    case Vector3(n,m,o) =>
      val inverse = (0 to 6).find(x => (x * n) % 7 == 1).get
      Some(Vector3(1, inverse*m %7,inverse*o %7))
  }
}