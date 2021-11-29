package com.github.cexj.dobble

import scala.collection.SortedMap

object WrongDeckCreator extends App {

  val n = 4
  val x = create(n*n-n+1 , n)
  println(x)
  println(x.size)



  def create(symbols: Int, cardSize: Int): Seq[Seq[Int]] = {
    def createTR(symbol: Int, deck: Seq[Seq[Int]], partialCards: SortedMap[Int, Seq[Seq[Int]]]): Seq[Seq[Int]] = {

      if(symbol > symbols) {
        deck
      } else {
        val newMapCandidates = SortedMap.from((0 to cardSize).map(i => i -> (partialCards.getOrElse(i-1, List()).map(symbol +: _) ++ partialCards.get(i).getOrElse(List()))))
        println("newMapCandidates: " + newMapCandidates)
        val candidates = newMapCandidates.get(cardSize).getOrElse(List())
        println("candidates: " + candidates)
        val partialCandidates = newMapCandidates.dropRight(1)
        println("partialCandidates: " + partialCandidates)
        val newDeck = addToDeck(deck, candidates)
        println("newDeck: " + newDeck)
        val newPartialCards = refinePartialCards(partialCandidates, deck, symbols - symbol, cardSize)
        println("newPartialCards: " + newPartialCards)
        createTR(symbol + 1, newDeck, newPartialCards)
      }
    }
    createTR(1, Seq(), SortedMap(0 -> Seq(Seq())))
  }

  def addToDeck(deck: Seq[Seq[Int]], candidates: Seq[Seq[Int]] ): Seq[Seq[Int]] = {
    candidates.foldLeft(deck){ (deck, candidate) =>
      if(globalCheck(deck, candidate)) candidate +: deck
      else deck
    }
  }

  def refinePartialCards(mapPartialCandidates: SortedMap[Int, Seq[Seq[Int]]], deck: Seq[Seq[Int]], remainingSymbolsSize: Int, cardSize: Int ): SortedMap[Int, Seq[Seq[Int]]] = {
    mapPartialCandidates
      .collect{
        case (size, partialCandidates) if size + remainingSymbolsSize >= cardSize =>
          size -> partialCandidates.filter(localCheck(deck,_))
      }
  }

  def check(card1: Seq[Int], card2: Seq[Int]): Int = {
    card1.intersect(card2).size
  }

  def globalCheck[Int](deck: Seq[Seq[Int]], candidate: Seq[Int]): Boolean = {
    deck.map(card => card.intersect(candidate).size).filter(_ != 1).isEmpty
  }

  def localCheck(deck: Seq[Seq[Int]], candidate: Seq[Int]): Boolean = {
    deck.map(card => check(card, candidate)).filter(_ > 1).isEmpty
  }

}
