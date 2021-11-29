package com.github.cexj.dobble

import scala.annotation.tailrec
import scala.collection.SortedMap

object WrongDeckCreator {

  def create(symbols: Int, cardSize: Int): Seq[Seq[Int]] = {
    @tailrec
    def createTR(symbol: Int, deck: Seq[Seq[Int]], partialCards: SortedMap[Int, Seq[Seq[Int]]]): Seq[Seq[Int]] = {

      if(symbol > symbols) {
        deck
      } else {
        val newMapCandidates = SortedMap.from((0 to cardSize).map(i => i -> (partialCards.getOrElse(i-1, Seq.empty).map(symbol +: _) ++ partialCards.getOrElse(i, Seq.empty))))
        val candidates = newMapCandidates.getOrElse(cardSize, Seq.empty)
        val partialCandidates = newMapCandidates.dropRight(1)
        val newDeck = addToDeck(deck, candidates)
        val newPartialCards = refinePartialCards(partialCandidates, deck, symbols - symbol, cardSize)
        createTR(symbol + 1, newDeck, newPartialCards)
      }
    }
    createTR(1, Seq.empty, SortedMap(0 -> Seq(Seq.empty)))
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
    deck.map(card => card.intersect(candidate).size).exists(_ != 1)
  }

  def localCheck(deck: Seq[Seq[Int]], candidate: Seq[Int]): Boolean = {
    deck.map(card => check(card, candidate)).exists(_ > 1)
  }

}
