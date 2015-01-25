package com.ruskulis.algorithms

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest._

import scala.collection.immutable.Stack
import scala.util.Random

class MergeSortSpec extends FlatSpec with Matchers {
  "A sort" should "sort values" in {
    val s = Seq(1, 9, 4, 2)
    val sorted = MergeSort.sort[Int](s, {
      (a, b) =>
        if(a >= b) false
        else true
    })
    s.sorted should be (sorted)
  }
  "A sort" should "sort values in uneven" in {
    val s = Seq(1, 9, 4, 2, 999)
    val sorted = MergeSort.sort[Int](s, {
      (a, b) =>
        if(a >= b) false
        else true
    })
    s.sorted should be (sorted)
  }

  "A sort" should "sort values in equa" in {
    val s = Seq(1, 9, 4, 2, 999, 1)
    val sorted = MergeSort.sort[Int](s, {
      (a, b) =>
        if(a >= b) false
        else true
    })
    s.sorted should be (sorted)
  }

  "A sort" should "sort empty list" in {
    val s = Seq.empty[Int]
    val sorted = MergeSort.sort[Int](s, {
      (a, b) =>
        if(a >= b) false
        else true
    })
    s.sorted should be (sorted)
  }

  "A sort" should "sort list quick" in {
    val s = Stream.continually(Random.nextInt()).take(1000000).toSeq

    timeit("Built sin Done in") {
      s.sortWith{ (a, b) => a >= b}
    }

    timeit("MergeSort Done in") {
      MergeSort.sort[Int](s, {
        (a, b) => a <= b
      })
    }

    true should be (true)

  }

  def timeit(title: String)(f: => Unit) = {
    val start3 = System.nanoTime()
    f
    println(s"$title: \t${System.nanoTime() - start3}ns")
  }

  "A sort" should "list benchmark" in {

    val s = Stream.continually(0).take(10000000).toSeq

    timeit("Vector append") {
      s.foldLeft(Vector.empty[Int]) {
        (acc, a) => acc :+ a
      }
    }

    timeit("List prepend") {
      s.foldLeft(List.empty[Int]) {
        (acc, a) => a :: acc
      }.reverse
    }

    timeit("Stack append") {
      s.foldLeft(Stack.empty[Int]) {
        (acc, a) => acc.push(a)
      }
    }

    true should be (true)

  }


}
