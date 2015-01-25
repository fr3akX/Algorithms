package com.ruskulis.algorithms

import scala.collection.immutable.Queue

/**
 * Merge Sort implementation
 */
object MergeSort {
  /**
   *
   * @param seq sequence to sort
   * @param lt comparision function
   * @tparam T type parameter
   * @return
   */
  def sort[T](seq: Seq[T], lt: (T, T) => Boolean): Seq[T] = {

    def merge(left: Seq[T], right: Seq[T], accum: Queue[T]): Seq[T] = {
      if(left.isEmpty) {
        right.foldLeft(accum) {
          (acc, r) => acc.enqueue(r)
        }
      }else if(right.isEmpty){
        left.foldLeft(accum) {
          (acc, r) => acc.enqueue(r)
        }
      }else if (lt(left.head, right.head)){
        //adding left
        merge(left.tail, right, accum.enqueue(left.head))
      }else {
        //adding right
        merge(left, right.tail, accum.enqueue(right.head))
      }
    }

    if(seq.length <= 1) seq //base case consider sorted
    else {
      // divide in equal sublists
      val middle = seq.length / 2
      val (left, right) = seq.splitAt(middle)

      val sleft: Seq[T] = sort(left, lt)
      val sright: Seq[T] = sort(right, lt)
      merge(sleft, sright, Queue.empty[T])
    }
  }
}
