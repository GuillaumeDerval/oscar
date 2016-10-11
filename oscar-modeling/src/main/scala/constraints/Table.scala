package constraints

import algebra.IntExpression

/**
  * Table constraint
  * @param X
  * @param table
  */
case class Table(X: Array[IntExpression], table: Array[Array[Int]]) extends Constraint {}
case class NegativeTable(X: Array[IntExpression], table: Array[Array[Int]]) extends Constraint {}

object Table {
  def apply(x1: IntExpression, x2: IntExpression, tuples: Iterable[(Int, Int)]): Table = {
    Table(Array(x1, x2), tuples.map(t => Array(t._1, t._2)).toArray)
  }
  def apply(x1: IntExpression, x2: IntExpression, x3: IntExpression, tuples: Iterable[(Int, Int, Int)]): Table = {
    Table(Array(x1, x2, x3), tuples.map(t => Array(t._1, t._2, t._3)).toArray)
  }
  def apply(x1: IntExpression, x2: IntExpression, x3: IntExpression, x4: IntExpression, tuples: Iterable[(Int, Int, Int, Int)]): Table = {
    Table(Array(x1, x2, x3, x4), tuples.map(t => Array(t._1, t._2, t._3, t._4)).toArray)
  }
  def apply(x1: IntExpression, x2: IntExpression, x3: IntExpression, x4: IntExpression, x5: IntExpression, tuples: Iterable[(Int, Int, Int, Int, Int)]): Table = {
    Table(Array(x1, x2, x3, x4, x5), tuples.map(t => Array(t._1, t._2, t._3, t._4, t._5)).toArray)
  }
}

object NegativeTable {
  def apply(x1: IntExpression, x2: IntExpression, tuples: Iterable[(Int, Int)]): NegativeTable = {
    NegativeTable(Array(x1, x2), tuples.map(t => Array(t._1, t._2)).toArray)
  }
  def apply(x1: IntExpression, x2: IntExpression, x3: IntExpression, tuples: Iterable[(Int, Int, Int)]): NegativeTable = {
    NegativeTable(Array(x1, x2, x3), tuples.map(t => Array(t._1, t._2, t._3)).toArray)
  }
  def apply(x1: IntExpression, x2: IntExpression, x3: IntExpression, x4: IntExpression, tuples: Iterable[(Int, Int, Int, Int)]): NegativeTable = {
    NegativeTable(Array(x1, x2, x3, x4), tuples.map(t => Array(t._1, t._2, t._3, t._4)).toArray)
  }
  def apply(x1: IntExpression, x2: IntExpression, x3: IntExpression, x4: IntExpression, x5: IntExpression, tuples: Iterable[(Int, Int, Int, Int, Int)]): NegativeTable = {
    NegativeTable(Array(x1, x2, x3, x4, x5), tuples.map(t => Array(t._1, t._2, t._3, t._4, t._5)).toArray)
  }
}