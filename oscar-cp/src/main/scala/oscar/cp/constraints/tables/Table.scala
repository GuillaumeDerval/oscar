/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/

package oscar.cp.constraints.tables

import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar

/**
  * @author Pierre Schaus pschaus@gmail.com
  */
object TableAlgo extends Enumeration {
  type TableAlgo = Value
  val CompactTable = Value("CompactTable (Perron et al)")
  val CompactTableGAC6 = Value("CompactTable GAC6 (Régin,Perrez,Schaus)")
  val CompactTableRefactored = Value("CompactTable Refactored")
  val CompactTableStar = Value("CompactTable for positive * table")
  val GAC4 = Value("GAC4 (Regin)")
  val GAC4R = Value("GAC4R (Perez and Regin")
  val STR2 = Value("STR2 (Lecoutre)")
  val STR3 = Value("STR3 (Lecoutre)")
  val MDD4R = Value("MDD4R (Perez and Regin)")
  val AC5TCRecomp = Value("AC5TCRecomp (Mairy et al)")
  val Decomp = Value("Basic Decomposition")
  val ShortSTR2 = Value("ShortSTR2")
}

object table {

  def apply(X: Array[CPIntVar], table: Array[Array[Int]], algo: TableAlgo.Value = TableAlgo.CompactTable): Constraint = {
    import oscar.cp.constraints.tables.TableAlgo._

    algo match {
      case CompactTable => compactTable(X, table)
      case CompactTableGAC6 => compactTableGAC6(X, table)
      case CompactTableStar => compactTableStar(X, table)
      case GAC4         => gac4(X, table)
      case GAC4R        => gac4r(X, table)
      case MDD4R        => mdd4r(X, table)
      case STR2         => str2(X, table)
      case STR3         => str3(X, table)
      case AC5TCRecomp  => ac5tcRecomp(X, table)
      case Decomp       => decomp(X, table)
      case ShortSTR2    => shortSTR2(X,table)
      case _            => compactTable(X, table)
    }
  }

  def compactTable(X: Array[CPIntVar], table: Array[Array[Int]]): Constraint = new TableCT(X, table)

  def compactTableGAC6(X: Array[CPIntVar], table: Array[Array[Int]]): Constraint = new TableCTAC6(X, table)

  def compactTableStar(X: Array[CPIntVar], table: Array[Array[Int]]): Constraint = new TableCTStar(X, table)

  def gac4(X: Array[CPIntVar], table: Array[Array[Int]]): Constraint = new TableGAC4(X, table)

  def gac4r(X: Array[CPIntVar], table: Array[Array[Int]]): Constraint = new TableGAC4R(X, table)

  def mdd4r(X: Array[CPIntVar], table: Array[Array[Int]]): Constraint = new TableMDD4R(X, table)

  def str2(X: Array[CPIntVar], table: Array[Array[Int]]): Constraint = new TableSTR2(X, table)

  def str3(X: Array[CPIntVar], table: Array[Array[Int]]): Constraint = new TableSTR3(X, table)

  def decomp(X: Array[CPIntVar], table: Array[Array[Int]]): Constraint = new TableDecomp(X, table)

  def shortSTR2(X: Array[CPIntVar], table: Array[Array[Int]]): Constraint = new TableShortSTR2(X, table)

  def ac5tcRecomp(X: Array[CPIntVar], table: Array[Array[Int]]): Constraint = {
    val data = new TableData(X.size)
    table.foreach(t => data.add(t: _*))
    new TableAC5TCRecomp(data, X: _*)
  }
}


object NegativeTableAlgo extends Enumeration {
  type NegativeTableAlgo = Value
  val STRNE = Value("STRNE (Hongbo Li et al)")
  val CompactTableNegative = Value("CompactTable for negative table")
  val CompactTableNegativeStar = Value("CompactTable for negative * table")
}

object negativeTable {

  def apply(X: Array[CPIntVar], invalidTuples: Array[Array[Int]], algo: NegativeTableAlgo.Value = NegativeTableAlgo.STRNE): Constraint = {
    import oscar.cp.constraints.tables.NegativeTableAlgo._

    algo match {
      case STRNE => str2ne(X, invalidTuples)
      case CompactTableNegative => compactTableNegative(X, invalidTuples)
      case CompactTableNegativeStar => compactTableNegativeStar(X, invalidTuples)
      case _     => str2ne(X, invalidTuples)
    }
  }


  def str2ne(X: Array[CPIntVar], table: Array[Array[Int]]): Constraint = new TableSTRNe(X, table)

  def compactTableNegative(X: Array[CPIntVar], table: Array[Array[Int]]): Constraint = new TableCTNeg(X, table)

  def compactTableNegativeStar(X: Array[CPIntVar], table: Array[Array[Int]]): Constraint = new TableCTNegStar(X, table)

}

