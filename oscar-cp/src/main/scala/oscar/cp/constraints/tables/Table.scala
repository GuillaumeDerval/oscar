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

import oscar.cp._
import oscar.cp.core._
import oscar.cp.core.CPOutcome._
import oscar.cp.constraints.TableSTR2
import oscar.cp.constraints.TableAC5TCRecomp
import oscar.cp.constraints.TableData

object TableAlgo extends Enumeration {
    type TableAlgo = Value
    val CompactTable = Value("CompactTable (Perron algo)")
    val GAC4 = Value("GAC4 (Regin algo)")
    val GAC4R = Value("GAC4R (Perez and Regin algo")
    val STR2 = Value("STR2 (Lecoutre algo)")
    val STR3 = Value("STR3 (Lecoutre algo)")
    val MDD4R = Value("MDD4R (Perez and Regin algo)")
    val AC5TCRecomp = Value("AC5TCRecomp (Mairy et al algo)")
}



object table {
  
  def apply(X: Array[CPIntVar], table: Array[Array[Int]], algo: TableAlgo.Value = TableAlgo.CompactTable): Constraint = {
    import oscar.cp.constraints.tables.TableAlgo._
    
    algo match {
      case CompactTable => compactTable(X,table)
      case GAC4 => gac4(X,table)
      case GAC4R => gac4r(X,table)
      case MDD4R => mdd4r(X,table)
      case STR2 => str2(X,table)
      case STR3 => str3(X,table)
      case AC5TCRecomp => ac5tcRecomp(X,table)
      case _ => compactTable(X,table)
    }
  }
  
  def compactTable(X: Array[CPIntVar], table: Array[Array[Int]]): Constraint = new TableCT(X,table)
  
  def gac4(X: Array[CPIntVar], table: Array[Array[Int]]): Constraint = new TableGAC4(X,table)
  
  def gac4r(X: Array[CPIntVar], table: Array[Array[Int]]): Constraint = new TableGAC4R(X,table)
  
  def mdd4r(X: Array[CPIntVar], table: Array[Array[Int]]): Constraint = new TableMDD4R(X,table)

  def str2(X: Array[CPIntVar], table: Array[Array[Int]]): Constraint = new TableSTR2(X,table)
  
  def str3(X: Array[CPIntVar], table: Array[Array[Int]]): Constraint = new TableSTR3(X,table)

  def ac5tcRecomp(X: Array[CPIntVar], table: Array[Array[Int]]): Constraint = {
    val data = new TableData(X.size)
    table.foreach(t => data.add(t: _*))
    new TableAC5TCRecomp(data, X: _*)
  }
  
  

}
