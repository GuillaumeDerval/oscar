package oscar.cbls.search.algo

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

object Pairs {

  /**
   * @param l a list
   * @return a list of all pairs of element made from two elements in l
   */
  def makeAllUnsortedPairs(l:List[Int]):List[(Int,Int)] = {
    def makeAllUnsortedPairsWithHead(head:Int, tail:List[Int], toAppend:List[(Int,Int)]):List[(Int,Int)] = {
      tail match{
        case other :: newTail => makeAllUnsortedPairsWithHead(head, newTail, (head,other) :: toAppend)
        case Nil => toAppend
      }
    }

    l match{
      case Nil => List.empty
      case head :: tail => makeAllUnsortedPairsWithHead(head,tail,makeAllUnsortedPairs(tail))
    }
  }

  def makeAllSortedPairs(l:List[Int]):List[(Int,Int)] = {
    def makeAllSortedPairsWithHead(head:Int,
                                   tail:List[Int],
                                   toAppend:List[(Int,Int)]):List[(Int,Int)] = {
      tail match{
        case Nil => toAppend
        case other::newTail => (head,other) :: makeAllSortedPairsWithHead(head,newTail,toAppend)
      }
    }
    l match{
      case Nil => Nil
      case h::t => makeAllSortedPairsWithHead(h,t,makeAllSortedPairs(t))
    }
  }

  def makeAllHeadAndTails[T](l:List[T]):List[(T,List[T])] = {
    l match{
      case Nil => Nil
      case h::t => (h,t) :: makeAllHeadAndTails(t)
    }
  }

  def zipIntoAllPossiblePairs[L,T](l:List[L],t:List[T]):List[(L,T)] = {
    l match{
      case Nil => Nil
      case hl::lt =>
        def myAggregate(lh:L,t:List[T]):List[(L,T)] = {
          t match {
            case ht :: tt => (lh, ht) :: myAggregate(lh, tt)
            case Nil => zipIntoAllPossiblePairs(lt,t)
          }
        }
        myAggregate(hl,t)
    }
  }
}
