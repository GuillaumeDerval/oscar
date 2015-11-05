package oscar.cbls.invariants.core.algo.tarjan

import oscar.cbls.invariants.core.algo.quick.QList

class TarjanNodeData{
  var Index:Int = -1
  var LowLink:Int = -1
  var OnStack:Boolean = false
}

/** The Tarjan algorithm for detecting SCC is graphs
  * This version is faster because it does not use dictionaries.
  * all data are stored in the nodes.
  * @author renaud.delandtsheer@cetic.be
  */
object TarjanWithExternalStorage{

  def getStronlyConnexComponents[T](Nodes:Iterable[T], GetSucceedingNodes:T => Iterable[T], getNodeStorage:T=>TarjanNodeData):List[QList[T]] = {
    var index:Int=0
    var Stack:QList[T]=null
    var Components:List[QList[T]]= List.empty

    def visit(v:T){
      val storageForV = getNodeStorage(v)
      storageForV.Index = index
      storageForV.LowLink = index
      index +=1
      Stack = QList(v,Stack)
      storageForV.OnStack = true

      // Consider successors of v
      for(w <- GetSucceedingNodes(v)){
        val storageForW = getNodeStorage(w)
        if(storageForW.Index == -1){
          // Successor w has not yet been visited; recurse on it
          visit(w)
          storageForV.LowLink = storageForV.LowLink.min(storageForW.LowLink)
        }else if(storageForW.OnStack){
          // Successor w is in stack S and hence in the current SCC
          storageForV.LowLink = storageForV.LowLink.min(storageForW.Index)
        }
      }

      // If v is a root node, pop the stack and generate an SCC
      if (storageForV.LowLink == storageForV.Index){
        //start a new strongly connected component
        var SCC:QList[T] = null
        var finished:Boolean = false
        while(!finished){
          val node = Stack.head
          val storageForNode = getNodeStorage(node)
          Stack = Stack.tail
          storageForNode.OnStack = false
          SCC = QList(node,SCC)
          finished = (node == v)
        }
        Components = SCC :: Components
      }
    }

    for(n <- Nodes) {if(getNodeStorage(n).Index == -1) visit(n)}

    Components
  }
}
