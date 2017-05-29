/*
package oscar.cbls.lib.invariant.routing.capa



/**
 * Created by rdl on 18-05-17.
 */
abstract class IntegerVehicleCapacity {

  registerStaticAndDynamicDependency(routes)
  registerStaticAndDynamicDependencyArrayIndex(contentAtStart)
  finishInitialization()
  for(i <- contentAtNode) i.setDefiningInvariant(this)
  for(i <- contentAtEnd) i.setDefiningInvariant(this)


  def getVehicleContentAtNode(node: Int): Int

  /**
   * sets the conent of the vehicle at node "node"
   * if a node is unrouted, you should call setVehicleContentToUnroutedNode instead
   * @param node the node
   * @param newValueAtNode the new value for the content of the vehicle at this node
   */
  def setVehicleContentAtNode(node: Int, newValueAtNode: Int) : Unit

  def getContentAtVehicleStart(vehicle:Int): Int

  def setVehicleContentAtEnd(vehicle:Int,content:Int)

}
*/