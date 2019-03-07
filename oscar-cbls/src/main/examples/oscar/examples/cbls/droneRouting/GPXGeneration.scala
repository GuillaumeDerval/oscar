package oscar.examples.cbls.routing.drone

import oscar.cbls.algo.seq.IntSequence

import scala.xml._

class GPXGeneration(v: Int,
                    routes: IntSequence,
                    nodePositions: Array[(Double,Double)]) {

  var myGPX = <gpx author="oscar" desc=""></gpx>

  def toBeAddedWaypoint(point: (Double,Double),
                        alt: Int,
                        vehicle: String,
                        sym: String):Elem = {
    <wpt lat={point._2.toString} lon={point._1.toString}>
      <ele>{ alt }</ele>
      <name>{ vehicle }</name>
      <sym>{ sym }</sym>
    </wpt>
  }

  def toBeAddedTrace(point1: (Double,Double),
                     point2 : (Double,Double),
                     alt:Int,
                     alt2: Int):Elem = {
    <trkseg>
      <trkpt lat={point1._2.toString} lon={point1._1.toString}>
        <ele>{ alt }</ele>
      </trkpt>
      <trkpt lat={point2._2.toString} lon={point2._1.toString}>
        <ele>{ alt2 }</ele>
      </trkpt>
    </trkseg>
  }

  //How to add a new Node waypoint
  def addNewWaypoint(originalXML: Elem,
                  point: (Double,Double),
                  alt: Int,
                  vehicle: String,
                  sym: String):Unit = {
    myGPX = originalXML match {
      case <gpx>{ children @ _* }</gpx> =>
          <gpx> {
          children ++ toBeAddedWaypoint(point,alt,vehicle, sym)
          }</gpx>
      case other => other
    }
  }

  //How to add a new Node trace
  def addNewTrace(originalXML: Elem,
                     point1: (Double,Double),
                     point2: (Double,Double),
                     alt: Int,
                     alt2: Int):Unit = {
    myGPX = originalXML match {
      case <gpx>{ children @ _* }</gpx> =>
        <gpx> {
          children ++ toBeAddedTrace(point1,point2,alt,alt2)
          }</gpx>
      case other => other
    }
  }

  def extractRouteOfVehicle(vehicle: Int,
                            routes: IntSequence) : List[Int] = {
    //println("extractRouteOfVehicle("+vehicle + " route " + routes)
    var reversedRoute = List[Int](vehicle)
    var explorer = routes.explorerAtAnyOccurrence(vehicle).head
    while(explorer.next match{
      case None =>
        // we are at the end of the road of the last vehicle
        false
      case Some(next) =>
        if(next.value < v){
          // we are at the beginning of the next vehicle route
          // we come back to the starting point of the vehicle
          false
        }else{
          // we're still on the road to the vehicle vehicle
          reversedRoute = next.value :: reversedRoute
          explorer = next
          true
        }
    }){}
    reversedRoute = vehicle :: reversedRoute
    reversedRoute.reverse
  }

  def writeAndSave(): Unit ={

    for (vehicle <- 0 until v){
      val route = extractRouteOfVehicle(vehicle,routes)
      for(i <- 0 until route.length){
        addNewWaypoint(myGPX,nodePositions(route(i)),0,"vehicle: "+vehicle+" and node:" + route(i),"node:" + route(i))
      }
      for(i <- 0 until route.length-1){
        addNewTrace(myGPX,nodePositions(route(i)),nodePositions(route(i+1)),0,0)
      }
      XML.save("myGPXdrone"+vehicle+".gpx",myGPX,"utf-8",xmlDecl = true)
      val desc = "waypoints for vehicle " + vehicle
      myGPX = <gpx author="oscar" desc={desc}></gpx>
    }

    for (vehicle <- 0 until v){
      val route = extractRouteOfVehicle(vehicle,routes)
      for(i <- 0 until route.length){
        addNewWaypoint(myGPX,nodePositions(route(i)),0,"vehicle: "+vehicle+" and node:" + route(i),"node:" + route(i))
      }
      for(i <- 0 until route.length-1){
        addNewTrace(myGPX,nodePositions(route(i)),nodePositions(route(i+1)),0,0)
      }
      XML.save("myGPXAllDrone.gpx",myGPX,"utf-8",xmlDecl = true)
    }
  }
}
