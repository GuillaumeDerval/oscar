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
package oscar.visual.map

import java.net.URL
import scala.io.Source
import scala.collection.mutable.{ HashMap, SynchronizedMap }
import scala.xml._
import java.io.IOException
import java.awt.Color

/*
 * Representation of a point defined by coordinates
 */
case class MapPoint(lat : Double, long : Double) {
    override def toString() = lat+","+long
}

/*
 * Mapquest web api requests
 */
object MapQuest {
  val baseUrl = "http://open.mapquestapi.com/directions/v1/route?outFormat=xml&shapeFormat=raw&generalize=200"
  private val cache = new HashMap[(MapPoint, MapPoint), List[MapPoint]] with SynchronizedMap[(MapPoint, MapPoint), List[MapPoint]]
  
	/*
	 * Perform the http request and extract path from response
	 */
    def getPath(orig: MapPoint, dest : MapPoint) : List[MapPoint] = 
    {
		  cache.getOrElseUpdate((orig, dest), {
		  val xmldata = getRoute(orig, dest)
		  if((xmldata \ "info" \ "statusCode").text != "0") throw new IOException ("Error retrieving path from mapquest");
		  else {
		    (xmldata \ "route" \ "shape" \ "shapePoints" \ "latLng").map(node => new MapPoint((node \ "lat").text.toDouble, (node \ "lng").text.toDouble)).toList
		  }}
		  )
    }
  	
  	/*
  	 * Http request to get an xml element containing the path
  	 */
  	private def getRoute(orig: MapPoint, dest : MapPoint) : Elem = {
  	  val answer = (new URL(baseUrl + "&from=" + orig + "&to=" + dest)).openStream()
  	  XML.loadString(Source.fromInputStream(answer).mkString(""))
  	}
}


/*
 * Represent a path from orig to dest as a list of MapLines
 */
class MapPath(m : VisualMap, o: MapPoint, d : MapPoint, col : Color = Color.BLACK) {
  val map = m
  private var _orig = o
  private var _dest = d
  private var _lines = List[MapLine]()
  
  val color = col
  
  
  refreshLines()
  
  
  /*
   * get mapquest path from orig to dest
   */
  private def refreshLines() = {
 	val waypoints = MapQuest.getPath(_orig, _dest)
 	//build lines ignoring "empty" shapes
 	_lines = (for(i <- 0 until waypoints.length-2 if waypoints(i).lat != waypoints(i+1).lat || waypoints(i).long != waypoints(i+1).long) 
 	  yield new MapLine(map, waypoints(i).lat, waypoints(i).long, waypoints(i+1).lat, waypoints(i+1).long, color)).toList
  }
  
  // constructor without using internal MapPoint structure
  def this(map : VisualMap, origlat : Double, origlong: Double, destlat : Double, destlong : Double, col : Color) = 
    this(map, new MapPoint(origlat,origlong), new MapPoint(destlat, destlong), col) 
  
  // constructor without using internal MapPoint structure
  def this(map : VisualMap, origlat : Double, origlong: Double, destlat : Double, destlong : Double) = 
    this(map, new MapPoint(origlat,origlong), new MapPoint(destlat, destlong), Color.BLACK)     
    
  def remove = map.removePath(this)
  /*
   *  change destination
   */
  def dest_=(d: (Double, Double)): Unit = {
    if(d._1 != _dest.lat || d._2 != _dest.long) {
      _dest = new MapPoint(d._1, d._2)
      refreshLines()
      map.viewer.repaint();
    }      
  }
  
  /*
   *  change origin
   */
  def orig_=(o: (Double,Double)): Unit =  {
    if(o._1 != _orig.lat || o._2 != _orig.long) {
      _orig = new MapPoint(o._1, o._2)
      refreshLines()
      map.viewer.repaint();
    }      
  }
  
  def dest = (_dest.lat, _dest.long)
  def orig = (_orig.lat, _orig.long)
  
  def lines = _lines
  
}


object MapPath {
  
  def main(args : Array[String]) {
    val mp = new MapPath(new VisualMap(), 50.466246,4.869278,50.708634,4.572647)
    println(mp._lines.map(lin => "["+lin.orig._1+","+lin.orig._2+" to "+lin.dest._1+","+lin.dest._2+"]")) 
    
  }
}


