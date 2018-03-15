package oscar.visual.map

import java.io.BufferedReader
import java.io.InputStreamReader
import java.net.URL
import java.net.URLEncoder
import java.io.IOException

object Geocoder {
  
  val ENCODING: String = "UTF-8"
  val KEY: String = "xyz"
    
  def getLocation(address: String): Location = {
    val input = new BufferedReader(new InputStreamReader (new URL ("http://maps.google.com/maps/geo?q="+URLEncoder.encode(address, ENCODING)+"&output=csv&key="+KEY).openStream ()))
    var location: Location = null
    var statusCode = -1
    var line = input.readLine()
    while(line != null) {
      // Format: 200,6,42.730070,-73.690570
      statusCode = line.substring(0, 3).toInt
      if (statusCode == 200) {
        location = Location(line.substring ("200,6,".length, line.indexOf(',', "200,6,".length)), line.substring (line.indexOf(',', "200,6,".length)+1, line.length))
      }
      line = input.readLine()
    }
    if (location == null) {
     statusCode match {
       case 400 => throw new IOException("Bad request")
       case 500 => throw new IOException("Unknown error from Google Encoder")
       case 601 => throw new IOException("Missing query")
       case 602 => return null
       case 603 => throw new IOException("Legal problem")
       case 604 => throw new IOException("No route")
       case 610 => throw new IOException("Bad key")
       case 620 => throw new IOException("Too many queries")
     }
    }
    location
  }
}

object GeocoderExample extends App {
  System.out.println(Geocoder.getLocation("New York"))
}
