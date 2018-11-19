package oscar.cbls.business

import org.locationtech.jts.geom.GeometryFactory

package object geometry {

  //this is the general factory to use, it is kept in floats because putting it to integers
  // only adds an extra rounding at the end of each operation.
  //so better to keep it all in floats, and perform any conversion when geting things out of JTS
  val factory:GeometryFactory = new GeometryFactory()

}
