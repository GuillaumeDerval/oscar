
package object utils{
  
  def times(s: String)(block:  => Unit){
    val t0 = System.currentTimeMillis()
    block
    println("Executed " + s + " in " + (System.currentTimeMillis() - t0))
  }
  
}