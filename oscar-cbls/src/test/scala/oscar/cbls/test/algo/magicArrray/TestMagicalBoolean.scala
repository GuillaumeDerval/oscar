package oscar.cbls.test.algo.magicArrray

import oscar.cbls.algo.magicArray.MagicBoolArray

import scala.util.Random

/**
  * Created by  Jannou Brohée on 3/10/16.
  */
object TestMagicalBoolean extends App{

  def checkEqual(a:Array[Boolean],b:MagicBoolArray) {
    require(a.length == b.length)
    for (id <- a.indices) {
      require(a(id) == b(id))
    }
  }
  var reference: Array[Boolean] =  Array.ofDim(10000)
  var sut:MagicBoolArray  = MagicBoolArray(10000)
  val random = new Random()

  for(it <- 0 to 9999){
    val value = random.nextBoolean()
    reference(it)=value
    sut(it) = value

  }
  var iter = sut.indicesAtTrue
  var iterString :String ="["
  var iter2List = iter.toList
  iter= sut.indicesAtTrue
  while(iter.hasNext){
    val tmp:Int = iter.next()
    iterString+= tmp.toString
    if(iter.hasNext) iterString+=","
    require(sut(tmp))
  }
  iterString+="]"
  //check toString()
  require(sut.toString.equals(iterString))

  //check if there are elements  "true" which is not in the iterator
  for(n <- sut.indices){
    if(!iter2List.contains(n)) require(!sut(n))
  }

  checkEqual(reference,sut)
  var reference2: Array[Boolean] =  Array[Boolean](false,false,false,false,false,false,false,false,false,false)
  var reference3: Array[Boolean] =  Array[Boolean](true,true,true,true,true,true,true,true,true,true)
  sut  = MagicBoolArray(10)
  //val numIters = Int.MaxValue-2
  val numIters = 1000000
  var tim:Long = System.currentTimeMillis()
  for(it <- 1 to numIters){
    val value = random.nextBoolean()
    val indie = random.nextInt(reference.length)
    //println("set element at id :"+0+" to value :"+value)
    sut.update(0,value)
    //println("clearAll()")
    sut all_= false//==> false
    checkEqual(reference2,sut)
    //println("setAll()")
    sut all_= true //==> true
    checkEqual(reference3,sut)
  }
  //require(sut.  .global== -1)
  println(s"$numIters iterations ==>  ${System.currentTimeMillis()-tim}ms")
}
