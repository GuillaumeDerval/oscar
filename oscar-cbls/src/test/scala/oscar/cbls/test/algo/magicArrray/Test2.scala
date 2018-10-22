package oscar.cbls.test.algo.magicArrray

import oscar.cbls.algo.magicArray.{IterableMagicBoolArray, MagicBoolArray}

import scala.util.Random

/**
  * Created by  Jannou Brohée on 3/10/16.
  */
object Test2 extends App {

  val n = 100

  var reference: Array[Boolean] = Array.fill(n)(false)

  val sut: MagicBoolArray = MagicBoolArray(n,false)
  val sut2: IterableMagicBoolArray = IterableMagicBoolArray(n,false)

  def checkEqual() {
    require(reference.length == sut.length)
    for (id <- sut.indices) {
      require(reference(id) == sut(id))
      require(reference(id) == sut2(id))
    }

    val indicesAtTrue = reference.indices.filter(reference(_)).toList

    for(i <- indicesAtTrue){
      require(sut(i))
      require(sut2(i))
    }

    val indicesAtFalse = reference.indices.filter(!reference(_)).toList

    for(i <- indicesAtFalse){
      require(!sut(i))
      require(!sut2(i))
    }


    require(sut.indicesAtTrue.length == indicesAtTrue.length)
    require(sut2.indicesAtTrue.length == indicesAtTrue.length)
  }

  val random = new Random()

  for (it <- 0 to 10*n) {

    if(it % n == 0){
      val value = random.nextBoolean()
      reference = Array.fill(n)(value)
      sut.all = value
      sut2.all = value

    }else {
      val value = random.nextBoolean()
      val indice = random.nextInt(reference.length)

      reference(indice) = value
      sut(indice) = value
      sut2(indice) = value
    }
    checkEqual()
  }


  println("done")
}