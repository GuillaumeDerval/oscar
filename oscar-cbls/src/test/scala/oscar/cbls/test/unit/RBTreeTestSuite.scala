package oscar.cbls.test.unit

import org.scalacheck.{Gen, Shrink}
import org.scalatest.{FunSuite, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import oscar.cbls.algo.rb.{RedBlackTreeMap, T}

import scala.util.Random

class RBTreeTestSuite extends FunSuite with GeneratorDrivenPropertyChecks with Matchers {

  test("empty tree has size 0 and no values"){
    val tree = RedBlackTreeMap.empty[Int]
    tree.size         should be (0)
    tree.values.size  should be (0)
  }

  test("tree.get on empty tree should be None"){
    val tree = RedBlackTreeMap.empty[Int]
    tree.get(2) should be (None)
  }

  test("tree initialized with makeFromSorted has expected size and content"){
    forAll(sequentialTuplesList){ list =>

      val tree = RedBlackTreeMap.makeFromSorted(list)

      val expl = tree.biggestPosition.get


      tree.size should be (list.size)
      tree.values should be (list.map(_._2))
      tree.values.forall(e => list.map(_._2).contains(e)) should be (true)
    }
  }

  test("tree initialized with makeFromContinuousSorted has expected size and content"){
    forAll(sequentialTuplesList){ list =>

      val arrayValues = list.map(_._2).toArray
      val tree = RedBlackTreeMap.makeFromSortedContinuousArray(arrayValues)

      tree.size should be (list.size)
      tree.values should be (list.map(_._2))
      tree.values.forall(e => list.map(_._2).contains(e)) should be (true)
    }
  }

  test("tree.get returns expected element"){
    forAll(sequentialTuplesList){ list =>

      val tree = RedBlackTreeMap.makeFromSorted(list)
      for(randomTuple <- Random.shuffle(list)){
        tree.get(randomTuple._1).get should be (randomTuple._2)
      }
    }
  }

  test("tree.biggestLowerOrEqual returns expected element"){
    forAll(nonSequentialTuplesList){ list =>
      whenever(list.nonEmpty){
        val tree = RedBlackTreeMap.makeFromSorted(list)
        val max = list.last._1.toInt
        val min = list.head._1.toInt

        val random = new Random()
        for(_ <- 0 until 100){

          val target = min + random.nextInt((max - min) + 1)
          val tupleFound = tree.biggestLowerOrEqual(target)
          list.filter(t => t._1 <= target).last should be (tupleFound.get)
        }
      }
    }
  }

  test("tree.smallestBiggerOrEqual returns expected element"){
    forAll(nonSequentialTuplesList){ list =>
      whenever(list.nonEmpty){
        val tree = RedBlackTreeMap.makeFromSorted(list)
        val max = list.last._1.toInt
        val min = list.head._1.toInt

        val random = new Random()
        for(_ <- 0 until 100){

          val target = min + random.nextInt((max - min) + 1)
          val tupleFound = tree.smallestBiggerOrEqual(target)
          list.filter(t => t._1 >= target).head should be (tupleFound.get)
        }
      }
    }
  }

  test("tree.smallest returns expected element"){
    forAll(nonSequentialTuplesList){ list =>
      whenever(list.nonEmpty){

        val minTuple = (-1L,50) //The generator does not generate negative values. This tuple is guaranteed to be the smallest
        val listWithMin  = List[(Long,Int)](minTuple) ::: list
        val tree = RedBlackTreeMap.makeFromSorted(listWithMin)

        tree.smallest.get should be (minTuple)
      }
    }
  }

  test("tree.biggest returns expected element"){
    forAll(nonSequentialTuplesList){ list =>
      whenever(list.nonEmpty){

        val maxTuple = (1001L,50) //The generator does not generate values above 1000. This tuple is guaranteed to be the biggest
        val listWithMax  = list ::: List[(Long,Int)](maxTuple)
        val tree = RedBlackTreeMap.makeFromSorted(listWithMax)

        tree.biggest.get should be (maxTuple)
      }
    }
  }

  test("tree.get after tree.remove returns None"){
    var tree = RedBlackTreeMap.empty[Int]

    tree = tree.insert(1,10)
    tree = tree.insert(2,11)
    tree = tree.insert(3,12)
    tree = tree.insert(6,13)
    tree = tree.insert(5,14)
    tree = tree.insert(4,15)
    tree = tree.insert(9,16)

    tree = tree.remove(5)

    tree.get(5) should be (None)
  }

  test("tree.get after tree.update returns expected value"){
    var tree = RedBlackTreeMap.empty[Int]

    tree = tree.insert(1,1)
    tree = tree.insert(2,2)
    tree = tree.insert(8,8)
    tree = tree.insert(3,3)
    tree = tree.insert(6,6)
    tree = tree.insert(5,5)
    tree = tree.insert(7,7)
    tree = tree.insert(4,4)
    tree = tree.insert(9,9)

    tree = tree.update(3,9,(key,value) => (key,value*2))

    for(i <- 3 to 9){
      tree.get(i).get should be (i*2)
    }
  }

  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny // Disables the shrink

  test("batch operations keep expected values"){
    forAll(nonSequentialTuplesList){list => {
      whenever(list.nonEmpty){ // Non empty list & no negative key

        var parrallelMap = list.sortBy(_._1)
        var tree = RedBlackTreeMap.makeFromSortedArray(list.toArray)
        val operations = Gen.listOfN(50,operationGenerator).sample.get
        var gapAboveLastKey = 0

        for(i <- operations){

          var operation = i
          var randomKey: Long = 1

          if(tree.content.isEmpty){
            operation = 2
          }
          else{
            randomKey = Random.shuffle(tree.content).head._1
          }

          operation match {
            case 0 => {
              tree = tree.remove(randomKey)
              parrallelMap = parrallelMap.filter(_._1 != randomKey)
            }
            case 1 => {
              var randomKeyTo = Random.shuffle(tree.content).head._1
              if(randomKey > randomKeyTo){
                val tmp = randomKey
                randomKey = randomKeyTo
                randomKeyTo = tmp
              }

              tree = tree.update(randomKey,randomKeyTo,(key,value) => (key,value+1))
              parrallelMap = parrallelMap.map(tuple => if(tuple._1 >= randomKey && tuple._1 <= randomKeyTo) (tuple._1,tuple._2 + 1) else (tuple._1,tuple._2))

            }
            case 2 => {
              val newkey :Long = 2000 + gapAboveLastKey // Ensures to add a new unique key
              gapAboveLastKey += 1
              tree = tree.insert(newkey,0)

              parrallelMap = parrallelMap ::: List((newkey,0))
            }
          }
        }
        parrallelMap.sortBy(_._1) should be (tree.content.sortBy(_._1))
      }
    }}
  }

  val operationGenerator = for (n <- Gen.choose(0,2)) yield n
  val intGenerator = for (n <- Gen.choose(10, 1000)) yield n

  // Generates a list of key-value tuples with incremental key (in order) and random values
  val sequentialTuplesList =  for {
    numElems <- Gen.choose(0, 500)
    valuesList <- Gen.listOfN(numElems,intGenerator)
  } yield valuesList.zipWithIndex.map(tuple => (tuple._2 :Long,tuple._1 :Int))

  // Generates a list of key-value tuples with sparse unique keys (in order) and random values
  val nonSequentialTuplesList =  for {
    numElems <- Gen.choose(0, 500)
    valuesList <- Gen.listOfN(numElems,intGenerator)
    keysList <- Gen.listOfN(numElems,intGenerator)
  } yield keysList.distinct.zip(valuesList).map(tuple => (tuple._1 :Long,tuple._2 :Int)).sortWith(_._1 < _._1)
}
