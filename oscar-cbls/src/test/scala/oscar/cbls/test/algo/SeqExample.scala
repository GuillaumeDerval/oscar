package oscar.cbls.test.algo

import oscar.cbls.algo.seq.functional.{ConcreteIntSequence, IntSequence}


/**
 * Created by rdl on 12-01-17.
 */
object SeqExample extends App{

  val a = IntSequence(0 to 39)
  println(a)

  val b = a.moveAfter(10, 19, 9, true, fast= false, autoRework = false).asInstanceOf[ConcreteIntSequence]

  println(b)

  println(b.bij.forward)

  val c = a.moveAfter(10, 19, 29, true, fast = false, autoRework = false).asInstanceOf[ConcreteIntSequence]

  println(c)
  println(c.bij.forward)

  val d = c.moveAfter(15, 24, 14, true, fast = false, autoRework = false).asInstanceOf[ConcreteIntSequence]

  println(d)
  println(d.bij.forward)

}
