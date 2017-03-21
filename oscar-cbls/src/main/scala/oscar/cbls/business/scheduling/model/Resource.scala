package oscar.cbls.business.scheduling.model

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

import oscar.cbls.core.computation.{CBLSSetVar, IntValue}

import scala.collection.immutable.SortedSet

/**
 * this is an abstract class representing a resource.
 * the purpose is to abstract away conflict identification and other stuff that are specific to each type of resource
 *
 * @param planning
 * @author renaud.delandtsheer@cetic.be
 */
abstract class Resource(planning: Planning, n: String) {
  val ResourceID = planning.addResource(this)
  def model = planning.model
  def maxDuration = planning.maxDuration
  val name = Option(n) getOrElse s"Resource $ResourceID"

  /**The set of activities using this resource at every position*/
  val use = Array.tabulate(maxDuration + 1)(t => new CBLSSetVar(model, SortedSet.empty, 0 to Int.MaxValue, s"use_amount_${name}_at_time_$t"))

  /**
   * the level of overshoot of the resource.
   * The higher, the more important it is to solve it first in the flattening
   */
  val overShoot: IntValue

  /**
   * this method is called by the framework before starting the scheduling
   * put anything that needs to be done after instantiation here
   */
  def close()

  /**
   * the first violation of the resource in time
   *
   * @return
   */
  def worseOverShootTime: Int

  /**
   * you need to eject one of these to solve the conflict
   * this can be null if the problem is actually solved in between,
   * or if the problem cannot be solved
   */
  def conflictingActivities(t: Int): Iterable[Activity]

  /**
   * these are the activities that you can use for ejecting
   * one of the conflicting activities
   */
  def activitiesUsingResourceAtThisTime(t: Int): Iterable[Activity]

  def toAsciiArt(headerLength: Int): String
}

