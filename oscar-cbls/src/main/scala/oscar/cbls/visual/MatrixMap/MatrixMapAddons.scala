package oscar.cbls.visual.MatrixMap

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

import java.awt._
import java.awt.event.{ItemEvent, ItemListener}
import javax.swing._

import scala.List

/**
  * Created by fabian on 23-02-16.
  */

trait RouteToDisplay extends RoutingMap{
  var container:RoutingMatrixContainer = _

  def initRouteToDisplay(container:RoutingMatrixContainer) {
    this.container = container

    val routesToDisplay:Array[Boolean] = Array.tabulate(vrp.v)(v =>false)

    val vehicleSelectionPane = new JPanel()
    vehicleSelectionPane.setLayout(new BoxLayout(vehicleSelectionPane, BoxLayout.Y_AXIS))
    val allCheckBox = new JCheckBox("All")
    allCheckBox.addItemListener(new ItemListener {
      override def itemStateChanged(e: ItemEvent): Unit = {
        for (c <- vehicleSelectionPane.getComponents) {
          val box = c.asInstanceOf[JCheckBox]
          box.setSelected(e.getStateChange == ItemEvent.SELECTED)
        }
      }
    })
    vehicleSelectionPane.add(allCheckBox)
    for (i <- 0 until vrp.v) {
      val checkBox = new JCheckBox("Vehicle : " + i)
      checkBox.addItemListener(new ItemListener {
        override def itemStateChanged(e: ItemEvent): Unit = {
          routesToDisplay(i) = e.getStateChange == ItemEvent.SELECTED
          setRouteToDisplay(container.allRoutes.map(x => if (routesToDisplay(container.allRoutes.indexOf(x))) x else List.empty))
        }
      })
      vehicleSelectionPane.add(checkBox)
    }
    container.add(vehicleSelectionPane, BorderLayout.EAST)
  }

  override def setRouteToDisplay(rtd: Array[List[Int]]): Unit = {
    super.setRouteToDisplay(rtd)
  }
}
