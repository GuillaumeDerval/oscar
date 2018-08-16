# VisualFX ![CI status](https://img.shields.io/badge/build-passing-brightgreen.svg)

**VisualFX** is a scala library to make visualisations of Oscar process using **ScalaFX** library.

## Installation

### Requirements

* Scala 2.12
* ScalaFX 8.0

## Usage of Plot class _(or any other window type classes)_

### Create a scalaFX app
```
import oscar.visualfx._

import scalafx.application.JFXApp
import scalafx.concurrent.{Service, Task}

object fxApp extends JFXApp {

  /**
  * create a window for plotting the objective function
  */
  val plotWindow = new plot.Plot(xAxisIsTime = true)
  
  val initTask = Task {
    //code that initialize the optimisation problem
  }
  
  val optimisationService = Service(Task{
    //function that will do the optimisation and takes plotWindow as parameter
  })
  
  optimisationService.onSucceeded = succeededEvent => {
    println("optimisation finished")
  }
  
  initTask.run()
  initTask.onSucceeded = succeededEvent => {
    optimisationService.start()
  }
}
```

### Plug the plot window to the optimisation function

Add the **addPoint** method _(or the method that updates the window)_ of **Plot** class in the **afterMove** or **afterMoveOnMove** statement of the **neighborhood** declaration.

```
...

def optimisationFunction(plotWindow: Plot): Unit = {
...

val neighborhood =((
      (BestSlopeFirst(
        List(
          Profile(AssignNeighborhood(warehouseOpenArray, "SwitchWarehouse")),
          Profile(swapsK(20) guard(() => openWarehouses.value.size >= 5)), //we set a minimal size because the KNearest is very expensive if the size is small
          Profile(SwapsNeighborhood(warehouseOpenArray, "SwapWarehouses") guard(() => openWarehouses.value.size >= 5))
        ),refresh = W/10)
        onExhaustRestartAfter(RandomizeNeighborhood(warehouseOpenArray, () => openWarehouses.value.size/5), 2, obj)
      ) /*onExhaust (() => {plot.addVLineMark("start VLSN")})*/ exhaust (Profile(muLine(3,3,15)) exhaustAndContinueIfMovesFound Profile(muLine(4,3,15))))
    afterMoveOnMove  ((m:Move) => {
      //plug the plot window here
      plotWindow.addPoint(objectiveFunction.value,m.toString)
    }))

...
}

...

```

## License
OscaR is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation, either version 2.1 of the License, or
(at your option) any later version.

OscaR is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License  for more details.
  
You should have received a copy of the GNU Lesser General Public License along with OscaR.
If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html