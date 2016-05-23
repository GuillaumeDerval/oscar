.. _oscar-cbls:


******************************
OscaR-CBLS
******************************


Learning Outcomes
=======================================

* Level 1: Beginning CBLS Modeler

  - can create a simple model, with a simple search based on one neighborhood
  - knows how modeling is organized (variables, invariants, constraints)
  - knows where to find invariants and their documentation

* Level 2: Advanced CBLS Modeler

  - can create a search involving several neighborhoods and a global meta-heuristic
  - has a good intuition of how propagation works

Create basic models (L1)
===================================================

Declare a solver, create variables, add constraints
-------------------------------------------------------

Lots of tutorial in this field start with the same example that is the NQueen.
    This one will not make an exception. A simple NQueen is shown here:

.. literalinclude:: ../../oscar-cbls/src/main/examples/oscar/examples/cbls/userguide/NQueenBasic.scala
   :language: scala
   :linenos:

This model mixes with the trait  ``CBLSModel ``. It offers the following features:

* it defines an implicit Store (named "s") and ConstraintSystem (named "c") behind the scene
* it supports an API to create all variables. They are implicitly added to the store "s"
* it also offers lots of methods to create every constraints or invariant of the problem you want to solve
* it includes some linear selectors that you can use for defining our search procedure (see the "selectMin" method)

You might view this object as the one containing all the info about your model (or more simply it is your model).

In OscaR each time you add a constraint with the method add you trigger the fix-point algorithm
    so that you can see immediately the effect of propagation when adding this constraint (with interleaved  ``println `` for instance).

On the next example we use a binary-first fail depth first search on the array of variables [x1,x2,x3].

The start(nSols = 1) is asking to start the search and stop as soon as the first solution is discovered.
To find all the solutions, you can simply use  ``start() `` which is equivalent to  ``start(nSols = Int.MaxValue) ``.
Each time a solution is found during the search tree exploration, the closure defined in onSolution(...) is executed. In this case, we just showing the value of x1 in the solution.



