.. _oscar-cbls:

******************************
OscaR-CBLS
******************************


Learning Outcomes
=======================================

* Level 1: Beginning CBLS Modeler

  - can create a simple model, with a simple search based on one neighborhood
  - knows how modeling is organized (variables, invariants, constraints)
  - knows where to find invariants and constraints

* Level 2: Advanced CBLS Modeler

  - can create a search involving several neighborhoods and a global meta-heuristic
  - has a good intuition of how propagation works

Hello Queens (L1)
===================================================

Lots of tutorial in this field start with the same example that is the NQueen.
    This one will not make an exception. A simple NQueen is shown here:

.. literalinclude:: ../../oscar-cbls/src/main/examples/oscar/examples/cbls/userguide/NQueenBasic.scala
   :language: scala
   :linenos:

This model mixes with the trait  ``CBLSModel``. It offers the following features:

* it defines an implicit Store (named ``s``) and ConstraintSystem (named ``c``) behind the scene
* it supports an API to create all variables. They are implicitly added to the store ``s``
* it also offers lots of methods to create every constraints or invariant of the problem you want to solve
* it includes some linear selectors that you can use for defining our search procedure (see the ``selectMin`` method)

Writing local search procedure is a tedious and time-consuming task.
    Besides, search procedure often include the same basic bricks (neighborhoods, solution management, meta-heuristics, etc.)
    For this reason, OscaR.cbls includes a library of standard neighborhoods that can be assembled together to easily constitute complex search procedures.

We show here below a more elaborate solver for the NQueen. Besides using a more elaborate search strategy,
it also relies on a standard neighborhood for implementing the search procedure.
It uses a standard ``swap`` neighborhood with problem-specific parameters that specifies what queen must be swapped with what other queen.

.. literalinclude:: ../../oscar-cbls/src/main/examples/oscar/examples/cbls/userguide/NQueenEasy.scala
   :language: scala
   :linenos:

Modeling with OscaR.cbls (L1)
===================================================

OscaR.cbls is based on the concept of cbls

(variables, invariants, constraints)



Modeling API of OscaR.cbls (L1)
===================================================


Principles of propagation in OscaR.cbls (L2)
===================================================


Searching with OscaR.cbls using linear selectors (L1)
===================================================

Searching with OscaR.cbls using standard neighborhoods (L1)
===================================================

Advanced Searching with OscaR.cbls (L2)
===================================================



