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
  - knows about partial propagation

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

Features of OScaR.cbls, for the impatient nerds
==================================================

OscaR.cbls is an implementation of constraint-based local search.
It also features a high-level module to help you define your search procedure.

OscaR.cbls has the following features:
* high-level modeling primitives with variables of types integer and set of integer
* Partial propagation for fast neighborhood exploration. Propagation is triggered when the value of a variable is queried by the search script. Deciding whether the propagation will be total or partial is done depending on the variable: if the variable is registered for partial propagation, the propagation will be partial. It will be total otherwise. Violation degrees are automatically registered for fast propagation.
* Propagation graph can be cyclic. Two propagation graphs are handled: a static graph that over-approximates the dependencies between variables and invariants, and a dynamic graph that represents the real dependencies given the data actually stored in the model. The static graph can include cycles. This makes it possible e.g. to implement JobShop scheduler from standard invariants.
* Constraints assign violation degree to their input variables, to identify the variable that cause a violation of each constraint. The violation degree propagates upwards through the model, it enables one to find the variable contributing the more to the overall violation even if it is not directly subject to the constraint.
* Libraries of standard invariants and constraints are proposed on integer and set of integer domains: Invariant library includes logic, numeric, min/max, and set invariants. Constraint library includes few global constraints: Alldiff, AtLeast, AtMost and equalities over integers.


Modeling with OscaR.cbls (L1)
===================================================


OscaR.cbls has two main modeling concepts: variables, and invariants.
OscaR.cbls natively supports both integer and set of integer variables.
Invariants are mathematical operators that maintain an output according to a set of inputs. There is a library of roughly eighty invariants available in OscaR.cbls. This library is explained in SECTIONXXX and covers different fields such as basic arithmetic operators, min-max operators, logical operators, and set operators.
OscaR.cbls supports a factory class that contains creation methods for all these concepts. The factory is the object XXXXXXXXX. It contains the documentation for all invariants, so it is a very convenient place to look at while searching for an invariant. In this tutorial, we create concepts through this factory, but you can also instantiate these concepts directly by calling their constructors.
These concepts are grouped into a store.


Modeling API of OscaR.cbls (L1)
===================================================


Principles of propagation in OscaR.cbls (L2)
===================================================

Propagation is the core mechanism in a CBLS engine. In the context of a CBLS engine, propagation is about updating the value of variables in a graph whose nodes are alternatively variables and invariants and whose edges are directed, and denote value dependencies between nodes. In this graph, the nodes are called propagation elements.
In a CP engine, propagation is omnidirectional: the algorithms of constraints are triggered on change of any of their variable, and can update all of their variables. In CBLS, invariants distinguish their input and output variables. They are triggered on change of their input variable, and can only update their output variables. A variable can only the output of a single invariants. Due to the distinction between input and output, propagation in CBLS is a single wave that crosses the propagation DAG, while in CP, it requires iterating until a fixpoint is reached.

The concepts related to propagation, and the related class constitute the propagation layer of Oscar.cbls.

A search procedure relies on propagation to update the value of the objective function either when a move is performed, or to evaluate the quality of a neighbour solution. Propagation must be performed very efficiently so that it is:

\begin{itemize}
\item \textbf{Performed in a single wave: } the engine ensures that when updates are propagated in the propagation graph, each node is reached at most once.
\item \textbf{Selective: } a propagation element is reached by the propagation wave only if it requires performing some update. Portions of the propagation graph where no change was performed on the input will therefore not be reached by the propagation wave.
\item \textbf{Locally incremental: } invariants are designed to perform incremental update of the variable(s) they output. This is achieved by proposing as additional mechanism on top of the propagation process so that invariants are notified about specific change of their input variable(s). This mechanism actually belongs to the computation layer, and will be discussed further in \refsec{sec:computation}.
\end{itemize}

To ensure that, Oscar.cbls maintains, and queries the propagation graph that declares the dependencies between variables and invariants.

Concretely, propagation consists in calling a method \verb+propagate+ on each propagation element that requires to be propagated. The concrete action taken in this method is defined in the computation layer, and dependent on each invariant.



Searching with OscaR.cbls using linear selectors (L1)
===================================================

Searching with OscaR.cbls using standard neighborhoods (L1)
===================================================

Advanced Searching with OscaR.cbls (L2)
===================================================


Violation of Variables (L2)
===================================================

A constraint declares a set of constrained variables. These are the ones that intervene in the constraint. For each of them, the constraint must be able to provide a violation degree. This is an IntVar that computes to which extend the variable contributes to the violation of the constraint.
Constraint systems propose the same mechanism, except that they do not compute the violation for each and every variable that appears in the constraints that are posted into them. Instead, any variable can be registered into them for a violation degree. These include not only the variables that intervene in some constraint posted into them, but any variable of the model.
In a constraint system, the \emph{local violation degree} of a variable is the sum of the violation degree attributed to it, for each constraint posted in the constraint system, weighted by the weighting factor of the constraint. Only variables that directly intervene in a constraint have a nonzero local violation degree. The \emph{global violation degree} of a variable is the sum of the local violation degrees of all variable that have one in the constraint system, and that contribute directly or indirectly to the variable, according to the static dependency graph. Global violation degrees are therefore built by constraint systems following a reachability query to the static propagation graph. This query is performed when the constraint system is closed, so that if the graph is enriched afterwards, these changes are not taken into account in the global violation degrees.


Bulking (L3)
===================================================

It is very common to encounter a large set of identical invariants that all input the same large array of variable with the same role.
For instance, consider the JobShop problem, where the start date of a task is the max end date among the tasks that are preceding it. Each task thus defined its start date using the Max invariant, ranging on the array of all end dates of all task, and using a SetVar index that specifies the task to be actually considered. This results in a large number of instances of the max invariant, all ranging on the same array, but with different indexes. If there are n such task, we end up with n² dependencies in the static graph, and all graph analysis algorithms that are run on model close will be slowed down accordingly.
Besides, all the invariants will perform the same initialization sequence that typically requires iterating over the array to identify some min and max bounds that will be identical for all of these max invariants, and this will again cause a waste of efficiency.
Bulking is a solution to this problem. The concept is to create an additional node in between the array of variables, and the set of invariants that all listen to the array of variables. This intermediary node is called a bulk node. It declares static dependencies to all the variables in the array, and the invariants declare a static dependency to the bulk node instead of declaring a static dependency to all variables of the array. Additionally, initialization values that need to be computed on initialization of the invariants are computed once, and stored in the bulk node, so that these values can be used by all the invariants that listen to this bulk node once they are available.
This way, storage is brought back from O(n²) to O(n), and computations performed to initialize the invariants is shared by all invariants, so the cost of this computation is also divided by O(n).
The use of bulking is the initiative of the invariant. All invariants that listen to an array of variable should actually use bulking. To use bulking, the invariant should have the trait Bulking included in his declaration.
Example

Concretely, when a bulked invariant is instantiated, and declares a bulked dependency to an array, the bulking mechanics first searches for an existing proper bulk node. Existing bulk nodes are stored in the Model, and can be retrieved, based on a pair (string,array), where the string is often the class name of the invariant, and the array is a reference to the bulked array. If one such bulk node is found, the bulking mechanism declares a static dependency to this bulk node, and the initialization values stored in this bulk node are returned to the invariant. If no such bulk node exists, it is created with static dependency to every variable of the array, the initialization values are computed by the invariants, stored in the bulk node, and returned to the invariant.

