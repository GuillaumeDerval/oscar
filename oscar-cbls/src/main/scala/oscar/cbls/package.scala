package oscar

/**
 * Created by rdl on 08-09-17.
 */
package object cbls {

  // Alias to useful classes and companion objects
  type CBLSIntVar = oscar.cbls.core.computation.CBLSIntVar
  final val CBLSIntVar = oscar.cbls.core.computation.CBLSIntVar

  type CBLSSetVar = oscar.cbls.core.computation.CBLSSetVar
  final val CBLSSetVar = oscar.cbls.core.computation.CBLSSetVar

  type CBLSSeqVar = oscar.cbls.core.computation.CBLSSeqVar
  final val CBLSSeqVar = oscar.cbls.core.computation.CBLSSeqVar

  type Store = oscar.cbls.core.computation.Store
  final val Store = oscar.cbls.core.computation.Store



}
