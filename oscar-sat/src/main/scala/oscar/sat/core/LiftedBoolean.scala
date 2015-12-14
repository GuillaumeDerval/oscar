package oscar.sat.core

abstract sealed class LiftedBoolean { 
  def opposite: LiftedBoolean 
}

final object False extends LiftedBoolean { 
  override val opposite: LiftedBoolean = True 
  override val toString: String = "False"
}

final object True extends LiftedBoolean { 
  override val opposite: LiftedBoolean = False 
  override val toString: String = "True"
}

final object Unassigned extends LiftedBoolean { 
  override val opposite: LiftedBoolean = this
  override val toString: String = "Unassigned"
}