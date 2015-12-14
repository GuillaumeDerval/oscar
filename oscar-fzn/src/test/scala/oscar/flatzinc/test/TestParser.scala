package oscar.flatzinc.test

import org.scalatest.FunSuite
import org.scalatest.Matchers
import oscar.flatzinc.parser.FZParser
import oscar.flatzinc.Log
import oscar.flatzinc.Options
import oscar.flatzinc.ParsingException
import oscar.flatzinc.model.FZProblem
import oscar.flatzinc.NoSuchConstraintException

class TestParser extends FunSuite with Matchers{

  def getSATModel(modelStr: String): FZProblem = {
    val log = new Log(0)
    val model = FZParser.readFlatZincModelFromString(modelStr+" solve satisfy;", log, false)
    model.problem
  }
  /*test("test no solve"){
    val log = new Log(0)
    
    a [ParsingException] should be thrownBy {
      FZParser.readFlatZincModelFromString("var int: x;", log, false)      
    }
  }*/
  test("test empty model"){
    val problem = getSATModel("")
    problem.variables.size should be(0)
    problem.constraints.size should be(0)
  }
  
  test("test simple model no constraint"){
    val problem = getSATModel("var int: x; ")
    problem.variables.size should be(1)
    problem.constraints.size should be(0)
  }
  
  test("test alias"){
    val problem = getSATModel("var int: x; var int: y = x;")
    problem.variables.size should be(1)
    problem.constraints.size should be(0)
  }
  
  test("test simple model"){
    val problem = getSATModel("var int: x; var int: y;\n"+ 
        "constraint int_eq(x,y);");
    problem.variables.size should be(2)
    problem.constraints.size should be(1)
  }
  /*
  test("test no constraint"){
    a [NoSuchConstraintException] should be thrownBy {
      getSATModel("var int: x; var int: y;\n"+ 
        "constraint my_constraint_does_not_exist(x,y);");
    }
  }*/
  
/*  test("test 2.0"){
    
var 1..8: X_INTRODUCED_0;
var 1..8: X_INTRODUCED_1;
var 1..8: X_INTRODUCED_2;
array[1..3] of var 1..8: q:: output_array([1..8]) = [X_INTRODUCED_0,X_INTRODUCED_1,X_INTRODUCED_2];
    val problem = getSATModel("""
        
        array[1..3] of var 1..8: q :: output_array([1..8]);
""")
  }*/
}