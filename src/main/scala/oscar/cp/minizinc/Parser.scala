package oscar.cp.minizinc

import scala.util.parsing.combinator._
import FZType._

import oscar.cp.modeling.CPSolver
import oscar.cp.core._
import oscar.cp.modeling._

class Parser extends JavaTokenParsers {// RegexParsers {
	var model : Minizinc_model = new Minizinc_model
	val cp = CPSolver()
	
	//def myParseAll(input: String) = {parseAll(var_decl, input)}
	def myParseAll(input: String) = {parseAll(flatzinc_model, input)}
	//def myParseAll(input: String) = {parseAll(constraint, input)}
	def parseParam(input: String) = {parseAll(param_decl, input)}
	
	def flatzinc_model : Parser[Any] = rep(pred_decl)~rep(param_decl)~rep(var_decl)~rep(constraint)~solve_goal
	def pred_decl : Parser[Any] = "predicate"~identifier~"("~rep1sep(pred_param, ",")~");"

	def identifier : Parser[Any] = index_set
	
	def pred_param : Parser[Any] = pred_param_type~":"~pred_ann_id // what about no space before the ":" and one after ?
	def pred_param_type : Parser[Any] = par_pred_param_type | var_pred_param_type	
	
	def par_type : Parser[Any] = (
	    "bool"
		| "float"
		| "int"
		| "set of int"
		| "array ["~index_set~"] of bool"
		| "array ["~index_set~"] of float"
		| "array ["~index_set~"] of int"
		| "array ["~index_set~"] of set of int"
	)
	def par_pred_param_type : Parser[Any] = (
		par_type
		| float_const~".."~float_const
		| int_const~".."~int_const
		| "{"~rep1sep(int_const, ",")~"}"
		| "set of"~int_const~".."~int_const
		| "set of"~"{"~rep1sep(int_const, ",")~"}"
		| "array ["~index_set~"] of"~float_const~".."~float_const
		| "array ["~index_set~"] of"~int_const~".."~int_const
		| "array ["~index_set~"] of"~"{"~rep1sep(int_const, ",")~"}"
		| "array ["~index_set~"] of"~"set of"~int_const~".."~int_const
		| "array ["~index_set~"] of"~"set of"~"{"~rep1sep(int_const, ",")~"}"
	)
	def var_type : Parser[Any] = (
	    "var"~int_const~".."~int_const
		| "var float"
	    | "var"~float_const~".."~float_const
	    | "var int"
	    | "var"~int_const~".."~int_const
	    | "var"~"{"~rep1sep(int_const, ",")~"}"
	    | "var set of"~int_const~".."~int_const
	    | "var set of"~"{"~rep1sep(int_const, ",")~"}"
	    | "array ["~index_set~"] of var bool"
	    | "array ["~index_set~"] of var float"
	    | "array ["~index_set~"] of var"~float_const~".."~float_const
	    | "array ["~index_set~"] of var int"
	    | "array ["~index_set~"] of var"~int_const~".."~int_const
	    | "array ["~index_set~"] of var"~"{"~rep1sep(int_const, ",")~"}"
	    | "array ["~index_set~"] of var set of"~int_const~".."~int_const
	    | "array ["~index_set~"] of var set of"~"{"~rep1sep(int_const, ",")~"}"
	)
	def var_pred_param_type : Parser[Any] = (
		var_type
		| "var set of int"
		| "array ["~index_set~"] of var set of int"
	)
	def index_set : Parser[Any] = (
	    "1.."~int_const ^^ {
	      case "1.."~i => Range(1, i+1, 1)
	    }
	    | "int" ^^ (_.toString())// what about the fact that "int" is only allowed in predicates ?
	)
	
	def expr : Parser[Any] = ( //need to find a way to return something else than any
		bool_const
		| set_const //should be float -> int -> set, inverted for set to work, need testing
		| float_const
		| int_const //^^ (_.toInt) how to avoid the casts when using an expr that is an Any
		| var_par_id
		| var_par_id~"["~int_const~"]" // why two entries for var_par_id ?
		| array_expr
		| annotation
		| "...string constant..." //???
	)
	def pred_ann_id : Parser[String] = "[A-Z_a-z][A-Z_a-z0-9_]*".r
	
	def var_par_id : Parser[String] = "-*[A-Za-z][A-Za-z0-9_]*".r
	
	
	//definition of the constants
	
	def bool_const : Parser[Boolean] = (
	    "true" ^^ (x => true)
	    | "false" ^^ (x => false)
	)
	def float_const : Parser[Float] = (
	    int_const~"."~"[0-9][0-9]*".r~opt("[eE]".r~int_const) ^^ {
	      case i1~"."~i2~exp => exp match {
	        case Some(e~i3) => (i1+"."+i2+e+i3).toFloat
	        case None => (i1+"."+i2).toFloat
	      }
	    }
	    | int_const~"[eE]".r~int_const ^^ {
	      case i1~e~i2 => (i1+e+i2).toFloat
	    }
	)
	def int_const : Parser[Int] = "[+-]?[0-9][0-9]*".r ^^ (_.toInt)// [+-] at the begining of the regex in the grammar, what does that mean ?
	def set_const : Parser[Any] = ( 
	    int_const~".."~int_const ^^ { 
	    	case int1~".."~int2 => Range(int1, int2+1, 1) 
	    } 
	    | "{"~>rep1sep(int_const, ",")<~"}" 
	)
	
	def array_expr : Parser[List[Any]] = (
	    //"[]" | not useful since repsep is used instead of rep1sep
	    "["~>repsep(expr, ",")<~"]"
	)
	
	
	//Parameter and variable declarations
	
	def param_decl : Parser[Any] = par_type~":"~var_par_id~"="~expr~";" ^^ 
	{
	  case tp~":"~id~"="~e~";" =>
	    tp match {
	      case "bool" => model.dict += 
	        ((id, (FZType.P_BOOL, 
	            new ParamBool(e.toString.toBoolean, id))))
	      case "int" => model.dict += 
	        ((id, (FZType.P_INT, 
	            new ParamInt(e.toString.toInt, id))))
	      case "float" => model.dict += 
	        ((id, (FZType.P_FLOAT,
	            new ParamFloat(e.toString.toFloat, id)))) // ! no floats in oscar, what to do ?
	      case "set of int" => model.dict += 
	        ((id, (FZType.P_SET_INT,
	          e match {
	          	case x:Range => new ParamSetOfInt(e, true, id)
	          	case x:List[Any] => new ParamSetOfInt(e, false, id)
	          }
	        )))
	        
	      //code duplication can be easily avoided but what's better ?
	      case "array ["~iset~"] of bool" => model.dict += 
	        ((id, (FZType.P_ARRAY_BOOL, 
	            new ParamArrayBool(e, 
	                iset match {
	                	case x:Range => iset
	                	case _ => None
	            	}, id))))
	      case "array ["~iset~"] of float" => model.dict += 
	        ((id, (FZType.P_ARRAY_FLOAT, 
	            new ParamArrayFloat(e, 
	                iset match {
	                	case x:Range => iset
	                	case _ => None
	            	}, id))))
	    }
	}
	  //the expr has restriction... what about it ?
	
	def var_decl : Parser[Any] = var_type~":"~var_par_id~annotations~opt("="~expr)~";" ^^
	{ 
//to be done when assignment of var is understood
//	  case tp~":"~id~ann~Some("="~e)~";" => model.dict.get(id) match {
//	    case Some((tp, fzo)) => model.dict += ((id, (tp, fzo)))
//	  }
	  case tp~":"~id~ann~None~";" => 
	    tp match {
	      case "var"~i1~".."~i2 => model.dict +=
	        ((id, (FZType.V_INT_RANGE,
	            new VarIntRange(Range(i1.toString.toInt, i2.toString.toInt+1, 1), ann, 
	                CPVarInt(cp, i1.toString.toInt to i2.toString.toInt), id))))
	            //println("cpvar created")
	      case "var int" => model.dict += 
	      	((id, (FZType.V_INT, 
	      		new VarInt(0, ann, id))))
	      case "array ["~iset~"] of var"~i1~".."~i2 => model.dict +=
	        ((id, (FZType.V_ARRAY_INT_R, 
	            new VarArrayIntRange(Range(i1.toString.toInt, i2.toString.toInt+1, 1), ann,
	                iset match {
	                	case x:Range => for(i <- iset.asInstanceOf[Range]) 
	                	  yield CPVarInt(cp, i1.toString.toInt to i2.toString.toInt)
	                	case _ => null
	            	}
	            	, id))))
	        println("cpvarArray created")
	    }
	    
	}// the vars in assignment must be declared earlier
	
	
	// Constraint declaration (every constraint should be a case below the match
	
	def constraint : Parser[Any] = "constraint"~pred_ann_id~"("~rep1sep(expr, ",")~")"~annotations~";" ^^ {
	  case "constraint"~cst~"("~varList~")"~ann~";" => cst match {
	    case "int_le" => 
	      (model.dict.get(varList(0).toString), model.dict.get(varList(1).toString)) match {
	        case (Some((tp0, fzo0)), Some((tp1, fzo1))) => 
	          assert(tp0 == FZType.V_INT_RANGE, "The FZObject 0 doesn't have the type V_INT_RANGE")
	          assert(tp1 == FZType.V_INT_RANGE, "The FZObject 1 doesn't have the type V_INT_RANGE")
	          cp.add(fzo0.asInstanceOf[VarIntRange].cpvar <= fzo1.asInstanceOf[VarIntRange].cpvar)
	          //println("Constraint int_le added")
	        case _ => assert(false, "varList didn't contains enough varibles")
	      }
	    case "set_ne" =>
	      (model.dict.get(varList(0).toString), model.dict.get(varList(1).toString)) match {
	        case (Some((tp0, fzo0)), Some((tp1, fzo1))) => 
	          assert(tp0 == FZType.V_INT_RANGE, "The FZObject 0 doesn't have the type V_INT_RANGE")
	          assert(tp1 == FZType.V_INT_RANGE, "The FZObject 1 doesn't have the type V_INT_RANGE")
	          cp.add(fzo0.asInstanceOf[VarIntRange].cpvar != fzo1.asInstanceOf[VarIntRange].cpvar)
	          //println("Constraint set_ne added")
	        case _ => assert(false, "varList didn't contains enough varibles")
	      }
	    case "int_lin_ne" =>
	      varList match {
	        
	        case _ => None
	      }
	      //that match can be avoided by using a self made mutable tuple
	      //"constraint"
	  }
	}
	
	
	def solve_goal : Parser[Any] = (
	    "solve"~annotations~"satisfy;" ^^ { case _ => //println(model.dict.toString) 
	      var x = Array[CPVarInt]()
	      var name = Array[String]() // only used for formating the output
	      var output = Array[Boolean]() // only used for formating the output
	      var first = true // only used for formating the output
	      model.dict.foreach { e => 
	        //println(x.mkString(","))
	        e._2 match {
	          case (tp, fzo) => // /!\ not always CPVarInt
	            //println(fzo.asInstanceOf[VarIntRange].cpvar + " " + fzo.asInstanceOf[VarIntRange].name)
	            tp match {
	              case FZType.V_INT_RANGE => {
	                x :+= fzo.asInstanceOf[VarIntRange].cpvar
	                name :+= fzo.asInstanceOf[VarIntRange].name
	                fzo.asInstanceOf[VarIntRange].annotations.foreach { ann =>
	            		if ( ann.name == "output_var" ) { output :+= true }
	                }
	                //println(output.length + "  " + x.length)
	                if ( output.length < x.length) { output :+= false }
	              }
	              case FZType.V_ARRAY_INT_R => {
	                var c = 0
	                fzo.asInstanceOf[VarArrayIntRange].cpvar.foreach { e =>
	                	x :+= e
	                	name :+= fzo.asInstanceOf[VarArrayIntRange].name + "[" + c.toString + "]"
	                	fzo.asInstanceOf[VarArrayIntRange].annotations.foreach { ann =>
	            			if ( ann.name == "output_array" ) { output :+= true }
	                	}
	                	if ( output.length < x.length) { output :+= false }
	                }
	              }
	            }
	            
	        }
	        println("done")
	      }
	      println(x.mkString(","))
	      cp.solve subjectTo {
	      } exploration {
	        cp.binary(x)
	        Range(0, x.length, 1).foreach { i =>
	        	if ( output(i) ) { 
	        	  if(!first) { print(" ")}
	        	  print(name(i) + " =" + x(i).toString) 
	        	  first = false // only used for formating the output
	        	}
	        }
	        first = true // only used for formating the output
	        print(";\n----------\n")
	        //println(x.mkString(","))
	      //} run ()
	      } run (nbSolMax = 2)
	      print("==========")
	      //println(model.dict.toString)
	    }
	    | "solve"~annotations~"minimize"~expr~";"
	    | "solve"~annotations~"maximize"~expr~";"
	) // expr must be a var name of var array element
	
	def annotations : Parser[List[Annotation]] = rep("::"~>annotation) 
	// is there a list of annotations ?
	def annotation : Parser[Annotation] = (
	    pred_ann_id~"("~rep1sep(expr, ",")~")" ^^ {
	      case ann~"("~list~")" => new Annotation(ann, list)
	    }
	    | pred_ann_id ^^ (new Annotation(_, null))
	)// some notes, see syntax
	
}