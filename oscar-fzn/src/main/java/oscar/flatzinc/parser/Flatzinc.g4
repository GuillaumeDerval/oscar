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
  ******************************************************************************
  * @author Jean-Noël Monette
  */
grammar Flatzinc;

@header{
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
//package oscar.flatzinc.parser;
import oscar.flatzinc.parser.intermediatemodel.*;
import oscar.flatzinc.parser.intermediatemodel.ASTLiterals.*;
import oscar.flatzinc.parser.intermediatemodel.ASTDecls.*;
import oscar.flatzinc.parser.intermediatemodel.ASTTypes.*;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

}

@parser::members{
	private Model m;
	private ASTModel astM;
	private IDContainer idC;
	public FlatzincParser(TokenStream input,Model m){
		this(input);
		this.m = m;
		this.astM = new ASTModel();
		idC = new IDContainer();
	}
}

//Assume correct model, i.e., only one solvegoal
flatzinc_model
    :( pred_decl
      |param_decl ';' {astM.addParamDecl($param_decl.d);}
      |var_decl ';'  {astM.addVarDecl($var_decl.d);}
      |constraint ';' {astM.addConstraint($constraint.c);}
      |solve_goal ';'{astM.setSolve($solve_goal.s);}
      |func_decl ';' {astM.addFuncDecl($func_decl.f);}
     )*{m.buildModel(astM);};
// Shoudln't depend on order
///flatzinc_model : preddecl* paramdecl* vardecl* constraint* solvegoal;
//changed to accomodate Mzn 2.0
//flatzinc_model : preddecl* (paramdecl|vardecl)* constraint* solvegoal;

//Nothing is done for the predicate declarations
pred_decl : 'predicate' PREDANNID '(' pred_param ( ',' pred_param)* ')' ';' ;

pred_param : pred_param_type ':' pred_ann_id ;

param_decl returns [ASTParamDecl d]
    : par_type ':' var_par_id '=' expr
	{$d = new ASTParamDecl($var_par_id.text, $par_type.t, $expr.e);};
// expr must be a boolconst, floatconst, intconst, setconst, or an array thereof.

func_decl returns [ASTFuncDecl f] locals [ArrayList<ASTVarDecl> params = new ArrayList<ASTVarDecl>();]
    : 'function' 'ann' ':' pred_ann_id ('(' (var_decl {$params.add($var_decl.d);} (',' var_decl {$params.add($var_decl.d);})*)? ')')?
       '=' let_expr
    {$f = new ASTFuncDecl($pred_ann_id.text, $params, $let_expr.l);};

let_expr returns [ASTLet l] locals [ArrayList<ASTNode> body]
    : 'let' '{' {$body = new ArrayList<ASTNode>();}
     (let_body {$body.add($let_body.n);} (',' let_body {$body.add($let_body.n);})*)?
     '}' 'in' '(' annotation ')' { $l = new ASTLet($body, $annotation.ann);};

let_body returns [ASTNode n]
    : var_decl {$n = $var_decl.d;}
    | param_decl {$n = $param_decl.d;}
    | constraint {$n = $constraint.c;} ;

var_decl returns [ASTVarDecl d] locals [ASTLit e = null;]
	: var_type ':' var_par_id annotations
	( '=' expr {$e = $expr.e;})?
	{$d = new ASTVarDecl($var_par_id.text, $var_type.t, $annotations.anns, $e);};



constraint returns [ASTConstraint c] locals [ArrayList<ASTLit> args]: 'constraint' pred_ann_id
	'(' e=expr {$args = new ArrayList<ASTLit>(); $args.add($e.e);}(',' e1=expr {$args.add($e1.e);})* ')'
	annotations {$c = new ASTConstraint($pred_ann_id.text, $args, $annotations.anns);} ;


solve_goal  returns [ASTSolve s] locals [ASTLit e, int t]
	: 'solve' annotations 
	( 'satisfy' {$e = null; $t = ASTSolve.SAT;}
	| 'minimize' expr {$e = $expr.e;$t = ASTSolve.MIN;}
	| 'maximize' expr {$e = $expr.e;$t = ASTSolve.MAX;}
	){$s = new ASTSolve($t,$e,$annotations.anns);}
	;
//expr must be a var name or var array element.

//TYPES

basic_par_type returns [ASTType t]
	: 'bool' {$t = ASTConstants.BOOL;}
	| 'float' {$t = ASTConstants.FLOAT;}
	| 'int' {$t = ASTConstants.INT;}
	| 'set' 'of' 'int' {$t = ASTConstants.SET;}
	;

basic_var_type returns [ASTVarType t]
	: 'var'
	( 'bool' {$t = new ASTVarType(ASTConstants.BOOL);}
	| 'float' {$t = new ASTVarType(ASTConstants.FLOAT);}
	| const_float_range {$t = new ASTVarType(ASTConstants.FLOAT,$const_float_range.r);}
 	| 'int' {$t = new ASTVarType(ASTConstants.INT);}
 	| const_range {$t = new ASTVarType(ASTConstants.INT, $const_range.r);}
 	| const_set {$t = new ASTVarType(ASTConstants.INT, $const_set.r);}
 	| 'set' 'of' const_range {$t = new ASTVarType(ASTConstants.SET, $const_range.r);}
 	| 'set' 'of' const_set {$t = new ASTVarType(ASTConstants.SET, $const_set.r);}
 	);
	
par_type returns [ASTType t]
	: basic_par_type {$t = $basic_par_type.t;}
	| par_array_type {$t = $par_array_type.t;}
	;
	
var_type returns [ASTType t]
	: basic_var_type {$t = $basic_var_type.t;}
	| array_type {$t = $array_type.t;}
	;
	

array_type returns [ASTArrayType t]:
    'array' '[' const_range ']' 'of' basic_var_type {$t = new ASTArrayType($const_range.r, $basic_var_type.t);};

par_array_type returns [ASTArrayType t]:
    'array' '[' const_range ']' 'of' basic_par_type {$t = new ASTArrayType($const_range.r, $basic_par_type.t);};


pred_param_type
	: basic_pred_param_type
	| pred_array_type
	;
basic_pred_param_type
	: basic_var_type
	| basic_par_type
	| float_const '..' float_const
	| const_set
	| const_range
	| 'set' 'of' const_range
	| 'set' 'of' const_set
	| 'var' 'set' 'of' 'int'
	;
pred_array_type:  'array' '[' (const_range
  | 'int'
  | 'int' (',' 'int') ) ']' 'of' basic_pred_param_type
  ;
	
	


expr returns [ASTLit e]
	: lit_expr {$e = $lit_expr.e;}
	;

lit_expr returns [ASTLit e]
    :bool_const {$e = $bool_const.b;}
    | float_const {$e = $float_const.f;}
    | int_const {$e = $int_const.i;}
    | const_range {$e = $const_range.r;}
    | const_set {$e = $const_set.r;}
    | annotation {$e = $annotation.ann;}
    //| var_par_id {$e = $var_par_id.text;}
    | array_expr {$e = $array_expr.a;}
	| string_constant {$e = $string_constant.str;};

const_set returns [ASTSet r] locals [Set<ASTInt> s] :
    '{' {$s = new HashSet<ASTInt>();} (f=int_const { $s.add($f.i); } (',' n=int_const { $s.add($n.i);})*)? '}'  {$r = new ASTSet($s);}
	;

const_range returns [ASTRange r]
	: lb=int_const '..' ub=int_const {$r = new ASTRange($lb.i,$ub.i);};


const_float_range returns [ASTFloatRange r]
	: lb=float_const '..' ub=float_const {$r = new ASTFloatRange($lb.f,$ub.f);};


array_expr returns [ASTArray a] locals [ArrayList<ASTLit> elems]:
	'[' {$elems = new ArrayList<ASTLit>();}
		(e=lit_expr {$elems.add($e.e);}
		(',' e=lit_expr {$elems.add($e.e);} )* )? ']' {$a = new ASTArray($elems);};

annotations returns [ArrayList<ASTLit> anns]
	: {$anns = new ArrayList<ASTLit>();} ( '::' annotation {$anns.add($annotation.ann);} )* ;

annotation returns [ASTLit ann] locals [ArrayList<ASTLit> args]
	: pred_ann_id {$ann = $pred_ann_id.text;}
	| pred_ann_id {$args = new ArrayList<ASTLit>();} (('()')|( '(' expr {$args.add($expr.e);} (',' expr {$args.add($expr.e);} )* ')' ))
	{$ann = new ASTAnnotation($pred_ann_id.text, $args);}
	;
// Whether an identifier is an annotation or a variable name can be identified from its type.
// FlatZinc does not permit overloading of names


//Pseudo-lexer rules
pred_ann_id returns [ASTId text]: PREDANNID {$text= idC.getId($PREDANNID.getText());};
bool_const returns [ASTBool b]: Boolconst {$b = idC.getBool($Boolconst.getText().equals("true"));};
float_const returns [ASTFloat f]: Floatconst {$f = idC.getFloat(Float.parseFloat($Floatconst.getText()));};
int_const returns [ASTInt i]: INT {$i = idC.getInt(Integer.parseInt($INT.getText()));};
string_constant returns [ASTString str]: STRING {$str = idC.getString($STRING.getText().substring(1,$STRING.getText().length()-1));};
var_par_id returns [ASTId text]
    : pred_ann_id {$text= $pred_ann_id.text;};
    //| VARPARID {$text= idC.getId($VARPARID.getText());}

//LEXER rules
Boolconst : 'true' | 'false' ;
PREDANNID : ('a'..'z'|'A'..'Z')('a'..'z'|'A'..'Z'|'0'..'9'|'_')* ;
VARPARID : '_'+ PREDANNID ;
Floatconst : INT '.' NUM (('e'|'E') INT )? | INT ('e'|'E') INT;
INT : ('+' | '-')? NUM ;
fragment NUM : ('0'..'9')+;
STRING :  '"' ~('"')+ '"' ;
WS : (' ' | '\t' | '\n' |'\r\n' )+  -> skip;
//WS : (' ' | '\t' | '\n' |'\r\n' )+ -> channel(HIDDEN);
