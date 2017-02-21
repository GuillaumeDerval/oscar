// Generated from /Users/gustavbjordal/OscaR/oscar/oscar-fzn/src/main/java/oscar/flatzinc/parser/Flatzinc.g4 by ANTLR 4.6
package oscar.flatzinc.parser;

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


import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link FlatzincParser}.
 */
public interface FlatzincListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link FlatzincParser#flatzinc_model}.
	 * @param ctx the parse tree
	 */
	void enterFlatzinc_model(FlatzincParser.Flatzinc_modelContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlatzincParser#flatzinc_model}.
	 * @param ctx the parse tree
	 */
	void exitFlatzinc_model(FlatzincParser.Flatzinc_modelContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlatzincParser#preddecl}.
	 * @param ctx the parse tree
	 */
	void enterPreddecl(FlatzincParser.PreddeclContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlatzincParser#preddecl}.
	 * @param ctx the parse tree
	 */
	void exitPreddecl(FlatzincParser.PreddeclContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlatzincParser#predparam}.
	 * @param ctx the parse tree
	 */
	void enterPredparam(FlatzincParser.PredparamContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlatzincParser#predparam}.
	 * @param ctx the parse tree
	 */
	void exitPredparam(FlatzincParser.PredparamContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlatzincParser#paramdecl}.
	 * @param ctx the parse tree
	 */
	void enterParamdecl(FlatzincParser.ParamdeclContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlatzincParser#paramdecl}.
	 * @param ctx the parse tree
	 */
	void exitParamdecl(FlatzincParser.ParamdeclContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlatzincParser#vardecl}.
	 * @param ctx the parse tree
	 */
	void enterVardecl(FlatzincParser.VardeclContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlatzincParser#vardecl}.
	 * @param ctx the parse tree
	 */
	void exitVardecl(FlatzincParser.VardeclContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlatzincParser#constraint}.
	 * @param ctx the parse tree
	 */
	void enterConstraint(FlatzincParser.ConstraintContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlatzincParser#constraint}.
	 * @param ctx the parse tree
	 */
	void exitConstraint(FlatzincParser.ConstraintContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlatzincParser#solvegoal}.
	 * @param ctx the parse tree
	 */
	void enterSolvegoal(FlatzincParser.SolvegoalContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlatzincParser#solvegoal}.
	 * @param ctx the parse tree
	 */
	void exitSolvegoal(FlatzincParser.SolvegoalContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlatzincParser#basicpartype}.
	 * @param ctx the parse tree
	 */
	void enterBasicpartype(FlatzincParser.BasicpartypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlatzincParser#basicpartype}.
	 * @param ctx the parse tree
	 */
	void exitBasicpartype(FlatzincParser.BasicpartypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlatzincParser#basicvartype}.
	 * @param ctx the parse tree
	 */
	void enterBasicvartype(FlatzincParser.BasicvartypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlatzincParser#basicvartype}.
	 * @param ctx the parse tree
	 */
	void exitBasicvartype(FlatzincParser.BasicvartypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlatzincParser#partype}.
	 * @param ctx the parse tree
	 */
	void enterPartype(FlatzincParser.PartypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlatzincParser#partype}.
	 * @param ctx the parse tree
	 */
	void exitPartype(FlatzincParser.PartypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlatzincParser#vartype}.
	 * @param ctx the parse tree
	 */
	void enterVartype(FlatzincParser.VartypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlatzincParser#vartype}.
	 * @param ctx the parse tree
	 */
	void exitVartype(FlatzincParser.VartypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlatzincParser#arraytype}.
	 * @param ctx the parse tree
	 */
	void enterArraytype(FlatzincParser.ArraytypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlatzincParser#arraytype}.
	 * @param ctx the parse tree
	 */
	void exitArraytype(FlatzincParser.ArraytypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlatzincParser#predparamtype}.
	 * @param ctx the parse tree
	 */
	void enterPredparamtype(FlatzincParser.PredparamtypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlatzincParser#predparamtype}.
	 * @param ctx the parse tree
	 */
	void exitPredparamtype(FlatzincParser.PredparamtypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlatzincParser#basicpredparamtype}.
	 * @param ctx the parse tree
	 */
	void enterBasicpredparamtype(FlatzincParser.BasicpredparamtypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlatzincParser#basicpredparamtype}.
	 * @param ctx the parse tree
	 */
	void exitBasicpredparamtype(FlatzincParser.BasicpredparamtypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlatzincParser#predarraytype}.
	 * @param ctx the parse tree
	 */
	void enterPredarraytype(FlatzincParser.PredarraytypeContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlatzincParser#predarraytype}.
	 * @param ctx the parse tree
	 */
	void exitPredarraytype(FlatzincParser.PredarraytypeContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlatzincParser#expr}.
	 * @param ctx the parse tree
	 */
	void enterExpr(FlatzincParser.ExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlatzincParser#expr}.
	 * @param ctx the parse tree
	 */
	void exitExpr(FlatzincParser.ExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlatzincParser#idorannot}.
	 * @param ctx the parse tree
	 */
	void enterIdorannot(FlatzincParser.IdorannotContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlatzincParser#idorannot}.
	 * @param ctx the parse tree
	 */
	void exitIdorannot(FlatzincParser.IdorannotContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlatzincParser#intorsetconst}.
	 * @param ctx the parse tree
	 */
	void enterIntorsetconst(FlatzincParser.IntorsetconstContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlatzincParser#intorsetconst}.
	 * @param ctx the parse tree
	 */
	void exitIntorsetconst(FlatzincParser.IntorsetconstContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlatzincParser#setconst}.
	 * @param ctx the parse tree
	 */
	void enterSetconst(FlatzincParser.SetconstContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlatzincParser#setconst}.
	 * @param ctx the parse tree
	 */
	void exitSetconst(FlatzincParser.SetconstContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlatzincParser#arrayexpr}.
	 * @param ctx the parse tree
	 */
	void enterArrayexpr(FlatzincParser.ArrayexprContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlatzincParser#arrayexpr}.
	 * @param ctx the parse tree
	 */
	void exitArrayexpr(FlatzincParser.ArrayexprContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlatzincParser#annotations}.
	 * @param ctx the parse tree
	 */
	void enterAnnotations(FlatzincParser.AnnotationsContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlatzincParser#annotations}.
	 * @param ctx the parse tree
	 */
	void exitAnnotations(FlatzincParser.AnnotationsContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlatzincParser#annotation}.
	 * @param ctx the parse tree
	 */
	void enterAnnotation(FlatzincParser.AnnotationContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlatzincParser#annotation}.
	 * @param ctx the parse tree
	 */
	void exitAnnotation(FlatzincParser.AnnotationContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlatzincParser#predannid}.
	 * @param ctx the parse tree
	 */
	void enterPredannid(FlatzincParser.PredannidContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlatzincParser#predannid}.
	 * @param ctx the parse tree
	 */
	void exitPredannid(FlatzincParser.PredannidContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlatzincParser#boolconst}.
	 * @param ctx the parse tree
	 */
	void enterBoolconst(FlatzincParser.BoolconstContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlatzincParser#boolconst}.
	 * @param ctx the parse tree
	 */
	void exitBoolconst(FlatzincParser.BoolconstContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlatzincParser#floatconst}.
	 * @param ctx the parse tree
	 */
	void enterFloatconst(FlatzincParser.FloatconstContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlatzincParser#floatconst}.
	 * @param ctx the parse tree
	 */
	void exitFloatconst(FlatzincParser.FloatconstContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlatzincParser#intconst}.
	 * @param ctx the parse tree
	 */
	void enterIntconst(FlatzincParser.IntconstContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlatzincParser#intconst}.
	 * @param ctx the parse tree
	 */
	void exitIntconst(FlatzincParser.IntconstContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlatzincParser#stringconstant}.
	 * @param ctx the parse tree
	 */
	void enterStringconstant(FlatzincParser.StringconstantContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlatzincParser#stringconstant}.
	 * @param ctx the parse tree
	 */
	void exitStringconstant(FlatzincParser.StringconstantContext ctx);
	/**
	 * Enter a parse tree produced by {@link FlatzincParser#varparid}.
	 * @param ctx the parse tree
	 */
	void enterVarparid(FlatzincParser.VarparidContext ctx);
	/**
	 * Exit a parse tree produced by {@link FlatzincParser#varparid}.
	 * @param ctx the parse tree
	 */
	void exitVarparid(FlatzincParser.VarparidContext ctx);
}