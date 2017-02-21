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

import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link FlatzincParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface FlatzincVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link FlatzincParser#flatzinc_model}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFlatzinc_model(FlatzincParser.Flatzinc_modelContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlatzincParser#preddecl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPreddecl(FlatzincParser.PreddeclContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlatzincParser#predparam}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPredparam(FlatzincParser.PredparamContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlatzincParser#paramdecl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitParamdecl(FlatzincParser.ParamdeclContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlatzincParser#vardecl}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVardecl(FlatzincParser.VardeclContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlatzincParser#constraint}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitConstraint(FlatzincParser.ConstraintContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlatzincParser#solvegoal}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSolvegoal(FlatzincParser.SolvegoalContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlatzincParser#basicpartype}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBasicpartype(FlatzincParser.BasicpartypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlatzincParser#basicvartype}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBasicvartype(FlatzincParser.BasicvartypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlatzincParser#partype}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPartype(FlatzincParser.PartypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlatzincParser#vartype}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVartype(FlatzincParser.VartypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlatzincParser#arraytype}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArraytype(FlatzincParser.ArraytypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlatzincParser#predparamtype}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPredparamtype(FlatzincParser.PredparamtypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlatzincParser#basicpredparamtype}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBasicpredparamtype(FlatzincParser.BasicpredparamtypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlatzincParser#predarraytype}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPredarraytype(FlatzincParser.PredarraytypeContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlatzincParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExpr(FlatzincParser.ExprContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlatzincParser#idorannot}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIdorannot(FlatzincParser.IdorannotContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlatzincParser#intorsetconst}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIntorsetconst(FlatzincParser.IntorsetconstContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlatzincParser#setconst}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitSetconst(FlatzincParser.SetconstContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlatzincParser#arrayexpr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitArrayexpr(FlatzincParser.ArrayexprContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlatzincParser#annotations}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAnnotations(FlatzincParser.AnnotationsContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlatzincParser#annotation}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAnnotation(FlatzincParser.AnnotationContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlatzincParser#predannid}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitPredannid(FlatzincParser.PredannidContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlatzincParser#boolconst}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBoolconst(FlatzincParser.BoolconstContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlatzincParser#floatconst}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFloatconst(FlatzincParser.FloatconstContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlatzincParser#intconst}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIntconst(FlatzincParser.IntconstContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlatzincParser#stringconstant}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitStringconstant(FlatzincParser.StringconstantContext ctx);
	/**
	 * Visit a parse tree produced by {@link FlatzincParser#varparid}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitVarparid(FlatzincParser.VarparidContext ctx);
}