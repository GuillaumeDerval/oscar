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

import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.ATN;
import org.antlr.v4.runtime.atn.ATNDeserializer;
import org.antlr.v4.runtime.atn.ParserATNSimulator;
import org.antlr.v4.runtime.atn.PredictionContextCache;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.tree.ParseTreeListener;
import org.antlr.v4.runtime.tree.TerminalNode;
import oscar.flatzinc.ParsingException;
import oscar.flatzinc.model.Annotation;
import oscar.flatzinc.model.DomainRange;
import oscar.flatzinc.parser.intermediatemodel.ArrayOfElement;
import oscar.flatzinc.parser.intermediatemodel.Element;
import oscar.flatzinc.parser.intermediatemodel.Type;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class FlatzincParser extends Parser {
	static { RuntimeMetaData.checkVersion("4.6", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, T__5=6, T__6=7, T__7=8, T__8=9, 
		T__9=10, T__10=11, T__11=12, T__12=13, T__13=14, T__14=15, T__15=16, T__16=17, 
		T__17=18, T__18=19, T__19=20, T__20=21, T__21=22, T__22=23, T__23=24, 
		T__24=25, Boolconst=26, PREDANNID=27, VARPARID=28, Floatconst=29, INT=30, 
		STRING=31, WS=32;
	public static final int
		RULE_flatzinc_model = 0, RULE_preddecl = 1, RULE_predparam = 2, RULE_paramdecl = 3, 
		RULE_vardecl = 4, RULE_constraint = 5, RULE_solvegoal = 6, RULE_basicpartype = 7, 
		RULE_basicvartype = 8, RULE_partype = 9, RULE_vartype = 10, RULE_arraytype = 11, 
		RULE_predparamtype = 12, RULE_basicpredparamtype = 13, RULE_predarraytype = 14, 
		RULE_expr = 15, RULE_idorannot = 16, RULE_intorsetconst = 17, RULE_setconst = 18, 
		RULE_arrayexpr = 19, RULE_annotations = 20, RULE_annotation = 21, RULE_predannid = 22, 
		RULE_boolconst = 23, RULE_floatconst = 24, RULE_intconst = 25, RULE_stringconstant = 26, 
		RULE_varparid = 27;
	public static final String[] ruleNames = {
		"flatzinc_model", "preddecl", "predparam", "paramdecl", "vardecl", "constraint", 
		"solvegoal", "basicpartype", "basicvartype", "partype", "vartype", "arraytype", 
		"predparamtype", "basicpredparamtype", "predarraytype", "expr", "idorannot", 
		"intorsetconst", "setconst", "arrayexpr", "annotations", "annotation", 
		"predannid", "boolconst", "floatconst", "intconst", "stringconstant", 
		"varparid"
	};

	private static final String[] _LITERAL_NAMES = {
		null, "'predicate'", "'('", "','", "')'", "';'", "':'", "'='", "'constraint'", 
		"'solve'", "'satisfy'", "'minimize'", "'maximize'", "'bool'", "'float'", 
		"'int'", "'set'", "'of'", "'var'", "'..'", "'array'", "'['", "']'", "'{'", 
		"'}'", "'::'"
	};
	private static final String[] _SYMBOLIC_NAMES = {
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, null, null, null, null, null, null, null, null, null, null, 
		null, null, "Boolconst", "PREDANNID", "VARPARID", "Floatconst", "INT", 
		"STRING", "WS"
	};
	public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

	/**
	 * @deprecated Use {@link #VOCABULARY} instead.
	 */
	@Deprecated
	public static final String[] tokenNames;
	static {
		tokenNames = new String[_SYMBOLIC_NAMES.length];
		for (int i = 0; i < tokenNames.length; i++) {
			tokenNames[i] = VOCABULARY.getLiteralName(i);
			if (tokenNames[i] == null) {
				tokenNames[i] = VOCABULARY.getSymbolicName(i);
			}

			if (tokenNames[i] == null) {
				tokenNames[i] = "<INVALID>";
			}
		}
	}

	@Override
	@Deprecated
	public String[] getTokenNames() {
		return tokenNames;
	}

	@Override

	public Vocabulary getVocabulary() {
		return VOCABULARY;
	}

	@Override
	public String getGrammarFileName() { return "Flatzinc.g4"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public ATN getATN() { return _ATN; }


		private Model m;
		public FlatzincParser(TokenStream input,Model m){
			this(input);
			this.m = m;
		}

	public FlatzincParser(TokenStream input) {
		super(input);
		_interp = new ParserATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}
	public static class Flatzinc_modelContext extends ParserRuleContext {
		public SolvegoalContext solvegoal() {
			return getRuleContext(SolvegoalContext.class,0);
		}
		public List<PreddeclContext> preddecl() {
			return getRuleContexts(PreddeclContext.class);
		}
		public PreddeclContext preddecl(int i) {
			return getRuleContext(PreddeclContext.class,i);
		}
		public List<ParamdeclContext> paramdecl() {
			return getRuleContexts(ParamdeclContext.class);
		}
		public ParamdeclContext paramdecl(int i) {
			return getRuleContext(ParamdeclContext.class,i);
		}
		public List<VardeclContext> vardecl() {
			return getRuleContexts(VardeclContext.class);
		}
		public VardeclContext vardecl(int i) {
			return getRuleContext(VardeclContext.class,i);
		}
		public List<ConstraintContext> constraint() {
			return getRuleContexts(ConstraintContext.class);
		}
		public ConstraintContext constraint(int i) {
			return getRuleContext(ConstraintContext.class,i);
		}
		public Flatzinc_modelContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_flatzinc_model; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).enterFlatzinc_model(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).exitFlatzinc_model(this);
		}
	}

	public final Flatzinc_modelContext flatzinc_model() throws RecognitionException {
		Flatzinc_modelContext _localctx = new Flatzinc_modelContext(_ctx, getState());
		enterRule(_localctx, 0, RULE_flatzinc_model);
		int _la;
		try {
			int _alt;
			enterOuterAlt(_localctx, 1);
			{
			setState(59);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__0) {
				{
				{
				setState(56);
				preddecl();
				}
				}
				setState(61);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(65);
			_errHandler.sync(this);
			_alt = getInterpreter().adaptivePredict(_input,1,_ctx);
			while ( _alt!=2 && _alt!=org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER ) {
				if ( _alt==1 ) {
					{
					{
					setState(62);
					paramdecl();
					}
					} 
				}
				setState(67);
				_errHandler.sync(this);
				_alt = getInterpreter().adaptivePredict(_input,1,_ctx);
			}
			setState(71);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__17 || _la==T__19) {
				{
				{
				setState(68);
				vardecl();
				}
				}
				setState(73);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(77);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__7) {
				{
				{
				setState(74);
				constraint();
				}
				}
				setState(79);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(80);
			solvegoal();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class PreddeclContext extends ParserRuleContext {
		public TerminalNode PREDANNID() { return getToken(FlatzincParser.PREDANNID, 0); }
		public List<PredparamContext> predparam() {
			return getRuleContexts(PredparamContext.class);
		}
		public PredparamContext predparam(int i) {
			return getRuleContext(PredparamContext.class,i);
		}
		public PreddeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_preddecl; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).enterPreddecl(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).exitPreddecl(this);
		}
	}

	public final PreddeclContext preddecl() throws RecognitionException {
		PreddeclContext _localctx = new PreddeclContext(_ctx, getState());
		enterRule(_localctx, 2, RULE_preddecl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(82);
			match(T__0);
			setState(83);
			match(PREDANNID);
			setState(84);
			match(T__1);
			setState(85);
			predparam();
			setState(90);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__2) {
				{
				{
				setState(86);
				match(T__2);
				setState(87);
				predparam();
				}
				}
				setState(92);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(93);
			match(T__3);
			setState(94);
			match(T__4);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class PredparamContext extends ParserRuleContext {
		public PredparamtypeContext predparamtype() {
			return getRuleContext(PredparamtypeContext.class,0);
		}
		public PredannidContext predannid() {
			return getRuleContext(PredannidContext.class,0);
		}
		public PredparamContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_predparam; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).enterPredparam(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).exitPredparam(this);
		}
	}

	public final PredparamContext predparam() throws RecognitionException {
		PredparamContext _localctx = new PredparamContext(_ctx, getState());
		enterRule(_localctx, 4, RULE_predparam);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(96);
			predparamtype();
			setState(97);
			match(T__5);
			setState(98);
			predannid();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ParamdeclContext extends ParserRuleContext {
		public PartypeContext partype;
		public VarparidContext varparid;
		public ExprContext expr;
		public PartypeContext partype() {
			return getRuleContext(PartypeContext.class,0);
		}
		public VarparidContext varparid() {
			return getRuleContext(VarparidContext.class,0);
		}
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public ParamdeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_paramdecl; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).enterParamdecl(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).exitParamdecl(this);
		}
	}

	public final ParamdeclContext paramdecl() throws RecognitionException {
		ParamdeclContext _localctx = new ParamdeclContext(_ctx, getState());
		enterRule(_localctx, 6, RULE_paramdecl);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(100);
			((ParamdeclContext)_localctx).partype = partype();
			setState(101);
			match(T__5);
			setState(102);
			((ParamdeclContext)_localctx).varparid = varparid();
			setState(103);
			match(T__6);
			setState(104);
			((ParamdeclContext)_localctx).expr = expr();
			setState(105);
			match(T__4);
			Element e = ((ParamdeclContext)_localctx).expr.e;
				e.name = ((ParamdeclContext)_localctx).varparid.text;
				e.typ = ((ParamdeclContext)_localctx).partype.t;
				m.addId(e.name,e);
				//TODO: Check that expr is a boolconst, floatconst, intconst, setconst, or an array thereof.
				
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class VardeclContext extends ParserRuleContext {
		public Element e = null;
		public VartypeContext vartype;
		public VarparidContext varparid;
		public AnnotationsContext annotations;
		public ExprContext expr;
		public VartypeContext vartype() {
			return getRuleContext(VartypeContext.class,0);
		}
		public VarparidContext varparid() {
			return getRuleContext(VarparidContext.class,0);
		}
		public AnnotationsContext annotations() {
			return getRuleContext(AnnotationsContext.class,0);
		}
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public VardeclContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_vardecl; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).enterVardecl(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).exitVardecl(this);
		}
	}

	public final VardeclContext vardecl() throws RecognitionException {
		VardeclContext _localctx = new VardeclContext(_ctx, getState());
		enterRule(_localctx, 8, RULE_vardecl);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(108);
			((VardeclContext)_localctx).vartype = vartype();
			setState(109);
			match(T__5);
			setState(110);
			((VardeclContext)_localctx).varparid = varparid();
			setState(111);
			((VardeclContext)_localctx).annotations = annotations();
			setState(116);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==T__6) {
				{
				setState(112);
				match(T__6);
				setState(113);
				((VardeclContext)_localctx).expr = expr();
				((VardeclContext)_localctx).e =  ((VardeclContext)_localctx).expr.e;
				}
			}

			setState(118);
			match(T__4);
			if(_localctx.e==null){
				  m.addNewVariable(((VardeclContext)_localctx).vartype.t,((VardeclContext)_localctx).vartype.d,((VardeclContext)_localctx).varparid.text, ((VardeclContext)_localctx).annotations.anns);
				}else{
				  m.addAliasVariable(((VardeclContext)_localctx).vartype.t,((VardeclContext)_localctx).vartype.d,((VardeclContext)_localctx).varparid.text, _localctx.e, ((VardeclContext)_localctx).annotations.anns);
				}
				// TODO: Check that Any vars in assignments must be declared earlier.
				
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ConstraintContext extends ParserRuleContext {
		public List<Element> args = null;;
		public PredannidContext predannid;
		public ExprContext e;
		public ExprContext e1;
		public AnnotationsContext annotations;
		public PredannidContext predannid() {
			return getRuleContext(PredannidContext.class,0);
		}
		public AnnotationsContext annotations() {
			return getRuleContext(AnnotationsContext.class,0);
		}
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public ConstraintContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_constraint; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).enterConstraint(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).exitConstraint(this);
		}
	}

	public final ConstraintContext constraint() throws RecognitionException {
		ConstraintContext _localctx = new ConstraintContext(_ctx, getState());
		enterRule(_localctx, 10, RULE_constraint);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(121);
			match(T__7);
			setState(122);
			((ConstraintContext)_localctx).predannid = predannid();
			setState(123);
			match(T__1);
			setState(124);
			((ConstraintContext)_localctx).e = expr();
			((ConstraintContext)_localctx).args =  new ArrayList<Element>(); _localctx.args.add(((ConstraintContext)_localctx).e.e);
			setState(132);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__2) {
				{
				{
				setState(126);
				match(T__2);
				setState(127);
				((ConstraintContext)_localctx).e1 = expr();
				_localctx.args.add(((ConstraintContext)_localctx).e1.e);
				}
				}
				setState(134);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			setState(135);
			match(T__3);
			setState(136);
			((ConstraintContext)_localctx).annotations = annotations();
			m.addConstraint(((ConstraintContext)_localctx).predannid.text,_localctx.args,((ConstraintContext)_localctx).annotations.anns);
			setState(138);
			match(T__4);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SolvegoalContext extends ParserRuleContext {
		public AnnotationsContext annotations;
		public ExprContext expr;
		public AnnotationsContext annotations() {
			return getRuleContext(AnnotationsContext.class,0);
		}
		public ExprContext expr() {
			return getRuleContext(ExprContext.class,0);
		}
		public SolvegoalContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_solvegoal; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).enterSolvegoal(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).exitSolvegoal(this);
		}
	}

	public final SolvegoalContext solvegoal() throws RecognitionException {
		SolvegoalContext _localctx = new SolvegoalContext(_ctx, getState());
		enterRule(_localctx, 12, RULE_solvegoal);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(140);
			match(T__8);
			setState(141);
			((SolvegoalContext)_localctx).annotations = annotations();
			setState(155);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case T__9:
				{
				setState(142);
				match(T__9);
				setState(143);
				match(T__4);
				m.setSATObjective(((SolvegoalContext)_localctx).annotations.anns);
				}
				break;
			case T__10:
				{
				setState(145);
				match(T__10);
				setState(146);
				((SolvegoalContext)_localctx).expr = expr();
				setState(147);
				match(T__4);
				m.setMINObjective(((SolvegoalContext)_localctx).expr.e,((SolvegoalContext)_localctx).annotations.anns);
				}
				break;
			case T__11:
				{
				setState(150);
				match(T__11);
				setState(151);
				((SolvegoalContext)_localctx).expr = expr();
				setState(152);
				match(T__4);
				m.setMAXObjective(((SolvegoalContext)_localctx).expr.e,((SolvegoalContext)_localctx).annotations.anns);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class BasicpartypeContext extends ParserRuleContext {
		public Type t;
		public BasicpartypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_basicpartype; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).enterBasicpartype(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).exitBasicpartype(this);
		}
	}

	public final BasicpartypeContext basicpartype() throws RecognitionException {
		BasicpartypeContext _localctx = new BasicpartypeContext(_ctx, getState());
		enterRule(_localctx, 14, RULE_basicpartype);
		try {
			setState(167);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case T__12:
				enterOuterAlt(_localctx, 1);
				{
				setState(157);
				match(T__12);
				((BasicpartypeContext)_localctx).t =  new Type(Type.BOOL);
				}
				break;
			case T__13:
				enterOuterAlt(_localctx, 2);
				{
				setState(159);
				match(T__13);
				((BasicpartypeContext)_localctx).t =  new Type(Type.FLOAT);
				}
				break;
			case T__14:
				enterOuterAlt(_localctx, 3);
				{
				setState(161);
				match(T__14);
				((BasicpartypeContext)_localctx).t =  new Type(Type.INT);
				}
				break;
			case T__15:
				enterOuterAlt(_localctx, 4);
				{
				setState(163);
				match(T__15);
				setState(164);
				match(T__16);
				setState(165);
				match(T__14);
				((BasicpartypeContext)_localctx).t =  new Type(Type.SET);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class BasicvartypeContext extends ParserRuleContext {
		public Type t;
		public Element d;
		public SetconstContext setconst;
		public List<FloatconstContext> floatconst() {
			return getRuleContexts(FloatconstContext.class);
		}
		public FloatconstContext floatconst(int i) {
			return getRuleContext(FloatconstContext.class,i);
		}
		public SetconstContext setconst() {
			return getRuleContext(SetconstContext.class,0);
		}
		public BasicvartypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_basicvartype; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).enterBasicvartype(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).exitBasicvartype(this);
		}
	}

	public final BasicvartypeContext basicvartype() throws RecognitionException {
		BasicvartypeContext _localctx = new BasicvartypeContext(_ctx, getState());
		enterRule(_localctx, 16, RULE_basicvartype);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(169);
			match(T__17);
			setState(189);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case T__12:
				{
				setState(170);
				match(T__12);
				((BasicvartypeContext)_localctx).t =  new Type(Type.BOOL); (_localctx.t).isVar = true;
				}
				break;
			case T__13:
				{
				setState(172);
				match(T__13);
				((BasicvartypeContext)_localctx).t =  new Type(Type.FLOAT); (_localctx.t).isVar = true;
				}
				break;
			case Floatconst:
				{
				setState(174);
				floatconst();
				setState(175);
				match(T__18);
				setState(176);
				floatconst();
				((BasicvartypeContext)_localctx).t =  new Type(Type.FLOAT); (_localctx.t).isVar = true;
				}
				break;
			case T__14:
				{
				setState(179);
				match(T__14);
				((BasicvartypeContext)_localctx).t =  new Type(Type.INT); (_localctx.t).isVar = true;
				}
				break;
			case T__22:
			case INT:
				{
				setState(181);
				((BasicvartypeContext)_localctx).setconst = setconst();
				((BasicvartypeContext)_localctx).t =  new Type(Type.INT); (_localctx.t).isVar = true; ((BasicvartypeContext)_localctx).d =  ((BasicvartypeContext)_localctx).setconst.e;
				}
				break;
			case T__15:
				{
				setState(184);
				match(T__15);
				setState(185);
				match(T__16);
				setState(186);
				((BasicvartypeContext)_localctx).setconst = setconst();
				((BasicvartypeContext)_localctx).t =  new Type(Type.SET); (_localctx.t).isVar = true; ((BasicvartypeContext)_localctx).d =  ((BasicvartypeContext)_localctx).setconst.e;
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class PartypeContext extends ParserRuleContext {
		public Type t;
		public BasicpartypeContext basicpartype;
		public ArraytypeContext arraytype;
		public BasicpartypeContext basicpartype() {
			return getRuleContext(BasicpartypeContext.class,0);
		}
		public ArraytypeContext arraytype() {
			return getRuleContext(ArraytypeContext.class,0);
		}
		public PartypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_partype; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).enterPartype(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).exitPartype(this);
		}
	}

	public final PartypeContext partype() throws RecognitionException {
		PartypeContext _localctx = new PartypeContext(_ctx, getState());
		enterRule(_localctx, 18, RULE_partype);
		try {
			setState(198);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case T__12:
			case T__13:
			case T__14:
			case T__15:
				enterOuterAlt(_localctx, 1);
				{
				setState(191);
				((PartypeContext)_localctx).basicpartype = basicpartype();
				((PartypeContext)_localctx).t =  ((PartypeContext)_localctx).basicpartype.t;
				}
				break;
			case T__19:
				enterOuterAlt(_localctx, 2);
				{
				setState(194);
				((PartypeContext)_localctx).arraytype = arraytype();
				setState(195);
				((PartypeContext)_localctx).basicpartype = basicpartype();
				((PartypeContext)_localctx).t =  ((PartypeContext)_localctx).basicpartype.t; (_localctx.t).isArray=true; (_localctx.t).size = ((PartypeContext)_localctx).arraytype.size;
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class VartypeContext extends ParserRuleContext {
		public Type t;
		public Element d;
		public BasicvartypeContext basicvartype;
		public ArraytypeContext arraytype;
		public BasicvartypeContext basicvartype() {
			return getRuleContext(BasicvartypeContext.class,0);
		}
		public ArraytypeContext arraytype() {
			return getRuleContext(ArraytypeContext.class,0);
		}
		public VartypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_vartype; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).enterVartype(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).exitVartype(this);
		}
	}

	public final VartypeContext vartype() throws RecognitionException {
		VartypeContext _localctx = new VartypeContext(_ctx, getState());
		enterRule(_localctx, 20, RULE_vartype);
		try {
			setState(207);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case T__17:
				enterOuterAlt(_localctx, 1);
				{
				setState(200);
				((VartypeContext)_localctx).basicvartype = basicvartype();
				((VartypeContext)_localctx).t =  ((VartypeContext)_localctx).basicvartype.t; ((VartypeContext)_localctx).d =  ((VartypeContext)_localctx).basicvartype.d;
				}
				break;
			case T__19:
				enterOuterAlt(_localctx, 2);
				{
				setState(203);
				((VartypeContext)_localctx).arraytype = arraytype();
				setState(204);
				((VartypeContext)_localctx).basicvartype = basicvartype();
				((VartypeContext)_localctx).t =  ((VartypeContext)_localctx).basicvartype.t; (_localctx.t).isArray=true; (_localctx.t).size = ((VartypeContext)_localctx).arraytype.size; ((VartypeContext)_localctx).d =  ((VartypeContext)_localctx).basicvartype.d;
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ArraytypeContext extends ParserRuleContext {
		public int size;
		public IntconstContext lb;
		public IntconstContext ub;
		public List<IntconstContext> intconst() {
			return getRuleContexts(IntconstContext.class);
		}
		public IntconstContext intconst(int i) {
			return getRuleContext(IntconstContext.class,i);
		}
		public ArraytypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_arraytype; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).enterArraytype(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).exitArraytype(this);
		}
	}

	public final ArraytypeContext arraytype() throws RecognitionException {
		ArraytypeContext _localctx = new ArraytypeContext(_ctx, getState());
		enterRule(_localctx, 22, RULE_arraytype);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(209);
			match(T__19);
			setState(210);
			match(T__20);
			setState(211);
			((ArraytypeContext)_localctx).lb = intconst();
			setState(212);
			match(T__18);
			setState(213);
			((ArraytypeContext)_localctx).ub = intconst();
			((ArraytypeContext)_localctx).size =  ((ArraytypeContext)_localctx).ub.i; if(((ArraytypeContext)_localctx).lb.i!=1) throw new ParsingException("Ranges of array must start at 1");
			setState(215);
			match(T__21);
			setState(216);
			match(T__16);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class PredparamtypeContext extends ParserRuleContext {
		public Type t;
		public BasicpredparamtypeContext basicpredparamtype;
		public PredarraytypeContext predarraytype;
		public BasicpredparamtypeContext basicpredparamtype() {
			return getRuleContext(BasicpredparamtypeContext.class,0);
		}
		public PredarraytypeContext predarraytype() {
			return getRuleContext(PredarraytypeContext.class,0);
		}
		public PredparamtypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_predparamtype; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).enterPredparamtype(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).exitPredparamtype(this);
		}
	}

	public final PredparamtypeContext predparamtype() throws RecognitionException {
		PredparamtypeContext _localctx = new PredparamtypeContext(_ctx, getState());
		enterRule(_localctx, 24, RULE_predparamtype);
		try {
			setState(225);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case T__12:
			case T__13:
			case T__14:
			case T__15:
			case T__17:
			case T__22:
			case Floatconst:
			case INT:
				enterOuterAlt(_localctx, 1);
				{
				setState(218);
				((PredparamtypeContext)_localctx).basicpredparamtype = basicpredparamtype();
				((PredparamtypeContext)_localctx).t =  ((PredparamtypeContext)_localctx).basicpredparamtype.t;
				}
				break;
			case T__19:
				enterOuterAlt(_localctx, 2);
				{
				setState(221);
				((PredparamtypeContext)_localctx).predarraytype = predarraytype();
				setState(222);
				((PredparamtypeContext)_localctx).basicpredparamtype = basicpredparamtype();
				((PredparamtypeContext)_localctx).t =  ((PredparamtypeContext)_localctx).basicpredparamtype.t; (_localctx.t).isArray=true; (_localctx.t).size = ((PredparamtypeContext)_localctx).predarraytype.size;
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class BasicpredparamtypeContext extends ParserRuleContext {
		public Type t;
		public BasicvartypeContext basicvartype;
		public BasicpartypeContext basicpartype;
		public BasicvartypeContext basicvartype() {
			return getRuleContext(BasicvartypeContext.class,0);
		}
		public BasicpartypeContext basicpartype() {
			return getRuleContext(BasicpartypeContext.class,0);
		}
		public List<FloatconstContext> floatconst() {
			return getRuleContexts(FloatconstContext.class);
		}
		public FloatconstContext floatconst(int i) {
			return getRuleContext(FloatconstContext.class,i);
		}
		public SetconstContext setconst() {
			return getRuleContext(SetconstContext.class,0);
		}
		public BasicpredparamtypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_basicpredparamtype; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).enterBasicpredparamtype(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).exitBasicpredparamtype(this);
		}
	}

	public final BasicpredparamtypeContext basicpredparamtype() throws RecognitionException {
		BasicpredparamtypeContext _localctx = new BasicpredparamtypeContext(_ctx, getState());
		enterRule(_localctx, 26, RULE_basicpredparamtype);
		try {
			setState(251);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,13,_ctx) ) {
			case 1:
				enterOuterAlt(_localctx, 1);
				{
				setState(227);
				((BasicpredparamtypeContext)_localctx).basicvartype = basicvartype();
				((BasicpredparamtypeContext)_localctx).t =  ((BasicpredparamtypeContext)_localctx).basicvartype.t;
				}
				break;
			case 2:
				enterOuterAlt(_localctx, 2);
				{
				setState(230);
				((BasicpredparamtypeContext)_localctx).basicpartype = basicpartype();
				((BasicpredparamtypeContext)_localctx).t =  ((BasicpredparamtypeContext)_localctx).basicpartype.t;
				}
				break;
			case 3:
				enterOuterAlt(_localctx, 3);
				{
				setState(233);
				floatconst();
				setState(234);
				match(T__18);
				setState(235);
				floatconst();
				((BasicpredparamtypeContext)_localctx).t =  new Type(Type.FLOAT);
				}
				break;
			case 4:
				enterOuterAlt(_localctx, 4);
				{
				setState(238);
				setconst();
				((BasicpredparamtypeContext)_localctx).t =  new Type(Type.INT);
				}
				break;
			case 5:
				enterOuterAlt(_localctx, 5);
				{
				setState(241);
				match(T__15);
				setState(242);
				match(T__16);
				setState(243);
				setconst();
				((BasicpredparamtypeContext)_localctx).t =  new Type(Type.SET);
				}
				break;
			case 6:
				enterOuterAlt(_localctx, 6);
				{
				setState(246);
				match(T__17);
				setState(247);
				match(T__15);
				setState(248);
				match(T__16);
				setState(249);
				match(T__14);
				((BasicpredparamtypeContext)_localctx).t =  new Type(Type.SET); (_localctx.t).isVar = true;
				}
				break;
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class PredarraytypeContext extends ParserRuleContext {
		public int size;
		public IntconstContext lb;
		public IntconstContext ub;
		public List<IntconstContext> intconst() {
			return getRuleContexts(IntconstContext.class);
		}
		public IntconstContext intconst(int i) {
			return getRuleContext(IntconstContext.class,i);
		}
		public PredarraytypeContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_predarraytype; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).enterPredarraytype(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).exitPredarraytype(this);
		}
	}

	public final PredarraytypeContext predarraytype() throws RecognitionException {
		PredarraytypeContext _localctx = new PredarraytypeContext(_ctx, getState());
		enterRule(_localctx, 28, RULE_predarraytype);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(253);
			match(T__19);
			setState(254);
			match(T__20);
			setState(266);
			_errHandler.sync(this);
			switch ( getInterpreter().adaptivePredict(_input,14,_ctx) ) {
			case 1:
				{
				setState(255);
				((PredarraytypeContext)_localctx).lb = intconst();
				setState(256);
				match(T__18);
				setState(257);
				((PredarraytypeContext)_localctx).ub = intconst();
				((PredarraytypeContext)_localctx).size =  ((PredarraytypeContext)_localctx).ub.i; if(((PredarraytypeContext)_localctx).lb.i!=1) throw new ParsingException("Ranges of array must start at 1");
				}
				break;
			case 2:
				{
				setState(260);
				match(T__14);
				((PredarraytypeContext)_localctx).size =  -1;
				}
				break;
			case 3:
				{
				setState(262);
				match(T__14);
				setState(263);
				match(T__2);
				setState(264);
				match(T__14);
				((PredarraytypeContext)_localctx).size =  -1;
				}
				break;
			}
			setState(268);
			match(T__21);
			setState(269);
			match(T__16);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ExprContext extends ParserRuleContext {
		public Element e;
		public BoolconstContext boolconst;
		public FloatconstContext floatconst;
		public IntorsetconstContext intorsetconst;
		public IdorannotContext idorannot;
		public ArrayexprContext arrayexpr;
		public StringconstantContext stringconstant;
		public BoolconstContext boolconst() {
			return getRuleContext(BoolconstContext.class,0);
		}
		public FloatconstContext floatconst() {
			return getRuleContext(FloatconstContext.class,0);
		}
		public IntorsetconstContext intorsetconst() {
			return getRuleContext(IntorsetconstContext.class,0);
		}
		public IdorannotContext idorannot() {
			return getRuleContext(IdorannotContext.class,0);
		}
		public ArrayexprContext arrayexpr() {
			return getRuleContext(ArrayexprContext.class,0);
		}
		public StringconstantContext stringconstant() {
			return getRuleContext(StringconstantContext.class,0);
		}
		public ExprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_expr; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).enterExpr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).exitExpr(this);
		}
	}

	public final ExprContext expr() throws RecognitionException {
		ExprContext _localctx = new ExprContext(_ctx, getState());
		enterRule(_localctx, 30, RULE_expr);
		try {
			setState(289);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case Boolconst:
				enterOuterAlt(_localctx, 1);
				{
				setState(271);
				((ExprContext)_localctx).boolconst = boolconst();
				((ExprContext)_localctx).e =  new Element(); (_localctx.e).value = ((ExprContext)_localctx).boolconst.b; (_localctx.e).typ = new Type(Type.BOOL);
				}
				break;
			case Floatconst:
				enterOuterAlt(_localctx, 2);
				{
				setState(274);
				((ExprContext)_localctx).floatconst = floatconst();
				((ExprContext)_localctx).e =  new Element(); (_localctx.e).value = ((ExprContext)_localctx).floatconst.f; (_localctx.e).typ = new Type(Type.FLOAT);
				}
				break;
			case T__22:
			case INT:
				enterOuterAlt(_localctx, 3);
				{
				setState(277);
				((ExprContext)_localctx).intorsetconst = intorsetconst();
				((ExprContext)_localctx).e =  ((ExprContext)_localctx).intorsetconst.e;
				}
				break;
			case PREDANNID:
			case VARPARID:
				enterOuterAlt(_localctx, 4);
				{
				setState(280);
				((ExprContext)_localctx).idorannot = idorannot();
				((ExprContext)_localctx).e =  ((ExprContext)_localctx).idorannot.e;
				}
				break;
			case T__20:
				enterOuterAlt(_localctx, 5);
				{
				setState(283);
				((ExprContext)_localctx).arrayexpr = arrayexpr();
				((ExprContext)_localctx).e =  ((ExprContext)_localctx).arrayexpr.a;
				}
				break;
			case STRING:
				enterOuterAlt(_localctx, 6);
				{
				setState(286);
				((ExprContext)_localctx).stringconstant = stringconstant();
				((ExprContext)_localctx).e =  new Element(); (_localctx.e).value = ((ExprContext)_localctx).stringconstant.str; (_localctx.e).typ = new Type(Type.STRING);// TODO: Check this: Annotation and string expressions are only permitted in annotation arguments. 
					
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class IdorannotContext extends ParserRuleContext {
		public Element e;
		public Annotation ann;
		public VarparidContext varparid;
		public IntconstContext intconst;
		public ExprContext expr;
		public VarparidContext varparid() {
			return getRuleContext(VarparidContext.class,0);
		}
		public IntconstContext intconst() {
			return getRuleContext(IntconstContext.class,0);
		}
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public IdorannotContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_idorannot; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).enterIdorannot(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).exitIdorannot(this);
		}
	}

	public final IdorannotContext idorannot() throws RecognitionException {
		IdorannotContext _localctx = new IdorannotContext(_ctx, getState());
		enterRule(_localctx, 32, RULE_idorannot);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(291);
			((IdorannotContext)_localctx).varparid = varparid();
			setState(313);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case T__2:
			case T__3:
			case T__4:
			case T__21:
				{
				((IdorannotContext)_localctx).e =  m.findId(((IdorannotContext)_localctx).varparid.text); 
				}
				break;
			case T__20:
				{
				setState(293);
				match(T__20);
				setState(294);
				((IdorannotContext)_localctx).intconst = intconst();
				setState(295);
				match(T__21);
				((IdorannotContext)_localctx).e =  ((ArrayOfElement)m.findId(((IdorannotContext)_localctx).varparid.text)).elements.get(((IdorannotContext)_localctx).intconst.i-1); 
				}
				break;
			case T__1:
				{
				((IdorannotContext)_localctx).ann =  new Annotation(((IdorannotContext)_localctx).varparid.text); ((IdorannotContext)_localctx).e =  new Element(); (_localctx.e).value = _localctx.ann; (_localctx.e).typ = new Type(Type.ANNOTATION);
				setState(299);
				match(T__1);
				setState(300);
				((IdorannotContext)_localctx).expr = expr();
				m.addAnnArg(_localctx.ann,((IdorannotContext)_localctx).expr.e);
				setState(308);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==T__2) {
					{
					{
					setState(302);
					match(T__2);
					setState(303);
					((IdorannotContext)_localctx).expr = expr();
					m.addAnnArg(_localctx.ann,((IdorannotContext)_localctx).expr.e);
					}
					}
					setState(310);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(311);
				match(T__3);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class IntorsetconstContext extends ParserRuleContext {
		public Element e;
		public Set<Integer> s;
		public IntconstContext lb;
		public IntconstContext intconst;
		public IntconstContext ub;
		public IntconstContext f;
		public IntconstContext n;
		public List<IntconstContext> intconst() {
			return getRuleContexts(IntconstContext.class);
		}
		public IntconstContext intconst(int i) {
			return getRuleContext(IntconstContext.class,i);
		}
		public IntorsetconstContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_intorsetconst; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).enterIntorsetconst(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).exitIntorsetconst(this);
		}
	}

	public final IntorsetconstContext intorsetconst() throws RecognitionException {
		IntorsetconstContext _localctx = new IntorsetconstContext(_ctx, getState());
		enterRule(_localctx, 34, RULE_intorsetconst);
		int _la;
		try {
			setState(340);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case INT:
				enterOuterAlt(_localctx, 1);
				{
				setState(315);
				((IntorsetconstContext)_localctx).lb = ((IntorsetconstContext)_localctx).intconst = intconst();
				setState(321);
				_errHandler.sync(this);
				switch (_input.LA(1)) {
				case T__2:
				case T__3:
				case T__4:
				case T__21:
					{
					((IntorsetconstContext)_localctx).e =  new Element(); (_localctx.e).value = ((IntorsetconstContext)_localctx).intconst.i; (_localctx.e).typ = new Type(Type.INT);
					}
					break;
				case T__18:
					{
					setState(317);
					match(T__18);
					setState(318);
					((IntorsetconstContext)_localctx).ub = ((IntorsetconstContext)_localctx).intconst = intconst();
					((IntorsetconstContext)_localctx).e =  new Element(); (_localctx.e).value = new DomainRange(((IntorsetconstContext)_localctx).lb.i,((IntorsetconstContext)_localctx).ub.i); (_localctx.e).typ = new Type(Type.SET);
					}
					break;
				default:
					throw new NoViableAltException(this);
				}
				}
				break;
			case T__22:
				enterOuterAlt(_localctx, 2);
				{
				setState(323);
				match(T__22);
				((IntorsetconstContext)_localctx).s =  new HashSet<Integer>();
				setState(336);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==INT) {
					{
					setState(325);
					((IntorsetconstContext)_localctx).f = intconst();
					 _localctx.s.add(((IntorsetconstContext)_localctx).f.i); 
					setState(333);
					_errHandler.sync(this);
					_la = _input.LA(1);
					while (_la==T__2) {
						{
						{
						setState(327);
						match(T__2);
						setState(328);
						((IntorsetconstContext)_localctx).n = intconst();
						 _localctx.s.add(((IntorsetconstContext)_localctx).n.i);
						}
						}
						setState(335);
						_errHandler.sync(this);
						_la = _input.LA(1);
					}
					}
				}

				setState(338);
				match(T__23);
				((IntorsetconstContext)_localctx).e =  new Element(); (_localctx.e).value =m.createDomainSet(_localctx.s); (_localctx.e).typ = new Type(Type.SET);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class SetconstContext extends ParserRuleContext {
		public Element e;
		public Set<Integer> s;
		public IntconstContext lb;
		public IntconstContext ub;
		public IntconstContext f;
		public IntconstContext n;
		public List<IntconstContext> intconst() {
			return getRuleContexts(IntconstContext.class);
		}
		public IntconstContext intconst(int i) {
			return getRuleContext(IntconstContext.class,i);
		}
		public SetconstContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_setconst; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).enterSetconst(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).exitSetconst(this);
		}
	}

	public final SetconstContext setconst() throws RecognitionException {
		SetconstContext _localctx = new SetconstContext(_ctx, getState());
		enterRule(_localctx, 36, RULE_setconst);
		int _la;
		try {
			setState(364);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case INT:
				enterOuterAlt(_localctx, 1);
				{
				setState(342);
				((SetconstContext)_localctx).lb = intconst();
				setState(343);
				match(T__18);
				setState(344);
				((SetconstContext)_localctx).ub = intconst();
				((SetconstContext)_localctx).e =  new Element(); (_localctx.e).value = new DomainRange(((SetconstContext)_localctx).lb.i,((SetconstContext)_localctx).ub.i); 
				}
				break;
			case T__22:
				enterOuterAlt(_localctx, 2);
				{
				setState(347);
				match(T__22);
				((SetconstContext)_localctx).s =  new HashSet<Integer>();
				setState(360);
				_errHandler.sync(this);
				_la = _input.LA(1);
				if (_la==INT) {
					{
					setState(349);
					((SetconstContext)_localctx).f = intconst();
					 _localctx.s.add(((SetconstContext)_localctx).f.i); 
					setState(357);
					_errHandler.sync(this);
					_la = _input.LA(1);
					while (_la==T__2) {
						{
						{
						setState(351);
						match(T__2);
						setState(352);
						((SetconstContext)_localctx).n = intconst();
						 _localctx.s.add(((SetconstContext)_localctx).n.i);
						}
						}
						setState(359);
						_errHandler.sync(this);
						_la = _input.LA(1);
					}
					}
				}

				setState(362);
				match(T__23);
				((SetconstContext)_localctx).e =  new Element(); (_localctx.e).value =m.createDomainSet(_localctx.s);
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class ArrayexprContext extends ParserRuleContext {
		public ArrayOfElement a;
		public ExprContext e;
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public ArrayexprContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_arrayexpr; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).enterArrayexpr(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).exitArrayexpr(this);
		}
	}

	public final ArrayexprContext arrayexpr() throws RecognitionException {
		ArrayexprContext _localctx = new ArrayexprContext(_ctx, getState());
		enterRule(_localctx, 38, RULE_arrayexpr);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(366);
			match(T__20);
			((ArrayexprContext)_localctx).a =  new ArrayOfElement(); (_localctx.a).typ = new Type(Type.NULL); (_localctx.a).typ.isArray = true; _localctx.a.typ.size = 0;
			setState(379);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if ((((_la) & ~0x3f) == 0 && ((1L << _la) & ((1L << T__20) | (1L << T__22) | (1L << Boolconst) | (1L << PREDANNID) | (1L << VARPARID) | (1L << Floatconst) | (1L << INT) | (1L << STRING))) != 0)) {
				{
				setState(368);
				((ArrayexprContext)_localctx).e = expr();
				_localctx.a.elements.add(((ArrayexprContext)_localctx).e.e); _localctx.a.typ.size+=1; if(((ArrayexprContext)_localctx).e.e.typ.isVar)_localctx.a.typ.isVar = true; 
				setState(376);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==T__2) {
					{
					{
					setState(370);
					match(T__2);
					setState(371);
					((ArrayexprContext)_localctx).e = expr();
					_localctx.a.elements.add(((ArrayexprContext)_localctx).e.e); _localctx.a.typ.size+=1; if(((ArrayexprContext)_localctx).e.e.typ.isVar)_localctx.a.typ.isVar = true; 
					}
					}
					setState(378);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				}
			}

			setState(381);
			match(T__21);
			_localctx.a.close();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class AnnotationsContext extends ParserRuleContext {
		public ArrayList<Annotation> anns;
		public AnnotationContext annotation;
		public List<AnnotationContext> annotation() {
			return getRuleContexts(AnnotationContext.class);
		}
		public AnnotationContext annotation(int i) {
			return getRuleContext(AnnotationContext.class,i);
		}
		public AnnotationsContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_annotations; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).enterAnnotations(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).exitAnnotations(this);
		}
	}

	public final AnnotationsContext annotations() throws RecognitionException {
		AnnotationsContext _localctx = new AnnotationsContext(_ctx, getState());
		enterRule(_localctx, 40, RULE_annotations);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			((AnnotationsContext)_localctx).anns =  new ArrayList<Annotation>();
			setState(391);
			_errHandler.sync(this);
			_la = _input.LA(1);
			while (_la==T__24) {
				{
				{
				setState(385);
				match(T__24);
				setState(386);
				((AnnotationsContext)_localctx).annotation = annotation();
				_localctx.anns.add(((AnnotationsContext)_localctx).annotation.ann);
				}
				}
				setState(393);
				_errHandler.sync(this);
				_la = _input.LA(1);
			}
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class AnnotationContext extends ParserRuleContext {
		public Annotation ann;
		public PredannidContext predannid;
		public ExprContext expr;
		public PredannidContext predannid() {
			return getRuleContext(PredannidContext.class,0);
		}
		public List<ExprContext> expr() {
			return getRuleContexts(ExprContext.class);
		}
		public ExprContext expr(int i) {
			return getRuleContext(ExprContext.class,i);
		}
		public AnnotationContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_annotation; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).enterAnnotation(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).exitAnnotation(this);
		}
	}

	public final AnnotationContext annotation() throws RecognitionException {
		AnnotationContext _localctx = new AnnotationContext(_ctx, getState());
		enterRule(_localctx, 42, RULE_annotation);
		int _la;
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(394);
			((AnnotationContext)_localctx).predannid = predannid();
			((AnnotationContext)_localctx).ann =  new Annotation(((AnnotationContext)_localctx).predannid.text);
			setState(410);
			_errHandler.sync(this);
			_la = _input.LA(1);
			if (_la==T__1) {
				{
				setState(396);
				match(T__1);
				setState(397);
				((AnnotationContext)_localctx).expr = expr();
				m.addAnnArg(_localctx.ann,((AnnotationContext)_localctx).expr.e);
				setState(405);
				_errHandler.sync(this);
				_la = _input.LA(1);
				while (_la==T__2) {
					{
					{
					setState(399);
					match(T__2);
					setState(400);
					((AnnotationContext)_localctx).expr = expr();
					m.addAnnArg(_localctx.ann,((AnnotationContext)_localctx).expr.e);
					}
					}
					setState(407);
					_errHandler.sync(this);
					_la = _input.LA(1);
				}
				setState(408);
				match(T__3);
				}
			}

			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class PredannidContext extends ParserRuleContext {
		public String text;
		public Token PREDANNID;
		public TerminalNode PREDANNID() { return getToken(FlatzincParser.PREDANNID, 0); }
		public PredannidContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_predannid; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).enterPredannid(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).exitPredannid(this);
		}
	}

	public final PredannidContext predannid() throws RecognitionException {
		PredannidContext _localctx = new PredannidContext(_ctx, getState());
		enterRule(_localctx, 44, RULE_predannid);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(412);
			((PredannidContext)_localctx).PREDANNID = match(PREDANNID);
			((PredannidContext)_localctx).text = ((PredannidContext)_localctx).PREDANNID.getText();
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class BoolconstContext extends ParserRuleContext {
		public boolean b;
		public Token Boolconst;
		public TerminalNode Boolconst() { return getToken(FlatzincParser.Boolconst, 0); }
		public BoolconstContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_boolconst; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).enterBoolconst(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).exitBoolconst(this);
		}
	}

	public final BoolconstContext boolconst() throws RecognitionException {
		BoolconstContext _localctx = new BoolconstContext(_ctx, getState());
		enterRule(_localctx, 46, RULE_boolconst);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(415);
			((BoolconstContext)_localctx).Boolconst = match(Boolconst);
			((BoolconstContext)_localctx).b =  ((BoolconstContext)_localctx).Boolconst.getText().equals("true");
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class FloatconstContext extends ParserRuleContext {
		public float f;
		public Token Floatconst;
		public TerminalNode Floatconst() { return getToken(FlatzincParser.Floatconst, 0); }
		public FloatconstContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_floatconst; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).enterFloatconst(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).exitFloatconst(this);
		}
	}

	public final FloatconstContext floatconst() throws RecognitionException {
		FloatconstContext _localctx = new FloatconstContext(_ctx, getState());
		enterRule(_localctx, 48, RULE_floatconst);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(418);
			((FloatconstContext)_localctx).Floatconst = match(Floatconst);
			((FloatconstContext)_localctx).f =  Float.parseFloat(((FloatconstContext)_localctx).Floatconst.getText());
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class IntconstContext extends ParserRuleContext {
		public int i;
		public Token INT;
		public TerminalNode INT() { return getToken(FlatzincParser.INT, 0); }
		public IntconstContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_intconst; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).enterIntconst(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).exitIntconst(this);
		}
	}

	public final IntconstContext intconst() throws RecognitionException {
		IntconstContext _localctx = new IntconstContext(_ctx, getState());
		enterRule(_localctx, 50, RULE_intconst);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(421);
			((IntconstContext)_localctx).INT = match(INT);
			((IntconstContext)_localctx).i =  Integer.parseInt(((IntconstContext)_localctx).INT.getText());
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class StringconstantContext extends ParserRuleContext {
		public String str;
		public Token STRING;
		public TerminalNode STRING() { return getToken(FlatzincParser.STRING, 0); }
		public StringconstantContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_stringconstant; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).enterStringconstant(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).exitStringconstant(this);
		}
	}

	public final StringconstantContext stringconstant() throws RecognitionException {
		StringconstantContext _localctx = new StringconstantContext(_ctx, getState());
		enterRule(_localctx, 52, RULE_stringconstant);
		try {
			enterOuterAlt(_localctx, 1);
			{
			setState(424);
			((StringconstantContext)_localctx).STRING = match(STRING);
			((StringconstantContext)_localctx).str =  ((StringconstantContext)_localctx).STRING.getText().substring(1,((StringconstantContext)_localctx).STRING.getText().length()-1);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static class VarparidContext extends ParserRuleContext {
		public String text;
		public Token VARPARID;
		public Token PREDANNID;
		public TerminalNode VARPARID() { return getToken(FlatzincParser.VARPARID, 0); }
		public TerminalNode PREDANNID() { return getToken(FlatzincParser.PREDANNID, 0); }
		public VarparidContext(ParserRuleContext parent, int invokingState) {
			super(parent, invokingState);
		}
		@Override public int getRuleIndex() { return RULE_varparid; }
		@Override
		public void enterRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).enterVarparid(this);
		}
		@Override
		public void exitRule(ParseTreeListener listener) {
			if ( listener instanceof FlatzincListener ) ((FlatzincListener)listener).exitVarparid(this);
		}
	}

	public final VarparidContext varparid() throws RecognitionException {
		VarparidContext _localctx = new VarparidContext(_ctx, getState());
		enterRule(_localctx, 54, RULE_varparid);
		try {
			setState(431);
			_errHandler.sync(this);
			switch (_input.LA(1)) {
			case VARPARID:
				enterOuterAlt(_localctx, 1);
				{
				setState(427);
				((VarparidContext)_localctx).VARPARID = match(VARPARID);
				((VarparidContext)_localctx).text = ((VarparidContext)_localctx).VARPARID.getText();
				}
				break;
			case PREDANNID:
				enterOuterAlt(_localctx, 2);
				{
				setState(429);
				((VarparidContext)_localctx).PREDANNID = match(PREDANNID);
				((VarparidContext)_localctx).text = ((VarparidContext)_localctx).PREDANNID.getText();
				}
				break;
			default:
				throw new NoViableAltException(this);
			}
		}
		catch (RecognitionException re) {
			_localctx.exception = re;
			_errHandler.reportError(this, re);
			_errHandler.recover(this, re);
		}
		finally {
			exitRule();
		}
		return _localctx;
	}

	public static final String _serializedATN =
		"\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd\3\"\u01b4\4\2\t\2\4"+
		"\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13\t"+
		"\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31\t\31"+
		"\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\3\2\7\2<\n\2\f\2\16\2?\13\2\3"+
		"\2\7\2B\n\2\f\2\16\2E\13\2\3\2\7\2H\n\2\f\2\16\2K\13\2\3\2\7\2N\n\2\f"+
		"\2\16\2Q\13\2\3\2\3\2\3\3\3\3\3\3\3\3\3\3\3\3\7\3[\n\3\f\3\16\3^\13\3"+
		"\3\3\3\3\3\3\3\4\3\4\3\4\3\4\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\5\3\6\3\6\3"+
		"\6\3\6\3\6\3\6\3\6\3\6\5\6w\n\6\3\6\3\6\3\6\3\7\3\7\3\7\3\7\3\7\3\7\3"+
		"\7\3\7\3\7\7\7\u0085\n\7\f\7\16\7\u0088\13\7\3\7\3\7\3\7\3\7\3\7\3\b\3"+
		"\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\3\b\5\b\u009e\n\b\3"+
		"\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\5\t\u00aa\n\t\3\n\3\n\3\n\3\n\3"+
		"\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\3\n\5\n\u00c0"+
		"\n\n\3\13\3\13\3\13\3\13\3\13\3\13\3\13\5\13\u00c9\n\13\3\f\3\f\3\f\3"+
		"\f\3\f\3\f\3\f\5\f\u00d2\n\f\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\16"+
		"\3\16\3\16\3\16\3\16\3\16\3\16\5\16\u00e4\n\16\3\17\3\17\3\17\3\17\3\17"+
		"\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17\3\17"+
		"\3\17\3\17\3\17\3\17\3\17\5\17\u00fe\n\17\3\20\3\20\3\20\3\20\3\20\3\20"+
		"\3\20\3\20\3\20\3\20\3\20\3\20\3\20\5\20\u010d\n\20\3\20\3\20\3\20\3\21"+
		"\3\21\3\21\3\21\3\21\3\21\3\21\3\21\3\21\3\21\3\21\3\21\3\21\3\21\3\21"+
		"\3\21\3\21\3\21\5\21\u0124\n\21\3\22\3\22\3\22\3\22\3\22\3\22\3\22\3\22"+
		"\3\22\3\22\3\22\3\22\3\22\3\22\3\22\7\22\u0135\n\22\f\22\16\22\u0138\13"+
		"\22\3\22\3\22\5\22\u013c\n\22\3\23\3\23\3\23\3\23\3\23\3\23\5\23\u0144"+
		"\n\23\3\23\3\23\3\23\3\23\3\23\3\23\3\23\3\23\7\23\u014e\n\23\f\23\16"+
		"\23\u0151\13\23\5\23\u0153\n\23\3\23\3\23\5\23\u0157\n\23\3\24\3\24\3"+
		"\24\3\24\3\24\3\24\3\24\3\24\3\24\3\24\3\24\3\24\3\24\7\24\u0166\n\24"+
		"\f\24\16\24\u0169\13\24\5\24\u016b\n\24\3\24\3\24\5\24\u016f\n\24\3\25"+
		"\3\25\3\25\3\25\3\25\3\25\3\25\3\25\7\25\u0179\n\25\f\25\16\25\u017c\13"+
		"\25\5\25\u017e\n\25\3\25\3\25\3\25\3\26\3\26\3\26\3\26\3\26\7\26\u0188"+
		"\n\26\f\26\16\26\u018b\13\26\3\27\3\27\3\27\3\27\3\27\3\27\3\27\3\27\3"+
		"\27\7\27\u0196\n\27\f\27\16\27\u0199\13\27\3\27\3\27\5\27\u019d\n\27\3"+
		"\30\3\30\3\30\3\31\3\31\3\31\3\32\3\32\3\32\3\33\3\33\3\33\3\34\3\34\3"+
		"\34\3\35\3\35\3\35\3\35\5\35\u01b2\n\35\3\35\2\2\36\2\4\6\b\n\f\16\20"+
		"\22\24\26\30\32\34\36 \"$&(*,.\60\62\64\668\2\2\u01c7\2=\3\2\2\2\4T\3"+
		"\2\2\2\6b\3\2\2\2\bf\3\2\2\2\nn\3\2\2\2\f{\3\2\2\2\16\u008e\3\2\2\2\20"+
		"\u00a9\3\2\2\2\22\u00ab\3\2\2\2\24\u00c8\3\2\2\2\26\u00d1\3\2\2\2\30\u00d3"+
		"\3\2\2\2\32\u00e3\3\2\2\2\34\u00fd\3\2\2\2\36\u00ff\3\2\2\2 \u0123\3\2"+
		"\2\2\"\u0125\3\2\2\2$\u0156\3\2\2\2&\u016e\3\2\2\2(\u0170\3\2\2\2*\u0182"+
		"\3\2\2\2,\u018c\3\2\2\2.\u019e\3\2\2\2\60\u01a1\3\2\2\2\62\u01a4\3\2\2"+
		"\2\64\u01a7\3\2\2\2\66\u01aa\3\2\2\28\u01b1\3\2\2\2:<\5\4\3\2;:\3\2\2"+
		"\2<?\3\2\2\2=;\3\2\2\2=>\3\2\2\2>C\3\2\2\2?=\3\2\2\2@B\5\b\5\2A@\3\2\2"+
		"\2BE\3\2\2\2CA\3\2\2\2CD\3\2\2\2DI\3\2\2\2EC\3\2\2\2FH\5\n\6\2GF\3\2\2"+
		"\2HK\3\2\2\2IG\3\2\2\2IJ\3\2\2\2JO\3\2\2\2KI\3\2\2\2LN\5\f\7\2ML\3\2\2"+
		"\2NQ\3\2\2\2OM\3\2\2\2OP\3\2\2\2PR\3\2\2\2QO\3\2\2\2RS\5\16\b\2S\3\3\2"+
		"\2\2TU\7\3\2\2UV\7\35\2\2VW\7\4\2\2W\\\5\6\4\2XY\7\5\2\2Y[\5\6\4\2ZX\3"+
		"\2\2\2[^\3\2\2\2\\Z\3\2\2\2\\]\3\2\2\2]_\3\2\2\2^\\\3\2\2\2_`\7\6\2\2"+
		"`a\7\7\2\2a\5\3\2\2\2bc\5\32\16\2cd\7\b\2\2de\5.\30\2e\7\3\2\2\2fg\5\24"+
		"\13\2gh\7\b\2\2hi\58\35\2ij\7\t\2\2jk\5 \21\2kl\7\7\2\2lm\b\5\1\2m\t\3"+
		"\2\2\2no\5\26\f\2op\7\b\2\2pq\58\35\2qv\5*\26\2rs\7\t\2\2st\5 \21\2tu"+
		"\b\6\1\2uw\3\2\2\2vr\3\2\2\2vw\3\2\2\2wx\3\2\2\2xy\7\7\2\2yz\b\6\1\2z"+
		"\13\3\2\2\2{|\7\n\2\2|}\5.\30\2}~\7\4\2\2~\177\5 \21\2\177\u0086\b\7\1"+
		"\2\u0080\u0081\7\5\2\2\u0081\u0082\5 \21\2\u0082\u0083\b\7\1\2\u0083\u0085"+
		"\3\2\2\2\u0084\u0080\3\2\2\2\u0085\u0088\3\2\2\2\u0086\u0084\3\2\2\2\u0086"+
		"\u0087\3\2\2\2\u0087\u0089\3\2\2\2\u0088\u0086\3\2\2\2\u0089\u008a\7\6"+
		"\2\2\u008a\u008b\5*\26\2\u008b\u008c\b\7\1\2\u008c\u008d\7\7\2\2\u008d"+
		"\r\3\2\2\2\u008e\u008f\7\13\2\2\u008f\u009d\5*\26\2\u0090\u0091\7\f\2"+
		"\2\u0091\u0092\7\7\2\2\u0092\u009e\b\b\1\2\u0093\u0094\7\r\2\2\u0094\u0095"+
		"\5 \21\2\u0095\u0096\7\7\2\2\u0096\u0097\b\b\1\2\u0097\u009e\3\2\2\2\u0098"+
		"\u0099\7\16\2\2\u0099\u009a\5 \21\2\u009a\u009b\7\7\2\2\u009b\u009c\b"+
		"\b\1\2\u009c\u009e\3\2\2\2\u009d\u0090\3\2\2\2\u009d\u0093\3\2\2\2\u009d"+
		"\u0098\3\2\2\2\u009e\17\3\2\2\2\u009f\u00a0\7\17\2\2\u00a0\u00aa\b\t\1"+
		"\2\u00a1\u00a2\7\20\2\2\u00a2\u00aa\b\t\1\2\u00a3\u00a4\7\21\2\2\u00a4"+
		"\u00aa\b\t\1\2\u00a5\u00a6\7\22\2\2\u00a6\u00a7\7\23\2\2\u00a7\u00a8\7"+
		"\21\2\2\u00a8\u00aa\b\t\1\2\u00a9\u009f\3\2\2\2\u00a9\u00a1\3\2\2\2\u00a9"+
		"\u00a3\3\2\2\2\u00a9\u00a5\3\2\2\2\u00aa\21\3\2\2\2\u00ab\u00bf\7\24\2"+
		"\2\u00ac\u00ad\7\17\2\2\u00ad\u00c0\b\n\1\2\u00ae\u00af\7\20\2\2\u00af"+
		"\u00c0\b\n\1\2\u00b0\u00b1\5\62\32\2\u00b1\u00b2\7\25\2\2\u00b2\u00b3"+
		"\5\62\32\2\u00b3\u00b4\b\n\1\2\u00b4\u00c0\3\2\2\2\u00b5\u00b6\7\21\2"+
		"\2\u00b6\u00c0\b\n\1\2\u00b7\u00b8\5&\24\2\u00b8\u00b9\b\n\1\2\u00b9\u00c0"+
		"\3\2\2\2\u00ba\u00bb\7\22\2\2\u00bb\u00bc\7\23\2\2\u00bc\u00bd\5&\24\2"+
		"\u00bd\u00be\b\n\1\2\u00be\u00c0\3\2\2\2\u00bf\u00ac\3\2\2\2\u00bf\u00ae"+
		"\3\2\2\2\u00bf\u00b0\3\2\2\2\u00bf\u00b5\3\2\2\2\u00bf\u00b7\3\2\2\2\u00bf"+
		"\u00ba\3\2\2\2\u00c0\23\3\2\2\2\u00c1\u00c2\5\20\t\2\u00c2\u00c3\b\13"+
		"\1\2\u00c3\u00c9\3\2\2\2\u00c4\u00c5\5\30\r\2\u00c5\u00c6\5\20\t\2\u00c6"+
		"\u00c7\b\13\1\2\u00c7\u00c9\3\2\2\2\u00c8\u00c1\3\2\2\2\u00c8\u00c4\3"+
		"\2\2\2\u00c9\25\3\2\2\2\u00ca\u00cb\5\22\n\2\u00cb\u00cc\b\f\1\2\u00cc"+
		"\u00d2\3\2\2\2\u00cd\u00ce\5\30\r\2\u00ce\u00cf\5\22\n\2\u00cf\u00d0\b"+
		"\f\1\2\u00d0\u00d2\3\2\2\2\u00d1\u00ca\3\2\2\2\u00d1\u00cd\3\2\2\2\u00d2"+
		"\27\3\2\2\2\u00d3\u00d4\7\26\2\2\u00d4\u00d5\7\27\2\2\u00d5\u00d6\5\64"+
		"\33\2\u00d6\u00d7\7\25\2\2\u00d7\u00d8\5\64\33\2\u00d8\u00d9\b\r\1\2\u00d9"+
		"\u00da\7\30\2\2\u00da\u00db\7\23\2\2\u00db\31\3\2\2\2\u00dc\u00dd\5\34"+
		"\17\2\u00dd\u00de\b\16\1\2\u00de\u00e4\3\2\2\2\u00df\u00e0\5\36\20\2\u00e0"+
		"\u00e1\5\34\17\2\u00e1\u00e2\b\16\1\2\u00e2\u00e4\3\2\2\2\u00e3\u00dc"+
		"\3\2\2\2\u00e3\u00df\3\2\2\2\u00e4\33\3\2\2\2\u00e5\u00e6\5\22\n\2\u00e6"+
		"\u00e7\b\17\1\2\u00e7\u00fe\3\2\2\2\u00e8\u00e9\5\20\t\2\u00e9\u00ea\b"+
		"\17\1\2\u00ea\u00fe\3\2\2\2\u00eb\u00ec\5\62\32\2\u00ec\u00ed\7\25\2\2"+
		"\u00ed\u00ee\5\62\32\2\u00ee\u00ef\b\17\1\2\u00ef\u00fe\3\2\2\2\u00f0"+
		"\u00f1\5&\24\2\u00f1\u00f2\b\17\1\2\u00f2\u00fe\3\2\2\2\u00f3\u00f4\7"+
		"\22\2\2\u00f4\u00f5\7\23\2\2\u00f5\u00f6\5&\24\2\u00f6\u00f7\b\17\1\2"+
		"\u00f7\u00fe\3\2\2\2\u00f8\u00f9\7\24\2\2\u00f9\u00fa\7\22\2\2\u00fa\u00fb"+
		"\7\23\2\2\u00fb\u00fc\7\21\2\2\u00fc\u00fe\b\17\1\2\u00fd\u00e5\3\2\2"+
		"\2\u00fd\u00e8\3\2\2\2\u00fd\u00eb\3\2\2\2\u00fd\u00f0\3\2\2\2\u00fd\u00f3"+
		"\3\2\2\2\u00fd\u00f8\3\2\2\2\u00fe\35\3\2\2\2\u00ff\u0100\7\26\2\2\u0100"+
		"\u010c\7\27\2\2\u0101\u0102\5\64\33\2\u0102\u0103\7\25\2\2\u0103\u0104"+
		"\5\64\33\2\u0104\u0105\b\20\1\2\u0105\u010d\3\2\2\2\u0106\u0107\7\21\2"+
		"\2\u0107\u010d\b\20\1\2\u0108\u0109\7\21\2\2\u0109\u010a\7\5\2\2\u010a"+
		"\u010b\7\21\2\2\u010b\u010d\b\20\1\2\u010c\u0101\3\2\2\2\u010c\u0106\3"+
		"\2\2\2\u010c\u0108\3\2\2\2\u010d\u010e\3\2\2\2\u010e\u010f\7\30\2\2\u010f"+
		"\u0110\7\23\2\2\u0110\37\3\2\2\2\u0111\u0112\5\60\31\2\u0112\u0113\b\21"+
		"\1\2\u0113\u0124\3\2\2\2\u0114\u0115\5\62\32\2\u0115\u0116\b\21\1\2\u0116"+
		"\u0124\3\2\2\2\u0117\u0118\5$\23\2\u0118\u0119\b\21\1\2\u0119\u0124\3"+
		"\2\2\2\u011a\u011b\5\"\22\2\u011b\u011c\b\21\1\2\u011c\u0124\3\2\2\2\u011d"+
		"\u011e\5(\25\2\u011e\u011f\b\21\1\2\u011f\u0124\3\2\2\2\u0120\u0121\5"+
		"\66\34\2\u0121\u0122\b\21\1\2\u0122\u0124\3\2\2\2\u0123\u0111\3\2\2\2"+
		"\u0123\u0114\3\2\2\2\u0123\u0117\3\2\2\2\u0123\u011a\3\2\2\2\u0123\u011d"+
		"\3\2\2\2\u0123\u0120\3\2\2\2\u0124!\3\2\2\2\u0125\u013b\58\35\2\u0126"+
		"\u013c\b\22\1\2\u0127\u0128\7\27\2\2\u0128\u0129\5\64\33\2\u0129\u012a"+
		"\7\30\2\2\u012a\u012b\b\22\1\2\u012b\u013c\3\2\2\2\u012c\u012d\b\22\1"+
		"\2\u012d\u012e\7\4\2\2\u012e\u012f\5 \21\2\u012f\u0136\b\22\1\2\u0130"+
		"\u0131\7\5\2\2\u0131\u0132\5 \21\2\u0132\u0133\b\22\1\2\u0133\u0135\3"+
		"\2\2\2\u0134\u0130\3\2\2\2\u0135\u0138\3\2\2\2\u0136\u0134\3\2\2\2\u0136"+
		"\u0137\3\2\2\2\u0137\u0139\3\2\2\2\u0138\u0136\3\2\2\2\u0139\u013a\7\6"+
		"\2\2\u013a\u013c\3\2\2\2\u013b\u0126\3\2\2\2\u013b\u0127\3\2\2\2\u013b"+
		"\u012c\3\2\2\2\u013c#\3\2\2\2\u013d\u0143\5\64\33\2\u013e\u0144\b\23\1"+
		"\2\u013f\u0140\7\25\2\2\u0140\u0141\5\64\33\2\u0141\u0142\b\23\1\2\u0142"+
		"\u0144\3\2\2\2\u0143\u013e\3\2\2\2\u0143\u013f\3\2\2\2\u0144\u0157\3\2"+
		"\2\2\u0145\u0146\7\31\2\2\u0146\u0152\b\23\1\2\u0147\u0148\5\64\33\2\u0148"+
		"\u014f\b\23\1\2\u0149\u014a\7\5\2\2\u014a\u014b\5\64\33\2\u014b\u014c"+
		"\b\23\1\2\u014c\u014e\3\2\2\2\u014d\u0149\3\2\2\2\u014e\u0151\3\2\2\2"+
		"\u014f\u014d\3\2\2\2\u014f\u0150\3\2\2\2\u0150\u0153\3\2\2\2\u0151\u014f"+
		"\3\2\2\2\u0152\u0147\3\2\2\2\u0152\u0153\3\2\2\2\u0153\u0154\3\2\2\2\u0154"+
		"\u0155\7\32\2\2\u0155\u0157\b\23\1\2\u0156\u013d\3\2\2\2\u0156\u0145\3"+
		"\2\2\2\u0157%\3\2\2\2\u0158\u0159\5\64\33\2\u0159\u015a\7\25\2\2\u015a"+
		"\u015b\5\64\33\2\u015b\u015c\b\24\1\2\u015c\u016f\3\2\2\2\u015d\u015e"+
		"\7\31\2\2\u015e\u016a\b\24\1\2\u015f\u0160\5\64\33\2\u0160\u0167\b\24"+
		"\1\2\u0161\u0162\7\5\2\2\u0162\u0163\5\64\33\2\u0163\u0164\b\24\1\2\u0164"+
		"\u0166\3\2\2\2\u0165\u0161\3\2\2\2\u0166\u0169\3\2\2\2\u0167\u0165\3\2"+
		"\2\2\u0167\u0168\3\2\2\2\u0168\u016b\3\2\2\2\u0169\u0167\3\2\2\2\u016a"+
		"\u015f\3\2\2\2\u016a\u016b\3\2\2\2\u016b\u016c\3\2\2\2\u016c\u016d\7\32"+
		"\2\2\u016d\u016f\b\24\1\2\u016e\u0158\3\2\2\2\u016e\u015d\3\2\2\2\u016f"+
		"\'\3\2\2\2\u0170\u0171\7\27\2\2\u0171\u017d\b\25\1\2\u0172\u0173\5 \21"+
		"\2\u0173\u017a\b\25\1\2\u0174\u0175\7\5\2\2\u0175\u0176\5 \21\2\u0176"+
		"\u0177\b\25\1\2\u0177\u0179\3\2\2\2\u0178\u0174\3\2\2\2\u0179\u017c\3"+
		"\2\2\2\u017a\u0178\3\2\2\2\u017a\u017b\3\2\2\2\u017b\u017e\3\2\2\2\u017c"+
		"\u017a\3\2\2\2\u017d\u0172\3\2\2\2\u017d\u017e\3\2\2\2\u017e\u017f\3\2"+
		"\2\2\u017f\u0180\7\30\2\2\u0180\u0181\b\25\1\2\u0181)\3\2\2\2\u0182\u0189"+
		"\b\26\1\2\u0183\u0184\7\33\2\2\u0184\u0185\5,\27\2\u0185\u0186\b\26\1"+
		"\2\u0186\u0188\3\2\2\2\u0187\u0183\3\2\2\2\u0188\u018b\3\2\2\2\u0189\u0187"+
		"\3\2\2\2\u0189\u018a\3\2\2\2\u018a+\3\2\2\2\u018b\u0189\3\2\2\2\u018c"+
		"\u018d\5.\30\2\u018d\u019c\b\27\1\2\u018e\u018f\7\4\2\2\u018f\u0190\5"+
		" \21\2\u0190\u0197\b\27\1\2\u0191\u0192\7\5\2\2\u0192\u0193\5 \21\2\u0193"+
		"\u0194\b\27\1\2\u0194\u0196\3\2\2\2\u0195\u0191\3\2\2\2\u0196\u0199\3"+
		"\2\2\2\u0197\u0195\3\2\2\2\u0197\u0198\3\2\2\2\u0198\u019a\3\2\2\2\u0199"+
		"\u0197\3\2\2\2\u019a\u019b\7\6\2\2\u019b\u019d\3\2\2\2\u019c\u018e\3\2"+
		"\2\2\u019c\u019d\3\2\2\2\u019d-\3\2\2\2\u019e\u019f\7\35\2\2\u019f\u01a0"+
		"\b\30\1\2\u01a0/\3\2\2\2\u01a1\u01a2\7\34\2\2\u01a2\u01a3\b\31\1\2\u01a3"+
		"\61\3\2\2\2\u01a4\u01a5\7\37\2\2\u01a5\u01a6\b\32\1\2\u01a6\63\3\2\2\2"+
		"\u01a7\u01a8\7 \2\2\u01a8\u01a9\b\33\1\2\u01a9\65\3\2\2\2\u01aa\u01ab"+
		"\7!\2\2\u01ab\u01ac\b\34\1\2\u01ac\67\3\2\2\2\u01ad\u01ae\7\36\2\2\u01ae"+
		"\u01b2\b\35\1\2\u01af\u01b0\7\35\2\2\u01b0\u01b2\b\35\1\2\u01b1\u01ad"+
		"\3\2\2\2\u01b1\u01af\3\2\2\2\u01b29\3\2\2\2!=CIO\\v\u0086\u009d\u00a9"+
		"\u00bf\u00c8\u00d1\u00e3\u00fd\u010c\u0123\u0136\u013b\u0143\u014f\u0152"+
		"\u0156\u0167\u016a\u016e\u017a\u017d\u0189\u0197\u019c\u01b1";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}