// Generated from /home/janho/workspace/oscar-dev/oscar-fzn/src/main/java/oscar/flatzinc/parser/Flatzinc.g by ANTLR 4.5.1

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
package oscar.flatzinc.parser;
import oscar.flatzinc.parser.intermediatemodel.*;
import oscar.flatzinc.model.Annotation;
import oscar.flatzinc.model.Domain;
import oscar.flatzinc.model.DomainSet;
import oscar.flatzinc.model.DomainRange;
import oscar.flatzinc.ParsingException;
import java.util.Set;
import java.util.HashSet;


import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenStream;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.misc.*;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class FlatzincLexer extends Lexer {
	static { RuntimeMetaData.checkVersion("4.5.1", RuntimeMetaData.VERSION); }

	protected static final DFA[] _decisionToDFA;
	protected static final PredictionContextCache _sharedContextCache =
		new PredictionContextCache();
	public static final int
		T__0=1, T__1=2, T__2=3, T__3=4, T__4=5, T__5=6, T__6=7, T__7=8, T__8=9, 
		T__9=10, T__10=11, T__11=12, T__12=13, T__13=14, T__14=15, T__15=16, T__16=17, 
		T__17=18, T__18=19, T__19=20, T__20=21, T__21=22, T__22=23, T__23=24, 
		T__24=25, Boolconst=26, PREDANNID=27, VARPARID=28, Floatconst=29, INT=30, 
		STRING=31, WS=32;
	public static String[] modeNames = {
		"DEFAULT_MODE"
	};

	public static final String[] ruleNames = {
		"T__0", "T__1", "T__2", "T__3", "T__4", "T__5", "T__6", "T__7", "T__8", 
		"T__9", "T__10", "T__11", "T__12", "T__13", "T__14", "T__15", "T__16", 
		"T__17", "T__18", "T__19", "T__20", "T__21", "T__22", "T__23", "T__24", 
		"Boolconst", "PREDANNID", "VARPARID", "Floatconst", "INT", "NUM", "STRING", 
		"WS"
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


	public FlatzincLexer(CharStream input) {
		super(input);
		_interp = new LexerATNSimulator(this,_ATN,_decisionToDFA,_sharedContextCache);
	}

	@Override
	public String getGrammarFileName() { return "Flatzinc.g"; }

	@Override
	public String[] getRuleNames() { return ruleNames; }

	@Override
	public String getSerializedATN() { return _serializedATN; }

	@Override
	public String[] getModeNames() { return modeNames; }

	@Override
	public ATN getATN() { return _ATN; }

	@Override
	public void action(RuleContext _localctx, int ruleIndex, int actionIndex) {
		switch (ruleIndex) {
		case 32:
			WS_action((RuleContext)_localctx, actionIndex);
			break;
		}
	}
	private void WS_action(RuleContext _localctx, int actionIndex) {
		switch (actionIndex) {
		case 0:
			skip();
			break;
		}
	}

	public static final String _serializedATN =
		"\3\u0430\ud6d1\u8206\uad2d\u4417\uaef1\u8d80\uaadd\2\"\u00f3\b\1\4\2\t"+
		"\2\4\3\t\3\4\4\t\4\4\5\t\5\4\6\t\6\4\7\t\7\4\b\t\b\4\t\t\t\4\n\t\n\4\13"+
		"\t\13\4\f\t\f\4\r\t\r\4\16\t\16\4\17\t\17\4\20\t\20\4\21\t\21\4\22\t\22"+
		"\4\23\t\23\4\24\t\24\4\25\t\25\4\26\t\26\4\27\t\27\4\30\t\30\4\31\t\31"+
		"\4\32\t\32\4\33\t\33\4\34\t\34\4\35\t\35\4\36\t\36\4\37\t\37\4 \t \4!"+
		"\t!\4\"\t\"\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\2\3\3\3\3\3\4\3\4\3"+
		"\5\3\5\3\6\3\6\3\7\3\7\3\b\3\b\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t\3\t"+
		"\3\t\3\n\3\n\3\n\3\n\3\n\3\n\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3\13\3"+
		"\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\f\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r\3\r"+
		"\3\16\3\16\3\16\3\16\3\16\3\17\3\17\3\17\3\17\3\17\3\17\3\20\3\20\3\20"+
		"\3\20\3\21\3\21\3\21\3\21\3\22\3\22\3\22\3\23\3\23\3\23\3\23\3\24\3\24"+
		"\3\24\3\25\3\25\3\25\3\25\3\25\3\25\3\26\3\26\3\27\3\27\3\30\3\30\3\31"+
		"\3\31\3\32\3\32\3\32\3\33\3\33\3\33\3\33\3\33\3\33\3\33\3\33\3\33\5\33"+
		"\u00be\n\33\3\34\3\34\7\34\u00c2\n\34\f\34\16\34\u00c5\13\34\3\35\6\35"+
		"\u00c8\n\35\r\35\16\35\u00c9\3\35\3\35\3\36\3\36\3\36\3\36\3\36\5\36\u00d3"+
		"\n\36\3\36\3\36\3\36\3\36\5\36\u00d9\n\36\3\37\5\37\u00dc\n\37\3\37\3"+
		"\37\3 \6 \u00e1\n \r \16 \u00e2\3!\3!\6!\u00e7\n!\r!\16!\u00e8\3!\3!\3"+
		"\"\3\"\3\"\5\"\u00f0\n\"\3\"\3\"\2\2#\3\3\5\4\7\5\t\6\13\7\r\b\17\t\21"+
		"\n\23\13\25\f\27\r\31\16\33\17\35\20\37\21!\22#\23%\24\'\25)\26+\27-\30"+
		"/\31\61\32\63\33\65\34\67\359\36;\37= ?\2A!C\"\3\2\b\4\2C\\c|\6\2\62;"+
		"C\\aac|\4\2GGgg\4\2--//\3\2$$\4\2\13\f\"\"\u00fa\2\3\3\2\2\2\2\5\3\2\2"+
		"\2\2\7\3\2\2\2\2\t\3\2\2\2\2\13\3\2\2\2\2\r\3\2\2\2\2\17\3\2\2\2\2\21"+
		"\3\2\2\2\2\23\3\2\2\2\2\25\3\2\2\2\2\27\3\2\2\2\2\31\3\2\2\2\2\33\3\2"+
		"\2\2\2\35\3\2\2\2\2\37\3\2\2\2\2!\3\2\2\2\2#\3\2\2\2\2%\3\2\2\2\2\'\3"+
		"\2\2\2\2)\3\2\2\2\2+\3\2\2\2\2-\3\2\2\2\2/\3\2\2\2\2\61\3\2\2\2\2\63\3"+
		"\2\2\2\2\65\3\2\2\2\2\67\3\2\2\2\29\3\2\2\2\2;\3\2\2\2\2=\3\2\2\2\2A\3"+
		"\2\2\2\2C\3\2\2\2\3E\3\2\2\2\5O\3\2\2\2\7Q\3\2\2\2\tS\3\2\2\2\13U\3\2"+
		"\2\2\rW\3\2\2\2\17Y\3\2\2\2\21[\3\2\2\2\23f\3\2\2\2\25l\3\2\2\2\27t\3"+
		"\2\2\2\31}\3\2\2\2\33\u0086\3\2\2\2\35\u008b\3\2\2\2\37\u0091\3\2\2\2"+
		"!\u0095\3\2\2\2#\u0099\3\2\2\2%\u009c\3\2\2\2\'\u00a0\3\2\2\2)\u00a3\3"+
		"\2\2\2+\u00a9\3\2\2\2-\u00ab\3\2\2\2/\u00ad\3\2\2\2\61\u00af\3\2\2\2\63"+
		"\u00b1\3\2\2\2\65\u00bd\3\2\2\2\67\u00bf\3\2\2\29\u00c7\3\2\2\2;\u00d8"+
		"\3\2\2\2=\u00db\3\2\2\2?\u00e0\3\2\2\2A\u00e4\3\2\2\2C\u00ef\3\2\2\2E"+
		"F\7r\2\2FG\7t\2\2GH\7g\2\2HI\7f\2\2IJ\7k\2\2JK\7e\2\2KL\7c\2\2LM\7v\2"+
		"\2MN\7g\2\2N\4\3\2\2\2OP\7*\2\2P\6\3\2\2\2QR\7.\2\2R\b\3\2\2\2ST\7+\2"+
		"\2T\n\3\2\2\2UV\7=\2\2V\f\3\2\2\2WX\7<\2\2X\16\3\2\2\2YZ\7?\2\2Z\20\3"+
		"\2\2\2[\\\7e\2\2\\]\7q\2\2]^\7p\2\2^_\7u\2\2_`\7v\2\2`a\7t\2\2ab\7c\2"+
		"\2bc\7k\2\2cd\7p\2\2de\7v\2\2e\22\3\2\2\2fg\7u\2\2gh\7q\2\2hi\7n\2\2i"+
		"j\7x\2\2jk\7g\2\2k\24\3\2\2\2lm\7u\2\2mn\7c\2\2no\7v\2\2op\7k\2\2pq\7"+
		"u\2\2qr\7h\2\2rs\7{\2\2s\26\3\2\2\2tu\7o\2\2uv\7k\2\2vw\7p\2\2wx\7k\2"+
		"\2xy\7o\2\2yz\7k\2\2z{\7|\2\2{|\7g\2\2|\30\3\2\2\2}~\7o\2\2~\177\7c\2"+
		"\2\177\u0080\7z\2\2\u0080\u0081\7k\2\2\u0081\u0082\7o\2\2\u0082\u0083"+
		"\7k\2\2\u0083\u0084\7|\2\2\u0084\u0085\7g\2\2\u0085\32\3\2\2\2\u0086\u0087"+
		"\7d\2\2\u0087\u0088\7q\2\2\u0088\u0089\7q\2\2\u0089\u008a\7n\2\2\u008a"+
		"\34\3\2\2\2\u008b\u008c\7h\2\2\u008c\u008d\7n\2\2\u008d\u008e\7q\2\2\u008e"+
		"\u008f\7c\2\2\u008f\u0090\7v\2\2\u0090\36\3\2\2\2\u0091\u0092\7k\2\2\u0092"+
		"\u0093\7p\2\2\u0093\u0094\7v\2\2\u0094 \3\2\2\2\u0095\u0096\7u\2\2\u0096"+
		"\u0097\7g\2\2\u0097\u0098\7v\2\2\u0098\"\3\2\2\2\u0099\u009a\7q\2\2\u009a"+
		"\u009b\7h\2\2\u009b$\3\2\2\2\u009c\u009d\7x\2\2\u009d\u009e\7c\2\2\u009e"+
		"\u009f\7t\2\2\u009f&\3\2\2\2\u00a0\u00a1\7\60\2\2\u00a1\u00a2\7\60\2\2"+
		"\u00a2(\3\2\2\2\u00a3\u00a4\7c\2\2\u00a4\u00a5\7t\2\2\u00a5\u00a6\7t\2"+
		"\2\u00a6\u00a7\7c\2\2\u00a7\u00a8\7{\2\2\u00a8*\3\2\2\2\u00a9\u00aa\7"+
		"]\2\2\u00aa,\3\2\2\2\u00ab\u00ac\7_\2\2\u00ac.\3\2\2\2\u00ad\u00ae\7}"+
		"\2\2\u00ae\60\3\2\2\2\u00af\u00b0\7\177\2\2\u00b0\62\3\2\2\2\u00b1\u00b2"+
		"\7<\2\2\u00b2\u00b3\7<\2\2\u00b3\64\3\2\2\2\u00b4\u00b5\7v\2\2\u00b5\u00b6"+
		"\7t\2\2\u00b6\u00b7\7w\2\2\u00b7\u00be\7g\2\2\u00b8\u00b9\7h\2\2\u00b9"+
		"\u00ba\7c\2\2\u00ba\u00bb\7n\2\2\u00bb\u00bc\7u\2\2\u00bc\u00be\7g\2\2"+
		"\u00bd\u00b4\3\2\2\2\u00bd\u00b8\3\2\2\2\u00be\66\3\2\2\2\u00bf\u00c3"+
		"\t\2\2\2\u00c0\u00c2\t\3\2\2\u00c1\u00c0\3\2\2\2\u00c2\u00c5\3\2\2\2\u00c3"+
		"\u00c1\3\2\2\2\u00c3\u00c4\3\2\2\2\u00c48\3\2\2\2\u00c5\u00c3\3\2\2\2"+
		"\u00c6\u00c8\7a\2\2\u00c7\u00c6\3\2\2\2\u00c8\u00c9\3\2\2\2\u00c9\u00c7"+
		"\3\2\2\2\u00c9\u00ca\3\2\2\2\u00ca\u00cb\3\2\2\2\u00cb\u00cc\5\67\34\2"+
		"\u00cc:\3\2\2\2\u00cd\u00ce\5=\37\2\u00ce\u00cf\7\60\2\2\u00cf\u00d2\5"+
		"? \2\u00d0\u00d1\t\4\2\2\u00d1\u00d3\5=\37\2\u00d2\u00d0\3\2\2\2\u00d2"+
		"\u00d3\3\2\2\2\u00d3\u00d9\3\2\2\2\u00d4\u00d5\5=\37\2\u00d5\u00d6\t\4"+
		"\2\2\u00d6\u00d7\5=\37\2\u00d7\u00d9\3\2\2\2\u00d8\u00cd\3\2\2\2\u00d8"+
		"\u00d4\3\2\2\2\u00d9<\3\2\2\2\u00da\u00dc\t\5\2\2\u00db\u00da\3\2\2\2"+
		"\u00db\u00dc\3\2\2\2\u00dc\u00dd\3\2\2\2\u00dd\u00de\5? \2\u00de>\3\2"+
		"\2\2\u00df\u00e1\4\62;\2\u00e0\u00df\3\2\2\2\u00e1\u00e2\3\2\2\2\u00e2"+
		"\u00e0\3\2\2\2\u00e2\u00e3\3\2\2\2\u00e3@\3\2\2\2\u00e4\u00e6\7$\2\2\u00e5"+
		"\u00e7\n\6\2\2\u00e6\u00e5\3\2\2\2\u00e7\u00e8\3\2\2\2\u00e8\u00e6\3\2"+
		"\2\2\u00e8\u00e9\3\2\2\2\u00e9\u00ea\3\2\2\2\u00ea\u00eb\7$\2\2\u00eb"+
		"B\3\2\2\2\u00ec\u00f0\t\7\2\2\u00ed\u00ee\7\17\2\2\u00ee\u00f0\7\f\2\2"+
		"\u00ef\u00ec\3\2\2\2\u00ef\u00ed\3\2\2\2\u00f0\u00f1\3\2\2\2\u00f1\u00f2"+
		"\b\"\2\2\u00f2D\3\2\2\2\f\2\u00bd\u00c3\u00c9\u00d2\u00d8\u00db\u00e2"+
		"\u00e8\u00ef\3\3\"\2";
	public static final ATN _ATN =
		new ATNDeserializer().deserialize(_serializedATN.toCharArray());
	static {
		_decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
		for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
			_decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
		}
	}
}