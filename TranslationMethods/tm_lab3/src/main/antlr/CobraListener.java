// Generated from Cobra.g4 by ANTLR 4.13.1
import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by
 * {@link CobraParser}.
 */
public interface CobraListener extends ParseTreeListener {
	/**
	 * Enter a parse tree produced by {@link CobraParser#script}.
	 * @param ctx the parse tree
	 */
	void enterScript(CobraParser.ScriptContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobraParser#script}.
	 * @param ctx the parse tree
	 */
	void exitScript(CobraParser.ScriptContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobraParser#fn}.
	 * @param ctx the parse tree
	 */
	void enterFn(CobraParser.FnContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobraParser#fn}.
	 * @param ctx the parse tree
	 */
	void exitFn(CobraParser.FnContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobraParser#runner}.
	 * @param ctx the parse tree
	 */
	void enterRunner(CobraParser.RunnerContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobraParser#runner}.
	 * @param ctx the parse tree
	 */
	void exitRunner(CobraParser.RunnerContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobraParser#text}.
	 * @param ctx the parse tree
	 */
	void enterText(CobraParser.TextContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobraParser#text}.
	 * @param ctx the parse tree
	 */
	void exitText(CobraParser.TextContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobraParser#line}.
	 * @param ctx the parse tree
	 */
	void enterLine(CobraParser.LineContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobraParser#line}.
	 * @param ctx the parse tree
	 */
	void exitLine(CobraParser.LineContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobraParser#map}.
	 * @param ctx the parse tree
	 */
	void enterMap(CobraParser.MapContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobraParser#map}.
	 * @param ctx the parse tree
	 */
	void exitMap(CobraParser.MapContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobraParser#reduce}.
	 * @param ctx the parse tree
	 */
	void enterReduce(CobraParser.ReduceContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobraParser#reduce}.
	 * @param ctx the parse tree
	 */
	void exitReduce(CobraParser.ReduceContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobraParser#lambda}.
	 * @param ctx the parse tree
	 */
	void enterLambda(CobraParser.LambdaContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobraParser#lambda}.
	 * @param ctx the parse tree
	 */
	void exitLambda(CobraParser.LambdaContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobraParser#list}.
	 * @param ctx the parse tree
	 */
	void enterList(CobraParser.ListContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobraParser#list}.
	 * @param ctx the parse tree
	 */
	void exitList(CobraParser.ListContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobraParser#and}.
	 * @param ctx the parse tree
	 */
	void enterAnd(CobraParser.AndContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobraParser#and}.
	 * @param ctx the parse tree
	 */
	void exitAnd(CobraParser.AndContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobraParser#or}.
	 * @param ctx the parse tree
	 */
	void enterOr(CobraParser.OrContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobraParser#or}.
	 * @param ctx the parse tree
	 */
	void exitOr(CobraParser.OrContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobraParser#cmpOut}.
	 * @param ctx the parse tree
	 */
	void enterCmpOut(CobraParser.CmpOutContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobraParser#cmpOut}.
	 * @param ctx the parse tree
	 */
	void exitCmpOut(CobraParser.CmpOutContext ctx);
	/**
	 * Enter a parse tree produced by the {@code brackets}
	 * labeled alternative in {@link CobraParser#cmpInn}.
	 * @param ctx the parse tree
	 */
	void enterBrackets(CobraParser.BracketsContext ctx);
	/**
	 * Exit a parse tree produced by the {@code brackets}
	 * labeled alternative in {@link CobraParser#cmpInn}.
	 * @param ctx the parse tree
	 */
	void exitBrackets(CobraParser.BracketsContext ctx);
	/**
	 * Enter a parse tree produced by the {@code eq}
	 * labeled alternative in {@link CobraParser#cmpInn}.
	 * @param ctx the parse tree
	 */
	void enterEq(CobraParser.EqContext ctx);
	/**
	 * Exit a parse tree produced by the {@code eq}
	 * labeled alternative in {@link CobraParser#cmpInn}.
	 * @param ctx the parse tree
	 */
	void exitEq(CobraParser.EqContext ctx);
	/**
	 * Enter a parse tree produced by the {@code ne}
	 * labeled alternative in {@link CobraParser#cmpInn}.
	 * @param ctx the parse tree
	 */
	void enterNe(CobraParser.NeContext ctx);
	/**
	 * Exit a parse tree produced by the {@code ne}
	 * labeled alternative in {@link CobraParser#cmpInn}.
	 * @param ctx the parse tree
	 */
	void exitNe(CobraParser.NeContext ctx);
	/**
	 * Enter a parse tree produced by the {@code gt}
	 * labeled alternative in {@link CobraParser#cmpInn}.
	 * @param ctx the parse tree
	 */
	void enterGt(CobraParser.GtContext ctx);
	/**
	 * Exit a parse tree produced by the {@code gt}
	 * labeled alternative in {@link CobraParser#cmpInn}.
	 * @param ctx the parse tree
	 */
	void exitGt(CobraParser.GtContext ctx);
	/**
	 * Enter a parse tree produced by the {@code gte}
	 * labeled alternative in {@link CobraParser#cmpInn}.
	 * @param ctx the parse tree
	 */
	void enterGte(CobraParser.GteContext ctx);
	/**
	 * Exit a parse tree produced by the {@code gte}
	 * labeled alternative in {@link CobraParser#cmpInn}.
	 * @param ctx the parse tree
	 */
	void exitGte(CobraParser.GteContext ctx);
	/**
	 * Enter a parse tree produced by the {@code lt}
	 * labeled alternative in {@link CobraParser#cmpInn}.
	 * @param ctx the parse tree
	 */
	void enterLt(CobraParser.LtContext ctx);
	/**
	 * Exit a parse tree produced by the {@code lt}
	 * labeled alternative in {@link CobraParser#cmpInn}.
	 * @param ctx the parse tree
	 */
	void exitLt(CobraParser.LtContext ctx);
	/**
	 * Enter a parse tree produced by the {@code lte}
	 * labeled alternative in {@link CobraParser#cmpInn}.
	 * @param ctx the parse tree
	 */
	void enterLte(CobraParser.LteContext ctx);
	/**
	 * Exit a parse tree produced by the {@code lte}
	 * labeled alternative in {@link CobraParser#cmpInn}.
	 * @param ctx the parse tree
	 */
	void exitLte(CobraParser.LteContext ctx);
	/**
	 * Enter a parse tree produced by the {@code ifElse}
	 * labeled alternative in {@link CobraParser#blocks}.
	 * @param ctx the parse tree
	 */
	void enterIfElse(CobraParser.IfElseContext ctx);
	/**
	 * Exit a parse tree produced by the {@code ifElse}
	 * labeled alternative in {@link CobraParser#blocks}.
	 * @param ctx the parse tree
	 */
	void exitIfElse(CobraParser.IfElseContext ctx);
	/**
	 * Enter a parse tree produced by the {@code whileCond}
	 * labeled alternative in {@link CobraParser#blocks}.
	 * @param ctx the parse tree
	 */
	void enterWhileCond(CobraParser.WhileCondContext ctx);
	/**
	 * Exit a parse tree produced by the {@code whileCond}
	 * labeled alternative in {@link CobraParser#blocks}.
	 * @param ctx the parse tree
	 */
	void exitWhileCond(CobraParser.WhileCondContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobraParser#alloc}.
	 * @param ctx the parse tree
	 */
	void enterAlloc(CobraParser.AllocContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobraParser#alloc}.
	 * @param ctx the parse tree
	 */
	void exitAlloc(CobraParser.AllocContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobraParser#fnAlloc}.
	 * @param ctx the parse tree
	 */
	void enterFnAlloc(CobraParser.FnAllocContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobraParser#fnAlloc}.
	 * @param ctx the parse tree
	 */
	void exitFnAlloc(CobraParser.FnAllocContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobraParser#io}.
	 * @param ctx the parse tree
	 */
	void enterIo(CobraParser.IoContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobraParser#io}.
	 * @param ctx the parse tree
	 */
	void exitIo(CobraParser.IoContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobraParser#return}.
	 * @param ctx the parse tree
	 */
	void enterReturn(CobraParser.ReturnContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobraParser#return}.
	 * @param ctx the parse tree
	 */
	void exitReturn(CobraParser.ReturnContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobraParser#expr}.
	 * @param ctx the parse tree
	 */
	void enterExpr(CobraParser.ExprContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobraParser#expr}.
	 * @param ctx the parse tree
	 */
	void exitExpr(CobraParser.ExprContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobraParser#term}.
	 * @param ctx the parse tree
	 */
	void enterTerm(CobraParser.TermContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobraParser#term}.
	 * @param ctx the parse tree
	 */
	void exitTerm(CobraParser.TermContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobraParser#factor}.
	 * @param ctx the parse tree
	 */
	void enterFactor(CobraParser.FactorContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobraParser#factor}.
	 * @param ctx the parse tree
	 */
	void exitFactor(CobraParser.FactorContext ctx);
	/**
	 * Enter a parse tree produced by {@link CobraParser#eval}.
	 * @param ctx the parse tree
	 */
	void enterEval(CobraParser.EvalContext ctx);
	/**
	 * Exit a parse tree produced by {@link CobraParser#eval}.
	 * @param ctx the parse tree
	 */
	void exitEval(CobraParser.EvalContext ctx);
}