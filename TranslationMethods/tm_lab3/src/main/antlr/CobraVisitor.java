// Generated from Cobra.g4 by ANTLR 4.13.1
import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced
 * by {@link CobraParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for
 * operations with no return type.
 */
public interface CobraVisitor<T> extends ParseTreeVisitor<T> {
	/**
	 * Visit a parse tree produced by {@link CobraParser#script}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitScript(CobraParser.ScriptContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobraParser#fn}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFn(CobraParser.FnContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobraParser#runner}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitRunner(CobraParser.RunnerContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobraParser#text}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitText(CobraParser.TextContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobraParser#line}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLine(CobraParser.LineContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobraParser#map}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitMap(CobraParser.MapContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobraParser#reduce}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitReduce(CobraParser.ReduceContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobraParser#lambda}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLambda(CobraParser.LambdaContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobraParser#list}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitList(CobraParser.ListContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobraParser#and}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAnd(CobraParser.AndContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobraParser#or}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitOr(CobraParser.OrContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobraParser#cmpOut}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitCmpOut(CobraParser.CmpOutContext ctx);
	/**
	 * Visit a parse tree produced by the {@code brackets}
	 * labeled alternative in {@link CobraParser#cmpInn}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitBrackets(CobraParser.BracketsContext ctx);
	/**
	 * Visit a parse tree produced by the {@code eq}
	 * labeled alternative in {@link CobraParser#cmpInn}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEq(CobraParser.EqContext ctx);
	/**
	 * Visit a parse tree produced by the {@code ne}
	 * labeled alternative in {@link CobraParser#cmpInn}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitNe(CobraParser.NeContext ctx);
	/**
	 * Visit a parse tree produced by the {@code gt}
	 * labeled alternative in {@link CobraParser#cmpInn}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGt(CobraParser.GtContext ctx);
	/**
	 * Visit a parse tree produced by the {@code gte}
	 * labeled alternative in {@link CobraParser#cmpInn}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitGte(CobraParser.GteContext ctx);
	/**
	 * Visit a parse tree produced by the {@code lt}
	 * labeled alternative in {@link CobraParser#cmpInn}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLt(CobraParser.LtContext ctx);
	/**
	 * Visit a parse tree produced by the {@code lte}
	 * labeled alternative in {@link CobraParser#cmpInn}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitLte(CobraParser.LteContext ctx);
	/**
	 * Visit a parse tree produced by the {@code ifElse}
	 * labeled alternative in {@link CobraParser#blocks}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIfElse(CobraParser.IfElseContext ctx);
	/**
	 * Visit a parse tree produced by the {@code whileCond}
	 * labeled alternative in {@link CobraParser#blocks}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitWhileCond(CobraParser.WhileCondContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobraParser#alloc}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitAlloc(CobraParser.AllocContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobraParser#fnAlloc}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFnAlloc(CobraParser.FnAllocContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobraParser#io}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitIo(CobraParser.IoContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobraParser#return}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitReturn(CobraParser.ReturnContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobraParser#expr}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitExpr(CobraParser.ExprContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobraParser#term}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitTerm(CobraParser.TermContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobraParser#factor}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitFactor(CobraParser.FactorContext ctx);
	/**
	 * Visit a parse tree produced by {@link CobraParser#eval}.
	 * @param ctx the parse tree
	 * @return the visitor result
	 */
	T visitEval(CobraParser.EvalContext ctx);
}