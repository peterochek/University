import org.antlr.v4.runtime.tree.TerminalNode

class CobraTranslator : CobraBaseVisitor<String>() {
    private fun StringBuilder.appendLine(a: String) {
        for (i in 0 until (4 * indent)) append(" ")
        append(a)
        append(System.lineSeparator())
    }

    private fun StringBuilder.appendLine(a: String, indent: Int) {
        for (i in 0 until (4 * indent)) append(" ")
        append(a)
        append(System.lineSeparator())
    }

    private var indent = 0

    private val tmpVarStack: MutableList<TmpVar> = ArrayList()

    private val lambdas: MutableList<String> = ArrayList()

    private fun block(runnable: () -> Unit) {
        indent++
        val c = if (tmpVarStack.isNotEmpty()) tmpVarStack.last().counter else 0
        tmpVarStack.add(TmpVar(c))
        runnable()
        tmpVarStack.removeAt(tmpVarStack.size - 1)
        indent--
    }

    private val stack get() = tmpVarStack.last()


    override fun aggregateResult(aggregate: String?, nextResult: String?) = (aggregate ?: "") + (nextResult ?: "")

    override fun visitScript(ctx: CobraParser.ScriptContext) = buildString {
        val functions = buildString {
            ctx.fn().forEach {
                append(visitFn(it))
                appendLine("")
            }
        }
        val main = buildString {
            ctx.runner()?.let {
                append(visitRunner(it))
            }
        }

        appendLine(Constants.IMPORT)
        for (lambda in lambdas) {
            appendLine(lambda)
        }
        appendLine(functions)
        appendLine(main)
    }

    override fun visitFn(ctx: CobraParser.FnContext) = buildString {
        val name = ctx.ID(0).toString()
        val args = ctx.ID().subList(1, ctx.ID().size).map { it.toString() }
        val joined = args.joinToString { "${Constants.TYPE} $it" }
        appendLine("${Constants.TYPE} $name($joined) {")
        block {
            ctx.text()?.let { append(visitText(it)) }
            append(visitReturn(ctx.return_()))
        }
        appendLine("}")
    }

    override fun visitRunner(ctx: CobraParser.RunnerContext) = buildString {
        appendLine("int main() {")
        block {
            append(visitText(ctx.text()))
            appendLine("return 0;")
        }
        appendLine("}")
    }

    private fun StringBuilder.isGenerated(v: String) {
        if (tmpVarStack.asReversed().all { v !in it.allocated }) append(generateVar(v))
    }

    override fun visitReturn(ctx: CobraParser.ReturnContext) = buildString {
        val txt = visitExpr(ctx.expr())
        appendLine("${Constants.RETURN_KEYWORD} $txt;")
    }

    override fun visitFnAlloc(ctx: CobraParser.FnAllocContext) = buildString {
        val outParam = ctx.ID(0).toString()
        val funName = ctx.ID(1).toString()
        val args = ctx.ID().subList(2, ctx.ID().size).map { it.toString() }
        isGenerated(outParam)
        appendLine("$outParam = $funName(${if (args.isNotEmpty()) args.joinToString() else ""});")
    }

    override fun visitIo(ctx: CobraParser.IoContext) = buildString {
        val ioCmd = ctx.getChild(0).text
        ctx.ID().map { it.toString() }.onEach { isGenerated(it) }
            .forEach { appendLine(IO.ioLine(ioCmd, it, stack, indent)) }
    }

    override fun visitAlloc(ctx: CobraParser.AllocContext) = buildString {
        val varName = ctx.ID().toString()
        if (ctx.expr() != null) {
            isGenerated(varName)
            appendLine("$varName = ${visitExpr(ctx.expr())};")
        } else {
            stack.allocated.add(varName)
            val listExpr = visitList(ctx.list())
            appendLine("${Constants.TYPE} $varName$listExpr;")
        }
    }

    override fun visitExpr(ctx: CobraParser.ExprContext): String {
        if (ctx.childCount == 1) {
            return visitTerm(ctx.term())
        }

        val txt1 = visitExpr(ctx.expr())
        val txt2 = visitTerm(ctx.term())
        val op = ctx.getChild(1).text

        return "$txt1 $op $txt2"
    }

    override fun visitTerm(ctx: CobraParser.TermContext): String {
        if (ctx.childCount == 1) {
            return visitFactor(ctx.factor())
        }

        val txt1 = visitTerm(ctx.term())
        val txt2 = visitFactor(ctx.factor())
        val op = ctx.getChild(1).text

        return "$txt1 $op $txt2"
    }

    override fun visitFactor(ctx: CobraParser.FactorContext): String {
        return if (ctx.childCount == 1) {
            visitEval(ctx.eval())
        } else {
            val afterMinus = visitFactor(ctx.factor())
            return "-($afterMinus)"
        }
    }

    override fun visitEval(ctx: CobraParser.EvalContext): String {
        if (ctx.childCount > 1) {
            return visitExpr(ctx.expr())
        }
        val node = ctx.getChild(0) as TerminalNode
        return node.text
    }

    private fun generateVar(varName: String) = buildString {
        stack.allocated.add(varName)
        appendLine("${Constants.TYPE} $varName;")
    }

    override fun visitIfElse(ctx: CobraParser.IfElseContext) = buildString {
        appendLine("if (${visitOr(ctx.or())}) {")
        block {
            append(visitText(ctx.text(0)))
        }
        appendLine("}")
        if (ctx.text().size > 1) {
            appendLine("else {")
            block {
                append(visitText(ctx.text(1)))
            }
            appendLine("}")
        }
    }

    override fun visitWhileCond(ctx: CobraParser.WhileCondContext) = buildString {
        appendLine("while (${visitOr(ctx.or())}) {")
        block {
            append(visitText(ctx.text()))
        }
        appendLine("}")
    }

    override fun visitOr(ctx: CobraParser.OrContext): String {
        return if (ctx.childCount == 1) visitAnd(ctx.and())
        else "(${visitOr(ctx.or())} || ${visitAnd(ctx.and())})"
    }

    override fun visitAnd(ctx: CobraParser.AndContext): String {
        return if (ctx.childCount == 1) visitCmpOut(ctx.cmpOut())
        else "(${visitAnd(ctx.and())} && ${visitCmpOut(ctx.cmpOut())})"
    }

    override fun visitEq(ctx: CobraParser.EqContext): String {
        return "(${ctx.ID(0)} == ${ctx.ID(1)})"
    }

    override fun visitNe(ctx: CobraParser.NeContext): String {
        return "(${ctx.ID(0)} != ${ctx.ID(1)})"
    }

    override fun visitLt(ctx: CobraParser.LtContext): String {
        return "(${ctx.ID(0)} < ${ctx.ID(1)})"
    }

    override fun visitLte(ctx: CobraParser.LteContext): String {
        return "(${ctx.ID(0)} <= ${ctx.ID(1)})"
    }

    override fun visitGt(ctx: CobraParser.GtContext): String {
        return "(${ctx.ID(0)} > ${ctx.ID(1)})"
    }

    override fun visitGte(ctx: CobraParser.GteContext): String {
        return "(${ctx.ID(0)} >= ${ctx.ID(1)})"
    }

    override fun visitBrackets(ctx: CobraParser.BracketsContext): String {
        return "(${visitOr(ctx.or())})"
    }

    private fun generateLambda(lambdaName: String, ctx: CobraParser.LambdaContext) = buildString {
        val args = ctx.ID().map { it.toString() }.joinToString { "${Constants.TYPE} $it" }
        appendLine("${Constants.TYPE} $lambdaName($args) {", 0)
        appendLine("return ${visitExpr(ctx.expr())};", 1)
        appendLine("}", 0)
    }

    override fun visitMap(ctx: CobraParser.MapContext) = buildString {
        val resultVar = ctx.ID(0).text
        stack.allocated.add(resultVar)
        val lambdaName = stack.createTmpVar()
        lambdas.add(generateLambda(lambdaName, ctx.lambda()))
        val tmpName = stack.createTmpVar()

        if (ctx.ID().size == 2) {
            val argArr = ctx.ID(1).text
            val arraySize = stack.createTmpVar()
            appendLine("int $arraySize = sizeof($argArr) / sizeof($argArr[0]);")
            append(iterArr(resultVar, lambdaName, arraySize, argArr))
        } else {
            val listExpr = visitList(ctx.list())
            val pattern = Regex("\\[(\\d+)]") // This regex pattern captures the number inside square brackets
            val arraySize = pattern.find(listExpr)!!.groupValues[1]
            appendLine("${Constants.TYPE} $tmpName$listExpr;")
            append(iterArr(resultVar, lambdaName, arraySize, tmpName))
        }
    }

    override fun visitReduce(ctx: CobraParser.ReduceContext) = buildString {
        val resultVar = ctx.ID(0).text
        stack.allocated.add(resultVar)
        val lambdaName = stack.createTmpVar()
        lambdas.add(generateLambda(lambdaName, ctx.lambda()))
        val arraySize = stack.createTmpVar()
        val arrName: String

        if (ctx.list() != null) {
            arrName = stack.createTmpVar()
            val listExpr = visitList(ctx.list())
            appendLine("${Constants.TYPE} $arrName$listExpr;")
        } else {
            arrName = ctx.ID(1).text
        }
        appendLine("int $arraySize = sizeof($arrName) / sizeof($arrName[0]);")

        val accum = ctx.NUMBER()
        val iterator = stack.createTmpVar()

        appendLine("${Constants.TYPE} $resultVar = $accum;")
        appendLine("for (int $iterator = 0; $iterator < $arraySize; $iterator++) {")
        block {
            appendLine("$resultVar = $lambdaName($resultVar, $arrName[$iterator]);")
        }
        appendLine("}")
    }

    private fun iterArr(resultVar: String, lambdaName: String, arraySize: String, iterName: String) = buildString {
        val iterator = stack.createTmpVar()

        appendLine("${Constants.TYPE} $resultVar[$arraySize];")
        appendLine("for (int $iterator = 0; $iterator < $arraySize; $iterator++) {")
        block {
            appendLine("$resultVar[$iterator] = $lambdaName($iterName[$iterator]);")
        }
        appendLine("}")
    }

    override fun visitLambda(ctx: CobraParser.LambdaContext) = buildString {
        return visitExpr(ctx.expr())
    }

    override fun visitList(ctx: CobraParser.ListContext) = buildString {
        if (ctx.NUMBER(0) != null) {
            val start = ctx.NUMBER(0).text.toInt()
            val end = ctx.NUMBER(1).text.toInt()
            if (ctx.NUMBER(2) != null) {
                val step = ctx.NUMBER(2).text.toInt()
                append(generateRange(start, end, step))
            } else {
                append(generateRange(start, end))
            }
        } else {
            val arithmList = ctx.expr()
            val elements = arithmList.joinToString(", ") { visitExpr(it) }
            append("[${arithmList.size}] = { $elements }")
        }
    }

    private fun generateRange(start: Int, end: Int, step: Int = 1): String {
        val range = (start..end step step).toList()
        val elements = range.joinToString()
        return "[${range.size}] = { $elements }"
    }
}

class TmpVar(initCounter: Int = 0) {
    val allocated: MutableSet<String> = HashSet()

    var counter = initCounter

    fun createTmpVar(): String {
        return "_cbr${counter++}"
    }
}
