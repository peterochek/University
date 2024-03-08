package codegen

import grammar.*
import grammar.Grammar.Companion.EPS

class ParserCodeGen(private val grammar: Grammar, override val grammarName: String, override val path: String) :
    BaseCodeGen {
    override val fileName: String
        get() = grammarName + "Parser"

    private val generics = grammar.generics.associate {
        it.genericName to it.type.substring(1, it.type.length - 1) // remove ".."
    }

    private val exception = "throw ${grammarName}ParserException(\"Wrong token: \${lexer.getToken()}\", lexer.getPos())"

    override fun generateInner(): String {
        var tmp = generateTmp()

        for ((genericName, typeName) in generics) {
            tmp = tmp.replace(genericName, typeName)
        }

        return tmp
    }

    private fun generateTmp() = buildString {
        val indent = 1

        appendLine("import ${path}.${grammarName}Token.*")

        grammar.imports.forEach {
            appendLine("import $it")
        }

        appendLine()

        appendLine("class ${grammarName}Parser(private val input: String) {")
        append("private val lexer = ${grammarName}Lexer(input)", indent)
        appendLine()
        append(getCheck(indent))
        appendLine()
        append(getParse(indent))
        appendLine()

        grammar.ntRs.forEach { append(getRules(it, indent)) }
        append("}")
    }

    private fun checkGeneric(arg: Arg): Arg {
        return if (arg.type[0] == '#') {
            Arg(arg.name, generics[arg.type]!!)
        } else {
            Arg(arg.name, arg.type)
        }
    }


    private fun getRules(rule: NTR, indent: Int) = buildString {
        append("class ${rule.parent.fc()} {", indent)
        rule.getFields().map { checkGeneric(it) }
            .map { it.getDefaultRepr() }.forEach { append(it, indent + 1) }
        append("}", indent)
        appendLine()

        val arguments: String = rule.args.map { checkGeneric(it) }.joinToString { it.getRepr() }

        append("private fun ${rule.parent}($arguments): ${rule.parent.fc()} {", indent)
        append("val result = ${rule.parent.fc()}()", indent + 1)
        append("when (lexer.getToken()) {", indent + 1)

        append(getCases(rule, indent + 2))

        append("else -> $exception", indent + 2)
        append("}", indent + 1)
        appendLine()
        append("return result", indent + 1)
        append("}", indent)
    }

    private fun getCases(rule: NTR, indent: Int) = buildString {
        var rightWithEPS: List<RuleToken>? = null

        rule.tokens.forEach {
            val firsts = grammar.fstSet(it)

            if (firsts.contains(EPS)) {
                rightWithEPS = it
                return@forEach
            }

            val firstsStr = firsts.joinToString()

            append("$firstsStr -> {", indent)
            append(getCase(it, indent + 1))
            append("}", indent)
            appendLine()
        }

        rightWithEPS?.let {
            val follows = grammar.flw[rule.parent]?.joinToString() ?: ""
            append("$follows -> {", indent)
            append(getCase(it, indent + 1))
            append("}", indent)
            appendLine()
        }
    }

    private fun getCase(tokens: List<RuleToken>, indent: Int) = buildString {
        tokens.forEach {
            when (it) {
                is Script -> {
                    append(it.script.trim().let { s -> s.substring(1, s.length - 1) }, indent)
                }

                is T -> {
                    append("val ${it.name} = check(${grammarName}Token.${it.name})", indent)
                }

                is NT -> {
                    append("val ${it.name} = ${it.name}(${it.args.joinToString()})", indent)
                }
            }
        }
    }

    private fun getParse(indent: Int) = buildString {
        val startRule: NTR = grammar.ntRs.find { it.parent == grammar.start } ?: throw CustomException()
        val arguments: String = startRule.args.map { checkGeneric(it) }
            .joinToString { it.getRepr() }
        val withoutType = startRule.args.joinToString { it.name }

        append("fun parse(${arguments}): ${startRule.parent.fc()} {", indent)
        append("lexer.nextToken()", indent + 1)
        append("val result = ${startRule.parent}(${withoutType})", indent + 1)
        append("if (lexer.getToken() != END) {", indent + 1)
        append(exception, indent + 2)
        append("}", indent + 1)
        append("return result", indent + 1)
        append("}", indent)
    }

    private fun getCheck(indent: Int) = buildString {
        append("private fun check(token: ${grammarName}Token): String {", indent)
        append("if (lexer.getToken() != token) {", indent + 1)
        append(exception, indent + 2)
        append("}", indent + 1)
        append("if (token == END) {", indent + 1)
        append("return \"\"", indent + 2)
        append("}", indent + 1)
        append("val result = lexer.getTokenValue()", indent + 1)
        append("lexer.nextToken()", indent + 1)
        append("return result", indent + 1)
        append("}", indent)
    }

    private fun String.fc() = this[0].uppercaseChar() + this.substring(1)
}