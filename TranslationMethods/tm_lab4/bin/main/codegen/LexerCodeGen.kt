package codegen

import grammar.Grammar
import grammar.RRImpl
import grammar.TRImpl

class LexerCodeGen(private val grammar: Grammar, override val grammarName: String, override val path: String) :
    BaseCodeGen {
    override val fileName: String
        get() = grammarName + "Lexer"

    override fun generateInner() = buildString {
        val indent = 1
        appendLine(getImport())

        appendLine("class $fileName(private val input: String) {")

        append(getSize(indent))
        appendLine()
        append(getFields(indent))
        appendLine()
        append(getInit(indent))
        appendLine()

        append("fun getToken(): ${grammarName}Token? = token", indent)
        append("fun getTokenValue(): String = match.group()", indent)
        append("fun getPos(): Int = position", indent)
        appendLine()

        append(getSkip(indent))
        appendLine()
        append(getNextToken(indent))

        appendLine("}")
    }

    private fun getNextToken(indent: Int) = buildString {
        append("fun nextToken(): ${grammarName}Token {", indent)
        append("skip()", indent + 1)
        appendLine()
        append("if (position == size) {", indent + 1)
        append("token = ${grammarName}Token.END", indent + 2)
        append("return ${grammarName}Token.END", indent + 2)
        append("}", indent + 1)
        appendLine()
        append("${grammarName}Token.values().forEach {", indent + 1)
        append("match.usePattern(map[it])", indent + 2)
        append("match.region(position, size)", indent + 2)
        append("if (match.lookingAt()) {", indent + 2)
        append("position += match.end() - match.start()", indent + 3)
        append("token = it", indent + 3)
        append("return it", indent + 3)
        append("}", indent + 2)
        append("}", indent + 1)
        appendLine()
        append(
            "throw ${grammarName}LexerException(\"Illegal sequence: \${input.substring(position)}\", position)",
            2
        )
        append("}", indent)
    }

    private fun getSkip(indent: Int) = buildString {
        append("private fun skip() {", indent)
        append("match.usePattern(skip)", indent + 1)
        append("match.region(position, size)", indent + 1)
        append("if (match.lookingAt()) {", indent + 1)
        append("position = match.end()", indent + 2)
        append("}", indent + 1)
        append("}", indent)
    }

    private fun getInit(indent: Int) = buildString {
        append("init {", indent)

        grammar.tRs.forEach {
            if (it is RRImpl) {
                append("map[${grammarName}Token.${it.parent}] = Pattern.compile(${it.child})", indent + 1)
                return@forEach
            }

            if (it is TRImpl) {
                val reg: String = buildString {
                    it.child.substring(1, it.child.length - 1).toCharArray().forEach { char ->
                        if ("^$".contains(char)) {
                            append("\\\\")
                            append(char)
                        } else {
                            append("[$char]")
                        }
                    }
                }


                append("map[${grammarName}Token.${it.parent}] = Pattern.compile(\"${reg}\")", indent + 1)
            }
        }

        append("}", indent)
    }

    private fun getFields(indent: Int) = buildString {
        append("private var position: Int = 0", indent)
        append("private var token: ${grammarName}Token? = null", indent)
        append(
            "private val map: MutableMap<${grammarName}Token, Pattern> = EnumMap(${grammarName}Token::class.java)",
            indent
        )
        append("private val skip: Pattern = Pattern.compile(\"[ \\n\\r\\t]+\")", indent)
        append("private val match: Matcher = Pattern.compile(\"\").matcher(input)", indent)
    }

    private fun getSize(indent: Int) = buildString {
        append("private val size: Int", indent)
        append("get() {", indent + 1)
        append("return input.length", indent + 2)
        append("}", indent + 1)
    }

    private fun getImport() = buildString {
        appendLine("import java.util.*")
        appendLine("import java.util.regex.*")
    }
}
