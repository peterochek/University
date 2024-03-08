package codegen


import grammar.Grammar

class TokenCodeGen(private val grammar: Grammar, override val grammarName: String, override val path: String) :
    BaseCodeGen {
    override val fileName: String
        get() = grammarName + "Token"

    override fun generateInner() = buildString {
        appendLine("enum class $fileName {")
        grammar.tRs.forEach {
            append("${it.parent},", 1)
        }
        appendLine("}")
    }
}