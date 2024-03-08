package codegen

import codegen.BaseCodeGen.Companion.NL
import java.io.BufferedWriter

class ExceptionCodeGen(private val exceptionName: String, override val grammarName: String, override val path: String) :
    BaseCodeGen {
    override val fileName = "${grammarName}${exceptionName}Exception"

    override fun generateInner() = buildString{
        appendLine("import java.text.ParseException")
        appendLine()

        appendLine("class ${fileName}(override val message: String, errorOffset: Int): ParseException(message, errorOffset)")
    }
}