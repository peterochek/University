package codegen

import GrammarLexer
import GrammarParser
import grammar.Grammar
import org.antlr.v4.runtime.CharStreams
import org.antlr.v4.runtime.CommonTokenStream
import java.io.File
import java.nio.file.Paths

class CodeGen(private val dataPath: String, private val dataOut: String, private val name: String) {
    private val path = Paths.get(dataPath).resolve(name)
    private val text = CharStreams.fromPath(path)

    private val parser =
        GrammarParser(CommonTokenStream(GrammarLexer(text)))
    val grammar: Grammar = parser.start().grammar

    init {
        grammar.buildAll()
    }

    fun codeGen() {
        val grammarName = File(name).nameWithoutExtension

        ExceptionCodeGen("Lexer", grammarName, dataOut).codeGen()
        ExceptionCodeGen("Parser", grammarName, dataOut).codeGen()
        TokenCodeGen(grammar, grammarName, dataOut).codeGen()
        LexerCodeGen(grammar, grammarName, dataOut).codeGen()
        ParserCodeGen(grammar, grammarName, dataOut).codeGen()
    }
}