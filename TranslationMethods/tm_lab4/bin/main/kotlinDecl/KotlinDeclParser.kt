package kotlinDecl

import kotlinDecl.KotlinDeclToken.*
import guru.nidi.graphviz.model.Factory.*
import util.ID.inc
import util.ID.last
import util.GraphVizStruct

class KotlinDeclParser(private val input: String) {
    private val lexer = KotlinDeclLexer(input)

    private fun check(token: KotlinDeclToken): String {
        if (lexer.getToken() != token) {
            throw KotlinDeclParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }
        if (token == END) {
            return ""
        }
        val result = lexer.getTokenValue()
        lexer.nextToken()
        return result
    }

    fun parse(): Script {
        lexer.nextToken()
        val result = script()
        if (lexer.getToken() != END) {
            throw KotlinDeclParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }
        return result
    }

    class Script {
        var res: GraphVizStruct? = null
    }

    private fun script(): Script {
        val result = Script()
        when (lexer.getToken()) {
            VAL, VAR -> {
                result.res = GraphVizStruct()
                val s = "Script"
                result.res!!.add(s, "DESC")
                val desc = desc(result.res!!, last())
                result.res!!.add(s, "CONT")
                val cont = cont(result.res!!, last())
                val END = check(KotlinDeclToken.END)
            }

            else -> throw KotlinDeclParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
    class Cont {
    }

    private fun cont(res: GraphVizStruct, parent: String): Cont {
        val result = Cont()
        when (lexer.getToken()) {
            VAL, VAR -> {
                res.add(parent, "DESC")
                val desc = desc(res, last())
                res.add(parent, "CONT")
                val cont = cont(res, last())
            }

            END -> {
                res.add(parent, "EPS")
            }

            else -> throw KotlinDeclParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
    class Desc {
    }

    private fun desc(res: GraphVizStruct, parent: String): Desc {
        val result = Desc()
        when (lexer.getToken()) {
            VAL, VAR -> {
                res.add(parent, "DECL")
                val decl = decl(res, last())
                res.add(parent, "VALUE")
                val value = value(res, last())
                val SEMICOLON = check(KotlinDeclToken.SEMICOLON)
            }

            else -> throw KotlinDeclParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
    class Decl {
    }

    private fun decl(res: GraphVizStruct, parent: String): Decl {
        val result = Decl()
        when (lexer.getToken()) {
            VAL, VAR -> {
                val v = v(res, last())
                val NAME = check(KotlinDeclToken.NAME)
                res.add(parent, NAME)
                val COLON = check(KotlinDeclToken.COLON)
                res.add(parent, COLON)
                val TYPE = check(KotlinDeclToken.TYPE)
                res.add(parent, TYPE)
            }

            else -> throw KotlinDeclParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
    class V {
    }

    private fun v(res: GraphVizStruct, parent: String): V {
        val result = V()
        when (lexer.getToken()) {
            VAR -> {
                val VAR = check(KotlinDeclToken.VAR)
                res.add(parent, VAR)
            }

            VAL -> {
                val VAL = check(KotlinDeclToken.VAL)
                res.add(parent, VAL)
            }

            else -> throw KotlinDeclParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
    class Value {
    }

    private fun value(res: GraphVizStruct, parent: String): Value {
        val result = Value()
        when (lexer.getToken()) {
            EQ -> {
                val EQ = check(KotlinDeclToken.EQ)
                res.add(parent, EQ)
                val NUMBER = check(KotlinDeclToken.NUMBER)
                res.add(parent, NUMBER)
            }

            SEMICOLON -> {
                res.add(parent, "EPS")
            }

            else -> throw KotlinDeclParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
}