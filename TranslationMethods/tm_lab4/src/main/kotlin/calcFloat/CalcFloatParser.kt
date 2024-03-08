package calcFloat

import calcFloat.CalcFloatToken.*

class CalcFloatParser(private val input: String) {
    private val lexer = CalcFloatLexer(input)

    private fun check(token: CalcFloatToken): String {
        if (lexer.getToken() != token) {
            throw CalcFloatParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }
        if (token == END) {
            return ""
        }
        val result = lexer.getTokenValue()
        lexer.nextToken()
        return result
    }

    fun parse(): Run {
        lexer.nextToken()
        val result = run()
        if (lexer.getToken() != END) {
            throw CalcFloatParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }
        return result
    }

    class Run {
        var v: Float = 0F
    }

    private fun run(): Run {
        val result = Run()
        when (lexer.getToken()) {
            LPAREN, NUMBER, MINUS -> {
                val expr = expr()
                result.v = expr.v
                val END = check(CalcFloatToken.END)
            }

            else -> throw CalcFloatParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
    class Expr {
        var v: Float = 0F
    }

    private fun expr(): Expr {
        val result = Expr()
        when (lexer.getToken()) {
            LPAREN, NUMBER, MINUS -> {
                val term = term()
                val expr_ = expr_(term.v)
                result.v = expr_.v
            }

            else -> throw CalcFloatParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
    class Expr_ {
        var v: Float = 0F
    }

    private fun expr_(left: Float): Expr_ {
        val result = Expr_()
        when (lexer.getToken()) {
            PLUS -> {
                val PLUS = check(CalcFloatToken.PLUS)
                val term = term()
                val next = left + term.v
                val expr_ = expr_(next)
                result.v = expr_.v
            }

            MINUS -> {
                val MINUS = check(CalcFloatToken.MINUS)
                val term = term()
                val next = left - term.v
                val expr_ = expr_(next)
                result.v = expr_.v
            }

            END, RPAREN -> {
                result.v = left
            }

            else -> throw CalcFloatParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
    class Term {
        var v: Float = 0F
    }

    private fun term(): Term {
        val result = Term()
        when (lexer.getToken()) {
            LPAREN, NUMBER, MINUS -> {
                val factor = factor()
                val term_ = term_(factor.v)
                result.v = term_.v
            }

            else -> throw CalcFloatParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
    class Term_ {
        var v: Float = 0F
    }

    private fun term_(left: Float): Term_ {
        val result = Term_()
        when (lexer.getToken()) {
            TIMES -> {
                val TIMES = check(CalcFloatToken.TIMES)
                val factor = factor()
                val term_ = term_(left * factor.v)
                result.v = term_.v
            }

            DIVIDE -> {
                val DIVIDE = check(CalcFloatToken.DIVIDE)
                val factor = factor()
                val term_ = term_(left / factor.v)
                result.v = term_.v
            }

            END, RPAREN, PLUS, MINUS -> {
                result.v = left
            }

            else -> throw CalcFloatParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
    class Factor {
        var v: Float = 0F
    }

    private fun factor(): Factor {
        val result = Factor()
        when (lexer.getToken()) {
            MINUS -> {
                val MINUS = check(CalcFloatToken.MINUS)
                val factor = factor()
                result.v = -factor.v
            }

            LPAREN, NUMBER -> {
                val eval = eval()
                result.v = eval.v
            }

            else -> throw CalcFloatParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
    class Eval {
        var v: Float = 0F
    }

    private fun eval(): Eval {
        val result = Eval()
        when (lexer.getToken()) {
            LPAREN -> {
                val LPAREN = check(CalcFloatToken.LPAREN)
                val expr = expr()
                val RPAREN = check(CalcFloatToken.RPAREN)
                result.v = expr.v
            }

            NUMBER -> {
                val NUMBER = check(CalcFloatToken.NUMBER)
                result.v = NUMBER.toFloat()
            }

            else -> throw CalcFloatParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
}