package calcDouble

import calcDouble.CalcDoubleToken.*

class CalcDoubleParser(private val input: String) {
    private val lexer = CalcDoubleLexer(input)

    private fun check(token: CalcDoubleToken): String {
        if (lexer.getToken() != token) {
            throw CalcDoubleParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
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
            throw CalcDoubleParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }
        return result
    }

    class Run {
        var v: Double = 0.0
    }

    private fun run(): Run {
        val result = Run()
        when (lexer.getToken()) {
            LPAREN, NUMBER, MINUS -> {
                val expr = expr()
                result.v = expr.v
                val END = check(CalcDoubleToken.END)
            }

            else -> throw CalcDoubleParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
    class Expr {
        var v: Double = 0.0
    }

    private fun expr(): Expr {
        val result = Expr()
        when (lexer.getToken()) {
            LPAREN, NUMBER, MINUS -> {
                val term = term()
                val expr_ = expr_(term.v)
                result.v = expr_.v
            }

            else -> throw CalcDoubleParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
    class Expr_ {
        var v: Double = 0.0
    }

    private fun expr_(left: Double): Expr_ {
        val result = Expr_()
        when (lexer.getToken()) {
            PLUS -> {
                val PLUS = check(CalcDoubleToken.PLUS)
                val term = term()
                val next = left + term.v
                val expr_ = expr_(next)
                result.v = expr_.v
            }

            MINUS -> {
                val MINUS = check(CalcDoubleToken.MINUS)
                val term = term()
                val next = left - term.v
                val expr_ = expr_(next)
                result.v = expr_.v
            }

            END, RPAREN -> {
                result.v = left
            }

            else -> throw CalcDoubleParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
    class Term {
        var v: Double = 0.0
    }

    private fun term(): Term {
        val result = Term()
        when (lexer.getToken()) {
            LPAREN, NUMBER, MINUS -> {
                val factor = factor()
                val term_ = term_(factor.v)
                result.v = term_.v
            }

            else -> throw CalcDoubleParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
    class Term_ {
        var v: Double = 0.0
    }

    private fun term_(left: Double): Term_ {
        val result = Term_()
        when (lexer.getToken()) {
            TIMES -> {
                val TIMES = check(CalcDoubleToken.TIMES)
                val factor = factor()
                val term_ = term_(left * factor.v)
                result.v = term_.v
            }

            DIVIDE -> {
                val DIVIDE = check(CalcDoubleToken.DIVIDE)
                val factor = factor()
                val term_ = term_(left / factor.v)
                result.v = term_.v
            }

            END, RPAREN, PLUS, MINUS -> {
                result.v = left
            }

            else -> throw CalcDoubleParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
    class Factor {
        var v: Double = 0.0
    }

    private fun factor(): Factor {
        val result = Factor()
        when (lexer.getToken()) {
            MINUS -> {
                val MINUS = check(CalcDoubleToken.MINUS)
                val factor = factor()
                result.v = -factor.v
            }

            LPAREN, NUMBER -> {
                val eval = eval()
                result.v = eval.v
            }

            else -> throw CalcDoubleParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
    class Eval {
        var v: Double = 0.0
    }

    private fun eval(): Eval {
        val result = Eval()
        when (lexer.getToken()) {
            LPAREN -> {
                val LPAREN = check(CalcDoubleToken.LPAREN)
                val expr = expr()
                val RPAREN = check(CalcDoubleToken.RPAREN)
                result.v = expr.v
            }

            NUMBER -> {
                val NUMBER = check(CalcDoubleToken.NUMBER)
                result.v = NUMBER.toDouble()
            }

            else -> throw CalcDoubleParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
}