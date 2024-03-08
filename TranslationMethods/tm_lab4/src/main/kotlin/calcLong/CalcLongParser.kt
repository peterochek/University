package calcLong

import calcLong.CalcLongToken.*

class CalcLongParser(private val input: String) {
    private val lexer = CalcLongLexer(input)

    private fun check(token: CalcLongToken): String {
        if (lexer.getToken() != token) {
            throw CalcLongParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
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
            throw CalcLongParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }
        return result
    }

    class Run {
        var v: Long = 0L
    }

    private fun run(): Run {
        val result = Run()
        when (lexer.getToken()) {
            LPAREN, NUMBER, MINUS -> {
                val expr = expr()
                result.v = expr.v
                val END = check(CalcLongToken.END)
            }

            else -> throw CalcLongParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
    class Expr {
        var v: Long = 0L
    }

    private fun expr(): Expr {
        val result = Expr()
        when (lexer.getToken()) {
            LPAREN, NUMBER, MINUS -> {
                val term = term()
                val expr_ = expr_(term.v)
                result.v = expr_.v
            }

            else -> throw CalcLongParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
    class Expr_ {
        var v: Long = 0L
    }

    private fun expr_(left: Long): Expr_ {
        val result = Expr_()
        when (lexer.getToken()) {
            PLUS -> {
                val PLUS = check(CalcLongToken.PLUS)
                val term = term()
                val next = left + term.v
                val expr_ = expr_(next)
                result.v = expr_.v
            }

            MINUS -> {
                val MINUS = check(CalcLongToken.MINUS)
                val term = term()
                val next = left - term.v
                val expr_ = expr_(next)
                result.v = expr_.v
            }

            END, RPAREN -> {
                result.v = left
            }

            else -> throw CalcLongParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
    class Term {
        var v: Long = 0L
    }

    private fun term(): Term {
        val result = Term()
        when (lexer.getToken()) {
            LPAREN, NUMBER, MINUS -> {
                val factor = factor()
                val term_ = term_(factor.v)
                result.v = term_.v
            }

            else -> throw CalcLongParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
    class Term_ {
        var v: Long = 0L
    }

    private fun term_(left: Long): Term_ {
        val result = Term_()
        when (lexer.getToken()) {
            TIMES -> {
                val TIMES = check(CalcLongToken.TIMES)
                val factor = factor()
                val term_ = term_(left * factor.v)
                result.v = term_.v
            }

            DIVIDE -> {
                val DIVIDE = check(CalcLongToken.DIVIDE)
                val factor = factor()
                val term_ = term_(left / factor.v)
                result.v = term_.v
            }

            END, RPAREN, PLUS, MINUS -> {
                result.v = left
            }

            else -> throw CalcLongParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
    class Factor {
        var v: Long = 0L
    }

    private fun factor(): Factor {
        val result = Factor()
        when (lexer.getToken()) {
            MINUS -> {
                val MINUS = check(CalcLongToken.MINUS)
                val factor = factor()
                result.v = -factor.v
            }

            LPAREN, NUMBER -> {
                val eval = eval()
                result.v = eval.v
            }

            else -> throw CalcLongParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
    class Eval {
        var v: Long = 0L
    }

    private fun eval(): Eval {
        val result = Eval()
        when (lexer.getToken()) {
            LPAREN -> {
                val LPAREN = check(CalcLongToken.LPAREN)
                val expr = expr()
                val RPAREN = check(CalcLongToken.RPAREN)
                result.v = expr.v
            }

            NUMBER -> {
                val NUMBER = check(CalcLongToken.NUMBER)
                result.v = NUMBER.toLong()
            }

            else -> throw CalcLongParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
}