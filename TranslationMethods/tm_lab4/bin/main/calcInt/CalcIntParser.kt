package calcInt

import calcInt.CalcIntToken.*

class CalcIntParser(private val input: String) {
    private val lexer = CalcIntLexer(input)

    private fun check(token: CalcIntToken): String {
        if (lexer.getToken() != token) {
            throw CalcIntParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
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
            throw CalcIntParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }
        return result
    }

    class Run {
        var v: Int = 0
    }

    private fun run(): Run {
        val result = Run()
        when (lexer.getToken()) {
            LPAREN, NUMBER, MINUS -> {
                val expr = expr()
                result.v = expr.v
                val END = check(CalcIntToken.END)
            }

            else -> throw CalcIntParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
    class Expr {
        var v: Int = 0
    }

    private fun expr(): Expr {
        val result = Expr()
        when (lexer.getToken()) {
            LPAREN, NUMBER, MINUS -> {
                val term = term()
                val expr_ = expr_(term.v)
                result.v = expr_.v
            }

            else -> throw CalcIntParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
    class Expr_ {
        var v: Int = 0
    }

    private fun expr_(left: Int): Expr_ {
        val result = Expr_()
        when (lexer.getToken()) {
            PLUS -> {
                val PLUS = check(CalcIntToken.PLUS)
                val term = term()
                val next = left + term.v
                val expr_ = expr_(next)
                result.v = expr_.v
            }

            MINUS -> {
                val MINUS = check(CalcIntToken.MINUS)
                val term = term()
                val next = left - term.v
                val expr_ = expr_(next)
                result.v = expr_.v
            }

            END, RPAREN -> {
                result.v = left
            }

            else -> throw CalcIntParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
    class Term {
        var v: Int = 0
    }

    private fun term(): Term {
        val result = Term()
        when (lexer.getToken()) {
            LPAREN, NUMBER, MINUS -> {
                val factor = factor()
                val term_ = term_(factor.v)
                result.v = term_.v
            }

            else -> throw CalcIntParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
    class Term_ {
        var v: Int = 0
    }

    private fun term_(left: Int): Term_ {
        val result = Term_()
        when (lexer.getToken()) {
            TIMES -> {
                val TIMES = check(CalcIntToken.TIMES)
                val factor = factor()
                val term_ = term_(left * factor.v)
                result.v = term_.v
            }

            DIVIDE -> {
                val DIVIDE = check(CalcIntToken.DIVIDE)
                val factor = factor()
                val term_ = term_(left / factor.v)
                result.v = term_.v
            }

            END, RPAREN, PLUS, MINUS -> {
                result.v = left
            }

            else -> throw CalcIntParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
    class Factor {
        var v: Int = 0
    }

    private fun factor(): Factor {
        val result = Factor()
        when (lexer.getToken()) {
            MINUS -> {
                val MINUS = check(CalcIntToken.MINUS)
                val factor = factor()
                result.v = -factor.v
            }

            LPAREN, NUMBER -> {
                val eval = eval()
                result.v = eval.v
            }

            else -> throw CalcIntParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
    class Eval {
        var v: Int = 0
    }

    private fun eval(): Eval {
        val result = Eval()
        when (lexer.getToken()) {
            LPAREN -> {
                val LPAREN = check(CalcIntToken.LPAREN)
                val expr = expr()
                val RPAREN = check(CalcIntToken.RPAREN)
                result.v = expr.v
            }

            NUMBER -> {
                val NUMBER = check(CalcIntToken.NUMBER)
                result.v = NUMBER.toInt()
            }

            else -> throw CalcIntParserException("Wrong token: ${lexer.getToken()}", lexer.getPos())
        }

        return result
    }
}