package calcInt

import java.util.*
import java.util.regex.*

class CalcIntLexer(private val input: String) {
    private val size: Int
        get() {
            return input.length
        }

    private var position: Int = 0
    private var token: CalcIntToken? = null
    private val map: MutableMap<CalcIntToken, Pattern> = EnumMap(CalcIntToken::class.java)
    private val skip: Pattern = Pattern.compile("[ \n\r\t]+")
    private val match: Matcher = Pattern.compile("").matcher(input)

    init {
        map[CalcIntToken.PLUS] = Pattern.compile("[+]")
        map[CalcIntToken.MINUS] = Pattern.compile("[-]")
        map[CalcIntToken.TIMES] = Pattern.compile("[*]")
        map[CalcIntToken.DIVIDE] = Pattern.compile("[/]")
        map[CalcIntToken.LPAREN] = Pattern.compile("[(]")
        map[CalcIntToken.RPAREN] = Pattern.compile("[)]")
        map[CalcIntToken.NUMBER] = Pattern.compile("[0-9]+")
        map[CalcIntToken.END] = Pattern.compile("\\$")
    }

    fun getToken(): CalcIntToken? = token
    fun getTokenValue(): String = match.group()
    fun getPos(): Int = position

    private fun skip() {
        match.usePattern(skip)
        match.region(position, size)
        if (match.lookingAt()) {
            position = match.end()
        }
    }

    fun nextToken(): CalcIntToken {
        skip()

        if (position == size) {
            token = CalcIntToken.END
            return CalcIntToken.END
        }

        CalcIntToken.values().forEach {
            match.usePattern(map[it])
            match.region(position, size)
            if (match.lookingAt()) {
                position += match.end() - match.start()
                token = it
                return it
            }
        }

        throw CalcIntLexerException("Illegal sequence: ${input.substring(position)}", position)
    }
}
