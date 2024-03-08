package calcLong

import java.util.*
import java.util.regex.*

class CalcLongLexer(private val input: String) {
    private val size: Int
        get() {
            return input.length
        }

    private var position: Int = 0
    private var token: CalcLongToken? = null
    private val map: MutableMap<CalcLongToken, Pattern> = EnumMap(CalcLongToken::class.java)
    private val skip: Pattern = Pattern.compile("[ \n\r\t]+")
    private val match: Matcher = Pattern.compile("").matcher(input)

    init {
        map[CalcLongToken.PLUS] = Pattern.compile("[+]")
        map[CalcLongToken.MINUS] = Pattern.compile("[-]")
        map[CalcLongToken.TIMES] = Pattern.compile("[*]")
        map[CalcLongToken.DIVIDE] = Pattern.compile("[/]")
        map[CalcLongToken.LPAREN] = Pattern.compile("[(]")
        map[CalcLongToken.RPAREN] = Pattern.compile("[)]")
        map[CalcLongToken.NUMBER] = Pattern.compile("[0-9]+")
        map[CalcLongToken.END] = Pattern.compile("\\$")
    }

    fun getToken(): CalcLongToken? = token
    fun getTokenValue(): String = match.group()
    fun getPos(): Int = position

    private fun skip() {
        match.usePattern(skip)
        match.region(position, size)
        if (match.lookingAt()) {
            position = match.end()
        }
    }

    fun nextToken(): CalcLongToken {
        skip()

        if (position == size) {
            token = CalcLongToken.END
            return CalcLongToken.END
        }

        CalcLongToken.values().forEach {
            match.usePattern(map[it])
            match.region(position, size)
            if (match.lookingAt()) {
                position += match.end() - match.start()
                token = it
                return it
            }
        }

        throw CalcLongLexerException("Illegal sequence: ${input.substring(position)}", position)
    }
}
