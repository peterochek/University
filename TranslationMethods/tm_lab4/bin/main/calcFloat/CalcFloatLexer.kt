package calcFloat

import java.util.*
import java.util.regex.*

class CalcFloatLexer(private val input: String) {
    private val size: Int
        get() {
            return input.length
        }

    private var position: Int = 0
    private var token: CalcFloatToken? = null
    private val map: MutableMap<CalcFloatToken, Pattern> = EnumMap(CalcFloatToken::class.java)
    private val skip: Pattern = Pattern.compile("[ \n\r\t]+")
    private val match: Matcher = Pattern.compile("").matcher(input)

    init {
        map[CalcFloatToken.PLUS] = Pattern.compile("[+]")
        map[CalcFloatToken.MINUS] = Pattern.compile("[-]")
        map[CalcFloatToken.TIMES] = Pattern.compile("[*]")
        map[CalcFloatToken.DIVIDE] = Pattern.compile("[/]")
        map[CalcFloatToken.LPAREN] = Pattern.compile("[(]")
        map[CalcFloatToken.RPAREN] = Pattern.compile("[)]")
        map[CalcFloatToken.NUMBER] = Pattern.compile("([0-9]+)(.[0-9]+)?")
        map[CalcFloatToken.END] = Pattern.compile("\\$")
    }

    fun getToken(): CalcFloatToken? = token
    fun getTokenValue(): String = match.group()
    fun getPos(): Int = position

    private fun skip() {
        match.usePattern(skip)
        match.region(position, size)
        if (match.lookingAt()) {
            position = match.end()
        }
    }

    fun nextToken(): CalcFloatToken {
        skip()

        if (position == size) {
            token = CalcFloatToken.END
            return CalcFloatToken.END
        }

        CalcFloatToken.values().forEach {
            match.usePattern(map[it])
            match.region(position, size)
            if (match.lookingAt()) {
                position += match.end() - match.start()
                token = it
                return it
            }
        }

        throw CalcFloatLexerException("Illegal sequence: ${input.substring(position)}", position)
    }
}
