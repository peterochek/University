package calcDouble

import java.util.*
import java.util.regex.*

class CalcDoubleLexer(private val input: String) {
    private val size: Int
        get() {
            return input.length
        }

    private var position: Int = 0
    private var token: CalcDoubleToken? = null
    private val map: MutableMap<CalcDoubleToken, Pattern> = EnumMap(CalcDoubleToken::class.java)
    private val skip: Pattern = Pattern.compile("[ \n\r\t]+")
    private val match: Matcher = Pattern.compile("").matcher(input)

    init {
        map[CalcDoubleToken.PLUS] = Pattern.compile("[+]")
        map[CalcDoubleToken.MINUS] = Pattern.compile("[-]")
        map[CalcDoubleToken.TIMES] = Pattern.compile("[*]")
        map[CalcDoubleToken.DIVIDE] = Pattern.compile("[/]")
        map[CalcDoubleToken.LPAREN] = Pattern.compile("[(]")
        map[CalcDoubleToken.RPAREN] = Pattern.compile("[)]")
        map[CalcDoubleToken.NUMBER] = Pattern.compile("([0-9]+)(.[0-9]+)?")
        map[CalcDoubleToken.END] = Pattern.compile("\\$")
    }

    fun getToken(): CalcDoubleToken? = token
    fun getTokenValue(): String = match.group()
    fun getPos(): Int = position

    private fun skip() {
        match.usePattern(skip)
        match.region(position, size)
        if (match.lookingAt()) {
            position = match.end()
        }
    }

    fun nextToken(): CalcDoubleToken {
        skip()

        if (position == size) {
            token = CalcDoubleToken.END
            return CalcDoubleToken.END
        }

        CalcDoubleToken.values().forEach {
            match.usePattern(map[it])
            match.region(position, size)
            if (match.lookingAt()) {
                position += match.end() - match.start()
                token = it
                return it
            }
        }

        throw CalcDoubleLexerException("Illegal sequence: ${input.substring(position)}", position)
    }
}
