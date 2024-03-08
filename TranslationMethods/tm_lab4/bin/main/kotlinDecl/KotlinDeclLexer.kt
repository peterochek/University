package kotlinDecl

import java.util.*
import java.util.regex.*

class KotlinDeclLexer(private val input: String) {
    private val size: Int
        get() {
            return input.length
        }

    private var position: Int = 0
    private var token: KotlinDeclToken? = null
    private val map: MutableMap<KotlinDeclToken, Pattern> = EnumMap(KotlinDeclToken::class.java)
    private val skip: Pattern = Pattern.compile("[ \n\r\t]+")
    private val match: Matcher = Pattern.compile("").matcher(input)

    init {
        map[KotlinDeclToken.VAR] = Pattern.compile("[v][a][r]")
        map[KotlinDeclToken.VAL] = Pattern.compile("[v][a][l]")
        map[KotlinDeclToken.NAME] = Pattern.compile("[a-z]+[a-zA-Z]*")
        map[KotlinDeclToken.COLON] = Pattern.compile("[:]")
        map[KotlinDeclToken.TYPE] = Pattern.compile("[I][n][t]")
        map[KotlinDeclToken.EQ] = Pattern.compile("[=]")
        map[KotlinDeclToken.NUMBER] = Pattern.compile("(0)|([1-9][0-9]*)")
        map[KotlinDeclToken.SEMICOLON] = Pattern.compile("[;]")
        map[KotlinDeclToken.END] = Pattern.compile("\\$")
    }

    fun getToken(): KotlinDeclToken? = token
    fun getTokenValue(): String = match.group()
    fun getPos(): Int = position

    private fun skip() {
        match.usePattern(skip)
        match.region(position, size)
        if (match.lookingAt()) {
            position = match.end()
        }
    }

    fun nextToken(): KotlinDeclToken {
        skip()

        if (position == size) {
            token = KotlinDeclToken.END
            return KotlinDeclToken.END
        }

        KotlinDeclToken.values().forEach {
            match.usePattern(map[it])
            match.region(position, size)
            if (match.lookingAt()) {
                position += match.end() - match.start()
                token = it
                return it
            }
        }

        throw KotlinDeclLexerException("Illegal sequence: ${input.substring(position)}", position)
    }
}
