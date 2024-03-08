package kotlinDecl

import java.text.ParseException

class KotlinDeclLexerException(override val message: String, errorOffset: Int): ParseException(message, errorOffset)
