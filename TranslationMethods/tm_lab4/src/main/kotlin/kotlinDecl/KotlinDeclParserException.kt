package kotlinDecl

import java.text.ParseException

class KotlinDeclParserException(override val message: String, errorOffset: Int): ParseException(message, errorOffset)
