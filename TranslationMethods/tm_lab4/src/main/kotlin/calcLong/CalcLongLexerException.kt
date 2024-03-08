package calcLong

import java.text.ParseException

class CalcLongLexerException(override val message: String, errorOffset: Int): ParseException(message, errorOffset)
