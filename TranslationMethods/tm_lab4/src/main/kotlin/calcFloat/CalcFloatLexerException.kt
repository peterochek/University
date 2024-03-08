package calcFloat

import java.text.ParseException

class CalcFloatLexerException(override val message: String, errorOffset: Int): ParseException(message, errorOffset)
