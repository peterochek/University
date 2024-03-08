package calcInt

import java.text.ParseException

class CalcIntLexerException(override val message: String, errorOffset: Int): ParseException(message, errorOffset)
