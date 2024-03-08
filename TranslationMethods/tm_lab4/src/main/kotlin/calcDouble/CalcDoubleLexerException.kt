package calcDouble

import java.text.ParseException

class CalcDoubleLexerException(override val message: String, errorOffset: Int): ParseException(message, errorOffset)
