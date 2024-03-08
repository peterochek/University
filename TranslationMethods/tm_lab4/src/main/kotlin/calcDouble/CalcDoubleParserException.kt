package calcDouble

import java.text.ParseException

class CalcDoubleParserException(override val message: String, errorOffset: Int): ParseException(message, errorOffset)
