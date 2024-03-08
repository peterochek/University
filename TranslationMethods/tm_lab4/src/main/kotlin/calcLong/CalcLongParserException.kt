package calcLong

import java.text.ParseException

class CalcLongParserException(override val message: String, errorOffset: Int): ParseException(message, errorOffset)
