package calcFloat

import java.text.ParseException

class CalcFloatParserException(override val message: String, errorOffset: Int): ParseException(message, errorOffset)
