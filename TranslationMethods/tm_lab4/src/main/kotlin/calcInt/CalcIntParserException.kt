package calcInt

import java.text.ParseException

class CalcIntParserException(override val message: String, errorOffset: Int): ParseException(message, errorOffset)
