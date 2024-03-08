import org.antlr.v4.runtime.BaseErrorListener
import org.antlr.v4.runtime.RecognitionException
import org.antlr.v4.runtime.Recognizer

class ErrorListener : BaseErrorListener() {
    private val errors = mutableListOf<String>()

    override fun syntaxError(
        recognizer: Recognizer<*, *>,
        offendingSymbol: Any?,
        line: Int,
        charPositionInLine: Int,
        msg: String,
        e: RecognitionException?
    ) {
        errors.add("$line:$charPositionInLine { $msg }")
    }

    fun isEmpty(): Boolean {
        return errors.isEmpty()
    }

    val getErrors get() = errors

    fun addError(errorMsg: String) {
        errors.add(errorMsg)
    }
}
