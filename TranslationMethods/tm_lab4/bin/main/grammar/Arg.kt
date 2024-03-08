package grammar

data class Arg(
    val name: String, val type: String
) {
    private fun defaultVal(type: String): String {
        return when (type) {
            "Int" -> {
                " = 0"
            }

            "Long" -> {
                " = 0L"
            }

            "Double" -> {
                " = 0.0"
            }

            "Float" -> {
                " = 0F"
            }

            "String" -> {
                " = \"\""
            }

            else -> {
                "? = null"
            }
        }
    }

    fun getDefaultRepr(): String {
        return "var $name: $type${defaultVal(type)}"
    }

    fun getRepr(): String {
        return "$name: $type"
    }
}
