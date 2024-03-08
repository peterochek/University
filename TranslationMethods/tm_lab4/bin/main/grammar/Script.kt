package grammar

data class Script(override val name: String) : RuleToken {
    val script: String
        get() {
            return name.replace("$", "result.")
        }
}