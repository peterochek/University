package grammar

data class NT(override val name: String) : RuleToken {
    val args: MutableList<String> = ArrayList()

    fun addArg(arg: String): Boolean = args.add(arg)
}

data class NTR(
    override val parent: String,
    val args: List<Arg>,
    val valueReturning: List<Arg>
) : Rule {
    val tokens: MutableList<List<RuleToken>> = ArrayList()

    fun addRule(list: List<RuleToken>) = tokens.add(list)

    fun getFields() = valueReturning
}