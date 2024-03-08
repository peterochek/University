package grammar

data class T(override val name: String) : RuleToken

sealed interface TR : Rule {
    val child: String
}

data class TRImpl(override val parent: String, override val child: String) : TR
data class RRImpl(override val parent: String, override val child: String) : TR
