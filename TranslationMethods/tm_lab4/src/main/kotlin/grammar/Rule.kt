package grammar

sealed interface Rule {
    val parent: String
}

interface RuleToken {
    val name: String
}


