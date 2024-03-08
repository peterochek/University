package grammar

class Grammar(val start: String) {
    val tRs: MutableList<TR> = ArrayList()
    val ntRs: MutableList<NTR> = ArrayList()
    val generics: MutableList<Generic> = ArrayList()

    private val fst: MutableMap<String, MutableSet<String>> = HashMap()
    val flw: MutableMap<String, MutableSet<String>> = HashMap()

    var imports: MutableList<String> = ArrayList()

    fun addTR(rule: TR) = tRs.add(rule)

    fun addNTR(rule: NTR) = ntRs.add(rule)

    fun addGeneric(generic: Generic) = generics.add(generic)

    fun buildAll() {
        getFstSet()
        getFlwSet()
    }

    fun fstSet(tokens: List<RuleToken>): Set<String> {
        tokens.filter { it is T || it is NT }.forEach {
            return if (it is T) {
                setOf(it.name)
            } else {
                fst[it.name]?.toHashSet() ?: setOf()
            }
        }

        return setOf(EPS)
    }

    private fun init(map: MutableMap<String, MutableSet<String>>) {
        if (map.isNotEmpty()) return

        ntRs.forEach {
            map[it.parent] = HashSet()
        }
    }

    private fun getFstSet() {
        init(fst)

        var changed = true

        while (changed) {
            changed = false

            ntRs.forEach {
                it.tokens.forEach { rightPart ->
                    changed = changed || fst[it.parent]?.addAll(fstSet(rightPart)) ?: false
                }
            }
        }
    }

    private fun getFlwSet() {
        init(flw)

        flw[start]?.add(END)

        var changed = true

        while (changed) {
            changed = false

            ntRs.forEach {
                it.tokens.forEach { rightPart ->
                    rightPart.forEachIndexed { index, ruleToken ->
                        if (ruleToken is NT) {
                            val set = HashSet<String>()
                            set.addAll(fstSet(rightPart.subList(index + 1, rightPart.size)))

                            if (set.remove(EPS)) {
                                flw[it.parent]?.let { itt -> set.addAll(itt) }
                            }

                            changed = changed || flw[ruleToken.name]?.addAll(set) ?: false
                        }
                    }
                }
            }
        }
    }

    companion object {
        const val EPS = "EPS"
        const val END = "$"
    }
}