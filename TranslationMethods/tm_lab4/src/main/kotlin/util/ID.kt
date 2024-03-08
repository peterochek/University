package util

object ID {
    private var value = 1
    private var last = ""

    fun inc(input: String): String {
        last = input + "\nid" + value++
        return last
    }

    fun last(): String {
        return last
    }
}