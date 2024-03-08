import calcDouble.CalcDoubleParser
import calcFloat.CalcFloatParser
import calcInt.CalcIntParser
import calcLong.CalcLongParser
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class Calc {

    @Test
    fun testDouble() {
        val tests = arrayOf<Pair<String, Number>>(
            "2.0" to 2.0,
            "1 + 2.0" to 3.0,
            "1 - 3" to -2.0,
            "1 * 2" to 2.0,
            "1 / 2" to 0.5,
            "1 * (2 + 1) - 3 / 2" to 1.5
        )

        for (test in tests) {
            assertEquals(CalcDoubleParser(test.first).parse().v, test.second)
        }
    }

    @Test
    fun testFloat() {
        val tests = arrayOf<Pair<String, Number>>(
            "2.0" to 2.0F,
            "1 + 2.0" to 3.0F,
            "1 - 3" to -2.0F,
            "1 * 2" to 2.0F,
            "1 / 2" to 0.5F,
            "1 * (2 + 1) - 3 / 2" to 1.5F
        )

        for (test in tests) {
            assertEquals(CalcFloatParser(test.first).parse().v, test.second)
        }
    }

    @Test
    fun testInt() {
        val tests = arrayOf<Pair<String, Number>>(
            "2" to 2,
            "1 + 2" to 3,
            "1 - 3" to -2,
            "1 * 2" to 2,
            "1 / 2" to 0,
            "1 * (2 + 1) - 3 / 2" to 2
        )

        for (test in tests) {
            assertEquals(CalcIntParser(test.first).parse().v, test.second)
        }
    }

    @Test
    fun testLong() {
        val tests = arrayOf<Pair<String, Number>>(
            "2" to 2L,
            "1 + 2" to 3L,
            "1 - 3" to -2L,
            "1 * 2" to 2L,
            "1 / 2" to 0L,
            "1 * (2 + 1) - 3 / 2" to 2L
        )

        for (test in tests) {
            assertEquals(CalcLongParser(test.first).parse().v, test.second)
        }
    }
}