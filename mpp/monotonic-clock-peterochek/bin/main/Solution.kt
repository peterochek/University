/**
 * В теле класса решения разрешено использовать только переменные делегированные в класс RegularInt.
 * Нельзя volatile, нельзя другие типы, нельзя блокировки, нельзя лазить в глобальные переменные.
 *
 * @author Korolev Peter
 */
class Solution : MonotonicClock {
    private var c1 by RegularInt(0)
    private var c2 by RegularInt(0)
    private var c3 by RegularInt(0)

    private var c1Tmp by RegularInt(0)
    private var c2Tmp by RegularInt(0)
    private var c3Tmp by RegularInt(0)

    override fun write(time: Time) {
        // write left-to-right
        c1Tmp = time.d1
        c2Tmp = time.d2
        c3Tmp = time.d3

        // write right-to-left
        c3 = time.d3 //c3 = c3Tmp
        c2 = time.d2 //c2 = c2Tmp
        c1 = time.d1 //c1 = c1Tmp
    }

    override fun read(): Time {
        // read left-to-right
        val r11 = c1
        val r12 = c2
        val r13 = c3

        // read right-to-left
        val r23 = c3Tmp
        val r22 = c2Tmp
        val r21 = c1Tmp

        if (Triple(r11, r12, r13) == Triple(r21, r22, r23)) {
            return Time(r11, r12, r13)
        }

        if (Pair(r11, r12) == Pair(r21, r22)) {
            return Time(r11, r12, r23)
        }

        if (r11 == r21) {
            return Time(r11, r22, 0)
        }

        return Time(r21, 0, 0)
    }
}