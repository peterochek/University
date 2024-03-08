import java.util.concurrent.ThreadLocalRandom
import java.util.concurrent.atomic.AtomicReferenceArray

/**
 * @author Korolev Peter
 */
open class TreiberStackWithElimination<E> : Stack<E> {
    private val stack = TreiberStack<E>()

    private val eliminationArray = AtomicReferenceArray<Any?>(ELIMINATION_ARRAY_SIZE)

    override fun push(element: E) {
        if (tryPushElimination(element)) return
        stack.push(element)
    }

    protected open fun tryPushElimination(element: E): Boolean {
        val idx = randomCellIndex()

        if (!eliminationArray.compareAndSet(idx, CELL_STATE_EMPTY, element)) {
            return false // No elimination
        }
        repeat(ELIMINATION_WAIT_CYCLES) {}
        if (!eliminationArray.compareAndSet(idx, element, CELL_STATE_EMPTY)) {
            return eliminationArray.compareAndSet(idx, CELL_STATE_RETRIEVED, CELL_STATE_EMPTY)
        }

        return false
    }

    override fun pop(): E? = tryPopElimination() ?: stack.pop()

    private fun tryPopElimination(): E? {
        val idx = randomCellIndex()
        val cell = eliminationArray.get(idx)

        if (cell == CELL_STATE_EMPTY || cell == CELL_STATE_RETRIEVED) {
            return null
        }
        if (!eliminationArray.compareAndSet(idx, cell, CELL_STATE_RETRIEVED)) {
            return null // No elimination occurred or empty
        }

        return cell as E?
    }

    private fun randomCellIndex(): Int =
        ThreadLocalRandom.current().nextInt(eliminationArray.length())

    companion object {
        private const val ELIMINATION_ARRAY_SIZE = 2 // Do not change!
        private const val ELIMINATION_WAIT_CYCLES = 1 // Do not change!

        // Initially, all cells are in EMPTY state.
        private val CELL_STATE_EMPTY = null

        // `tryPopElimination()` moves the cell state
        // to `RETRIEVED` if the cell contains element.
        private val CELL_STATE_RETRIEVED = Any()
    }
}
