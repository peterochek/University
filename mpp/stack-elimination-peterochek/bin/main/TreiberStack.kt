import java.util.concurrent.atomic.AtomicReference

/**
 * @author Korolev Peter
 */
class TreiberStack<E> : Stack<E> {
    // Initially, the stack is empty.
    private val top = AtomicReference<Node<E>?>(null)

    override fun push(element: E) {
        while (true) {
            val curTop = top.get()
            val newTop = Node(element, curTop)
            if (!top.compareAndSet(curTop, newTop)) {
                continue
            }
            return
        }
    }

    override fun pop(): E? {
        while (true) {
            val curTop = top.get() ?: return null
            val next = curTop.next
            if (!top.compareAndSet(curTop, next)) {
                continue
            }
            return curTop.element
        }
    }

    private class Node<E>(
        val element: E,
        val next: Node<E>?
    )
}
