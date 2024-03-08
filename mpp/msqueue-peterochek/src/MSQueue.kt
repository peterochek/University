import java.util.concurrent.atomic.AtomicReference

/**
 * @author Korolev Peter
 */

class MSQueue<E> : Queue<E> {
    private val head: AtomicReference<Node<E>>
    private val tail: AtomicReference<Node<E>>

    init {
        val dummy = Node<E>(null)
        head = AtomicReference(dummy)
        tail = AtomicReference(dummy)
    }

    override fun enqueue(element: E) {
        val node = Node(element)
        while (true) {
            val tail = this.tail.get()
            if (tail.next.compareAndSet(null, node)) {
                this.tail.compareAndSet(tail, node)
                return
            } else {
                this.tail.compareAndSet(tail, tail.next.get())
            }
        }
    }

    override fun dequeue(): E? {
        while (true) {
            val head = this.head.get()
            val tail = this.tail.get()
            val next = head.next.get() ?: return null

            if (head == tail) {
                this.tail.compareAndSet(tail, next)
            } else {
                val pvalue = next.element
                if (this.head.compareAndSet(head, next)) {
                    next.element = null
                    return pvalue
                }
            }
        }
    }

    // FOR TEST PURPOSE, DO NOT CHANGE IT.
    override fun validate() {
        check(tail.get().next.get() == null) {
            "At the end of the execution, `tail.next` must be `null`"
        }
        check(head.get().element == null) {
            "At the end of the execution, the dummy node shouldn't store an element"
        }
    }

    private class Node<E>(
        var element: E?
    ) {
        val next = AtomicReference<Node<E>?>(null)
    }
}
