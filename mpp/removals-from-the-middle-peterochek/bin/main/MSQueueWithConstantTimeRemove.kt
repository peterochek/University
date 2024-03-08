@file:Suppress("DuplicatedCode", "FoldInitializerAndIfToElvis")

import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference

/**
 * @author Korolev Peter
 */
class MSQueueWithConstantTimeRemove<E> : QueueWithRemove<E> {
    private val head: AtomicReference<Node<E>>
    private val tail: AtomicReference<Node<E>>

    init {
        val dummy = Node<E>(element = null, prev = null)
        head = AtomicReference(dummy)
        tail = AtomicReference(dummy)
    }

    override fun enqueue(element: E) {
        while (true) {
            val tmpTail = tail.get()
            val newTail = Node(element, tmpTail)
            if (tmpTail.next.compareAndSet(null, newTail)) { // this tail is the last (no races)
                tail.compareAndSet(tmpTail, newTail)
                tmpTail.checkExtractedOrRemoved()
                return
            }

            val updatedTail = tmpTail.next.get()
            if (updatedTail != null) {
                tail.compareAndSet(tmpTail, updatedTail)
            }
        }
    }

    override fun dequeue(): E? {
        while (true) {
            val tmpHead = head.get()
            val updatedHead = tmpHead.next.get() ?: return null
            updatedHead.prev.set(null)
            if (head.compareAndSet(tmpHead, updatedHead) && updatedHead.markExtractedOrRemoved()) {
                return updatedHead.element
            }
        }
    }

    override fun remove(element: E): Boolean {
        // Traverse the linked list, searching the specified
        // element. Try to remove the corresponding node if found.
        // DO NOT CHANGE THIS CODE.
        var node = head.get()
        while (true) {
            val next = node.next.get()
            if (next == null) return false
            node = next
            if (node.element == element && node.remove()) return true
        }
    }

    /**
     * This is an internal function for tests.
     * DO NOT CHANGE THIS CODE.
     */
    override fun validate() {
        check(head.get().prev.get() == null) {
            "`head.prev` must be null"
        }
        check(tail.get().next.get() == null) {
            "tail.next must be null"
        }
        // Traverse the linked list
        var node = head.get()
        while (true) {
            if (node !== head.get() && node !== tail.get()) {
                check(!node.extractedOrRemoved) {
                    "Removed node with element ${node.element} found in the middle of the queue"
                }
            }
            val nodeNext = node.next.get()
            // Is this the end of the linked list?
            if (nodeNext == null) break
            // Is next.prev points to the current node?
            val nodeNextPrev = nodeNext.prev.get()
            check(nodeNextPrev != null) {
                "The `prev` pointer of node with element ${nodeNext.element} is `null`, while the node is in the middle of the queue"
            }
            check(nodeNextPrev == node) {
                "node.next.prev != node; `node` contains ${node.element}, `node.next` contains ${nodeNext.element}"
            }
            // Process the next node.
            node = nodeNext
        }
    }

    private class Node<E>(
        var element: E?,
        prev: Node<E>?
    ) {
        val next = AtomicReference<Node<E>?>(null)
        val prev = AtomicReference(prev)

        private val _extractedOrRemoved = AtomicBoolean(false)
        val extractedOrRemoved
            get() =
                _extractedOrRemoved.get()

        fun markExtractedOrRemoved(): Boolean =
            _extractedOrRemoved.compareAndSet(false, true)

        fun checkExtractedOrRemoved() {
            if (this.extractedOrRemoved) {
                this.remove()
            }
        }

        /**
         * Removes this node from the queue structure.
         * Returns `true` if this node was successfully
         * removed, or `false` if it has already been
         * removed by [remove] or extracted by [dequeue].
         */
        fun remove(): Boolean {
            val marker = markExtractedOrRemoved()
            val next = next.get() ?: return marker
            val prev = prev.get() ?: return marker
            prev.next.set(next)
            while (true) {
                val tmp = next.prev.get() ?: break
                if (next.prev.compareAndSet(tmp, prev)) {
                    break
                }
            }
            prev.checkExtractedOrRemoved()
            next.checkExtractedOrRemoved()
            return marker
        }
    }
}