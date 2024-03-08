import java.util.concurrent.atomic.AtomicReference

class Solution(private val env: Environment) : Lock<Solution.Node> {
    private val tail = AtomicReference<Node>(null)

    override fun lock(): Node {
        val myNode = Node()
        val prevNode = tail.getAndSet(myNode)

        if (prevNode != null) {
            myNode.locked.set(true)
            prevNode.nextNode.set(myNode)

            while (myNode.locked.get()) {
                env.park()
            }
        }

        return myNode
    }

    override fun unlock(node: Node) {
        val nextNode = node.nextNode.get()
        if (nextNode == null) {
            if (tail.compareAndSet(node, null)) {
                return
            }

            while (true) {
            }
        }

        nextNode.locked.set(false)
        env.unpark(nextNode.thread)
    }

    class Node {
        val thread: Thread = Thread.currentThread()
        val locked = AtomicReference(false)
        val nextNode = AtomicReference<Node>(null)
    }
}
