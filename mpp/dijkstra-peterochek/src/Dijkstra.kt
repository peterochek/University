package dijkstra

import kotlinx.atomicfu.atomic
import java.util.*
import java.util.concurrent.Phaser
import kotlin.concurrent.thread

private val NODE_DISTANCE_COMPARATOR = Comparator<Node> { o1, o2 -> Integer.compare(o1!!.distance, o2!!.distance) }

// Returns `Integer.MAX_VALUE` if a path has not been found.
fun shortestPathParallel(start: Node) {
    val workers = Runtime.getRuntime().availableProcessors()
    // The distance to the start node is `0`
    start.distance = 0
    // Create a priority (by distance) queue and add the start node into it
    val q = PriorityMultiQueue(workers, NODE_DISTANCE_COMPARATOR)
    q.add(start)
    // Run worker threads and wait until the total work is done
    val onFinish = Phaser(workers + 1) // `arrive()` should be invoked at the end by each worker
    repeat(workers) {
        thread {
            while (true) {
                val cur: Node? = synchronized(q) { q.poll() }
                if (cur == null) {
                    if (q.isEmpty()) break else continue
                }
                for (e in cur.outgoingEdges) {
                    val updated = cur.distance + e.weight
                    while (true) {
                        val current = e.to.distance
                        if (updated >= current) break
                        if (e.to.casDistance(current, updated)) {
                            q.add(e.to)
                            break
                        }
                    }
                }
                q.size.decrementAndGet()
            }
            onFinish.arrive()
        }
    }
    onFinish.arriveAndAwaitAdvance()
}

private class PriorityMultiQueue(val workers: Int, comparator: Comparator<Node>) {
    val size = atomic(0)
    val queues: MutableList<PriorityQueue<Node>> = Collections.nCopies(workers, PriorityQueue(comparator))
    val randGen = Random(239)

    fun poll(): Node? {
        val fstIdx = randGen.nextInt(workers)
        var lstIdx = randGen.nextInt(workers)
        while (lstIdx == fstIdx) {
            lstIdx = randGen.nextInt(workers)
        }
        val fstQ = queues[fstIdx]
        val lstQ = queues[lstIdx]

        synchronized(fstQ) {
            synchronized(lstQ) {
                val fstP = fstQ.peek()
                val lstP = lstQ.peek()
                if (fstP == null) return lstP
                if (lstP == null) return fstP
                return minOf(fstQ, lstQ, compareBy {
                    it.peek().distance
                }).poll()
            }
        }
    }

    fun add(element: Node) {
        val rndIdx = randGen.nextInt(workers)
        val queue = queues[rndIdx]
        synchronized(queue) {
            queue.add(element)
        }
        size.incrementAndGet()
    }

    fun isEmpty(): Boolean {
        return size.compareAndSet(0, 0)
    }
}