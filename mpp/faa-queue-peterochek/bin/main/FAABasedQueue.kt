import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.atomic.AtomicReferenceArray

/**
 * @author Korolev Peter
 */
class FAABasedQueue<E> : Queue<E> {
    private val initSegment = Segment(0L)
    private val head = AtomicReference(initSegment)
    private val tail = AtomicReference(initSegment)
    private val enqIdx = AtomicLong(0) // tail
    private val deqIdx = AtomicLong(0) // head

    override fun enqueue(element: E) {
        while (true) {
            val (freeSegment, inSegmentIdx) = process(tail, enqIdx)
            if (freeSegment.cells.compareAndSet(inSegmentIdx, null, element)) {
                return
            }
        }
    }

    @Suppress("UNCHECKED_CAST")
    override fun dequeue(): E? {
        while (true) {
            if (deqIdx.get() >= enqIdx.get()) {
                return null // Empty queue
            }
            val (freeSegment, inSegmentIdx) = process(head, deqIdx)
            if (freeSegment.cells.compareAndSet(inSegmentIdx, null, POISONED)) {
                continue
            }
            return freeSegment.cells.get(inSegmentIdx) as E?
        }
    }

    private fun process(init: AtomicReference<Segment>, idx: AtomicLong): Pair<Segment, Int> {
        val curSegment = init.get()
        val curIdx = idx.getAndIncrement()
        var freeSegment = curSegment
        while (freeSegment.id < curIdx / SEGMENT_SIZE) {
            val newSegment = Segment(freeSegment.id + 1)
            freeSegment.next.compareAndSet(null, newSegment)
            freeSegment = freeSegment.next.get()
        }
        return freeSegment to (curIdx % SEGMENT_SIZE).toInt()
    }
}

private class Segment(val id: Long) {
    val next = AtomicReference<Segment?>(null)
    val cells = AtomicReferenceArray<Any?>(SEGMENT_SIZE)
}

// DO NOT CHANGE THIS CONSTANT
private const val SEGMENT_SIZE = 2

private val POISONED = Any()