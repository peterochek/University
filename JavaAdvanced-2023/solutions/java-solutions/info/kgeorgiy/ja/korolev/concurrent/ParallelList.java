package info.kgeorgiy.ja.korolev.concurrent;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * A thread-safe list implementation that allows parallel processing.
 *
 * @param <R> the type of elements held in this list
 */
public class ParallelList<R> {
    private final List<R> mapped = new ArrayList<>();
    private int jobsDone = 0;

    /**
     * Creates a new ParallelList with the specified size.
     *
     * @param size the size of the list to be created
     */
    public ParallelList(int size) {
        mapped.addAll(Collections.nCopies(size, null));
    }

    /**
     * Sets the element at the specified index.
     *
     * @param idx    the index at which the specified element is to be set
     * @param mapped the element to be set
     */
    public synchronized void set(final int idx, final R mapped) {
        this.mapped.set(idx, mapped);
        if (++jobsDone == this.mapped.size()) {
            notifyAll();
        }
    }

    /**
     * Returns the list of mapped elements.
     * This method blocks until all elements are set.
     *
     * @return the list of mapped elements
     * @throws InterruptedException if the current thread is interrupted while waiting
     */
    public synchronized List<R> getMapped() throws InterruptedException {
        while (jobsDone < mapped.size()) {
            wait();
        }

        return mapped;
    }
}