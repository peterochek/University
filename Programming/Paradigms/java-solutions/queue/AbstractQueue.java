package queue;

import java.util.function.Function;
import java.util.function.Predicate;

public abstract class AbstractQueue implements Queue {
    protected int size = 0;

    @Override
    public int size() {
        return size;
    }

    @Override
    public boolean isEmpty() {
        return (size == 0);
    }

    @Override
    public Queue map(final Function<Object, Object> function) {
        final Queue queue = newQueue();
        for (int i = 0; i < size; i++) {
            final Object element = dequeue();
            queue.enqueue(function.apply(element));
            this.enqueue(element);
        }

        return queue;
    }

    @Override
    public Queue filter(final Predicate<Object> predicate) {
        final Queue queue = newQueue();
        for (int i = 0; i < size; i++) {
            final Object element = this.dequeue();
            if (predicate.test(element)) {
                queue.enqueue(element);
            }
            this.enqueue(element);
        }

        return queue;
    }

    public abstract Queue newQueue();
}
