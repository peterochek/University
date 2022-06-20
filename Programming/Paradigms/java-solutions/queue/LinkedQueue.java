package queue;

public class LinkedQueue extends AbstractQueue {
    private static class Element {
        private final Object element;
        private Element next;

        public Element(final Object element) {
            this.element = element;
        }
    }

    private Element start, end;

    @Override
    public void enqueue(final Object element) {
        size += 1;

        if (start != null) {
            final Element newElement = new Element(element);
            end.next = end = newElement;
            return;
        }

        start = new Element(element);
        end = start;
    }

    @Override
    public Object element() {
        return start.element;
    }

    @Override
    public Object dequeue() {
        final Object element = start.element;
        start = start.next;
        size -= 1;
        return element;
    }

    @Override
    public void clear() {
        start = null;
        size = 0;
    }

    @Override
    public Queue newQueue() {
        return new LinkedQueue();
    }
}
