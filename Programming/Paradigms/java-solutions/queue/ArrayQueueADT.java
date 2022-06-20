package queue;

import java.util.Arrays;

public class ArrayQueueADT {
    //Immutable(shift, l, r) <-> ∀i: l <= i < r => queue[i - shift] = queue`[i] //indexing is used to show order
    //* in every method: Pre condition includes (queue != null)

    //Model: queue[0]...queue[n - 1] //queue is sequence of elements
    //for (element : queue) element != null
    private int start = 0;
    private int size = 0;
    private Object[] elements = new Object[2];

    //Pre: queue != null && n != 0
    //Post: n` > n && Immutable(0, 0, n)
    private static void ensureCapacity(ArrayQueueADT queue, int capacity) {
        if (capacity > queue.elements.length) {
            Object[] tmp = new Object[2 * capacity];
            System.arraycopy(queue.elements, 0, tmp, queue.elements.length - queue.start, queue.start);
            System.arraycopy(queue.elements, queue.start, tmp, 0, queue.elements.length - queue.start);
            queue.elements = tmp;
            queue.start = 0;
        }
    }

    //Pre: element != null
    //Post: n` = n + 1 && queue[0] = element && Immutable(1, 1, n + 1)
    public static void enqueue(ArrayQueueADT queue, Object element) {
        assert element != null;
        ensureCapacity(queue, queue.size + 1);
        queue.elements[(queue.start + queue.size++) % queue.elements.length] = element;
    }

    //Pre: n > 0
    //Post: R = queue[n - 1] && Immutable(0, 0, n)
    public static Object element(ArrayQueueADT queue) {
        assert queue.size > 0;
        return queue.elements[queue.start];
    }

    //Pre: n > 0
    //Post: R = queue[n - 1] && n` = n - 1 && Immutable(0, 0, n - 1)
    public static Object dequeue(ArrayQueueADT queue) {
        assert queue.size > 0;
        Object element = element(queue);
        queue.size--;
        queue.elements[queue.start++] = null;
        queue.start %= queue.elements.length;
        return element;
    }

    //Pre: element != null
    //Post: queue[n] = element && n` = n + 1 && Immutable(0, 0, n)
    public static void push(ArrayQueueADT queue, Object element) {
        assert element != null;
        ensureCapacity(queue, queue.size++ + 1);
        int position = Math.floorMod(queue.start - 1, queue.elements.length);
        queue.elements[position] = element;
        queue.start = position;
    }

    //Pre: n > 0
    //Post: R = queue[0] && Immutable(0, n)
    public static Object peek(ArrayQueueADT queue) {
        assert queue.size > 0;
        return queue.elements[(queue.start + queue.size - 1) % queue.elements.length];
    }

    //Pre: size > 0
    //Post: R = queue[0] && n` = n - 1 && Immutable(-1, 1, n) <- old
    public static Object remove(ArrayQueueADT queue) {
        assert queue.size > 0;
        Object element = peek(queue);
        queue.elements[(queue.start + queue.size-- - 1) % queue.elements.length] = null;
        return element;
    }

    //Pre: true
    //element != null //implementation
    //Post: R = count(queue, element = queue_element), queue_element in elements && Immutable(0, 0, n)
    public static int count(ArrayQueueADT queue, Object element) {
        assert element != null;
        int count = 0;

        for (Object local_element : queue.elements) {
            if (element.equals(local_element)) {
                count++;
            }
        }

        return count;
    }

    //Pre: true
    //Post: R = n && Immutable(0, 0, n)
    public static int size(ArrayQueueADT queue) {
        return queue.size;
    }

    //Pre: true
    //Post: R = (n == 0) && Immutable(0, 0, n)
    public static boolean isEmpty(ArrayQueueADT queue) {
        return queue.size == 0;
    }

    //Pre: true
    //Post: n = 0
    public static void clear(ArrayQueueADT queue) {
        queue.start = queue.size = 0;
        Arrays.fill(queue.elements, null);
        //elements = new Object[2];
    }

}
