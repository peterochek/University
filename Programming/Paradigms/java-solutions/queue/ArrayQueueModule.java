package queue;

import java.util.Arrays;

public class ArrayQueueModule {
    //Immutable(shift, l, r) <-> ∀i: l <= i < r => queue[i - shift] = queue`[i] //indexing is used to show order

	//Model: queue[0]...queue[n - 1] //queue is sequence of elements
    //for (element : queue) element != null
    private static int start = 0;
    private static int size = 0;
    private static Object[] elements = new Object[2];

    //Pre: queue != null && n != 0
    //Post: n` > n && Immutable(0, 0, n)
    private static void ensureCapacity(int capacity) {
        if (capacity > elements.length) {
            Object[] tmp = new Object[2 * capacity];
            System.arraycopy(elements, 0, tmp, elements.length - start, start);
            System.arraycopy(elements, start, tmp, 0, elements.length - start);
            elements = tmp;
            start = 0;
        }
    }

    //Pre: element != null
    //Post: n` = n + 1 && queue[0] = element && Immutable(1, 1, n + 1)
    public static void enqueue(Object element) {
        assert element != null;
        ensureCapacity(size + 1);
        elements[(start + size++) % elements.length] = element;
    }

    //Pre: n > 0
    //Post: R = queue[n - 1] && Immutable(0, 0, n)
    public static Object element() {
        assert size > 0;
        return elements[start];
    }

    //Pre: n > 0
    //Post: R = queue[n - 1] && n` = n - 1 && Immutable(0, 0, n - 1)
    public static Object dequeue() {
        assert size > 0;
        Object element = element();
        size--;
        elements[start++] = null;
        start %= elements.length;
        return element;
    }

    //Pre: element != null
    //Post: queue[n] = element && n` = n + 1 && Immutable(0, 0, n)
    public static void push(Object element) {
        assert element != null;
        ensureCapacity(size++ + 1);
        int position = Math.floorMod(start - 1, elements.length);
        elements[position] = element;
        start = position;
    }

    //Pre: n > 0
    //Post: R = queue[0] && Immutable(0, 0, n)
    public static Object peek() {
        assert size > 0;
        return elements[(start + size - 1) % elements.length];
    }

    //Pre: size > 0
    //Post: R = queue[0] && n` = n - 1 && Immutable(-1, 1, n) <- old
    public static Object remove() {
        assert size > 0;
        Object element = peek();
        elements[(start + size-- - 1) % elements.length] = null;
        return element;
    }

    //Pre: true
    //element != null //implementation
    //Post: R = count(queue, element = queue_element), queue_element in elements && Immutable(0, 0, n)
    public static int count(Object element) {
        assert element != null;
        int count = 0;

        for (Object local_element : elements) {
            if (element.equals(local_element)) {
                count++;
            }
        }

        return count;
    }

    //Pre: true
    //Post: R = n && Immutable(0, 0, n)
    public static int size() {
        return size;
    }

    //Pre: true
    //Post: R = (n == 0) && Immutable(0, 0, n)
    public static boolean isEmpty() {
        return size == 0;
    }

    //Pre: true
    //Post: n = 0
    public static void clear() {
        start = size = 0;
        Arrays.fill(elements, null);
        //elements = new Object[2];
    }

}
