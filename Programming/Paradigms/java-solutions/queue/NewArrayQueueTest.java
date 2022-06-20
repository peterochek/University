package queue;

public class NewArrayQueueTest {
    public static void fill_enqueue(ArrayQueue queue) {
        for (int i = 0; i < 10; i++) {
            queue.enqueue(i);
        }
    }

    public static void fill_push(ArrayQueue queue) {
        for (int i = 0; i < 10; i++) {
            queue.push(i);
        }
    }

    public static void dump(ArrayQueue queue) {
        while (!queue.isEmpty()) {
            System.out.println(queue.size() + " " +
                    queue.element() + " " + queue.dequeue() + " " + queue.peek());
        }
    }

    public static void clear(ArrayQueue queue) {
        queue.clear();
        System.out.println("Size after clear = " + queue.size());
    }

    public static void main(String[] args) {
        System.out.println("ArrayQueue test started...");
        ArrayQueue queue = new ArrayQueue();
        fill_enqueue(queue);
        dump(queue);
        System.out.println("=============");
        fill_enqueue(queue);
        clear(queue);
        System.out.println("=============");
        fill_push(queue);
        dump(queue);
        clear(queue);
    }
}

