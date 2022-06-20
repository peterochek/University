package queue;

public class NewArrayQueueModuleTest {
    public static void fill_enqueue() {
        for (int i = 0; i < 10; i++) {
            ArrayQueueModule.enqueue(i);
        }
    }

    public static void fill_push() {
        for (int i = 0; i < 10; i++) {
            ArrayQueueModule.push(i);
        }
    }

    public static void dump() {
        while (!ArrayQueueModule.isEmpty()) {
            System.out.println(ArrayQueueModule.size() + " " +
                    ArrayQueueModule.element() + " " + ArrayQueueModule.dequeue() + " " + ArrayQueueModule.peek());
        }
    }

    public static void clear() {
        ArrayQueueModule.clear();
        System.out.println("Size after clear = " + ArrayQueueModule.size());
    }

    public static void main(String[] args) {
        System.out.println("ArrayQueueModule test started...");
        fill_enqueue();
        dump();
        System.out.println("=============");
        fill_enqueue();
        clear();
        System.out.println("=============");
        fill_push();
        dump();
        clear();
    }
}

