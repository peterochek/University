package queue;

public class NewArrayQueueADTTest {
    public static void fill_enqueue(ArrayQueueADT queue) {
        for (int i = 0; i < 10; i++) {
            ArrayQueueADT.enqueue(queue, i);
        }
    }

    public static void dump(ArrayQueueADT queue) {
        while (!ArrayQueueADT.isEmpty(queue)) {
            System.out.println(ArrayQueueADT.size(queue) + " " +
                    ArrayQueueADT.element(queue) + " " + ArrayQueueADT.dequeue(queue) + " " + ArrayQueueADT.peek(queue));
        }
    }

    public static void fill_push(ArrayQueueADT queue) {
        for (int i = 0; i < 10; i++) {
            ArrayQueueADT.push(queue, i);
        }
    }

    public static void clear(ArrayQueueADT queue) {
        ArrayQueueADT.clear(queue);
        System.out.println("Size after clear = " + ArrayQueueADT.size(queue));
    }

    public static void main(String[] args) {
        System.out.println("ArrayQueueADT test started...");
        ArrayQueueADT queue = new ArrayQueueADT();
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

