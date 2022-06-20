package queue;

import java.util.function.Function;
import java.util.function.Predicate;

//Immutable(shift, l, r) <-> ∀i: l <= i < r => queue[i - shift] = queue`[i] //indexesexing is used to show order

//Model: queue[0]...queue[n - 1] //queue is sequence of elements
//for (element : queue) element != null
public interface Queue {
    //Pre: n > 0
    //Post: R = n && Immutable(0, 0, n)
    int size();

    //Pre: n > 0
    //Post: R = (n == 0) && Immutable(0, 0, n)
    boolean isEmpty();

    //Pre: n > 0
    //Post: R = queue[n - 1] && Immutable(0, 0, n)
    Object element();

    //Pre: element != null
    //Post: n` = n + 1 && queue`[0] = element && Immutable(1, 1, n + 1)
    void enqueue(Object element);

    //Pre: n > 0
    //Post: R = queue[n - 1] && n` = n - 1 && Immutable(0, 0, n - 1)
    Object dequeue();

    //Pre: true
    //Post: n = 0
    void clear();

    //Pre: true
    //Post: R = queue`: ∀i: 0 <= i < n => queue`[i] = function(queue[i]) && n` = n
    Queue map(Function<Object, Object> function);

    //Pre: true
    //Post: R = queue` = [queue[indexes[0]], queue[indexes[1]], ... , queue[indexes[m` - 1]]) && n` <= n
    //∀i`: 0 <= i` < n` - 1 => indexes[i`] < indexes[i` + 1] && predicate(queue`[i`]) = true
    //!∃ m: 0 <= m < n => predicate(queue[m]) = true && !∃ m`: 0 <= m` < n`: indexes[m`] = m
    Queue filter(Predicate<Object> predicate);

    //Pre: true
    //Post: R = queue` && n` = 0
    Queue newQueue();
}
