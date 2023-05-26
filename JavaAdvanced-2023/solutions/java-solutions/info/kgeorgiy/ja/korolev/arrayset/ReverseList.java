package info.kgeorgiy.ja.korolev.arrayset;

import java.util.AbstractList;
import java.util.Collections;
import java.util.List;
import java.util.RandomAccess;

public class ReverseList<E> extends AbstractList<E> implements RandomAccess {
    private final List<E> collection;
    private final boolean reversed;

    public ReverseList(List<E> list, boolean reversed) {
        this.collection = Collections.unmodifiableList(list);
        this.reversed = reversed;
    }

    public boolean reversed() {
        return reversed;
    }

    @Override
    public E get(int idx) {
        return reversed ? collection.get(size() - idx - 1) : collection.get(idx);
    }

    @Override
    public int size() {
        return collection.size();
    }
}