package info.kgeorgiy.ja.korolev.arrayset;

import java.util.*;

public class ArraySet<E> extends AbstractSet<E> implements NavigableSet<E> {

    private final ReverseList<E> collection;
    private final Comparator<? super E> comparator;

    public ArraySet() {
        this(Collections.emptyList(), null);
    }

    public ArraySet(Collection<? extends E> collection) {
        this(collection, null);
    }

    public ArraySet(Comparator<? super E> comparator) {
        this(Collections.emptyList(), comparator);
    }

    public ArraySet(Collection<? extends E> collection, Comparator<? super E> comparator) {
        TreeSet<E> treeSet = new TreeSet<>(comparator);
        treeSet.addAll(collection);
        this.collection = new ReverseList<>(new ArrayList<>(treeSet), false);
        this.comparator = comparator;
    }

    private ArraySet(ReverseList<E> reverseList, Comparator<? super E> comparator) {
        this.collection = reverseList;
        this.comparator = comparator;
    }

    public ArraySet(SortedSet<E> sortedSet) {
        this(new ReverseList<>(new ArrayList<>(sortedSet), false), sortedSet.comparator());
    }

    @Override
    public E lower(E e) {
        return elOrNull(e, false, true);
    }

    @Override
    public E floor(E e) {
        return elOrNull(e, true, true);
    }

    @Override
    public E ceiling(E e) {
        return elOrNull(e, true, false);
    }

    @Override
    public E higher(E e) {
        return elOrNull(e, false, false);
    }

    @Override
    public E pollFirst() {
        throw new UnsupportedOperationException();
    }

    @Override
    public E pollLast() {
        throw new UnsupportedOperationException();
    }

    @Override
    public int size() {
        return collection.size();
    }

    @Override
    public boolean isEmpty() {
        return collection.isEmpty();
    }

    @Override
    public boolean contains(Object o) {
        @SuppressWarnings("unchecked") E e = (E) o;
        return Collections.binarySearch(collection, e, comparator) >= 0;
    }

    @Override
    public Iterator<E> iterator() {
        return collection.iterator();
    }

    @Override
    public NavigableSet<E> descendingSet() {
        return new ArraySet<>(new ReverseList<>(collection, true), Collections.reverseOrder(comparator));
    }

    @Override
    public Iterator<E> descendingIterator() {
        return descendingSet().iterator();
    }

    private NavigableSet<E> genericSubSet(E fromElement, boolean fromInclusive, E toElement, boolean toInclusive) {
        int fromIdx = binarySearch(fromElement, fromInclusive, false);
        int toIdx = binarySearch(toElement, toInclusive, true);

        if (fromIdx > toIdx) {
            return new ArraySet<>(comparator);
        }

        return new ArraySet<>(new ReverseList<>(collection.subList(fromIdx, toIdx + 1), collection.reversed()), comparator);
    }

    @Override
    public NavigableSet<E> subSet(E fromElement, boolean fromInclusive, E toElement, boolean toInclusive) {
        if (comparator.compare(fromElement, toElement) > 0) {
            throw new IllegalArgumentException();
        }
        return genericSubSet(fromElement, fromInclusive, toElement, toInclusive);
    }

    @Override
    public NavigableSet<E> headSet(E toElement, boolean inclusive) {
        if (isEmpty()) {
            return this;
        }
        return genericSubSet(first(), true, toElement, inclusive);
    }


    @Override
    public NavigableSet<E> tailSet(E fromElement, boolean inclusive) {
        if (isEmpty()) {
            return this;
        }
        return genericSubSet(fromElement, inclusive, last(), true);
    }

    @Override
    public Comparator<? super E> comparator() {
        return comparator;
    }

    @Override
    public SortedSet<E> subSet(E fromElement, E toElement) {
        return subSet(fromElement, true, toElement, false);
    }

    @Override
    public SortedSet<E> headSet(E toElement) {
        return headSet(toElement, false);
    }

    @Override
    public SortedSet<E> tailSet(E fromElement) {
        return tailSet(fromElement, true);
    }

    @Override
    public E first() {
        if (isEmpty()) {
            throw new NoSuchElementException();
        }
        return collection.get(0);
    }

    @Override
    public E last() {
        if (isEmpty()) {
            throw new NoSuchElementException();
        }
        return collection.get(size() - 1);
    }

    private int binarySearch(E element, boolean inclusive, boolean lower) {
        int idx = Collections.binarySearch(collection, element, comparator);

        if (idx >= 0) {
            return inclusive ? idx : (lower ? (idx - 1) : (idx + 1));
        } else {
            return -(lower ? idx + 2 : idx + 1);
        }
    }

    private E elOrNull(E e, boolean inclusive, boolean lower) {
        int idx = binarySearch(e, inclusive, lower);
        if (0 <= idx && idx < size()) {
            return collection.get(idx);
        } else {
            return null;
        }
    }
}
