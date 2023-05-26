package info.kgeorgiy.ja.korolev.concurrent;

import info.kgeorgiy.java.advanced.concurrent.ListIP;
import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class IterativeParallelism implements ListIP {
    private final ParallelMapper parallelMapper;

    public IterativeParallelism() {
        this(null);
    }

    public IterativeParallelism(ParallelMapper parallelMapper) {
        this.parallelMapper = parallelMapper;
    }

    /**
     * Returns the minimum value in the input list using multiple threads.
     *
     * @param <T>        the type of the input values
     * @param threads    the number of threads to use
     * @param values     the list of values to find the minimum from
     * @param comparator the comparator used to compare values
     * @return the minimum value in the list, or null if the list is empty
     * @throws InterruptedException if an interrupt request is received while waiting for the threads to complete
     */
    @Override
    public <T> T minimum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
        Function<Stream<? extends T>, ? extends T> extractMin = stream -> stream.min(comparator).orElse(null);

        return concurrent(
                threads,
                values,
                extractMin,
                extractMin
        );
    }

    /**
     * Returns the maximum value in the input list using multiple threads.
     *
     * @param <T>        the type of the input values
     * @param threads    the number of threads to use
     * @param values     the list of values to find the maximum from
     * @param comparator the comparator used to compare values
     * @return the maximum value in the list, or null if the list is empty
     * @throws InterruptedException if an interrupt request is received while waiting for the threads to complete
     */
    @Override
    public <T> T maximum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
        return minimum(threads, values, comparator.reversed());
    }

    /**
     * Checks if all elements in the input list satisfy the given predicate using multiple threads.
     *
     * @param <T>       the type of the input values
     * @param threads   the number of threads to use
     * @param values    the list of values to check
     * @param predicate the predicate to apply to the values
     * @return true if all elements in the list satisfy the predicate, false otherwise
     * @throws InterruptedException if an interrupt request is received while waiting for the threads to complete
     */
    @Override
    public <T> boolean all(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return concurrent(
                threads,
                values,
                stream -> stream.allMatch(predicate),
                stream -> stream.allMatch(Boolean::booleanValue)
        );
    }

    /**
     * Checks if any element in the input list satisfies the given predicate using multiple threads.
     *
     * @param <T>       the type of the input values
     * @param threads   the number of threads to use
     * @param values    the list of values to check
     * @param predicate the predicate to apply to the values
     * @return true if any element in the list satisfies the predicate, false otherwise
     * @throws InterruptedException if an interrupt request is received while waiting for the threads to complete
     */
    @Override
    public <T> boolean any(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return !all(threads, values, predicate.negate());
    }

    /**
     * Returns the number of elements in the input list that satisfy the given predicate using multiple threads.
     *
     * @param <T>       the type of the input values
     * @param threads   the number of threads to use
     * @param values    the list of values to count
     * @param predicate the predicate to apply to the values
     * @return the number of elements in the list that satisfy the predicate
     * @throws InterruptedException if an interrupt request is received while waiting for the threads to complete
     */
    @Override
    public <T> int count(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return concurrent(
                threads,
                values,
                stream -> (int) stream.filter(predicate).count(),
                stream -> stream.mapToInt(Integer::intValue).sum()
        );
    }

    /**
     * Joins all elements in the input list into a single string using multiple threads.
     *
     * @param threads the number of threads to use
     * @param values  the list of values to join
     * @return a string containing all elements in the list, concatenated together in the order they appear
     * @throws InterruptedException if an interrupt request is received while waiting for the threads to complete
     */
    @Override
    public String join(int threads, List<?> values) throws InterruptedException {
        return concurrent(
                threads,
                values,
                stream -> stream.map(Object::toString).collect(Collectors.joining()),
                stream -> stream.collect(Collectors.joining())
        );
    }

    /**
     * Filters a given list of values based on a predicate function using multiple threads.
     *
     * @param <T>       the type of elements in the input list
     * @param threads   the number of threads to use for filtering
     * @param values    the list of values to filter
     * @param predicate the predicate function to filter the values
     * @return a list of filtered values in the same order as the input list
     * @throws InterruptedException if any of the threads are interrupted while executing
     */
    @Override
    public <T> List<T> filter(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return concurrent(
                threads,
                values,
                stream -> stream.filter(predicate).collect(Collectors.toList()),
                stream -> stream.flatMap(Collection::stream).collect(Collectors.toList())
        );
    }

    /**
     * Maps a given list of values to a list of values of another type using multiple threads.
     * using a specified number of threads.
     *
     * @param <T>     the type of elements in the input list
     * @param <U>     the type of elements in the output list
     * @param threads the number of threads to use for mapping
     * @param values  the list of values to map
     * @param f       the mapping function to transform the values
     * @return a list of mapped values in the same order as the input list
     * @throws InterruptedException if any of the threads are interrupted while executing
     */
    @Override
    public <T, U> List<U> map(int threads, List<? extends T> values, Function<? super T, ? extends U> f) throws InterruptedException {
        return concurrent(
                threads,
                values,
                stream -> stream.map(f).collect(Collectors.toList()),
                stream -> stream.flatMap(Collection::stream).collect(Collectors.toList())
        );
    }

    private <T, U> U concurrent(int threads, List<? extends T> values,
                                Function<Stream<? extends T>, ? extends U> reducer,
                                Function<Stream<? extends U>, ? extends U> collector) throws InterruptedException {
        threads = Math.min(threads, values.size());

        List<Stream<? extends T>> partitions = partitionList(values, threads);

        List<U> results;

        if (parallelMapper != null) {
            results = parallelMapper.map(reducer, partitions);
        } else {
            results = new ArrayList<>(Collections.nCopies(threads, null));
            List<Thread> threadList = new ArrayList<>(threads);

            for (int i = 0; i < threads; i++) {
                int finalI = i;

                Thread thread = new Thread(() -> {
                    Stream<? extends T> chunk = partitions.get(finalI);

                    U result = reducer.apply(chunk);

                    results.set(finalI, result);
                });

                threadList.add(thread);
            }

            threadList.forEach(Thread::start);

            InterruptedException exceptions = new InterruptedException();
            for (Thread thread : threadList) {
                try {
                    thread.join();
                } catch (InterruptedException e) {
                    exceptions.addSuppressed(e);
                }
            }

            if (exceptions.getSuppressed().length > 0) {
                throw exceptions;
            }
        }

        return collector.apply(results.stream());
    }

    private <T> List<Stream<? extends T>> partitionList(List<? extends T> list, int parts) {
        int listSize = list.size();
        int partitionSize = listSize / parts;
        int partitionRemainder = listSize % parts;
        List<Stream<? extends T>> partitions = new ArrayList<>(parts);

        int index = 0;
        for (int i = 0; i < parts; i++) {
            int size = partitionSize + (i < partitionRemainder ? 1 : 0);
            partitions.add(list.subList(index, index + size).stream());
            index += size;
        }

        return partitions;
    }
}
