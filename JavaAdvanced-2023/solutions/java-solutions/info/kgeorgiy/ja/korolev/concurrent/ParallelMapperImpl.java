package info.kgeorgiy.ja.korolev.concurrent;

import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

public class ParallelMapperImpl implements ParallelMapper {
    private final List<Thread> threadList = new ArrayList<>();
    private final ParallelQueue queue;

    public ParallelMapperImpl(final int threads) {
        queue = new ParallelQueue(threadList, threads);
    }

    @Override
    public <T, R> List<R> map(final Function<? super T, ? extends R> f, final List<? extends T> args) throws InterruptedException {
        ParallelList<R> results = new ParallelList<>(args.size());

        RuntimeException exceptions = new RuntimeException();

        for (int i = 0; i < args.size(); i++) {
            int finalI = i;
            queue.addJob(
                    () -> {
                        try {
                            results.set(finalI, f.apply(args.get(finalI)));
                        } catch (RuntimeException e) {
                            synchronized (exceptions) {
                                exceptions.addSuppressed(e);
                            }
                        }
                    }
            );
        }

        if (exceptions.getSuppressed().length > 0) {
            throw exceptions;
        }

        return results.getMapped();
    }

    @Override
    public void close() {
        threadList.forEach(Thread::interrupt);

        InterruptedException exceptions = new InterruptedException();
        for (Thread thread : threadList) {
            try {
                thread.join();
            } catch (InterruptedException e) {
                exceptions.addSuppressed(e);
            }
        }

        if (exceptions.getSuppressed().length > 0) {
            exceptions.printStackTrace();
        }
    }
}
