package info.kgeorgiy.ja.korolev.concurrent;

import java.util.ArrayDeque;
import java.util.List;
import java.util.Queue;

/**
 * The {@code ParallelQueue} class represents a queue of jobs that can be executed in parallel by multiple threads.
 * <p>
 * It uses a job queue to store the jobs to be executed and a list of threads to execute the jobs.
 */
public class ParallelQueue {
    private final Queue<Runnable> jobQueue = new ArrayDeque<>();
    private final Runnable job = () -> {
        while (!Thread.interrupted()) {
            Runnable job;
            synchronized (jobQueue) {
                job = takeJob();
                if (job == null) {
                    return;
                }
            }
            job.run();
        }
    };

    /**
     * Constructs a new {@code ParallelQueue} object with the specified number of threads.
     *
     * @param threadList the list of threads that will execute the jobs
     * @param threads    the number of threads to be used for parallel execution
     */
    public ParallelQueue(final List<Thread> threadList, final int threads) {
        for (int i = 0; i < threads; i++) {
            threadList.add(new Thread(job));
        }

        threadList.forEach(Thread::start);
    }

    private Runnable takeJob() {
        while (jobQueue.isEmpty()) {
            try {
                jobQueue.wait();
            } catch (InterruptedException ignored) {
                return null;
            }
        }

        return jobQueue.poll();
    }

    /**
     * Adds a new job to the queue.
     *
     * @param job the job to be added to the queue
     */
    public void addJob(final Runnable job) {
        synchronized (jobQueue) {
            jobQueue.add(job);
            jobQueue.notifyAll();
        }
    }
}
