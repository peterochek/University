package info.kgeorgiy.ja.korolev.crawler;

import java.util.ArrayDeque;
import java.util.Queue;
import java.util.concurrent.ExecutorService;

public class Host {
    private final ExecutorService downloadPool;
    private final int perHost;
    private final Queue<Runnable> jobQueue = new ArrayDeque<>();
    private int activeDownloads = 0;

    public Host(final ExecutorService downloadPool, final int perHost) {
        this.downloadPool = downloadPool;
        this.perHost = perHost;
    }

    public synchronized void addJob(final Runnable job) {
        jobQueue.add(job);
        if (activeDownloads < perHost) {
            executeJob();
        }
    }

    public synchronized void executeJob() {
        final Runnable job = jobQueue.remove();

        final Runnable lambda = () -> {
            job.run();
            if (--activeDownloads < perHost) {
                executeJob();
            }
        };

        activeDownloads++;
        downloadPool.submit(lambda);
    }
}
