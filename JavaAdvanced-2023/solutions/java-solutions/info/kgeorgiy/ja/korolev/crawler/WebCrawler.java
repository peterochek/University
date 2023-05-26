package info.kgeorgiy.ja.korolev.crawler;

import info.kgeorgiy.java.advanced.crawler.*;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.*;
import java.util.stream.IntStream;

public class WebCrawler implements Crawler {
    private final Set<String> visitedUrls = ConcurrentHashMap.newKeySet();
    private final Map<String, IOException> exceptions = new ConcurrentHashMap<>();
    private final ConcurrentLinkedQueue<String> toDo = new ConcurrentLinkedQueue<>();
    private final Downloader downloader;
    private final ExecutorService downloadPool, extractorPool;
    private final int perHost;
    private final Map<String, Host> hosts = new ConcurrentHashMap<>();
    private final Phaser phaser = new Phaser();

    public WebCrawler(Downloader downloader, int downloaders, int extractors, int perHost) {
        this.downloader = downloader;
        this.downloadPool = Executors.newFixedThreadPool(downloaders);
        this.extractorPool = Executors.newFixedThreadPool(extractors);
        this.perHost = perHost;
    }

    private static int getArg(final String[] args, final int idx) {
        if (idx < args.length) {
            try {
                return Integer.parseInt(args[idx]);
            } catch (final NumberFormatException exception) {
                return 1;
            }
        }

        return 1;
    }

    public static void main(final String[] args) {
        if (args == null || args[0] == null) {
            throw new IllegalArgumentException("Incorrect arguments. Example: WebCrawler url [depth [downloads [extractors [perHost]]]]");
        }

        final String url = args[0];
        final int depth = getArg(args, 1);
        final int downloaders = getArg(args, 2);
        final int extractors = getArg(args, 3);
        final int perHost = getArg(args, 4);

        try (final WebCrawler webCrawler = new WebCrawler(new CachingDownloader(1), downloaders, extractors, perHost)) {
            webCrawler.download(url, depth);
        } catch (final IOException exception) {
            exception.printStackTrace();
        }
    }

    @Override
    public Result download(final String url, final int depth) {
        toDo.add(url);

        IntStream.range(0, depth)
                .forEach(i -> {
                    final int finalI = depth - i;
                    phaser.register();
                    List<String> inProgress = new ArrayList<>(toDo);
                    toDo.clear();
                    inProgress.forEach(link -> recursiveDownload(link, finalI));
                    phaser.arriveAndAwaitAdvance();
                });

        visitedUrls.removeIf(exceptions::containsKey);

        return new Result(new ArrayList<>(visitedUrls), exceptions);
    }

    private void recursiveDownload(final String url, final int depth) {
        if (!visitedUrls.add(url)) {
            return;
        }

        try {
            final String hostName = URLUtils.getHost(url);

            final Host host = hosts.computeIfAbsent(hostName, key -> new Host(downloadPool, perHost));

            phaser.register();

            host.addJob(() -> {
                try {
                    traverse(url, depth);
                } catch (final IOException exception) {
                    exceptions.put(url, exception);
                } finally {
                    phaser.arrive();
                }
            });
        } catch (final MalformedURLException exception) {
            exceptions.put(url, exception);
        }
    }

    private void traverse(final String url, final int depth) throws IOException {
        final Document document = downloader.download(url);
        if (depth > 1) {
            phaser.register();
            extractorPool.submit(() -> {
                try {
                    List<String> urls = document.extractLinks();
                    toDo.addAll(urls);
                } catch (final IOException exception) {
                    exceptions.put(url, exception);
                } finally {
                    phaser.arrive();
                }
            });
        }
    }

    @Override
    public void close() {
        downloadPool.close();
        extractorPool.close();
    }
}