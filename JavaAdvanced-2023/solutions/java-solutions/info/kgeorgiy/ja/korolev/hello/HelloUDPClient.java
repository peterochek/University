package info.kgeorgiy.ja.korolev.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.io.IOException;
import java.net.*;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

public class HelloUDPClient implements HelloClient {
    @Override
    public void run(String host, int port, String prefix, int threads, int requests) {
        final ExecutorService threadPool = Executors.newFixedThreadPool(threads);
        final SocketAddress socketAddress = new InetSocketAddress(host, port);

        for (int threadIdx = 1; threadIdx <= threads; threadIdx++) {
            final Runnable job = createJob(socketAddress, prefix, threadIdx, requests);
            threadPool.submit(job);
        }

        terminatePool(threadPool);
    }

    private Runnable createJob(final SocketAddress socketAddress, final String prefix, final int threadIdx, final int requests) {
        return new Thread(() -> {
            try (final DatagramSocket datagramSocket = new DatagramSocket()) {
                datagramSocket.setSoTimeout(1000);

                for (int requestIdx = 1; requestIdx <= requests; requestIdx++) {
                    final String generated = prefix + threadIdx + "_" + requestIdx;

                    while (true) {
                        try {
                            final DatagramPacket toServer = new DatagramPacket(
                                    generated.getBytes(StandardCharsets.UTF_8), generated.length(), socketAddress
                            );

                            datagramSocket.send(toServer);

                            try {
                                int size = datagramSocket.getReceiveBufferSize();

                                final DatagramPacket fromServerRaw = new DatagramPacket(new byte[size], size);

                                datagramSocket.receive(fromServerRaw);

                                String fromServer = new String(
                                        fromServerRaw.getData(), fromServerRaw.getOffset(), fromServerRaw.getLength(), StandardCharsets.UTF_8
                                );

                                if (valid(fromServer, prefix, threadIdx, requestIdx)) {
                                    System.out.println(fromServer);
                                    break;
                                }
                            } catch (final IOException ioException) {
                                System.err.println("Error occurred while receiving: " + ioException.getMessage());
                            }
                        } catch (final IOException ioException) {
                            System.err.println("Error occurred while sending: " + ioException.getMessage());
                        }
                    }
                }
            } catch (final SocketException socketException) {
                System.err.println("Error occurred while creating socket: " + socketException.getMessage());
            }
        });
    }

    private static boolean valid(final String fromServer, final String prefix, final int threadIdx, final int requestIdx) {
        String helloPrefix = "Hello, " + prefix;

        if (fromServer.startsWith(helloPrefix)) {
            String substr = fromServer.substring(helloPrefix.length());
            String[] nParts = substr.split("_");

            try {
                int parsedThread = Integer.parseInt(nParts[0]);
                int parsedRequest = Integer.parseInt(nParts[1]);

                return parsedThread == threadIdx && parsedRequest == requestIdx;
            } catch (NumberFormatException numberFormatException) {
                return false;
            }
        } else {
            return false;
        }
    }

    private static void terminatePool(final ExecutorService pool) {
        pool.shutdown();
        try {
            if (!pool.awaitTermination(10, TimeUnit.SECONDS)) {
                pool.shutdownNow();
                if (!pool.awaitTermination(10, TimeUnit.SECONDS)) {
                    System.err.println("Pool is still running!");
                }
            }
        } catch (final InterruptedException interruptedException) {
            pool.shutdownNow();
            Thread.currentThread().interrupt();
        }
    }

    private static int getArg(final String[] args, final int idx) {
        try {
            return Integer.parseInt(args[idx]);
        } catch (final NumberFormatException numberFormatException) {
            throw new IllegalArgumentException("Incorrect argument: " + args[idx]);
        }
    }

    /**
     * Main function for {@link HelloUDPClient}.
     *
     * @param args An array of command-line arguments.
     */
    public static void main(final String[] args) {
        if (args == null || Arrays.stream(args).anyMatch(Objects::isNull)) {
            System.err.println("Incorrect arguments");
            return;
        }

        try {
            final String host = args[0];
            final int port = getArg(args, 1);
            final String prefix = args[2];
            final int threads = getArg(args, 3);
            final int requests = getArg(args, 4);

            HelloUDPClient client = new HelloUDPClient();
            client.run(host, port, prefix, threads, requests);
        } catch (IllegalArgumentException illegalArgumentException) {
            System.err.println("Incorrect args passed: " + illegalArgumentException.getMessage());
        }
    }
}