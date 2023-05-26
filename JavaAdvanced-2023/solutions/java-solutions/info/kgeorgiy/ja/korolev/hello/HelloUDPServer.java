package info.kgeorgiy.ja.korolev.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class HelloUDPServer implements HelloServer {
    private DatagramSocket datagramSocket;
    private ExecutorService threadPool;
    private final ExecutorService executor = Executors.newSingleThreadExecutor();

    @Override
    public void start(int port, int threads) {
        try {
            datagramSocket = new DatagramSocket(port);
            threadPool = Executors.newFixedThreadPool(threads);

            Runnable job = createJob();

            executor.submit(job);
        } catch (final SocketException socketException) {
            System.err.println("Error occurred while creating socket: " + socketException.getMessage());
        }
    }

    private Runnable createJob() {
        return () -> {
            while (!datagramSocket.isClosed() && !Thread.currentThread().isInterrupted()) {
                try {
                    int size = datagramSocket.getReceiveBufferSize();

                    final DatagramPacket fromClientRaw = new DatagramPacket(new byte[size], size);

                    datagramSocket.receive(fromClientRaw);

                    threadPool.submit(() -> {
                        try {
                            final String fromClient = new String(
                                    fromClientRaw.getData(), fromClientRaw.getOffset(), fromClientRaw.getLength(), StandardCharsets.UTF_8
                            );

                            final String helloToClient = "Hello, " + fromClient;

                            final DatagramPacket toClient = new DatagramPacket(
                                    helloToClient.getBytes(StandardCharsets.UTF_8), helloToClient.length(),
                                    fromClientRaw.getSocketAddress()
                            );

                            datagramSocket.send(toClient);
                        } catch (final IOException ioException) {
                            System.err.println("Error occurred while sending: " + ioException.getMessage());
                        }
                    });
                } catch (final IOException ioException) {
                    System.err.println("Error occurred while receiving: " + ioException.getMessage());
                }
            }
        };
    }

    @Override
    public void close() {
        datagramSocket.close();

        threadPool.close();
        executor.close();
    }

    private static int getArg(final String[] args, final int idx) {
        try {
            return Integer.parseInt(args[idx]);
        } catch (final NumberFormatException numberFormatException) {
            throw new IllegalArgumentException("Incorrect argument: " + args[idx]);
        }
    }

    /**
     * Main function for {@link HelloUDPServer}.
     *
     * @param args An array of command-line arguments.
     */
    public static void main(final String[] args) {
        if (args == null || Arrays.stream(args).anyMatch(Objects::isNull)) {
            throw new IllegalArgumentException("Incorrect arguments");
        }

        try {
            final int port = getArg(args, 0);
            final int threads = getArg(args, 1);

            try (HelloUDPServer server = new HelloUDPServer()) {
                server.start(port, threads);
            }
        } catch (IllegalArgumentException illegalArgumentException) {
            System.err.println("Incorrect args passed: " + illegalArgumentException.getMessage());
        }
    }
}
