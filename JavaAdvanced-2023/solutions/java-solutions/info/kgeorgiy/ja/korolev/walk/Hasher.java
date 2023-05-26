package info.kgeorgiy.ja.korolev.walk;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.util.HexFormat;

final public class Hasher {
    private static final String ERROR_HASH = "0".repeat(64);
    private final static byte[] buffer = new byte[1024];
    private final static HexFormat hexFormat = HexFormat.of();
    private final Writer writer;
    private final MessageDigest messageDigest;

    Hasher(final Writer writer, final MessageDigest messageDigest) {
        this.writer = writer;
        this.messageDigest = messageDigest;
    }

    private String hash(final Path path) {
        try (final BufferedInputStream bufferedInputStream = new BufferedInputStream(Files.newInputStream(path))) {
            messageDigest.reset();
            int charCount;
            while ((charCount = bufferedInputStream.read(buffer)) >= 0) {
                messageDigest.update(buffer, 0, charCount);
            }
            return hexFormat.formatHex(messageDigest.digest());
        } catch (final IOException e) {
            System.err.println("Error occurred while creating InputStream: " + e.getMessage());
            return ERROR_HASH;
        }
    }

    private void writeHash(final String hash, final String path) throws IOException {
        writer.write(String.format("%s %s%n", hash, path));
    }

    public void writePathHash(final Path path) throws IOException {
        writeHash(hash(path), path.toString());
    }

    public void writeZeroHash(final String path) throws IOException {
        writeHash(ERROR_HASH, path);
    }
}
