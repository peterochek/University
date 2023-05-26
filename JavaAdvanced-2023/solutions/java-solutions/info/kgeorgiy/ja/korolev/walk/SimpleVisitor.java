package info.kgeorgiy.ja.korolev.walk;

import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;

public class SimpleVisitor extends SimpleFileVisitor<Path> {
    private final Hasher hasher;

    private final VisitMode visitMode;

    public SimpleVisitor(final Hasher hasher, final VisitMode visitMode) {
        this.visitMode = visitMode;
        this.hasher = hasher;
    }

    @Override
    public FileVisitResult visitFile(final Path file, final BasicFileAttributes attrs) throws IOException {
        hasher.writePathHash(file);

        return FileVisitResult.CONTINUE;
    }

    @Override
    public FileVisitResult preVisitDirectory(final Path dir, final BasicFileAttributes attrs) throws IOException {
        if (visitMode == VisitMode.RECURSIVE) {
            return FileVisitResult.CONTINUE;
        }

        hasher.writeZeroHash(dir.toString());
        return FileVisitResult.TERMINATE;
    }

    @Override
    public FileVisitResult visitFileFailed(final Path file, final IOException e) throws IOException {
        System.err.println(e.getMessage());
        hasher.writeZeroHash(file.toString());
        return FileVisitResult.CONTINUE;
    }
}