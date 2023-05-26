package info.kgeorgiy.ja.korolev.implementor;

import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;

/**
 * A helper class for cleaning up generated files and directories.
 */
public class FileCleaner extends SimpleFileVisitor<Path> {
    /**
     * Deletes a directory after visiting all its entries.
     *
     * @param dir the directory to visit
     * @param e   exception encountered while accessing the directory
     * @return {@link FileVisitResult#CONTINUE} to continue processing directory entries
     * @throws IOException if an I/O error occurs while deleting the directory
     */
    @Override
    public FileVisitResult postVisitDirectory(Path dir, IOException e) throws IOException {
        Files.delete(dir);
        return FileVisitResult.CONTINUE;
    }

    /**
     * Deletes a file.
     *
     * @param file  the file to delete
     * @param attrs the file attributes
     * @return {@link FileVisitResult#CONTINUE} to continue processing directory entries
     * @throws IOException if an I/O error occurs while deleting the file
     */
    @Override
    public FileVisitResult visitFile(Path file, BasicFileAttributes attrs) throws IOException {
        Files.delete(file);
        return FileVisitResult.CONTINUE;
    }
}