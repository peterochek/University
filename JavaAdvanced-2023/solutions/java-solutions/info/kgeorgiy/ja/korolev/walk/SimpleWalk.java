package info.kgeorgiy.ja.korolev.walk;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

public class SimpleWalk {

    private final VisitMode visitMode;

    SimpleWalk(final VisitMode visitMode) {
        this.visitMode = visitMode;
    }

    private static boolean validArg(final String arg, final String identifier) {
        if (arg == null) {
            System.err.println(identifier + " argument is null");
            return false;
        }

        return true;
    }

    private static boolean processArgs(final String[] args, final String className) {
        if (args == null || args.length != 2) {
            System.err.println("Incorrect files declaration. Example: java " + className +
                    "Walk \"input file\" \"output file\"");
            return false;
        }

        return validArg(args[0], "first") && validArg(args[1], "second");
    }

    private void traverse(final Path inputFile, final Path outputFile) {
        try (final BufferedReader files = Files.newBufferedReader(inputFile)) {
            try (final BufferedWriter bufWriter = Files.newBufferedWriter(outputFile)) {
                final MessageDigest messageDigest = MessageDigest.getInstance("SHA-256");
                final Hasher hasher = new Hasher(bufWriter, messageDigest);

                while (true) {
                    final String fileName;
                    try {
                        if ((fileName = files.readLine()) == null) {
                            break;
                        }
                    } catch (final IOException e) {
                        System.err.println("Error occurred while reading: " + e.getMessage());
                        break;
                    }

                    try {
                        Files.walkFileTree(Path.of(fileName), new SimpleVisitor(hasher, visitMode));
                    } catch (final InvalidPathException e) {
                        System.err.println("Invalid path while traversing file tree: " + e.getMessage());
                        hasher.writeZeroHash(fileName);
                    } catch (final IOException e) {
                        System.err.println("Error occurred while traversing file tree: " + e.getMessage());
                        break;
                    }
                }
            } catch (final IOException e) {
                System.err.println("Output file error: " + e.getMessage());
            }
        } catch (final IOException e) {
            System.err.println("Input file error: " + e.getMessage());
        } catch (final NoSuchAlgorithmException e) {
            System.err.println("SHA-256 algo is not presented on platform: " + e.getMessage());
        }
    }

    public void run(final String[] args, final String className) {
        if (!processArgs(args, className)) {
            return;
        }

        final Path inputFile = getPath(args[0], "Input");
        final Path outputFile = getPath(args[1], "Output");

        if (outputFile == null || inputFile == null) {
            return;
        }

        try {
            if (outputFile.getParent() != null) {
                Files.createDirectory(outputFile.getParent());
            }
        } catch (final FileAlreadyExistsException e) {
            System.err.println("Continuing...");
        } catch (final IOException e) {
            System.err.println("Couldn't create directory for output file: " + e.getMessage());
        }

        traverse(inputFile, outputFile);
    }

    private Path getPath(final String arg, final String s) {
        try {
            return Path.of(arg);
        } catch (final InvalidPathException e) {
            System.err.println(s + " file doesn't exist: " + e.getMessage());
            return null;
        }
    }
}
