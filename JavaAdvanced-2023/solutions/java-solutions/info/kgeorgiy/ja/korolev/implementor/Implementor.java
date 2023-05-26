package info.kgeorgiy.ja.korolev.implementor;

import info.kgeorgiy.java.advanced.implementor.ImplerException;
import info.kgeorgiy.java.advanced.implementor.JarImpler;

import java.io.BufferedWriter;
import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static info.kgeorgiy.ja.korolev.implementor.SystemUtil.*;

/**
 * This class provides an implementation of the {@link JarImpler} interface for generating an implementation of
 * a given class or interface. The implementation is generated as a Java source file, and optionally compiled and
 * packaged as a JAR file.
 */
public class Implementor implements JarImpler {
    /**
     * Returns the simple name of the implementation class for the given token.
     *
     * @param token the class or interface to implement
     * @return the simple name of the implementation class
     */
    public static String getImplClassName(Class<?> token) {
        return token.getSimpleName() + "Impl";
    }

    /**
     * Parses the command line arguments and generates an implementation of the specified class or interface.
     * <p>
     * If the first argument is "-jar", the implementation is compiled and packaged as a JAR file.
     * Otherwise, the implementation is generated as a Java source file.
     *
     * @param args the command line arguments, which must be of the form "{@code token root}" or "-jar {@code token root}"
     */
    public static void main(String[] args) {
        if (args == null || args.length < 2 || args[0] == null || args[1] == null) {
            System.err.println("Incorrect arguments. Example: java [-jar] token root");
            return;
        }

        List<String> argsList = new ArrayList<>(Arrays.asList(args));

        boolean jarMode = false;

        if (argsList.contains("-jar")) {
            jarMode = true;
            argsList.remove(0);
        }

        Class<?> token;

        try {
            token = Class.forName(argsList.get(0));
        } catch (ClassNotFoundException e) {
            System.err.println("Invalid token error " + argsList.get(0) + ". " + e.getMessage());
            return;
        }

        Path root;

        try {
            root = Path.of(argsList.get(1));
        } catch (InvalidPathException e) {
            System.err.println("Invalid path error: " + e.getMessage());
            return;
        }

        Implementor implementor = new Implementor();

        try {
            if (jarMode) {
                implementor.implementJar(token, root);
            } else {
                implementor.implement(token, root);
            }
        } catch (ImplerException e) {
            System.err.println("Error occurred while implementing " + token.getCanonicalName() + ": " + e.getMessage());
        }
    }

    /**
     * Generates an implementation of the specified class or interface and writes it to the specified directory.
     *
     * @param token the class or interface to implement
     * @param root  the root directory to write the implementation to
     * @throws ImplerException if an error occurs during implementation
     */
    @Override
    public void implement(Class<?> token, Path root) throws ImplerException {
        int modifiers = token.getModifiers();
        if (token == Enum.class || Modifier.isFinal(modifiers) || Modifier.isPrivate(modifiers)) {
            throw new ImplerException(
                    "Can't implement current token: " + token.getCanonicalName()
            );
        }

        List<Constructor<?>> constructors = new ArrayList<>(Arrays.asList(token.getDeclaredConstructors()));
        constructors.removeIf(c -> Modifier.isPrivate(c.getModifiers()));

        if (!token.isInterface() && constructors.isEmpty()) {
            throw new ImplerException(
                    "Can't implement class with all private constructors: " +
                            token.getCanonicalName()
            );
        }

        Path path = resolveDirPath(token, root);

        try {
            Files.createDirectories(path);

            Path file = path.resolve(getImplClassName(token) + ".java");

            try (
                    BufferedWriter writer = Files.newBufferedWriter(file)
            ) {
                writer.write(TokenCode.getTokenCode(token));
            }
        } catch (IOException e) {
            throw new ImplerException("Error occurred while creating token file: " + e.getMessage(), e);
        }
    }

    /**
     * Generates an implementation of the specified class or interface, compiles it, and writes it to a JAR file
     * with the specified name.
     *
     * @param token   the class or interface to implement
     * @param jarFile the name of the JAR file to write the implementation to
     * @throws ImplerException if an error occurs during implementation or packaging
     */
    @Override
    public void implementJar(Class<?> token, Path jarFile) throws ImplerException {
        Path tmpDir;
        try {

            tmpDir = Files.createTempDirectory(Path.of("").toAbsolutePath(), "tmp");
        } catch (IOException e) {
            throw new ImplerException("Error occurred while creating tmp dir", e);
        }

        FileCleaner fileCleaner = new FileCleaner();

        try {
            implement(token, tmpDir);
            compile(token, tmpDir);
            createJar(token, jarFile, tmpDir);

            Files.walkFileTree(tmpDir, fileCleaner);
        } catch (IOException e) {
            throw new ImplerException("Error occurred while removing tmp dirs", e);
        }
    }
}
