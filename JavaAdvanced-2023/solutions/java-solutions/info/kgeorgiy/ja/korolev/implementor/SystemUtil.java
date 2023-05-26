package info.kgeorgiy.ja.korolev.implementor;

import info.kgeorgiy.java.advanced.implementor.ImplerException;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.CodeSource;
import java.util.jar.Attributes;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.zip.ZipEntry;

import static info.kgeorgiy.ja.korolev.implementor.Implementor.getImplClassName;

/**
 * This class provides utility methods for implementing classes and working with files.
 */
public class SystemUtil {

    /**
     * The file extension for Java source files.
     */
    public static final String JAVA = ".java";
    /**
     * The file extension for Java class files.
     */
    public static final String CLASS = ".class";

    /**
     * Resolves the path to the directory where the generated implementation should be saved.
     *
     * @param token the class to implement.
     * @param root  the root directory.
     * @return the path to the directory where the implementation should be saved.
     */
    public static Path resolveDirPath(Class<?> token, Path root) {
        return root
                .resolve(token.getPackageName().replace(".", File.separator));
    }

    /**
     * Resolves the path to the file where the generated implementation should be saved.
     *
     * @param token         the class to implement.
     * @param root          the root directory.
     * @param fileExtension the file fileExtension.
     * @return the path to the file where the implementation should be saved.
     */
    private static Path resolveFilePath(Class<?> token, Path root, String fileExtension) {
        return resolveDirPath(token, root).resolve(getImplClassName(token) + fileExtension);
    }

    /**
     * Compiles the generated implementation.
     *
     * @param token the class to implement.
     * @param dir   the dir where the generated implementation is saved.
     * @throws ImplerException if an error occurs while compiling.
     */
    public static void compile(Class<?> token, Path dir) throws ImplerException {
        String cp;
        CodeSource codeSource = token.getProtectionDomain().getCodeSource();
        if (codeSource == null) {
            cp = "";
        } else {
            try {
                cp = Path.of(codeSource.getLocation().toURI()).toString();
            } catch (URISyntaxException e) {
                throw new ImplerException("Error occurred URL to URI", e);
            }
        }

        String file = resolveFilePath(token, dir, JAVA).toString();
        String[] compilerArgs = {"-encoding", "utf8", "-cp", cp, file};
        if (codeSource == null) {
            compilerArgs = new String[]{"-encoding", "utf8", "--patch-module", String.format("%s=%s", token.getModule().getName(), dir),
                    resolveFilePath(token, dir, JAVA).toString()};
        }
        JavaCompiler systemCompiler = ToolProvider.getSystemJavaCompiler();
        if (systemCompiler.run(null, null, null, compilerArgs) != 0) {
            throw new ImplerException("Error occurred while compiling");
        }
    }

    /**
     * Creates a JAR file containing the generated implementation.
     *
     * @param token     the class to implement.
     * @param jarFile   the path to the JAR file to create.
     * @param directory the directory where the generated implementation is saved.
     * @throws ImplerException if an error occurs while creating the JAR file.
     */
    public static void createJar(Class<?> token, Path jarFile, Path directory) throws ImplerException {
        try (JarOutputStream jarOutputStream = new JarOutputStream(Files.newOutputStream(jarFile), manifest())) {
            jarOutputStream.putNextEntry(new ZipEntry(getFullSpecifierPath(token)));

            Files.copy(resolveFilePath(token, directory, CLASS), jarOutputStream);
        } catch (IOException e) {
            throw new ImplerException("Error occurred while creating jar file", e);
        }
    }

    private static String getFullSpecifierPath(Class<?> token) {
        String fullSpecifier = (token.getPackageName() + "." + getImplClassName(token));
        Path parent = Path.of(fullSpecifier.replace('.', '/')).getParent();

        Path filePath = parent.resolve(getImplClassName(token) + CLASS);

        return filePath.toString().replace('\\', '/');  // Windows-specific
    }

    /**
     * Creates a new manifest for the JAR file.
     *
     * @return a new manifest for the JAR file.
     */
    private static Manifest manifest() {
        Manifest manifest = new Manifest();
        Attributes attributes = manifest.getMainAttributes();
        attributes.put(Attributes.Name.MANIFEST_VERSION, "1.0");

        return manifest;
    }
}
