package info.kgeorgiy.ja.korolev.implementor;

import java.lang.reflect.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import static info.kgeorgiy.ja.korolev.implementor.Implementor.getImplClassName;

/**
 * The {@code TokenCode} class provides a way to generate a string containing code for a given {@code Class} token.
 * <p>
 * The generated code contains an implementation of all abstract methods and constructors of the given token.
 */

public class TokenCode {

    /**
     * The line separator used for the generated code.
     */
    private static final String LINE_BREAK = System.lineSeparator();
    /**
     * The string representation of an open curly brace used for the generated code.
     */
    private static final String OPEN_CURLY_BRACE = " {";
    /**
     * The string representation of a close curly brace used for the generated code.
     */
    private static final String CLOSE_CURLY_BRACE = "}";
    /**
     * The string representation of a tab character used for the generated code.
     */
    private static final String TAB = "\t";
    /**
     * The initial depth for indentation of the generated code.
     */
    private static final int INIT_DEPTH = 1;

    /**
     * Returns the string representation of code that implements all abstract methods and constructors of the given
     * {@code token} class or interface.
     *
     * @param token the class or interface for which to generate the code
     * @return the generated code as a string
     */
    public static String getTokenCode(Class<?> token) {
        return String.join(
                LINE_BREAK,
                getPackageName(token),
                getClassHeader(token) + OPEN_CURLY_BRACE,
                getClassBody(token, INIT_DEPTH),
                CLOSE_CURLY_BRACE
        );
    }

    /**
     * Returns the package statement for the given {@code token}.
     *
     * @param token the class or interface for which to generate the code
     * @return the package statement as a string
     */
    private static String getPackageName(Class<?> token) {
        if (!token.getPackageName().isEmpty()) {
            return "package " + token.getPackageName() + ";";
        }
        return "";
    }

    /**
     * Returns the class header for the given {@code token}.
     *
     * @param token the class or interface for which to generate the code
     * @return the class header as a string
     */
    private static String getClassHeader(Class<?> token) {
        return String.join(
                " ",
                "public class",
                getImplClassName(token),
                token.isInterface() ? "implements" : "extends",
                token.getCanonicalName()
        );
    }

    /**
     * Returns the body of the class or interface for the given {@code token} with all constructors and methods
     * implemented.
     *
     * @param token the class or interface for which to generate the code
     * @param depth the current depth of indentation
     * @return the class or interface body as a string
     */
    private static String getClassBody(Class<?> token, int depth) {
        return concat(getConstructors(token, depth), LINE_BREAK, getMethods(token, depth));
    }

    /**
     * Returns the string representation of all public constructors of the given {@code token}.
     *
     * @param token the class or interface for which to generate the code
     * @param depth the current depth of indentation
     * @return the constructors as a string
     */
    private static String getConstructors(Class<?> token, int depth) {
        return Arrays
                .stream(token.getDeclaredConstructors())
                .filter(constructor -> !Modifier.isPrivate(constructor.getModifiers()))
                .map(constructor -> TokenCode.getConstructor(constructor, depth))
                .collect(Collectors.joining(LINE_BREAK));
    }

    /**
     * Returns the string representation of the given {@code constructor}.
     *
     * @param constructor the constructor for which to generate the code
     * @param depth       the current depth of indentation
     * @return the constructor as a string
     */
    private static String getConstructor(Constructor<?> constructor, int depth) {
        return concat(
                TAB.repeat(depth),
                "public ",
                getImplClassName(constructor.getDeclaringClass()),
                getParameters(constructor),
                " ",
                getExceptions(constructor),
                OPEN_CURLY_BRACE,
                LINE_BREAK,
                TAB.repeat(depth + 1),
                Arrays
                        .stream(constructor.getParameters())
                        .map(Parameter::getName)
                        .collect(Collectors.joining(", ", "super(", ");")),
                LINE_BREAK,
                TAB.repeat(depth),
                CLOSE_CURLY_BRACE
        );
    }

    /**
     * Returns the string representation of all abstract methods of the given {@code token}.
     *
     * @param token the class or interface for which to generate the code
     * @param depth the current depth of indentation
     * @return the methods as a string
     */
    private static String getMethods(Class<?> token, int depth) {
        List<Method> treeMethods = new ArrayList<>(Arrays.asList(token.getMethods()));

        for (Class<?> clazz = token; clazz != null; clazz = clazz.getSuperclass()) {
            treeMethods.addAll(Arrays.asList(clazz.getDeclaredMethods()));
        }

        return treeMethods
                .stream()
                .map(Comparer::new)
                .distinct()
                .map(Comparer::getMethod)
                .filter(method -> Modifier.isAbstract(method.getModifiers()))
                .map(method -> TokenCode.getMethod(method, depth))
                .collect(Collectors.joining(LINE_BREAK));
    }

    /**
     * Returns a string representation of the given method's signature and body.
     *
     * @param method The method to generate a string representation for.
     * @param depth  The depth to indent the generated code.
     * @return A string representation of the given method's signature and body.
     */
    private static String getMethod(Method method, int depth) {
        return concat(
                TAB.repeat(depth),
                getMethodHeader(method),
                OPEN_CURLY_BRACE,
                LINE_BREAK,
                getMethodBody(method, depth + 1),
                LINE_BREAK,
                TAB.repeat(depth),
                CLOSE_CURLY_BRACE
        );
    }

    /**
     * Returns a string representation of the given method's header, including the access modifier, return type,
     * method name, parameters, and exceptions.
     *
     * @param method The method to generate a header for.
     * @return A string representation of the given method's header.
     */
    private static String getMethodHeader(Method method) {
        return String.join(
                " ",
                "public",
                method.getReturnType().getCanonicalName(),
                method.getName(),
                getParameters(method),
                getExceptions(method)
        );
    }

    /**
     * Returns a string representation of the given method's parameter list.
     *
     * @param executable The executable to generate a parameter list for.
     * @return A string representation of the given method's parameter list.
     */
    private static String getParameters(Executable executable) {
        return Arrays
                .stream(executable.getParameters())
                .map(parameter ->
                        parameter.getType().getCanonicalName() + " " + parameter.getName()
                )
                .collect(Collectors.joining(", ", "(", ")"));
    }

    /**
     * Returns a string representation of the given method's exception list.
     *
     * @param executable The executable to generate an exception list for.
     * @return A string representation of the given method's exception list.
     */
    private static String getExceptions(Executable executable) {
        if (executable.getExceptionTypes().length != 0) {
            return (
                    "throws " +
                            Arrays
                                    .stream(executable.getExceptionTypes())
                                    .map(Class::getCanonicalName)
                                    .collect(Collectors.joining(", "))
            );
        }

        return "";
    }

    /**
     * Returns a string representation of the given method's body.
     *
     * @param method The method to generate a body for.
     * @param depth  The depth to indent the generated code.
     * @return A string representation of the given method
     */
    private static String getMethodBody(Method method, int depth) {
        return concat(TAB.repeat(depth), "return ", getDefaultValue(method.getReturnType()), ";");
    }

    /**
     * Returns a default value for the specified primitive or object type.
     * If the type is a primitive, returns 0 for numeric types, false for boolean, and empty string for void.
     * If the type is an object, returns null.
     *
     * @param type the type for which to return a default value.
     * @return a default value for the specified type.
     */
    private static String getDefaultValue(Class<?> type) {
        if (type.isPrimitive()) {
            if (type.equals(boolean.class)) {
                return "false";
            } else if (type.equals(void.class)) {
                return "";
            } else {
                return "0";
            }
        } else {
            return "null";
        }
    }

    /**
     * Concatenates the specified strings into a single string.
     *
     * @param strings the strings to concatenate.
     * @return the concatenated string.
     */
    private static String concat(String... strings) {
        return String.join("", strings);
    }
}
