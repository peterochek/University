package info.kgeorgiy.ja.korolev.implementor;

import java.lang.reflect.Method;
import java.util.Arrays;

/**
 * The {@code Comparer} class is a utility class for comparing methods.
 */
public class Comparer {
    /**
     * The method to compare.
     */
    private final Method method;

    /**
     * Constructs a {@code Comparer} object with the given {@link Method}.
     *
     * @param method the method to compare.
     */
    Comparer(Method method) {
        this.method = method;
    }

    /**
     * Returns the method of this {@code Comparer} object.
     *
     * @return the method of this {@code Comparer} object.
     */
    public Method getMethod() {
        return method;
    }

    /**
     * Compares this {@code Comparer} object to the specified object.
     *
     * @param object the object to compare.
     * @return true if the given object is equal to this {@code Comparer} object, false otherwise.
     */
    @Override
    public boolean equals(Object object) {
        if (getClass() != object.getClass()) {
            return false;
        }

        Comparer comparer = (Comparer) object;

        return Arrays.equals(
                method.getParameterTypes(), comparer.method.getParameterTypes()
        ) && method.getName().equals(comparer.method.getName());
    }

    /**
     * Returns the hash code value for this {@code Comparer} object.
     *
     * @return the hash code value for this {@code Comparer} object.
     */
    @Override
    public int hashCode() {
        return method.getName().hashCode() ^ Arrays.hashCode(method.getParameterTypes());
    }
}