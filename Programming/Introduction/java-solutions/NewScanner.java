import java.io.*;
import java.nio.charset.Charset;
import java.util.function.Function;

public class NewScanner implements AutoCloseable {
    private final Reader reader;
    private final StringBuilder sb = new StringBuilder();
    public int numLines = 0;

    NewScanner(InputStream stream, Charset charset) {
        reader = new BufferedReader(new InputStreamReader(stream, charset));
    }

    NewScanner(String line, Charset charset) {
        InputStream in = new ByteArrayInputStream(line.getBytes(charset));
        reader = new BufferedReader(new InputStreamReader(in));
    }

    public boolean hasNext() {
        boolean b = false;
        try {
            b = reader.ready();
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }
        return b;
    }

    public String next(Function<Character, Boolean> isGood) {
        try {
            sb.setLength(0);

            while (true) {
                int code = reader.read();

                char ch = (char) code;

                if (code == -1) {
                    return sb.toString();
                }

                if (isGood.apply(ch)) {
                    sb.append(ch);
                } else {
                    if (ch == '\n' || ch == '\r' || ch == '\u0085' || ch == '\u2028' || ch == '\u2029') {
                        if (ch == '\r') {
                            reader.mark(3);
                            char nextPos = (char) reader.read();
                            if (nextPos != '\n') {
                                reader.reset();
                            }
                        }
                        numLines++;
                        if (sb.length() > 0) {
                            return sb.toString();
                        }
                    }
                    if (sb.length() > 0) {
                        return sb.toString();
                    }
                    break;
                }
            }
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }

        return null;
    }

    @Override
    public void close() {
        try {
            reader.close();
        } catch (IOException e) {
            System.out.println("I/O Exception: " + e.getMessage());
        }
    }
}