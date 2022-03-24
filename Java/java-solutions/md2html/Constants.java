package md2html;

import java.util.HashMap;
import java.util.Map;

public final class Constants {
    final static String EMPHASIS_STAR = "emphasisStar";
    final static String EMPHASIS_UNDERLINE = "emphasisUnderline";
    final static String STRONG = "strong";
    final static String STRIKEOUT = "strikeout";
    final static String CODE = "code";
    final static String PRE = "pre";
    final static String ESCAPING = "escaping";

    final static Map<Character, String> mdHtml = new HashMap<>(Map.of(
            '*', EMPHASIS_STAR,
            '_', EMPHASIS_UNDERLINE,
            '-', STRIKEOUT,
            '`', CODE,
            '\\', ESCAPING
    ));

    final static Map<Character, String> unique = new HashMap<>(Map.of(
            '<', "&lt;",
            '>', "&gt;",
            '&', "&amp;"
    ));

    private Constants() {

    }
}