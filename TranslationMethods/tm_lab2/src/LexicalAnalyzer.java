import java.text.ParseException;

public class LexicalAnalyzer {

    private static final char END = '\0';
    private final String scriptText;
    private int curPos;
    private char curChar;
    private Token curToken;
    private String curString;

    public LexicalAnalyzer(String scriptText) throws ParseException {
        this.scriptText = scriptText + END;
        curPos = -1;
        nextChar();
    }

    private boolean isBlank(int c) {
        return c == ' ' || c == '\r' || c == '\n' || c == '\t';
    }

    public void nextChar() throws ParseException {
        curPos++;
        if (scriptText.length() <= curPos) {
            throw new ParseException(scriptText, curPos);
        }
        curChar = scriptText.charAt(curPos);
    }

    public void nextToken() throws ParseException {
        while (isBlank(curChar)) {
            nextChar();
        }
        switch (curChar) {
            case ';' -> {
                nextChar();
                curString = ";";
                curToken = Token.SEMICOLON;
            }
            case ':' -> {
                nextChar();
                curString = ":";
                curToken = Token.COLON;
            }
            case '=' -> {
                nextChar();
                curString = "=";
                curToken = Token.EQ;
            }
            case '(' -> {
                nextChar();
                curString = "(";
                curToken = Token.LPAREN;
            }
            case ')' -> {
                nextChar();
                curString = ")";
                curToken = Token.RPAREN;
            }
            case '+' -> {
                nextChar();
                curString = "+";
                curToken = Token.PLUS;
            }
            case '-' -> {
                nextChar();
                curString = "-";
                curToken = Token.MINUS;
            }
            case '*' -> {
                nextChar();
                curString = "*";
                curToken = Token.TIMES;
            }
            case '/' -> {
                nextChar();
                curString = "/";
                curToken = Token.DIVIDE;
            }
            case END -> curToken = Token.END;
            default -> {
                curString = parseToken();
                switch (curString) {
                    case "var" -> {
                        curString = "var";
                        curToken = Token.VAR;
                    }
                    case "val" -> {
                        curString = "val";
                        curToken = Token.VAL;
                    }
                    case "Int" -> {
                        curString = "Int";
                        curToken = Token.TYPE;
                    }
                    default -> {
                        curPos -= curString.length() + 1;
                        nextChar();
                        curToken = getLongToken();
                    }
                }
            }
        }
    }

    private String parseToken() throws ParseException {
        StringBuilder sb = new StringBuilder();
        sb.append(curChar);
        nextChar();
        while (Character.isLetterOrDigit(curChar) || curChar == '_' || curChar == '-') {
            sb.append(curChar);
            nextChar();
        }
        return sb.toString();
    }

    private Token getLongToken() throws ParseException {
        if (Character.isDigit(curChar) || curChar == '-') {
            curString = parseNumber();
            return Token.NUMBER;
        } else if (Character.isLetter(curChar) || curChar == '_') {
            curString = parseIdentifier();
            return Token.NAME;
        }
        throw new ParseException("Unexpected char in token: " + curChar, curPos);
    }

    private String parseIdentifier() throws ParseException {
        StringBuilder sb = new StringBuilder();
        sb.append(curChar);
        nextChar();
        while (Character.isLetterOrDigit(curChar) || curChar == '_') {
            sb.append(curChar);
            nextChar();
        }
        return sb.toString();
    }

    private String parseNumber() throws ParseException {
        String number = readDigits();
        try {
            Integer.parseInt(number);
            return number;
        } catch (final NumberFormatException e) {
            curPos -= number.length();
            throw new ParseException("Incorrect int literal: " + number + "  ", curPos);
        }
    }

    private String readDigits() throws ParseException {
        StringBuilder sb = new StringBuilder();
        do {
            sb.append(curChar);
            nextChar();
        } while (Character.isDigit(curChar));
        return sb.toString();
    }


    public Token curToken() {
        return curToken;
    }

    public int curPos() {
        return curPos;
    }

    public String curString() {
        return curString;
    }
}