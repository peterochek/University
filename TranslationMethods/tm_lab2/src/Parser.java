import java.text.ParseException;

public class Parser {

    private LexicalAnalyzer lex;

    Tree parse(String scriptText) throws ParseException {
        lex = new LexicalAnalyzer(scriptText);

        return All();
    }

    private Tree All() throws ParseException {
        Tree desc = Desc();
        Tree cont = Cont();

        return new Tree("All", desc, cont);
    }

    private Tree Desc() throws ParseException {
        lex.nextToken();
        if (lex.curToken() == Token.END) {
            return new Tree("END");
        }

        if (lex.curToken() != Token.VAL && lex.curToken() != Token.VAR) {
            throw new ParseException("Unexpected token", lex.curPos());
        }
        Tree v = new Tree(lex.curString());

        lex.nextToken();
        if (lex.curToken() != Token.NAME) {
            throw new ParseException("Expected variable name", lex.curPos());
        }
        Tree name = new Tree(lex.curString());

        lex.nextToken();
        if (lex.curToken() != Token.COLON) {
            throw new ParseException("Expected : after name", lex.curPos());
        }
        Tree colon = new Tree(lex.curString());

        lex.nextToken();
        if (lex.curToken() != Token.TYPE) {
            throw new ParseException("Expected Type(Int) after :", lex.curPos());
        }
        Tree value = Value();

        if (lex.curToken() != Token.SEMICOLON) {
            throw new ParseException("Expected ; at the end", lex.curPos());
        }
        Tree semicolon = new Tree(lex.curString());


        return new Tree("DESC", v, name, colon, value, semicolon);
    }

    private Tree Cont() throws ParseException {
        Tree desc = Desc();

        if (desc.getNode().equals("END")) {
            return new Tree("END");
        }

        Tree cont = Cont();

        return new Tree("CONT", desc, cont);
    }

    private Tree Value() throws ParseException {
        lex.nextToken();
        if (lex.curToken() != Token.EQ) {
            throw new ParseException("Expected '='", lex.curPos());
        }
        Tree eq = new Tree(lex.curString());

        lex.nextToken();
        Tree exp = Exp();

        return new Tree("VAL", eq, exp);
    }

    private Tree Exp() throws ParseException {
        Tree term = Term();
        Tree expPrime = ExpPrime();

        if (expPrime.getNode().isEmpty()) {
            return term;
        }

        return new Tree("Exp", term, expPrime);
    }

    private Tree ExpPrime() throws ParseException {
        if (lex.curToken() == Token.PLUS) {
            Tree plus = new Tree(lex.curString());
            lex.nextToken();
            Tree term = Term();
            Tree expPrime = ExpPrime();

            return new Tree("Exp'", plus, term, expPrime);
        } else if (lex.curToken() == Token.MINUS) {
            Tree minus = new Tree(lex.curString());
            lex.nextToken();
            Tree term = Term();
            Tree expPrime = ExpPrime();

            return new Tree("Exp'", minus, term, expPrime);
        }

        return new Tree("");
    }

    private Tree Term() throws ParseException {
        Tree factor = Factor();
        Tree termPrime = TermPrime();

        if (termPrime.getNode().isEmpty()) {
            return factor;
        }

        return new Tree("Term", factor, termPrime);
    }

    private Tree TermPrime() throws ParseException {
        if (lex.curToken() == Token.TIMES) {
            Tree times = new Tree(lex.curString());
            lex.nextToken();
            Tree factor = Factor();
            Tree termPrime = TermPrime();

            return new Tree("Term'", times, factor, termPrime);
        } else if (lex.curToken() == Token.DIVIDE) {
            Tree div = new Tree(lex.curString());
            lex.nextToken();
            Tree factor = Factor();
            Tree termPrime = TermPrime();

            return new Tree("Term'", div, factor, termPrime);
        }

        return new Tree("");
    }

    private Tree Factor() throws ParseException {
        if (lex.curToken() == Token.LPAREN) {
            lex.nextToken();
            Tree exp = Exp();
            if (lex.curToken() != Token.RPAREN) {
                throw new ParseException("Expected closing parenthesis", lex.curPos());
            }
            lex.nextToken();

            return new Tree("Factor", new Tree("("), exp, new Tree(")"));
        } else if (lex.curToken() == Token.PLUS) {
            Tree plus = new Tree(lex.curString());
            lex.nextToken();
            Tree factor = Factor();

            return new Tree("Factor", plus, factor);
        } else if (lex.curToken() == Token.MINUS) {
            Tree minus = new Tree(lex.curString());
            lex.nextToken();
            Tree factor = Factor();

            return new Tree("Factor", minus, factor);
        } else if (lex.curToken() == Token.NUMBER) {
            Tree number = new Tree(lex.curString());
            lex.nextToken();

            return new Tree("Factor", number);
        } else {
            throw new ParseException("Unexpected token", lex.curPos());
        }
    }
}