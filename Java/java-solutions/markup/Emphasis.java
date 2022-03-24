package markup;

import java.util.List;

public class Emphasis extends Abstract implements IParagraph {
    public Emphasis(List<IParagraph> elements) {
        super(elements);
        outer = "*";
        BBOuterFirst = "[i]";
        BBOuterSecond = "[/i]";
    }
}
