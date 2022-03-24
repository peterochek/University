package markup;

import java.util.List;

public class Strikeout extends Abstract implements IParagraph {
    public Strikeout(List<IParagraph> elements) {
        super(elements);
        outer = "~";
        BBOuterFirst = "[s]";
        BBOuterSecond = "[/s]";
    }
}
