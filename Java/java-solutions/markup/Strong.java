package markup;

import java.util.List;

public class Strong extends Abstract implements IParagraph {
    public Strong(List<IParagraph> elements) {
        super(elements);
        outer = "__";
        BBOuterFirst = "[b]";
        BBOuterSecond = "[/b]";
    }
}
