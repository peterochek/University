package markup;

import java.util.List;

public class UnorderedList extends Abstract implements IList {
    public UnorderedList(List<ListItem> elements) {
        super(elements);
        BBOuterFirst = "[list]";
        BBOuterSecond = "[/list]";
    }
}
