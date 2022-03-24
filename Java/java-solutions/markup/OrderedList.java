package markup;

import java.util.List;

public class OrderedList extends Abstract implements IList {
    public OrderedList(List<ListItem> elements) {
        super(elements);
        BBOuterFirst = "[list=1]";
        BBOuterSecond = "[/list]";
    }
}