package markup;

import java.util.List;

public class ListItem extends Abstract {
    public ListItem(List<IList> elements) {
        super(elements);
    }

    @Override
    public void toBBCode(StringBuilder sb) {
        sb.append("[*]");
        for (IAll el : elements) {
            el.toBBCode(sb);
        }
    }
}
