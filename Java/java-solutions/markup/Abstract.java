package markup;

import java.util.List;

public abstract class Abstract implements IAll {
    protected final List<? extends IAll> elements;
    protected String outer = "";
    protected String BBOuterFirst = "";
    protected String BBOuterSecond = "";

    public Abstract(List<? extends IAll> elements) {
        this.elements = elements;
    }

    @Override
    public void toBBCode(StringBuilder sb) {
        sb.append(BBOuterFirst);
        for (IAll el : elements) {
            el.toBBCode(sb);
        }
        sb.append(BBOuterSecond);
    }

    @Override
    public void toMarkdown(StringBuilder sb) {
        sb.append(outer);
        for (IAll el : elements) {
            el.toMarkdown(sb);
        }
        sb.append(outer);
    }
}
