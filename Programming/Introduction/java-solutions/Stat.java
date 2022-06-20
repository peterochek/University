public class Stat {
    private final IntList list;
    private int count = 0;

    public Stat(int index) {
        this.list = new IntList(index);
        count++;
    }

    public void addToList(int value) {
        list.add(value);
    }

    public void addCount() {
        count++;
    }

    public int getCount() {
        return count;
    }

    public String listToString() {
        return list.listToString(true);
    }
}
