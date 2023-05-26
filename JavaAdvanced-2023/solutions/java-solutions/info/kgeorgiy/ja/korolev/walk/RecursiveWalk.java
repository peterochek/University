package info.kgeorgiy.ja.korolev.walk;

public class RecursiveWalk {
    public static void main(String[] args) {
        new SimpleWalk(VisitMode.RECURSIVE).run(args, "Recursive");
    }
}
