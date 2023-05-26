package info.kgeorgiy.ja.korolev.student;

import info.kgeorgiy.java.advanced.student.Group;
import info.kgeorgiy.java.advanced.student.GroupName;
import info.kgeorgiy.java.advanced.student.GroupQuery;
import info.kgeorgiy.java.advanced.student.Student;

import java.util.*;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class StudentDB implements GroupQuery {

    private static final Comparator<Student> STUDENT_BY_FULL_NAME_COMPARATOR = Comparator
            .comparing(Student::getLastName)
            .thenComparing(Student::getFirstName)
            .reversed()
            .thenComparing(Student::getId);
    private static final Comparator<Group> GROUPS_BY_NAME_COMPARATOR = Comparator.comparing(Group::getName);

    private static String getStudentFullName(Student student) {
        return student.getFirstName() + " " + student.getLastName();
    }

    @Override
    public List<Group> getGroupsByName(Collection<Student> students) {
        return getGroupsSorted(students, STUDENT_BY_FULL_NAME_COMPARATOR);
    }

    @Override
    public List<Group> getGroupsById(Collection<Student> students) {
        return getGroupsSorted(students, Comparator.naturalOrder());
    }

    @Override
    public GroupName getLargestGroup(Collection<Student> students) {
        return getGroupByComparator(students, List::size, true);
    }

    @Override
    public GroupName getLargestGroupFirstName(Collection<Student> students) {
        return getGroupByComparator(students, list -> getDistinctFirstNames(list).size(), false);
    }

    @Override
    public List<String> getFirstNames(List<Student> students) {
        return map(students, Student::getFirstName);
    }

    @Override
    public List<String> getLastNames(List<Student> students) {
        return map(students, Student::getLastName);
    }

    @Override
    public List<GroupName> getGroups(List<Student> students) {
        return map(students, Student::getGroup);
    }

    @Override
    public List<String> getFullNames(List<Student> students) {
        return map(students, StudentDB::getStudentFullName);
    }

    @Override
    public Set<String> getDistinctFirstNames(List<Student> students) {
        return map(students, Student::getFirstName, Collectors.toCollection(TreeSet::new));
    }

    @Override
    public String getMaxStudentFirstName(List<Student> students) {
        return students.stream().max(Comparator.naturalOrder()).map(Student::getFirstName).orElse("");
    }

    @Override
    public List<Student> sortStudentsById(Collection<Student> students) {
        return sortStudentsByComparator(students, Comparator.naturalOrder());
    }

    @Override
    public List<Student> sortStudentsByName(Collection<Student> students) {
        return sortStudentsByComparator(students, STUDENT_BY_FULL_NAME_COMPARATOR);
    }

    @Override
    public List<Student> findStudentsByFirstName(Collection<Student> students, String name) {
        return filter(students, Student::getFirstName, name).toList();
    }

    @Override
    public List<Student> findStudentsByLastName(Collection<Student> students, String name) {
        return filter(students, Student::getLastName, name).toList();
    }

    @Override
    public List<Student> findStudentsByGroup(Collection<Student> students, GroupName group) {
        return filterStudentsByGroup(students, group).toList();
    }

    @Override
    public Map<String, String> findStudentNamesByGroup(Collection<Student> students, GroupName group) {
        return filterStudentsByGroup(students, group).collect(Collectors.toMap(
                Student::getLastName,
                Student::getFirstName,
                BinaryOperator.minBy(Comparator.naturalOrder())
        ));
    }

    private <T, R> R map(Collection<Student> students, Function<Student, T> mapFn, Collector<T, ?, R> collector) {
        return students.stream().map(mapFn).collect(collector);
    }

    private <T> List<T> map(Collection<Student> students, Function<Student, T> mapFn) {
        return map(students, mapFn, Collectors.toList());
    }

    private <T> Stream<Student> filter(Collection<Student> students, Function<Student, T> field, T element) {
        return students.stream().filter(field.andThen(element::equals)::apply).sorted(STUDENT_BY_FULL_NAME_COMPARATOR);
    }

    private List<Student> sortStudentsByComparator(Collection<Student> students, Comparator<Student> comparator) {
        return students.stream().sorted(comparator).toList();
    }

    private Stream<Student> filterStudentsByGroup(Collection<Student> students, GroupName groupName) {
        return filter(students, Student::getGroup, groupName);
    }

    private Stream<Group> getGroupStudentEntries(Collection<Student> students, Comparator<Student> comparator) {
        return students
                .stream()
                .sorted(comparator)
                .collect(Collectors.groupingBy(Student::getGroup))
                .entrySet()
                .stream()
                .map(entry -> new Group(entry.getKey(), entry.getValue()));
    }

    private List<Group> getGroupsSorted(Collection<Student> students, Comparator<Student> comparator) {
        return getGroupStudentEntries(students, comparator).sorted(GROUPS_BY_NAME_COMPARATOR).toList();
    }

    private Comparator<Group> groupComparator(Function<List<Student>, Integer> comparator, boolean ascending) {
        return Comparator.comparing((Group group) -> comparator.apply(group.getStudents()))
                .thenComparing(ascending ? GROUPS_BY_NAME_COMPARATOR : GROUPS_BY_NAME_COMPARATOR.reversed());
    }

    private GroupName getGroupByComparator(Collection<Student> students, Function<List<Student>, Integer> comparator, boolean ascending) {
        return getGroupStudentEntries(students, Comparator.naturalOrder())
                .max(groupComparator(comparator, ascending))
                .map(Group::getName)
                .orElse(null);
    }
}