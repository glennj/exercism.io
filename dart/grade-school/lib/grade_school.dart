typedef Student = (String, int);

class GradeSchool {
  List<Student> directory = [];

  List<String> roster() => _names(directory);

  List<String> _names(Iterable<Student> students) =>
      students.map((s) => s.$1).toList();

  List<bool> add(List<Student> students) => students.map(_addStudent).toList();

  bool _addStudent(Student student) {
    if (directory.any((s) => s.$1 == student.$1)) {
      return false;
    }

    directory.add(student);
    directory.sort((a, b) {
      var cmp = a.$2.compareTo(b.$2);
      return cmp != 0 ? cmp : a.$1.compareTo(b.$1);
    });
    return true;
  }

  List<String> grade(int grade) =>
      _names(directory.where((s) => s.$2 == grade));
}
