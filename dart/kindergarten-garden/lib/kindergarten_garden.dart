enum Plant { radishes, clover, violets, grass }

enum Student {
  Alice, Bob, Charlie, David, Eve, Fred,
  Ginny, Harriet, Ileana, Joseph, Kincaid, Larry,
}

class KindergartenGarden {
  final String diagram;
  Map<Student, List<Plant>> plots = {};
  KindergartenGarden(this.diagram);

  List<Plant> plants(Student student) {
    if (plots.isEmpty) _parse();
    return plots[student]!;
  }

  void _parse() {
    var plantLookup = {
      'R': Plant.radishes,
      'C': Plant.clover,
      'V': Plant.violets,
      'G': Plant.grass
    };
    var lines = diagram.split('\n');

    var plants = List.generate((lines.first.length / 2).toInt(), (i) {
      var plot = lines.map((line) => line.substring(2 * i, 2 * i + 2)).join('');
      return plot.split('').map((p) {
        if (plantLookup[p] == null) throw ArgumentError('not a plant $p');
        return plantLookup[p]!;
      }).toList();
    });

    var students = Student.values;
    for (var i = 0; i < plants.length; i++) {
      plots[students[i]] = plants[i];
    }
  }
}
