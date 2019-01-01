const deepcopy = obj => JSON.parse(JSON.stringify(obj));

class School {
  constructor() {
    this.students = {};
  }

  // return a *copy* of the roster
  roster() {
    return deepcopy(this.students);
  }

  grade(grade) {
    const classroom = this.students[grade] || [];
    return deepcopy(classroom);
  }

  add(name, grade) {
    const classroom = this.grade(grade);
    classroom.push(name);
    this.students[grade] = classroom.sort();
    return this;
  }
}

module.exports = School;
