export class GradeSchool {
  constructor() {
    this.directory = new Map();
  }

  add(name, grade) {
    this.directory.set(name, grade);
    return this;
  }

  grade(grade) {
    return this.roster()[grade] ?? [];
  }

  roster() {
    const roster = {};
    this.directory.forEach((grade, name) => {
      roster[grade] ??= [];
      roster[grade].push(name);
    });
    for (const g of Object.keys(roster))
      roster[g].sort();
    return roster;
  }
}
