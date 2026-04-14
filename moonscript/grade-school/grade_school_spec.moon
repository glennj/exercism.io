import roster, add, grade from require 'grade_school'

describe 'grade-school', ->
  it 'Roster is empty when no student is added', ->
    result = roster {}
    expected = {}
    assert.are.same expected, result

  it 'Add a student', ->
    result = add {{'Aimee', 2}}
    expected = {true}
    assert.are.same expected, result

  it 'Student is added to the roster', ->
    result = roster {{'Aimee', 2}}
    expected = {'Aimee'}
    assert.are.same expected, result

  it 'Adding multiple students in the same grade in the roster', ->
    result = add {{'Blair', 2}, {'James', 2}, {'Paul', 2}}
    expected = {true, true, true}
    assert.are.same expected, result

  it 'Multiple students in the same grade are added to the roster', ->
    result = roster {{'Blair', 2}, {'James', 2}, {'Paul', 2}}
    expected = {'Blair', 'James', 'Paul'}
    assert.are.same expected, result

  it 'Cannot add student to same grade in the roster more than once', ->
    result = add {{'Blair', 2}, {'James', 2}, {'James', 2}, {'Paul', 2}}
    expected = {true, true, false, true}
    assert.are.same expected, result

  it 'Student not added to same grade in the roster more than once', ->
    result = roster {{'Blair', 2}, {'James', 2}, {'James', 2}, {'Paul', 2}}
    expected = {'Blair', 'James', 'Paul'}
    assert.are.same expected, result

  it 'Adding students in multiple grades', ->
    result = add {{'Chelsea', 3}, {'Logan', 7}}
    expected = {true, true}
    assert.are.same expected, result

  it 'Students in multiple grades are added to the roster', ->
    result = roster {{'Chelsea', 3}, {'Logan', 7}}
    expected = {'Chelsea', 'Logan'}
    assert.are.same expected, result

  it 'Cannot add same student to multiple grades in the roster', ->
    result = add {{'Blair', 2}, {'James', 2}, {'James', 3}, {'Paul', 3}}
    expected = {true, true, false, true}
    assert.are.same expected, result

  it 'Student not added to multiple grades in the roster', ->
    result = roster {{'Blair', 2}, {'James', 2}, {'James', 3}, {'Paul', 3}}
    expected = {'Blair', 'James', 'Paul'}
    assert.are.same expected, result

  it 'Students are sorted by grades in the roster', ->
    result = roster {{'Jim', 3}, {'Peter', 2}, {'Anna', 1}}
    expected = {'Anna', 'Peter', 'Jim'}
    assert.are.same expected, result

  it 'Students are sorted by name in the roster', ->
    result = roster {{'Peter', 2}, {'Zoe', 2}, {'Alex', 2}}
    expected = {'Alex', 'Peter', 'Zoe'}
    assert.are.same expected, result

  it 'Students are sorted by grades and then by name in the roster', ->
    result = roster {{'Peter', 2}, {'Anna', 1}, {'Barb', 1}, {'Zoe', 2}, {'Alex', 2}, {'Jim', 3}, {'Charlie', 1}}
    expected = {'Anna', 'Barb', 'Charlie', 'Alex', 'Peter', 'Zoe', 'Jim'}
    assert.are.same expected, result

  it 'Grade is empty if no students in the roster', ->
    result = grade {}, 1
    expected = {}
    assert.are.same expected, result

  it 'Grade is empty if no students in that grade', ->
    result = grade {{'Peter', 2}, {'Zoe', 2}, {'Alex', 2}, {'Jim', 3}}, 1
    expected = {}
    assert.are.same expected, result

  it 'Student not added to same grade more than once', ->
    result = grade {{'Blair', 2}, {'James', 2}, {'James', 2}, {'Paul', 2}}, 2
    expected = {'Blair', 'James', 'Paul'}
    assert.are.same expected, result

  it 'Student not added to multiple grades', ->
    result = grade {{'Blair', 2}, {'James', 2}, {'James', 3}, {'Paul', 3}}, 2
    expected = {'Blair', 'James'}
    assert.are.same expected, result

  it 'Student not added to other grade for multiple grades', ->
    result = grade {{'Blair', 2}, {'James', 2}, {'James', 3}, {'Paul', 3}}, 3
    expected = {'Paul'}
    assert.are.same expected, result

  it 'Students are sorted by name in a grade', ->
    result = grade {{'Franklin', 5}, {'Bradley', 5}, {'Jeff', 1}}, 5
    expected = {'Bradley', 'Franklin'}
    assert.are.same expected, result
