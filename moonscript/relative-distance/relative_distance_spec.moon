import degreeOfSeparation from require 'relative_distance'

describe 'relative-distance:', ->
  it 'Direct parent-child relation', ->
    familyTree = {
      Tomoko: {'Aditi'}
      Vera: {'Tomoko'}
    }
    result = degreeOfSeparation familyTree, 'Vera', 'Tomoko'
    expected = 1
    assert.are.equal expected, result

  it 'Sibling relationship', ->
    familyTree = {
      Dalia: {'Olga', 'Yassin'}
    }
    result = degreeOfSeparation familyTree, 'Olga', 'Yassin'
    expected = 1
    assert.are.equal expected, result

  it 'Two degrees of separation, grandchild', ->
    familyTree = {
      Khadija: {'Mateo'}
      Mateo: {'Rami'}
    }
    result = degreeOfSeparation familyTree, 'Khadija', 'Rami'
    expected = 2
    assert.are.equal expected, result

  it 'Unrelated individuals', ->
    familyTree = {
      Kaito: {'Elif'}
      Priya: {'Rami'}
    }
    result = degreeOfSeparation familyTree, 'Priya', 'Kaito'
    expected = nil
    assert.are.equal expected, result

  it 'Complex graph, cousins', ->
    familyTree = {
      Jing: {'Wyatt'}
      Noah: {'Xiomara'}
      Sven: {'Fabio'}
      Ravi: {'Celine'}
      Diego: {'Qi'}
      Aditi: {'Nia'}
      Kaito: {'Xia'}
      Rami: {'Ewa'}
      Mina: {'Viktor', 'Wang'}
      Aiko: {'Bao', 'Carlos'}
      Hana: {'Umar'}
      Viktor: {'Hana', 'Ian'}
      Isla: {'Pedro'}
      Dalia: {'Hassan', 'Isla'}
      Wang: {'Jing'}
      Quynh: {'Boris'}
      Zara: {'Mohammed'}
      Yassin: {'Lucia'}
      Leila: {'Yassin'}
      Giorgio: {'Tomoko'}
      Elif: {'Rami'}
      Hassan: {'Noah', 'Olga'}
      Olga: {'Yuki'}
      Bao: {'Dalia', 'Elias'}
      Yuki: {'Leila'}
      Fatima: {'Khadija', 'Liam'}
      Elias: {'Javier'}
      Vera: {'Igor'}
      Mateo: {'Zara'}
      Tariq: {'Farah'}
      Liam: {'Tariq', 'Uma'}
      Umar: {'Helena'}
      Tomoko: {'Gabriela'}
      Qi: {'Dimitri'}
      Carlos: {'Fatima', 'Gustavo'}
      Ian: {'Vera'}
      Oscar: {'Bianca'}
      Khadija: {'Sofia'}
      Xia: {'Kim'}
      Zane: {'Mateo'}
      Javier: {'Quynh', 'Ravi'}
      Celine: {'Priya'}
      Uma: {'Giorgio'}
      Priya: {'Cai'}
      Nia: {'Antonio'}
      Farah: {'Sven'}
      Sofia: {'Diego', 'Elif'}
      Boris: {'Oscar'}
      Wyatt: {'Jun'}
      Xiomara: {'Kaito'}
      Pedro: {'Zane', 'Aditi'}
      Gustavo: {'Mina'}
    }
    result = degreeOfSeparation familyTree, 'Dimitri', 'Fabio'
    expected = 9
    assert.are.equal expected, result

  it 'Complex graph, no shortcut, far removed nephew', ->
    familyTree = {
      Jing: {'Wyatt'}
      Noah: {'Xiomara'}
      Sven: {'Fabio'}
      Ravi: {'Celine'}
      Diego: {'Qi'}
      Aditi: {'Nia'}
      Kaito: {'Xia'}
      Rami: {'Ewa'}
      Mina: {'Viktor', 'Wang'}
      Aiko: {'Bao', 'Carlos'}
      Hana: {'Umar'}
      Viktor: {'Hana', 'Ian'}
      Isla: {'Pedro'}
      Dalia: {'Hassan', 'Isla'}
      Wang: {'Jing'}
      Quynh: {'Boris'}
      Zara: {'Mohammed'}
      Yassin: {'Lucia'}
      Leila: {'Yassin'}
      Giorgio: {'Tomoko'}
      Elif: {'Rami'}
      Hassan: {'Noah', 'Olga'}
      Olga: {'Yuki'}
      Bao: {'Dalia', 'Elias'}
      Yuki: {'Leila'}
      Fatima: {'Khadija', 'Liam'}
      Elias: {'Javier'}
      Vera: {'Igor'}
      Mateo: {'Zara'}
      Tariq: {'Farah'}
      Liam: {'Tariq', 'Uma'}
      Umar: {'Helena'}
      Tomoko: {'Gabriela'}
      Qi: {'Dimitri'}
      Carlos: {'Fatima', 'Gustavo'}
      Ian: {'Vera'}
      Oscar: {'Bianca'}
      Khadija: {'Sofia'}
      Xia: {'Kim'}
      Zane: {'Mateo'}
      Javier: {'Quynh', 'Ravi'}
      Celine: {'Priya'}
      Uma: {'Giorgio'}
      Priya: {'Cai'}
      Nia: {'Antonio'}
      Farah: {'Sven'}
      Sofia: {'Diego', 'Elif'}
      Boris: {'Oscar'}
      Wyatt: {'Jun'}
      Xiomara: {'Kaito'}
      Pedro: {'Zane', 'Aditi'}
      Gustavo: {'Mina'}
    }
    result = degreeOfSeparation familyTree, 'Lucia', 'Jun'
    expected = 14
    assert.are.equal expected, result

  it 'Complex graph, some shortcuts, cross-down and cross-up, cousins several times removed, with unrelated family tree', ->
    familyTree = {
      Jing: {'Wyatt'}
      Noah: {'Xiomara'}
      Sven: {'Fabio'}
      Ravi: {'Celine'}
      Diego: {'Qi'}
      Giorgio: {'Tomoko'}
      Kaito: {'Xia'}
      Rami: {'Ewa'}
      Mina: {'Viktor', 'Wang'}
      Carlos: {'Fatima', 'Gustavo'}
      Hana: {'Umar'}
      Viktor: {'Hana', 'Ian'}
      Isla: {'Pedro'}
      Dalia: {'Hassan', 'Isla'}
      Wang: {'Jing'}
      Quynh: {'Boris'}
      Zara: {'Mohammed'}
      Leila: {'Yassin'}
      Yassin: {'Lucia'}
      Khadija: {'Sofia'}
      Hassan: {'Noah', 'Olga'}
      Olga: {'Yuki'}
      Bao: {'Dalia'}
      Yuki: {'Leila'}
      Fatima: {'Khadija', 'Liam'}
      Xiomara: {'Kaito'}
      Elif: {'Rami'}
      Mateo: {'Zara'}
      Tariq: {'Farah'}
      Liam: {'Tariq', 'Uma'}
      Vera: {'Igor'}
      Umar: {'Helena'}
      Tomoko: {'Gabriela'}
      Qi: {'Dimitri'}
      Ian: {'Vera'}
      Aiko: {'Bao', 'Carlos'}
      Oscar: {'Bianca'}
      Xia: {'Kim'}
      Zane: {'Mateo'}
      Javier: {'Quynh', 'Ravi'}
      Celine: {'Priya'}
      Uma: {'Giorgio'}
      Priya: {'Cai'}
      Nia: {'Antonio'}
      Farah: {'Sven'}
      Sofia: {'Diego', 'Elif'}
      Boris: {'Oscar'}
      Wyatt: {'Jun'}
      Aditi: {'Nia'}
      Pedro: {'Zane', 'Aditi'}
      Gustavo: {'Mina'}
    }
    result = degreeOfSeparation familyTree, 'Wyatt', 'Xia'
    expected = 12
    assert.are.equal expected, result

