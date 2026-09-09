import grep from require 'grep'
file_util = require 'pl.file'


contents = {
  "iliad.txt": [[
Achilles sing, O Goddess! Peleus' son;
His wrath pernicious, who ten thousand woes
Caused to Achaia's host, sent many a soul
Illustrious into Ades premature,
And Heroes gave (so stood the will of Jove)
To dogs and to all ravening fowls a prey,
When fierce dispute had separated once
The noble Chief Achilles from the son
Of Atreus, Agamemnon, King of men.]],

  "midsummer-night.txt": [[
I do entreat your grace to pardon me.
I know not by what power I am made bold,
Nor how it may concern my modesty,
In such a presence here to plead my thoughts;
But I beseech your grace that I may know
The worst that may befall me in this case,
If I refuse to wed Demetrius.]],

  "paradise-lost.txt": [[
Of Mans First Disobedience, and the Fruit
Of that Forbidden Tree, whose mortal tast
Brought Death into the World, and all our woe,
With loss of Eden, till one greater Man
Restore us, and regain the blissful Seat,
Sing Heav'nly Muse, that on the secret top
Of Oreb, or of Sinai, didst inspire
That Shepherd, who first taught the chosen Seed]]
}

before_each -> file_util.write f, g for f, g in pairs contents
after_each -> file_util.delete f for f, _ in pairs contents


describe 'grep:', ->
  describe 'Test grepping a single file:', ->

    it 'One file, one match, no flags', ->
      result = grep {}, 'Agamemnon', 'iliad.txt'
      expected = {
        'Of Atreus, Agamemnon, King of men.',
      }
      assert.are.same expected, result

    pending 'One file, one match, print line numbers flag', ->
      result = grep {'-n'}, 'Forbidden', 'paradise-lost.txt'
      expected = {
        '2:Of that Forbidden Tree, whose mortal tast',
      }
      assert.are.same expected, result

    pending 'One file, one match, case-insensitive flag', ->
      result = grep {'-i'}, 'FORBIDDEN', 'paradise-lost.txt'
      expected = {
        'Of that Forbidden Tree, whose mortal tast',
      }
      assert.are.same expected, result

    pending 'One file, one match, print file names flag', ->
      result = grep {'-l'}, 'Forbidden', 'paradise-lost.txt'
      expected = {
        'paradise-lost.txt',
      }
      assert.are.same expected, result

    pending 'One file, one match, match entire lines flag', ->
      result = grep {'-x'}, 'With loss of Eden, till one greater Man', 'paradise-lost.txt'
      expected = {
        'With loss of Eden, till one greater Man',
      }
      assert.are.same expected, result

    pending 'One file, one match, multiple flags', ->
      result = grep {'-n', '-i', '-x'}, 'OF ATREUS, Agamemnon, KIng of MEN.', 'iliad.txt'
      expected = {
        '9:Of Atreus, Agamemnon, King of men.',
      }
      assert.are.same expected, result

    pending 'One file, several matches, no flags', ->
      result = grep {}, 'may', 'midsummer-night.txt'
      expected = {
        'Nor how it may concern my modesty,',
        'But I beseech your grace that I may know',
        'The worst that may befall me in this case,',
      }
      assert.are.same expected, result

    pending 'One file, several matches, print line numbers flag', ->
      result = grep {'-n'}, 'may', 'midsummer-night.txt'
      expected = {
        '3:Nor how it may concern my modesty,',
        '5:But I beseech your grace that I may know',
        '6:The worst that may befall me in this case,',
      }
      assert.are.same expected, result

    pending 'One file, several matches, match entire lines flag', ->
      result = grep {'-x'}, 'may', 'midsummer-night.txt'
      expected = {}
      assert.are.same expected, result

    pending 'One file, several matches, case-insensitive flag', ->
      result = grep {'-i'}, 'ACHILLES', 'iliad.txt'
      expected = {
        "Achilles sing, O Goddess! Peleus' son;",
        'The noble Chief Achilles from the son',
      }
      assert.are.same expected, result

    pending 'One file, several matches, inverted flag', ->
      result = grep {'-v'}, 'Of', 'paradise-lost.txt'
      expected = {
        'Brought Death into the World, and all our woe,',
        'With loss of Eden, till one greater Man',
        'Restore us, and regain the blissful Seat,',
        "Sing Heav'nly Muse, that on the secret top",
        'That Shepherd, who first taught the chosen Seed',
      }
      assert.are.same expected, result

    pending 'One file, no matches, various flags', ->
      result = grep {'-n', '-l', '-x', '-i'}, 'Gandalf', 'iliad.txt'
      expected = {}
      assert.are.same expected, result

    pending 'One file, one match, file flag takes precedence over line flag', ->
      result = grep {'-n', '-l'}, 'ten', 'iliad.txt'
      expected = {
        'iliad.txt',
      }
      assert.are.same expected, result

    pending 'One file, several matches, inverted and match entire lines flags', ->
      result = grep {'-x', '-v'}, 'Illustrious into Ades premature,', 'iliad.txt'
      expected = {
        "Achilles sing, O Goddess! Peleus' son;",
        'His wrath pernicious, who ten thousand woes',
        "Caused to Achaia's host, sent many a soul",
        'And Heroes gave (so stood the will of Jove)',
        'To dogs and to all ravening fowls a prey,',
        'When fierce dispute had separated once',
        'The noble Chief Achilles from the son',
        'Of Atreus, Agamemnon, King of men.',
      }
      assert.are.same expected, result

  describe 'Test grepping multiples files at once:', ->

    pending 'Multiple files, one match, no flags', ->
      result = grep {}, 'Agamemnon', 'iliad.txt', 'midsummer-night.txt', 'paradise-lost.txt'
      expected = {
        'iliad.txt:Of Atreus, Agamemnon, King of men.',
      }
      assert.are.same expected, result

    pending 'Multiple files, several matches, no flags', ->
      result = grep {}, 'may', 'iliad.txt', 'midsummer-night.txt', 'paradise-lost.txt'
      expected = {
        'midsummer-night.txt:Nor how it may concern my modesty,',
        'midsummer-night.txt:But I beseech your grace that I may know',
        'midsummer-night.txt:The worst that may befall me in this case,',
      }
      assert.are.same expected, result

    pending 'Multiple files, several matches, print line numbers flag', ->
      result = grep {'-n'}, 'that', 'iliad.txt', 'midsummer-night.txt', 'paradise-lost.txt'
      expected = {
        'midsummer-night.txt:5:But I beseech your grace that I may know',
        'midsummer-night.txt:6:The worst that may befall me in this case,',
        'paradise-lost.txt:2:Of that Forbidden Tree, whose mortal tast',
        "paradise-lost.txt:6:Sing Heav'nly Muse, that on the secret top",
      }
      assert.are.same expected, result

    pending 'Multiple files, one match, print file names flag', ->
      result = grep {'-l'}, 'who', 'iliad.txt', 'midsummer-night.txt', 'paradise-lost.txt'
      expected = {
        'iliad.txt',
        'paradise-lost.txt',
      }
      assert.are.same expected, result

    pending 'Multiple files, several matches, case-insensitive flag', ->
      result = grep {'-i'}, 'TO', 'iliad.txt', 'midsummer-night.txt', 'paradise-lost.txt'
      expected = {
        "iliad.txt:Caused to Achaia's host, sent many a soul",
        'iliad.txt:Illustrious into Ades premature,',
        'iliad.txt:And Heroes gave (so stood the will of Jove)',
        'iliad.txt:To dogs and to all ravening fowls a prey,',
        'midsummer-night.txt:I do entreat your grace to pardon me.',
        'midsummer-night.txt:In such a presence here to plead my thoughts;',
        'midsummer-night.txt:If I refuse to wed Demetrius.',
        'paradise-lost.txt:Brought Death into the World, and all our woe,',
        'paradise-lost.txt:Restore us, and regain the blissful Seat,',
        "paradise-lost.txt:Sing Heav'nly Muse, that on the secret top",
      }
      assert.are.same expected, result

    pending 'Multiple files, several matches, inverted flag', ->
      result = grep {'-v'}, 'a', 'iliad.txt', 'midsummer-night.txt', 'paradise-lost.txt'
      expected = {
        "iliad.txt:Achilles sing, O Goddess! Peleus' son;",
        'iliad.txt:The noble Chief Achilles from the son',
        'midsummer-night.txt:If I refuse to wed Demetrius.',
      }
      assert.are.same expected, result

    pending 'Multiple files, one match, match entire lines flag', ->
      result = grep {'-x'}, 'But I beseech your grace that I may know', 'iliad.txt', 'midsummer-night.txt', 'paradise-lost.txt'
      expected = {
        'midsummer-night.txt:But I beseech your grace that I may know',
      }
      assert.are.same expected, result

    pending 'Multiple files, one match, multiple flags', ->
      result = grep {'-n', '-i', '-x'}, 'WITH LOSS OF EDEN, TILL ONE GREATER MAN', 'iliad.txt', 'midsummer-night.txt', 'paradise-lost.txt'
      expected = {
        'paradise-lost.txt:4:With loss of Eden, till one greater Man',
      }
      assert.are.same expected, result

    pending 'Multiple files, no matches, various flags', ->
      result = grep {'-n', '-l', '-x', '-i'}, 'Frodo', 'iliad.txt', 'midsummer-night.txt', 'paradise-lost.txt'
      expected = {}
      assert.are.same expected, result

    pending 'Multiple files, several matches, file flag takes precedence over line number flag', ->
      result = grep {'-n', '-l'}, 'who', 'iliad.txt', 'midsummer-night.txt', 'paradise-lost.txt'
      expected = {
        'iliad.txt',
        'paradise-lost.txt',
      }
      assert.are.same expected, result

    pending 'Multiple files, several matches, inverted and match entire lines flags', ->
      result = grep {'-x', '-v'}, 'Illustrious into Ades premature,', 'iliad.txt', 'midsummer-night.txt', 'paradise-lost.txt'
      expected = {
        "iliad.txt:Achilles sing, O Goddess! Peleus' son;",
        'iliad.txt:His wrath pernicious, who ten thousand woes',
        "iliad.txt:Caused to Achaia's host, sent many a soul",
        'iliad.txt:And Heroes gave (so stood the will of Jove)',
        'iliad.txt:To dogs and to all ravening fowls a prey,',
        'iliad.txt:When fierce dispute had separated once',
        'iliad.txt:The noble Chief Achilles from the son',
        'iliad.txt:Of Atreus, Agamemnon, King of men.',
        'midsummer-night.txt:I do entreat your grace to pardon me.',
        'midsummer-night.txt:I know not by what power I am made bold,',
        'midsummer-night.txt:Nor how it may concern my modesty,',
        'midsummer-night.txt:In such a presence here to plead my thoughts;',
        'midsummer-night.txt:But I beseech your grace that I may know',
        'midsummer-night.txt:The worst that may befall me in this case,',
        'midsummer-night.txt:If I refuse to wed Demetrius.',
        'paradise-lost.txt:Of Mans First Disobedience, and the Fruit',
        'paradise-lost.txt:Of that Forbidden Tree, whose mortal tast',
        'paradise-lost.txt:Brought Death into the World, and all our woe,',
        'paradise-lost.txt:With loss of Eden, till one greater Man',
        'paradise-lost.txt:Restore us, and regain the blissful Seat,',
        "paradise-lost.txt:Sing Heav'nly Muse, that on the secret top",
        'paradise-lost.txt:Of Oreb, or of Sinai, didst inspire',
        'paradise-lost.txt:That Shepherd, who first taught the chosen Seed',
      }
      assert.are.same expected, result

