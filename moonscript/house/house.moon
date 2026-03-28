data = {
  {'house', 'Jack built.'},
  {'malt', 'lay in'},
  {'rat', 'ate'},
  {'cat', 'killed'},
  {'dog', 'worried'},
  {'cow with the crumpled horn', 'tossed'},
  {'maiden all forlorn', 'milked'},
  {'man all tattered and torn', 'kissed'},
  {'priest all shaven and shorn', 'married'},
  {'rooster that crowed in the morn', 'woke'},
  {'farmer sowing his corn', 'kept'},
  {'horse and the hound and the horn', 'belonged to'},
}

verse = (n) ->
  seq = [" the #{noun} that #{verb}" for {noun, verb} in *[data[i] for i = n, 1, -1]]
  "This is" .. table.concat seq

{
  recite: (start, stop) ->
    [verse i for i = start, stop]
}
