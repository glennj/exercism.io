'use strict';

const sharps = ['A', 'A#', 'B', 'C', 'C#', 'D', 'D#', 'E', 'F', 'F#', 'G', 'G#'];
const flats  = ['A', 'Bb', 'B', 'C', 'Db', 'D', 'Eb', 'E', 'F', 'Gb', 'G', 'Ab'];
const useFlats = ['F', 'Bb', 'Eb', 'Ab', 'Db', 'Gb', 'd', 'g', 'c', 'f', 'bb', 'eb'];
const steps = {'m': 1, 'M': 2, 'A': 3};

String.prototype.toTitleCase = function() {
  return this.substring(0, 1).toUpperCase() + this.substring(1);
}


export class Scale {
  constructor(tonic) {
    var _notes = useFlats.includes(tonic) ? flats : sharps;
    let idx = _notes.indexOf(tonic.toTitleCase());
    if (idx == -1)
      throw new Error('unknown tonic: ' + tonic);
    this.notes = _notes.slice(idx).concat(_notes.slice(0, idx));
  }

  chromatic() {
    return this.notes;
  }

  interval(intervals) {
    var scale = [];
    var idx = 0;
    for (var i = 0; i < intervals.length; i++) {
      scale.push(this.notes[idx]);
      let interval = intervals.charAt(i);
      if (!(interval in steps))
        throw new Error('unknown interval: ' + interval);
      idx += steps[interval];
    }
    return scale;
  }
}
