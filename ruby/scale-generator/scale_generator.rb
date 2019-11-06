class Scale
  CHROMATIC = {
    sharps: %w[A A# B C C# D D# E F F# G G#],
    flats:  %w[A Bb B C Db D Eb E F Gb G Ab]
  }.freeze

  FLATS = %w[F Bb Eb Ab Db Gb d g c f bb eb].freeze

  SCALE_INTERVALS = {
    chromatic: '',         major:      'MMmMMMm', minor:      'MmMMmMM',
    dorian:    'MmMMMmM',  mixolydian: 'MMmMMmM', lydian:     'MMMmMMm',
    phrygian:  'mMMMmMM',  locrian:    'mMMmMMM', enigma:     'mAMMMmM',
    octatonic: 'MmMmMmMm', hexatonic:  'MMMMMM',  pentatonic: 'MMAMA',
    harmonic_minor: 'MmMMmAm'
  }.freeze

  INTERVAL = { 'm' => 1, 'M' => 2, 'A' => 3 }.freeze

  attr_reader :name, :pitches

  def initialize(note, type, intervals = '')
    raise ArgumentError, 'Unknown type' unless SCALE_INTERVALS.key? type
    raise ArgumentError, 'Invalid intervals' if SCALE_INTERVALS[type] != intervals

    tonic = note.capitalize
    raise ArgumentError, "Unknown tonic #{tonic}" \
      unless %i[sharps flats].any? { |s| CHROMATIC[s].include?(tonic) }

    @name = "#{tonic} #{type}"

    @pitches = filter(rotate(notes(note), tonic), intervals)
  end

  private

  def notes(note)
    CHROMATIC[FLATS.include?(note) ? :flats : :sharps]
  end

  def rotate(notes, tonic)
    idx = notes.index tonic
    # a 2-dot range and a 3-dot range
    notes[idx..-1] + notes[0...idx]
  end

  def filter(notes, intervals)
    return notes if intervals.empty?

    pitches = []
    idx = 0
    intervals.chars.each do |i|
      pitches << notes[idx]
      idx += INTERVAL[i]
    end
    pitches
  end
end
