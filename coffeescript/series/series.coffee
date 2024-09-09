class Series
  @slices: (series, sliceLength) ->
    throw new Error 'series cannot be empty' if series.length == 0
    throw new Error 'slice length cannot be greater than series length' if sliceLength > series.length
    throw new Error 'slice length cannot be zero' if sliceLength == 0
    throw new Error 'slice length cannot be negative' if sliceLength < 0
      
    (series[i ... i + sliceLength] for i in [0 .. series.length - sliceLength])

module.exports = Series
