colors = ['black', 'brown', 'red', 'orange', 'yellow',
          'green', 'blue', 'violet', 'grey', 'white']

class ResistorColor
  @colorCode: (color) -> colors.indexOf(color)
  @colors: () -> colors

module.exports = ResistorColor
