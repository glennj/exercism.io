// Taking the Date module from RosettaCode
// -- https://rosettacode.org/wiki/Category:Wren
import "./date" for Date


class Meetup {
  static date(description) {
    var words = description.split(" ")
    var wanted = words[1]
    var weekDay = words[2]
    var month = words[4]
    var year = words[5]

    var fmtDate = Fn.new {|day, month, year| "%(month) %(day), %(year)" }

    var date = Date.parse(fmtDate.call(1, month, year), Date.civilian)

    var days = []
    while (date.monthName == month) {
      if (date.weekDay == weekDay) days.add(date.day)
      date = date.addDays(1)
    }

    if (wanted == "first")  return fmtDate.call(days[0], month, year)
    if (wanted == "second") return fmtDate.call(days[1], month, year)
    if (wanted == "third")  return fmtDate.call(days[2], month, year)
    if (wanted == "fourth") return fmtDate.call(days[3], month, year)
    if (wanted == "last")   return fmtDate.call(days[-1], month, year)
    if (wanted == "teenth") {
      var day = days.where {|d| 13 <= d && d <= 19}.toList[0]
      return fmtDate.call(day, month, year)
    }

    Fiber.abort("Unknown category: %(wanted)")
  }

}
