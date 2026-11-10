/* Module "date.wren" */

import "./trait" for Comparable

/*
    Date represents a date (and the time within that date) as the number of milliseconds
    which have elapsed according to the Gregorian Proleptic calendar since or, if negative,
    before midnight on 1st January, 0001. Dates after the year 99,999 or before -99,999
    are not supported and leap seconds are ignored. Note that there is no year zero in this
    calendar and the year 1 AD immediately follows 1 BC which is therefore a leap year.
    It is also possible to store a time zone designator in a Date object even though Wren
    currently has no direct way of detecting its locale or the current local time.
    A Date object is immutable and its 'number' property can be used as a map key.
*/
class Date is Comparable {
    // Private method to initialize date tables and other static variables.
    static init_() {
        __diy  = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365]
        __diy2 = [0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366]
        __days = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]

        __mths = ["January", "February", "March", "April", "May", "June", "July", "August",
                  "September", "October", "November", "December"]

        __ords = ["First", "Second", "Third", "Fourth", "Fifth", "Sixth", "Seventh", "Eighth",
                  "Ninth", "Tenth", "Eleventh", "Twelfth", "Thirteenth", "Fourteenth",
                  "Fifteenth", "Sixteenth", "Seventeenth", "Eighteenth", "Nineteenth",
                  "Twentieth", "Twenty-first", "Twenty-second", "Twenty-third", "Twenty-fourth",
                  "Twenty-fifth", "Twenty-sixth", "Twenty-seventh", "Twenty-eighth",
                  "Twenty-ninth", "Thirtieth", "Thirty-first"]

        __caps   = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        __caps12 = "ABCDEFGHIKLM" // First 12 excluding 'J'

        __digs = "0123456789"

        __default = standard

        __localZone = "UTC"

        // Just North America and Europe.
        __tzs  = { "HST": "-1000", "HDT" : "-0900", "AKST": "-0900", "AKDT": "-0800", "PST": "-0800", 
                   "PDT": "-0700", "MST" : "-0700", "MDT" : "-0600", "CST" : "-0600", "CDT": "-0500",
                   "EST": "-0500", "EDT" : "-0400", "AST" : "-0400", "ADT" : "-0300", "NST": "-0330",
                   "NDT": "-0230", "PMST": "-0300", "PMDT": "-0200", "CVT" : "-0100", "UTC": "+0000",
                   "GMT": "+0000", "WET" : "+0000", "WEST": "+0100", "BST" : "+0100", "IST": "+0100",
                   "CET": "+0100", "CEST": "+0200", "EET" : "+0200", "EEST": "+0300", "MSK": "+0300" }
    }

    // Constants.
    static zero      { Date.new(1, 1, 1) }
    static maximum   { Date.new(99999, 12, 31, 23, 59, 59, 999) }
    static minimum   { Date.new(-99999, 1, 1, 0, 0, 0, 0) }
    static unixEpoch { Date.new(1970, 1, 1) }

    // Predefined formats.
    static standard { "yyyy|-|mm|-|dd| |hh|:|MM|:|ss|.|ttt" }       // readable & sortable format
    static isoFull  { "yyyy|-|mm|-|dd|T|hh|:|MM|:|ss|.|ttt|zzzzz" } // includes UTC offset
    static isoDate  { "yyyy|-|mm|-|dd"   }
    static rawDate  { "yyyy|mm|dd"       }
    static usDate   { "mm|/|dd|/|yyyy"   }
    static ukDate   { "dd|/|mm|/|yyyy"   }
    static isoTime  { "hh|:|MM|:|ss"     }
    static civilian { "mmmm| |d|, |yyyy" }

    // Gets or sets the default format to be used by toString.
    static default { __default }
    static default=(fmt) { __default = (fmt is List || fmt is String) ? fmt : standard }

    // Gets or sets the local time zone.
    static localZone { __localZone }
    static localZone=(tz) { __localZone = (tz = isValidTz_(tz) && tz != "J") ? tz  :__localZone }

    // Determines whether a particular year is a leap year.
    static isLeapYear(y) {
        if (y == 0) Fiber.abort("Year cannot be zero.")
        if (y < 0) y = y + 1
        return y%4 == 0 && (y%100 != 0 || y%400 == 0)
    }

    // Returns the year length for a given date.
    static yearLength(y) { isLeapYear(y) ? 366 : 365 }

    // Returns the month length for a given date.
    static monthLength(y, m) { isLeapYear(y) ? __diy2[m] - __diy2[m-1] : __diy[m] - __diy[m-1] }

    // Returns the year day for a given date.
    static yearDay(y, m, d) { d + (isLeapYear(y) ? __diy2[m-1]  : __diy[m-1]) }

    // private helper method for isoWeek method
    static isoWeekOne_(y) {
        var dt = Date.new(y, 1, 4)
        var dow = dt.dayOfWeek
        return dt.addDays(1 - dow) // first Monday <= 4 January
    }

    // Returns the year and week number therein for a given date as per ISO 8601.
    // See https://secondboyet.com/Articles/PublishedArticles/CalculatingtheISOweeknumb.html
    static isoWeek(y, m, d) {
        var dt = Date.new(y, m, d)
        var week1
        if (dt >= Date.new(y, 12, 29)) { // ISO year might be next year
            week1 = isoWeekOne_(y + 1)
            if (dt < week1) {
                week1 = isoWeekOne_(y)  // it's this year
            } else {
                y = y + 1               // it's next year
            }
        } else {                        // ISO year might be previous year
            week1 = isoWeekOne_(y)
            if (dt < week1) {
                y = y - 1               // it's previous year
                week1 = isoWeekOne_(y)
            }
        }
        return [y, ((dt - week1).days / 7).truncate + 1]
    }

    // Private helper method to check a string is all digits and convert it to an integer.
    static int_(str) {
        for (c in str) { // make sure string only contains digits
            if (!__digs.contains(c)) return null
        }
        return Num.fromString(str)
    }

   // Private helper method to get the longest integer, up to maxLen, from the start of a string.
    static sint_(str, maxLen) {
        if (str == "" || maxLen < 1) return null
        var c = str.count
        if (c < maxLen) maxLen = c
        var n = null
        for (i in 1..maxLen) {
            var t = int_(str[0...i])
            if (t) n = t else return n
        }
        return n
    }

    // Private helper method which fills an integer with leading zeros up to a given length.
    static zeroFill_(length, n) {
        n = "%(n)"
        return (n.count < length) ? "0" * (length - n.count) + n : n
    }

    // Private helper method to convert a single letter time zone designator to a UTC offset.
    static militaryToUtc_(letter) {
        if (letter == "J") return fmtTz_(__localZone, 4)
        if (letter == "Z") return "+0000"
        var ix = __caps12.indexOf(letter)
        if (ix >= 0) return "-" + zeroFill_(2, ix + 1) + "00"
        return "+" + zeroFill_(2, letter.bytes[0] - 77) + "00"
    }

    // Private helper method to convert any time zone designator to a single letter zone.
    static zoneToMilitary_(tz) {
        var c = tz.count
        if (c <= 1) return tz
        var hasName = !(tz[0] == "+" || tz[0] == "-")
        var offset = (hasName) ? __tzs[tz] : tz
        if (!offset) return "?"
        offset = offset[0..2]
        var n = Num.fromString(offset[1..2])
        if (n == 0) return "Z"
        return (offset[0] == "-") ? __caps12[n-1] : __caps[12 + n]
    }

    // Private helper method to check if a time zone is valid or potentially valid.
    // To be valid it must either be a name of between 1 and 5 consecutive capital letters
    // or be a UTC offset of the form ±hh:mm, ±hhmm or just ±hh. If it's an offset it will be
    // returned in ±hhmm format, otherwise as a name. If invalid, null will be returned.
    static isValidTz_(tz) {
        var c = tz.count
        if (c == 0) return null
        var hasName = !(tz[0] == "+" || tz[0] == "-")
        if (hasName) {
            if (c > 5) return null
            for (i in 0...tz.count) {
                if (!__caps.contains(tz[i])) return null
            }
            return tz
        }
        if (c < 3 || c == 4 || c > 6) return null
        if (c == 3) tz = tz + "00"
        if (c == 6) tz = tz.replace(":", "")
        if ((c = tz.count) != 5) return null
        for (i in 1...c) {
            if (!__digs.contains(tz[i])) return null
        }
        return tz
    }

    // Private helper method to format a time zone. Codes (number of 'z's) are:
    // 1 : Military (single capital letter)
    // 2 : Abbreviated name of a zone (2 to 5 capital letters)
    // 3 : Abbreviated UTC offset (±hh)
    // 4 : Raw offset (±hhmm)
    // 5 : Offset with colon separator (±hh:mm)
    static fmtTz_(tz, code) {
        if (code == 1) return (tz.count == 1) ? tz : zoneToMilitary_(tz)
        var hasName = !(tz[0] == "+" || tz[0] == "-")
        if (code == 2) return (hasName) ? tz : "??"
        var iso = hasName ? __tzs[tz] : tz
        if (!iso) iso = (tz.count == 1) ? militaryToUtc_(tz) : "+????"
        if (code == 3) return iso[0..2]
        if (code == 4) return iso
        return iso[0..2] + ":" + iso[3..4]
    }

    // Private helper method to parse a UTC offset into hours and minutes.
    static parseOffset_(offset) {
        var sign = offset[0]
        var mins = Num.fromString(offset[1..-1])
        var hrs = (mins/100).truncate
        mins = mins % 100
        if (sign == "-") {
            hrs = -hrs
            mins = -mins
        }
       return [hrs, mins]
    }

    // Parses a 'full' IS0 8601 string into a Date object, provided that ANY single character
    // separator may be used in place of dash, space, colon or dot and the following parts
    // may be omitted: the UTC part, the UTC and time parts, the UTC and secs/msecs part or just
    // the msecs part of the time. Any trailing literal or unparseable text is ignored.
    // BC date strings are distinguished by always ending with " BC".
    static parse(str) {
        str = str.trim()
        var c = str.count
        if (c < 10) Fiber.abort("Unparseable date format string.")
        var y  = int_(str[0..3])
        if (str.endsWith(" BC")) y = -y
        var mo = int_(str[5..6])
        var d  = int_(str[8..9])
        if (c < 16) return Date.new(y, mo, d)
        var h  = int_(str[11..12])
        var mi = int_(str[14..15])
        if (c < 19) return Date.new(y, mo, d, h, mi)
        var s  = int_(str[17..18])
        if (c < 23) return Date.new(y, mo, d, h, mi, s)
        var ms = int_(str[20..22])
        if (c < 29) return Date.new(y, mo, d, h, mi, s, ms)
        var tz = str[23..28]
        return Date.new(y, mo, d, h, mi, s, ms, tz)
    }

    // Parses a date string into a Date object using the supplied format, provided that literal
    // text is only matched by length (not content) and trailing literal text is ignored altogether.
    // A default value is used for any missing constructor parameters. See 'format' for mask meanings.
    // BC date strings are distinguished by always ending with " BC" and this suffix should not
    // be included in the format itself.
    static parse(str, fmt) {
        if (fmt == Date.standard || fmt == Date.isoFull || fmt == Date.isoDate) return parse(str)
        str = str.trim()
        if (fmt is String) fmt = fmt.split("|")
        var y = 1
        var mo = 1
        var d = 1
        var h = 0
        var mi = 0
        var s = 0
        var ms = 0
        var tz = __localZone
        var used = 0
        for (i in 0...fmt.count) {
            var f = fmt[i]
            var c = str.count
            if (f == "y") {
                if (c < 1 || !(y = sint_(str, 5))) Fiber.abort("Unable to parse year.")
                str = (y > 0) ? str.trimStart("0") : "0" + str.trimStart("0")
                used = "%(y)".count
            } else if (f == "yy") {
                if (c < 2 || !(y = int_(str[0..1]))) Fiber.abort("Unable to parse year.")
                y = y + 2000
                used = 2
            } else if (f == "yyy" || f == "yyyy" || f == "yyyyy") {
                var fc = f.count
                if (c < fc || !(y = int_(str[0...fc]))) Fiber.abort("Unable to parse year.")
                used = fc
            } else if (f == "m") {
                if (c < 1 || !(mo = sint_(str, 2))) Fiber.abort("Unable to parse month.")
                str = (mo > 0) ? str.trimStart("0") : "0" + str.trimStart("0")
                used = "%(mo)".count
            } else if (f == "mm") {
                if (c < 2 || !(mo = int_(str[0..1]))) Fiber.abort("Unable to parse month.")
                used = 2
            } else if (f == "mmm") {
                if (c < 3) Fiber.abort("Unable to parse abbreviated month name.")
                var t = str[0..2]
                var found = false
                for (i in 0..11) {
                    if (__mths[i].startsWith(t)) {
                        found = true
                        mo = i + 1
                        break
                    }
                }
                if (!found) Fiber.abort("Unable to parse abbreviated month name.")
                used = 3
            } else if (f == "mmmm") {
                if (c < 3) Fiber.abort("Unable to parse month name.")
                var found = false
                for (i in 0..11) {
                    if (str.startsWith(__mths[i])) {
                        found = true
                        mo = i + 1
                        used = __mths[i].count
                        break
                    }
                }
                if (!found) Fiber.abort("Unable to parse month name.")
            } else if (f == "d") {
                if (c < 1 || !(d = sint_(str, 2))) Fiber.abort("Unable to parse day.")
                str = (d > 0) ? str.trimStart("0") : "0" + str.trimStart("0")
                used = "%(d)".count
             } else if (f == "dd") {
                if (c < 2 || !(d = int_(str[0..1]))) Fiber.abort("Unable to parse day.")
                used = 2
            } else if (f == "ddd") { // cannot deduce 'day' from this but check anyway
                  if (c < 3) Fiber.abort("Unable to parse abbreviated day name.")
                  var t = str[0..2]
                  var found = false
                  for (i in 0..6) {
                    if (__days[i].startsWith(t)) {
                        found = true
                        break
                    }
                }
                if (!found) Fiber.abort("Unable to parse abbreviated day name.")
                used = 3
            } else if (f == "dddd") {
                if (c < 6) Fiber.abort("Unable to parse day name.")
                var found = false
                for (i in 0..6) {
                    if (str.startsWith(__days[i])) {
                        found = true
                        used = __days[i].count
                        break
                    }
                }
                if (!found) Fiber.abort("Unable to parse day name.")
            } else if (f == "ooo") { // not worth checking that suffix ties in with day
                if (c < 3 || !(d = sint_(str, 2))) Fiber.abort("Unable to parse day ordinal")
                str = (d > 0) ? str.trimStart("0") : "0" + str.trimStart("0")
                used = "%(d)".count
                var sfx = str[used..used+1]
                if (!["th", "st", "nd", "rd"].contains(sfx)) {
                    Fiber.abort("Unable to parse day ordinal.")
                }
                used = used + 2
            } else if (f == "oooo") {
                if (c < 5) Fiber.abort("Unable to parse day ordinal name.")
                var found = false
                for (i in 0..30) {
                    if (str.startsWith(__ords[i])) {
                        found = true
                        d = i + 1
                        used = __ords[i].count
                        break
                    }
                }
                if (!found) Fiber.abort("Unable to parse day ordinal name.")
            } else if (f == "h") {
                if (c < 1 || !(h = sint_(str, 2))) Fiber.abort("Unable to parse hour.")
                str = (h > 0) ? str.trimStart("0") : "0" + str.trimStart("0")
                used = "%(h)".count
            } else if (f == "hh") {
                if (c < 2 || !(h = int_(str[0..1]))) Fiber.abort("Unable to parse hour.")
                used = 2
            } else if (f == "H") {
                if (c < 1 || !(h = sint_(str, 2))) Fiber.abort("Unable to parse hour.")
                str = (h > 0) ? str.trimStart("0") : "0" + str.trimStart("0")
                used = "%(h)".count
                if (h == 12) h = 0
            } else if (f == "HH") {
                if (c < 2 || !(h = int_(str[0..1]))) Fiber.abort("Unable to parse hour.")
                if (h == 12) h = 0
                used = 2
            } else if (f == "M") {
                if (c < 1 || !(mi = sint_(str, 2))) Fiber.abort("Unable to parse minute.")
                str = (mi > 0) ? str.trimStart("0") : "0" + str.trimStart("0")
                used = "%(mi)".count
            } else if (f == "MM") {
                if (c < 2 || !(mi = int_(str[0..1]))) Fiber.abort("Unable to parse minute.")
                used = 2
            } else if (f == "s") {
                if (c < 1 || !(s = sint_(str, 2))) Fiber.abort("Unable to parse second.")
                str = (s > 0) ? str.trimStart("0") : "0" + str.trimStart("0")
                used = "%(s)".count
            } else if (f == "ss") {
                if (c < 2 || !(s = int_(str[0..1]))) Fiber.abort("Unable to parse second.")
                used = 2
            } else if (f == "t") {
                if (c < 1 || !(ms = sint_(str, 3))) Fiber.abort("Unable to parse millisecond.")
                str = (ms > 0) ? str.trimStart("0") : "0" + str.trimStart("0")
                used = "%(ms)".count
            } else if (f == "ttt") {
                if (c < 3 || !(ms = int_(str[0..2]))) Fiber.abort("Unable to parse millisecond.")
                used = 3
            } else if (f == "am" || f == "AM") {
                if (h < 12) h = h + 12
                used = 2
            } else if (f == "z") {
                if (c < 1 || !__caps.contains(str[0])) Fiber.abort("Unable to parse military time zone.")
                used = 1
            } else if (f == "zz") {
                if (c < 2) Fiber.abort("Unable to parse time zone name.")
                tz = str[0]
                var maxLen = 5
                if (c < maxLen) maxLen = c
                for (i in 1...maxLen) {
                    if (!__caps.contains(str[i])) {
                        used = tz.count
                        break
                    }
                    tz = tz + str[i]
                }
            } else if (f == "zzz" || f == "zzzz") {
                var fc = f.count + 2
                if (c < fc) Fiber.abort("Unable to parse time zone UTC offset.")
                tz = str[0...fc]
                used = fc
            } else if (i == fmt.count - 1) {
                break // not bothered about length of trailing literal text
            } else {
                f = f.replace("\f", "").replace("\v", "|")
                if (c < f.count) Fiber.abort("Unable to parse literal text.")
                used = f.count
            }
            str = str[used..-1]
        }
        if (str.endsWith(" BC")) y = -y
        return Date.new(y, mo, d, h, mi, s, ms, tz)
    }

    // Private helper method to get the number of days from Date.zero up to the start of a
    // year AD or down to the start of a year BC.
    static soyDays_(y) {
        if (y > 0) {
            y = y - 1
            return y*365 + (y/4).truncate - (y/100).truncate + (y/400).truncate
        } else {
            y = y + 1
            return y*365 + (y/4).truncate - (y/100).truncate + (y/400).truncate - 366
        }
    }

    // Private helper method which adds an ordinal suffix to a day number.
    static ord_(n) {
        var suffix = "th"
        if (n == 1 || n == 21 || n == 22) {
            suffix = "st"
        } else if (n == 2 || n == 22) {
            suffix = "nd"
        } else if (n == 3 || n == 23) {
            suffix = "rd"
        }
        return "%(n)" + suffix
    }

    // Constructs a new Date object by passing it: the year, month, day, hour, minute, second
    // and millisecond of an instance in time plus a time zone designator. A negative year
    // should be passed for a BC date. It is a runtime error to pass invalid values
    // though, for convenience, if the number of days is more than the month has, 
    // they will wrap around to the following month.
    construct new(y, mo, d, h, mi, s, ms, tz) {  
        if (!(y is Num && y.isInteger && y.abs <= 99999)) {
            Fiber.abort("The year must be a non-zero integer in the range [-99,999, 99,999].")
        }
        if (y == 0) Fiber.abort("Year cannot be zero.")
        if (!(mo is Num && mo.isInteger && mo >= 1 && mo <= 12)) {
            Fiber.abort("The month must be an integer in the range [1, 12].")
        }
        if (!(d is Num && d.isInteger && d >= 1 && d <= 31)) {
            Fiber.abort("The day must be an integer in the range [1, 31].")
        }
        if (!(h is Num && h.isInteger && h >= 0 && h <= 23)) {
            Fiber.abort("The hour must be an integer in the range [0, 23].")
        }
        if (!(mi is Num && mi.isInteger && mi >= 0 && mi <= 59)) {
            Fiber.abort("The minute must be an integer in the range [0, 59].")
        }
        if (!(s is Num && s.isInteger && s >= 0 && s <= 59)) {
            Fiber.abort("The second must be an integer in the range [0, 59].")
        }
        if (!(ms is Num && ms.isInteger && ms >= 0 && ms <= 999)) {
            Fiber.abort("The millisecond must be an integer in the range [0, 999].")
        }
        if (!(tz = Date.isValidTz_(tz))) Fiber.abort("Invalid time zone designator.")
        var days = Date.soyDays_(y)
        days = days + d - 1 + (Date.isLeapYear(y) ? __diy2[mo-1] : __diy[mo-1])
        _num = days * 86400000 + h * 3600000 + mi * 60000 + s * 1000 + ms
        _tz = tz
    }

    // Constructor for creating a Date object directly from a number of milliseconds.
    construct fromNumber(num, tz) {
        if (num < Date.minimum.number || num > Date.maximum.number) {
            Fiber.abort("Number is out of range.")
        }
        if (!(tz = Date.isValidTz_(tz))) Fiber.abort("Invalid time zone designator.")
        _num = (num == -0) ? 0 : num.floor
        _tz = tz
    }

    // Convenience methods to construct a Date object from a subset of its parameters.
    static new(y, mo, d, h, mi, s, ms) { Date.new(y, mo, d, h, mi, s, ms, "UTC") } 
    static new(y, mo, d, h, mi, s)     { Date.new(y, mo, d, h, mi, s,  0, "UTC") } 
    static new(y, mo, d, h, mi)        { Date.new(y, mo, d, h, mi, 0,  0, "UTC") } 
    static new(y, mo, d)               { Date.new(y, mo, d, 0,  0, 0,  0, "UTC") }
    static new(y)                      { Date.new(y,  1, 1, 0,  0, 0,  0, "UTC") }

    static fromNumber(num)             { Date.fromNumber(num, "UTC") }

    // Returns the Gregorian Proleptic date equivalent to a Julian date given in y/m/d format.
    // Julian dates before 3 March AD 4 are problematic and are not supported.
    // If 'ay' is 1582, the first omitted date is 5th October 1582 or, if 'ay' is 1752,
    // 3rd September 1752. Years are always assumed to begin on 1st January and leap years
    // to contain 29th February. Omitted Julian dates map respectively to 15th October 1582 or
    // 14th September 1752. Subsequent dates are returned unchanged as Gregorian dates.
    // If 'ay' is 2200, conversions continue up to 14th February 2200 but no further.
    static fromJulian(y, m, d, ay) {
        var dt = Date.new(y, m, d)
        var lya = 0
        if ((y%100 == 0) && (y%400 != 0) && m == 2 && d == 29) lya = -1
        if (ay != 1582 && ay != 1752 && ay != 2200) Fiber.abort("Invalid adoption year.")
        if (dt <= Date.new(   3,  3,  2))  Fiber.abort("Unsupported Julian date.")
        if (dt <= Date.new( 100,  2,  28)) return dt.addDays(-2)
        if (dt <= Date.new( 200,  2, 28))  return dt.addDays(-1 + lya)
        if (dt <= Date.new( 300,  2, 28))  return dt.addDays(0 + lya)
        if (dt <= Date.new( 500,  2, 28))  return dt.addDays(1 + lya) 
        if (dt <= Date.new( 600,  2, 28))  return dt.addDays(2 + lya)
        if (dt <= Date.new( 700,  2, 28))  return dt.addDays(3 + lya)
        if (dt <= Date.new( 900,  2, 28))  return dt.addDays(4 + lya)
        if (dt <= Date.new(1000,  2, 28))  return dt.addDays(5 + lya)
        if (dt <= Date.new(1100,  2, 28))  return dt.addDays(6 + lya)
        if (dt <= Date.new(1300,  2, 28))  return dt.addDays(7 + lya)
        if (dt <= Date.new(1400,  2, 28))  return dt.addDays(8 + lya)
        if (dt <= Date.new(1500,  2, 28))  return dt.addDays(9 + lya)
        if (dt <= Date.new(1582, 10,  4))  return dt.addDays(10 + lya)
        if (dt <= Date.new(1582, 10, 14))  return ay == 1582 ? Date.new(1582, 10, 15) : dt.addDays(10)
        if (ay == 1582) return dt
        if (dt <= Date.new(1700,  2, 28))  return dt.addDays(10 + lya)
        if (dt <= Date.new(1752,  9,  2))  return dt.addDays(11 + lya)
        if (dt <= Date.new(1752,  9, 13))  return ay == 1752 ? Date.new(1752, 9, 14) : dt.addDays(11)
        if (ay == 1752) return dt
        if (dt <= Date.new(1800,  2, 28))  return dt.addDays(11 + lya)
        if (dt <= Date.new(1900,  2, 28))  return dt.addDays(12 + lya)
        if (dt <= Date.new(2100,  2, 28))  return dt.addDays(13 + lya)
        if (dt <= Date.new(2200,  2, 28))  return dt.addDays(14 + lya)
        Fiber.abort("Unsupported Julian date.")
    }

    // As above method but always uses a 1582 adoption year.
    static fromJulian(y, m, d) { fromJulian(y, m, d, 1582) }

    // Gets the component parts of this date, as a list, from its number
    // and in the same order as the constructor parameters above.
    parts {
        var days = (_num/86400000).floor
        var time = _num  - days * 86400000
        var h = (time/3600000).floor
        time = time % 3600000
        var mi = (time/60000).floor
        time = time % 60000
        var s = (time/1000).floor
        var ms = time % 1000
        var y = (days/365).truncate
        if (days >= 0) y = y + 1 else y = y - 1
        while(true) {
            var soyd = Date.soyDays_(y)
            var diff = days - soyd
            if (diff >= 0) {
                if (!Date.isLeapYear(y) && diff <= 364) {
                    var mo = 0
                    for (i in 0..11) {
                        if (diff < __diy[i]) {
                            mo = i
                            break
                        }
                    }
                    if (mo == 0) mo = 12
                    var d = diff - __diy[mo-1] + 1
                    return [y, mo, d, h, mi, s, ms, _tz]
                } else if (Date.isLeapYear(y) && diff <= 365) {
                    var mo = 0
                    for (i in 0..11) {
                        if (diff < __diy2[i]) {
                            mo = i
                            break
                        }
                    }
                    if (mo == 0) mo = 12
                    var d = diff - __diy2[mo-1] + 1
                    return [y, mo, d, h, mi, s, ms, _tz]
                }
            }
            y = (days >= 0) ? y - 1 : y + 1
        }
    }

    // Methods to get this date's basic properties.
    year     { parts[0] }
    month    { parts[1] }
    day      { parts[2] }
    hour     { parts[3] }
    minute   { parts[4] }
    second   { parts[5] }
    millisec { parts[6] }

    // Return a new Date object after adding positive (or negative) integral increments.
    addYears(y) { 
        var z = year + y
        if (z - y > 0 && z <= 0) {
            z = z - 1
        } else if (z - y < 0 && z >= 0) {
            z = z + 1
        }
        return Date.new(z, month, day, hour, minute, second, millisec, _tz)
    }

    addMonths(mo) {
        var y = (mo/12).truncate
        if ((mo = mo%12) == 0) return addYears(y)
        var m = month
        if (mo >= 0) {
            if ((m + mo) <= 12) {
                var d  = Date.new(year, m + mo, day, hour, minute, second, millisec, _tz)
                return d.addYears(y)
            }
            var d = Date.new(year, m + mo - 12, day, hour, minute, second, millisec, _tz)
            return d.addYears(y + 1)
        }
        if ((m + mo) >= 1) {
            var d = Date.new(year, m + mo, day, hour, minute, second, millisec, _tz)
            return d.addYears(y)
        }
        var d = Date.new(year, m + mo + 12, day, hour, minute, second, millisec, _tz)
        return d.addYears(y - 1)
    }

    // Increments for these methods need not be integral
    // but will be rounded to the lower millisecond where necessary.
    addWeeks(w)      { Date.fromNumber(_num + w * 86400000 * 7, _tz) }
    addDays(d)       { Date.fromNumber(_num + d * 86400000, _tz)     }
    addHours(h)      { Date.fromNumber(_num + h * 3600000, _tz)      }
    addMinutes(mi)   { Date.fromNumber(_num + mi * 60000, _tz)       }
    addSeconds(s)    { Date.fromNumber(_num + s * 1000, _tz)         }
    addMillisecs(ms) { Date.fromNumber(_num + ms, _tz)               }

    // Returns the day of the year in which this date falls.
    dayOfYear { day + (Date.isLeapYear(year) ? __diy2[month-1] : __diy[month-1]) }

    dayOfWeek { (_num/86400000).truncate % 7 + 1 } // as an integer (1 to 7), Monday = 1

    weekDay  { __days[dayOfWeek-1] } // as a string

    monthName { __mths[month-1] }    // ditto

    number { _num } // gets the number of miiliseconds since, or before, Date.zero

    unixTime { ((_num - Date.unixEpoch.number)/1000).truncate } // can be negative

    // Returns the ISO year and week in which this date falls.
    weekOfYear { Date.isoWeek(year, month, day) }

    // Returns the time zone designator for this date.
    zone { _tz }

    // Returns a new date object with the new time zone. Doesn't adjust the time.
    changeZone(newZone) { Date.fromNumber(_num, newZone) }

    // Attempts to adjust the time to a new time zone. If successful, returns a 
    // new Date object otherwise returns null.
    adjustTime(newZone) {
        if (newZone == _tz) return this // no adjustment needed
        var hasName = !(_tz[0] == "+" || _tz[0] == "-")
        var oldOffset
        if (hasName && !(oldOffset = __tzs[_tz])) return null
        if (!(newZone = Date.isValidTz_(newZone))) return null
        hasName = !(newZone[0] == "+" || newZone[0] == "-")
        var newOffset
        if (hasName && !(newOffset = __tzs[newZone])) return null
        if (oldOffset == newOffset) return Date.fromNumber(_num, newZone) // no time adjustment needed
        var ohm = Date.parseOffset_(oldOffset)
        var nhm = Date.parseOffset_(newOffset)
        var d = Date.fromNumber(_num, newZone).addHours(nhm[0]).addMinutes(nhm[1])
        return d.addHours(-ohm[0]).addMinutes(-ohm[1])
    }

    // The inherited 'clone' method just returns 'this' as Date objects are immutable.
    // If you need an actual copy use this method instead.
    copy() { Date.fromNumber(_num, _tz) }
 
    // Compares this date with another one to enable comparison operators via Comparable trait.
    compare(other) { (_num - other.number).sign }

    // Constructs the string representation of this date from a 'fmt' list or string.
    // To treat a part of the format literally (and avoid a clash with a standard mask)
    // insert a '\f' within it which will otherwise be ignored.
    // If 'fmt' is a string then its components should be separated by '|'s. However, to use '|'
    // literally, replace it with a '\v' which will then be changed back during processing.
    // BC date strings are automatically suffixed with " BC" and this suffix should not be
    // included in the format itself.
    format(fmt) {
        if (fmt is String) fmt = fmt.split("|")
        var str = ""
        var p = parts
        for (f in fmt) {
            if (f == "y") {
                str = str + "%(p[0].abs)"                       // minimum digit year
            } else if (f == "yy") {
                str = str + Date.zeroFill_(2, p[0].abs % 100)   // 2 digit year
            } else if (f == "yyy") {
                str = str + Date.zeroFill_(3, p[0].abs % 1000)  // 3 digit year
            } else if (f == "yyyy") {
                str = str + Date.zeroFill_(4, p[0].abs % 10000) // 4 digit year
            } else if (f == "yyyyy") {
                str = str + Date.zeroFill_(5, p[0].abs)         // 5 digit year
            } else if (f == "m") {
                str = str + "%(p[1])"                           // minimum digit month
            } else if (f == "mm") {
                str = str + Date.zeroFill_(2, p[1])             // 2 digit month
            } else if (f == "mmm") {
                str = str + monthName[0..2]                     // abbreviated month name
            } else if (f == "mmmm") {
                str = str + monthName                           // full month name
            } else if (f == "d") {
                str = str + "%(p[2])"                           // minimum digit day
            } else if (f == "dd") {
                str = str + Date.zeroFill_(2, p[2])             // 2 digit day
            } else if (f == "ddd") {
                str = str + weekDay[0..2]                       // abbreviated day name
            } else if (f == "dddd") {
                str = str + weekDay                             // full day name
            } else if (f == "ooo") {
                str = str + Date.ord_(p[2])                     // ordinal day number
            } else if (f == "oooo") {
                str = str + __ords[p[2]-1]                      // full ordinal day name
            } else if (f == "h") {
                str = str + "%(p[3])"                           // mimimum digit hour (24 hr)
            } else if (f == "hh") {
                str = str + Date.zeroFill_(2, p[3])             // 2 digit hour (24 hr)
            } else if (f == "H") {
                var hr = p[3] % 12
                if (hr == 0) hr = 12
                str = str + "%(hr)"                             // mimimum digit hour (12 hr)
            } else if (f == "HH") {
                var hr = p[3] % 12
                if (hr == 0) hr = 12
                str = str + Date.zeroFill_(2, hr)               // 2 digit hour (12 hr)
            } else if (f == "M") {
                str = str + "%(p[4])"                           // minimum digit minute
            } else if (f == "MM") {
                str = str + Date.zeroFill_(2, p[4])             // 2 digit minute
            } else if (f == "s") {
                str = str + "%(p[5])"                           // minimum digit second
            } else if (f == "ss") {
                str = str + Date.zeroFill_(2, p[5])             // 2 digit second
            } else if (f == "t") {
                str = str + "%(p[6])"                           // minimum digit millisecond
            } else if (f == "ttt") {
                str = str + Date.zeroFill_(3, parts[6])         // 3 digit millisecond
            } else if (f == "am") {
                str = str + ((p[3] < 12) ? "am" : "pm")         // am/pm designation
            } else if (f == "AM") {
                str = str + ((p[3] < 12) ? "AM" : "PM")         // AM/PM designation
            } else if (f == "z" || f == "zz" || f == "zzz" ||
                       f == "zzzz" || f == "zzzzz") {           // time zone designations
                str = str + Date.fmtTz_(_tz, f.count)
            } else {
                f = f.replace("\f", "").replace("\v", "|")
                str = str + f                                   // literal string
            }
        }
        if (p[0] < 0) str = str + " BC"
        return str
    }

    // Returns the string representation of this date using the default format
    // adjusted where necessary to deal with 5 digit years.
    toString {
        if (year.abs < 10000) {
            return format(Date.default)
        } else {
            var d = Date.default
            if (d[0..4] == "yyyy|" || d.contains("|yyyy|")) d = d.replace("yyyy", "yyyyy")
            return format(d)
        }
    }

    // Returns the duration (positive or negative milliseconds) from this to another date.
    -(other) { Duration.new(_num - other.number) }
}

/*
    Duration represents a time interval (positive or negative) measured in milliseconds.
    It is immutable and any of its properties can therefore be used as a map key.
*/
class Duration is Comparable {
    // Maximum safe integer (2^53 - 1) and hence duration
    static maximum { 9007199254740991 }

    // Blocks the current fiber for a given number of milliseconds.
    static wait(ms) {
        if (ms.type != Num || ms < 0) {
            Fiber.abort("Argument must be a non-negative number of milliseconds.")
        }
        var finish = System.clock + ms/1000
        while (System.clock <= finish) {}
    }

    // Constructs a new Duration object by passing it a number (positive or negative) of:
    // days, hours, minutes, seconds and milliseconds.
    construct new(d, h, mi, s, ms) {
        ms = d*86400000 + h*3600000 + mi*60000 + s*1000 + ms
        if (ms.abs > Duration.maximum) Fiber.abort("Duration is out of safe range.")
        _ms = ms
    }

    // Convenience method to construct a Duration object from just a number of milliseconds.
    static new(ms) { Duration.new(0, 0, 0, 0, ms) }

    // Gets the duration in various time intervals (as a floating point number).
    millisecs { _ms }
    seconds   { _ms/1000 }
    minutes   { _ms/60000 }
    hours     { _ms/3600000 }
    days      { _ms/86400000 }

    // The inherited 'clone' method just returns 'this' as Duration objects are immutable.
    // If you need an actual copy use this method instead.
    copy() { Duration.new(_ms) }

    // Compares this duration with another one to enable comparison operators via Comparable trait.
    compare(other) { (_ms - other.millisecs).sign }

    // Duration arithmetic.
    +(other)  { Duration.new(_ms + other.millisecs) }
    -(other)  { Duration.new(_ms - other.millisecs) }

    // Returns the string representation of this duration.
    toString  { _ms.toString }
}

/* Stopwatch enables one to easily time events. */
class Stopwatch {
    // Times the execution of a function returning the duration it took to execute.
    static time(fn) {
        var sw = Stopwatch.new()
        fn.call()
        var dur = sw.duration
        sw.stop()
        return dur
    }

    // Creates a new Stopwatch object and starts it, recording the time.
    construct new() { _start = System.clock }

    // Returns the duration since 'start'.
    duration { Duration.new(((System.clock - _start)*1000).round) }

    // Returns the elapsed time since 'start' in milliseconds.
    elapsed  { duration.millisecs }

    // Prevents this Stopwatch object from being used again.
    stop() { _start = null }
}

// Initialize Date tables.
Date.init_()

