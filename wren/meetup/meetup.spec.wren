import "./meetup" for Meetup
import "wren-testie/testie" for Testie, Expect

Testie.test("Meetup") { |do, skip|
  do.test("when teenth Monday is the 13th, the first day of the teenth week") {
    var expected = "May 13, 2013"
    var received = Meetup.date("the teenth Monday of May 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when teenth Monday is the 19th, the last day of the teenth week") {
    var expected = "August 19, 2013"
    var received = Meetup.date("the teenth Monday of August 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when teenth Monday is some day in the middle of the teenth week") {
    var expected = "September 16, 2013"
    var received = Meetup.date("the teenth Monday of September 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when teenth Tuesday is the 19th, the last day of the teenth week") {
    var expected = "March 19, 2013"
    var received = Meetup.date("the teenth Tuesday of March 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when teenth Tuesday is some day in the middle of the teenth week") {
    var expected = "April 16, 2013"
    var received = Meetup.date("the teenth Tuesday of April 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when teenth Tuesday is the 13th, the first day of the teenth week") {
    var expected = "August 13, 2013"
    var received = Meetup.date("the teenth Tuesday of August 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when teenth Wednesday is some day in the middle of the teenth week") {
    var expected = "January 16, 2013"
    var received = Meetup.date("the teenth Wednesday of January 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when teenth Wednesday is the 13th, the first day of the teenth week") {
    var expected = "February 13, 2013"
    var received = Meetup.date("the teenth Wednesday of February 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when teenth Wednesday is the 19th, the last day of the teenth week") {
    var expected = "June 19, 2013"
    var received = Meetup.date("the teenth Wednesday of June 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when teenth Thursday is some day in the middle of the teenth week") {
    var expected = "May 16, 2013"
    var received = Meetup.date("the teenth Thursday of May 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when teenth Thursday is the 13th, the first day of the teenth week") {
    var expected = "June 13, 2013"
    var received = Meetup.date("the teenth Thursday of June 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when teenth Thursday is the 19th, the last day of the teenth week") {
    var expected = "September 19, 2013"
    var received = Meetup.date("the teenth Thursday of September 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when teenth Friday is the 19th, the last day of the teenth week") {
    var expected = "April 19, 2013"
    var received = Meetup.date("the teenth Friday of April 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when teenth Friday is some day in the middle of the teenth week") {
    var expected = "August 16, 2013"
    var received = Meetup.date("the teenth Friday of August 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when teenth Friday is the 13th, the first day of the teenth week") {
    var expected = "September 13, 2013"
    var received = Meetup.date("the teenth Friday of September 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when teenth Saturday is some day in the middle of the teenth week") {
    var expected = "February 16, 2013"
    var received = Meetup.date("the teenth Saturday of February 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when teenth Saturday is the 13th, the first day of the teenth week") {
    var expected = "April 13, 2013"
    var received = Meetup.date("the teenth Saturday of April 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when teenth Saturday is the 19th, the last day of the teenth week") {
    var expected = "October 19, 2013"
    var received = Meetup.date("the teenth Saturday of October 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when teenth Sunday is the 19th, the last day of the teenth week") {
    var expected = "May 19, 2013"
    var received = Meetup.date("the teenth Sunday of May 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when teenth Sunday is some day in the middle of the teenth week") {
    var expected = "June 16, 2013"
    var received = Meetup.date("the teenth Sunday of June 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when teenth Sunday is the 13th, the first day of the teenth week") {
    var expected = "October 13, 2013"
    var received = Meetup.date("the teenth Sunday of October 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when first Monday is some day in the middle of the first week") {
    var expected = "March 4, 2013"
    var received = Meetup.date("the first Monday of March 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when first Monday is the 1st, the first day of the first week") {
    var expected = "April 1, 2013"
    var received = Meetup.date("the first Monday of April 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when first Tuesday is the 7th, the last day of the first week") {
    var expected = "May 7, 2013"
    var received = Meetup.date("the first Tuesday of May 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when first Tuesday is some day in the middle of the first week") {
    var expected = "June 4, 2013"
    var received = Meetup.date("the first Tuesday of June 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when first Wednesday is some day in the middle of the first week") {
    var expected = "July 3, 2013"
    var received = Meetup.date("the first Wednesday of July 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when first Wednesday is the 7th, the last day of the first week") {
    var expected = "August 7, 2013"
    var received = Meetup.date("the first Wednesday of August 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when first Thursday is some day in the middle of the first week") {
    var expected = "September 5, 2013"
    var received = Meetup.date("the first Thursday of September 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when first Thursday is another day in the middle of the first week") {
    var expected = "October 3, 2013"
    var received = Meetup.date("the first Thursday of October 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when first Friday is the 1st, the first day of the first week") {
    var expected = "November 1, 2013"
    var received = Meetup.date("the first Friday of November 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when first Friday is some day in the middle of the first week") {
    var expected = "December 6, 2013"
    var received = Meetup.date("the first Friday of December 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when first Saturday is some day in the middle of the first week") {
    var expected = "January 5, 2013"
    var received = Meetup.date("the first Saturday of January 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when first Saturday is another day in the middle of the first week") {
    var expected = "February 2, 2013"
    var received = Meetup.date("the first Saturday of February 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when first Sunday is some day in the middle of the first week") {
    var expected = "March 3, 2013"
    var received = Meetup.date("the first Sunday of March 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when first Sunday is the 7th, the last day of the first week") {
    var expected = "April 7, 2013"
    var received = Meetup.date("the first Sunday of April 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when second Monday is some day in the middle of the second week") {
    var expected = "March 11, 2013"
    var received = Meetup.date("the second Monday of March 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when second Monday is the 8th, the first day of the second week") {
    var expected = "April 8, 2013"
    var received = Meetup.date("the second Monday of April 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when second Tuesday is the 14th, the last day of the second week") {
    var expected = "May 14, 2013"
    var received = Meetup.date("the second Tuesday of May 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when second Tuesday is some day in the middle of the second week") {
    var expected = "June 11, 2013"
    var received = Meetup.date("the second Tuesday of June 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when second Wednesday is some day in the middle of the second week") {
    var expected = "July 10, 2013"
    var received = Meetup.date("the second Wednesday of July 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when second Wednesday is the 14th, the last day of the second week") {
    var expected = "August 14, 2013"
    var received = Meetup.date("the second Wednesday of August 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when second Thursday is some day in the middle of the second week") {
    var expected = "September 12, 2013"
    var received = Meetup.date("the second Thursday of September 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when second Thursday is another day in the middle of the second week") {
    var expected = "October 10, 2013"
    var received = Meetup.date("the second Thursday of October 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when second Friday is the 8th, the first day of the second week") {
    var expected = "November 8, 2013"
    var received = Meetup.date("the second Friday of November 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when second Friday is some day in the middle of the second week") {
    var expected = "December 13, 2013"
    var received = Meetup.date("the second Friday of December 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when second Saturday is some day in the middle of the second week") {
    var expected = "January 12, 2013"
    var received = Meetup.date("the second Saturday of January 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when second Saturday is another day in the middle of the second week") {
    var expected = "February 9, 2013"
    var received = Meetup.date("the second Saturday of February 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when second Sunday is some day in the middle of the second week") {
    var expected = "March 10, 2013"
    var received = Meetup.date("the second Sunday of March 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when second Sunday is the 14th, the last day of the second week") {
    var expected = "April 14, 2013"
    var received = Meetup.date("the second Sunday of April 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when third Monday is some day in the middle of the third week") {
    var expected = "March 18, 2013"
    var received = Meetup.date("the third Monday of March 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when third Monday is the 15th, the first day of the third week") {
    var expected = "April 15, 2013"
    var received = Meetup.date("the third Monday of April 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when third Tuesday is the 21st, the last day of the third week") {
    var expected = "May 21, 2013"
    var received = Meetup.date("the third Tuesday of May 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when third Tuesday is some day in the middle of the third week") {
    var expected = "June 18, 2013"
    var received = Meetup.date("the third Tuesday of June 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when third Wednesday is some day in the middle of the third week") {
    var expected = "July 17, 2013"
    var received = Meetup.date("the third Wednesday of July 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when third Wednesday is the 21st, the last day of the third week") {
    var expected = "August 21, 2013"
    var received = Meetup.date("the third Wednesday of August 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when third Thursday is some day in the middle of the third week") {
    var expected = "September 19, 2013"
    var received = Meetup.date("the third Thursday of September 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when third Thursday is another day in the middle of the third week") {
    var expected = "October 17, 2013"
    var received = Meetup.date("the third Thursday of October 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when third Friday is the 15th, the first day of the third week") {
    var expected = "November 15, 2013"
    var received = Meetup.date("the third Friday of November 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when third Friday is some day in the middle of the third week") {
    var expected = "December 20, 2013"
    var received = Meetup.date("the third Friday of December 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when third Saturday is some day in the middle of the third week") {
    var expected = "January 19, 2013"
    var received = Meetup.date("the third Saturday of January 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when third Saturday is another day in the middle of the third week") {
    var expected = "February 16, 2013"
    var received = Meetup.date("the third Saturday of February 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when third Sunday is some day in the middle of the third week") {
    var expected = "March 17, 2013"
    var received = Meetup.date("the third Sunday of March 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when third Sunday is the 21st, the last day of the third week") {
    var expected = "April 21, 2013"
    var received = Meetup.date("the third Sunday of April 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when fourth Monday is some day in the middle of the fourth week") {
    var expected = "March 25, 2013"
    var received = Meetup.date("the fourth Monday of March 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when fourth Monday is the 22nd, the first day of the fourth week") {
    var expected = "April 22, 2013"
    var received = Meetup.date("the fourth Monday of April 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when fourth Tuesday is the 28th, the last day of the fourth week") {
    var expected = "May 28, 2013"
    var received = Meetup.date("the fourth Tuesday of May 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when fourth Tuesday is some day in the middle of the fourth week") {
    var expected = "June 25, 2013"
    var received = Meetup.date("the fourth Tuesday of June 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when fourth Wednesday is some day in the middle of the fourth week") {
    var expected = "July 24, 2013"
    var received = Meetup.date("the fourth Wednesday of July 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when fourth Wednesday is the 28th, the last day of the fourth week") {
    var expected = "August 28, 2013"
    var received = Meetup.date("the fourth Wednesday of August 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when fourth Thursday is some day in the middle of the fourth week") {
    var expected = "September 26, 2013"
    var received = Meetup.date("the fourth Thursday of September 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when fourth Thursday is another day in the middle of the fourth week") {
    var expected = "October 24, 2013"
    var received = Meetup.date("the fourth Thursday of October 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when fourth Friday is the 22nd, the first day of the fourth week") {
    var expected = "November 22, 2013"
    var received = Meetup.date("the fourth Friday of November 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when fourth Friday is some day in the middle of the fourth week") {
    var expected = "December 27, 2013"
    var received = Meetup.date("the fourth Friday of December 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when fourth Saturday is some day in the middle of the fourth week") {
    var expected = "January 26, 2013"
    var received = Meetup.date("the fourth Saturday of January 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when fourth Saturday is another day in the middle of the fourth week") {
    var expected = "February 23, 2013"
    var received = Meetup.date("the fourth Saturday of February 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when fourth Sunday is some day in the middle of the fourth week") {
    var expected = "March 24, 2013"
    var received = Meetup.date("the fourth Sunday of March 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when fourth Sunday is the 28th, the last day of the fourth week") {
    var expected = "April 28, 2013"
    var received = Meetup.date("the fourth Sunday of April 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("last Monday in a month with four Mondays") {
    var expected = "March 25, 2013"
    var received = Meetup.date("the last Monday of March 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("last Monday in a month with five Mondays") {
    var expected = "April 29, 2013"
    var received = Meetup.date("the last Monday of April 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("last Tuesday in a month with four Tuesdays") {
    var expected = "May 28, 2013"
    var received = Meetup.date("the last Tuesday of May 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("last Tuesday in another month with four Tuesdays") {
    var expected = "June 25, 2013"
    var received = Meetup.date("the last Tuesday of June 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("last Wednesday in a month with five Wednesdays") {
    var expected = "July 31, 2013"
    var received = Meetup.date("the last Wednesday of July 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("last Wednesday in a month with four Wednesdays") {
    var expected = "August 28, 2013"
    var received = Meetup.date("the last Wednesday of August 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("last Thursday in a month with four Thursdays") {
    var expected = "September 26, 2013"
    var received = Meetup.date("the last Thursday of September 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("last Thursday in a month with five Thursdays") {
    var expected = "October 31, 2013"
    var received = Meetup.date("the last Thursday of October 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("last Friday in a month with five Fridays") {
    var expected = "November 29, 2013"
    var received = Meetup.date("the last Friday of November 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("last Friday in a month with four Fridays") {
    var expected = "December 27, 2013"
    var received = Meetup.date("the last Friday of December 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("last Saturday in a month with four Saturdays") {
    var expected = "January 26, 2013"
    var received = Meetup.date("the last Saturday of January 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("last Saturday in another month with four Saturdays") {
    var expected = "February 23, 2013"
    var received = Meetup.date("the last Saturday of February 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("last Sunday in a month with five Sundays") {
    var expected = "March 31, 2013"
    var received = Meetup.date("the last Sunday of March 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("last Sunday in a month with four Sundays") {
    var expected = "April 28, 2013"
    var received = Meetup.date("the last Sunday of April 2013")
    Expect.value(received).toEqual(expected)
  }

  do.test("when last Wednesday in February in a leap year is the 29th") {
    var expected = "February 29, 2012"
    var received = Meetup.date("the last Wednesday of February 2012")
    Expect.value(received).toEqual(expected)
  }

  do.test("last Wednesday in December that is also the last day of the year") {
    var expected = "December 31, 2014"
    var received = Meetup.date("the last Wednesday of December 2014")
    Expect.value(received).toEqual(expected)
  }

  do.test("when last Sunday in February in a non-leap year is not the 29th") {
    var expected = "February 22, 2015"
    var received = Meetup.date("the last Sunday of February 2015")
    Expect.value(received).toEqual(expected)
  }

  do.test("when first Friday is the 7th, the last day of the first week") {
    var expected = "December 7, 2012"
    var received = Meetup.date("the first Friday of December 2012")
    Expect.value(received).toEqual(expected)
  }

  do.test("when last Thursday in February in a non-leap year is not the 29th") {
    var expected = "February 22, 2300"
    var received = Meetup.date("the last Thursday of February 2300")
    Expect.value(received).toEqual(expected)
  }

  do.test("when fourth Monday is the 23nd, the second day of the fourth week") {
    var expected = "January 23, 2468"
    var received = Meetup.date("the fourth Monday of January 2468")
    Expect.value(received).toEqual(expected)
  }
}
