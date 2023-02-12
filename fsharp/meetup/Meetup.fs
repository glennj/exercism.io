module Meetup

open System

type Week = First | Second | Third | Fourth | Last | Teenth

let meetup (year: int) (month: int) (week: Week) (dayOfWeek: DayOfWeek): DateTime = 
    let rec findMeetup (datetime: DateTime) =
        if datetime.DayOfWeek = dayOfWeek
            then datetime
            else findMeetup (datetime.AddDays 1.0)

    let firstDay = match week with
                   | Week.First  -> 1
                   | Week.Second -> 8
                   | Week.Teenth -> 13
                   | Week.Third  -> 15
                   | Week.Fourth -> 22
                   | Week.Last   -> DateTime.DaysInMonth(year, month) - 6

    findMeetup (DateTime(year, month, firstDay))
