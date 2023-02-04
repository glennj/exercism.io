module BookingUpForBeauty

// The following line is needed to use the DateTime type
open System

let launchDate = DateTime(2012, 9, 15)  // Sept 15 2012

let schedule (appointmentDateDescription: string): DateTime = 
    DateTime.Parse(appointmentDateDescription)  // assume success

let hasPassed (appointmentDate: DateTime): bool = 
    appointmentDate < DateTime.Now

let isAfternoonAppointment (appointmentDate: DateTime): bool =
    12 <= appointmentDate.Hour && appointmentDate.Hour < 18

let description (appointmentDate: DateTime): string = 
    $"You have an appointment on {appointmentDate}."

let anniversaryDate(): DateTime = 
    DateTime(DateTime.Now.Year, launchDate.Month, launchDate.Day)
