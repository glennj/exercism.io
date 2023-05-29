import ballerina/io;

type Fueling record {|
    int employeeId;
    int odometerReading;
    decimal gallons;
    decimal gasPrice;
|};

type Employee record {|
    readonly int employeeId;
    int gasFillUpCount = 0;
    decimal totalFuelCost = 0;
    decimal totalGallons = 0;
    int totalMilesAccrued = 0;
|};

type Employees table<Employee> key(employeeId);

function processFuelRecords(string inputFilePath, string outputFilePath) returns error? {
    Fueling[] fillups = check readInput(inputFilePath);
    Employees employees = aggregate(fillups);
    check writeOutput(outputFilePath, employees);
}

function readInput(string inputFilePath) returns Fueling[]|error {
    json data = check io:fileReadJson(inputFilePath);
    Fueling[] fillups = check data.cloneWithType();
    return fillups;
}

type CurrentOdometer record {|
    readonly int employeeId;
    int reading;
|};

function aggregate(Fueling[] fillups) returns Employees {
    table<CurrentOdometer> key(employeeId) currentOdo = table [];
    Employees employees = table [];
    foreach Fueling fillup in fillups {
        Employee emp = employees.hasKey(fillup.employeeId)
            ? employees.get(fillup.employeeId)
            : {employeeId: fillup.employeeId};

        CurrentOdometer odo = currentOdo.hasKey(fillup.employeeId)
            ? currentOdo.get(fillup.employeeId)
            : {employeeId: fillup.employeeId, reading: fillup.odometerReading};
        int prev = odo.reading;
        odo.reading = fillup.odometerReading;
        currentOdo.put(odo);

        emp.gasFillUpCount += 1;
        emp.totalMilesAccrued += fillup.odometerReading - prev;
        emp.totalGallons += fillup.gallons;
        emp.totalFuelCost += fillup.gallons * fillup.gasPrice;
        employees.put(emp);
    }
    return employees;
}

function writeOutput(string outputFilePath, Employees employees) returns error? {
    var byId = isolated function(Employee e) returns int => e.employeeId;
    json content = employees.toArray().sort(key = byId);
    check io:fileWriteJson(outputFilePath, content);
}
