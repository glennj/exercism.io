import ballerina/io;

type Fueling record {|
    int employeeId;
    int odometer;
    decimal gallons;
    decimal pricePerGallon;
|};

type Employee record {|
    readonly int employeeId;
    int numFillups = 0;
    decimal totalCost = 0;
    decimal totalGallons = 0;
    int totalMiles = 0;
    int previousOdometer = -1;
|};

type Employees table<Employee> key(employeeId);

function processFuelRecords(string inputFilePath, string outputFilePath) returns error? {
    Fueling[] fillups = check readInput(inputFilePath);
    Employees employees = aggregate(fillups);
    check writeOutput(outputFilePath, employees);
}

function readInput(string inputFilePath) returns Fueling[]|error {
    string[][] rows = check io:fileReadCsv(inputFilePath);
    return
        from string[] row in rows
        let string[] trimmed = row.map(s => s.trim())
        select {
            employeeId: check int:fromString(trimmed[0]),
            odometer: check int:fromString(trimmed[1]),
            gallons: check decimal:fromString(trimmed[2]),
            pricePerGallon: check decimal:fromString(trimmed[3])
        };
}

function aggregate(Fueling[] fillups) returns Employees {
    Employees employees = table [];
    foreach Fueling fillup in fillups {
        Employee emp = employees.hasKey(fillup.employeeId)
            ? employees.get(fillup.employeeId)
            : {employeeId: fillup.employeeId};

        int prev = emp.previousOdometer == -1
            ? fillup.odometer
            : emp.previousOdometer;
        emp.previousOdometer = fillup.odometer;

        emp.numFillups += 1;
        emp.totalMiles += fillup.odometer - prev;
        emp.totalGallons += fillup.gallons;
        emp.totalCost += fillup.gallons * fillup.pricePerGallon;

        employees.put(emp);
    }
    return employees;
}

function writeOutput(string outputFilePath, Employees employees) returns error? {
    string[][] result =
        from Employee e in employees
        order by e.employeeId
        select [
            e.employeeId.toString(),
            e.numFillups.toString(),
            e.totalCost.toString(),
            e.totalGallons.toString(),
            e.totalMiles.toString()
        ];

    check io:fileWriteCsv(outputFilePath, result);
}
