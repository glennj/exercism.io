import ballerina/io;

type Fueling record {|
    int employeeId;
    int odometerReading = 0;
    decimal gallons = 0;
    decimal gasPrice = 0;
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

// Wait, we don't actually need the `xmlns` declaration for this to succeed?
function readInput(string inputFilePath) returns Fueling[]|error {
    xml x = check io:fileReadXml(inputFilePath);
    Fueling[] fillups = [];
    foreach xml item in x.elementChildren() {
        string id = check item.employeeId;
        Fueling f = {employeeId: check int:fromString(id)};
        foreach xml child in item.elementChildren() {
            string tag = child.getName();   // "{http://www.so2w.org}elemName"
            if tag.endsWith("}odometerReading") {
                f.odometerReading = check int:fromString(child.data());
            }
            else if tag.endsWith("}gallons") {
                f.gallons = check decimal:fromString(child.data());
            }
            else if tag.endsWith("}gasPrice") {
                f.gasPrice = check decimal:fromString(child.data());
            }
        }
        fillups.push(f);
    }
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
    //io:println(employees);
    return employees;
}

// This was so hard to get right and to get even semi-readable.
// Thanks to the xml namespace, the entire document must be created in a single sentence.
// Absolutely does not scale.
function writeOutput(string outputFilePath, Employees employees) returns error? {
    xml:Element content =
        xml `<s:employeeFuelRecords xmlns:s="http://www.so2w.org">${
            from Employee e in employees.toArray()
            order by e.employeeId
            let xml inner =
                xml `<s:gasFillUpCount>${e.gasFillUpCount}</s:gasFillUpCount>` +
                xml `<s:totalFuelCost>${e.totalFuelCost}</s:totalFuelCost>` +
                xml `<s:totalGallons>${e.totalGallons}</s:totalGallons>` +
                xml `<s:totalMilesAccrued>${e.totalMilesAccrued}</s:totalMilesAccrued>`
            select xml `<s:employeeFuelRecord employeeId="${e.employeeId}">${inner}</s:employeeFuelRecord>`
        }</s:employeeFuelRecords>`;

    check io:fileWriteXml(outputFilePath, content);
}

// Although this is more LOC, it's more readable.
// It lays out the structure pretty clearly.
// But it doesn't work: cannot attach the namespace to the document properly.
//function writeOutput(string outputFilePath, Employees employees) returns error? {
//    string NS = "http://www.so2w.org";
//    xml:Element content = xml:createElement("s:employeeFuelRecords", {"xmlns:s": NS});
//    // WTF is this BS:
//    //   ERROR [main.bal:(84:63,84:70)] xml namespaces cannot be interpolated
//    xml inner = xml ``;
//    foreach Employee e in employees.toArray() {
//        xml:Element rec = xml:createElement("s:employeeFuelRecord", {"employeeId": e.employeeId.toString()});
//        xml:Element count = xml:createElement("s:gasFillUpCount");
//        xml:Element cost = xml:createElement("s:totalFuelCost");
//        xml:Element gals = xml:createElement("s:totalGallons");
//        xml:Element miles = xml:createElement("s:totalMilesAccrued");
//
//        count.setChildren(e.gasFillUpCount.toString());
//        cost.setChildren(e.totalFuelCost.toString());
//        gals.setChildren(e.totalGallons.toString());
//        miles.setChildren(e.totalMilesAccrued.toString());
//        rec.setChildren(count + cost + gals + miles);
//        inner += rec;
//    }
//    content.setChildren(inner);
//    check io:fileWriteXml(outputFilePath, content);
//}
