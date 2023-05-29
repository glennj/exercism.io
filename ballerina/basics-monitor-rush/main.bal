type Monitor record {|
    readonly int id;
|};

// take 3: query expression
function allocateCubicles(int[] requests) returns int[] {
    table<Monitor> key(id) allocations =
        table key(id) from int id in requests select {id};
    return allocations.keys().sort();
}

// take 1: functional
//function allocateCubicles(int[] requests) returns int[] {
//    int[] allocation = requests.reduce(
//        function (int[] alloc, int monitor) returns int[] {
//            if alloc.indexOf(monitor) is () {
//                alloc.push(monitor);
//            }
//            return alloc;
//        },
//        []
//    );
//    return allocation.sort();
//}

// take 2: iterative
//function allocateCubicles(int[] requests) returns int[] {
//    int[] allocation = [];
//    foreach int monitor in requests {
//        if allocation.indexOf(monitor) is () {
//            allocation.push(monitor);
//        }
//    }
//    return allocation.sort();
//}
