public function score(float x, float y) returns int {
    match hypot(x, y) {
        var d if d <=  1.0 => {return 10;}
        var d if d <=  5.0 => {return  5;}
        var d if d <= 10.0 => {return  1;}
        _                  => {return  0;}
    }
}

function hypot(float x, float y) returns float {
    return (x.pow(2) + y.pow(2)).sqrt();
}
