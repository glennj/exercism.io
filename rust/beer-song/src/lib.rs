pub fn verse(n: u32) -> String {
    let mut result = String::new();

    let b = bottle(n);
    result.push_str(b.as_str());
    result.push_str(" on the wall, ");
    result.push_str(b.to_lowercase().as_str());
    result.push_str(".\n");

    let b = bottle(match n { 0 => 99, _ => n - 1 });
    result.push_str(action(n).as_str());
    result.push_str(", ");
    result.push_str(b.to_lowercase().as_str());
    result.push_str(" on the wall.\n");
    result
}

fn bottle(n: u32) -> String {
    let mut b = match n {
        0 => String::from("No more"),
        _ => n.to_string(),
    };
    b.push_str(" bottle");
    if n != 1 {
        b.push('s')
    }
    b.push_str(" of beer");
    b
}

fn action(n: u32) -> String {
    match n {
        0 => String::from("Go to the store and buy some more"),
        1 => String::from("Take it down and pass it around"),
        _ => String::from("Take one down and pass it around"),
    }
}

pub fn sing(start: u32, end: u32) -> String {
    let mut result = String::new();
    let mut i = start + 1;
    while i > end {
        i -= 1;
        if i < start {
            result.push('\n')
        }
        result.push_str(verse(i).as_str());
    }
    result
}
