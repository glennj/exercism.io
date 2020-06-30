use std::fmt;

#[derive(Eq, PartialEq, Debug)]
pub struct Clock {
    minutes: i32,
}

const MINS_PER_DAY: i32 = 24 * 60;

impl Clock {
    pub fn new(hours: i32, minutes: i32) -> Self {
        let mins = math::floor_mod(hours * 60 + minutes, MINS_PER_DAY);
        Self { minutes: mins }
    }

    pub fn add_minutes(&self, minutes: i32) -> Self {
        Self::new(0, self.minutes + minutes)
    }
}

impl fmt::Display for Clock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let h = self.minutes / 60;
        let m = self.minutes % 60;
        write!(f, "{:02}:{:02}", h, m)
    }
}

mod math {
    // ensure the remainder is in range [0, a)
    pub fn floor_mod(a: i32, b: i32) -> i32 {
        ((a % b) + b) % b
    }
}
