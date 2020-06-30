const SECONDS_PER_EARTH_YEAR: f64 = 31_557_600.0;

pub struct Duration {
    seconds: f64,
}

impl From<u64> for Duration {
    fn from(s: u64) -> Self {
        Self { seconds: s as f64 }
    }
}

pub trait Planet {
    const EARTH_YEARS_PER_ORBIT: f64;

    fn years_during(d: &Duration) -> f64 {
        d.seconds / SECONDS_PER_EARTH_YEAR / Self::EARTH_YEARS_PER_ORBIT
    }
}

// take 2: macro to encapsulate repeated code
//
macro_rules! planetary_orbit {
    ($name:ident, $relative_orbit:expr) => {
        pub struct $name {}
        impl Planet for $name {
            const EARTH_YEARS_PER_ORBIT: f64 = $relative_orbit;
        }
    };
}

planetary_orbit!(Mercury,   0.2408467 );
planetary_orbit!(Venus,     0.61519726);
planetary_orbit!(Earth,     1.0       );
planetary_orbit!(Mars,      1.8808158 );
planetary_orbit!(Jupiter,  11.862615  );
planetary_orbit!(Saturn,   29.447498  );
planetary_orbit!(Uranus,   84.016846  );
planetary_orbit!(Neptune, 164.79132   );


// take 1: repetitive manual implementation:
//
// pub struct Mercury {}
// impl Planet for Mercury {
//     fn earth_years_per_orbit() -> f64 {
//         0.2408467
//     }
// }
// 
// pub struct Venus {}
// impl Planet for Venus {
//     fn earth_years_per_orbit() -> f64 {
//         0.61519726
//     }
// }
// 
// pub struct Earth {}
// impl Planet for Earth {
//     fn earth_years_per_orbit() -> f64 {
//         1.0
//     }
// }
// 
// pub struct Mars {}
// impl Planet for Mars {
//     fn earth_years_per_orbit() -> f64 {
//         1.8808158
//     }
// }
// 
// pub struct Jupiter {}
// impl Planet for Jupiter {
//     fn earth_years_per_orbit() -> f64 {
//         11.862615
//     }
// }
// 
// pub struct Saturn {}
// impl Planet for Saturn {
//     fn earth_years_per_orbit() -> f64 {
//         29.447498
//     }
// }
// 
// pub struct Uranus {}
// impl Planet for Uranus {
//     fn earth_years_per_orbit() -> f64 {
//         84.016846
//     }
// }
// 
// pub struct Neptune {}
// impl Planet for Neptune {
//     fn earth_years_per_orbit() -> f64 {
//         164.79132
//     }
// }
