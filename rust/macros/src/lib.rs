// "local_inner_macros" -- the macro refers to itself, perhaps?

#[macro_export(local_inner_macros)]
macro_rules! hashmap {

    ( $($key:literal => $value:expr),* ) => {
        {
            let mut hash = ::std::collections::HashMap::new();
            $(
                // capture but ignore the return value
                let _ = hash.insert($key, $value);
            )*
            hash
        }
    };

    // This pattern required to handle the last key-value pair ending with a comma.
    ( $($key:literal => $value:expr,)+ ) => { hashmap!( $($key => $value),+ ) };

}
