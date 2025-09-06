#[macro_export]
macro_rules! s {
    ($lit:literal) => {
        String::from($lit)
    };
}

#[macro_export]
macro_rules! none_to_err {
    ($opt:expr, $err:expr) => {
        match $opt {
            Some(val) => val,
            None => return Err($err),
        }
    };
}
