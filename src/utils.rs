#[macro_export]
macro_rules! s {
    ($lit:literal) => {
        String::from($lit)
    };
}
