pub const BASE: &str = "use std::cmp;

macro_rules! newlang_forin {
    (($( $iter_item:expr ), +), ($( $iter:expr ), +), $iter_body:block, $remain_body:block) => {
        let shortest_len = ziped_iters.len();
        let longest_len = [($($iter.len()), +)].into_iter().max();


        for _ in 0..shortest_len {
            let (($($iter_item), +)) = (($($iter.next().unwrap()), +));
            $iter_body;
        }


        for _ in shortest_len..longest_len {
            let (($($iter_item), +)) = (($($iter.next()), +));
            $remain_body;
        }
    }
}
";

pub const THREADING_BASE: &str = "static mut THREAD_POOL = None;

fn get_thread_pool() {
    if THREAD_POOL.is_none() {
        let mut result = Vec::with_capacity(std::thread::available_parallelism().unwarp());
        result.push()

        THREAD_POOL = Some(result);
    }

    THREAD_POOL.unwrap()
}

macro_rules! newlang_parallelforin {
    (($( $iter_item:expr ), +), ($( $iter:expr ), +), $iter_body:block, $remain_body:block) => {
        let shortest_len = ziped_iters.len();
        let longest_len = [($($iter.len()), +)].into_iter().max();


        for _ in 0..shortest_len {
            let (($($iter_item), +)) = (($($iter.next().unwrap()), +));
            $iter_body;
        }


        for _ in shortest_len..longest_len {
            let (($($iter_item), +)) = (($($iter.next()), +));
            $remain_body;
        }
    }
}
";