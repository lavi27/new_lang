pub const BASE: &str = "mod {newlang_base}
";

pub const THREADING_BASE: &str = "use std::sync::{mpsc, Arc, Mutex};
use std::thread;
use std::sync::OnceLock;

static THREAD_POOL: OnceLock<ThreadPool> = OnceLock::new();

pub fn get_thread_pool() -> &'static ThreadPool {
    THREAD_POOL.get_or_init(|| ThreadPool::new(4))
}

pub type Job = Box<dyn FnOnce() + Send + 'static>;

pub struct Worker {
    handle: Option<thread::JoinHandle<()>>,
}

impl Worker {
    fn new(receiver: Arc<Mutex<mpsc::Receiver<Job>>>) -> Worker {
        let handle = thread::spawn(move || loop {
            let job = {
                let lock = receiver.lock().unwrap();
                lock.recv()
            };

            if let Ok(job) = job {
                job();
            } else {
                break;
            }
        });

        Worker {
            handle: Some(handle),
        }
    }
}

pub struct ThreadPool {
    workers: Vec<Worker>,
    sender: mpsc::Sender<Job>,
    pub size: usize,
}

impl ThreadPool {
    pub fn new(size: usize) -> ThreadPool {
        assert!(size > 0);

        let (sender, receiver) = mpsc::channel();
        let receiver = Arc::new(Mutex::new(receiver));

        let mut workers = Vec::with_capacity(size);
        for _ in 0..size {
            workers.push(Worker::new(Arc::clone(&receiver)));
        }

        ThreadPool { workers, sender, size }
    }

    pub fn execute<F>(&self, f: F)
    where
        F: FnOnce() + Send + 'static,
    {
        self.sender.send(Box::new(f)).unwrap();
    }
}

impl Drop for ThreadPool {
    fn drop(&mut self) {
        drop(&self.sender);
        for worker in &mut self.workers {
            if let Some(handle) = worker.handle.take() {
                handle.join().unwrap();
            }
        }
    }
}
";
