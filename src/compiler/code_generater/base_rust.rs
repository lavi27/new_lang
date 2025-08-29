pub const BASE: &str = "mod {newlang_base}
";

pub const THREADING_BASE: &str = "use std::sync::{mpsc, Arc, Mutex};
use std::thread;
use std::sync::OnceLock;
use std::ops::Range;
use std::mem;

static THREAD_POOL: OnceLock<ThreadPool> = OnceLock::new();

pub fn get_thread_pool() -> &'static ThreadPool {
    THREAD_POOL.get_or_init(|| ThreadPool::new(thread::available_parallelism().unwrap().get()))
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
            };
        });

        Worker {
            handle: Some(handle),
        }
    }
}

pub struct Scope<'a, R> {
    thread_pool: &'a ThreadPool,
    handles: Vec<mpsc::Receiver<R>>,
}

impl<'a, R: Send> Scope<'a, R> {
    pub fn execute<F>(&mut self, f: F)
    where
        F: FnOnce() -> R + Send,
    {
        let handle = self.thread_pool.execute_scope(f);

        self.handles.push(handle);
    }

    fn scope_end(&mut self) {
        for handle in self.handles.drain(..) {
            handle.recv().unwrap();
        }
    }
}

pub struct ThreadPool {
    workers: Vec<Worker>,
    sender: mpsc::Sender<Job>,
    size: usize,
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

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn execute<F, R>(&self, f: F) -> mpsc::Receiver<R>
    where
        F: FnOnce() -> R + Send + 'static,
        R: Send + 'static,
    {
        let (tx, rx) = mpsc::channel::<R>();

        let closure: Box<dyn FnOnce() + Send> = Box::new(move || {
            let result = f();
            tx.send(result).unwrap();
        });

        self.sender
            .send(closure)
            .unwrap();

        rx
    }

    fn execute_scope<F, R>(&self, f: F) -> mpsc::Receiver<R>
    where
        F: FnOnce() -> R + Send,
        R: Send,
    {
        let (tx, rx) = mpsc::channel::<R>();

        let closure: Box<dyn FnOnce() + Send> = Box::new(move || {
            let result = f();
            tx.send(result).unwrap();
        });
        let closure: Box<dyn FnOnce() + Send + 'static> = unsafe { mem::transmute(closure) };

        self.sender
            .send(closure)
            .unwrap();

        rx
    }

    pub fn scope<F, R>(&self, f: F)
    where
        F: FnOnce(&mut Scope<R>) -> R + Send,
        R: Send,
    {
        let mut scope = Scope {
            thread_pool: &self,
            handles: Vec::new(),
        };

        f(&mut scope);

        scope.scope_end();
    }
}

impl Drop for ThreadPool {
    fn drop(&mut self) {
        for worker in &mut self.workers {
            if let Some(handle) = worker.handle.take() {
                handle.join().unwrap();
            }
        }
    }
}
";
