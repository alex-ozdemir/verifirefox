use std::cmp;

use anyhow::{Context, Error};
use rayon::{ThreadPool, ThreadPoolBuilder};

use crate::passes::Pass;

const MIN_THREAD_COUNT: usize = 1;
const MAX_THREAD_COUNT: usize = 4;

static mut STATE: Option<State> = None;

struct State {
    async_thread_pool: ThreadPool,
    fail_cb: FailCallback,
}

pub type FailCallback = Box<dyn Fn()>;

pub unsafe fn init(fail_cb: FailCallback) -> Result<(), Error> {
    let num_async_threads = cmp::max(
        MIN_THREAD_COUNT,
        cmp::min(MAX_THREAD_COUNT, num_cpus::get()),
    );

    let async_thread_pool = ThreadPoolBuilder::new()
        .num_threads(num_async_threads)
        .thread_name(|thread_index| format!("Verifirefox Thread {}", thread_index))
        .build()
        .context("Failed to initialize async verification thread pool")?;

    STATE = Some(State {
        async_thread_pool,
        fail_cb,
    });

    Ok(())
}

fn with_state<F, T>(f: F) -> T
where
    F: FnOnce(&State) -> T,
{
    f((unsafe { &STATE })
        .as_ref()
        .expect("verifirefox executor not initialized"))
}

fn check<T>(pass: T, result: Result<(), Error>)
where
    T: Pass,
{
    let err = match result {
        Ok(()) => return,
        Err(err) => err,
    };

    eprintln!(
        "!!! BEGIN VERIFIREFOX FAIL !!!\n\
        \n\
        Error: {:?}\n\
        \n\
        Pass: {:#?}\n\
        \n\
        !!! END VERIFIREFOX FAIL !!!",
        err, pass
    );

    with_state(|state| (state.fail_cb)());
}

pub fn sync_pass<T>(pass: T)
where
    T: Pass,
{
    let result = pass.run();
    check(pass, result);
}

pub fn async_pass<T>(pass: T)
where
    T: Pass,
{
    with_state(move |state| state.async_thread_pool.spawn(move || sync_pass(pass)));
}
