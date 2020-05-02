use std::sync::mpsc::{self, Sender};
use std::thread;

use anyhow::Error;

use crate::passes::Pass;

static mut STATE: Option<State> = None;

struct State {
    async_pass_sender: Sender<Box<dyn Pass>>,
    fail_cb: FailCallback,
}

pub type FailCallback = Box<dyn Fn()>;

pub unsafe fn init(fail_cb: FailCallback) {
    let (async_pass_sender, async_pass_receiver) = mpsc::channel();

    thread::spawn(move || {
        for pass in async_pass_receiver.iter() {
            sync_pass(pass);
        }
    });

    STATE = Some(State {
        async_pass_sender,
        fail_cb,
    })
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
    thread_local! {
        static SENDER: Sender<Box<dyn Pass>> = with_state(|state| state.async_pass_sender.clone());
    }

    SENDER.with(|sender| {
        let _ = sender.send(Box::new(pass));
    });
}
