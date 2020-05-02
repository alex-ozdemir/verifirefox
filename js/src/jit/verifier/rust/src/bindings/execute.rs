use crate::execute;

#[no_mangle]
pub unsafe extern "C" fn verifirefox_execute_init(verify_fail_cb: extern "C" fn()) {
    execute::init(Box::new(move || {
        verify_fail_cb();
    }));
}
