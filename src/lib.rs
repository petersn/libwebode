// libwebode

use std::collections::HashMap;
use once_cell::sync::OnceCell;
use wasm_bindgen::prelude::*;

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

struct IntegratorState {
    values: Vec<f32>,
}

impl IntegratorState {
    pub fn new() -> Self {
        Self {
            values: Vec::new(),
        }
    }
}

struct GlobalState {
    handle_counter: i32,
    integrator_states: HashMap<i32, IntegratorState>,
}

impl GlobalState {
    pub fn new() -> Self {
        Self {
            handle_counter: 0,
            integrator_states: HashMap::new(),
        }
    }
}

fn get_global_state() -> &'static mut GlobalState {
    unsafe {
        static mut GLOBAL_STATE: OnceCell<GlobalState> = OnceCell::new();
        GLOBAL_STATE.get_or_init(GlobalState::new);
        GLOBAL_STATE.get_mut().unwrap()
    }
}

#[wasm_bindgen]
pub fn generate_number() -> f64 {
    return 42.0;
}

#[wasm_bindgen]
pub fn newIntegratorState() -> i32 {
    let global_state = get_global_state();
    global_state.handle_counter += 1;
    let new_handle = global_state.handle_counter;
    global_state.integrator_states.insert(new_handle, IntegratorState::new());
    new_handle
}

#[wasm_bindgen]
pub fn deleteIntegratorState(handle: i32) {

}
