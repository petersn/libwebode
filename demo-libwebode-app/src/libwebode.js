// libwebode

import init, {
    generate_number,
} from "./wasm-build/libwebode_wasm_interface.js";

export let isInitialized = false;
export let initializationPromise = init(process.env.PUBLIC_URL + "/libwebode_bg.wasm")
    .then(() => isInitialized = true);

function assertIsInitialized() {
    if (isInitialized === false) {
        throw "libwebode is not initialized -- please await libwebode.initializationPromise, or make sure that libwebode.isInitialized === true";
    }
}

export function getValue() {
    assertIsInitialized();
    return generate_number();
}
