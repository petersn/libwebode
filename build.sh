
set -x

APP_DIR=./demo-libwebode-app

# Rebuild pkg/*
wasm-pack build --target web

# Copy the build into the example application.
mkdir -p $APP_DIR/src/wasm-build
cp pkg/libwebode.js $APP_DIR/src/wasm-build/libwebode_wasm_interface.js
cp pkg/libwebode_bg.wasm $APP_DIR/public/

# Currently wasm-pack is using some future import.meta feature I don't have.
sed -i "s/^.*import[.]meta.*$/\/\/ Line deleted./" $APP_DIR/src/wasm-build/libwebode_wasm_interface.js

