# List available recipes
default:
    @just --list

# Build the uberjar into target/
build:
    clojure -T:build uber

# Run the test suite (no ClojureStorm instrumentation)
test:
    clojure -M:test

# Start an nREPL server for your editor (writes .nrepl-port)
nrepl:
    clojure -M:dev:nrepl

# Start an nREPL server with the FlowStorm debugger UI open
flowstorm:
    clojure -M:dev:nrepl:flowstorm
