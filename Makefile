all: run

run:
	cargo run --release

dev:
	cargo run

lint:
	cargo fmt -- --check --color always
	cargo clippy --all-targets --all-features -- -D warnings

test:
	make lint
	cargo test
	cargo test --test parser
	cargo test --test target_wasm_tests