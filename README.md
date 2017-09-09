# ELVM Interpreter in Rust

An ELVM interpreter, which I wrote for learning Rust.

## Usage

### Build

```bash
$ cargo build
$ cargo run 
```

### Usage

```bash
$ ./rust_elvmi EIR_FILE
```

### Test

Testing is done by a Ruby script that is in `t/` directory. It tries to run the Release build of `rust_elvmi`.

```bash
$ cargo build --release
$ cd ./t
$ ./test.rb
```
