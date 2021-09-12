# Rust Experiments

This directory consists of a bunch of random rust code written while learning Rust.

## To run

```
cargo run (print|generics|lifetimes|threads|...)
```

## Install Rust

Install via official management tool.

```
$ curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
$ source $HOME/.cargo/env
```

Add `source $HOME/.cargo/env` to end of .zshrc

## API Docs

https://doc.rust-lang.org/std/fmt/

(Use `S` to search.)

## New project

Follow instructions: https://www.rust-lang.org/learn/get-started

```
$ cargo new hello-rust
$ cargo run
```

You can also:

```
$ rustc main.rs
$ ./main
```

Build for production:

```
$ cargo build --release
```

## VSCode

Install rust extension. Then cmd-shift-p --> restart rust server, will prompt to install if not found.
