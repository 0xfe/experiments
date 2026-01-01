# FooButton

A tiny SwiftUI macOS calculator built with Swift Package Manager. The code is
intentionally barebones and heavily commented so it is easy to learn from.

## Requirements

- macOS 11 or newer
- Swift 6.2 toolchain (Xcode 16 or the Swift.org toolchain)

## Run

```bash
swift run
```

This builds the package and opens a small calculator window.

## What it does

- Supports digits 0-9, plus, minus, multiply, divide, equals, and clear.
- Uses integer-style formatting (no decimal input yet).
- Keeps UI and logic together in a single file for easy reading.

## Project layout

- `Sources/FooButton/FooButton.swift` contains the entire app.
- `Package.swift` defines the Swift Package Manager configuration.

## Next ideas

- Add a decimal point button and decimal handling.
- Support keyboard input.
- Add a history list of operations.
