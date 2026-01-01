import SwiftUI

// This is a tiny SwiftUI macOS app built with Swift Package Manager.
// The goal is to keep everything in one file with clear, readable code.
@main
struct FooButtonApp: App {
    var body: some Scene {
        WindowGroup {
            CalculatorView()
                .frame(minWidth: 320, minHeight: 420)
        }
    }
}

// The calculator UI and logic live together here so it is easy to learn from.
struct CalculatorView: View {
    // The text shown at the top of the calculator.
    @State private var displayText = "0"

    // When an operation is tapped, we store the left-hand value here.
    @State private var storedValue: Double? = nil

    // The operation waiting for the next number.
    @State private var pendingOperation: Operation? = nil

    // Tracks whether the next digit should start a new number.
    @State private var isEnteringNewNumber = true

    // A simple 4x4 grid of buttons (16 total).
    private let buttonRows: [[ButtonSpec]] = [
        [
            ButtonSpec(title: "C", kind: .clear),
            ButtonSpec(title: "/", kind: .operation(.divide)),
            ButtonSpec(title: "*", kind: .operation(.multiply)),
            ButtonSpec(title: "-", kind: .operation(.subtract)),
        ],
        [
            ButtonSpec(title: "7", kind: .digit),
            ButtonSpec(title: "8", kind: .digit),
            ButtonSpec(title: "9", kind: .digit),
            ButtonSpec(title: "+", kind: .operation(.add)),
        ],
        [
            ButtonSpec(title: "4", kind: .digit),
            ButtonSpec(title: "5", kind: .digit),
            ButtonSpec(title: "6", kind: .digit),
            ButtonSpec(title: "=", kind: .equals),
        ],
        [
            ButtonSpec(title: "1", kind: .digit),
            ButtonSpec(title: "2", kind: .digit),
            ButtonSpec(title: "3", kind: .digit),
            ButtonSpec(title: "0", kind: .digit),
        ],
    ]

    private let columns = Array(repeating: GridItem(.flexible(), spacing: 12), count: 4)

    var body: some View {
        VStack(spacing: 16) {
            // The display is right-aligned like a classic calculator.
            Text(displayText)
                .font(.system(size: 44, weight: .medium, design: .rounded))
                .frame(maxWidth: .infinity, alignment: .trailing)
                .padding()
                .background(Color.gray.opacity(0.15))
                .cornerRadius(8)

            LazyVGrid(columns: columns, spacing: 12) {
                ForEach(buttonRows.flatMap { $0 }) { spec in
                    CalculatorButton(
                        title: spec.title,
                        background: buttonBackground(for: spec.kind),
                        foreground: buttonForeground(for: spec.kind)
                    ) {
                        handleButton(spec)
                    }
                }
            }
        }
        .padding()
    }

    // Decide which action to take for each button type.
    private func handleButton(_ spec: ButtonSpec) {
        switch spec.kind {
        case .digit:
            handleDigit(spec.title)
        case .operation(let op):
            handleOperation(op)
        case .equals:
            handleEquals()
        case .clear:
            clearAll()
        }
    }

    // Append a digit or start a new number if needed.
    private func handleDigit(_ digit: String) {
        if displayText == "Error" {
            displayText = "0"
        }

        if isEnteringNewNumber {
            displayText = digit
            isEnteringNewNumber = false
        } else {
            displayText = displayText == "0" ? digit : displayText + digit
        }
    }

    // Store the operation and prepare to capture the next number.
    private func handleOperation(_ op: Operation) {
        if storedValue == nil {
            storedValue = Double(displayText)
        } else if !isEnteringNewNumber {
            commitPendingOperation()
        }

        pendingOperation = op
        isEnteringNewNumber = true
    }

    // Finish the pending operation and reset for a new calculation.
    private func handleEquals() {
        if !isEnteringNewNumber {
            commitPendingOperation()
        }

        pendingOperation = nil
        storedValue = nil
        isEnteringNewNumber = true
    }

    // Apply the pending operation using the current display as the right-hand value.
    private func commitPendingOperation() {
        guard
            let op = pendingOperation,
            let lhs = storedValue,
            let rhs = Double(displayText)
        else {
            return
        }

        guard let result = apply(op, lhs, rhs) else {
            displayText = "Error"
            storedValue = nil
            pendingOperation = nil
            isEnteringNewNumber = true
            return
        }

        storedValue = result
        displayText = format(result)
    }

    // Clear all state back to the starting point.
    private func clearAll() {
        displayText = "0"
        storedValue = nil
        pendingOperation = nil
        isEnteringNewNumber = true
    }

    // Perform the math for a given operation.
    private func apply(_ op: Operation, _ lhs: Double, _ rhs: Double) -> Double? {
        switch op {
        case .add:
            return lhs + rhs
        case .subtract:
            return lhs - rhs
        case .multiply:
            return lhs * rhs
        case .divide:
            return rhs == 0 ? nil : lhs / rhs
        }
    }

    // Format numbers so whole values show without a decimal.
    private func format(_ value: Double) -> String {
        if value.rounded(.towardZero) == value {
            return String(Int(value))
        }

        return String(value)
    }

    private func buttonBackground(for kind: ButtonKind) -> Color {
        switch kind {
        case .digit:
            return Color.gray.opacity(0.2)
        case .operation:
            return Color.orange.opacity(0.9)
        case .equals:
            return Color.blue.opacity(0.9)
        case .clear:
            return Color.red.opacity(0.9)
        }
    }

    private func buttonForeground(for kind: ButtonKind) -> Color {
        switch kind {
        case .digit:
            return .primary
        case .operation, .equals, .clear:
            return .white
        }
    }
}

// The supported calculator operations.
enum Operation {
    case add
    case subtract
    case multiply
    case divide
}

// A small model for each button so the grid is data-driven.
struct ButtonSpec: Identifiable {
    let id: String
    let title: String
    let kind: ButtonKind

    init(title: String, kind: ButtonKind) {
        self.id = title
        self.title = title
        self.kind = kind
    }
}

// Buttons are grouped into a small set of kinds for easy styling.
enum ButtonKind {
    case digit
    case operation(Operation)
    case equals
    case clear
}

// A reusable button view so the grid code stays tidy.
private struct CalculatorButton: View {
    let title: String
    let background: Color
    let foreground: Color
    let action: () -> Void

    var body: some View {
        Button(action: action) {
            Text(title)
                .font(.system(size: 22, weight: .semibold, design: .rounded))
                .frame(maxWidth: .infinity, minHeight: 44)
                .padding(.vertical, 6)
                .background(background)
                .foregroundColor(foreground)
                .cornerRadius(8)
        }
        .buttonStyle(.plain)
    }
}
