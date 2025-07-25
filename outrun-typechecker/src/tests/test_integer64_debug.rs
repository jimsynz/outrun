use crate::typecheck_program;
use outrun_parser::parse_program;
use std::error::Error;

#[test]
fn test_integer64_type_unification_debug() {
    let code = r#"
@Doc(content: """
64-bit signed integer concrete type.

This is the default concrete implementation of the Integer protocol in Outrun.
Integer literals without explicit type annotations default to this type.

## Examples

```outrun
42                    # Defaults to Outrun.Core.Integer64
-1337                 # Negative integer
0xFF                  # Hexadecimal literal
0o755                 # Octal literal
0b1010                # Binary literal
```
""")
struct Outrun.Core.Integer64() {}

# Binary arithmetic operations
impl BinaryAddition for Outrun.Core.Integer64 {
    def add(lhs: Self, rhs: Self): Self {
        Outrun.Intrinsic.i64_add(lhs: lhs, rhs: rhs)
    }
}

impl BinarySubtraction for Outrun.Core.Integer64 {
    def subtract(lhs: Self, rhs: Self): Self {
        Outrun.Intrinsic.i64_sub(lhs: lhs, rhs: rhs)
    }
}

impl BinaryMultiplication for Outrun.Core.Integer64 {
    def multiply(lhs: Self, rhs: Self): Self {
        Outrun.Intrinsic.i64_mul(lhs: lhs, rhs: rhs)
    }
}

impl BinaryDivision for Outrun.Core.Integer64 {
    def divide(lhs: Self, rhs: Self): Option<Self> {
        Outrun.Intrinsic.i64_div(lhs: lhs, rhs: rhs)
    }
}

impl BinaryModulo for Outrun.Core.Integer64 {
    def modulo(lhs: Self, rhs: Self): Option<Self> {
        Outrun.Intrinsic.i64_mod(lhs: lhs, rhs: rhs)
    }
}

impl BinaryExponentiation for Outrun.Core.Integer64 {
    def exponentiate(lhs: Self, rhs: Self): Self {
        Outrun.Intrinsic.i64_pow(lhs: lhs, rhs: rhs)
    }
}

# Unary operations
impl UnaryPlus for Outrun.Core.Integer64 {
    def plus(value: Self): Self {
        Outrun.Intrinsic.i64_pos(value: value)
    }
}

impl UnaryMinus for Outrun.Core.Integer64 {
    def minus(value: Self): Self {
        Outrun.Intrinsic.i64_neg(value: value)
    }
}

# Comparison operations
impl Equality for Outrun.Core.Integer64 {
    def equal?(lhs: Self, rhs: Self): Outrun.Core.Boolean {
        Outrun.Intrinsic.i64_eq(lhs: lhs, rhs: rhs)
    }
}

impl Comparison for Outrun.Core.Integer64 {
    def greater?(lhs: Self, rhs: Self): Outrun.Core.Boolean {
        Outrun.Intrinsic.i64_gt(lhs: lhs, rhs: rhs)
    }

    def greater_equal?(lhs: Self, rhs: Self): Outrun.Core.Boolean {
        Outrun.Intrinsic.i64_ge(lhs: lhs, rhs: rhs)
    }

    def less?(lhs: Self, rhs: Self): Outrun.Core.Boolean {
        Outrun.Intrinsic.i64_lt(lhs: lhs, rhs: rhs)
    }

    def less_equal?(lhs: Self, rhs: Self): Outrun.Core.Boolean {
        Outrun.Intrinsic.i64_le(lhs: lhs, rhs: rhs)
    }
}

# Bitwise operations
impl BitwiseAnd for Outrun.Core.Integer64 {
    def and(lhs: Self, rhs: Self): Self {
        Outrun.Intrinsic.i64_and(lhs: lhs, rhs: rhs)
    }
}

impl BitwiseOr for Outrun.Core.Integer64 {
    def or(lhs: Self, rhs: Self): Self {
        Outrun.Intrinsic.i64_or(lhs: lhs, rhs: rhs)
    }
}

impl BitwiseXor for Outrun.Core.Integer64 {
    def xor(lhs: Self, rhs: Self): Self {
        Outrun.Intrinsic.i64_xor(lhs: lhs, rhs: rhs)
    }
}

impl BitwiseNot for Outrun.Core.Integer64 {
    def not(value: Self): Self {
        Outrun.Intrinsic.i64_not(value: value)
    }
}

impl ShiftLeft for Outrun.Core.Integer64 {
    def shift_left(lhs: Self, rhs: Self): Self {
        Outrun.Intrinsic.i64_shl(lhs: lhs, rhs: rhs)
    }
}

impl ShiftRight for Outrun.Core.Integer64 {
    def shift_right(lhs: Self, rhs: Self): Self {
        Outrun.Intrinsic.i64_shr(lhs: lhs, rhs: rhs)
    }
}

# Display protocol implementation
impl Display for Outrun.Core.Integer64 {
    def to_string(value: Self): Outrun.Core.String {
        Outrun.Intrinsic.i64_to_string_radix(value: value, radix: 10)
    }
}


# Integer protocol implementation
impl Integer for Outrun.Core.Integer64 {
    def to_string_radix(value: Self, radix: Integer): Outrun.Core.String {
        Outrun.Intrinsic.i64_to_string_radix(value: value, radix: radix)
    }

    def abs(value: Self): Self {
        Outrun.Intrinsic.i64_abs(value: value)
    }

    def zero?(value: Self): Boolean {
        Equality.equal?(lhs: value, rhs: 0)
    }

    def positive?(value: Self): Boolean {
        Comparison.greater?(lhs: value, rhs: 0)
    }

    def negative?(value: Self): Boolean {
        Comparison.less?(lhs: value, rhs: 0)
    }
}

# Default protocol implementation
impl Default for Outrun.Core.Integer64 {
    def default(): Self {
        0
    }
}
"#;

    // Parse the code
    let mut program = match parse_program(code) {
        Ok(program) => program,
        Err(parse_error) => {
            panic!("Parse error: {:?}", parse_error);
        }
    };

    // Try to typecheck it
    let result = typecheck_program(&mut program);

    match result {
        Ok(_) => {
            panic!("Expected type checking to fail, but it succeeded");
        }
        Err(error) => {
            println!("Type checking error details:");
            println!("{}", error);

            // Also print the error in debug format to see full structure
            println!("\nDebug format:");
            println!("{:?}", error);

            // Print the error source chain
            println!("\nError source chain:");
            let mut source = error.source();
            let mut level = 0;
            while let Some(err) = source {
                println!("  {}: {}", level, err);
                source = err.source();
                level += 1;
            }
        }
    }
}
