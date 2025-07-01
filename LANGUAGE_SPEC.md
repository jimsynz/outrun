# Outrun Language Specification

## Overview

Outrun is a statically-typed, functional programming language built around the concept of traits. The language emphasises immutability, named parameters, explicit error handling, and a powerful type system designed for concurrent, distributed systems.

## Core Design Principles

- **Everything is traits** - All types implement traits, which define behaviour
- **Named arguments only** - No positional arguments in function calls
- **Static typing** with trait constraints and guards
- **Immutable and functional** - No mutation, rebinding allowed
- **Actor model runtime** - Built for concurrent, distributed systems with fault isolation
- **Explicit error handling** - `Option<T>` and `Result<T, E>` for expected failures, panics for exceptional cases
- **Pest-based parser** - Complete parsing implementation with comprehensive error reporting

## Type System and Runtime Model

### Concrete Types vs Traits

Outrun distinguishes between **concrete types** (actual runtime values) and **traits** (behaviour interfaces):

- **Concrete types**: `Outrun.Core.Integer64`, `Outrun.Core.Float64`, `Outrun.Core.Boolean`, `Outrun.Core.String`, `Outrun.Core.Atom`
- **Traits**: `Integer`, `Float`, `Boolean`, `String` define behaviour interface
- **Static dispatch**: Integer literals default to `Outrun.Core.Integer64` but other concrete types can implement `Integer`
- **Type checking**: The type checker maintains trait implementation mappings, not the interpreter
- **Static analysis**: All trait compatibility validated at compile time, not runtime

### Interning System

For performance and memory efficiency, atoms and types use interning:

- **AtomId**: Interned atom names like `:hello` - any two atoms with same name are identical objects
- **TypeNameId**: Interned type names like `Outrun.Core.Integer64`, `Outrun.Core.List<Outrun.Core.String>`
- **Fast equality**: Atoms and types compared by ID, not string comparison
- **Collections store types**: `List { element_type: TypeId, ... }`, `Tuple { element_types: Vec<TypeId>, ... }`

### Collection Types

All collections are strongly typed and store their element type information:

- **Lists**: `Outrun.Core.List<T>` - homogeneous linked lists (not vectors) for functional programming
- **Tuples**: `Outrun.Core.Tuple<T1, T2, ...>` - statically-typed product types with fixed arity
- **Maps**: `Outrun.Core.Map<K, V>` - core type with literal syntax and spread operations

### Option and Result Types

Core types for explicit error handling:

- **Option**: `Outrun.Option.Some<T>{ value: T }` and `Outrun.Option.None{}`
- **Result**: `Outrun.Result.Ok<T>{ value: T }` and `Outrun.Result.Err<E>{ error: E }`
- **Creation**: `Option.some(value: T)` and `Option.none()`, `Result.ok(value: T)` and `Result.err(error: E)`

### Error Handling Strategy

Outrun uses explicit error handling with no exceptions or implicit failures:

- **Expected failures**: Use `Option<T>` for potentially missing values, `Result<T, E>` for operations that can fail
- **Guard-based validation**: Functions use guards to catch error conditions explicitly
- **Example**: `def div(lhs: Integer64, rhs: Integer64): Result<Integer64, Error> when Integer.zero?(value: rhs) { RuntimeError }`
- **Actor panics**: Reserve for truly unexpected situations (VM errors, assertion failures, impossible states)
- **Fault isolation**: Panics only terminate the failing actor, not the whole system
- **No truthiness**: Only values implementing the `Boolean` trait can be used in conditionals

## Module System and Function Dispatch

Outrun's module system is based on types, with clear separation between static functions and trait dispatch.

### Types ARE Modules

- **Module definition**: Each type (struct/trait) defines its own module namespace
- **File organization**: File structure is conventional - module names come from type definitions
- **Example**: `struct Http.Client(...)` creates the `Http.Client` module
- **Multiple types per file**: Each type has its own module namespace, even in the same file

### Two Types of Function Calls

1. **Static module functions**: `Integer64.parse(value: "123")` - direct module function lookup
2. **Trait functions**: `let x: Integer64 = Parseable.parse(value: "123")` - explicit type annotation guides dispatch

### Module Privacy

- **Private functions**: `defp` functions only visible within their containing module (type)
- **Module boundaries**: Each type defines its own private function namespace
- **Cross-module isolation**: Private functions cannot be called from other modules, even in same file
- **File independence**: Privacy is module-scoped, not file-scoped

### Trait Implementation Dispatch

- **Type checker responsibility**: Builds module lookup table.
- **Runtime dispatch**: `Display.to_string(value: int)` becomes module lookup + function call within module
- **Opaque trait modules**: Separate from type's own module
- **No naming conflicts**: Static functions in type module vs trait functions in separate impl modules

### Module Resolution

- **No dynamic module lookup**: All calls are either static module functions or trait dispatch
- **No implicit imports**: `String.length()` is trait function dispatch, not implicit `Outrun.Core.String`
- **Alias resolution**: All aliases expanded to fully qualified names before runtime
- **Very simple type inference initially**: Manually implemented inference for common idioms.
- **Clean separation**: Typechecker desugars aliases, interpreter does module dispatch with qualified names

### Function Types and Named Parameters

Functions store parameter and return type information:

```rust
Value::Function {
    param_types: Vec<(AtomId, TypeId)>,  // (param_name, type)
    return_type: TypeId,
    body: FunctionBody,
}
```

- **Parameter names as AtomId**: Consistent with atom interning system
- **Unique parameter validation**: At load/compile time, not runtime (set-like semantics)
- **Very simple type inference initially**: Manually implemented inference for common idioms.

### Function Call Semantics

Outrun enforces strict function call requirements with no magical partial application:

- **All required parameters must be provided**: Compile-time error if missing arguments
- **Optional parameters**: Can use `Default.default()` for missing optional arguments
- **No partial application**: If you want currying behavior, create explicit wrapper functions
- **Clear function signatures**: What you see is what you get - no hidden partial application behavior
- **Named parameter enforcement**: All function calls use named parameters, no positional arguments

### Closure Semantics

Closures use capture-by-value with efficient reference counting optimization:

- **Simple mental model**: Closures "copy" all captured variables from their environment
- **No lifetime management**: No complex borrowing rules or lifetime annotations needed
- **Reference counting optimization**: Within an actor, "copies" are actually reference count increments
- **Immutable safety**: Safe to share references since values cannot be mutated
- **Actor isolation**: Each actor has its own memory arena - no cross-actor reference sharing

## Actor Model and Message Passing

Outrun is designed for the actor model but starts with a simplified single-actor approach:

### Single-Actor Development Model

- **Initial scope**: Interpreter runs as single actor in sole-actor system
- **Simplification**: Avoids multi-actor complexity during language development
- **Pragmatic approach**: Get core language working first, add actor complexity later
- **Future evolution**: Design allows later expansion to multi-actor system

### Message Passing Semantics

- **Within actor**: Reference counting for efficient sharing within single actor's memory arena
- **Between actors**: Deep copy/serialization when sending messages between actors
- **Isolation guarantee**: No shared state across actor boundaries - each actor owns its data
- **Performance trade-off**: Efficient intra-actor operations, explicit cost for inter-actor communication

### Actor Primitives

- **Message primitives**: `send` and `receive` implemented as NIFs (Native Implemented Functions)
- **NIF implementation**: Allows testing actor semantics without implementing full actor system
- **Future integration**: NIFs can be replaced with native actor runtime later

## Macro System

### Development Strategy

- **Chicken-and-egg problem**: Need working interpreter to expand macros, but building interpreter to run language with macros
- **Pragmatic deferral**: Focus on core language features first
- **Future implementation**: Add macro support once core interpreter is stable
- **Runtime expansion approach**: When implemented, macros will likely expand during interpretation

## Display and Inspect Formatting

Two separate formatting traits with different semantics:

- **Display trait**: Clean, user-facing formatting for end users
- **Inspect trait**: REPL pretty-printing with structure/debug info for developers
- **String interpolation**: `"Hello #{expr}"` uses Display trait on expression result

## Basic Syntax Elements

### Comments

```outrun
# Single line comment

###
Block comment
can span multiple lines
###
```

### Literals

#### Integers

```outrun
42          # Decimal
0b1010      # Binary
0o755       # Octal
0xFF        # Hexadecimal
```

#### Floats

```outrun
3.14        # Standard notation
1.23e-4     # Scientific notation
```

#### Booleans

```outrun
true
false
```

#### Strings

```outrun
"Hello, world!"                    # Basic string
"Name: #{user.name}"               # String interpolation
"""
Multi-line string
can span multiple lines
"""                                # Multi-line string
```

#### Atoms

```outrun
:name                              # Basic atom
:user_id                          # Atom with underscore
:some_atom                        # Atom with multiple parts
:"arbitrary string"               # Atom with arbitrary string content
:"with spaces and symbols!"       # Atom with special characters
```

#### Collections

```outrun
[1, 2, 3]                         # List
(42, "hello", true)               # Tuple
{name: "James", age: 35}          # Map

# List construction with head|tail syntax (similar to Elixir's [head | tail])
[head, ..tail]                   # Prepend element(s) to existing list
```

### Sigils

Sigils are trait-based syntactic sugar for parsing string literals:

```outrun
~SQL"""
SELECT * FROM users WHERE id = #{user_id}
"""
# Equivalent to: SQL.parse("SELECT * FROM users WHERE id = #{user_id}")
# Returns: Result<SQL, ParseError>
```

## Type System

### Struct Definitions

```outrun
struct User(id: UUID, email: String, name: Option<String>) {
    # Functions go here
}

# With generics
struct Container<T>(value: T) {
    # Functions go here
}

# With trait constraints
struct Processor<T>(data: T) when T: Serializable && T: Validatable {
    # Functions go here
}
```

### Trait Definitions

```outrun
trait Drawable {
    def draw(self: Self): String
}

# With generics
trait Container<T> {
    def get(self: Self): T
    def put(self: Self, value: T): Self
}

# With constraints
trait Serializable<T> when T: Display {
    def serialize(self: Self): String
    def deserialize(data: String): Result<Self, Error>
}
```

### Trait Implementation

```outrun
impl Drawable for User {
    def draw(self: Self): String {
        "User(#{self.email})"
    }
}

# With generics
impl<T> Display for SynthesizerBank<T> when T: Waveform {
    def to_string(self: Self): String {
        "SynthBank: #{self.patches.length()} patches loaded"
    }

    def add_patch(self: Self, patch: T): Self {
        SynthesizerBank { patches: [patch, ..self.patches] }
    }
}
```

### Static Trait Functions

Traits can define static functions using the `defs` keyword. These functions are implemented in the trait itself and provide constructor patterns and trait-level utilities without requiring implementation by concrete types. They cannot refer to the `Self` type.

```outrun
trait Result<T, E> {
    # Static constructor functions - implemented in the trait
    defs ok(result: T): Result<T, E> {
        Result.Ok { result: result}
    }

    defs error(error: E): Result<T, E> {
        Result.Error { error: error }
    }

    # Instance functions - must be implemented by concrete types
    def is_ok?(self: Self): Boolean
    def unwrap(self: Self): T
    def map<U>(self: Self, f: Function<(T) -> U>): Result<U, E>
}

trait Option<T> {
    # Static constructors
    defs some(value: T): Option<T> {
        Option.Some { value: value }
    }

    defs none(): Option<T> {
        Option.None {}
    }

    # Instance functions
    def some?(self: Self): Boolean
    def unwrap(self: Self): T
}
```

**Static Function Characteristics:**

- **No `Self` parameter**: Static functions don't operate on values
- **Trait-level implementation**: Function body is defined in the trait itself
- **Constructor patterns**: Commonly used for ergonomic type construction
- **Callable via trait name**: `Result.ok(value: 42)`, `Option.some(value: "hello")`
- **Not implemented by types**: Concrete types implementing the trait don't provide these functions
- **Generic support**: Can use trait's generic parameters in signatures and bodies

**Usage Examples:**

```outrun
# Create Result values using static constructors
let success = Result.ok(value: 42)
let failure = Result.error(error: "Invalid input")

# Create Option values
let some_value = Option.some(value: "hello")
let no_value = Option.none()
```

## Generic Types

Generic types provide parametric polymorphism in Outrun, allowing types and traits to work with multiple concrete types while maintaining type safety. Generic parameters are declared on **structs and traits only** - functions do not declare their own generic parameters.

### Generic Type Declaration

Generic parameters are declared using angle brackets `<T>` on structs and traits:

```outrun
# Generic struct with single parameter
struct Container<T>(value: T) {
    def get(self: Self): T { self.value }
}

# Generic struct with multiple parameters
struct Pair<T, U>(first: T, second: U) {
    def swap(self: Self): Pair<U, T> {
        Pair { first: self.second, second: self.first }
    }
}

# Generic trait with parameter
trait Serializable<T> {
    def serialize(self: Self): T
    def deserialize(data: T): Result<Self, String>
}
```

### Generic Constraints

Generic parameters can be constrained to implement specific traits using the `when` clause:

```outrun
# Single constraint
struct Processor<T>(data: T) when T: Display {
    def show(self: Self): String {
        T.to_string(self.data)
    }
}

# Multiple constraints
struct RaceTrack<T>(segments: List<T>) when T: Measurable && T: Navigable {
    def calculate_lap_time(self: Self): Duration {
        # Can use both Measurable and Navigable operations
    }
}

# Trait with constraints
trait NeonDisplay<T> when T: Illuminated {
    def render_frame(self: Self): Frame
}
```

### Generic Type Usage in Functions

Functions **do not declare generic parameters** like `def foo<T>()`. Instead, functions use existing generic types in their signatures:

```outrun
# ✅ Valid: Using existing generic types
def process_list(items: List<String>): List<Integer> {
    # Process List<String> to produce List<Integer>
}

def handle_result(result: Result<User, DatabaseError>): Option<User> {
    # Process Result to produce Option
}

def create_container(value: String): Container<String> {
    Container { value: value }
}

# ❌ Invalid: Functions cannot declare generic parameters
# def map<T, U>(list: List<T>, fn: T -> U): List<U>  # Not supported
```

The only generic types that can be referred to by function arguments or return values are those which are defined in the surrounding type, eg:

```outrun
struct Point<T>(x: T, y: T) {
    def as_tuple(point: Self): (T, T) {
        (point.x, point.y)
    }
}
```

### Generic Type Arguments

When using generic types, you specify concrete type or trait arguments:

```outrun
# Creating values of generic types
let numbers: List<Integer> = List.new()
let user_result: Result<User, String> = Result.ok(value: user)
let name_option: Option<String> = Option.some(value: "Alice")

# Generic types in struct literals
let container = Container<String> { value: "hello" }
let pair = Pair<Integer, String> { first: 42, second: "answer" }
```

### Generic Implementation Blocks

Implementations can be generic over type parameters:

```outrun
# Generic implementation
impl Display<T> for Container<T> when T: Display {
    def to_string(self: Self): String {
        "Container(#{T.to_string(self.value)})"
    }
}

# Implementation for specific generic instantiation
impl Double for Pair<Integer> {
    def double(self: Self): Integer {
        Self { a: self.a * 2, b: self.b * 2 }
    }
}
```

### Built-in Generic Types

Outrun provides several built-in generic types:

- **`Option<T>`**: Optional values that may be `Some<T>` or `None`
- **`Result<T, E>`**: Success (`Ok<T>`) or failure (`Err<E>`) values
- **`List<T>`**: Homogeneous collections of type `T`
- **`Map<K, V>`**: Key-value associations from `K` to `V`
- **`Function<(args...) -> ReturnType>`**: Function types with specified signatures

### The Self Type

`Self` is a special generic type that always refers to the implementing concrete type:

```outrun
trait Synthesizable {
    def create_waveform(self: Self): Waveform  # Self refers to the implementing type
}

impl Synthesizable for CyberSynth {
    def create_waveform(self: Self): Waveform {  # Self = CyberSynth in this implementation
        Waveform { frequency: self.frequency, amplitude: self.amplitude }
    }
}

impl Synthesizable for LaserHarp<String> {
    def create_waveform(self: Self): Waveform {  # Self = LaserHarp<String> in this implementation
        Waveform.from_note(note: self.current_note)
    }
}
```

**Self Type Characteristics:**

- **Always available**: `Self` is implicitly available in all trait and struct function definitions
- **Refers to implementer**: In trait implementations, `Self` refers to the concrete implementing type
- **Type-safe**: Ensures functions return the correct concrete type, not a generic
- **No explicit declaration**: `Self` doesn't need to be declared like other generic parameters

### Generic Type Rules

1. **Declaration Scope**: Only structs and traits can declare generic parameters
2. **Usage Scope**: Functions, variables, and expressions can use existing generic types
3. **Constraint Scope**: Generic constraints (`when T: Trait`) are only valid on struct and trait declarations
4. **Type Arguments**: Must be provided when creating values of generic types
5. **Variance**: Generic types are invariant (no subtyping between `Container<A>` and `Container<B>`)
6. **Self Type**: `Self` is a special generic that always refers to the implementing concrete type

### Function Types

Function types provide explicit type annotations for first-class functions. They specify the parameter types and return type for function values:

```outrun
# Basic function type syntax: Function<(param: Type) -> ReturnType>
def process(callback: Function<(x: Integer) -> String>) {
    callback(x: 42)
}

# Function type with no parameters
def run(task: Function<() -> Option>) {
    task()
}

# Function type with multiple parameters
def combine(merger: Function<(x: Integer, y: String) -> Boolean>) {
    merger(x: 1, y: "test")
}

# Function types in struct fields
struct EventHandler(
    on_click: Function<(event: Event) -> Unit>,
    validator: Function<(input: String) -> Boolean>
) {
    def handle_event(self: Self, event: Event) {
        if self.validator(event.data) {
            self.on_click(event)
        }
    }
}

# Functions using existing generic types in signatures
def process_list(
    list: List<String>,
    mapper: Function<(item: String) -> Integer>
): List<Integer> {
    List.map(list, mapper)
}

# Function types with complex generic types
def process_data(
    handler: Function<(data: String) -> Result<User, DatabaseError>>
) {
    handler("user_data")
}

# Function types in variable bindings and constants
const PROCESSOR: Function<(data: String) -> Integer> = &String.length
let validator: Function<(email: String) -> Boolean> = &Email.valid?
```

**Function Type Features:**

- **Explicit parameter types**: Each parameter must specify its type
- **Named parameters**: Function types follow the same named parameter convention
- **Generic support**: Return types can use generics like `Result<T, E>`
- **First-class values**: Function types work with function captures (`&`) and anonymous functions
- **Struct fields**: Function types can be used in struct field definitions
- **Higher-order functions**: Essential for map, filter, and other functional patterns

## Functions

### Function Definitions

```outrun
# Public function
def add(left: Integer, right: Integer): Integer {
    left + right
}

# Private function
defp validate_email(email: String): Boolean {
    String.contains?(string: email, pattern: "@")
}

# Function with guards
def divide(numerator: Integer, denominator: Integer): Result<Integer, DivisionError>
when Integer.non_zero?(value: denominator) {
    Result.ok(result: numerator / denominator)
}

def divide(numerator: Integer, denominator: Integer): Result<Integer, DivisionError>
when Integer.zero?(value: denominator) {
    Result.error(DivisionError.error(message: "Cannot divide by zero"))
}
```

### Function Return Types

All functions in Outrun **must have explicit return type annotations** and **must return a value**. Every function body contains at least one expression, and all expressions produce values:

```outrun
# ✅ Valid: Function returns calculated value
def calculate(x: Integer): Integer { x * 2 }

# ✅ Valid: Function returns Option value
def process_data(msg: String): Option<String> {
    if String.empty?(string: msg) {
        Option.none()
    } else {
        Option.some(option: String.uppercase(string: msg))
    }
}

# ✅ Valid: Function that performs side effects but still returns a value
def log_and_return(msg: String): String {
    Logger.info(msg)  # Side effect
    msg              # Return value
}

# ❌ Invalid: Missing return type annotation
# def invalid_function(x: Integer) { x * 2 }  # Compilation error
```

**Return Type Rules:**

1. **Mandatory Annotations**: Every function must specify its return type explicitly
2. **Must Return Value**: All functions must contain at least one expression that produces the return value
3. **Type Matching**: Function body must produce a value that matches the declared return type exactly
4. **Guard Functions**: Functions ending with `?` must return `Boolean`
5. **Generic Returns**: Functions can return generic types like `Option<User>`, `Result<T, E>`

### Function Calls

```outrun
# All parameters are named
result = add(left: 5, right: 3)

# Shorthand syntax when variable name matches parameter name
let left = 5
let right = 3
result = add(left, right)           # Equivalent to add(left: left, right: right)

# Mixed shorthand and explicit syntax
result = add(left, right: 10)      # Equivalent to add(left: left, right: 10)

# Remote function calls
user = User.create(email: "test@example.com", name: Option.some(option: "James"))

# Parentheses required even for no arguments
current_time = DateTime.now()
```

### Spread Arguments

Spread arguments allow passing struct fields as named parameters using the `..` operator:

```outrun
# Basic spread - pass all compatible struct fields as arguments
let login_result = LoginResult { user: user, session: session, created_at: timestamp }
process_user(..login_result)

# Mixed spread and explicit arguments (explicit arguments override spread)
process_user(..login_result, timestamp: custom_time)

# Multiple spreads (later spreads override earlier ones)
create_account(..signup_data, ..preferences, ..metadata)

# Lenient spread with ..? - ignores non-matching fields
let user_data = UserData { user: user, session: session, extra_field: "ignored" }
process_user(..?user_data, timestamp: explicit_time)
```

**Spread Mechanism:**

Spread arguments work through automatic trait derivation:

```outrun
# For this function definition:
def process_user(user: User, session: Session, timestamp: DateTime): Result<(), Error> {
    # Function implementation
}

# The compiler auto-generates a type whose name is opaque but is discoverable via the `Function` trait.
struct ProcessUserInput(user: User, session: Session, timestamp: DateTime)

# And automatically implements Spreadable<ProcessUserInput> for compatible structs:
trait Spreadable<T> {
    def spread(self: Self): T
}
```

**Conflict Resolution:**

When multiple sources provide the same parameter:

1. **Explicit arguments** override spread arguments
2. **Later spreads** override earlier spreads
3. **Lenient spreads** (`..?`) ignore mismatched fields

**Type Requirements:**

- Source struct fields must have matching names and compatible types
- Missing fields are filled using `Default.default()` if available
- Lenient spread (`..?`) ignores extra fields and missing fields without defaults

### Anonymous Functions

Anonymous functions provide lambda expressions with multiple clauses, guards, and pattern matching. They support the same guard-based dispatch as regular functions but with strict type consistency requirements.

```outrun
# Single expression
increment = fn { x: Integer -> x + 1 }

# Multiple expressions in block
logger = fn {
    message: String -> {
        let timestamp = DateTime.now()
        IO.puts("#{timestamp}: #{message}")
        Result.ok(result: ())
    }
}

# Multiple function heads with guards
classifier = fn {
    x: Integer when Integer.positive?(value: x) -> "positive"
    x: Integer when Integer.negative?(value: x) -> "negative"
    x: Integer when Integer.zero?(value: x) -> "zero"
}

# Multiple parameters with guards
comparer = fn {
    (x: Integer, y: Integer) when Integer.greater?(left: x, right: y) -> "first is greater"
    (x: Integer, y: Integer) when Integer.less?(left: x, right: y) -> "second is greater"
    (x: Integer, y: Integer) when Integer.equal?(left: x, right: y) -> "equal"
}

# No parameters
generator = fn { () -> UUID.new() }

# Complex parameter patterns
processor = fn {
    _user: User { name, email } when String.contains?(string: email, substring: "@") -> process_user(name: name, email: email)
    _user: Guest { session_id } -> process_guest(id: session_id)
    _user: Admin { permissions } -> process_admin(perms: permissions)
}
```

#### Anonymous Function Type Rules

Anonymous functions follow strict type consistency rules to enable static analysis and efficient dispatch:

**1. Parameter Signature Consistency**

- All clauses in an anonymous function **must have identical parameter signatures**
- Parameter names, types, and arity must match exactly across all clauses
- Pattern types are considered part of the parameter signature

```outrun
# ✅ Valid: Same parameter signature across clauses
processor = fn {
    x: Integer when Integer.positive?(value: x) -> x * 2
    x: Integer when Integer.negative?(value: x) -> x * -1
    x: Integer -> 0  # Default case, same signature
}

# ❌ Invalid: Different parameter types
mixed_processor = fn {
    x: Integer -> x * 2      # Parameter type: Integer
    x: String -> x.length()  # Parameter type: String - COMPILATION ERROR
}

# ❌ Invalid: Different parameter arity
arity_mismatch = fn {
    x: Integer -> x * 2                    # One parameter
    (x: Integer, y: Integer) -> x + y      # Two parameters - COMPILATION ERROR
}
```

**2. Return Type Consistency**

- All clauses **must return the same concrete type**
- Return type is inferred from the first clause and validated against all subsequent clauses
- No implicit type conversions or common supertype inference

```outrun
# ✅ Valid: All clauses return String
formatter = fn {
    x: Integer when Integer.positive?(value: x) -> "positive: #{x}"
    x: Integer when Integer.negative?(value: x) -> "negative: #{x}"
    x: Integer -> "zero"
}

# ❌ Invalid: Mixed return types
mixed_returns = fn {
    x: Integer when Integer.positive?(value: x) -> "positive"  # Returns String
    x: Integer when Integer.negative?(value: x) -> x          # Returns Integer - COMPILATION ERROR
    x: Integer -> 0                                           # Returns Integer - COMPILATION ERROR
}

# ❌ Invalid: Option vs concrete type mismatch
option_mismatch = fn {
    x: Integer when Integer.positive?(value: x) -> Option.some(value: x)  # Returns Option<Integer>
    x: Integer -> x                                                       # Returns Integer - COMPILATION ERROR
}
```

**3. Guard Function Requirements**

- Guards must be side-effect-free functions returning Boolean
- Guards can access all parameters bound by the clause pattern
- Guards follow the same rules as function guards (functions ending with `?`)

```outrun
# ✅ Valid: Guards return Boolean
validator = fn {
    _user: User { age } when Integer.greater?(value: age, other: 18) -> "adult"
    _user: User { age } when Integer.positive?(value: age) -> "minor"
    _user: User { age } -> "invalid age"
}

# ❌ Invalid: Guard doesn't return Boolean
invalid_guard = fn {
    x: Integer when Integer.abs(value: x) -> "processed"  # Guard returns Integer, not Boolean - COMPILATION ERROR
    x: Integer -> "unprocessed"
}
```

**4. Pattern Matching Consistency**

- When using pattern matching in parameters, all clauses must use the same pattern structure
- Cannot mix simple parameters with pattern parameters

```outrun
# ✅ Valid: All clauses use struct patterns
struct_processor = fn {
    _user: User { name } when String.not_empty?(value: name) -> process_user(name: name)
    _user: User { name } -> process_anonymous_user()
}

# ❌ Invalid: Mixing pattern types
pattern_mismatch = fn {
    _user: User { name } -> process_user(name: name)    # Struct pattern
    x: User -> process_user_simple(user: x)      # Simple parameter - COMPILATION ERROR
}
```

**5. Function Type Inference**

- Anonymous functions have type `Function<(params...) -> ReturnType>`
- Parameter and return types are inferred from clause analysis
- Function type is used for type checking in assignments and function calls

```outrun
# Function type: Function<(x: Integer) -> String>
let processor: Function<(x: Integer) -> String> = fn {
    x: Integer when Integer.positive?(value: x) -> "positive"
    x: Integer -> "non-positive"
}

# Type compatibility checking
def apply_processor(data: Integer, proc: Function<(x: Integer) -> String>): String {
    proc(x: data)
}

let result = apply_processor(data: 42, proc: processor)  # ✅ Valid: types match
```

**Error Messages:**

- Parameter signature mismatches produce clear compilation errors
- Return type inconsistencies highlight the conflicting clauses
- Guard validation errors specify the problematic guard expression
- Type inference failures provide expected vs actual type information

These rules ensure that anonymous functions are statically analyzable, enabling efficient compilation and clear error reporting while maintaining the flexibility of guard-based dispatch.

### Function Capture Syntax

Outrun supports capturing functions as first-class values using the `&` operator:

```outrun
# Simple function capture
let mapper = &String.upcase
let validator = &String.not_empty?
let processor = &my_local_function

# Qualified function capture
let logger = &Logger.error
let connector = &Database.connect

# With arity specification (for overloaded functions)
let simple_logger = &Logger.log/1
let detailed_logger = &Logger.log/2

# Used in higher-order functions
users |> List.filter(filter: &User.verified?)
names |> List.map(map: &String.upcase)

# For complex expressions, use full anonymous function syntax
users |> List.filter(filter: fn { user: User -> user.age >= 18 && user.verified })
coordinates |> List.map(map: fn { point: Point -> Point { x: point.x * 2, y: point.y * 2 } })
```

## Control Flow

### If/Else (Rust-style)

```outrun
result = if condition {
    "true branch"
} else {
    "false branch"
}

# Single expression
value = if check { 1 } else { 0 }
```

### Case Statements

Case statements use **unified pattern matching** with optional guards. All case branches follow the same syntax: `pattern when guard -> result`.

#### Unified Case Expression Syntax

Case expressions support all pattern types with optional guards in a single, consistent syntax:

```outrun
result = case user {
    User { name } when String.equal?(value: name, other: "Marty") -> :is_marty
    User { name } when String.length(string: name) > 10 -> :long_name
    User { name } -> :other_user
    Guest { session_id } -> :guest
    Admin { permissions } when List.contains?(list: permissions, item: :admin) -> :admin_user
    other -> :unknown_user  # Identifier pattern catches anything
}

# Pattern matching with complex destructuring and guards
result = case data {
    (User { name }, Session { active }) when Boolean.and?(left: active, right: String.not_empty?(value: name)) -> {
        Logger.info(message: "Active user session")
        :valid_session
    }
    (User { name }, Session { active: false }) -> :inactive_session
    (Guest { id }, session) -> :guest_session  # Binds second element as 'session'
    other -> :invalid_data  # Default case with binding
}

# Pattern types: struct, tuple, list, value, and identifier patterns
result = case value {
    # Struct destructuring
    User { name, email } when String.contains?(string: email, substring: "@") -> "valid user"

    # Tuple destructuring
    (x, y) when Integer.positive?(value: x) && Integer.positive?(value: y) -> "positive coordinates"
    (x, y) -> "coordinates"

    # List destructuring with head/tail
    [head, ..tail] when List.not_empty?(list: tail) -> "list with multiple items"
    [single] -> "single item list"
    [] -> "empty list"

    # Value patterns (exact matches)
    0 -> "zero"
    42 -> "the answer"

    # Identifier patterns (match anything, bind to variable)
    n when Integer.positive?(value: n) -> "positive number"
    data when String.type?(value: data) -> "some string"

    # Identifier pattern as catch-all (no underscore needed)
    other -> "unknown: #{other}"
}

# Comprehensive example: unified patterns with guards using infix operators
result = case request_data {
    # Type pattern with destructuring and guard
    HttpRequest { method: "GET", path } when path |> String.starts_with?(prefix: "/api") -> {
        handle_api_get(path: path)
    }

    # Type pattern with full destructuring
    HttpRequest { method: "POST", path, body } -> {
        handle_post(path: path, body: body)
    }

    # Type pattern with partial destructuring and infix guard
    HttpRequest { method } when method == "DELETE" -> :unauthorized

    # Literal pattern with guard
    :health_check when System.healthy?() -> :ok

    # Identifier pattern with guard (catches any non-HttpRequest)
    other when Debug.enabled?() -> {
        Logger.debug(message: "Unknown request: #{other}")
        :invalid_request
    }

    # Final identifier pattern (default case)
    unknown -> :invalid_request
}
```

#### Trait Dispatch with Type Annotations

Case expressions can use type annotations to enable trait-based dispatch and exhaustiveness checking. When a case expression includes a type annotation `as TraitName`, the compiler enforces exhaustiveness across all concrete types implementing that trait:

```outrun
# Trait-based case with exhaustiveness checking
result = case mixed_values as Display {
    # Must handle all concrete types that implement Display
    # Compiler enforces exhaustiveness using orphan rules
    User { name } -> "User: #{name}"
    Product { title, price } -> "Product: #{title} (#{price})"
    Order { id, total } -> "Order ##{id}: #{total}"
    # Compiler error if any Display implementor is missing
}

# Trait dispatch with identifier patterns and guards
result = case data as Serializable {
    config when Config.valid?(config) -> config.serialize()
    settings when Settings.active?(settings) -> settings.to_json()
    metadata -> metadata.to_string()
    # Each identifier pattern binds the value with its concrete type
}
```

#### Pattern Types and Exhaustiveness

The unified case syntax supports all pattern types with intelligent exhaustiveness checking:

- **Type patterns**: `User { name }`, `Config { ... }` - match specific concrete types with destructuring
- **Identifier patterns**: `config`, `other` - match any value and bind to variable (default cases)
- **Literal patterns**: `42`, `"hello"` - match exact values
- **Complex patterns**: `(User { name }, active)` - nested destructuring with guards

**Exhaustiveness rules:**

- **Without type annotation**: Exhaustiveness based on patterns and identifier catch-alls
- **With type annotation** (`as Trait`): Must handle all concrete types implementing the trait
- **Orphan rule analysis**: Compiler determines possible implementations for exhaustiveness

#### Pattern Types

Case expressions use Outrun's **unified destructuring pattern system**. The same five pattern types work identically in `let` bindings and case statements:

- **Identifier patterns**: `data`, `_` - match anything and bind to that variable
- **Value patterns**: `42`, `"hello"` - match exact literal values
- **Struct patterns**: `User { name, email }` - destructure struct fields into variables
- **Tuple patterns**: `(x, y, z)` - destructure tuple elements into variables
- **List patterns**: `[head, ..tail]` - destructure list with head/tail syntax

See **Unified Destructuring Patterns** section below for complete details and examples.

#### Guards and Variable Scope

Guards are optional expressions that must evaluate to a Boolean. Variables bound by patterns are available in both guards and result expressions:

```outrun
case user_data {
    User { name, age } when Integer.greater?(value: age, other: 18) -> {
        # Both 'name' and 'age' are available here
        process_adult_user(name: name, age: age)
    }
    User { name } -> {
        # Only 'name' is available here (age wasn't bound)
        process_minor_user(name: name)
    }
    _ -> handle_invalid_user()
}
```

### Unified Destructuring Patterns

**Outrun uses a unified pattern system for all assignment contexts.** The same destructuring patterns work consistently across:

- **Variable bindings** (`let pattern = value`)
- **Case statements** (`case value { pattern -> ... }`)
- **Function parameters** (in anonymous functions)

This unification means any pattern that works in one context works in all contexts, providing consistent syntax and semantics throughout the language.

#### Pattern Types

All assignment contexts support the same five pattern types:

**1. Identifier Patterns** - Match anything, bind to variable:

```outrun
# In let bindings
let x = some_value                    # Binds any value to 'x'
let _ = expensive_computation()       # Convention: underscore means "unused"

# In case statements
case user_input {
    value -> process(value: value)    # Binds any value to 'value'
    _ -> handle_unknown()             # Convention: underscore means "unused"
}
```

**2. Value Patterns** - Match exact literals:

```outrun
# In let bindings (assertion)
let 42 = get_answer()                 # Asserts the value equals 42
let "hello" = get_greeting()          # Asserts the value equals "hello"

# In case statements (matching)
case user_input {
    42 -> "the answer"                # Matches exactly 42
    "quit" -> "goodbye"               # Matches exactly "quit"
    other -> process(value: other)    # Matches anything else
}
```

**3. Struct Patterns** - Destructure struct fields:

```outrun
# In let bindings
let User { name, email } = fetch_user()          # Extracts name and email
let Config { database: db } = load_config()     # Extracts database field as 'db'

# In case statements
case user_data {
    User { name, email } -> process_user(name: name, email: email)
    Guest { session_id } -> process_guest(id: session_id)
    Admin { permissions } -> process_admin(perms: permissions)
}
```

**4. Tuple Patterns** - Destructure tuple elements:

```outrun
# In let bindings
let (x, y) = get_coordinates()               # Extracts both coordinates
let ((a, b), (c, d)) = get_nested_pairs()   # Nested tuple destructuring

# In case statements
case coordinate_data {
    (x, y) when Integer.positive?(value: x) -> process_positive(x: x, y: y)
    (0, 0) -> "origin"                       # Exact tuple match
    (x, y) -> process_general(x: x, y: y)    # General tuple match
}
```

**5. List Patterns** - Destructure lists with head/tail:

```outrun
# In let bindings
let [head, ..tail] = process_items()              # Head/tail extraction
let [first, second, ..rest] = get_sequence()     # Multiple head elements

# In case statements
case item_list {
    [] -> "empty list"                            # Empty list match
    [single] -> process_single(item: single)      # Single item list
    [head, ..tail] -> process_multiple(head: head, tail: tail)
}
```

#### Pattern Composition and Nesting

**All patterns are fully recursive** - any pattern can contain any other pattern at any depth. This provides unlimited compositional power for complex data extraction:

```outrun
# Tuple containing nested patterns with literals and structs
let (42, [User { name, email }, :active], metadata) = get_response()

# List containing mixed pattern types
let [1, "success", User { name }, (x, y)] = parse_complex_response()

# Struct containing nested tuples and lists
let Config { database: (host, port), servers: [primary, ..backups] } = load_config()

# Deep nesting with multiple levels
let ((id, status), [Record { value }, Record { backup: (x, y) }], :valid) = process_data()
```

**Recursive Pattern Grammar:**

- **Tuples**: `(pattern1, pattern2, ...)` - each element can be any recursive pattern
- **Lists**: `[pattern1, pattern2, ..rest_identifier]` - each element can be any recursive pattern, but rest patterns (`..name`) must be identifiers only
- **Structs**: `Type { field1: pattern1, field2 }` - field patterns can be any recursive pattern
- **Literals**: `42`, `"text"`, `:atom` - terminal patterns (no recursion)
- **Identifiers**: `variable_name` - terminal patterns (no recursion)

**Practical Examples:**

```outrun
# API response parsing with deeply nested data
let Response {
    status: (code, message),
    data: [User { profile: Profile { settings } }, ..others],
    metadata: :success
} = api_call()

# Complex case matching with mixed patterns
case complex_data {
    (id, [Record { value: (x, y) }, :active], 200) -> process_valid(id: id, coords: (x, y))
    (_, [], error_code) when Integer.greater?(value: error_code, other: 400) -> handle_error(code: error_code)
    _ -> fallback_handler()
}

# Data pipeline with nested extraction
let (
    source_id,
    [Input { raw_data: [first, ..rest] }, processed],
    Output { results: (success_count, errors) }
) = pipeline_result
```

#### Guards in Assignment

Guards are supported in case statements and anonymous functions to add conditional logic to patterns:

```outrun
case user_age {
    age when Integer.greater?(value: age, other: 65) -> "senior"
    age when Integer.greater?(value: age, other: 18) -> "adult"
    age when Integer.positive?(value: age) -> "minor"
    _ -> "invalid age"
}
```

This unified approach ensures that learning one assignment context gives you expertise in all contexts, reducing cognitive load and maintaining consistency across the language.

## Operators

All operators are trait-based and follow Ruby's precedence rules. Operators are syntactic sugar for trait function calls with strict type requirements:

### Binary Operations and Type Safety

Binary operations like `a + b` are syntactic sugar for trait function calls:

- `a + b` becomes `BinaryAddition.add(lhs: a, rhs: b)`
- Trait functions defined as `def add(lhs: Self, rhs: Self): Self`
- **Both operands must be same concrete type**, result is same concrete type
- **No implicit conversions** - requires explicit type conversion for mixed operations

### Arithmetic

- `+` Addition (BinaryAddition trait)
- `-` Subtraction (BinarySubtraction trait)
- `*` Multiplication (BinaryMultiplication trait)
- `/` Division (BinaryDivision trait)
- `%` Modulo (BinaryModulo trait)
- `**` Exponentiation (BinaryExponentiation trait)

### Comparison

- `==` Equality (Equality trait)
- `>`, `>=` Greater than (Comparison trait)
- `<`, `<=` Less than (Comparison trait)

### Logical

- `&&` Logical AND (LogicalAnd trait)
- `||` Logical OR (LogicalOr trait)
- `!` Logical NOT (LogicalNot trait)

### Bitwise

- `&` Bitwise AND (BitwiseAnd trait)
- `|` Bitwise OR (BitwiseOr trait)
- `^` Bitwise XOR (BitwiseXor trait)
- `~` Bitwise complement (BitwiseNot trait)
- `<<`, `>>` Bitwise shift (BitwiseShift trait)

### Unary

- `+` Unary plus (UnaryPlus trait)
- `-` Unary minus (UnaryMinus trait)

### Pipe Operators

- `|>` Pipe (calls `Pipe.pipe_into` trait method)
- `|?` Pipe with unwrap (calls `Maybe.maybe_pipe` trait method)

## Type Annotations

Type annotations provide explicit type information to the type checker for validation and trait dispatch. Outrun uses two syntactic forms for the same underlying concept:

### Annotation Syntax

**Variable binding syntax**: `: Type`

```outrun
let name: String = "James"
let config: Config = parse_config()
let result: Result<User, Error> = User.create(name: name)
```

**Expression syntax**: `as Type`

```outrun
let result = Sigil.parse(input: "content") as HTML
let data = (fetch_data() |> transform()) as ProcessedData
let parsed = parse_json(text: input) as Config
let total = (price * quantity) as Currency
```

### Unified Semantics

Both syntactic forms provide the same functionality:

1. **Static type checking**: Compiler verifies type compatibility at compile time
2. **Trait dispatch**: Enables calling trait functions on the annotated type
3. **Type validation**: Ensures the value matches the specified type
4. **No runtime overhead**: Pure compile-time information

### Expression Type Annotations (`as Type`)

The `as Type` syntax is an infix operator for annotating expressions:

- **Precedence**: Lower than most operators, higher than pipes (`|>`, `|?`) and logical operators
- **Associativity**: Right-associative (`expr as Type1 as Type2` → `expr as (Type1 as Type2)`)
- **Usage**: Anywhere an expression needs explicit type information

**Common patterns:**

```outrun
# Sigil desugaring (automatic)
~HTML"content"  # Becomes: Sigil.parse(input: "content") as HTML

# Trait method chaining
let text = (user_input as Displayable).to_string()

# Case expression trait dispatch
case data as Serializable {
    config -> config.to_json()
    settings -> settings.serialize()
}

# Disambiguation in complex expressions
let result = (compute_value() * factor) as PreciseNumber
```

Both `: Type` and `as Type` are compile-time type annotations that serve identical purposes in different syntactic contexts.

## Variables and Constants

### Variable Binding

Variable bindings support optional type annotations and powerful destructuring patterns. Type annotations are required during early development until type inference is implemented:

```outrun
# Basic variable bindings with explicit type annotations
let name: String = "James"
let age: Integer = 35
let user: User = User.create(name: name, age: age)

# Type inference syntax (supported by grammar, future feature)
let name = "James"           # Will infer String
let age = 35                 # Will infer Integer
let user = User.create(...)  # Will infer User

# Rebinding allowed (not mutation)
let name: String = "James Neil"
```

**Complex Variable Patterns:**

Variable bindings support sophisticated destructuring patterns for extracting values from nested data structures:

```outrun
# Multiple nested struct destructuring
let User { name, profile } = fetch_user()
let UserProfile { preferences, settings } = profile
let UserPreferences { theme, language } = preferences

# Nested tuple and list patterns
let ((x, y), (lat, lon)) = get_coordinates()
let [head, ..tail] = process_items()
let [first_item, second_item, ..remaining] = inventory_list

# Mixed patterns with complex structures
let complex_tuple = ((user_id, session), [first_pref, ..other_prefs])
let ((extracted_id, extracted_session), preference_data) = complex_tuple
let [extracted_first, ..extracted_rest] = preference_data

# List construction with head|tail syntax
let enhanced_list = [new_item, ..existing_items]
let with_header = [header, ..body_data]

# Struct patterns in tuples
let config_data = (Config { database, cache }, Settings { logging, monitoring })
let (database_config, app_settings) = config_data
```

See **Unified Destructuring Patterns** section above for complete details on the pattern system that works across all assignment contexts.

**Implementation Notes:**

- **Grammar**: Supports both explicit and inferred types
- **Current Parser**: Requires explicit type annotations
- **Future**: Type inference will allow omitting obvious types
- **Recursive Patterns**: Parser must implement recursive descent for destructuring patterns. Each pattern type (tuple, list, struct) must accept nested `destructure_pattern` rules rather than just identifiers
- **AST Structure**: All destructuring creates `destructure_pattern` nodes containing the specific pattern type, enabling uniform processing across all assignment contexts

### Constants

Constants are always module-private. To expose values, use functions:

```outrun
# Private constants
const MAX_USERS: Integer = 1000
const DEFAULT_TIMEOUT: Duration = Duration.seconds(30)

# Public accessors
def max_users(): Integer { MAX_USERS }
def default_timeout(): Duration { DEFAULT_TIMEOUT }
```

## Data Construction and Access

### Struct Construction

```outrun
user = User {
    id: UUID.new(),
    email: "james@example.com",
    name: Some("James")
}

# Shorthand syntax when variable name matches field name
let email = "james@example.com"
let name = Some("James")
user = User {
    id: UUID.new(),
    email,                          # Equivalent to email: email
    name                           # Equivalent to name: name
}

# Mixed shorthand and explicit syntax
user = User {
    id: UUID.new(),
    email,                          # Shorthand
    name: Some("Different Name")    # Explicit
}

# Struct update syntax
updated_user = User {
    email: "newemail@example.com",
    ..user
}
```

### Field Access

```outrun
email = user.email
first_element = tuple.0
```

## Module System

### Module Organization

Outrun uses a type-based module system where:

- **Types (structs/traits) ARE modules** - each type defines its own module namespace
- **File structure is conventional** - file names and directory structure are for organization, not module definition
- **Module names come from type definitions** - `struct Http.Client(...)` creates the `Http.Client` module

```
src/
  user.outrun               # Contains User struct/module (conventional)
  auth_token.outrun         # Contains AuthToken struct/module (conventional)
  http/
    client.outrun           # Contains Http.Client struct/module (conventional)
    server.outrun           # Contains Http.Server struct/module (conventional)
  user/
    preferences.outrun      # Contains User.Preferences struct/module (conventional)
    profile.outrun          # Contains User.Profile struct/module (conventional)
```

### Module References

```outrun
# Type names define module paths - each type IS a module
let user = User.create(name: "James")                    # User module
let client = Http.Client.new(timeout: 5000)             # Http.Client module
let preferences = User.Preferences.default()            # User.Preferences module

# Multiple types can be defined in the same file for organization
# but each has its own module namespace based on its name
struct Http.Client(...)     # Defines Http.Client module
struct Http.Request(...)    # Defines Http.Request module (separate from Http.Client)
```

### Aliases

```outrun
# Simple alias
alias MyApp.Very.Long.Module.Name as Short

# Default to last segment
alias MyApp.User  # Available as User

# Brace expansion
alias MyApp.{User, Post, Comment}
alias Ash.Resource.{Info as ARI, Change}

# Standard library needs explicit aliasing
alias Outrun.Option as Option
alias Outrun.Result as Result
```

### Imports

```outrun
# Elixir-style imports with arity
import Logger                           # All functions
import Logger, only: [info: 1, warn: 2] # Specific functions by arity
import Logger, except: [debug: 1]       # Exclude specific functions
```

## Attributes

Attributes are trait-based and use named parameter syntax:

```outrun
@Derive(traits: [Debug, Display])
struct SportsCar(model: String, speed: Integer) {
    # Functions go here
}

@Timeout(duration: Duration.seconds(5))
def slow_operation(): Result<String, Error> {
    # ...
}

@Validate(min_length: 3, format: :email)
def process_email(email: String): Result<Email, ValidationError> {
    # ...
}
```

Attributes implement the `Attribute` trait:

```outrun
trait Attribute {
    def apply(target: AST, args: Map<Atom, Any>): Result<AST, AttributeError>
}

impl Attribute for Derive {
    def apply(target: AST, args: Map<Atom, Any>): Result<AST, AttributeError> {
        # Generate derived trait implementations
    }
}
```

## Guard System

Guards are side-effect-free functions ending in `?` that return Boolean:

```outrun
# In function definitions
def safe_divide(a: Integer, b: Integer): Float
when Integer.non_zero?(value: b) {
    Float.from_integer(a) / Float.from_integer(b)
}

# In case statements
case number {
    n when Integer.even?(value: n) -> "even"
    n when Integer.odd?(value: n) -> "odd"
}

# In anonymous functions
classifier = fn {
    x: Integer when Integer.positive?(value: x) -> "positive"
    x: Integer when Integer.negative?(value: x) -> "negative"
}

# Guards can access multiple parameters
sorter = fn {
    (a: Integer, b: Integer) when Integer.less?(left: a, right: b) -> [a, b]
    (a: Integer, b: Integer) when Integer.greater_equal?(left: a, right: b) -> [b, a]
}

# Complex guard expressions
def process(data: String): Result<ProcessedData, Error>
when String.not_empty?(value: data) && String.valid_format?(value: data) {
    # Process the data
}
```

## Macros

Outrun has a hygienic macro system using the same syntax as the language:

```outrun
macro unless(condition, do_block) {
    if !^condition {        # ^ injects the argument AST
        ^do_block
    }
}

# Usage
unless(user.active?, {
    send_reactivation_email(user: user)
})
```

Macros use `^` to inject arguments from the call site, while variables without `^` remain hygienic (macro-local).

## Application Entry Point

Applications implement the `Application` trait:

```outrun
trait Application {
    def start(args: List<String>): Result<(), ApplicationError>
}

struct MyApp() {}

impl Application for MyApp {
    def start(args: List<String>): Result<(), ApplicationError> {
        # Application logic
        Ok(())
    }
}
```

The runtime discovers which struct implements `Application` through module introspection.

## Package System

Projects use TOML configuration files:

```toml
# outrun.toml
[package]
name = "my_app"
version = "0.1.0"
authors = ["James Harton <james@example.com>"]

[dependencies]
http_client = "1.0.0"
json = "2.1.0"

[tasks]
test = "outrun test"
build = "outrun build"
format = "outrun fmt"
```

## Core Traits

All functionality is built on traits, including operators and control flow:

**Operators:**

```outrun
trait BinaryAddition<T> { def add(left: Self, right: T): Self }
trait Comparison<T> { def compare(left: Self, right: T): Ordering }
trait Pipe<T> { def pipe_into(self: Self, f: fn(Self) -> U): U }
trait Maybe<T, E> { def maybe_pipe(self: Self, f: fn(T) -> Maybe<U, E>): Maybe<U, E> }
```

**Core Types:**

```outrun
alias Outrun.Option as Option
alias Outrun.Result as Result
alias Outrun.Iterator as Iterator

trait Option<T> {
    def some?(self: Self): Boolean
    def none?(self: Self): Boolean
    def unwrap(self: Self): T
}

trait Result<T, E> {
    def ok?(self: Self): Boolean
    def error?(self: Self): Boolean
    def unwrap(self: Self): T
    def unwrap_error(self: Self): E
}
```

**Sigils:**

```outrun
trait Sigil {
    def parse(content: String): Result<Self, ParseError>
}

impl Sigil for SQL {
    def parse(content: String): Result<SQL, ParseError> {
        SQL.compile(content)
    }
}

# Usage
query = ~SQL"SELECT * FROM users"
# Equivalent to: SQL.parse("SELECT * FROM users")
# Returns: Result<SQL, ParseError>
```

**Attributes:**

```outrun
trait Attribute {
    def apply(target: AST, args: Map<Atom, Any>): Result<AST, AttributeError>
}

# Usage: @Derive(traits: [Debug, Display])
```

**Destructuring:**

Outrun uses a **unified destructuring pattern system** that works consistently across variable bindings, case statements, and function parameters. See the "Unified Destructuring Patterns" section for complete details on pattern types and usage.

The examples below show destructuring in `let` binding contexts:

```outrun
# Basic tuple destructuring
let (x, y, z) = some_tuple

# Basic struct destructuring
let User { name, email } = user

# Basic list destructuring with rest patterns
let [first, second, ..rest] = list
```

**Complex Nested Destructuring:**

```outrun
# Nested struct destructuring
let User {
    profile: UserProfile {
        preferences: UserPreferences { theme, language }
    }
} = fetch_user()

# Nested tuple destructuring
let ((x, y), (lat, lon)) = get_coordinates()
let (first, second, third) = calculate_positions()

# Multiple destructuring assignments
let UserProfile { preferences, settings } = profile
let UserPreferences { theme, language } = preferences

# Complex tuple patterns with mixed types
let complex_tuple = ((user_id, session), [first_pref, other_prefs])
let ((extracted_id, extracted_session), preference_data) = complex_tuple

# Further destructuring of extracted data
let [extracted_first, ..extracted_rest] = preference_data

# Struct patterns in tuples
let config_data = (Config { database, cache }, Settings { logging, monitoring })
let (database_config, app_settings) = config_data
```

**Rest Patterns and Spread Syntax:**

Rest patterns (`..identifier`) provide Elixir-style head|tail operations and can be used in both **destructuring contexts** (extracting values) and **construction contexts** (building lists):

**Destructuring (extracting values):**

```outrun
# Rest patterns in destructuring - head|tail pattern
let [head, ..tail] = process_items()
let [first_item, second_item, ..remaining] = inventory_list
```

**Construction (building lists):**

```outrun
# Head|tail construction - prepend elements to existing list
let new_list = [first, ..existing_list]
let enhanced = [new_item, second_item, ..rest_of_list]

# Real-world examples
let shopping_list = ["milk", ..weekly_items]
let all_users = [current_user, ..existing_users]
let with_header = [header_item, ..body_items]
```

**Type Requirements:**

- In construction contexts, the spread variable must be of list type
- The resulting list type is inferred from all elements
- Only supports head|tail pattern (prepending to lists)

**Rest Pattern Constraints:**

- **Rest patterns in destructuring must be identifiers only**: `..tail`, `..rest`, `..remaining` (not complex patterns)
- **Main patterns are fully recursive**: Regular elements in lists/tuples can be any nested pattern
- **Example**: `[first, (x, y), ..rest]` - `first` and `(x, y)` can be complex patterns, but `rest` must be a simple identifier

**Case Expressions with Pattern Matching:**

Case expressions support pattern matching with optional guards, where bound variables are available in both guards and results:

```outrun
let result = case user_data {
    User { name, email } when String.contains?(string: email, substring: "@") -> {
        process_user(name: name, email: email)
    }
    User { name } -> {
        # email not available in this pattern
        process_user_without_email(name: name)
    }
    Guest { session_id } when String.not_empty?(value: session_id) -> {
        process_guest(id: session_id)
    }
    Guest { session_id } -> handle_invalid_guest()
    _ -> handle_unknown()
}
```

## Interning System and Performance

### Atom and Type Interning

Outrun uses a sophisticated interning system for performance and memory efficiency:

```rust
// Interpreter context manages interning (not global state)
struct InterpreterContext {
    atom_interner: AtomInterner,
    type_interner: TypeInterner,
    environment: Environment,
}

// Atoms are interned like symbols in other languages
// Any two atoms with same name are identical objects
let user_id_1 = :user_id  // AtomId(42)
let user_id_2 = :user_id  // AtomId(42) - same instance

// Types are interned for fast equality checks
// Collection types store their element types
Value::List { element_type: TypeId("Outrun.Core.String"), nodes: ... }
Value::Tuple { element_types: vec![TypeId("Outrun.Core.Integer64"), TypeId("Outrun.Core.String")], values: ... }
```

**Interning Benefits:**

- **Fast equality**: Compare by ID instead of string comparison
- **Memory efficiency**: Single instance per unique atom/type name
- **Collection optimization**: Element types stored efficiently
- **Function signatures**: Parameter names and types use interned IDs

**Implementation Notes:**

- Interning managed by interpreter context, not global state
- Thread-safe design for future multi-threaded execution
- String interning crates provide efficient implementation

### Reference Counting Within Actors

Within a single actor, values use reference counting for efficient sharing:

```rust
// Within actor: reference counting for efficiency
let shared_data = expensive_computation()
let closure1 = fn { -> process(shared_data) }  // Reference count increment
let closure2 = fn { -> validate(shared_data) } // Reference count increment

// No copying within actor boundaries
// Immutability ensures safe reference sharing
```

## Return Values

Functions return their last expression implicitly (like Rust/Elixir):

```outrun
def calculate(x: Integer): Integer {
    let doubled = x * 2
    let incremented = doubled + 1
    incremented  # This is returned
}
```

## Type System Implementation Details

### Concrete Type Hierarchy

The runtime type system distinguishes between concrete types and trait abstractions:

```rust
// Concrete types used at runtime
enum Value {
    Integer64(i64),           // Outrun.Core.Integer64
    Float64(f64),             // Outrun.Core.Float64
    Boolean(bool),            // Outrun.Core.Boolean
    String(String),           // Outrun.Core.String
    Atom(AtomId),             // Outrun.Core.Atom
    List {
        element_type: TypeId,
        nodes: LinkedList<Value>
    },                        // Outrun.Core.List<T>
    Tuple {
        element_types: Vec<TypeId>,
        values: Vec<Value>
    },                        // Outrun.Core.Tuple<T1, T2, ...>
    Map {
        key_type: TypeId,
        value_type: TypeId,
        entries: IndexMap<Value, Value>
    },                        // Outrun.Core.Map<K, V>
    Option(OptionValue),      // Outrun.Option.Some<T>, Outrun.Option.None
    Result(ResultValue),      // Outrun.Result.Ok<T>, Outrun.Result.Err<E>
}

// Trait abstractions define behaviour
// Integer trait can be implemented by Integer64, Integer32, BigInteger, etc.
// Float trait can be implemented by Float64, Float32, Decimal, etc.
```

### Type Checking Strategy

Type checking happens at compile time, not runtime:

```rust
// Type checker builds trait implementation lookup tables
struct TypeChecker {
    trait_impls: HashMap<(TraitId, TypeId), OpaqueModuleId>,
    type_registry: HashMap<TypeId, TypeInfo>,
    module_registry: HashMap<ModuleId, ModuleInfo>,
}

// Runtime uses pre-computed dispatch tables
// No dynamic type checking during interpretation
// All trait compatibility validated at compile time
```

### Function Type Representation

Functions store complete type information for validation:

```rust
Value::Function {
    param_types: Vec<(AtomId, TypeId)>,  // (param_name, type)
    return_type: TypeId,
    body: FunctionBody,
    captures: Vec<(AtomId, Value)>,      // Captured variables
}

// Parameter validation at load time
// Set-like semantics ensure unique parameter names
// Type annotations required initially (no inference)
```

## Development and Evolution Strategy

### Interpreter Development Phases

1. **Single-Actor Foundation**: Start with simplified actor model
2. **Core Language Features**: Implement all basic language constructs
3. **Type System Integration**: Add full trait dispatch and type checking
4. **Multi-Actor Evolution**: Expand to full actor model with message passing
5. **Performance Optimization**: Reference counting, interning, and compilation
6. **Macro System**: Add macro expansion once core language is stable

### Future Type Inference

The language is designed to support type inference in the future:

```outrun
// Current requirement: explicit type annotations
let name: String = "James"
let numbers: List<Integer> = [1, 2, 3]
let result: Result<User, Error> = User.create(name: name)

// Future: type inference where unambiguous
let name = "James"              // Infers String
let numbers = [1, 2, 3]         // Infers List<Integer>
let result = User.create(...)   // Infers Result<User, Error>

// Trait calls always require explicit type annotations
let parsed: Integer = Parseable.parse(value: "123")  // Required
```

### Compilation Strategy

While starting as an interpreter, Outrun is designed for eventual compilation:

- **AST preservation**: Parser maintains all source information
- **Type erasure preparation**: Runtime types can be optimized away
- **Trait dispatch optimization**: Static dispatch where possible
- **Actor model compilation**: Efficient message passing and scheduling
- **Memory management**: Reference counting can be optimized to stack allocation

## Whitespace and Parsing

The language aims to be whitespace-agnostic using context-sensitive parsing. The parser looks ahead to determine if lines continue:

```outrun
# These are equivalent:
user = User { name: "James", email: "james@example.com" }

user = User {
    name: "James",
    email: "james@example.com"
}

# Function composition with pipe operator
result = data
    |> process()
    |> validate()
    |> save()
```

No semicolons are required for statement termination.
