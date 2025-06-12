# Outrun Language Syntax Specification

## Overview

Outrun is a statically-typed, functional programming language built around the concept of traits (similar to protocols in Elixir). The language emphasises immutability, named parameters, and a powerful guard system for control flow.

## Core Design Principles

- **Everything is traits** - All types implement traits, which define behaviour
- **Named arguments only** - No positional arguments in function calls
- **Static typing** with trait constraints and guards
- **Immutable and functional** - No mutation, rebinding allowed
- **Actor model runtime** - Built for concurrent, distributed systems
- **Tree-sitter based** - Enables embedded DSLs with full language server support

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
impl<T> Container<T> for Box<T> when T: Clone {
    def get(self: Self): T {
        self.value.clone()
    }
    
    def put(self: Self, value: T): Self {
        Box { value: value }
    }
}
```

### Function Types

Function types provide explicit type annotations for first-class functions. They specify the parameter types and return type for function values:

```outrun
# Basic function type syntax: Function<(param: Type) -> ReturnType>
def process(callback: Function<(x: Integer) -> String>) {
    callback(42)
}

# Function type with no parameters  
def run(task: Function<() -> Unit>) {
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

# Function types with generic return types
def map_list<T, U>(
    list: List<T>, 
    mapper: Function<(item: T) -> U>
): List<U> {
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
    String.contains?(email, "@")
}

# Function with guards
def divide(numerator: Integer, denominator: Integer): Result<Integer, DivisionError>
when Integer.non_zero?(denominator) {
    Ok(numerator / denominator)
}

def divide(numerator: Integer, denominator: Integer): Result<Integer, DivisionError>
when Integer.zero?(denominator) {
    Err(DivisionError("Cannot divide by zero"))
}
```

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
user = User.create(email: "test@example.com", name: Some("James"))

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

# The compiler auto-generates:
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

```outrun
# Single expression
increment = fn { x: Integer -> x + 1 }

# Multiple expressions in block
logger = fn {
    message: String -> {
        let timestamp = DateTime.now()
        IO.puts("#{timestamp}: #{message}")
        Ok(())
    }
}

# Multiple function heads with guards
classifier = fn {
    x: Integer when Integer.positive?(x) -> "positive"
    x: Integer when Integer.negative?(x) -> "negative"
    x: Integer when Integer.zero?(x) -> "zero"
}

# Multiple parameters with guards
comparer = fn {
    (x: Integer, y: Integer) when Integer.greater?(x, y) -> "first is greater"
    (x: Integer, y: Integer) when Integer.less?(x, y) -> "second is greater"
    (x: Integer, y: Integer) when Integer.equal?(x, y) -> "equal"
}
```

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
users |> List.filter(&User.verified?)
names |> List.map(&String.upcase)

# For complex expressions, use full anonymous function syntax
users |> List.filter(fn { user: User -> user.age >= 18 && user.verified })
coordinates |> List.map(fn { point: Point -> Point { x: point.x * 2, y: point.y * 2 } })
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

```outrun
result = case value {
    when Integer.positive?(value) -> "positive"
    when Integer.negative?(value) -> {
        Logger.warn("Negative value encountered")
        "negative"
    }
    when Integer.zero?(value) -> "zero"
    else -> "unknown"
}
```

## Operators

All operators are trait-based and follow Ruby's precedence rules:

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

See **Destructuring** section below for complete details on pattern syntax and capabilities.

**Implementation Notes:**
- **Grammar**: Supports both explicit and inferred types
- **Current Parser**: Requires explicit type annotations
- **Future**: Type inference will allow omitting obvious types

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

Outrun uses a file-based module system where:
- Each `.outrun` file represents a module namespace
- Directory structure maps to module hierarchy with dots
- Types (structs/traits) within a file belong to that module namespace

```
src/
  user.outrun               # User module
  auth_token.outrun         # AuthToken module  
  http/
    client.outrun           # Http.Client module
    server.outrun           # Http.Server module
  user/
    preferences.outrun      # User.Preferences module
    profile.outrun          # User.Profile module
```

### Module References

```outrun
# Direct references using full module path
let user = User.create(name: "James")
let client = Http.Client.new(timeout: 5000)

# Multiple types in same module namespace
let client = Http.Client.HttpClient.new()
let request = Http.Client.HttpRequest.new()
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
@Derive(traits: [Debug, Clone])
struct User(email: String, name: String) {
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
when Integer.non_zero?(b) {
    Float.from_integer(a) / Float.from_integer(b)
}

# In case statements
case number {
    when Integer.even?(number) -> "even"
    when Integer.odd?(number) -> "odd"
}

# In anonymous functions
classifier = fn {
    x: Integer when Integer.positive?(x) -> "positive"
    x: Integer when Integer.negative?(x) -> "negative"
}

# Guards can access multiple parameters
sorter = fn {
    (a: Integer, b: Integer) when Integer.less?(a, b) -> [a, b]
    (a: Integer, b: Integer) when Integer.greater_equal?(a, b) -> [b, a]
}

# Complex guard expressions
def process(data: String): Result<ProcessedData, Error>
when String.not_empty?(data) && String.valid_format?(data) {
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

# Usage: @Derive(traits: [Debug, Clone])
```

**Destructuring:**

Outrun supports powerful destructuring patterns for extracting values from complex data structures:

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

**Destructuring in Case Expressions:**

Destructuring patterns can be used in case expressions for powerful pattern matching:

```outrun
let result = case user_data {
    when User { name, email } -> {
        process_user(name: name, email: email)
    }
    when Guest { session_id } -> {
        process_guest(id: session_id)
    }
    else -> handle_unknown()
}
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