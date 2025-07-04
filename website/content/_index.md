+++
title = "Outrun Programming Language"
description = "A modern programming language where everything is a trait. Built for elegance, type safety, and synthwave aesthetics."
template = "index.html"
+++

# Outrun Programming Language

Welcome to the future of programming - where everything is a protocol and code flows like synthwave music.

## What is Outrun?

Outrun is a modern programming language designed around the principle that **everything is a protocol**. Born from the desire to build elegant, type-safe systems without sacrificing expressiveness, Outrun brings together the best ideas from functional programming, protocol-based polymorphism, and actor-model concurrency.

Named after the retro-futuristic aesthetic of synthwave music and 80s culture, Outrun aims to capture that same sense of speed, style, and limitless possibility in code.

## Key Features

### 🎯 Everything is Protocols
All operators, control flow, and behaviour is defined through protocols:
- `+` calls `BinaryAddition.add()`
- `|>` calls `Pipe.pipe_into()`
- `|?` calls `Maybe.maybe_pipe()`
- `@Attribute()` calls `Attribute.apply()`
- `~Sigil""` calls `Sigil.parse()`

### 🏷️ Named Parameters Only
Crystal clear function calls with no confusion about argument order:
```outrun
user = User.create(
    name: "Neo",
    email: "neo@matrix.io"
)

divide(numerator: 10, denominator: 2)
```

### 🛡️ Powerful Guard System
Functions ending in `?` must be side-effect-free and return Boolean:
```outrun
def divide(a: Integer, b: Integer): Float 
when Integer.non_zero?(b) {
    Float.from_integer(a) / Float.from_integer(b)
}
```

### 🔮 Hygienic Macros
Same syntax as the language, using `^` for argument injection:
```outrun
macro unless(condition, do_block) {
    if !^condition {
        ^do_block
    }
}
```

### 🏗️ Module System
Types (structs/protocols) ARE modules! The file system maps directly to the module hierarchy:
```
src/
  user.outrun           → User module
  http/
    client.outrun       → Http.Client module
    server.outrun       → Http.Server module
```

### 🎭 No Nulls, No Exceptions
Explicit error handling with `Result` and `Option` types:
```outrun
case maybe_user {
    when Option.some?(maybe_user) -> {
        let user = Option.unwrap(maybe_user)
        process_user(user: user)
    }
    when Option.none?(maybe_user) -> handle_missing_user()
}
```

## Current Status

**Alpha Development** - Core language infrastructure complete:

- ✅ **Language Specification** - Complete syntax and semantics defined
- ✅ **BNF Grammar** - Formal grammar specification written
- ✅ **Parser Implementation** - Complete Pest-based parser with 449 tests
- ✅ **Expression Desugarer** - All operators transformed to protocol calls
- ✅ **Type Checker** - Comprehensive type checking with protocol system
- ✅ **Core Library** - Bootstrap library with dependency resolution
- ✅ **CLI Tool** - Parse and typecheck commands with beautiful error reporting
- ✅ **Example Programs** - Working code demonstrating features
- 🔄 **Rust Compiler** - In development using Cranelift backend
- 📋 **Language Server** - IDE support planned
- 📋 **Package Manager** - Built-in dependency management planned

## Quick Start

```bash
# Clone the repository
git clone https://harton.dev/outrun/outrun.git
cd outrun

# Build the parser
cd outrun-parser
cargo build

# Run tests
cargo test
```

### Hello, World!

```outrun
# hello.outrun
struct HelloApp() {}

impl Application for HelloApp {
    def start(args: List<String>): Result<(), ApplicationError> {
        IO.puts("Hello, Outrun! 🌅")
        Ok(())
    }
}
```

## Philosophy

> "The best way to predict the future is to invent it." - Alan Kay

Outrun embraces the idea that programming languages should be:

- **Predictable** - The same code should always do the same thing
- **Composable** - Small pieces should combine into larger systems
- **Readable** - Code should tell a story that humans can understand
- **Safe** - The compiler should catch your mistakes
- **Fast** - Performance should never be an afterthought

## Inspiration

Outrun draws inspiration from:

- **Rust** - Trait system and memory safety
- **Elixir** - Actor model and functional programming
- **Haskell** - Type system and purity
- **Swift** - Protocols and modern syntax design
- **Synthwave** - Aesthetic and retro-futuristic vibes 🌅

---

*Built with 💜 for the future of programming*