# Unified Module System Implementation Plan

## Overview

This plan outlines the complete refactor of Outrun's type system to use a unified module registry that matches the language's "everything is a module" philosophy. This will replace the current dual-registry system (TypeRegistry + ProtocolRegistry) with a single, unified namespace.

## Current Problems

1. **Dual Registry Complexity**: Separate `ProtocolRegistry` and `concrete_types` HashMap creates coordination bugs
2. **Artificial Type Separation**: `TypeId`/`ProtocolId`/`ModuleId` are identical `String` wrappers in different namespaces
3. **Missing Implementation Modules**: Protocol implementations aren't first-class modules
4. **Lookup Bugs**: Current `Map` protocol lookup failure due to registry coordination issues

## Target Architecture

### Unified ModuleName Type
```rust
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleName(String);
```

### Complete TypeModule Enum
```rust
#[derive(Debug, Clone, PartialEq)]
pub enum TypeModule {
    Protocol {
        name: ModuleName,
        definition: ProtocolDefinition,
        source_location: Span,
        generic_arity: usize,
    },
    Struct {
        name: ModuleName,
        definition: ConcreteTypeDefinition, 
        source_location: Span,
        generic_arity: usize,
    },
    Implementation {
        name: ModuleName,                    // "List:Display"
        implementing_type: ModuleName,       // "List"
        protocol: ModuleName,                // "Display"
        generic_bindings: Vec<Type>,         // T -> String for this impl
        functions: Vec<FunctionDefinition>,  // The actual impl functions
        source_location: Span,
        defining_module: ModuleName,         // Where this impl lives
    },
    ForwardBinding {
        name: ModuleName,
        expected_arity: Option<usize>,
        source_location: Span,
        references: Vec<Span>,
    },
}
```

### Unified TypeRegistry
```rust
#[derive(Debug, Clone)]
pub struct TypeRegistry {
    modules: HashMap<ModuleName, TypeModule>,
    // No separate registries - everything is unified
}
```

## Implementation Phases

### Phase 1: Core Unification & Implementation (3-4 days)

#### 1.1 Replace TypeId/ProtocolId/ModuleId with ModuleName

**DELETE these entirely:**
- `pub struct TypeId { name: String }`
- `pub struct ProtocolId { name: String }`  
- `pub struct ModuleId { name: String }`

**REPLACE with unified:**
```rust
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleName(String);

impl ModuleName {
    pub fn new(name: impl Into<String>) -> Self {
        Self(name.into())
    }
    
    pub fn as_str(&self) -> &str {
        &self.0
    }
}
```

#### 1.2 Design Complete TypeModule with Implementations

Add Implementation modules as first-class citizens with colon naming:
- `impl List<T>: Display` → module name: `"List:Display"`
- `impl Map<K,V>: Iterator` → module name: `"Map:Iterator"`  
- `impl String: Debug` → module name: `"String:Debug"`

#### 1.3 Unified TypeRegistry

**DELETE entirely:**
- `ProtocolRegistry` struct and all implementations (~400 lines)
- `ImplementationRegistry` type alias
- `concrete_types` HashMap from TypeRegistry
- `protocol_registry` field from TypeRegistry

**REPLACE with:**
```rust
impl TypeRegistry {
    // Unified API
    pub fn get_module(&self, name: &str) -> Option<&TypeModule>
    pub fn register_module(&mut self, module: TypeModule) -> Result<(), TypeError>
    pub fn is_protocol(&self, name: &str) -> bool
    pub fn is_struct(&self, name: &str) -> bool
    pub fn get_implementation(&self, impl_type: &str, protocol: &str) -> Option<&TypeModule>
}
```

#### 1.4 Slash & Burn Immediately

**Files Modified:**
- `src/types.rs` - Remove old ID types, add ModuleName
- `src/registry.rs` - Complete rewrite with unified system
- `src/lib.rs` - Update exports
- `src/error.rs` - Add new error types

### Phase 2: Core Systems Migration (2-3 days)

#### 2.1 Update All Type References

**REPLACE throughout codebase:**
- Every `TypeId::new("Foo")` → `ModuleName::new("Foo")`
- Every `ProtocolId::new("Bar")` → `ModuleName::new("Bar")`
- Every `ModuleId::new("Baz")` → `ModuleName::new("Baz")`
- Update all function signatures

#### 2.2 Rewrite Compilation Phases

**REWRITE** `register_protocols_and_structs()`:
- Replace dual-registry coordination with single module registration
- Update type resolution to use unified lookup
- **DELETE** all registry coordination code

#### 2.3 Rewrite Dispatch System

**REPLACE** in dispatch.rs:
- Protocol registry lookups → module lookups
- Separate implementation tracking → unified module system
- **DELETE** ImplementationKey system

**Files Modified:**
- `src/inference.rs` - Major rewrite of compilation phases
- `src/dispatch.rs` - Rewrite for unified modules
- `src/constraints.rs` - Update type resolution
- `src/unification.rs` - Update type references

### Phase 3: Advanced Features & Test Migration (2-3 days)

#### 3.1 Implementation Module Support

- Add implementation registration during compilation
- Implement implementation lookup by type + protocol
- **DELETE** old ImplementationKey system

#### 3.2 Forward Bindings & Arity Detection

- Implement forward binding registration during parsing
- Add generic arity conflict detection during registration
- **DELETE** manual arity checking workarounds

#### 3.3 Test Migration

**Priority order:**
1. **FIX** `test_unified_type_registry.rs` immediately (should fix our bug)
2. **FIX** `test_map_protocol_classification.rs` (our current failing test)
3. **REWRITE** all remaining tests to use ModuleName and unified API
4. **DELETE** redundant test utilities
5. **CONSOLIDATE** similar test patterns

**Files Modified:**
- All 32+ test files in `src/tests/`
- **DELETE** old test utilities
- **CONSOLIDATE** test helpers

## Major Deletions

### Complete Structs/Enums to Delete
```rust
// From src/types.rs - DELETE entirely
pub struct TypeId { ... }
pub struct ProtocolId { ... }  
pub struct ModuleId { ... }

// From src/registry.rs - DELETE entirely
pub struct ProtocolRegistry { ... }
impl ProtocolRegistry { ... }  // ~400 lines
pub type ImplementationRegistry = ProtocolRegistry;

// From TypeRegistry - DELETE these fields
protocol_registry: ProtocolRegistry,
concrete_types: HashMap<String, ConcreteTypeDefinition>,
```

### Methods to Delete
```rust
// DELETE from TypeRegistry
pub fn protocol_registry_mut(&mut self) -> &mut ProtocolRegistry
pub fn protocol_registry(&self) -> &ProtocolRegistry
pub fn register_concrete_type(...)
pub fn is_concrete_type(...)

// DELETE from everywhere - REPLACE with unified API
.has_protocol(&protocol_id)              // → .is_protocol("name")
.get_protocol_definition(&protocol_id)   // → .get_protocol("name")
.register_concrete_type(type_id, ...)    // → .register_module(TypeModule::Struct { ... })
```

## Implementation Module Naming Strategy

```rust
// Implementation modules use colon syntax
impl List<T>: Display     → module name: "List:Display"
impl Map<K,V>: Iterator   → module name: "Map:Iterator"  
impl String: Debug        → module name: "String:Debug"

// Registry operations
registry.register_implementation("List", "Display", impl_info);
registry.get_implementation("List", "Display");
```

## Benefits of Complete Unification

1. **Single Namespace** - Matches Outrun's language model exactly
2. **Unified Dispatch** - All function calls go through same module lookup
3. **Simpler API** - No more wrapper type conversions or registry coordination
4. **Implementation Modules** - First-class support for `impl` blocks as modules
5. **Bug Fix** - Should resolve our `Map` lookup issue immediately
6. **Forward Bindings** - Support for types referenced before definition
7. **Arity Conflicts** - Detect `List` vs `List<T>` vs `List<T,V>` conflicts

## Risk Mitigation

### High Risk: Breaking Everything at Once
- **Mitigation**: Work in small, testable increments within each phase
- **Strategy**: Fix compilation errors immediately, don't let them accumulate

### Medium Risk: Test Failures
- **Mitigation**: Fix tests as soon as they break
- **Strategy**: Run `cargo test` frequently, fix failures before moving on

## Success Criteria

- **Phase 1**: New TypeRegistry compiles, basic tests pass, old code deleted
- **Phase 2**: Core compilation works, dispatch works, critical tests pass  
- **Phase 3**: All features implemented, all tests pass, no old code remains

## Estimated Timeline: 7-10 days total

## Progress Tracking

- [ ] **Phase 1.1**: Replace TypeId/ProtocolId/ModuleId with ModuleName
- [ ] **Phase 1.2**: Design complete TypeModule with Implementations  
- [ ] **Phase 1.3**: Implement unified TypeRegistry
- [ ] **Phase 1.4**: Delete old code and update exports
- [ ] **Phase 2.1**: Update all type references throughout codebase
- [ ] **Phase 2.2**: Rewrite compilation phases
- [ ] **Phase 2.3**: Rewrite dispatch system
- [ ] **Phase 3.1**: Add implementation module support
- [ ] **Phase 3.2**: Add forward bindings & arity detection
- [ ] **Phase 3.3**: Migrate all tests and cleanup

---

*This plan represents a complete architectural overhaul of Outrun's type system to match the language's unified module philosophy. The aggressive "slash and burn" approach ensures we maintain a clean codebase throughout the migration.*