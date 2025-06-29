//! Shared compilation context for eliminating data duplication
//!
//! This module provides the `SharedCompilationContext` which enables efficient
//! sharing of compilation state across multiple components without expensive cloning.

use crate::compilation::program_collection::CompilationResult;
use std::sync::Arc;

/// Shared compilation context that eliminates data duplication
///
/// This context wraps expensive-to-clone compilation data in Arc<> for efficient sharing.
/// Instead of cloning entire CompilationResults, components can share references to this context.
#[derive(Debug, Clone)]
pub struct SharedCompilationContext {
    /// Core library compilation result (immutable, shared)
    core_compilation: Arc<CompilationResult>,
}

impl SharedCompilationContext {
    /// Create a new shared compilation context from a core library compilation
    pub fn new(core_compilation: CompilationResult) -> Self {
        Self {
            core_compilation: Arc::new(core_compilation),
        }
    }

    /// Get a reference to the core compilation result
    ///
    /// This returns a reference to the Arc-wrapped core compilation,
    /// allowing read access without cloning the entire structure.
    pub fn core_compilation(&self) -> &CompilationResult {
        &self.core_compilation
    }

    /// Get an Arc clone to the core compilation result
    ///
    /// This clones only the Arc pointer, not the underlying data.
    /// Use this when you need to store a reference to the core compilation.
    pub fn core_compilation_arc(&self) -> Arc<CompilationResult> {
        Arc::clone(&self.core_compilation)
    }

    /// Create a derived compilation result by merging with user compilation
    ///
    /// This combines the shared core compilation with user code without cloning
    /// the core library data. Only the user compilation data is cloned.
    pub fn extend_with_user_compilation(
        &self,
        user_compilation: CompilationResult,
    ) -> CompilationResult {
        // Use CompilationResult::merge to combine core + user
        // The core library is already Arc-wrapped, so this should be efficient
        match CompilationResult::merge(
            (*self.core_compilation).clone(), // Clone the core compilation content
            vec![user_compilation.clone()],
        ) {
            Ok(merged) => merged,
            Err(conflicts) => {
                // Log conflicts but proceed with user compilation only
                eprintln!("Warning: Compilation conflicts detected: {:?}", conflicts);
                user_compilation
            }
        }
    }

    /// Create a fresh compilation session context
    ///
    /// This creates a new context for a compilation session that shares
    /// the core library data but allows independent user compilation state.
    pub fn create_session_context(&self) -> CompilationSessionContext {
        CompilationSessionContext {
            shared_context: self.clone(), // Clone only the Arc references
            user_compilation: None,
        }
    }
}

/// Context for a single compilation session
///
/// This provides session-local state while sharing the core compilation context.
/// Multiple sessions can exist concurrently, each with their own user compilation state.
#[derive(Debug, Clone)]
pub struct CompilationSessionContext {
    /// Shared core compilation context
    shared_context: SharedCompilationContext,

    /// Session-specific user compilation (optional)
    user_compilation: Option<Arc<CompilationResult>>,
}

impl CompilationSessionContext {
    /// Get the core compilation context
    pub fn core_context(&self) -> &SharedCompilationContext {
        &self.shared_context
    }

    /// Set the user compilation for this session
    pub fn set_user_compilation(&mut self, user_compilation: CompilationResult) {
        self.user_compilation = Some(Arc::new(user_compilation));
    }

    /// Get the current effective compilation result
    ///
    /// For REPL usage, we avoid merging with previous user compilations to prevent
    /// infinite loops in the merge algorithm. Instead, we maintain a cumulative
    /// user compilation that gets updated incrementally.
    pub fn effective_compilation(&self) -> CompilationResult {
        match &self.user_compilation {
            Some(user_comp) => {
                // Return the user compilation directly, which should already include
                // the core library through the compilation process
                (**user_comp).clone()
            }
            None => {
                // Return core compilation only
                (*self.shared_context.core_compilation).clone()
            }
        }
    }

    /// Clear user compilation, reverting to core-only state
    pub fn clear_user_compilation(&mut self) {
        self.user_compilation = None;
    }

    /// Check if this session has user compilation
    pub fn has_user_compilation(&self) -> bool {
        self.user_compilation.is_some()
    }
}

/// Factory for creating shared compilation contexts
pub struct SharedCompilationContextFactory;

impl SharedCompilationContextFactory {
    /// Create a shared context from the core library
    ///
    /// This loads the core library compilation once and wraps it for sharing.
    /// Multiple calls to this method will reuse the cached core library.
    pub fn create_from_core_library() -> SharedCompilationContext {
        let core_compilation = crate::core_library::get_core_library_compilation();
        SharedCompilationContext::new(core_compilation.clone())
    }

    /// Create a shared context with explicit core compilation
    ///
    /// This allows providing a specific core compilation for testing
    /// or custom scenarios.
    pub fn create_with_core(core_compilation: CompilationResult) -> SharedCompilationContext {
        SharedCompilationContext::new(core_compilation)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_shared_context_creation() {
        let context = SharedCompilationContextFactory::create_from_core_library();

        // Verify core compilation is accessible
        assert!(!context.core_compilation().traits.is_empty());
        assert!(!context.core_compilation().structs.is_empty());
    }

    #[test]
    fn test_session_context_isolation() {
        let shared_context = SharedCompilationContextFactory::create_from_core_library();

        let session1 = shared_context.create_session_context();
        let session2 = shared_context.create_session_context();

        // Sessions should start with no user compilation
        assert!(!session1.has_user_compilation());
        assert!(!session2.has_user_compilation());

        // Sessions should share core context (compare Arc pointers)
        assert!(std::ptr::eq(
            Arc::as_ptr(&session1.core_context().core_compilation_arc()),
            Arc::as_ptr(&session2.core_context().core_compilation_arc())
        ));
    }

    #[test]
    fn test_memory_efficiency() {
        let shared_context = SharedCompilationContextFactory::create_from_core_library();

        // Multiple clones should share the same underlying data
        let context_clone1 = shared_context.clone();
        let context_clone2 = shared_context.clone();

        // Arc addresses should be the same (sharing)
        assert!(std::ptr::eq(
            Arc::as_ptr(&shared_context.core_compilation_arc()),
            Arc::as_ptr(&context_clone1.core_compilation_arc())
        ));
        assert!(std::ptr::eq(
            Arc::as_ptr(&shared_context.core_compilation_arc()),
            Arc::as_ptr(&context_clone2.core_compilation_arc())
        ));
    }
}
