//! Phase-specific visitors for multi-program compilation
//!
//! This module contains visitors that extract different types of definitions
//! during the phased compilation process. Each visitor focuses on a specific
//! type of AST node (traits, structs, impl blocks) for clean separation of concerns.

use crate::visitor::{Visitor, VisitorResult};
use outrun_parser::{ImplBlock, Item, ItemKind, StructDefinition, TraitDefinition};
use std::collections::HashMap;

/// Visitor for extracting trait definitions (Phase 1)
#[derive(Default)]
pub struct TraitExtractionVisitor {
    pub traits: HashMap<String, TraitDefinition>,
}

/// Visitor for extracting struct definitions (Phase 2)
#[derive(Default)]
pub struct StructExtractionVisitor {
    pub structs: HashMap<String, StructDefinition>,
}

/// Visitor for extracting impl blocks (Phase 3)
#[derive(Default)]
pub struct ImplExtractionVisitor {
    pub implementations: Vec<ImplBlock>,
}

impl<T> Visitor<T> for TraitExtractionVisitor {
    fn visit_item(&mut self, item: &Item) -> VisitorResult {
        match &item.kind {
            ItemKind::TraitDefinition(trait_def) => {
                let trait_name = trait_def.name_as_string();
                self.traits.insert(trait_name, trait_def.clone());
                Ok(())
            }
            _ => Ok(()), // Ignore non-trait items in this phase
        }
    }
}

impl<T> Visitor<T> for StructExtractionVisitor {
    fn visit_item(&mut self, item: &Item) -> VisitorResult {
        match &item.kind {
            ItemKind::StructDefinition(struct_def) => {
                let struct_name = struct_def.name_as_string();
                self.structs.insert(struct_name, struct_def.clone());
                Ok(())
            }
            _ => Ok(()), // Ignore non-struct items in this phase
        }
    }
}

impl<T> Visitor<T> for ImplExtractionVisitor {
    fn visit_item(&mut self, item: &Item) -> VisitorResult {
        match &item.kind {
            ItemKind::ImplBlock(impl_block) => {
                self.implementations.push(impl_block.clone());
                Ok(())
            }
            _ => Ok(()), // Ignore non-impl items in this phase
        }
    }
}
