//! Phase-specific visitors for multi-program compilation
//!
//! This module contains visitors that extract different types of definitions
//! during the phased compilation process. Each visitor focuses on a specific
//! type of AST node (protocols, structs, impl blocks) for clean separation of concerns.

use crate::visitor::{Visitor, VisitorResult};
use outrun_parser::{ImplBlock, Item, ItemKind, ProtocolDefinition, StructDefinition};
use std::collections::HashMap;

/// Visitor for extracting protocol definitions (Phase 1)
#[derive(Default)]
pub struct ProtocolExtractionVisitor {
    pub protocols: HashMap<String, ProtocolDefinition>,
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

impl<T> Visitor<T> for ProtocolExtractionVisitor {
    fn visit_item(&mut self, item: &Item) -> VisitorResult {
        match &item.kind {
            ItemKind::ProtocolDefinition(protocol_def) => {
                let protocol_name = protocol_def.name_as_string();
                self.protocols.insert(protocol_name, protocol_def.clone());
                Ok(())
            }
            _ => Ok(()), // Ignore non-protocol items in this phase
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
