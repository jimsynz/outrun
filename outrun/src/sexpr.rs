// S-expression formatter for Outrun AST
// Converts complex Rust AST structures into readable Lisp-like syntax

use outrun_parser::*;

pub fn format_program_as_sexpr(program: &Program) -> String {
    format_program_with_indent(program, 0)
}

fn format_program_with_indent(program: &Program, indent: usize) -> String {
    // Interleave comments with items based on spans for proper source order
    let mixed_items = interleave_comments_with_items(&program.items, &program.debug_info.comments);
    
    let formatted_items: Vec<String> = mixed_items
        .iter()
        .map(|item| format_mixed_item_with_indent(item, indent + 2))
        .collect();
    
    if formatted_items.is_empty() {
        "(program)".to_string()
    } else if formatted_items.len() == 1 {
        format!("(program {})", formatted_items[0])
    } else {
        format!(
            "(program\n{}\n{})",
            formatted_items.join(&format!("\n{}", " ".repeat(indent + 2))),
            " ".repeat(indent)
        )
    }
}

// Mixed item type to represent either an AST item or a comment in source order
#[derive(Debug)]
enum MixedItem<'a> {
    Item(&'a Item),
    Comment(&'a Comment),
}

impl<'a> MixedItem<'a> {
    fn span(&self) -> &Span {
        match self {
            MixedItem::Item(item) => &item.span,
            MixedItem::Comment(comment) => &comment.span,
        }
    }
}

// Interleave comments with items based on their spans to maintain source order
fn interleave_comments_with_items<'a>(
    items: &'a [Item], 
    comments: &'a [Comment]
) -> Vec<MixedItem<'a>> {
    let mut mixed_items = Vec::new();
    
    // Add all items
    for item in items {
        mixed_items.push(MixedItem::Item(item));
    }
    
    // Add all comments
    for comment in comments {
        mixed_items.push(MixedItem::Comment(comment));
    }
    
    // Sort by span start position to maintain source order
    mixed_items.sort_by_key(|item| item.span().start);
    
    mixed_items
}

// Format either an item or a comment
fn format_mixed_item_with_indent(mixed_item: &MixedItem, indent: usize) -> String {
    match mixed_item {
        MixedItem::Item(item) => format_item_with_indent(item, indent),
        MixedItem::Comment(comment) => format_comment_with_indent(comment, indent),
    }
}

fn format_item_with_indent(item: &Item, indent: usize) -> String {
    match &item.kind {
        ItemKind::LetBinding(let_binding) => format_let_binding_with_indent(let_binding, indent),
        ItemKind::ConstDefinition(const_def) => format_const_definition_with_indent(const_def, indent),
        ItemKind::StructDefinition(struct_def) => format_struct_definition_with_indent(struct_def, indent),
        ItemKind::TraitDefinition(trait_def) => format_trait_definition_with_indent(trait_def, indent),
        ItemKind::ImplBlock(impl_block) => format_impl_block_with_indent(impl_block, indent),
        ItemKind::FunctionDefinition(func_def) => format_function_definition_with_indent(func_def, indent),
        ItemKind::Expression(expr) => format_expression_with_indent(expr, indent),
        ItemKind::Comment(comment) => format_comment_with_indent(comment, indent),
        ItemKind::Newline => "(newline)".to_string(),
        _ => "(unknown-item)".to_string(),
    }
}

fn format_let_binding_with_indent(let_binding: &LetBinding, indent: usize) -> String {
    let pattern = format_pattern_with_indent(&let_binding.pattern, indent + 2);
    let type_annotation = match &let_binding.type_annotation {
        Some(type_ann) => format!("\n{}(type {})", " ".repeat(indent + 2), format_type_with_indent(type_ann, indent + 4)),
        None => String::new(),
    };
    let expression = format_expression_with_indent(&let_binding.expression, indent + 2);
    
    if type_annotation.is_empty() {
        format!("(let {} {})", pattern, expression)
    } else {
        format!(
            "(let {}{}\n{}{})",
            pattern,
            type_annotation,
            " ".repeat(indent + 2),
            expression
        )
    }
}

fn format_const_definition_with_indent(const_def: &ConstDefinition, indent: usize) -> String {
    let name = &const_def.name.name;
    let type_annotation = format_type_with_indent(&const_def.type_annotation, indent + 2);
    let expression = format_expression_with_indent(&const_def.expression, indent + 2);
    
    format!(
        "(const {}\n{}(type {})\n{}{})",
        name,
        " ".repeat(indent + 2),
        type_annotation,
        " ".repeat(indent + 2),
        expression
    )
}

fn format_comment_with_indent(comment: &Comment, _indent: usize) -> String {
    match comment.kind {
        CommentKind::Line => format!("(comment \"{}\")", escape_string_content(&comment.content)),
        CommentKind::Block => format!("(block-comment \"{}\")", escape_string_content(&comment.content)),
    }
}

fn escape_string_content(content: &str) -> String {
    content
        .replace("\\", "\\\\")
        .replace("\"", "\\\"")
        .replace("\n", "\\n")
        .replace("\t", "\\t")
}

fn format_pattern_with_indent(pattern: &Pattern, _indent: usize) -> String {
    match pattern {
        Pattern::Identifier(id) => id.name.clone(),
        _ => "(unknown-pattern)".to_string(),
    }
}

fn format_type_with_indent(type_ann: &TypeAnnotation, _indent: usize) -> String {
    match type_ann {
        TypeAnnotation::Simple { path, .. } => {
            let path_names: Vec<String> = path.iter().map(|t| t.name.clone()).collect();
            path_names.join(".")
        }
        TypeAnnotation::Function { .. } => "(function-type)".to_string(),
        _ => "(unknown-type)".to_string(),
    }
}

fn format_expression_with_indent(expr: &Expression, indent: usize) -> String {
    match &expr.kind {
        ExpressionKind::Boolean(bool_lit) => format!("(boolean {})", bool_lit.value),
        ExpressionKind::Integer(int_lit) => format!("(integer {})", int_lit.value),
        ExpressionKind::Float(float_lit) => format!("(float {})", float_lit.value),
        ExpressionKind::String(str_lit) => format_string_with_indent(str_lit, indent),
        ExpressionKind::Atom(atom_lit) => format!("(atom :{})", atom_lit.name),
        ExpressionKind::Identifier(id) => id.name.clone(),
        ExpressionKind::BinaryOp(bin_op) => format_binary_op_with_indent(bin_op, indent),
        ExpressionKind::FunctionCall(call) => format_function_call_with_indent(call, indent),
        _ => "(unknown-expr)".to_string(),
    }
}

fn format_string_with_indent(str_lit: &StringLiteral, indent: usize) -> String {
    let content: Vec<String> = str_lit.parts.iter().map(|part| {
        match part {
            StringPart::Text { content, .. } => format!("\"{}\"", content.replace("\"", "\\\"")),
            StringPart::Interpolation { expression, .. } => 
                format!("(interpolation {})", format_expression_with_indent(expression, indent + 2)),
        }
    }).collect();
    
    if content.len() == 1 {
        content[0].clone()
    } else if content.len() == 2 {
        format!("(string-interpolation {} {})", content[0], content[1])
    } else {
        format!(
            "(string-interpolation\n{}\n{})",
            content.join(&format!("\n{}", " ".repeat(indent + 2))),
            " ".repeat(indent)
        )
    }
}

fn format_binary_op_with_indent(bin_op: &BinaryOperation, indent: usize) -> String {
    let left = format_expression_with_indent(&bin_op.left, indent + 2);
    let right = format_expression_with_indent(&bin_op.right, indent + 2);
    let op = format_binary_operator_as_sexpr(&bin_op.operator);
    
    // Simple expressions on one line
    if !left.contains('\n') && !right.contains('\n') && (left.len() + right.len() + op.len()) < 50 {
        format!("({} {} {})", op, left, right)
    } else {
        // Complex expressions with indentation
        format!(
            "({}\n{}{}\n{}{})",
            op,
            " ".repeat(indent + 2), left,
            " ".repeat(indent + 2), right
        )
    }
}

fn format_binary_operator_as_sexpr(op: &BinaryOperator) -> String {
    match op {
        BinaryOperator::Add => "+",
        BinaryOperator::Subtract => "-",
        BinaryOperator::Multiply => "*",
        BinaryOperator::Divide => "/",
        BinaryOperator::Modulo => "%",
        BinaryOperator::Exponent => "**",
        BinaryOperator::Equal => "==",
        BinaryOperator::NotEqual => "!=",
        BinaryOperator::Less => "<",
        BinaryOperator::LessEqual => "<=",
        BinaryOperator::Greater => ">",
        BinaryOperator::GreaterEqual => ">=",
        BinaryOperator::LogicalAnd => "&&",
        BinaryOperator::LogicalOr => "||",
        BinaryOperator::BitwiseAnd => "&",
        BinaryOperator::BitwiseOr => "|",
        BinaryOperator::BitwiseXor => "^",
        BinaryOperator::ShiftLeft => "<<",
        BinaryOperator::ShiftRight => ">>",
        BinaryOperator::PipeMaybe => "|?",
        BinaryOperator::Pipe => "|>",
    }.to_string()
}

fn format_function_call_with_indent(call: &FunctionCall, indent: usize) -> String {
    let function_name = format_function_path(&call.path);
    
    if call.arguments.is_empty() {
        format!("(call {})", function_name)
    } else {
        let args: Vec<String> = call.arguments.iter()
            .map(|arg| format!("({}: {})", arg.name.name, format_expression_with_indent(&arg.expression, indent + 2)))
            .collect();
        
        if args.len() == 1 && args[0].len() < 50 {
            format!("(call {} {})", function_name, args[0])
        } else {
            format!(
                "(call {}\n{}\n{})",
                function_name,
                args.join(&format!("\n{}", " ".repeat(indent + 2))),
                " ".repeat(indent)
            )
        }
    }
}

fn format_function_path(path: &FunctionPath) -> String {
    match path {
        FunctionPath::Simple { name } => name.name.clone(),
        FunctionPath::Qualified { module, name } => format!("{}.{}", module.name, name.name),
        FunctionPath::Expression { expression } => format!("({})", format_expression_with_indent(expression, 0)),
    }
}

fn format_struct_definition_with_indent(struct_def: &StructDefinition, indent: usize) -> String {
    let name = &struct_def.name.name;
    let methods_count = struct_def.methods.len();
    
    if methods_count == 0 {
        format!("(struct {})", name)
    } else {
        format!(
            "(struct {}\n{}(methods {}))",
            name,
            " ".repeat(indent + 2),
            methods_count
        )
    }
}

fn format_trait_definition_with_indent(trait_def: &TraitDefinition, indent: usize) -> String {
    let name = &trait_def.name.name;
    let functions_count = trait_def.functions.len();
    
    if functions_count == 0 {
        format!("(trait {})", name)
    } else {
        format!(
            "(trait {}\n{}(functions {}))",
            name,
            " ".repeat(indent + 2),
            functions_count
        )
    }
}

fn format_impl_block_with_indent(impl_block: &ImplBlock, indent: usize) -> String {
    let trait_name = format_type_path(&impl_block.trait_spec);
    let type_name = format_type_path(&impl_block.type_spec);
    let methods_count = impl_block.methods.len();
    
    format!(
        "(impl {} for {}\n{}(methods {}))",
        trait_name,
        type_name,
        " ".repeat(indent + 2),
        methods_count
    )
}

fn format_function_definition_with_indent(func_def: &FunctionDefinition, indent: usize) -> String {
    let name = &func_def.name.name;
    let param_count = func_def.parameters.len();
    
    if func_def.body.statements.is_empty() {
        format!(
            "(function {} (params {}))",
            name,
            param_count
        )
    } else {
        let body_statements: Vec<String> = func_def.body.statements.iter()
            .map(|stmt| format_statement_with_indent(stmt, indent + 4))
            .collect();
        
        format!(
            "(function {} (params {})\n{}(body\n{}\n{}))",
            name,
            param_count,
            " ".repeat(indent + 2),
            body_statements.join(&format!("\n{}", " ".repeat(indent + 4))),
            " ".repeat(indent + 2)
        )
    }
}

fn format_statement_with_indent(stmt: &Statement, indent: usize) -> String {
    match &stmt.kind {
        StatementKind::LetBinding(let_binding) => format_let_binding_with_indent(let_binding, indent),
        StatementKind::Expression(expr) => format_expression_with_indent(expr, indent),
    }
}

fn format_type_path(type_spec: &TypeSpec) -> String {
    let path_names: Vec<String> = type_spec.path.iter().map(|t| t.name.clone()).collect();
    path_names.join(".")
}