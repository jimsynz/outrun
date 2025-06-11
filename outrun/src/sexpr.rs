// S-expression formatter for Outrun AST
// Converts complex Rust AST structures into readable Lisp-like syntax

use outrun_parser::*;

pub fn format_program_as_sexpr(program: &Program) -> String {
    format_program_with_indent(program, 0)
}

fn format_program_with_indent(program: &Program, indent: usize) -> String {
    let items: Vec<String> = program.items
        .iter()
        .map(|item| format_item_with_indent(item, indent + 2))
        .collect();
    
    if items.is_empty() {
        "(program)".to_string()
    } else if items.len() == 1 {
        format!("(program {})", items[0])
    } else {
        format!(
            "(program\n{}\n{})",
            items.join(&format!("\n{}", " ".repeat(indent + 2))),
            " ".repeat(indent)
        )
    }
}

fn format_item_with_indent(item: &Item, indent: usize) -> String {
    match &item.kind {
        ItemKind::LetBinding(let_binding) => format_let_binding_with_indent(let_binding, indent),
        ItemKind::Expression(expr) => format_expression_with_indent(expr, indent),
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