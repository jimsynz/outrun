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
    comments: &'a [Comment],
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
        ItemKind::ConstDefinition(const_def) => {
            format_const_definition_with_indent(const_def, indent)
        }
        ItemKind::StructDefinition(struct_def) => {
            format_struct_definition_with_indent(struct_def, indent)
        }
        ItemKind::TraitDefinition(trait_def) => {
            format_trait_definition_with_indent(trait_def, indent)
        }
        ItemKind::ImplBlock(impl_block) => format_impl_block_with_indent(impl_block, indent),
        ItemKind::FunctionDefinition(func_def) => {
            format_function_definition_with_indent(func_def, indent)
        }
        ItemKind::Expression(expr) => format_expression_with_indent(expr, indent),
        ItemKind::Comment(comment) => format_comment_with_indent(comment, indent),
        _ => "(unknown-item)".to_string(),
    }
}

fn format_let_binding_with_indent(let_binding: &LetBinding, indent: usize) -> String {
    let pattern = format_pattern_with_indent(&let_binding.pattern, indent + 2);
    let type_annotation = match &let_binding.type_annotation {
        Some(type_ann) => format!(
            "\n{}(type {})",
            " ".repeat(indent + 2),
            format_type_with_indent(type_ann, indent + 4)
        ),
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
        CommentKind::Block => format!(
            "(block-comment \"{}\")",
            escape_string_content(&comment.content)
        ),
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
        ExpressionKind::Sigil(sigil_lit) => format!("(sigil ~{})", sigil_lit.sigil_type.name),
        ExpressionKind::List(list_lit) => format_list_with_indent(list_lit, indent),
        ExpressionKind::Map(map_lit) => format_map_with_indent(map_lit, indent),
        ExpressionKind::Tuple(tuple_lit) => format_tuple_with_indent(tuple_lit, indent),
        ExpressionKind::Struct(struct_lit) => format_struct_literal_with_indent(struct_lit, indent),
        ExpressionKind::Identifier(id) => id.name.clone(),
        ExpressionKind::TypeIdentifier(type_id) => type_id.name.clone(),
        ExpressionKind::QualifiedIdentifier(qual_id) => {
            format!("{}.{}", qual_id.module.name, qual_id.name.name)
        }
        ExpressionKind::BinaryOp(bin_op) => format_binary_op_with_indent(bin_op, indent),
        ExpressionKind::UnaryOp(unary_op) => format_unary_op_with_indent(unary_op, indent),
        ExpressionKind::FieldAccess(field_access) => {
            format_field_access_with_indent(field_access, indent)
        }
        ExpressionKind::FunctionCall(call) => format_function_call_with_indent(call, indent),
        ExpressionKind::IfExpression(if_expr) => format_if_expression_with_indent(if_expr, indent),
        ExpressionKind::CaseExpression(case_expr) => {
            format_case_expression_with_indent(case_expr, indent)
        }
        ExpressionKind::MacroInjection(macro_inj) => {
            format!("(macro-injection ^{})", macro_inj.parameter.name)
        }
        ExpressionKind::AnonymousFunction(anon_fn) => {
            format_anonymous_function_with_indent(anon_fn, indent)
        }
        ExpressionKind::FunctionCapture(fn_capture) => {
            format_function_capture_with_indent(fn_capture, indent)
        }
        ExpressionKind::Parenthesized(expr) => {
            format!("({})", format_expression_with_indent(expr, indent))
        }
    }
}

fn format_string_with_indent(str_lit: &StringLiteral, indent: usize) -> String {
    let content: Vec<String> = str_lit
        .parts
        .iter()
        .map(|part| match part {
            StringPart::Text { content, .. } => format!("\"{}\"", content.replace("\"", "\\\"")),
            StringPart::Interpolation { expression, .. } => format!(
                "(interpolation {})",
                format_expression_with_indent(expression, indent + 2)
            ),
        })
        .collect();

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
            " ".repeat(indent + 2),
            left,
            " ".repeat(indent + 2),
            right
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
    }
    .to_string()
}

fn format_function_call_with_indent(call: &FunctionCall, indent: usize) -> String {
    let function_name = format_function_path(&call.path);

    if call.arguments.is_empty() {
        format!("(call {})", function_name)
    } else {
        let args: Vec<String> = call
            .arguments
            .iter()
            .map(|arg| match arg {
                Argument::Named {
                    name, expression, ..
                } => {
                    format!(
                        "({}: {})",
                        name.name,
                        format_expression_with_indent(expression, indent + 2)
                    )
                }
                Argument::Spread {
                    expression, kind, ..
                } => {
                    let spread_type = match kind {
                        SpreadKind::Strict => "spread",
                        SpreadKind::Lenient => "spread?",
                    };
                    format!(
                        "({} {})",
                        spread_type,
                        format_expression_with_indent(expression, indent + 2)
                    )
                }
            })
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
        FunctionPath::Expression { expression } => {
            format!("({})", format_expression_with_indent(expression, 0))
        }
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
        // Count different types of functions
        let mut signatures = 0;
        let mut definitions = 0;
        let mut static_definitions = 0;

        for function in &trait_def.functions {
            match function {
                TraitFunction::Signature(_) => signatures += 1,
                TraitFunction::Definition(_) => definitions += 1,
                TraitFunction::StaticDefinition(_) => static_definitions += 1,
            }
        }

        let mut function_details = Vec::new();
        if signatures > 0 {
            function_details.push(format!("signatures {}", signatures));
        }
        if definitions > 0 {
            function_details.push(format!("definitions {}", definitions));
        }
        if static_definitions > 0 {
            function_details.push(format!("static {}", static_definitions));
        }

        format!(
            "(trait {}\n{}(functions {} [{}]))",
            name,
            " ".repeat(indent + 2),
            functions_count,
            function_details.join(", ")
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
        format!("(function {} (params {}))", name, param_count)
    } else {
        let body_statements: Vec<String> = func_def
            .body
            .statements
            .iter()
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
        StatementKind::LetBinding(let_binding) => {
            format_let_binding_with_indent(let_binding, indent)
        }
        StatementKind::Expression(expr) => format_expression_with_indent(expr, indent),
    }
}

fn format_type_path(type_spec: &TypeSpec) -> String {
    let path_names: Vec<String> = type_spec.path.iter().map(|t| t.name.clone()).collect();
    path_names.join(".")
}

// Additional formatter functions for missing expression types

fn format_list_with_indent(list_lit: &ListLiteral, indent: usize) -> String {
    if list_lit.elements.is_empty() {
        "[]".to_string()
    } else {
        let elements: Vec<String> = list_lit
            .elements
            .iter()
            .map(|element| match element {
                ListElement::Expression(expr) => format_expression_with_indent(expr, indent + 2),
                ListElement::Spread(id) => format!("..{}", id.name),
            })
            .collect();

        if elements.len() == 1 && elements[0].len() < 50 {
            format!("[{}]", elements[0])
        } else {
            format!(
                "[\n{}\n{}]",
                elements.join(&format!("\n{}", " ".repeat(indent + 2))),
                " ".repeat(indent)
            )
        }
    }
}

fn format_map_with_indent(map_lit: &MapLiteral, indent: usize) -> String {
    if map_lit.entries.is_empty() {
        "{}".to_string()
    } else {
        let pairs: Vec<String> = map_lit
            .entries
            .iter()
            .map(|entry| match entry {
                MapEntry::Assignment { key, value } => format!(
                    "{} => {}",
                    format_expression_with_indent(key, indent + 2),
                    format_expression_with_indent(value, indent + 2)
                ),
                MapEntry::Shorthand { name, value } => format!(
                    "{}: {}",
                    name.name,
                    format_expression_with_indent(value, indent + 2)
                ),
                MapEntry::Spread(name) => format!("..{}", name.name),
            })
            .collect();

        if pairs.len() == 1 && pairs[0].len() < 40 {
            format!("{{{}}}", pairs[0])
        } else {
            format!(
                "{{\n{}\n{}}}",
                pairs.join(&format!("\n{}", " ".repeat(indent + 2))),
                " ".repeat(indent)
            )
        }
    }
}

fn format_tuple_with_indent(tuple_lit: &TupleLiteral, indent: usize) -> String {
    if tuple_lit.elements.is_empty() {
        "()".to_string()
    } else {
        let elements: Vec<String> = tuple_lit
            .elements
            .iter()
            .map(|expr| format_expression_with_indent(expr, indent + 2))
            .collect();

        if elements.len() == 1 {
            format!("({})", elements[0])
        } else if elements.iter().all(|e| e.len() < 20) && elements.len() <= 3 {
            format!("({})", elements.join(", "))
        } else {
            format!(
                "(\n{}\n{})",
                elements.join(&format!("\n{}", " ".repeat(indent + 2))),
                " ".repeat(indent)
            )
        }
    }
}

fn format_struct_literal_with_indent(struct_lit: &StructLiteral, indent: usize) -> String {
    let type_name = struct_lit
        .type_path
        .iter()
        .map(|t| t.name.as_str())
        .collect::<Vec<_>>()
        .join(".");

    if struct_lit.fields.is_empty() {
        format!("{} {{}}", type_name)
    } else {
        let fields: Vec<String> = struct_lit
            .fields
            .iter()
            .map(|field| match field {
                StructLiteralField::Shorthand(name) => name.name.clone(),
                StructLiteralField::Assignment { name, value } => format!(
                    "{}: {}",
                    name.name,
                    format_expression_with_indent(value, indent + 2)
                ),
                StructLiteralField::Spread(name) => format!("..{}", name.name),
            })
            .collect();

        format!(
            "{} {{\n{}\n{}}}",
            type_name,
            fields.join(&format!("\n{}", " ".repeat(indent + 2))),
            " ".repeat(indent)
        )
    }
}

fn format_unary_op_with_indent(unary_op: &UnaryOperation, _indent: usize) -> String {
    let op = match unary_op.operator {
        UnaryOperator::Plus => "+",
        UnaryOperator::Minus => "-",
        UnaryOperator::LogicalNot => "!",
        UnaryOperator::BitwiseNot => "~",
    };
    let operand = format_expression_with_indent(&unary_op.operand, 0);
    format!("({} {})", op, operand)
}

fn format_field_access_with_indent(field_access: &FieldAccess, indent: usize) -> String {
    let object = format_expression_with_indent(&field_access.object, indent);
    let field = &field_access.field.name;
    format!("(field-access {} {})", object, field)
}

fn format_if_expression_with_indent(if_expr: &IfExpression, indent: usize) -> String {
    let condition = format_expression_with_indent(&if_expr.condition, indent + 2);
    let then_block = format_block_with_indent(&if_expr.then_block, indent + 2);

    match &if_expr.else_block {
        Some(else_block) => {
            let else_formatted = format_block_with_indent(else_block, indent + 2);
            format!(
                "(if {}\n{}(then {})\n{}(else {}))",
                condition,
                " ".repeat(indent + 2),
                then_block,
                " ".repeat(indent + 2),
                else_formatted
            )
        }
        None => format!(
            "(if {}\n{}(then {}))",
            condition,
            " ".repeat(indent + 2),
            then_block
        ),
    }
}

fn format_case_expression_with_indent(case_expr: &CaseExpression, indent: usize) -> String {
    match case_expr {
        CaseExpression::Concrete(concrete) => {
            format_concrete_case_expression_with_indent(concrete, indent)
        }
        CaseExpression::Trait(trait_case) => {
            format_trait_case_expression_with_indent(trait_case, indent)
        }
    }
}

fn format_concrete_case_expression_with_indent(
    case_expr: &ConcreteCaseExpression,
    indent: usize,
) -> String {
    let expr = format_expression_with_indent(&case_expr.expression, indent + 2);
    let when_clauses: Vec<String> = case_expr
        .when_clauses
        .iter()
        .map(|clause| {
            format!(
                "(when {} -> {})",
                format_expression_with_indent(&clause.guard, indent + 4),
                format_case_result_with_indent(&clause.result, indent + 4)
            )
        })
        .collect();

    format!(
        "(case {}\n{})",
        expr,
        when_clauses.join(&format!("\n{}", " ".repeat(indent + 2)))
    )
}

fn format_trait_case_expression_with_indent(
    trait_case: &TraitCaseExpression,
    indent: usize,
) -> String {
    let expr = format_expression_with_indent(&trait_case.expression, indent + 2);
    let trait_name = &trait_case.trait_name.name;
    let type_clauses: Vec<String> = trait_case
        .type_clauses
        .iter()
        .map(|clause| {
            let mut clause_parts = vec![format!("(type {})", clause.type_name.name)];

            if let Some(pattern) = &clause.pattern {
                clause_parts.push(format!("(pattern {})", format_struct_pattern(pattern)));
            }

            if let Some(guard) = &clause.guard {
                clause_parts.push(format!(
                    "(guard {})",
                    format_expression_with_indent(guard, indent + 6)
                ));
            }

            clause_parts.push(format!(
                "(result {})",
                format_case_result_with_indent(&clause.result, indent + 6)
            ));

            format!("(trait-clause {})", clause_parts.join(" "))
        })
        .collect();

    format!(
        "(case-as {} (trait {})\n{})",
        expr,
        trait_name,
        type_clauses.join(&format!("\n{}", " ".repeat(indent + 2)))
    )
}

fn format_struct_pattern(pattern: &StructPattern) -> String {
    let fields: Vec<String> = pattern
        .fields
        .iter()
        .map(|field| match &field.pattern {
            Some(p) => format!("{}: {}", field.name.name, format_pattern(p)),
            None => field.name.name.clone(),
        })
        .collect();

    let type_name = pattern
        .type_path
        .iter()
        .map(|t| t.name.as_str())
        .collect::<Vec<_>>()
        .join(".");
    format!("{} {{ {} }}", type_name, fields.join(", "))
}

fn format_pattern(pattern: &Pattern) -> String {
    match pattern {
        Pattern::Identifier(id) => id.name.clone(),
        Pattern::Literal(lit) => format!("{:?}", lit.literal), // Basic formatting for now
        Pattern::Tuple(tuple) => {
            let elements: Vec<String> = tuple.elements.iter().map(format_pattern).collect();
            format!("({})", elements.join(", "))
        }
        Pattern::List(list) => {
            let mut elements: Vec<String> = list.elements.iter().map(format_pattern).collect();
            if let Some(rest) = &list.rest {
                elements.push(format!("..{}", rest.name));
            }
            format!("[{}]", elements.join(", "))
        }
        Pattern::Struct(struct_pat) => format_struct_pattern(struct_pat),
    }
}

fn format_case_result_with_indent(result: &CaseResult, indent: usize) -> String {
    match result {
        CaseResult::Block(block) => format_block_with_indent(block, indent),
        CaseResult::Expression(expr) => format_expression_with_indent(expr, indent),
    }
}

fn format_anonymous_function_with_indent(anon_fn: &AnonymousFunction, _indent: usize) -> String {
    let clauses: Vec<String> = anon_fn
        .clauses
        .iter()
        .map(|clause| format!("(clause {})", clause.parameters))
        .collect();

    format!("(fn {})", clauses.join(" "))
}

fn format_function_capture_with_indent(fn_capture: &FunctionCapture, _indent: usize) -> String {
    let module_path = fn_capture
        .module_path
        .as_ref()
        .map(|path| {
            path.iter()
                .map(|t| t.name.clone())
                .collect::<Vec<_>>()
                .join(".")
        })
        .unwrap_or_default();

    let function_name = if module_path.is_empty() {
        fn_capture.function_name.name.clone()
    } else {
        format!("{}.{}", module_path, fn_capture.function_name.name)
    };

    let arity = fn_capture
        .arity
        .map(|a| a.to_string())
        .unwrap_or("?".to_string());
    format!("(capture {} arity: {})", function_name, arity)
}

fn format_block_with_indent(block: &Block, indent: usize) -> String {
    if block.statements.is_empty() {
        "{}".to_string()
    } else {
        let statements: Vec<String> = block
            .statements
            .iter()
            .map(|stmt| format_statement_with_indent(stmt, indent + 2))
            .collect();

        format!(
            "{{\n{}\n{}}}",
            statements.join(&format!("\n{}", " ".repeat(indent + 2))),
            " ".repeat(indent)
        )
    }
}
