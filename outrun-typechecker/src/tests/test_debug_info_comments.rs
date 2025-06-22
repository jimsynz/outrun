//! Tests for debug info and comment attachment in typed AST

use crate::checker::{CommentAttachment, InterpolationPart, TypedDebugInfo};
use crate::typed_ast_builder::CommentAttacher;
use outrun_parser::{parse_program, Comment, CommentKind, Span};

#[test]
fn test_comment_attachment_spatial_analysis() {
    let mut attacher = CommentAttacher::new(vec![
        Comment {
            content: "Function documentation".to_string(),
            kind: CommentKind::Line,
            span: Span::new(0, 22),
        },
        Comment {
            content: "Inline comment".to_string(),
            kind: CommentKind::Line,
            span: Span::new(50, 64),
        },
        Comment {
            content: "Trailing comment".to_string(),
            kind: CommentKind::Line,
            span: Span::new(100, 116),
        },
    ]);

    // Test preceding comment attachment
    let node_span = Span::new(25, 80);
    let attached = attacher.attach_comments_to_node(node_span);

    assert_eq!(attached.len(), 2);
    assert!(matches!(
        attached[0].attachment,
        CommentAttachment::Preceding
    ));
    assert_eq!(attached[0].comment.content, "Function documentation");
    assert!(matches!(
        attached[1].attachment,
        CommentAttachment::Internal
    ));
    assert_eq!(attached[1].comment.content, "Inline comment");
}

#[test]
fn test_comment_attachment_immediately_preceding() {
    let mut attacher = CommentAttacher::new(vec![Comment {
        content: "This is documentation".to_string(),
        kind: CommentKind::Line,
        span: Span::new(90, 111),
    }]);

    // Comment immediately precedes the node (within 10 characters)
    let node_span = Span::new(115, 150);
    let attached = attacher.attach_comments_to_node(node_span);

    assert_eq!(attached.len(), 1);
    assert!(matches!(
        attached[0].attachment,
        CommentAttachment::Preceding
    ));
}

#[test]
fn test_comment_attachment_immediately_following() {
    let mut attacher = CommentAttacher::new(vec![Comment {
        content: "Trailing comment".to_string(),
        kind: CommentKind::Line,
        span: Span::new(105, 121),
    }]);

    // Comment immediately follows the node (within 10 characters)
    let node_span = Span::new(90, 100);
    let attached = attacher.attach_comments_to_node(node_span);

    assert_eq!(attached.len(), 1);
    assert!(matches!(
        attached[0].attachment,
        CommentAttachment::Trailing
    ));
}

#[test]
fn test_comment_attachment_scope_distance() {
    let mut attacher = CommentAttacher::new(vec![Comment {
        content: "Scope comment".to_string(),
        kind: CommentKind::Line,
        span: Span::new(50, 63),
    }]);

    // Comment is within scope distance (less than 50 characters) but not immediate
    let node_span = Span::new(80, 120);
    let attached = attacher.attach_comments_to_node(node_span);

    assert_eq!(attached.len(), 1);
    assert!(matches!(attached[0].attachment, CommentAttachment::Scope));
}

#[test]
fn test_comment_attachment_too_distant() {
    let mut attacher = CommentAttacher::new(vec![Comment {
        content: "Distant comment".to_string(),
        kind: CommentKind::Line,
        span: Span::new(50, 65),
    }]);

    // Comment is too far away (more than 50 characters)
    let node_span = Span::new(150, 200);
    let attached = attacher.attach_comments_to_node(node_span);

    assert_eq!(attached.len(), 0);
}

#[test]
fn test_typed_ast_builder_with_comments() {
    // Create a simple program with comments
    let source = r#"
    # This is a function comment
    def add(a: Integer, b: Integer): Integer {
        a + b  # inline calculation
    }
    "#;

    let program = parse_program(source).expect("Failed to parse program");

    // Verify the program has comments
    assert!(!program.debug_info.comments.is_empty());

    // TODO: Once TypedASTBuilder integration is complete, test that comments
    // are properly attached to typed nodes
}

#[test]
fn test_debug_info_default() {
    let debug_info = TypedDebugInfo::default();
    assert!(debug_info.comments.is_empty());
    assert!(debug_info.source_file.is_none());
    assert_eq!(debug_info.original_span, Span::new(0, 0));
    assert!(debug_info.type_annotations.is_empty());
    assert!(debug_info.inferred_types.is_empty());
    assert!(debug_info.literal_format.is_none());
}

#[test]
fn test_string_interpolation_format_preservation() {
    use crate::multi_program_compiler::FunctionRegistry;
    use crate::typed_ast_builder::TypedASTBuilder;
    use crate::unification::UnificationContext;
    use outrun_parser::{Span, StringFormat, StringLiteral, StringPart};

    // Create a mock TypedASTBuilder for testing
    let context = UnificationContext::default();
    let function_registry = FunctionRegistry::default();
    let builder =
        TypedASTBuilder::new(context, function_registry, std::collections::HashMap::new());

    // Test simple string reconstruction
    let simple_string = StringLiteral {
        parts: vec![StringPart::Text {
            content: "Hello World".to_string(),
            raw_content: "Hello World".to_string(),
        }],
        format: StringFormat::Basic,
        span: Span::new(0, 13),
    };

    let (original_text, interpolation_parts) = builder.reconstruct_simple_string(&simple_string);
    assert_eq!(original_text, "\"Hello World\"");
    assert!(interpolation_parts.is_none());
}

#[test]
fn test_interpolated_string_reconstruction() {
    use crate::multi_program_compiler::FunctionRegistry;
    use crate::typed_ast_builder::TypedASTBuilder;
    use crate::unification::UnificationContext;
    use outrun_parser::{
        Expression, ExpressionKind, Identifier, Span, StringFormat, StringLiteral, StringPart,
    };

    // Create a mock TypedASTBuilder for testing
    let context = UnificationContext::default();
    let function_registry = FunctionRegistry::default();
    let builder =
        TypedASTBuilder::new(context, function_registry, std::collections::HashMap::new());

    // Create an interpolated string: "Hello #{name}!"
    let name_expr = Expression {
        kind: ExpressionKind::Identifier(Identifier {
            name: "name".to_string(),
            span: Span::new(8, 12),
        }),
        span: Span::new(8, 12),
    };

    let interpolated_string = StringLiteral {
        parts: vec![
            StringPart::Text {
                content: "Hello ".to_string(),
                raw_content: "Hello ".to_string(),
            },
            StringPart::Interpolation {
                expression: Box::new(name_expr),
                span: Span::new(7, 13),
            },
            StringPart::Text {
                content: "!".to_string(),
                raw_content: "!".to_string(),
            },
        ],
        format: StringFormat::Basic,
        span: Span::new(0, 15),
    };

    let (original_text, interpolation_parts) =
        builder.reconstruct_interpolated_string(&interpolated_string);

    // Check that the original text is reconstructed correctly
    assert_eq!(original_text, "\"Hello #{name}!\"");

    // Check that interpolation parts are extracted
    assert!(interpolation_parts.is_some());
    let parts = interpolation_parts.unwrap();
    assert_eq!(parts.len(), 3);

    // Verify the parts
    match &parts[0] {
        InterpolationPart::Text { content, .. } => {
            assert_eq!(content, "Hello ");
        }
        _ => panic!("Expected text part"),
    }

    match &parts[1] {
        InterpolationPart::Expression {
            original_expression_text,
            ..
        } => {
            assert_eq!(original_expression_text, "name");
        }
        _ => panic!("Expected expression part"),
    }

    match &parts[2] {
        InterpolationPart::Text { content, .. } => {
            assert_eq!(content, "!");
        }
        _ => panic!("Expected text part"),
    }
}

#[test]
fn test_expression_to_text_reconstruction() {
    use crate::multi_program_compiler::FunctionRegistry;
    use crate::typed_ast_builder::TypedASTBuilder;
    use crate::unification::UnificationContext;
    use outrun_parser::{
        AtomLiteral, BooleanLiteral, Expression, ExpressionKind, Identifier, IntegerFormat,
        IntegerLiteral, Span,
    };

    // Create a mock TypedASTBuilder for testing
    let context = UnificationContext::default();
    let function_registry = FunctionRegistry::default();
    let builder =
        TypedASTBuilder::new(context, function_registry, std::collections::HashMap::new());

    // Test identifier
    let identifier_expr = Expression {
        kind: ExpressionKind::Identifier(Identifier {
            name: "variable".to_string(),
            span: Span::new(0, 8),
        }),
        span: Span::new(0, 8),
    };
    assert_eq!(builder.expression_to_text(&identifier_expr), "variable");

    // Test integer
    let integer_expr = Expression {
        kind: ExpressionKind::Integer(IntegerLiteral {
            value: 42,
            format: IntegerFormat::Decimal,
            raw_text: "42".to_string(),
            span: Span::new(0, 2),
        }),
        span: Span::new(0, 2),
    };
    assert_eq!(builder.expression_to_text(&integer_expr), "42");

    // Test boolean
    let boolean_expr = Expression {
        kind: ExpressionKind::Boolean(BooleanLiteral {
            value: true,
            span: Span::new(0, 4),
        }),
        span: Span::new(0, 4),
    };
    assert_eq!(builder.expression_to_text(&boolean_expr), "true");

    // Test atom
    let atom_expr = Expression {
        kind: ExpressionKind::Atom(AtomLiteral {
            name: "symbol".to_string(),
            content: "symbol".to_string(),
            format: outrun_parser::AtomFormat::Simple,
            raw_content: "symbol".to_string(),
            span: Span::new(0, 7),
        }),
        span: Span::new(0, 7),
    };
    assert_eq!(builder.expression_to_text(&atom_expr), ":symbol");
}
