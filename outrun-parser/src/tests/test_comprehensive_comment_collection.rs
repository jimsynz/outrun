// Test comprehensive comment collection from all AST positions
// Verifies that the pre-walk comment extraction captures comments from anywhere in the source

use crate::ast::*;
use crate::parser::OutrunParser;

#[test]
fn test_comprehensive_comment_collection() {
    let input = r#"# Top level comment
const FIRST: Integer = 42 # Inline comment after constant

### Block comment ###

# Comment before second constant  
const SECOND: Integer = 100

# Final comment"#;

    let program = OutrunParser::parse_program(input).unwrap();

    // Should collect all comments regardless of position
    assert_eq!(program.debug_info.comments.len(), 5);

    // Verify comment content and types
    assert_eq!(program.debug_info.comments[0].content, " Top level comment");
    assert_eq!(program.debug_info.comments[0].kind, CommentKind::Line);

    assert_eq!(
        program.debug_info.comments[1].content,
        " Inline comment after constant"
    );
    assert_eq!(program.debug_info.comments[1].kind, CommentKind::Line);

    assert_eq!(program.debug_info.comments[2].content, " Block comment ");
    assert_eq!(program.debug_info.comments[2].kind, CommentKind::Block);

    assert_eq!(
        program.debug_info.comments[3].content,
        " Comment before second constant  "
    );
    assert_eq!(program.debug_info.comments[3].kind, CommentKind::Line);

    assert_eq!(program.debug_info.comments[4].content, " Final comment");
    assert_eq!(program.debug_info.comments[4].kind, CommentKind::Line);

    // Verify comments are ordered by span position
    for i in 1..program.debug_info.comments.len() {
        assert!(
            program.debug_info.comments[i - 1].span.start
                < program.debug_info.comments[i].span.start,
            "Comments should be ordered by span position"
        );
    }

    // Verify that comments don't appear as items anymore
    let comment_items: Vec<_> = program
        .items
        .iter()
        .filter(|item| matches!(item.kind, ItemKind::Comment(_)))
        .collect();

    assert_eq!(
        comment_items.len(),
        0,
        "Comments should not appear as items"
    );

    // Should still have the non-comment items
    let const_items: Vec<_> = program
        .items
        .iter()
        .filter(|item| matches!(item.kind, ItemKind::ConstDefinition(_)))
        .collect();

    assert_eq!(const_items.len(), 2, "Should have 2 constant definitions");
}

#[test]
fn test_pre_walk_comment_extraction_vs_inline_collection() {
    // This test verifies that the pre-walk approach captures comments
    // that would be missed by inline collection during AST construction

    let input = r#"# Comment 1
# Comment 2  
const VALUE: Integer = 42
# Comment 3"#;

    let program = OutrunParser::parse_program(input).unwrap();

    // Pre-walk should capture all 3 comments
    assert_eq!(program.debug_info.comments.len(), 3);

    // Verify all comments are captured with correct content
    let comment_contents: Vec<&str> = program
        .debug_info
        .comments
        .iter()
        .map(|c| c.content.as_str())
        .collect();

    assert_eq!(
        comment_contents,
        vec![" Comment 1", " Comment 2  ", " Comment 3"]
    );
}

#[test]
fn test_mixed_comment_types_comprehensive() {
    let input = r#"# Line comment 1
### Block comment 1 ###
const FIRST: Integer = 1
# Line comment 2
### Block comment 2 ###"#;

    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.debug_info.comments.len(), 4);

    // Verify mixed comment types are all captured
    let line_comments: Vec<_> = program
        .debug_info
        .comments
        .iter()
        .filter(|c| c.kind == CommentKind::Line)
        .collect();
    let block_comments: Vec<_> = program
        .debug_info
        .comments
        .iter()
        .filter(|c| c.kind == CommentKind::Block)
        .collect();

    assert_eq!(line_comments.len(), 2);
    assert_eq!(block_comments.len(), 2);
}
