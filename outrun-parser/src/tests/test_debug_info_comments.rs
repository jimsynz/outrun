use crate::ast::*;
use crate::parser::OutrunParser;

#[test]
fn test_comments_stored_in_debug_info() {
    let input = r#"# Top comment
const VALUE: Integer = 42
# Bottom comment"#;

    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.debug_info.comments.len(), 2);

    assert_eq!(program.debug_info.comments[0].content, " Top comment");
    assert_eq!(program.debug_info.comments[0].kind, CommentKind::Line);

    assert_eq!(program.debug_info.comments[1].content, " Bottom comment");
    assert_eq!(program.debug_info.comments[1].kind, CommentKind::Line);

    assert!(program.debug_info.comments[0].span.start < program.debug_info.comments[1].span.start);
}

#[test]
fn test_block_comments_stored_in_debug_info() {
    let input = r#"### Block comment ###
const VALUE: Integer = 42"#;

    let program = OutrunParser::parse_program(input).unwrap();

    let block_comments: Vec<_> = program
        .debug_info
        .comments
        .iter()
        .filter(|c| c.kind == CommentKind::Block)
        .collect();

    assert_eq!(block_comments.len(), 1);
    assert_eq!(block_comments[0].content, " Block comment ");
}

//
//
//
//
//
//

#[test]
fn test_no_comments_empty_debug_info() {
    let input = r#"struct User(name: String) {
    def new(name: String): User {
        User { name: name }
    }
}"#;

    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.debug_info.comments.len(), 0);
}

#[test]
fn test_comments_not_in_items_list() {
    let input = r#"# Comment
const VALUE: Integer = 42
# Another comment"#;

    let program = OutrunParser::parse_program(input).unwrap();

    assert_eq!(program.debug_info.comments.len(), 2);

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

    let const_items: Vec<_> = program
        .items
        .iter()
        .filter(|item| matches!(item.kind, ItemKind::ConstDefinition(_)))
        .collect();

    assert_eq!(const_items.len(), 1);
}
