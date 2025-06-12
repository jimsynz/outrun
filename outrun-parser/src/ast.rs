// Outrun AST Definitions
// Abstract Syntax Tree nodes with source preservation

/// Source position information for AST nodes
#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub start_line_col: Option<(usize, usize)>,
    pub end_line_col: Option<(usize, usize)>,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self {
            start,
            end,
            start_line_col: None,
            end_line_col: None,
        }
    }

    pub fn with_line_col(
        start: usize,
        end: usize,
        start_line_col: (usize, usize),
        end_line_col: (usize, usize),
    ) -> Self {
        Self {
            start,
            end,
            start_line_col: Some(start_line_col),
            end_line_col: Some(end_line_col),
        }
    }
}

/// Debug information for source preservation and tooling
#[derive(Debug, Clone, PartialEq, Default)]
pub struct DebugInfo {
    pub comments: Vec<Comment>,
    pub source_file: Option<String>,
    // Future: source_text, line_directives, source_maps, etc.
}

impl DebugInfo {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_source_file(source_file: Option<String>) -> Self {
        Self {
            comments: Vec::new(),
            source_file,
        }
    }
}

/// Top-level program containing all items
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub items: Vec<Item>,
    pub debug_info: DebugInfo,
    pub span: Span,
}

/// Top-level items (for now, very minimal)
#[derive(Debug, Clone, PartialEq)]
pub struct Item {
    pub kind: ItemKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind {
    Keyword(Keyword),
    Expression(Expression),
    BooleanLiteral(BooleanLiteral),
    IntegerLiteral(IntegerLiteral),
    FloatLiteral(FloatLiteral),
    StringLiteral(StringLiteral),
    AtomLiteral(AtomLiteral),
    SigilLiteral(SigilLiteral),
    ListLiteral(ListLiteral),
    MapLiteral(MapLiteral),
    TupleLiteral(TupleLiteral),
    Identifier(Identifier),
    TypeIdentifier(TypeIdentifier),
    FunctionDefinition(FunctionDefinition),
    ConstDefinition(ConstDefinition),
    LetBinding(LetBinding),
    StructDefinition(StructDefinition),
    TraitDefinition(TraitDefinition),
    ImplBlock(ImplBlock),
    AliasDefinition(AliasDefinition),
    ImportDefinition(ImportDefinition),
    MacroDefinition(MacroDefinition),
    Comment(Comment),
}

/// Keywords with their exact source representation
#[derive(Debug, Clone, PartialEq)]
pub struct Keyword {
    pub kind: KeywordKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum KeywordKind {
    Struct,
    Trait,
    Impl,
    Def,
    Defp,
    Let,
    Const,
    Fn,
    If,
    Else,
    Case,
    When,
    Alias,
    Import,
    Macro,
    For,
    Self_,
    As,
    Only,
    Except,
}

/// Boolean literals with source preservation
#[derive(Debug, Clone, PartialEq)]
pub struct BooleanLiteral {
    pub value: bool,
    pub span: Span,
}

/// Integer literals with format preservation
#[derive(Debug, Clone, PartialEq)]
pub struct IntegerLiteral {
    pub value: i64, // Parse the actual numeric value
    pub format: IntegerFormat,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IntegerFormat {
    Decimal,
    Binary,      // 0b prefix
    Octal,       // 0o prefix
    Hexadecimal, // 0x prefix
}

/// Float literals with format preservation
#[derive(Debug, Clone, PartialEq)]
pub struct FloatLiteral {
    pub value: f64, // Parse the actual numeric value
    pub format: FloatFormat,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FloatFormat {
    Standard,                                   // 3.14
    Scientific { exponent_case: ExponentCase }, // 1.23e-4 or 1.23E-4
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExponentCase {
    Lowercase, // e
    Uppercase, // E
}

/// String literals with format preservation and interpolation support
#[derive(Debug, Clone, PartialEq)]
pub struct StringLiteral {
    pub parts: Vec<StringPart>, // String parts (text and interpolations)
    pub format: StringFormat,
    pub span: Span,
}

/// Parts of a string (text or interpolated expressions)
#[derive(Debug, Clone, PartialEq)]
pub enum StringPart {
    Text {
        content: String,     // Processed content (escapes resolved)
        raw_content: String, // Original content (for source reconstruction)
    },
    Interpolation {
        expression: Box<Expression>, // The parsed expression inside #{...}
        span: Span,                  // Span of the interpolation
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum StringFormat {
    Basic,     // "..."
    Multiline, // """..."""
}

/// Atom literals with format preservation
#[derive(Debug, Clone, PartialEq)]
pub struct AtomLiteral {
    pub name: String,        // The atom name (without : prefix)
    pub content: String,     // Processed content for quoted atoms (escapes resolved)
    pub raw_content: String, // Original content for quoted atoms (for source reconstruction)
    pub format: AtomFormat,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AtomFormat {
    Simple, // :symbol
    Quoted, // :"string"
}

/// Sigil literals with embedded DSL support
#[derive(Debug, Clone, PartialEq)]
pub struct SigilLiteral {
    pub sigil_type: TypeIdentifier, // The type name (e.g., SQL, JSON, HTML)
    pub string: StringLiteral,      // The string content with interpolation support
    pub span: Span,
}

/// List element that can be either a regular expression or a spread
#[derive(Debug, Clone, PartialEq)]
pub enum ListElement {
    Expression(Box<Expression>),
    Spread(Identifier),
}

/// List literals [element1, element2, ...] with spread support
#[derive(Debug, Clone, PartialEq)]
pub struct ListLiteral {
    pub elements: Vec<ListElement>, // The list elements (expressions or spreads)
    pub span: Span,
}

/// Map literals {key: value} or {key => value}
#[derive(Debug, Clone, PartialEq)]
pub struct MapLiteral {
    pub entries: Vec<MapEntry>,
    pub span: Span,
}

/// Map entry that can be assignment, shorthand, or spread
#[derive(Debug, Clone, PartialEq)]
pub enum MapEntry {
    Assignment {
        key: Box<Expression>,
        value: Box<Expression>,
    },
    Shorthand {
        name: Identifier,
        value: Box<Expression>,
    },
    Spread(Identifier),
}

#[derive(Debug, Clone, PartialEq)]
pub enum MapEntryFormat {
    Shorthand, // {name: value} - identifier key becomes atom
    Explicit,  // {key => value} - any expression as key
}

/// Tuple literals (element1, element2, ...)
#[derive(Debug, Clone, PartialEq)]
pub struct TupleLiteral {
    pub elements: Vec<Expression>, // The tuple elements
    pub span: Span,
}

/// Struct literal field that can be assignment, shorthand, or spread
#[derive(Debug, Clone, PartialEq)]
pub enum StructLiteralField {
    Assignment {
        name: Identifier,
        value: Box<Expression>,
    },
    Shorthand(Identifier),
    Spread(Identifier),
}

/// Struct literals TypeName { field: value, ..spread }
#[derive(Debug, Clone, PartialEq)]
pub struct StructLiteral {
    pub type_name: TypeIdentifier,
    pub fields: Vec<StructLiteralField>,
    pub span: Span,
}

/// Simple identifiers (variables, functions)
#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub name: String,
    pub span: Span,
}

/// Type identifiers (structs, traits, modules)
#[derive(Debug, Clone, PartialEq)]
pub struct TypeIdentifier {
    pub name: String,
    pub span: Span,
}

/// Qualified identifiers (Module.function, Type.method)
#[derive(Debug, Clone, PartialEq)]
pub struct QualifiedIdentifier {
    pub module: TypeIdentifier,
    pub name: Identifier,
    pub span: Span,
}

/// Comments with content and source spans
#[derive(Debug, Clone, PartialEq)]
pub struct Comment {
    pub content: String,
    pub kind: CommentKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CommentKind {
    Line,  // # comment
    Block, // ### comment ###
}

/// Attribute decorations (@Name or @Name(args))
#[derive(Debug, Clone, PartialEq)]
pub struct Attribute {
    pub name: TypeIdentifier,
    pub args: Option<AttributeArgs>,
    pub span: Span,
}

/// Attribute arguments (same syntax as function call arguments)
#[derive(Debug, Clone, PartialEq)]
pub struct AttributeArgs {
    pub arguments: Vec<Argument>,
    pub span: Span,
}

/// Binary operations with operator tracking
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOperation {
    pub left: Box<Expression>,
    pub operator: BinaryOperator,
    pub right: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    // Arithmetic operators
    Add,      // +
    Subtract, // -
    Multiply, // *
    Divide,   // /
    Modulo,   // %
    Exponent, // **

    // Comparison operators
    Equal,        // ==
    NotEqual,     // !=
    Less,         // <
    LessEqual,    // <=
    Greater,      // >
    GreaterEqual, // >=

    // Logical operators
    LogicalAnd, // &&
    LogicalOr,  // ||

    // Bitwise operators
    BitwiseAnd, // &
    BitwiseOr,  // |
    BitwiseXor, // ^
    ShiftLeft,  // <<
    ShiftRight, // >>

    // Pipe operators
    Pipe,      // |>
    PipeMaybe, // |?
}

/// Unary operations with operator tracking
#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOperation {
    pub operator: UnaryOperator,
    pub operand: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Plus,       // +
    Minus,      // -
    LogicalNot, // !
    BitwiseNot, // ~
}

/// Field access expressions (e.g., object.field)
#[derive(Debug, Clone, PartialEq)]
pub struct FieldAccess {
    pub object: Box<Expression>,
    pub field: Identifier,
    pub span: Span,
}

/// Function calls with named parameters
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
    pub path: FunctionPath,
    pub arguments: Vec<Argument>,
    pub span: Span,
}

/// Function path (simple or qualified)
#[derive(Debug, Clone, PartialEq)]
pub enum FunctionPath {
    Simple {
        name: Identifier,
    },
    Qualified {
        module: TypeIdentifier,
        name: Identifier,
    },
    Expression {
        expression: Box<Expression>,
    },
}

/// Function argument (named, shorthand, or spread)
#[derive(Debug, Clone, PartialEq)]
pub enum Argument {
    Named {
        name: Identifier,
        expression: Expression,
        format: ArgumentFormat,
        span: Span,
    },
    Spread {
        expression: Expression,
        kind: SpreadKind,
        span: Span,
    },
}

/// Format tracking for named arguments
#[derive(Debug, Clone, PartialEq)]
pub enum ArgumentFormat {
    Shorthand, // `func(arg)` - shorthand where variable name matches parameter
    Explicit,  // `func(arg: expr)` - explicit parameter name
}

/// Spread argument kind
#[derive(Debug, Clone, PartialEq)]
pub enum SpreadKind {
    Strict,  // `..expr` - requires exact field matches
    Lenient, // `..?expr` - ignores mismatched fields
}

/// Function definitions (def/defp)
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    pub attributes: Vec<Attribute>,
    pub visibility: FunctionVisibility,
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<TypeAnnotation>,
    pub guard: Option<GuardClause>,
    pub body: Block,
    pub span: Span,
}

/// Function visibility (public or private)
#[derive(Debug, Clone, PartialEq)]
pub enum FunctionVisibility {
    Public,  // def
    Private, // defp
}

impl std::fmt::Display for FunctionVisibility {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionVisibility::Public => write!(f, "def"),
            FunctionVisibility::Private => write!(f, "defp"),
        }
    }
}

/// Function parameter with type annotation
#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: Identifier,
    pub type_annotation: TypeAnnotation,
    pub span: Span,
}

/// Type annotation (basic module path for now)
#[derive(Debug, Clone, PartialEq)]
pub enum TypeAnnotation {
    Simple {
        path: Vec<TypeIdentifier>, // Module.Type or just Type
        generic_args: Option<GenericArgs>,
        span: Span,
    },
    Tuple {
        types: Vec<TypeAnnotation>,
        span: Span,
    },
    Function {
        params: Vec<FunctionTypeParam>,
        return_type: Box<TypeAnnotation>,
        span: Span,
    },
}

/// Function type parameter: name: Type
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionTypeParam {
    pub name: Identifier,
    pub type_annotation: TypeAnnotation,
    pub span: Span,
}

/// Guard clause for function definitions
#[derive(Debug, Clone, PartialEq)]
pub struct GuardClause {
    pub condition: Expression,
    pub span: Span,
}

/// Block statement containing multiple statements
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub span: Span,
}

/// Statements within blocks (for now, just expressions)
#[derive(Debug, Clone, PartialEq)]
pub struct Statement {
    pub kind: StatementKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementKind {
    Expression(Box<Expression>),
    LetBinding(Box<LetBinding>),
}

// === PATTERNS ===

/// Pattern for destructuring assignments
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Identifier(Identifier),
    Tuple(TuplePattern),
    Struct(StructPattern),
    List(ListPattern),
}

/// Tuple destructuring pattern
#[derive(Debug, Clone, PartialEq)]
pub struct TuplePattern {
    pub elements: Vec<Identifier>,
    pub span: Span,
}

/// Struct destructuring pattern
#[derive(Debug, Clone, PartialEq)]
pub struct StructPattern {
    pub type_name: TypeIdentifier,
    pub fields: Vec<Identifier>,
    pub span: Span,
}

/// List destructuring pattern
#[derive(Debug, Clone, PartialEq)]
pub struct ListPattern {
    pub elements: Vec<Identifier>,
    pub rest: Option<Identifier>, // for [first, ..rest] syntax
    pub span: Span,
}

/// Constant definition with required type annotation
#[derive(Debug, Clone, PartialEq)]
pub struct ConstDefinition {
    pub name: TypeIdentifier,
    pub type_annotation: TypeAnnotation,
    pub expression: Expression,
    pub span: Span,
}

/// Let binding with optional type annotation for inference
#[derive(Debug, Clone, PartialEq)]
pub struct LetBinding {
    pub pattern: Pattern,
    pub type_annotation: Option<TypeAnnotation>, // None = infer type
    pub expression: Expression,
    pub span: Span,
}

/// If expression with optional else clause
#[derive(Debug, Clone, PartialEq)]
pub struct IfExpression {
    pub condition: Box<Expression>,
    pub then_block: Block,
    pub else_block: Option<Block>, // None for if without else
    pub span: Span,
}

/// Case expression with when clauses and else clause
#[derive(Debug, Clone, PartialEq)]
pub struct CaseExpression {
    pub expression: Box<Expression>, // The value being matched
    pub when_clauses: Vec<CaseWhenClause>,
    pub else_clause: CaseElseClause,
    pub span: Span,
}

/// When clause in case expression
#[derive(Debug, Clone, PartialEq)]
pub struct CaseWhenClause {
    pub guard: Expression,  // The guard condition (must return boolean)
    pub result: CaseResult, // Block or expression result
    pub span: Span,
}

/// Else clause in case expression  
#[derive(Debug, Clone, PartialEq)]
pub struct CaseElseClause {
    pub result: CaseResult, // Block or expression result
    pub span: Span,
}

/// Result of a case clause (either block or expression)
#[derive(Debug, Clone, PartialEq)]
pub enum CaseResult {
    Block(Block),
    Expression(Box<Expression>),
}

/// Expressions (placeholder for future development)
#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionKind {
    Boolean(BooleanLiteral),
    Integer(IntegerLiteral),
    Float(FloatLiteral),
    String(StringLiteral),
    Atom(AtomLiteral),
    Sigil(SigilLiteral),
    List(ListLiteral),
    Map(MapLiteral),
    Tuple(TupleLiteral),
    Struct(StructLiteral),
    Identifier(Identifier),
    TypeIdentifier(TypeIdentifier),
    QualifiedIdentifier(QualifiedIdentifier),
    BinaryOp(BinaryOperation),
    UnaryOp(UnaryOperation),
    FieldAccess(FieldAccess),
    FunctionCall(FunctionCall),
    IfExpression(IfExpression),
    CaseExpression(CaseExpression),
    MacroInjection(MacroInjection),
    AnonymousFunction(AnonymousFunction),
    FunctionCapture(FunctionCapture),
    Parenthesized(Box<Expression>),
}

// Implementation of Display for pretty-printing (preserving original formatting)
impl std::fmt::Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for item in &self.items {
            write!(f, "{}", item)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ItemKind::Keyword(kw) => write!(f, "{}", kw),
            ItemKind::Expression(expr) => write!(f, "{}", expr),
            ItemKind::BooleanLiteral(lit) => write!(f, "{}", lit),
            ItemKind::IntegerLiteral(lit) => write!(f, "{}", lit),
            ItemKind::FloatLiteral(lit) => write!(f, "{}", lit),
            ItemKind::StringLiteral(lit) => write!(f, "{}", lit),
            ItemKind::AtomLiteral(lit) => write!(f, "{}", lit),
            ItemKind::SigilLiteral(lit) => write!(f, "{}", lit),
            ItemKind::ListLiteral(lit) => write!(f, "{}", lit),
            ItemKind::MapLiteral(lit) => write!(f, "{}", lit),
            ItemKind::TupleLiteral(lit) => write!(f, "{}", lit),
            ItemKind::Identifier(id) => write!(f, "{}", id),
            ItemKind::TypeIdentifier(id) => write!(f, "{}", id),
            ItemKind::FunctionDefinition(func) => write!(f, "{}", func),
            ItemKind::ConstDefinition(const_def) => write!(f, "{}", const_def),
            ItemKind::LetBinding(let_binding) => write!(f, "{}", let_binding),
            ItemKind::StructDefinition(struct_def) => write!(f, "{}", struct_def),
            ItemKind::TraitDefinition(trait_def) => write!(f, "{}", trait_def),
            ItemKind::ImplBlock(impl_block) => write!(f, "{}", impl_block),
            ItemKind::AliasDefinition(alias_def) => write!(f, "{}", alias_def),
            ItemKind::ImportDefinition(import_def) => write!(f, "{}", import_def),
            ItemKind::MacroDefinition(macro_def) => write!(f, "{}", macro_def),
            ItemKind::Comment(comment) => write!(f, "{}", comment),
        }
    }
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let keyword_str = match self.kind {
            KeywordKind::Struct => "struct",
            KeywordKind::Trait => "trait",
            KeywordKind::Impl => "impl",
            KeywordKind::Def => "def",
            KeywordKind::Defp => "defp",
            KeywordKind::Let => "let",
            KeywordKind::Const => "const",
            KeywordKind::Fn => "fn",
            KeywordKind::If => "if",
            KeywordKind::Else => "else",
            KeywordKind::Case => "case",
            KeywordKind::When => "when",
            KeywordKind::Alias => "alias",
            KeywordKind::Import => "import",
            KeywordKind::Macro => "macro",
            KeywordKind::For => "for",
            KeywordKind::Self_ => "Self",
            KeywordKind::As => "as",
            KeywordKind::Only => "only",
            KeywordKind::Except => "except",
        };
        write!(f, "{}", keyword_str)
    }
}

impl std::fmt::Display for BooleanLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", if self.value { "true" } else { "false" })
    }
}

impl std::fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Preserve the original format when displaying
        match self.format {
            IntegerFormat::Decimal => write!(f, "{}", self.value),
            IntegerFormat::Binary => write!(f, "0b{:b}", self.value),
            IntegerFormat::Octal => write!(f, "0o{:o}", self.value),
            IntegerFormat::Hexadecimal => write!(f, "0x{:x}", self.value),
        }
    }
}

impl std::fmt::Display for FloatLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // For now, just display the value - we'll improve format preservation later
        match &self.format {
            FloatFormat::Standard => write!(f, "{}", self.value),
            FloatFormat::Scientific { exponent_case } => {
                let output = format!("{:e}", self.value);
                if matches!(exponent_case, ExponentCase::Uppercase) {
                    write!(f, "{}", output.replace('e', "E"))
                } else {
                    write!(f, "{}", output)
                }
            }
        }
    }
}

impl std::fmt::Display for StringLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Display with quotes and reconstruct content from parts
        let quote_str = match self.format {
            StringFormat::Basic => "\"",
            StringFormat::Multiline => "\"\"\"",
        };

        write!(f, "{}", quote_str)?;
        for part in &self.parts {
            match part {
                StringPart::Text { raw_content, .. } => write!(f, "{}", raw_content)?,
                StringPart::Interpolation { expression, .. } => write!(f, "#{{{}}}", expression)?,
            }
        }
        write!(f, "{}", quote_str)
    }
}

impl std::fmt::Display for AtomLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Preserve the original format when displaying
        match self.format {
            AtomFormat::Simple => write!(f, ":{}", self.name),
            AtomFormat::Quoted => write!(f, ":\"{}\"", self.raw_content),
        }
    }
}

impl std::fmt::Display for SigilLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Display format: ~TypeName"content"
        write!(f, "~{}{}", self.sigil_type, self.string)
    }
}

impl std::fmt::Display for ListElement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ListElement::Expression(expr) => write!(f, "{}", expr),
            ListElement::Spread(identifier) => write!(f, "..{}", identifier),
        }
    }
}

impl std::fmt::Display for ListLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        for (i, element) in self.elements.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", element)?;
        }
        write!(f, "]")
    }
}

impl std::fmt::Display for MapLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for (i, entry) in self.entries.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", entry)?;
        }
        write!(f, "}}")
    }
}

impl std::fmt::Display for MapEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MapEntry::Assignment { key, value } => write!(f, "{} => {}", key, value),
            MapEntry::Shorthand { name, value } => write!(f, "{}: {}", name, value),
            MapEntry::Spread(name) => write!(f, "..{}", name),
        }
    }
}

impl std::fmt::Display for TupleLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        for (i, element) in self.elements.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", element)?;
        }
        // Add trailing comma for single-element tuples
        if self.elements.len() == 1 {
            write!(f, ",")?;
        }
        write!(f, ")")
    }
}

impl std::fmt::Display for StructLiteralField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StructLiteralField::Assignment { name, value } => write!(f, "{}: {}", name, value),
            StructLiteralField::Shorthand(name) => write!(f, "{}", name),
            StructLiteralField::Spread(name) => write!(f, "..{}", name),
        }
    }
}

impl std::fmt::Display for StructLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {{", self.type_name)?;
        for (i, field) in self.fields.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", field)?;
        }
        write!(f, "}}")
    }
}

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl std::fmt::Display for TypeIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl std::fmt::Display for QualifiedIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.module, self.name)
    }
}

impl std::fmt::Display for Comment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            CommentKind::Line => write!(f, "#{}", self.content),
            CommentKind::Block => write!(f, "###{}###", self.content),
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ExpressionKind::Boolean(lit) => write!(f, "{}", lit),
            ExpressionKind::Integer(lit) => write!(f, "{}", lit),
            ExpressionKind::Float(lit) => write!(f, "{}", lit),
            ExpressionKind::String(lit) => write!(f, "{}", lit),
            ExpressionKind::Atom(lit) => write!(f, "{}", lit),
            ExpressionKind::Sigil(lit) => write!(f, "{}", lit),
            ExpressionKind::List(lit) => write!(f, "{}", lit),
            ExpressionKind::Map(lit) => write!(f, "{}", lit),
            ExpressionKind::Tuple(lit) => write!(f, "{}", lit),
            ExpressionKind::Struct(lit) => write!(f, "{}", lit),
            ExpressionKind::Identifier(id) => write!(f, "{}", id),
            ExpressionKind::TypeIdentifier(id) => write!(f, "{}", id),
            ExpressionKind::QualifiedIdentifier(qid) => write!(f, "{}", qid),
            ExpressionKind::BinaryOp(op) => write!(f, "{}", op),
            ExpressionKind::UnaryOp(op) => write!(f, "{}", op),
            ExpressionKind::FieldAccess(access) => write!(f, "{}", access),
            ExpressionKind::FunctionCall(call) => write!(f, "{}", call),
            ExpressionKind::IfExpression(if_expr) => write!(f, "{}", if_expr),
            ExpressionKind::CaseExpression(case_expr) => write!(f, "{}", case_expr),
            ExpressionKind::MacroInjection(injection) => write!(f, "{}", injection),
            ExpressionKind::AnonymousFunction(anon_fn) => write!(f, "{}", anon_fn),
            ExpressionKind::FunctionCapture(capture) => write!(f, "{}", capture),
            ExpressionKind::Parenthesized(expr) => write!(f, "({})", expr),
        }
    }
}

impl std::fmt::Display for BinaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op_str = match self.operator {
            BinaryOperator::Add => " + ",
            BinaryOperator::Subtract => " - ",
            BinaryOperator::Multiply => " * ",
            BinaryOperator::Divide => " / ",
            BinaryOperator::Modulo => " % ",
            BinaryOperator::Exponent => " ** ",
            BinaryOperator::Equal => " == ",
            BinaryOperator::NotEqual => " != ",
            BinaryOperator::Less => " < ",
            BinaryOperator::LessEqual => " <= ",
            BinaryOperator::Greater => " > ",
            BinaryOperator::GreaterEqual => " >= ",
            BinaryOperator::LogicalAnd => " && ",
            BinaryOperator::LogicalOr => " || ",
            BinaryOperator::BitwiseAnd => " & ",
            BinaryOperator::BitwiseOr => " | ",
            BinaryOperator::BitwiseXor => " ^ ",
            BinaryOperator::ShiftLeft => " << ",
            BinaryOperator::ShiftRight => " >> ",
            BinaryOperator::Pipe => " |> ",
            BinaryOperator::PipeMaybe => " |? ",
        };
        write!(f, "{}{}{}", self.left, op_str, self.right)
    }
}

impl std::fmt::Display for UnaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op_str = match self.operator {
            UnaryOperator::Plus => "+",
            UnaryOperator::Minus => "-",
            UnaryOperator::LogicalNot => "!",
            UnaryOperator::BitwiseNot => "~",
        };
        write!(f, "{}{}", op_str, self.operand)
    }
}

impl std::fmt::Display for FieldAccess {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.object, self.field)
    }
}

impl std::fmt::Display for FunctionCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(", self.path)?;
        for (i, arg) in self.arguments.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", arg)?;
        }
        write!(f, ")")
    }
}

impl std::fmt::Display for FunctionPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionPath::Simple { name } => write!(f, "{}", name),
            FunctionPath::Qualified { module, name } => write!(f, "{}.{}", module, name),
            FunctionPath::Expression { expression } => write!(f, "{}", expression),
        }
    }
}

impl std::fmt::Display for Argument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Argument::Named {
                name,
                expression,
                format,
                ..
            } => {
                match format {
                    ArgumentFormat::Shorthand => {
                        // For shorthand, just display the identifier (no colon)
                        write!(f, "{}", name)
                    }
                    ArgumentFormat::Explicit => {
                        // For explicit, display name: expression
                        write!(f, "{}: {}", name, expression)
                    }
                }
            }
            Argument::Spread {
                expression, kind, ..
            } => {
                let spread_op = match kind {
                    SpreadKind::Strict => "..",
                    SpreadKind::Lenient => "..?",
                };
                write!(f, "{}{}", spread_op, expression)
            }
        }
    }
}

impl std::fmt::Display for FunctionDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Display attributes
        for attr in &self.attributes {
            writeln!(f, "{}", attr)?;
        }

        // Display function visibility
        match self.visibility {
            FunctionVisibility::Public => write!(f, "def")?,
            FunctionVisibility::Private => write!(f, "defp")?,
        }

        // Function name
        write!(f, " {}", self.name)?;

        // Parameters
        write!(f, "(")?;
        for (i, param) in self.parameters.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", param)?;
        }
        write!(f, ")")?;

        // Return type
        if let Some(return_type) = &self.return_type {
            write!(f, ": {}", return_type)?;
        }

        // Guard clause
        if let Some(guard) = &self.guard {
            write!(f, " {}", guard)?;
        }

        // Body
        write!(f, " {}", self.body)
    }
}

impl std::fmt::Display for Parameter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.type_annotation)
    }
}

impl std::fmt::Display for TypeAnnotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeAnnotation::Simple {
                path, generic_args, ..
            } => {
                for (i, part) in path.iter().enumerate() {
                    if i > 0 {
                        write!(f, ".")?;
                    }
                    write!(f, "{}", part)?;
                }
                if let Some(generic_args) = generic_args {
                    write!(f, "{}", generic_args)?;
                }
                Ok(())
            }
            TypeAnnotation::Tuple { types, .. } => {
                write!(f, "(")?;
                for (i, type_annotation) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", type_annotation)?;
                }
                write!(f, ")")
            }
            TypeAnnotation::Function {
                params,
                return_type,
                ..
            } => {
                write!(f, "Function<(")?;
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, ") -> {}>", return_type)
            }
        }
    }
}

impl std::fmt::Display for FunctionTypeParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.type_annotation)
    }
}

impl std::fmt::Display for GuardClause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "when {}", self.condition)
    }
}

impl std::fmt::Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{{")?;
        for statement in &self.statements {
            writeln!(f, "    {}", statement)?;
        }
        write!(f, "}}")
    }
}

impl std::fmt::Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            StatementKind::Expression(expr) => write!(f, "{}", expr),
            StatementKind::LetBinding(let_binding) => write!(f, "{}", let_binding),
        }
    }
}

impl std::fmt::Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pattern::Identifier(identifier) => write!(f, "{}", identifier),
            Pattern::Tuple(tuple) => write!(f, "{}", tuple),
            Pattern::Struct(struct_pattern) => write!(f, "{}", struct_pattern),
            Pattern::List(list) => write!(f, "{}", list),
        }
    }
}

impl std::fmt::Display for TuplePattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        for (i, element) in self.elements.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", element)?;
        }
        write!(f, ")")
    }
}

impl std::fmt::Display for StructPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {{ ", self.type_name)?;
        for (i, field) in self.fields.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", field)?;
        }
        write!(f, " }}")
    }
}

impl std::fmt::Display for ListPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        for (i, element) in self.elements.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", element)?;
        }
        if let Some(rest) = &self.rest {
            if !self.elements.is_empty() {
                write!(f, ", ")?;
            }
            write!(f, "..{}", rest)?;
        }
        write!(f, "]")
    }
}

impl std::fmt::Display for ConstDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "const {}: {} = {}",
            self.name, self.type_annotation, self.expression
        )
    }
}

impl std::fmt::Display for LetBinding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let {}", self.pattern)?;

        // Add type annotation if present
        if let Some(type_annotation) = &self.type_annotation {
            write!(f, ": {}", type_annotation)?;
        }

        write!(f, " = {}", self.expression)
    }
}

impl std::fmt::Display for IfExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if {} {}", self.condition, self.then_block)?;

        if let Some(else_block) = &self.else_block {
            write!(f, " else {}", else_block)?;
        }

        Ok(())
    }
}

impl std::fmt::Display for CaseExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "case {} {{", self.expression)?;

        for when_clause in &self.when_clauses {
            writeln!(f, "    {}", when_clause)?;
        }

        writeln!(f, "    {}", self.else_clause)?;
        write!(f, "}}")
    }
}

impl std::fmt::Display for CaseWhenClause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "when {} -> {}", self.guard, self.result)
    }
}

impl std::fmt::Display for CaseElseClause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "else -> {}", self.result)
    }
}

impl std::fmt::Display for CaseResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CaseResult::Block(block) => write!(f, "{}", block),
            CaseResult::Expression(expr) => write!(f, "{}", expr),
        }
    }
}

// === TYPE SYSTEM ===

/// Struct definition with optional generics and methods
#[derive(Debug, Clone, PartialEq)]
pub struct StructDefinition {
    pub attributes: Vec<Attribute>,
    pub name: TypeIdentifier,
    pub generic_params: Option<GenericParams>,
    pub fields: Vec<StructField>,
    pub methods: Vec<FunctionDefinition>,
    pub span: Span,
}

/// Struct field definition
#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: Identifier,
    pub type_annotation: TypeAnnotation,
    pub span: Span,
}

/// Trait definition with optional generics and constraints
#[derive(Debug, Clone, PartialEq)]
pub struct TraitDefinition {
    pub attributes: Vec<Attribute>,
    pub name: TypeIdentifier,
    pub generic_params: Option<GenericParams>,
    pub constraints: Option<ConstraintExpression>,
    pub functions: Vec<TraitFunction>,
    pub span: Span,
}

/// Trait function (signature or full definition)
#[derive(Debug, Clone, PartialEq)]
pub enum TraitFunction {
    Signature(FunctionSignature),
    Definition(FunctionDefinition),
}

/// Function signature without body
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSignature {
    pub visibility: FunctionVisibility,
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<TypeAnnotation>,
    pub guard: Option<GuardClause>,
    pub span: Span,
}

/// Implementation block
#[derive(Debug, Clone, PartialEq)]
pub struct ImplBlock {
    pub generic_params: Option<GenericParams>,
    pub trait_spec: TypeSpec,
    pub type_spec: TypeSpec,
    pub constraints: Option<ConstraintExpression>,
    pub methods: Vec<FunctionDefinition>,
    pub span: Span,
}

/// Generic parameters <T, U, V>
#[derive(Debug, Clone, PartialEq)]
pub struct GenericParams {
    pub params: Vec<GenericParam>,
    pub span: Span,
}

/// Generic parameter
#[derive(Debug, Clone, PartialEq)]
pub struct GenericParam {
    pub name: TypeIdentifier,
    pub span: Span,
}

/// Generic arguments <String, Integer>
#[derive(Debug, Clone, PartialEq)]
pub struct GenericArgs {
    pub args: Vec<TypeAnnotation>,
    pub span: Span,
}

/// Type specification for traits and implementations
#[derive(Debug, Clone, PartialEq)]
pub struct TypeSpec {
    pub path: Vec<TypeIdentifier>,
    pub generic_args: Option<GenericArgs>,
    pub span: Span,
}

/// Constraint expressions for traits and implementations
#[derive(Debug, Clone, PartialEq)]
pub enum ConstraintExpression {
    And {
        left: Box<ConstraintExpression>,
        right: Box<ConstraintExpression>,
        span: Span,
    },
    Constraint {
        type_param: TypeIdentifier,
        trait_bound: Vec<TypeIdentifier>,
        span: Span,
    },
    Parenthesized {
        expression: Box<ConstraintExpression>,
        span: Span,
    },
}

// Display implementations for type system

impl std::fmt::Display for StructDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Display attributes
        for attr in &self.attributes {
            writeln!(f, "{}", attr)?;
        }

        write!(f, "struct {}", self.name)?;
        if let Some(generics) = &self.generic_params {
            write!(f, "{}", generics)?;
        }
        write!(f, "(")?;
        for (i, field) in self.fields.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", field)?;
        }
        write!(f, ") {{")?;
        for method in &self.methods {
            write!(f, "\n    {}", method)?;
        }
        write!(f, "\n}}")
    }
}

impl std::fmt::Display for StructField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.type_annotation)
    }
}

impl std::fmt::Display for TraitDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Display attributes
        for attr in &self.attributes {
            writeln!(f, "{}", attr)?;
        }

        write!(f, "trait {}", self.name)?;
        if let Some(generics) = &self.generic_params {
            write!(f, "{}", generics)?;
        }
        if let Some(constraints) = &self.constraints {
            write!(f, " when {}", constraints)?;
        }
        write!(f, " {{")?;
        for function in &self.functions {
            write!(f, "\n    {}", function)?;
        }
        write!(f, "\n}}")
    }
}

impl std::fmt::Display for TraitFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TraitFunction::Signature(sig) => write!(f, "{}", sig),
            TraitFunction::Definition(def) => write!(f, "{}", def),
        }
    }
}

impl std::fmt::Display for FunctionSignature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.visibility, self.name)?;
        write!(f, "(")?;
        for (i, param) in self.parameters.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", param)?;
        }
        write!(f, ")")?;
        if let Some(return_type) = &self.return_type {
            write!(f, ": {}", return_type)?;
        }
        if let Some(guard) = &self.guard {
            write!(f, " {}", guard)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for ImplBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "impl")?;
        if let Some(generics) = &self.generic_params {
            write!(f, "{}", generics)?;
        }
        write!(f, " {} for {}", self.trait_spec, self.type_spec)?;
        if let Some(constraints) = &self.constraints {
            write!(f, " when {}", constraints)?;
        }
        write!(f, " {{")?;
        for method in &self.methods {
            write!(f, "\n    {}", method)?;
        }
        write!(f, "\n}}")
    }
}

impl std::fmt::Display for GenericParams {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<")?;
        for (i, param) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", param)?;
        }
        write!(f, ">")
    }
}

impl std::fmt::Display for GenericParam {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl std::fmt::Display for GenericArgs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<")?;
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", arg)?;
        }
        write!(f, ">")
    }
}

impl std::fmt::Display for TypeSpec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, part) in self.path.iter().enumerate() {
            if i > 0 {
                write!(f, ".")?;
            }
            write!(f, "{}", part)?;
        }
        if let Some(args) = &self.generic_args {
            write!(f, "{}", args)?;
        }
        Ok(())
    }
}

impl ConstraintExpression {
    pub fn span(&self) -> &Span {
        match self {
            ConstraintExpression::And { span, .. } => span,
            ConstraintExpression::Constraint { span, .. } => span,
            ConstraintExpression::Parenthesized { span, .. } => span,
        }
    }
}

impl std::fmt::Display for ConstraintExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstraintExpression::And { left, right, .. } => {
                write!(f, "{} && {}", left, right)
            }
            ConstraintExpression::Constraint {
                type_param,
                trait_bound,
                ..
            } => {
                write!(f, "{}: ", type_param)?;
                for (i, bound) in trait_bound.iter().enumerate() {
                    if i > 0 {
                        write!(f, ".")?;
                    }
                    write!(f, "{}", bound)?;
                }
                Ok(())
            }
            ConstraintExpression::Parenthesized { expression, .. } => {
                write!(f, "({})", expression)
            }
        }
    }
}

// === MODULE SYSTEM ===

/// Alias definition: alias Module.Path as Name
#[derive(Debug, Clone, PartialEq)]
pub struct AliasDefinition {
    pub path: AliasPath,
    pub alias_name: Option<TypeIdentifier>, // None means use default (last segment)
    pub span: Span,
}

/// Alias path variants
#[derive(Debug, Clone, PartialEq)]
pub enum AliasPath {
    Simple {
        path: Vec<TypeIdentifier>,
        span: Span,
    },
    BraceExpansion {
        base_path: Vec<TypeIdentifier>,
        items: Vec<AliasBraceItem>,
        span: Span,
    },
}

/// Item in brace expansion: TypeName or TypeName as Alias
#[derive(Debug, Clone, PartialEq)]
pub struct AliasBraceItem {
    pub name: TypeIdentifier,
    pub alias_name: Option<TypeIdentifier>,
    pub span: Span,
}

/// Import definition: import Module, only: [func: 1]
#[derive(Debug, Clone, PartialEq)]
pub struct ImportDefinition {
    pub path: Vec<TypeIdentifier>,
    pub clauses: Vec<ImportClause>,
    pub span: Span,
}

/// Import clause variants
#[derive(Debug, Clone, PartialEq)]
pub enum ImportClause {
    Only {
        functions: Vec<ImportFunctionSpec>,
        span: Span,
    },
    Except {
        functions: Vec<ImportFunctionSpec>,
        span: Span,
    },
}

/// Function specification in import: function_name: arity
#[derive(Debug, Clone, PartialEq)]
pub struct ImportFunctionSpec {
    pub name: Identifier,
    pub arity: i64,
    pub span: Span,
}

// Display implementations for module system

impl std::fmt::Display for AliasDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "alias {}", self.path)?;
        if let Some(alias_name) = &self.alias_name {
            write!(f, " as {}", alias_name)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for AliasPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AliasPath::Simple { path, .. } => {
                for (i, part) in path.iter().enumerate() {
                    if i > 0 {
                        write!(f, ".")?;
                    }
                    write!(f, "{}", part)?;
                }
                Ok(())
            }
            AliasPath::BraceExpansion {
                base_path, items, ..
            } => {
                for (i, part) in base_path.iter().enumerate() {
                    if i > 0 {
                        write!(f, ".")?;
                    }
                    write!(f, "{}", part)?;
                }
                write!(f, ".{{")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", item)?;
                }
                write!(f, "}}")
            }
        }
    }
}

impl std::fmt::Display for AliasBraceItem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)?;
        if let Some(alias_name) = &self.alias_name {
            write!(f, " as {}", alias_name)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for ImportDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "import ")?;
        for (i, part) in self.path.iter().enumerate() {
            if i > 0 {
                write!(f, ".")?;
            }
            write!(f, "{}", part)?;
        }
        for clause in &self.clauses {
            write!(f, ", ")?;
            write!(f, "{}", clause)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for ImportClause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ImportClause::Only { functions, .. } => {
                write!(f, "only: [")?;
                for (i, func) in functions.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", func)?;
                }
                write!(f, "]")
            }
            ImportClause::Except { functions, .. } => {
                write!(f, "except: [")?;
                for (i, func) in functions.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", func)?;
                }
                write!(f, "]")
            }
        }
    }
}

impl std::fmt::Display for ImportFunctionSpec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.arity)
    }
}

// === MACROS ===

/// Macro definition with parameters and body
#[derive(Debug, Clone, PartialEq)]
pub struct MacroDefinition {
    pub name: Identifier,
    pub parameters: Vec<Identifier>,
    pub body: Block,
    pub span: Span,
}

/// Macro injection expression: ^parameter
#[derive(Debug, Clone, PartialEq)]
pub struct MacroInjection {
    pub parameter: Identifier,
    pub span: Span,
}

/// Anonymous function: fn { x: Integer -> x + 1 }
#[derive(Debug, Clone, PartialEq)]
pub struct AnonymousFunction {
    pub clauses: Vec<AnonymousClause>,
    pub span: Span,
}

/// Single clause of an anonymous function
#[derive(Debug, Clone, PartialEq)]
pub struct AnonymousClause {
    pub parameters: AnonymousParameters,
    pub guard: Option<Expression>,
    pub body: AnonymousBody,
    pub span: Span,
}

/// Parameter patterns for anonymous functions
#[derive(Debug, Clone, PartialEq)]
pub enum AnonymousParameters {
    /// No parameters: ()
    None { span: Span },
    /// Single parameter: x: Type
    Single {
        parameter: Box<Parameter>,
        span: Span,
    },
    /// Multiple parameters: (x: Type1, y: Type2)
    Multiple {
        parameters: Vec<Parameter>,
        span: Span,
    },
}

/// Body of an anonymous function clause
#[derive(Debug, Clone, PartialEq)]
pub enum AnonymousBody {
    /// Single expression
    Expression(Box<Expression>),
    /// Block with statements
    Block(Block),
}

/// Function capture: &function or &Module.function/2
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCapture {
    pub module_path: Option<Vec<TypeIdentifier>>,
    pub function_name: Identifier,
    pub arity: Option<i32>,
    pub span: Span,
}

// Display implementations for macros

impl std::fmt::Display for MacroDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "macro {}(", self.name)?;
        for (i, param) in self.parameters.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", param)?;
        }
        write!(f, ") {}", self.body)
    }
}

impl std::fmt::Display for MacroInjection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "^{}", self.parameter)
    }
}

// Display implementations for anonymous functions

impl std::fmt::Display for AnonymousFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {{ ")?;
        for (i, clause) in self.clauses.iter().enumerate() {
            if i > 0 {
                write!(f, " ")?; // Separate multiple clauses
            }
            write!(f, "{}", clause)?;
        }
        write!(f, " }}")
    }
}

impl std::fmt::Display for AnonymousClause {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.parameters)?;
        if let Some(guard) = &self.guard {
            write!(f, " when {}", guard)?;
        }
        write!(f, " -> {}", self.body)
    }
}

impl std::fmt::Display for AnonymousParameters {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnonymousParameters::None { .. } => write!(f, "()"),
            AnonymousParameters::Single { parameter, .. } => {
                write!(f, "{}", parameter)
            }
            AnonymousParameters::Multiple { parameters, .. } => {
                write!(f, "(")?;
                for (i, param) in parameters.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl std::fmt::Display for AnonymousBody {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AnonymousBody::Expression(expr) => write!(f, "{}", expr),
            AnonymousBody::Block(block) => write!(f, "{}", block),
        }
    }
}

impl std::fmt::Display for FunctionCapture {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "&")?;
        if let Some(module_path) = &self.module_path {
            for (i, part) in module_path.iter().enumerate() {
                if i > 0 {
                    write!(f, ".")?;
                }
                write!(f, "{}", part)?;
            }
            write!(f, ".")?;
        }
        write!(f, "{}", self.function_name)?;
        if let Some(arity) = self.arity {
            write!(f, "/{}", arity)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for Attribute {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "@{}", self.name)?;
        if let Some(args) = &self.args {
            write!(f, "({})", args)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for AttributeArgs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, arg) in self.arguments.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", arg)?;
        }
        Ok(())
    }
}
