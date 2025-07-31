use std::{collections::VecDeque, ops::Deref, path::Path};

use tree_sitter::TreeCursor;

pub struct ParseTree<'a> {
    pub tree: tree_sitter::Tree,
    pub content: &'a [u8],
    pub path: &'a Path,
}

#[derive(Clone, Copy)]
pub struct ParseNode<'a, 'b> {
    pub node: tree_sitter::Node<'b>,
    pub kind: NodeKind,
    pub content: &'a [u8],
    pub path: &'a Path,
}

impl<'a> ParseTree<'a> {
    pub fn new(
        parser: &mut tree_sitter::Parser,
        content: &'a [u8],
        path: &'a Path,
    ) -> Option<ParseTree<'a>> {
        let tree = parser.parse(content, None);
        tree.map(|tree| ParseTree {
            tree,
            content,
            path,
        })
    }

    pub fn root_node(&self) -> ParseNode {
        ParseNode {
            node: self.tree.root_node(),
            kind: kind_to_enum(self.tree.root_node().kind()),
            content: self.content,
            path: self.path,
        }
    }
}

impl<'a, 'b> std::fmt::Display for ParseNode<'a, 'b> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

impl<'a, 'b> std::fmt::Debug for ParseNode<'a, 'b> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ParseNode(kind: {}, content: {})", self.kind(), self)
    }
}

impl<'a, 'tree> Deref for ParseNode<'a, 'tree> {
    type Target = tree_sitter::Node<'tree>;
    fn deref(&self) -> &Self::Target {
        &self.node
    }
}

impl<'a, 'tree> ParseNode<'a, 'tree> {
    pub fn children(self) -> impl ExactSizeIterator<Item = ParseNode<'a, 'tree>> {
        let mut cursor: TreeCursor<'tree> = self.node.walk();
        cursor.reset(self.node);
        cursor.goto_first_child();
        (0..self.node.child_count()).map(move |_| {
            let result = ParseNode {
                node: cursor.node(),
                kind: kind_to_enum(cursor.node().kind()),
                content: self.content,
                path: self.path,
            };
            cursor.goto_next_sibling();
            result
        })
    }

    pub fn get_by_kind(self, kind: NodeKind) -> Option<ParseNode<'a, 'tree>> {
        self.children().find(|child| child.kind == kind)
    }

    /// Perform a DFS on the node (starting with the node itself!) to find the first node with the kind provided.
    pub fn extract_by_kind(self, kind: NodeKind) -> Option<ParseNode<'a, 'tree>> {
        let mut visit_stack = VecDeque::new();
        visit_stack.push_back(self);
        while let Some(current) = visit_stack.pop_back() {
            if current.kind == kind {
                return Some(current);
            }
            for child in current.children() {
                visit_stack.push_front(child);
            }
        }
        None
    }

    pub fn child(self, idx: usize) -> Option<ParseNode<'a, 'tree>> {
        self.node.child(idx).map(|node| ParseNode {
            node,
            kind: kind_to_enum(node.kind()),
            content: self.content,
            path: self.path,
        })
    }

    pub fn new(node: tree_sitter::Node<'tree>, content: &'a [u8], path: &'a Path) -> Self {
        ParseNode {
            node,
            content,
            kind: kind_to_enum(node.kind()),
            path,
        }
    }

    pub fn start_byte(&self) -> usize {
        self.node.start_byte()
    }

    pub fn end_byte(&self) -> usize {
        self.node.end_byte()
    }

    pub fn kind(&self) -> &'static str {
        self.node.kind()
    }
    pub fn to_u8(&self) -> &'a [u8] {
        &self.content[self.start_byte()..self.end_byte()]
    }
    pub fn to_str(&self) -> String {
        String::from_utf8_lossy(self.to_u8()).to_string()
    }
    pub fn to_str_slice(&self) -> &'a str {
        std::str::from_utf8(self.to_u8()).expect("Invalid UTF-8 in ParseNode")
    }
    pub fn dbg_str(&self) -> String {
        self.dbg_indented(0)
    }

    pub fn loc(&self) -> String {
        let pos = self.node.start_position();
        format!("{}:{}:{}", self.path.display(), pos.row + 1, pos.column + 1)
    }

    fn dbg_indented(&self, indent: usize) -> String {
        let indent_str = "\t".repeat(indent);
        let kind = self.node.kind();
        let children = self.children();
        let has_children = self.child_count() != 0;
        if !has_children {
            return format!(
                "{}{} \"{}\"",
                indent_str,
                kind,
                String::from_utf8_lossy(self.to_u8())
            );
        }
        let children = children
            .map(|c| c.dbg_indented(indent + 1))
            .collect::<Vec<_>>()
            .join("\n");
        format!("{}{}{}{}", indent_str, kind, "\n", children)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum NodeKind {
    AccentQuoted,
    ArgumentList,
    ArrayConstruction,
    AssemblyStatement,
    Assignment,
    BindStatement,
    BlankIdentifier,
    Block,
    BlockComment,
    BlockDocumentationComment,
    BracketExpression,
    BreakStatement,
    Call,
    Case,
    Cast,
    CharLiteral,
    ColonExpression,
    Comment,
    CommentContent,
    ConceptDeclaration,
    ConditionalDeclaration,
    ConstSection,
    ContinueStatement,
    ConverterDeclaration,
    CurlyConstruction,
    CurlyExpression,
    CustomNumericLiteral,
    Defer,
    DiscardStatement,
    DistinctType,
    DoBlock,
    DocumentationComment,
    DotExpression,
    DotGenericCall,
    ElifBranch,
    ElseBranch,
    EnumDeclaration,
    EnumFieldDeclaration,
    EnumType,
    EqualExpression,
    EscapeSequence,
    ExceptBranch,
    ExceptClause,
    ExportStatement,
    ExportedSymbol,
    ExpressionList,
    FieldDeclaration,
    FieldDeclarationList,
    FinallyBranch,
    FloatLiteral,
    For,
    FuncDeclaration,
    FuncExpression,
    GeneralizedString,
    GenericArgumentList,
    GenericParameterList,
    Identifier,
    If,
    ImportFromStatement,
    ImportStatement,
    IncludeStatement,
    InfixExpression,
    IntegerLiteral,
    InterpretedStringLiteral,
    IteratorDeclaration,
    IteratorExpression,
    IteratorType,
    LetSection,
    LongStringLiteral,
    MacroDeclaration,
    MethodDeclaration,
    MixinStatement,
    NilLiteral,
    ObjectDeclaration,
    ObjectType,
    OfBranch,
    Operator,
    OutType,
    ParameterDeclaration,
    ParameterDeclarationList,
    ParameterList,
    Parenthesized,
    PointerParameter,
    PointerType,
    PragmaExpression,
    PragmaList,
    PragmaStatement,
    PrefixExpression,
    ProcDeclaration,
    ProcExpression,
    ProcType,
    RaiseStatement,
    RawStringLiteral,
    RefParameter,
    RefType,
    RefinementList,
    ReturnStatement,
    SourceFile,
    StatementList,
    StaticParameter,
    StaticStatement,
    StringContent,
    SymbolDeclaration,
    SymbolDeclarationList,
    TemplateDeclaration,
    TermRewritingPattern,
    Try,
    TupleConstruction,
    TupleDeconstructDeclaration,
    TupleType,
    TypeDeclaration,
    TypeExpression,
    TypeParameter,
    TypeSection,
    TypeSymbolDeclaration,
    Typeof,
    UsingSection,
    VarParameter,
    VarSection,
    VarType,
    VariableDeclaration,
    VariantDeclaration,
    VariantDiscriminatorDeclaration,
    When,
    While,
    YieldStatement,
    And,
    As,
    Asm,
    Bind,
    Break,
    Concept,
    Const,
    Continue,
    Converter,
    Discard,
    Distinct,
    Div,
    Do,
    Elif,
    Else,
    Enum,
    Except,
    Export,
    Finally,
    From,
    Func,
    Import,
    In,
    Include,
    Is,
    Isnot,
    Iterator,
    Let,
    Macro,
    Method,
    Mixin,
    Mod,
    Nil,
    Not,
    Notin,
    Object,
    Of,
    Or,
    Out,
    Proc,
    Ptr,
    Raise,
    Ref,
    Return,
    Shl,
    Shr,
    Static,
    Template,
    Tuple,
    Type,
    Using,
    Var,
    Xor,
    Yield,
    DoubleQuote,
    DoubleQuoteDoubleQuote,
    DoubleQuoteDoubleQuoteDoubleQuote,
    Hash,
    HashHash,
    HashHashLBracket,
    HashLBracket,
    Quote,
    LParen,
    RParen,
    Mul,
    Comma,
    SubGt,
    Dot,
    DotRBrace,
    Colon,
    Semicolon,
    Eq,
    RDoubleQuote,
    RDoubleQuoteDoubleQuoteDoubleQuote,
    LBracket,
    LBracketColon,
    RBracket,
    RBracketHash,
    RBracketHashHash,
    Backtick,
    RDoubleQuote_,
    RDoubleQuoteDoubleQuoteDoubleQuote_,
    LBrace,
    LBraceDot,
    RBrace,
    Unknown,
}

#[allow(unreachable_code)]
pub fn kind_to_enum(kind: &str) -> NodeKind {
    match kind {
        "accent_quoted" => NodeKind::AccentQuoted,
        "argument_list" => NodeKind::ArgumentList,
        "array_construction" => NodeKind::ArrayConstruction,
        "assembly_statement" => NodeKind::AssemblyStatement,
        "assignment" => NodeKind::Assignment,
        "bind_statement" => NodeKind::BindStatement,
        "blank_identifier" => NodeKind::BlankIdentifier,
        "block" => NodeKind::Block,
        "block_comment" => NodeKind::BlockComment,
        "block_documentation_comment" => NodeKind::BlockDocumentationComment,
        "bracket_expression" => NodeKind::BracketExpression,
        "break_statement" => NodeKind::BreakStatement,
        "call" => NodeKind::Call,
        "case" => NodeKind::Case,
        "cast" => NodeKind::Cast,
        "char_literal" => NodeKind::CharLiteral,
        "colon_expression" => NodeKind::ColonExpression,
        "comment" => NodeKind::Comment,
        "comment_content" => NodeKind::CommentContent,
        "concept_declaration" => NodeKind::ConceptDeclaration,
        "conditional_declaration" => NodeKind::ConditionalDeclaration,
        "const_section" => NodeKind::ConstSection,
        "continue_statement" => NodeKind::ContinueStatement,
        "converter_declaration" => NodeKind::ConverterDeclaration,
        "curly_construction" => NodeKind::CurlyConstruction,
        "curly_expression" => NodeKind::CurlyExpression,
        "custom_numeric_literal" => NodeKind::CustomNumericLiteral,
        "defer" => NodeKind::Defer,
        "discard_statement" => NodeKind::DiscardStatement,
        "distinct_type" => NodeKind::DistinctType,
        "do_block" => NodeKind::DoBlock,
        "documentation_comment" => NodeKind::DocumentationComment,
        "dot_expression" => NodeKind::DotExpression,
        "dot_generic_call" => NodeKind::DotGenericCall,
        "elif_branch" => NodeKind::ElifBranch,
        "else_branch" => NodeKind::ElseBranch,
        "enum_declaration" => NodeKind::EnumDeclaration,
        "enum_field_declaration" => NodeKind::EnumFieldDeclaration,
        "enum_type" => NodeKind::EnumType,
        "equal_expression" => NodeKind::EqualExpression,
        "escape_sequence" => NodeKind::EscapeSequence,
        "except_branch" => NodeKind::ExceptBranch,
        "except_clause" => NodeKind::ExceptClause,
        "export_statement" => NodeKind::ExportStatement,
        "exported_symbol" => NodeKind::ExportedSymbol,
        "expression_list" => NodeKind::ExpressionList,
        "field_declaration" => NodeKind::FieldDeclaration,
        "field_declaration_list" => NodeKind::FieldDeclarationList,
        "finally_branch" => NodeKind::FinallyBranch,
        "float_literal" => NodeKind::FloatLiteral,
        "for" => NodeKind::For,
        "func_declaration" => NodeKind::FuncDeclaration,
        "func_expression" => NodeKind::FuncExpression,
        "generalized_string" => NodeKind::GeneralizedString,
        "generic_argument_list" => NodeKind::GenericArgumentList,
        "generic_parameter_list" => NodeKind::GenericParameterList,
        "identifier" => NodeKind::Identifier,
        "if" => NodeKind::If,
        "import_from_statement" => NodeKind::ImportFromStatement,
        "import_statement" => NodeKind::ImportStatement,
        "include_statement" => NodeKind::IncludeStatement,
        "infix_expression" => NodeKind::InfixExpression,
        "integer_literal" => NodeKind::IntegerLiteral,
        "interpreted_string_literal" => NodeKind::InterpretedStringLiteral,
        "iterator_declaration" => NodeKind::IteratorDeclaration,
        "iterator_expression" => NodeKind::IteratorExpression,
        "iterator_type" => NodeKind::IteratorType,
        "let_section" => NodeKind::LetSection,
        "long_string_literal" => NodeKind::LongStringLiteral,
        "macro_declaration" => NodeKind::MacroDeclaration,
        "method_declaration" => NodeKind::MethodDeclaration,
        "mixin_statement" => NodeKind::MixinStatement,
        "nil_literal" => NodeKind::NilLiteral,
        "object_declaration" => NodeKind::ObjectDeclaration,
        "object_type" => NodeKind::ObjectType,
        "of_branch" => NodeKind::OfBranch,
        "operator" => NodeKind::Operator,
        "out_type" => NodeKind::OutType,
        "parameter_declaration" => NodeKind::ParameterDeclaration,
        "parameter_declaration_list" => NodeKind::ParameterDeclarationList,
        "parameter_list" => NodeKind::ParameterList,
        "parenthesized" => NodeKind::Parenthesized,
        "pointer_parameter" => NodeKind::PointerParameter,
        "pointer_type" => NodeKind::PointerType,
        "pragma_expression" => NodeKind::PragmaExpression,
        "pragma_list" => NodeKind::PragmaList,
        "pragma_statement" => NodeKind::PragmaStatement,
        "prefix_expression" => NodeKind::PrefixExpression,
        "proc_declaration" => NodeKind::ProcDeclaration,
        "proc_expression" => NodeKind::ProcExpression,
        "proc_type" => NodeKind::ProcType,
        "raise_statement" => NodeKind::RaiseStatement,
        "raw_string_literal" => NodeKind::RawStringLiteral,
        "ref_parameter" => NodeKind::RefParameter,
        "ref_type" => NodeKind::RefType,
        "refinement_list" => NodeKind::RefinementList,
        "return_statement" => NodeKind::ReturnStatement,
        "source_file" => NodeKind::SourceFile,
        "statement_list" => NodeKind::StatementList,
        "static_parameter" => NodeKind::StaticParameter,
        "static_statement" => NodeKind::StaticStatement,
        "string_content" => NodeKind::StringContent,
        "symbol_declaration" => NodeKind::SymbolDeclaration,
        "symbol_declaration_list" => NodeKind::SymbolDeclarationList,
        "template_declaration" => NodeKind::TemplateDeclaration,
        "term_rewriting_pattern" => NodeKind::TermRewritingPattern,
        "try" => NodeKind::Try,
        "tuple_construction" => NodeKind::TupleConstruction,
        "tuple_deconstruct_declaration" => NodeKind::TupleDeconstructDeclaration,
        "tuple_type" => NodeKind::TupleType,
        "type_declaration" => NodeKind::TypeDeclaration,
        "type_expression" => NodeKind::TypeExpression,
        "type_parameter" => NodeKind::TypeParameter,
        "type_section" => NodeKind::TypeSection,
        "type_symbol_declaration" => NodeKind::TypeSymbolDeclaration,
        "typeof" => NodeKind::Typeof,
        "using_section" => NodeKind::UsingSection,
        "var_parameter" => NodeKind::VarParameter,
        "var_section" => NodeKind::VarSection,
        "var_type" => NodeKind::VarType,
        "variable_declaration" => NodeKind::VariableDeclaration,
        "variant_declaration" => NodeKind::VariantDeclaration,
        "variant_discriminator_declaration" => NodeKind::VariantDiscriminatorDeclaration,
        "when" => NodeKind::When,
        "while" => NodeKind::While,
        "yield_statement" => NodeKind::YieldStatement,
        "and" => NodeKind::And,
        "as" => NodeKind::As,
        "asm" => NodeKind::Asm,
        "bind" => NodeKind::Bind,
        "break" => NodeKind::Break,
        "concept" => NodeKind::Concept,
        "const" => NodeKind::Const,
        "continue" => NodeKind::Continue,
        "converter" => NodeKind::Converter,
        "discard" => NodeKind::Discard,
        "distinct" => NodeKind::Distinct,
        "div" => NodeKind::Div,
        "do" => NodeKind::Do,
        "elif" => NodeKind::Elif,
        "else" => NodeKind::Else,
        "enum" => NodeKind::Enum,
        "except" => NodeKind::Except,
        "export" => NodeKind::Export,
        "finally" => NodeKind::Finally,
        "from" => NodeKind::From,
        "func" => NodeKind::Func,
        "import" => NodeKind::Import,
        "in" => NodeKind::In,
        "include" => NodeKind::Include,
        "is" => NodeKind::Is,
        "isnot" => NodeKind::Isnot,
        "iterator" => NodeKind::Iterator,
        "let" => NodeKind::Let,
        "macro" => NodeKind::Macro,
        "method" => NodeKind::Method,
        "mixin" => NodeKind::Mixin,
        "mod" => NodeKind::Mod,
        "nil" => NodeKind::Nil,
        "not" => NodeKind::Not,
        "notin" => NodeKind::Notin,
        "object" => NodeKind::Object,
        "of" => NodeKind::Of,
        "or" => NodeKind::Or,
        "out" => NodeKind::Out,
        "proc" => NodeKind::Proc,
        "ptr" => NodeKind::Ptr,
        "raise" => NodeKind::Raise,
        "ref" => NodeKind::Ref,
        "return" => NodeKind::Return,
        "shl" => NodeKind::Shl,
        "shr" => NodeKind::Shr,
        "static" => NodeKind::Static,
        "template" => NodeKind::Template,
        "tuple" => NodeKind::Tuple,
        "type" => NodeKind::Type,
        "using" => NodeKind::Using,
        "var" => NodeKind::Var,
        "xor" => NodeKind::Xor,
        "yield" => NodeKind::Yield,
        "=" => NodeKind::Eq,
        "(" => NodeKind::LParen,
        ")" => NodeKind::RParen,
        "," => NodeKind::Comma,
        "[" => NodeKind::LBracket,
        "]" => NodeKind::RBracket,
        "*" => NodeKind::Mul,
        ":" => NodeKind::Colon,
        ";" => NodeKind::Semicolon,
        "{." => NodeKind::LBraceDot, // pragmas
        ".}" => NodeKind::DotRBrace,
        _ => {
            panic!("Unknown node kind: {kind}");
            NodeKind::Unknown
        }
    }
}
