// ===================================================
// This file is part of the Kulfon compiler.
// Author: Rafał Równiak
// License: Read LICENSE file
// Created on: 15.06.2024
// ---------------------------------------------------
use crate::comp_msg::TextPoint;

// tree structure will be represented as a flast AST
// https://www.cs.cornell.edu/~asampson/blog/flattening.html

/// Index type referencing elements in the [`Tree`] AST structure.
///
/// `NodeRef` acts as a stable handle to nodes in the flat tree layout.
/// Internally, it stores an index as `i32`, but most operations use `usize`.
#[derive(Copy, Clone, Debug)]
pub struct NodeRef(i32);

impl NodeRef {
    /// Converts the inner index value to a `usize`.
    ///
    /// This is commonly used to index into the `Tree::flat` vector.
    pub fn as_usize(self) -> usize {
        self.0 as usize
    }
}

/// A collection of [`NodeRef`]s representing multiple AST nodes (e.g., expressions or statements).
pub type NodeRefs = Vec<NodeRef>;

/// Function definition in the AST.
///
/// Represents both user-defined and possibly generated functions.
#[derive(Debug, Clone)]
pub struct Fun {
    /// Function name.
    pub name: String,
    /// Function arguments.
    pub args: Vec<VarDecl>,
    /// Indicates whether the function accepts a variadic number of arguments.
    pub variadic: bool,
    /// Declared return type.
    pub ret: TypeDecl,
    /// Reference to the function body (a [`NodeRef`] pointing to a [`Scope`] node).
    pub body: NodeRef,
}

/// Represents reference qualifiers (borrow or mutable borrow).
#[derive(Debug, Clone)]
pub enum RefType {
    /// Shared reference (`&T`)
    Borrow,
    /// Mutable reference (`&mut T`)
    BorrowMut,
}

/// Represents the core structure of a type declaration.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum TypeKind {
    /// An array with a size expression (e.g., `[T; 3]`)
    Array(Box<TypeDecl>, Option<NodeRef>),
    /// A slice type (e.g., `[T]`)
    Slice(Box<TypeDecl>),
    /// A simple named type (e.g., `int`, `String`)
    JustName(String),
    /// A generic type with type parameters (e.g., `Vec<T>`)
    JustNameGeneric(String, Vec<TypeDecl>),
}

/// Complete type declaration, including optional referencing and scope resolution.
///
/// Used for variables, function signatures, and type annotations.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct TypeDecl {
    /// Stack of reference modifiers (`&`, `&mut`)
    pub reference_stack: Vec<RefType>,
    /// Whether the type is relative to current scope (e.g., not fully qualified i.e.
    /// ::VariableType).
    pub scope_res_relative: bool,
    /// Components of the scoped path (e.g., `std::vec::Vec` → `["std", "vec", "Vec"]`)
    pub scope_resolution: Vec<String>,
    /// The actual type description (name, generics, etc.)
    pub type_id: TypeKind,
}

impl TypeDecl {
    /// Creates a new simple type declaration by name.
    pub fn new(typename: String) -> Self {
        Self {
            reference_stack: Vec::new(),
            scope_res_relative: true,
            scope_resolution: Vec::new(),
            type_id: TypeKind::JustName(typename),
        }
    }

    /// Returns the base typename if the type is a simple name.
    ///
    /// Returns `None` if the type is an array, slice, or generic.
    pub fn typename(&self) -> Option<String> {
        match &self.type_id {
            TypeKind::JustName(n) => Some(n.clone()),
            _ => None,
        }
    }
}

/// Represents a variable declaration.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct VarDecl {
    /// Variable name.
    pub name: String,
    /// Associated type declaration.
    pub type_dec: TypeDecl,
}

/// A single `else if` branch in an [`If`] construct.
#[derive(Debug, Clone)]
pub struct Elif {
    /// Condition to be evaluated.
    pub cond: NodeRef,
    /// Body to execute if condition is true.
    pub body: NodeRef,
}

/// Conditional branching expression (if/else if/else).
#[derive(Debug, Clone)]
pub struct If {
    /// Main condition.
    pub cond: NodeRef,
    /// Body for the main `if`.
    pub body: NodeRef,
    /// List of `else if` branches.
    pub elif: Vec<Elif>,
    /// Optional `else` block.
    pub else_body: Option<NodeRef>,
}

/// For-loop construct with a pattern binding.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct For {
    /// Variable pattern (e.g., `i`, or tuple destructure).
    pub var_pattern: String,
    /// Expression representing the iterable.
    pub in_expr: NodeRef,
    /// Loop body.
    pub body: NodeRef,
}

/// While-loop construct.
#[derive(Debug, Clone)]
pub struct While {
    /// Loop condition.
    pub cond: NodeRef,
    /// Loop body.
    pub body: NodeRef,
}

/// Infinite loop construct.
#[derive(Debug, Clone)]
pub struct Loop {
    /// Body of the loop.
    pub body: NodeRef,
}

/// Definition of a variable (mutable or constant).
#[derive(Debug, Clone)]
pub struct VarDef {
    /// Whether the variable is mutable.
    pub mutable: bool,
    /// Variable name.
    pub name: String,
    /// Optional type annotation.
    pub vartype: Option<TypeDecl>,
    /// Optional initializer expression.
    pub expr: Option<NodeRef>,
}

/// Definition of a struct.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct Struct {
    /// Struct name.
    pub name: String,
    /// Generic type parameters.
    pub generic_args: Vec<TypeDecl>,
    /// Fields (members) of the struct.
    pub members: Vec<VarDecl>,
}

/// Definition of an enum.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct Enum {
    /// Enum name.
    pub name: String,
    /// Generic parameters.
    pub generic_args: Vec<TypeDecl>,
    /// Variants of the enum.
    ///
    /// Each variant is a pair of a name and an optional payload type.
    pub enums: Vec<(String, Option<TypeDecl>)>,
}

/// Implementation block for a type.
#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct Impl {
    /// Name of the type being implemented.
    pub name: String,
    /// Scope containing methods or associated items.
    pub scope: NodeRefs,
}

/// An abstract syntax tree (AST) for the Kulfon language, organized as a flat vector.
///
/// The `Tree` structure holds all parsed nodes in a single flat array,
/// and references between them are made using [`NodeRef`] indices.
/// This simplifies memory management and enables efficient traversal.
///
/// # Fields
///
/// - `root`: The entry point of the AST (typically the `main` function or top-level scope).
/// - `flat`: The collection of all AST nodes created during parsing.
#[derive(Debug, Clone)]
pub struct Tree {
    /// Reference to the root node in the AST.
    pub root: NodeRef,
    /// Flat list of all nodes that make up the AST.
    pub flat: Vec<Node>,
}

impl Tree {
    /// Creates a new `Tree` with preallocated capacity.
    ///
    /// # Arguments
    /// - `cap`: Estimated number of nodes to reserve space for.
    pub fn new(cap: usize) -> Self {
        Self {
            // kinda dodgy as this is no valid Id so far
            // but it'll be valid in the most cases
            root: NodeRef(0),
            flat: Vec::with_capacity(cap),
        }
    }

    /// Appends a new node to the AST and returns a reference to it.
    ///
    /// # Arguments
    /// - `node`: The node to insert into the tree.
    ///
    /// # Returns
    /// - A [`NodeRef`] referencing the inserted node.
    pub fn push(&mut self, node: Node) -> NodeRef {
        let nref = NodeRef(self.flat.len() as i32);
        self.flat.push(node);
        nref
    }

    /// Retrieves a node by its reference.
    ///
    /// # Arguments
    /// - `nref`: A reference to the node.
    ///
    /// # Panics
    /// Panics if the index is out of bounds.
    pub fn get(&self, nref: NodeRef) -> &Node {
        &self.flat[nref.0 as usize]
    }
}

/// A single node in the abstract syntax tree.
///
/// Each node holds a value of variant [`Ntype`] representing the language construct,
/// along with a [`TextPoint`] indicating its location in the source code.
#[derive(Debug, Clone)]
pub struct Node {
    /// The syntactic value of this node (e.g., expression, statement, declaration).
    pub val: Ntype,
    /// The source location where this node was parsed from.
    pub at: TextPoint,
    // pub meta_idx: Option<usize>,
}

impl Node {
    /// Constructs a new AST node.
    ///
    /// # Arguments
    /// - `val`: The node’s kind (AST construct).
    /// - `at`: The source location of this node.
    pub fn new(val: Ntype, at: TextPoint) -> Self {
        Node {
            val,
            at,
            // meta_idx: None,
        }
    }
}

/// Enum representing all possible node types in the Kulfon AST.
///
/// This includes expressions, statements, control flow constructs,
/// declarations, literals, and structural elements.
#[derive(Debug, Clone)]
pub enum Ntype {
    // === Binary Operators ===
    Eq(NodeRef, NodeRef),
    Neq(NodeRef, NodeRef),
    Gt(NodeRef, NodeRef),
    Ge(NodeRef, NodeRef),
    Lt(NodeRef, NodeRef),
    Le(NodeRef, NodeRef),
    Plus(NodeRef, NodeRef),
    Minus(NodeRef, NodeRef),
    Slash(NodeRef, NodeRef),
    Star(NodeRef, NodeRef),
    // === Unary Operators ===
    Bang(NodeRef),
    UMinus(NodeRef), // unary minus
    // === Assignment ===
    Assign(String, NodeRef),
    // === Control Flow ===
    If(If),
    For(For),
    While(While),
    Loop(Loop),
    Break,
    Continue,
    // === Declarations ===
    Struct(Struct),
    #[allow(dead_code)]
    Enum(Enum),
    Impl(Impl),
    Scope(NodeRefs),
    FnDef(Fun),
    // === Expressions ===
    FnCall(String, NodeRefs),
    StructInit(String, Vec<(String, NodeRef)>),
    EnumInit(String, String, Option<NodeRef>),
    VarDef(VarDef),
    // === Terminals ===
    String(String),
    Literal(String),
    Char(String),
    True,
    False,
}
