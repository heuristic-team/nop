mod type_decl_map;
pub use type_decl_map::{TypeAliasChainIterator, TypeDeclMap};

use crate::ast::AST;

pub type TranslationUnit = (AST, TypeDeclMap);
