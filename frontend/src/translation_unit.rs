use std::collections::HashMap;

use crate::ast::AST;
use crate::typesystem::TypeDecl;

pub type TypeAliasMap = HashMap<String, TypeDecl>;
pub type TranslationUnit = (AST, TypeAliasMap);
