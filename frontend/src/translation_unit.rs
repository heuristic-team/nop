use std::collections::HashMap;
use std::rc::Rc;

use crate::ast::AST;
use crate::typesystem::{Type, TypeDecl};

pub type TranslationUnit = (AST, TypeDeclMap);

/// Map with all the type declarations, such as type aliases and structs.
pub struct TypeDeclMap {
    repr: HashMap<String, TypeDecl>,
}

impl TypeDeclMap {
    /// Create a new empty `TypeDeclMap`.
    pub fn new() -> Self {
        Self {
            repr: HashMap::new(),
        }
    }

    /// Check if the map contains type declaration with given name.
    pub fn contains(&self, name: &str) -> bool {
        self.repr.contains_key(name)
    }

    /// Try and find type declaration by its name.
    pub fn get_decl(&self, name: &str) -> Option<&TypeDecl> {
        self.repr.get(name)
    }

    /// Insert a new type declaration into the map.
    pub fn insert_decl(&mut self, decl: TypeDecl) {
        self.repr.insert(decl.name.value.clone(), decl);
    }

    /// Get an iterator over all declarations in the map.
    pub fn decls(&self) -> impl Iterator<Item = &TypeDecl> {
        self.repr.values()
    }

    /// Unwrap any type alias chains (even empty ones) until we get a concrete type.
    ///
    /// IMPORTANT: this method panics if provided with an invalid alias.
    /// We expect that all aliases are checked by the respective pass before this functionality is used.
    pub fn unalias_type<'a>(&'a self, tp: &'a Type) -> &'a Type {
        let mut cur = tp;
        while let Type::Alias(name) = cur {
            let typedecl = self
                .repr
                .get(name)
                .expect("expected valid type alias, see `TypeNameCorrectnessCheck` pass");

            cur = typedecl.value.value.as_ref();
        }

        cur
    }

    /// Unwrap any type alias chains (even empty ones) until we get a concrete type and return it in `Rc`.
    ///
    /// IMPORTANT: this method panics if provided with an invalid alias.
    /// We expect that all aliases are checked by the respective pass before this functionality is used.
    pub fn unalias_type_rc(&self, tp: Rc<Type>) -> Rc<Type> {
        let mut cur = tp;
        while let Type::Alias(name) = cur.as_ref() {
            let typedecl = self
                .repr
                .get(name)
                .expect("expected valid type alias, see `TypeNameCorrectnessCheck` pass");

            cur = typedecl.value.value.clone();
        }
        cur
    }
}
