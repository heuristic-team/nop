use std::{collections::HashMap, rc::Rc};

use crate::typesystem::{Type, TypeDecl};

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

    /// Get an iterator over immurable references to all declarations in the map.
    pub fn decls(&self) -> impl Iterator<Item = &TypeDecl> {
        self.repr.values()
    }

    /// Get an iterator over mutable references to all declarations in the map.
    pub fn decls_mut(&mut self) -> impl Iterator<Item = &mut TypeDecl> {
        self.repr.values_mut()
    }

    /// Get a type alias chain iterator in the context of given map and starting from `tp`.
    ///
    /// For more information, see `TypeAliasChainIterator`.
    pub fn get_type_alias_chain<'a>(&'a self, tp: &'a Type) -> TypeAliasChainIterator<'a> {
        TypeAliasChainIterator {
            map: self,
            cur_type: Some(tp),
        }
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

/// Iterator over chain of type aliases.
///
/// For example, given that `Foo = Bar`, `Bar = Baz`, `Baz = bool`, iterator will produce `Foo` -> `Bar` -> `Baz` -> `bool`.
pub struct TypeAliasChainIterator<'a> {
    map: &'a TypeDeclMap,
    cur_type: Option<&'a Type>,
}

impl<'a> Iterator for TypeAliasChainIterator<'a> {
    type Item = &'a Type;

    fn next(&mut self) -> Option<Self::Item> {
        let res = self.cur_type?;

        self.cur_type = match res {
            Type::Alias(name) => self
                .map
                .get_decl(name)
                .map(|decl| decl.value.value.as_ref()),
            _ => None,
        };

        Some(res)
    }
}
