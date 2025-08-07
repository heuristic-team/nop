use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;

/// A stack of `HashMap`s, useful for collecting some scoped information with shadowing, like variable types.
/// Basic usage:
/// ```
/// use frontend::support::ScopedMap;
///
/// let mut map: ScopedMap<i32, i32> = ScopedMap::new();
/// map.enter_scope();
/// map.insert(24, 42);
///
/// assert!(map.contains_key(&24));
///
/// map.enter_scope();
/// map.insert(37, 73);
///
/// assert!(map.contains_key(&24));
/// assert!(map.contains_key(&37));
///
/// assert_eq!(Some(42), map.get(&24).copied());
/// map.insert(24, 14);
/// assert_eq!(Some(14), map.get(&24).copied());
///
/// map.leave_scope();
///
/// assert!(map.contains_key(&24));
/// assert!(!map.contains_key(&37));
/// ```
pub struct ScopedMap<K, V> {
    repr: Vec<HashMap<K, V>>,
}

impl<K, V> ScopedMap<K, V> {
    /// Create a new `ScopedMap`. The map is initialized with zero active scopes.
    pub fn new() -> Self {
        Self { repr: Vec::new() }
    }

    /// Enter a new scope. All newly added items will be inserted into it until any scope-changing method is called. All outer scopes are left untouched.
    pub fn enter_scope(&mut self) {
        self.add_scope(HashMap::new())
    }

    /// Leave current scope. All items present in it are discarded, but all the items from outer scopes, including the items identical to the deleted ones, are left untouched.
    ///
    /// Panics if no scopes are active.
    pub fn leave_scope(&mut self) -> HashMap<K, V> {
        self.repr.pop().expect("expected at least one active scope")
    }

    /// Same as `enter_scope`, but new scope is initialized with given `HashMap`.
    pub fn add_scope(&mut self, scope: HashMap<K, V>) {
        self.repr.push(scope)
    }
}

impl<K, V> ScopedMap<K, V>
where
    K: Eq + Hash,
{
    /// Get an immutable reference to a value by key if it is present in the map.
    ///
    /// If the map contains two identical keys in different scopes item from the most nested one will be returned. For example:
    ///
    /// 1: { (42, 14) }
    /// 2: { (42, 37) }
    /// 3: {}
    ///
    /// Implying that the stack of scopes is growing down, `get(42)` will return `37`.
    pub fn get<Q>(&self, k: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.repr
            .iter()
            .rev()
            .map(|scope| scope.get(k))
            .find(|opt| opt.is_some())
            .flatten()
    }

    /// Get a mutable reference to a value by key if it is present in the map.
    ///
    /// If the map contains two identical keys in different scopes item from the most nested one will be returned. For example:
    ///
    /// 1: { (42, 14) }
    /// 2: { (42, 37) }
    /// 3: {}
    ///
    /// Implying that the stack of scopes is growing down, `get(42)` will return `37`.
    pub fn get_mut<Q>(&mut self, k: &Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.repr
            .iter_mut()
            .rev()
            .map(|scope| scope.get_mut(k))
            .find(|opt| opt.is_some())
            .flatten()
    }

    /// Add a new key-value pair into the current scope, leaving the outer ones untouched. If the key was present in an outer scope, it will be shadowed. If the key was present in the current scope, its value will be replaced.
    ///
    /// Panics if no scopes are active.
    pub fn insert(&mut self, k: K, v: V) {
        self.repr
            .last_mut()
            .expect("expected at least one active scope")
            .insert(k, v);
    }

    /// Checks if a key is present in the map.
    pub fn contains_key<Q>(&self, k: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.repr.iter().any(|m| m.contains_key(k))
    }
}
