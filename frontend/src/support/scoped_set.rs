use std::borrow::Borrow;
use std::collections::HashSet;
use std::hash::Hash;

/// A stack of `HashSet`s, useful for checking names in nested scopes.
/// Basic usage:
/// ```
/// use frontend::support::ScopedSet;
///
/// let mut set: ScopedSet<i32> = ScopedSet::new();
/// set.enter_scope();
/// set.insert(42);
///
/// assert!(set.contains(&42));
///
/// set.enter_scope();
/// set.insert(37);
///
/// assert!(set.contains(&42));
/// assert!(set.contains(&37));
///
/// set.leave_scope();
///
/// assert!(set.contains(&42));
/// assert!(!set.contains(&37));
/// ```
pub struct ScopedSet<T> {
    repr: Vec<HashSet<T>>,
}

impl<T> ScopedSet<T> {
    /// Create a new `ScopedSet`. The set is initialized with zero active scopes.
    pub fn new() -> Self {
        Self { repr: Vec::new() }
    }

    /// Enter a new scope. All newly added items will be inserted into it until any scope-changing method is called. All outer scopes are left untouched.
    pub fn enter_scope(&mut self) {
        self.add_scope(HashSet::new())
    }

    /// Leave current scope. All items present in it are discarded, but all the items from outer scopes, including the items identical to the deleted ones, are left untouched.
    ///
    /// Panics if no scopes are active.
    pub fn leave_scope(&mut self) -> HashSet<T> {
        self.repr.pop().expect("expected at least one active scope")
    }

    /// Same as `enter_scope`, but new scope is initialized with given `HashSet`.
    pub fn add_scope(&mut self, scope: HashSet<T>) {
        self.repr.push(scope)
    }
}

impl<T> ScopedSet<T>
where
    T: Eq + Hash,
{
    /// Add an item into current active scope, leaving the outer ones untouched.
    ///
    /// Panics if no scopes are active.
    pub fn insert(&mut self, x: T) {
        self.repr
            .last_mut()
            .expect("expected at least one active scope")
            .insert(x);
    }

    /// Checks if an item is present in the set.
    pub fn contains<Q>(&self, x: &Q) -> bool
    where
        T: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.repr.iter().any(|m| m.contains(x))
    }
}
