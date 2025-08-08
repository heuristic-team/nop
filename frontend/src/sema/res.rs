use crate::Diagnostic;

/// Custom result type for semantic analysis.
///
/// Error is bound to `Vec<Diagnostic>` as the only needed type, `Problematic` is used when a pass generated some diagnostics, but they are not fatal.
#[derive(Debug)]
pub enum Res<T> {
    Ok(T),
    Problematic(T, Vec<Diagnostic>),
    Fatal(Vec<Diagnostic>),
}

impl<T> Res<T> {
    pub fn map<U, F>(self, op: F) -> Res<U>
    where
        F: FnOnce(T) -> U,
    {
        match self {
            Res::Ok(value) => Res::Ok(op(value)),
            Res::Problematic(value, diags) => Res::Problematic(op(value), diags),
            Res::Fatal(diags) => Res::Fatal(diags),
        }
    }

    pub fn add_diags(self, mut new_diags: Vec<Diagnostic>) -> Self {
        match self {
            Res::Ok(value) => Res::Problematic(value, new_diags),
            Res::Problematic(value, mut diags) => {
                diags.append(&mut new_diags);
                Res::Problematic(value, diags)
            }
            Res::Fatal(mut diags) => {
                diags.append(&mut new_diags);
                Res::Fatal(diags)
            }
        }
    }

    pub fn and_then<U, F>(self, op: F) -> Res<U>
    where
        F: FnOnce(T) -> Res<U>,
    {
        match self {
            Res::Ok(value) => op(value),
            Res::Fatal(diags) => Res::Fatal(diags),
            Res::Problematic(value, mut diags) => match op(value) {
                Res::Ok(value) => Res::Problematic(value, diags),

                Res::Problematic(value, mut diags2) => {
                    diags.append(&mut diags2);
                    Res::Problematic(value, diags)
                }
                Res::Fatal(mut diags2) => {
                    diags.append(&mut diags2);
                    Res::Fatal(diags)
                }
            },
        }
    }

    pub fn inspect_value(&self) -> Option<&T> {
        match self {
            Res::Ok(value) | Res::Problematic(value, _) => Some(value),
            Res::Fatal(_) => None,
        }
    }

    pub fn extract_value(self) -> Option<T> {
        match self {
            Res::Ok(value) | Res::Problematic(value, _) => Some(value),
            Res::Fatal(_) => None,
        }
    }

    pub fn inspect_diagnostics(&self) -> Option<impl Iterator<Item = &Diagnostic>> {
        match self {
            Res::Ok(_) => None,
            Res::Problematic(_, diags) | Res::Fatal(diags) => Some(diags.into_iter()),
        }
    }

    pub fn extract_diagnostics(self) -> Option<impl Iterator<Item = Diagnostic>> {
        match self {
            Res::Ok(_) => None,
            Res::Problematic(_, diags) | Res::Fatal(diags) => Some(diags.into_iter()),
        }
    }
}
