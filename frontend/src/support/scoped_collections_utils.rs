use std::fmt::{Debug, Formatter, Result};

pub(super) fn debug_print_scopes<'a, I, S>(scopes: I, f: &mut Formatter<'_>) -> Result
where
    I: Iterator<Item = &'a S>,
    S: Debug + 'a,
{
    let mut first = true;
    for (i, scope) in scopes.enumerate() {
        if !first {
            write!(f, "\n")?;
        }

        write!(f, "{}: {:?}", i, scope)?;
        first = false;
    }

    Ok(())
}
