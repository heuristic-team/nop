mod pretty_printable;
pub use pretty_printable::PrettyPrintable;

mod ast;

fn make_offset(depth: u8) {
    for _ in 0..depth {
        print!("  ");
    }
}
