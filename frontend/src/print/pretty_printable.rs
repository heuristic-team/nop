pub trait PrettyPrintable {
    fn print_with_depth(&self, depth: u8);

    fn print(&self) {
        self.print_with_depth(0);
    }
}
