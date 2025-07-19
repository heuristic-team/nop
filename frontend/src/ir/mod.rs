trait Value {}

enum Const {
    Int(u64),
    Bool(bool),
    Label(String),
    Unit,
}

enum Operands {
    Var,
    Val(Const),
}

enum Instr {}
