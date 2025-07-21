#![allow(dead_code)]

use std::rc::Rc;

use frontend::{
    ast::{AST, Expr, FnDecl},
    typesystem::Type,
};

use crate::ir::{
    function::Func,
    instr::Instr,
    operand::{Const, Var},
    program::Program,
};

pub trait Translator<Input> {
    fn new() -> Self;
    fn translate(&mut self, input: Input) -> Program;
}

struct Namer {
    current_id: u64,
}

impl Namer {
    fn new() -> Self {
        Namer { current_id: 0 }
    }
    fn name_temp(&mut self) -> String {
        let ret = format!("%{}", self.current_id);
        self.current_id += 1;
        ret
    }
}

pub struct ASTTranslator {
    namer: Namer,
}

impl ASTTranslator {
    fn get_temp(&mut self, tp: Type) -> Rc<Var> {
        Rc::new(Var::new(self.namer.name_temp(), tp))
    }

    fn translate_num(&mut self, func: &mut Func, value: u64, tp: Type) -> Rc<Var> {
        let dest = self.get_temp(tp);
        let imm = Const::create_int(value);
        let instruction = Instr::create_const(dest.clone(), imm);
        func.add_to_current_block(instruction);
        dest
    }

    fn translate_bool(&mut self, func: &mut Func, value: bool) -> Rc<Var> {
        let dest = self.get_temp(Type::Bool);
        let imm = Const::create_bool(value);
        let instruction = Instr::create_const(dest.clone(), imm);
        func.add_to_current_block(instruction);
        dest
    }

    fn translate_ref(&mut self, name: String, tp: Type) -> Rc<Var> {
        Rc::new(Var::new(name, tp))
    }

    fn translate_call(
        &mut self,
        func: &mut Func,
        tp: Type,
        callee: Box<Expr>,
        args: Vec<Expr>,
    ) -> Rc<Var> {
        let dest = self.get_temp(tp);

        let label = self.translate_expr(func, *callee);

        let args: Vec<Rc<Var>> = args
            .into_iter()
            .map(|arg| self.translate_expr(func, arg))
            .collect();

        let instr = Instr::create_call(label, dest.clone(), args);

        func.add_to_current_block(instr);

        dest
    }

    fn translate_expr(&mut self, func: &mut Func, expr: Expr) -> Rc<Var> {
        match expr {
            Expr::Num { tp, value } => self.translate_num(func, value.value, tp),
            Expr::Ref { tp, name } => self.translate_ref(name.value, tp),
            Expr::Bool { value, .. } => self.translate_bool(func, value),
            Expr::Call {
                tp, callee, args, ..
            } => self.translate_call(func, tp, callee, args),
            _ => todo!(),
        }
    }

    fn translate_function(&mut self, func: FnDecl) {
        let mut ir_func = Func::empty(func.name.value, func.tp.value);
        for param in func.params.into_iter() {
            let param_var = Var::new(param.name.value, param.tp.value);
            ir_func.add_parameter(param_var);
        }
    }
}

impl Translator<AST> for ASTTranslator {
    fn new() -> Self {
        Self {
            namer: Namer::new(),
        }
    }
    fn translate(&mut self, input: AST) -> Program {
        for (_, func) in input.into_iter() {
            self.translate_function(func);
        }
        todo!()
    }
}
