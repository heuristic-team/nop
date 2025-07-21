#![allow(dead_code)]

use std::{collections::HashMap, rc::Rc};

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

type Defs = HashMap<String, Rc<Var>>;

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

    fn translate_ref(&mut self, defs: &Defs, name: String) -> Rc<Var> {
        // prob have to check there but i believe in sema(and that
        // i didn't fuck up anywhere myself).
        defs.get(&name).unwrap().clone()
    }

    fn translate_call(
        &mut self,
        func: &mut Func,
        defs: &mut Defs,
        tp: Type,
        callee: Box<Expr>,
        args: Vec<Expr>,
    ) -> Rc<Var> {
        let dest = self.get_temp(tp);

        let label = self.translate_expr(func, defs, *callee);

        let args: Vec<Rc<Var>> = args
            .into_iter()
            .map(|arg| self.translate_expr(func, defs, arg))
            .collect();

        let instr = Instr::create_call(label, dest.clone(), args);

        func.add_to_current_block(instr);

        dest
    }

    fn translate_declare(
        &mut self,
        func: &mut Func,
        defs: &mut Defs,
        name: String,
        value: Box<Expr>,
    ) -> Rc<Var> {
        let expr = self.translate_expr(func, defs, *value);
        defs.insert(name, expr.clone());
        expr
    }

    fn translate_expr(&mut self, func: &mut Func, defs: &mut Defs, expr: Expr) -> Rc<Var> {
        match expr {
            Expr::Num { tp, value } => self.translate_num(func, value.value, tp),
            Expr::Ref { name, .. } => self.translate_ref(defs, name.value),
            Expr::Bool { value, .. } => self.translate_bool(func, value),
            Expr::Call {
                tp, callee, args, ..
            } => self.translate_call(func, defs, tp, callee, args),
            Expr::Declare { name, value, .. } => {
                self.translate_declare(func, defs, name.value, value)
            }
            _ => todo!(),
        }
    }

    fn translate_function(&mut self, func: FnDecl, mut defs: Defs) {
        let mut ir_func = Func::empty(func.name.value, func.tp.value);
        for param in func.params.into_iter() {
            let param_var = Var::new(param.name.value, param.tp.value);
            ir_func.add_parameter(param_var);
        }
        let _ = self.translate_expr(&mut ir_func, &mut defs, func.body);
    }
}

impl Translator<AST> for ASTTranslator {
    fn new() -> Self {
        Self {
            namer: Namer::new(),
        }
    }
    fn translate(&mut self, input: AST) -> Program {
        let defs: Defs = input
            .iter()
            .map(|(name, func)| {
                (
                    name.clone(),
                    Rc::new(Var::new(func.name.value.clone(), func.tp.value.clone())),
                )
            })
            .collect();
        for (_, func) in input.into_iter() {
            self.translate_function(func, defs.clone());
        }
        todo!()
    }
}
