#![allow(dead_code)]

use std::{collections::HashMap, rc::Rc};

use frontend::{
    ast::{AST, BinaryOp, Expr, FnDecl},
    typesystem::Type,
};

use crate::ir::{
    basic_block::BasicBlock,
    function::Func,
    instr::Instr,
    operand::{Const, Op, Var},
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
    fn zero(&mut self) {
        self.current_id = 0;
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

    fn translate_ret(
        &mut self,
        func: &mut Func,
        defs: &mut Defs,
        value: Option<Box<Expr>>,
    ) -> Rc<Var> {
        let val = value.map(|expr| self.translate_expr(func, defs, *expr));

        let ret = Instr::create_ret(val.clone());

        func.add_to_current_block(ret);

        let new_block_name = self.namer.name_temp();
        func.start_block(new_block_name);

        match val {
            // TODO: think of how to unkostilit this sheise
            None => Rc::new(Var::new("".to_string(), Type::Unit)),
            Some(var) => var,
        }
    }

    fn translate_binary(
        &mut self,
        func: &mut Func,
        defs: &mut Defs,
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    ) -> Rc<Var> {
        let lhs = self.translate_expr(func, defs, *lhs);
        let rhs = self.translate_expr(func, defs, *rhs);
        let dest = self.get_temp(lhs.tp.clone());
        match op {
            BinaryOp::Assign => {
                defs.insert(lhs.name.clone(), rhs.clone());
                rhs
            }
            BinaryOp::Mul => {
                let mul = Instr::create_mul(dest.clone(), Op::Variable(lhs), Op::Variable(rhs));
                func.add_to_current_block(mul);
                dest
            }
            BinaryOp::Plus => {
                let add = Instr::create_add(dest.clone(), Op::Variable(lhs), Op::Variable(rhs));
                func.add_to_current_block(add);
                dest
            }
            BinaryOp::Minus => {
                let sub = Instr::create_sub(dest.clone(), Op::Variable(lhs), Op::Variable(rhs));
                func.add_to_current_block(sub);
                dest
            }
        }
    }

    fn translate_block(&mut self, func: &mut Func, defs: &mut Defs, body: Vec<Expr>) -> Rc<Var> {
        body.into_iter()
            .map(|expr| self.translate_expr(func, defs, expr))
            .last()
            .unwrap() // probably blows up on empty block have to test and fix if so.
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
            Expr::Ret { value, .. } => self.translate_ret(func, defs, value),
            Expr::Binary { op, lhs, rhs, .. } => {
                self.translate_binary(func, defs, op.value, lhs, rhs)
            }
            Expr::Block { body, .. } => self.translate_block(func, defs, body),
        }
    }

    fn check_function_end(func: &mut Func) {
        if func.blocks.last().unwrap().borrow().instrs.len() > 0 {
            assert!(false);
        }
        func.pop_block();
        match func.blocks.last().unwrap().borrow().instrs.last() {
            None => assert!(false),
            Some(instr) => match instr {
                Instr::Ret(_) => (),
                _ => assert!(false),
            },
        }
    }

    fn translate_function(&mut self, func: FnDecl, mut defs: Defs) -> Func {
        let mut ir_func = Func::empty(func.name.value, func.tp.value);
        for param in func.params.into_iter() {
            let param_var = Rc::new(Var::new(param.name.value, param.tp.value));
            ir_func.add_parameter(param_var.clone());
            defs.insert(param_var.name.clone(), param_var);
        }
        self.namer.zero();
        ir_func.start_block(self.namer.name_temp());
        let _ = self.translate_expr(&mut ir_func, &mut defs, func.body);
        Self::check_function_end(&mut ir_func);
        ir_func
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
        let mut program = Program::empty();
        for (_, func) in input.into_iter() {
            program.add_owned_function(self.translate_function(func, defs.clone()));
        }
        program
    }
}
