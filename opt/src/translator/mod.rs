use crate::ir::Dest;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use frontend::{
    ast::{AST, BinaryOp, Expr, FnDecl},
    typesystem::Type,
};

use crate::ir::{
    function::Func,
    instr::Instr,
    operand::{Const, Label, Op, Var},
    program::Program,
};

type Defs = HashMap<String, Dest>;

pub trait Translator<Input>: Default {
    fn translate(&mut self, input: Input) -> Program;
}

struct Namer {
    current_id: u64,
    loop_id: u64,
    if_id: u64,
}

impl Namer {
    fn new() -> Self {
        Namer {
            current_id: 0,
            loop_id: 0,
            if_id: 0,
        }
    }
    fn reset(&mut self) {
        self.current_id = 0;
        self.loop_id = 0;
        self.if_id = 0;
    }
    fn name_temp(&mut self) -> String {
        let ret = format!("%{}", self.current_id);
        self.current_id += 1;
        ret
    }
    fn loop_names(&mut self) -> (String, String, String) {
        let cond = format!("%loop.cond{}", self.loop_id);
        let body = format!("%loop.body{}", self.loop_id);
        let end = format!("%loop.end{}", self.loop_id);
        self.loop_id += 1;
        (cond, body, end)
    }
    fn if_names(&mut self) -> (String, String, String) {
        let true_branch = format!("%if.true{}", self.if_id);
        let false_branch = format!("%if.false{}", self.if_id);
        let out_branch = format!("%if.out{}", self.if_id);
        self.if_id += 1;
        (true_branch, false_branch, out_branch)
    }
}

pub struct ASTTranslator {
    namer: Namer,
}

impl ASTTranslator {
    fn get_temp(&mut self, tp: Rc<Type>) -> Dest {
        Rc::new(RefCell::new(Var::new(self.namer.name_temp(), tp)))
    }

    fn translate_num(&mut self, func: &mut Func, value: u64, tp: Rc<Type>) -> Dest {
        let dest = self.get_temp(tp);
        let imm = Const::create_int(value);
        let instruction = Instr::create_const(dest.clone(), imm);
        func.add_to_current_block(instruction);
        dest
    }

    fn translate_bool(&mut self, func: &mut Func, value: bool) -> Dest {
        let dest = self.get_temp(Rc::new(Type::Bool));
        let imm = Const::create_bool(value);
        let instruction = Instr::create_const(dest.clone(), imm);
        func.add_to_current_block(instruction);
        dest
    }

    fn translate_ref(&mut self, defs: &Defs, name: String) -> Dest {
        // prob have to check there but i believe in sema(and that
        // i didn't fuck up anywhere myself).
        defs.get(&name).unwrap().clone()
    }

    fn translate_call(
        &mut self,
        func: &mut Func,
        defs: &mut Defs,
        tp: Rc<Type>,
        callee: Box<Expr>,
        args: Vec<Expr>,
    ) -> Dest {
        let dest = self.get_temp(tp);

        let label = self.translate_expr(func, defs, *callee);

        let args: Vec<Dest> = args
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
    ) -> Dest {
        let expr = self.translate_expr(func, defs, *value);
        defs.insert(name, expr.clone());
        expr
    }

    fn translate_ret(
        &mut self,
        func: &mut Func,
        defs: &mut Defs,
        value: Option<Box<Expr>>,
    ) -> Dest {
        let val = value.map(|expr| self.translate_expr(func, defs, *expr));

        let ret = Instr::create_ret(val.clone());

        func.add_to_current_block(ret);

        let new_block_name = self.namer.name_temp();
        func.start_block(new_block_name);

        val.unwrap_or(self.get_temp(Rc::new(Type::Unit)))
    }

    fn translate_binary(
        &mut self,
        func: &mut Func,
        defs: &mut Defs,
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    ) -> Dest {
        let lhs = self.translate_expr(func, defs, *lhs);
        let rhs = self.translate_expr(func, defs, *rhs);
        let dest = self.get_temp(lhs.borrow().tp.clone());
        match op {
            BinaryOp::Assign => {
                assert!(defs.contains_key(&lhs.borrow().name));
                defs.insert(lhs.borrow().name.clone(), rhs.clone());
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
            _ => todo!(), // Implement logical and comparison BinaryOp
        }
    }

    fn translate_while(
        &mut self,
        func: &mut Func,
        defs: &mut Defs,
        cond: Box<Expr>,
        body: Box<Expr>,
    ) -> Dest {
        let (cond_name, loop_name, end_name) = self.namer.loop_names();
        let prev_block = func.current_block().unwrap().clone();
        let cond_block = func.start_block(cond_name);
        let jmp = Instr::create_jmp(Label::block_label(cond_block.clone()));

        prev_block.borrow_mut().add_instr(jmp);

        let cond = self.translate_expr(func, defs, *cond);

        let loop_block = func.start_block(loop_name);

        let body = self.translate_expr(func, defs, *body); // probably return on break or some shit
        let jmp = Instr::create_jmp(Label::block_label(cond_block.clone()));
        func.add_to_current_block(jmp);

        let end_block = func.start_block(end_name);
        let loop_label = Label::block_label(loop_block);
        let end_label = Label::block_label(end_block);
        let branch = Instr::create_br(loop_label, end_label, cond);
        cond_block.borrow_mut().add_instr(branch);

        body
    }

    fn translate_if(
        &mut self,
        func: &mut Func,
        defs: &mut Defs,
        cond: Box<Expr>,
        if_true: Box<Expr>,
        if_false: Option<Box<Expr>>,
    ) -> Dest {
        let cond = self.translate_expr(func, defs, *cond);

        let (true_name, false_name, out_name) = self.namer.if_names();

        let cond_block = func.current_block().unwrap().clone();

        let true_block = func.start_block(true_name);

        let ret = self.translate_expr(func, defs, *if_true);

        let false_defined = if_false.is_some();

        if let Some(false_expr) = if_false {
            let false_block = func.start_block(false_name);

            let true_label = Label::block_label(true_block.clone());
            let false_label = Label::block_label(false_block);

            let branch = Instr::create_br(true_label, false_label, cond.clone());

            cond_block.borrow_mut().add_instr(branch);

            let false_var = self.translate_expr(func, defs, *false_expr);
            false_var.borrow_mut().name = ret.borrow().name.clone();
        }

        let fblock = func.current_block().unwrap().clone();

        let outer_block = func.start_block(out_name);

        let out_label = Label::block_label(outer_block);

        true_block
            .borrow_mut()
            .add_instr(Instr::Jmp(out_label.clone()));

        if !false_defined {
            let true_label = Label::block_label(true_block.clone());
            let br = Instr::create_br(true_label, out_label, cond);
            cond_block.borrow_mut().add_instr(br);
        } else {
            fblock.borrow_mut().add_instr(Instr::Jmp(out_label));
        }

        ret
    }

    fn translate_block(&mut self, func: &mut Func, defs: &mut Defs, body: Vec<Expr>) -> Dest {
        body.into_iter()
            .map(|expr| self.translate_expr(func, defs, expr))
            .last()
            .unwrap() // probably blows up on empty block have to test and fix if so.
    }

    fn translate_expr(&mut self, func: &mut Func, defs: &mut Defs, expr: Expr) -> Dest {
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
            Expr::While { cond, body, .. } => self.translate_while(func, defs, cond, body),
            Expr::If {
                cond,
                on_true,
                on_false,
                ..
            } => self.translate_if(func, defs, cond, on_true, on_false),
            Expr::MemberRef { tp, target, member } => todo!(),
        }
    }

    fn check_function_end(func: &mut Func) {
        assert!(func.blocks.last().unwrap().borrow().instrs.is_empty());
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
        let mut ir_func = Func::empty(func.name.value, func.return_type.value);
        for param in func.params.into_iter() {
            let param_var = Rc::new(RefCell::new(Var::new(param.name.value, param.tp.value)));
            ir_func.add_parameter(param_var.clone());
            let name = param_var.borrow().name.clone();
            defs.insert(name, param_var);
        }
        self.namer.reset();
        ir_func.start_block(self.namer.name_temp());
        let _ = self.translate_expr(&mut ir_func, &mut defs, func.body);
        Self::check_function_end(&mut ir_func);
        ir_func
    }
}

impl Default for ASTTranslator {
    fn default() -> Self {
        Self {
            namer: Namer::new(),
        }
    }
}

impl Translator<AST> for ASTTranslator {
    fn translate(&mut self, input: AST) -> Program {
        let defs: Defs = input
            .iter()
            .map(|(name, func)| {
                (
                    name.clone(),
                    Rc::new(RefCell::new(Var::new(
                        func.name.value.clone(),
                        func.return_type.value.clone(),
                    ))),
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
