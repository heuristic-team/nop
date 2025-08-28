use crate::ir::IRVal;
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

type Defs = HashMap<String, IRVal>;

/// Translator has to have a default constructor and translate method.
pub trait Translator<Input>: Default {
    fn translate(&mut self, input: Input) -> Program;
}

/// Gives names to entities in IR.
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
    /// Names for temporary vars.
    fn name_temp(&mut self) -> String {
        let ret = format!("%{}", self.current_id);
        self.current_id += 1;
        ret
    }
    /// Names for labels of loops.
    fn loop_names(&mut self) -> (String, String, String) {
        let cond = format!("%loop.cond{}", self.loop_id);
        let body = format!("%loop.body{}", self.loop_id);
        let end = format!("%loop.end{}", self.loop_id);
        self.loop_id += 1;
        (cond, body, end)
    }
    /// Names for labels of if statements.
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
    /// Creates temporary variables.
    fn get_temp(&mut self, tp: Rc<Type>) -> IRVal {
        Rc::new(RefCell::new(Var::new(self.namer.name_temp(), tp)))
    }

    /// Translates [[`Expr::Num`]] into [[`Instr::Const`]] instruction of IR.
    /// Returns temporary variable that is a result of const instruction.
    fn translate_num(&mut self, func: &mut Func, value: u64, tp: Rc<Type>) -> IRVal {
        let dest = self.get_temp(tp);
        let imm = Const::create_int(value);
        let instruction = Instr::create_const(dest.clone(), imm);
        func.add_to_current_block(instruction);
        dest
    }

    /// Translates [[`Expr::Bool`]] into [[`Instr::Const`]] instruction of IR.
    /// Returns temporary variable that is a result of const instruction.
    fn translate_bool(&mut self, func: &mut Func, value: bool) -> IRVal {
        let dest = self.get_temp(Rc::new(Type::Bool));
        let imm = Const::create_bool(value);
        let instruction = Instr::create_const(dest.clone(), imm);
        func.add_to_current_block(instruction);
        dest
    }

    /// Returns temporary variable that currently represents given [[`Expr::Ref`]].
    fn translate_ref(&mut self, defs: &Defs, name: String) -> IRVal {
        // prob have to check there but i believe in sema(and that
        // i didn't fuck up anywhere myself).
        defs.get(&name).unwrap().clone()
    }

    /// Translates [[`Expr::Call`]] into the [[`Instr::Call`]] instruction.
    fn translate_call(
        &mut self,
        func: &mut Func,
        defs: &mut Defs,
        tp: Rc<Type>,
        callee: Box<Expr>,
        args: Vec<Expr>,
    ) -> IRVal {
        let dest = self.get_temp(tp);

        let label = self.translate_expr(func, defs, *callee);

        // Here we first translate recursively all arguments and then gather temporary
        // variables that represent each argument and they will be the arguments for the call
        // instruction.
        let args: Vec<IRVal> = args
            .into_iter()
            .map(|arg| self.translate_expr(func, defs, arg))
            .collect();

        let instr = Instr::create_call(label, dest.clone(), args);

        func.add_to_current_block(instr);

        dest
    }

    /// Translate [[`Expr::Declare`]].
    fn translate_declare(
        &mut self,
        func: &mut Func,
        defs: &mut Defs,
        name: String,
        value: Box<Expr>,
    ) -> IRVal {
        let expr = self.translate_expr(func, defs, *value);
        // Now this definition is represented by translated expression.
        // Whenever we try to take ref of that variable this expression will be given, unless
        // reassigned.
        defs.insert(name, expr.clone());
        expr
    }

    /// Translates [[`Expr::Ret`]] into [[`Instr::Ret`]] instruction.
    fn translate_ret(
        &mut self,
        func: &mut Func,
        defs: &mut Defs,
        value: Option<Box<Expr>>,
    ) -> IRVal {
        let val = value.map(|expr| self.translate_expr(func, defs, *expr));

        let ret = Instr::create_ret(val.clone());

        func.add_to_current_block(ret);

        let new_block_name = self.namer.name_temp();
        func.start_block(new_block_name);

        // If parser and sema are correct then either this will be unwrapped and variable
        // will be used, or the temp of type of unit will be returned and will not be used
        // anywhere.
        val.unwrap_or(self.get_temp(Rc::new(Type::Unit)))
    }

    /// Translates [[`Expr::Binary`]] into needed instruction.
    fn translate_binary(
        &mut self,
        func: &mut Func,
        defs: &mut Defs,
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    ) -> IRVal {
        let lhs = self.translate_expr(func, defs, *lhs);
        let rhs = self.translate_expr(func, defs, *rhs);
        let dest = self.get_temp(lhs.borrow().tp.clone());
        match op {
            BinaryOp::Assign => {
                // Assertion that assigned variable was declared somewhere.
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

    /// Translate [[`Expr::While`]] into labels and jumps.
    fn translate_while(
        &mut self,
        func: &mut Func,
        defs: &mut Defs,
        cond: Box<Expr>,
        body: Box<Expr>,
    ) -> IRVal {
        // Label names.
        let (cond_name, loop_name, end_name) = self.namer.loop_names();
        // Pointer to the block before loop.
        let prev_block = func.current_block().unwrap().clone();
        // Pointer to the block where condition will be positioned.
        let cond_block = func.start_block(cond_name);
        // Jump from previous block to the condition block
        // because every block should end in a terminator instruction
        let jmp = Instr::create_jmp(Label::block_label(cond_block.clone()));

        prev_block.borrow_mut().add_instr(jmp);

        // Translate condition and get boolean variable that represents
        // whether we should loop or not.
        let cond = self.translate_expr(func, defs, *cond);

        // Block with the body of the loop.
        let loop_block = func.start_block(loop_name);

        let body = self.translate_expr(func, defs, *body);
        // Jump from body to condition block to recheck condition.
        let jmp = Instr::create_jmp(Label::block_label(cond_block.clone()));
        func.add_to_current_block(jmp);

        // Block after the loop.
        let end_block = func.start_block(end_name);
        let loop_label = Label::block_label(loop_block);
        let end_label = Label::block_label(end_block);
        // Branch instruction based on condition of the loop:
        // - If true then we loop, i.e. jump to body of the loop.
        // - If false then we jump to the block after the loop.
        let branch = Instr::create_br(loop_label, end_label, cond);
        cond_block.borrow_mut().add_instr(branch);

        body
    }

    /// Translates [[`Expr::If`]] into labels and jumps.
    fn translate_if(
        &mut self,
        func: &mut Func,
        defs: &mut Defs,
        cond: Box<Expr>,
        if_true: Box<Expr>,
        if_false: Option<Box<Expr>>,
    ) -> IRVal {
        // Translate condition of if and get boolean variable.
        let cond = self.translate_expr(func, defs, *cond);

        // Label names.
        let (true_name, false_name, out_name) = self.namer.if_names();

        // Block in which conditions were translated.
        let cond_block = func.current_block().unwrap().clone();

        // Block with the body of if.
        let true_block = func.start_block(true_name);

        let ret = self.translate_expr(func, defs, *if_true);

        let false_defined = if_false.is_some();

        if let Some(false_expr) = if_false {
            // Block with the body of else.
            let false_block = func.start_block(false_name);

            let true_label = Label::block_label(true_block.clone());
            let false_label = Label::block_label(false_block);

            let branch = Instr::create_br(true_label, false_label, cond.clone());

            cond_block.borrow_mut().add_instr(branch);

            let false_var = self.translate_expr(func, defs, *false_expr);

            // This is a funny one. Basically, we give both returned temporary variables
            // from true if block and from false if block the same name, so that later
            // SSA algorithm renames them and places phi-node where it should be.
            false_var.borrow_mut().name = ret.borrow().name.clone();
        }

        let fblock = func.current_block().unwrap().clone();

        // Block after the if.
        let outer_block = func.start_block(out_name);

        let out_label = Label::block_label(outer_block);

        // True block always should have jump to the outer block.
        true_block
            .borrow_mut()
            .add_instr(Instr::Jmp(out_label.clone()));

        // If there was no else, then we should create branch instruction
        // that jumps to body of the if on true and to the outer block on false.
        if !false_defined {
            let true_label = Label::block_label(true_block.clone());
            let br = Instr::create_br(true_label, out_label, cond);
            cond_block.borrow_mut().add_instr(br);
        } else {
            // If there was else then else block should have jump to the outer block too.
            fblock.borrow_mut().add_instr(Instr::Jmp(out_label));
        }

        ret
    }

    /// Translate [[`Expr::Block`]].
    fn translate_block(&mut self, func: &mut Func, defs: &mut Defs, body: Vec<Expr>) -> IRVal {
        body.into_iter()
            .map(|expr| self.translate_expr(func, defs, expr))
            .last()
            .unwrap_or(self.get_temp(Rc::new(Type::Unit)))
    }

    // Recursive function that looks at the type of the expression and calls
    // the needed function.
    fn translate_expr(&mut self, func: &mut Func, defs: &mut Defs, expr: Expr) -> IRVal {
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
            Expr::MemberRef {
                tp: _,
                target: _,
                member: _,
            } => todo!(),
        }
    }

    /// Check if function ends on return.
    fn check_function_end(func: &mut Func) {
        // Assertion that the last block is empty and meaningless. If that is not the case
        // Then frontend probably didn't insert return expression in the unit function.
        assert!(func.blocks.last().unwrap().borrow().instrs.is_empty());
        // Last block was created when we were translating return and is empty and
        // meaningless. We should pop it.
        func.pop_block();
        match func.blocks.last().unwrap().borrow().instrs.last() {
            None => assert!(false),
            Some(instr) => match instr {
                Instr::Ret(_) => (),
                _ => assert!(false),
            },
        }
    }

    /// Translate [[`FnDecl`]] into [[`Func`]] in IR.
    fn translate_function(&mut self, func: FnDecl, mut defs: Defs) -> Func {
        let mut ir_func = Func::empty(func.name.value, func.return_type.value);

        // Create definitions for parameters of the function.
        func.params.into_iter().for_each(|param| {
            let param_var = Rc::new(RefCell::new(Var::new(param.name.value, param.tp.value)));
            ir_func.add_parameter(param_var.clone());
            let name = param_var.borrow().name.clone();
            defs.insert(name, param_var);
        });

        // Reset namer for this function.
        self.namer.reset();
        ir_func.start_block(self.namer.name_temp());
        // Translate body of the function.
        let _ = self.translate_expr(&mut ir_func, &mut defs, func.body);
        // Check the end of the function and pop the last block.
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
        // Create definition of variables out of functions for `call` instructions
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
