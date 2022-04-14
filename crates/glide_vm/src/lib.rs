#[cfg(not(target_pointer_width = "64"))]
compile_error!("only works on 64 bit systems");

mod alloc;
mod object;

use std::{
    alloc::Layout,
    io::{self, Write},
    mem::MaybeUninit,
    ptr, slice,
};

use glide_bytecode::{insn, Bytecode, Func, FuncData, Ty, TyId, Tys};

use self::object::{ObjectHeader, StringHeader};

pub fn interpret(bytecode: Bytecode) {
    Vm::run(bytecode);
}

struct Vm {
    tys: Tys,
    strings: Vec<*mut StringHeader>,
    stack: Vec<StackValue>,
    stack_tys: Vec<TyId>,
    heap: *mut ObjectHeader,
}

impl Vm {
    fn run(
        Bytecode {
            funcs,
            main_func,
            tys,
            strings: bytecode_strings,
        }: Bytecode,
    ) {
        let mut heap = ptr::null_mut();
        let mut strings = Vec::new();
        for string in &bytecode_strings {
            let len = string.len();
            let (ptr, buffer_ptr) = alloc_string(&mut heap, len);
            unsafe {
                buffer_ptr.copy_from(string.as_ptr(), len);
            }
            strings.push(ptr);
        }

        let mut vm = Self {
            tys,
            strings,
            stack: vec![StackValue { uint: main_func }],
            stack_tys: vec![TyId::FUNC],
            heap,
        };

        vm.call(&funcs, 0);
    }

    fn call(&mut self, funcs: &[Func], base: usize) {
        let func_id = self.stack[base];
        let func_id = unsafe { func_id.uint };
        let func = &funcs[func_id];
        {
            let ptr = self.strings[func.name as usize];
            let len = unsafe { StringHeader::len(ptr) };
            let buffer_ptr = unsafe { StringHeader::buffer_ptr(ptr) };
            let buf = unsafe { slice::from_raw_parts(buffer_ptr, len) };
            let mut stderr = std::io::stderr();
            // eprint!("calling: ");
            // std::io::Write::write(&mut stderr, buf).unwrap();
            // eprintln!();
            // for ty in &self.stack_tys {
            //     eprint!("{:?} ", ty);
            // }
            // eprintln!();
        }
        match &func.data {
            FuncData::Custom(insns) => {
                let mut ip = 0;
                while ip < insns.len() {
                    let insn = insns[ip];
                    ip += 1;
                    match insn {
                        insn::PUSH_SCALAR => {
                            let value = i64::from_le_bytes(insns[ip..][..8].try_into().unwrap());
                            ip += 8;
                            self.push(StackValue { int: value }, TyId::SCALAR);
                        }
                        insn::PUSH_CONSTANT_STRING => {
                            let id = u32::from_le_bytes(insns[ip..][..4].try_into().unwrap());
                            ip += 4;
                            let value = self.strings[id as usize];
                            self.push(StackValue { string: value }, TyId::STRING);
                        }
                        insn::PUSH_FUNC => {
                            let value = usize::from_le_bytes(insns[ip..][..8].try_into().unwrap());
                            ip += 8;
                            self.push(StackValue { uint: value }, TyId::FUNC);
                        }
                        insn::CALL => {
                            let at = u32::from_le_bytes(insns[ip..][..4].try_into().unwrap());
                            ip += 4;
                            self.call(funcs, base as usize + at as usize);
                        }
                        insn::RET => {
                            let ret_size = func.ret_size as usize;
                            let len = self.stack.len();
                            let src = len - ret_size;
                            let truncate_to = base + ret_size;
                            self.stack.copy_within(src..len, base);
                            self.stack.truncate(truncate_to);
                            self.stack_tys.copy_within(src..len, base);
                            self.stack_tys.truncate(truncate_to);
                            break;
                        }
                        insn::POP => {
                            self.pop();
                        }
                        insn::PUSH_LOCAL => {
                            let at =
                                u32::from_le_bytes(insns[ip..][..4].try_into().unwrap()) as usize;
                            ip += 4;
                            let size =
                                u32::from_le_bytes(insns[ip..][..4].try_into().unwrap()) as usize;
                            ip += 4;
                            self.stack
                                .extend_from_within((base + at)..(base + at + size));
                            self.stack_tys
                                .extend_from_within((base + at)..(base + at + size));
                        }
                        insn => panic!("unknown insn {}", insn),
                    }
                }
            }
            FuncData::Print => {
                let top = self.pop();
                let ptr = unsafe { top.string };
                let len = unsafe { StringHeader::len(ptr) };
                let buffer_ptr = unsafe { StringHeader::buffer_ptr(ptr) };
                let buf = unsafe { slice::from_raw_parts(buffer_ptr, len) };
                io::stdout().write_all(buf).unwrap();
                println!();
                self.pop();
            }
            FuncData::StringConcat => {
                let second = self.pop();
                let first = self.pop();

                let second = unsafe { second.string };
                let second_len = unsafe { StringHeader::len(second) };
                let second_ptr = unsafe { StringHeader::buffer_ptr(second) };

                let first = unsafe { first.string };
                let first_len = unsafe { StringHeader::len(first) };
                let first_ptr = unsafe { StringHeader::buffer_ptr(first) };

                let total_len = first_len.checked_add(second_len).unwrap();
                assert!(total_len <= isize::MAX as usize);

                let (alloc, buf_ptr) = self.alloc_string(total_len);

                unsafe {
                    buf_ptr.copy_from(first_ptr, first_len);
                    buf_ptr.add(first_len).copy_from(second_ptr, second_len);
                }

                let value = StackValue { string: alloc };

                self.pop();
                self.push(value, TyId::STRING);
            }
        }
    }

    fn push(&mut self, value: StackValue, ty: TyId) {
        self.stack.push(value);
        self.stack_tys.push(ty);
    }

    fn pop(&mut self) -> StackValue {
        let value = self.stack.pop().unwrap();
        self.stack_tys.pop().unwrap();
        value
    }

    fn alloc_string(&mut self, len: usize) -> (*mut StringHeader, *mut u8) {
        alloc_string(&mut self.heap, len)
    }

    fn gc(&mut self) {
        self.clear_marks();
        self.mark();
        self.sweep();
    }

    fn clear_marks(&mut self) {
        let mut cur = self.heap;
        while !cur.is_null() {
            unsafe { ObjectHeader::clear_mark(cur) };
            cur = unsafe { ObjectHeader::next(cur) };
        }
    }

    fn mark(&mut self) {
        for string in &self.strings {
            unsafe {
                StringHeader::mark(*string);
            }
        }
        debug_assert_eq!(self.stack.len(), self.stack_tys.len());
        for (&value, &ty) in self.stack.iter().zip(&self.stack_tys) {
            match self.tys.get(ty) {
                Ty::Scalar => (),
                Ty::String => unsafe {
                    StringHeader::mark(value.string);
                },
                Ty::Func => (),
                Ty::Slice(inner) => {
                    for inner_ty in inner {}
                    todo!()
                }
            }
        }
    }

    fn sweep(&mut self) {
        let mut cur = loop {
            if self.heap.is_null() {
                break ptr::null_mut();
            } else {
                let next = unsafe { ObjectHeader::next(self.heap) };
                if unsafe { ObjectHeader::should_sweep(self.heap) } {
                    unsafe {
                        ObjectHeader::free(self.heap);
                    }
                    self.heap = next;
                } else {
                    break next;
                }
            }
        };
        while !cur.is_null() {
            let next = unsafe { ObjectHeader::next(cur) };
            if unsafe { ObjectHeader::should_sweep(cur) } {
                unsafe {
                    ObjectHeader::free(cur);
                }
            }
            cur = next;
        }
    }
}

fn alloc_string(heap: &mut *mut ObjectHeader, len: usize) -> (*mut StringHeader, *mut u8) {
    let (ptr, buffer_ptr) = StringHeader::alloc(*heap, len);
    *heap = ptr.cast();
    (ptr, buffer_ptr)
}

#[derive(Copy, Clone)]
union StackValue {
    int: i64,
    uint: usize,
    string: *mut StringHeader,
}
