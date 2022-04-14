mod strings;

use glide_bytecode::{insn, Bytecode};
use glide_ir::Ir;

use crate::strings::Strings;

pub fn lower(ir: &Ir) -> Bytecode {
    let mut strings = Strings::new();

    let mut tys_builder = glide_bytecode::TysBuilder::new();

    let mut funcs = Vec::new();
    for func in ir.funcs.inner() {
        let name = strings.add(func.name.clone().into_bytes());

        let func_size: u32 = ir.tys.size(func.signature).try_into().unwrap();
        let mut locals_size: u32 = func_size;
        let mut locals: Vec<(u32, u32)> = vec![(0, func_size)];

        match ir.tys.get(func.signature) {
            glide_ir::Ty::Func(params, _) => {
                for &param in params {
                    let size: u32 = ir.tys.size(param).try_into().unwrap();
                    locals.push((locals_size, size));
                    locals_size += size;
                }
            }
            _ => unreachable!(),
        }

        let data = match &func.data {
            glide_ir::FuncData::Print => glide_bytecode::FuncData::Print,
            glide_ir::FuncData::StringConcat => glide_bytecode::FuncData::StringConcat,
            glide_ir::FuncData::Normal(ir_insns) => {
                let mut bytecode_insns = Vec::new();
                for insn in ir_insns {
                    match insn {
                        glide_ir::Insn::PushVoid => {
                            locals.push((locals_size, 0));
                        }
                        glide_ir::Insn::PushInt(value) => {
                            bytecode_insns.push(insn::PUSH_SCALAR);
                            bytecode_insns.extend(value.to_le_bytes());
                            locals.push((locals_size, 1));
                            locals_size += 1;
                        }
                        glide_ir::Insn::PushString(value) => {
                            let value = strings.add(value.clone());
                            bytecode_insns.push(insn::PUSH_CONSTANT_STRING);
                            bytecode_insns.extend(value.to_le_bytes());
                            locals.push((locals_size, 1));
                            locals_size += 1;
                        }
                        glide_ir::Insn::PushLocal(at) => {
                            let (at, size) = locals[*at];
                            bytecode_insns.push(insn::PUSH_LOCAL);
                            bytecode_insns.extend(at.to_le_bytes());
                            bytecode_insns.extend(size.to_le_bytes());
                            locals.push((locals_size, size));
                            locals_size += size;
                        }
                        glide_ir::Insn::PushFunc(id) => {
                            bytecode_insns.push(insn::PUSH_FUNC);
                            bytecode_insns.extend(id.0.to_le_bytes());
                            locals.push((locals_size, 1));
                            locals_size += 1;
                        }
                        glide_ir::Insn::Pop => {
                            let (_, size) = locals.pop().unwrap();
                            locals_size -= size;
                            bytecode_insns.resize(
                                bytecode_insns.len() + usize::try_from(size).unwrap(),
                                insn::POP,
                            );
                        }
                        glide_ir::Insn::Call { at, ret } => {
                            bytecode_insns.push(insn::CALL);
                            let at_u32: u32 = (*at).try_into().unwrap();
                            bytecode_insns.extend(at_u32.to_le_bytes());
                            let size: u32 = ir.tys.size(*ret).try_into().unwrap();
                            // bytecode_insns.extend(size.to_le_bytes());
                            for (_, size) in locals.drain(at..) {
                                locals_size -= size;
                            }
                            locals.push((locals_size, size));
                            locals_size += size;
                        }
                        glide_ir::Insn::Ret => {
                            let (_, size) = locals.pop().unwrap();
                            locals_size -= size;
                            bytecode_insns.push(insn::RET);
                            locals.pop().unwrap();
                        }
                    }
                }
                glide_bytecode::FuncData::Custom(bytecode_insns)
            }
        };
        let ret_size: u32 = match ir.tys.get(func.signature) {
            glide_ir::Ty::Func(_, ret) => {
                let ret = *ret;
                ir.tys.size(ret).try_into().unwrap()
            }
            _ => unreachable!(),
        };
        funcs.push(glide_bytecode::Func {
            name,
            ret_size,
            data,
        });
    }

    Bytecode {
        funcs,
        main_func: ir.main_func.0,
        tys: tys_builder.build(),
        strings: strings.into_inner(),
    }
}
