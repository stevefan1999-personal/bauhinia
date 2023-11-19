use dynasmrt::{dynasm, DynasmApi, DynasmLabelApi};

use std::{io, slice, mem};
use std::io::Write;

fn main() {
    let mut ops = dynasmrt::x64::Assembler::new().unwrap();

    let hello = ops.offset();
    dynasm!(ops
        ; .arch x64
        ; jmp ->foobar    
        ; ->init:
        ; push rbp
        ; mov rbp, rsp
        ; ->bar:
        ; sub rsp, 4
        ; mov r10, [rbp+12]
        ; mov r12, [rbp+8]
        ; mov r11, r12
        ; and r11, r10
        ; mov r9, r11
        ; shl r9, 1
        ; mov r13, r12
        ; xor r13, r10
        ; mov r11, r13
        ; add r11, r9
        ; mov r9, r11
        ; mov rsp, rbp
        ; pop rbp
        ; ret
        ; ->fn1:
        ; push rbp
        ; mov rbp, rsp
        ; sub rsp, 12
        ; mov r13, [rbp+8]
        ; cmp r13, 2
        ; jle ->branch1
        ; jmp ->branch2
        ; ->branch1:
        ; mov r11, r9
        ; mov r9, 1
        ; mov rsp, rbp
        ; pop rbp
        ; ret
        ; ->branch2:
        ; mov r14, r13
        ; sub r14, 1
        ; mov r12, r14
        ; push r12
        ; int 3
        ; mov [rbp+8], r13
        ; mov [rbp-8], r12
        ; call ->fn1
        ; add rsp, 4
        ; mov r12, 2
        ; push rax
        ; mov rax, r12
        ; mul r9
        ; mov r12, rax
        ; pop rax
        ; mov r9, [rbp-8]
        ; mov r9, r12
        ; mov r13, [rbp+8]
        ; mov r12, r13
        ; sub r12, 2
        ; mov r10, r12
        ; push r10
        ; mov [rbp+8], r13
        ; mov [rbp-8], r9
        ; mov [rbp-12], r10
        ; call ->fn1
        ; add rsp, 4
        ; mov r10, [rbp-12]
        ; mov r10, r9
        ; push r10
        ; mov r9, [rbp-8]
        ; push r9
        ; mov [rbp-8], r9
        ; mov [rbp-12], r10
        ; call ->init
        ; add rsp, 8
        ; mov r10, r9
        ; mov r9, r10
        ; mov rsp, rbp
        ; pop rbp
        ; ret
        ; ->foobar:
        ; sub rsp, 104
        ; mov r10, rsp
        ; mov rsp, rbp
        ; sub rsp, 0
        ; push DWORD 0x8341013fu32 as i32
        ; push DWORD 0x83391117u32 as i32
        ; push DWORD 0xe35141cfu32 as i32
        ; push DWORD 0xa3899167u32 as i32
        ; push DWORD 0xc3e101dfu32 as i32
        ; push DWORD 0x43599137
        ; push DWORD 0x23f1416f
        ; push DWORD 0x63a91187
        ; push DWORD 0x381017f
        ; push DWORD 0x3791157
        ; push DWORD 0x6391410f
        ; push DWORD 0x23c991a7
        ; push DWORD 0x3e1e602a
        ; push DWORD 0xaac6fc18u32 as i32
        ; push DWORD 0x940434ccu32 as i32
        ; push DWORD 0xbcdd4ea9u32 as i32
        ; push DWORD 0xb39e6f8fu32 as i32
        ; push DWORD 0xea8e25edu32 as i32
        ; push DWORD 0xd2bc703bu32 as i32
        ; push DWORD 0xd339ce89u32 as i32
        ; push DWORD 0xa23e362au32 as i32
        ; push DWORD 0x73bba5e8
        ; push DWORD 0x54412994
        ; push DWORD 0x501b6575
        ; push DWORD 0x66626a69
        ; mov rsp, r10
        ; mov r13, 0
        ; ->foo:
        ; push r13
        ; mov [rbp-104], r13
        ; call ->fn1
        ; add rsp, 4
        ; mov r10, r9
        ; and r10, 255
        ; mov r12, r10
        ; mov r9, rbp
        ; sub r9, 100
        ; mov r10, r9
        ; mov r13, [rbp-104]
        ; add r10, r13
        ; xor [r10], r12
        ; mov r9, rbp
        ; sub r9, 100
        ; mov r10, r9
        ; add r10, r13
        ; xor r10, r9
        ; xor r9, r10
        ; xor r10, r9
        ; mov r14, r10
        ; mov r10, 1
        ; mov r15, 1
        ; syscall
        ; mov r9, r10
        ; add r13, 1
        ; cmp r13, 100
        ; mov [rbp-104], r13
        ; jle ->foo
        ; mov r10, r9
        ; mov r9, 0
        ; mov r15, 2
        ; syscall
        ; add rsp, 104
    );

    let buf = ops.finalize().unwrap();

    let hello_fn: extern "win64" fn() -> bool = unsafe { mem::transmute(buf.ptr(hello)) };

    hello_fn();
}

pub extern "win64" fn print(buffer: *const u8, length: u64) -> bool {
    io::stdout()
        .write_all(unsafe { slice::from_raw_parts(buffer, length as usize) })
        .is_ok()
}