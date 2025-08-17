
extern go_native
extern go_back

section .text

unsafe_state:
    mov rdi, rbp
    mov rsi, rsp
    call go_native
    ret

safe_state:
    call go_back
    ret









seq:

        push    rbp
        mov     rbp, rsp
        sub     rsp, 32
        mov     DWORD PTR [rbp-20], edi
        mov     edi, 4
        call    malloc
        mov     QWORD PTR [rbp-8], rax
        mov     eax, DWORD PTR [rbp-20]
        sub     eax, 1
        mov     edi, eax
        call    seq
        mov     rdx, QWORD PTR [rbp-8]
        mov     DWORD PTR [rdx], eax
        mov     rax, QWORD PTR [rbp-8]
        mov     edx, DWORD PTR [rax]
        mov     eax, DWORD PTR [rbp-20]
        add     eax, edx
        leave
        safepoint //rbp сейчас

        ret


фрейм функции
---------------
/ число
/ птр
/ ra
/ число
/ птр





func:
    push 0b1010

    push rbp
    mov rbp, rsp
    mov [rbp], число
    call alloc
    mov [rbp-8], ptr
    sub rsp, 16
    call foo


foo:
    push 0
    push 0
    push rbp
    mov rbp, rsp
    leave
    ret











/ old_rbp /8
/ edi     /4 -4
/ nop     /4 -8
/ ptr     /8 -16

sseq:
         push    0
         push    16
         push    rbp
         mov     rbp, rsp
         sub     rsp, 16
         mov     dword ptr [rbp - 4], edi
         cmp     dword ptr [rbp - 4], 5
         jle     .LBB0_2
         mov     edi, 4
         call    malloc@PLT
         mov     qword ptr [rbp - 16], rax
         mov     [rbp+...],
 .LBB0_2:
         mov     rax, qword ptr [rbp - 16]
         cmp
         add     rsp, 16
         pop     rbp
         ret





