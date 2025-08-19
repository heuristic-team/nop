
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
