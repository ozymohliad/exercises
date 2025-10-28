// -- 4.45 ----------------- 
// subq $8, %rsp
// movq REG, (%rsp)
//
// A. No, pushq %rsp will store the decremented value of %rsp on the stack
// B. movq REG, -8(%rsp)
//    subq $8, %rsp
// ------------------------- 


// -- 4.46 ----------------- 
// movq (%rsp), REG
// addq $8, %rsp
//
// A. No, popq %rsp will add 8 to the popped value
// B. addq %8, %rsp
//    movq -8(%rsp), REG
// ------------------------- 


// -- 4.47 - 4.49 ---------- 
.globl bubble_sort
bubble_sort:
    testq %rsi, %rsi
out_start:
    subq $1, %rsi
    jle out_end
    xorq %rax, %rax
in_start:
    cmpq %rax, %rsi
    jle in_end
    movq (%rdi, %rax, 8), %r10
    movq 8(%rdi, %rax, 8), %r11

// Jump
    // cmpq %r10, %r11
    // jg no_swap
    // xorq %r10, %r11
    // xorq %r11, %r10
    // xorq %r10, %r11
    // no_swap:

// OR Three conditional moves
    // cmpq %r10, %r11
    // cmovl %r10, %r12
    // cmovl %r11, %r10
    // cmovl %r12, %r11

// OR One conditional move
    movq %r10, %r12
    xorq %r11, %r12
    xorq %r13, %r13
    cmpq %r10, %r11
    cmovg %r13, %r12
    xorq %r12, %r10
    xorq %r12, %r11

    movq %r10, (%rdi, %rax, 8)
    movq %r11, 8(%rdi, %rax, 8)
    addq $1, %rax
    jmp in_start
in_end:
    jmp out_start
out_end:
    ret
// ------------------------- 


// -- 4.50 ----------------- 
// ------------------------- 


// -- 4.51 ----------------- 
// Stage            iaddq V, rB
// -------------------------------------------------
// Fetch            icode:ifun <- M_1[PC]
//                  rA:rB      <- M_1[PC+1]
//                  valC       <- M_8[PC+2]
//                  valP       <- PC + 10
//
// Decode           valB       <- R[rB]
//
// Execute          valE       <- valB + valC
//                  Set CC
//
// Memory
//
// Write back       R[rB]      <- valE
//
// PC update        PC         <- valP
// ------------------------- 
