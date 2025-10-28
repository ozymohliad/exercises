	.file	"3.c"
	.text
	.globl	decode2
	.type	decode2, @function
decode2:
.LFB11:
	.cfi_startproc
	subq	%rdx, %rsi
	imulq	%rsi, %rdi
	sall	$7, %esi
	sarb	$7, %sil
	movsbq	%sil, %rax
	xorq	%rdi, %rax
	ret
	.cfi_endproc
.LFE11:
	.size	decode2, .-decode2
	.globl	loop
	.type	loop, @function
loop:
.LFB12:
	.cfi_startproc
	movl	%esi, %ecx
	movl	$1, %eax
	movl	$0, %edx
	jmp	.L3
.L4:
	movq	%rax, %r8
	andq	%rdi, %r8
	orq	%r8, %rdx
	salq	%cl, %rax
.L3:
	testq	%rax, %rax
	jne	.L4
	movq	%rdx, %rax
	ret
	.cfi_endproc
.LFE12:
	.size	loop, .-loop
	.globl	cread_alt
	.type	cread_alt, @function
cread_alt:
.LFB13:
	.cfi_startproc
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	%fs:40, %rax
	movq	%rax, 8(%rsp)
	xorl	%eax, %eax
	movq	$0, (%rsp)
	testq	%rdi, %rdi
	je	.L9
.L6:
	movq	(%rdi), %rax
	movq	8(%rsp), %rdx
	subq	%fs:40, %rdx
	jne	.L10
	addq	$24, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 8
	ret
.L9:
	.cfi_restore_state
	movq	%rsp, %rdi
	jmp	.L6
.L10:
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE13:
	.size	cread_alt, .-cread_alt
	.globl	switch3
	.type	switch3, @function
switch3:
.LFB14:
	.cfi_startproc
	cmpl	$4, %edx
	ja	.L12
	movl	%edx, %edx
	leaq	.L14(%rip), %rcx
	movslq	(%rcx,%rdx,4), %rax
	addq	%rcx, %rax
	jmp	*%rax
	.section	.rodata
	.align 4
	.align 4
.L14:
	.long	.L18-.L14
	.long	.L17-.L14
	.long	.L16-.L14
	.long	.L15-.L14
	.long	.L19-.L14
	.text
.L12:
	movl	$12, %eax
	ret
.L18:
	movq	(%rsi), %rax
	movq	(%rdi), %rdx
	movq	%rdx, (%rsi)
	ret
.L17:
	movq	(%rsi), %rax
	addq	(%rdi), %rax
	movq	%rax, (%rdi)
	ret
.L16:
	movq	$59, (%rdi)
.L15:
	movq	(%rsi), %rax
	movq	%rax, (%rdi)
	movl	$27, %eax
	ret
.L19:
	movl	$27, %eax
	ret
	.cfi_endproc
.LFE14:
	.size	switch3, .-switch3
	.globl	switch_prob
	.type	switch_prob, @function
switch_prob:
.LFB15:
	.cfi_startproc
	subq	$60, %rsi
	cmpq	$5, %rsi
	ja	.L21
	leaq	.L23(%rip), %rdx
	movslq	(%rdx,%rsi,4), %rax
	addq	%rdx, %rax
	jmp	*%rax
	.section	.rodata
	.align 4
	.align 4
.L23:
	.long	.L26-.L23
	.long	.L21-.L23
	.long	.L26-.L23
	.long	.L25-.L23
	.long	.L24-.L23
	.long	.L22-.L23
	.text
.L26:
	leaq	0(,%rdi,8), %rax
	ret
.L25:
	movq	%rdi, %rax
	sarq	$3, %rax
	ret
.L24:
	movq	%rdi, %rax
	salq	$4, %rax
	subq	%rdi, %rax
	movq	%rax, %rdi
.L22:
	imulq	%rdi, %rdi
.L21:
	leaq	75(%rdi), %rax
	ret
	.cfi_endproc
.LFE15:
	.size	switch_prob, .-switch_prob
	.globl	proc
	.type	proc, @function
proc:
.LFB16:
	.cfi_startproc
	movq	8(%rdi), %rdx
	movq	(%rdx), %rax
	movq	(%rax), %rax
	subq	8(%rdx), %rax
	movq	%rax, (%rdi)
	ret
	.cfi_endproc
.LFE16:
	.size	proc, .-proc
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"%s"
	.text
	.globl	good_echo
	.type	good_echo, @function
good_echo:
.LFB17:
	.cfi_startproc
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset 6, -16
	pushq	%rbx
	.cfi_def_cfa_offset 24
	.cfi_offset 3, -24
	subq	$56, %rsp
	.cfi_def_cfa_offset 80
	movq	%fs:40, %rax
	movq	%rax, 40(%rsp)
	xorl	%eax, %eax
	jmp	.L32
.L34:
	movq	%rax, %rbp
.L31:
	movq	%rbx, %rsi
	leaq	.LC0(%rip), %rdi
	movl	$0, %eax
	call	printf@PLT
	testq	%rbp, %rbp
	jne	.L29
.L32:
	movq	%rsp, %rdi
	movq	stdin(%rip), %rdx
	movl	$32, %esi
	call	fgets@PLT
	movq	%rax, %rbx
	testq	%rax, %rax
	je	.L29
	movl	$10, %esi
	movq	%rax, %rdi
	call	strchr@PLT
	testq	%rax, %rax
	je	.L34
	leaq	1(%rax), %rbp
	movb	$0, 1(%rax)
	jmp	.L31
.L29:
	movq	40(%rsp), %rax
	subq	%fs:40, %rax
	jne	.L36
	addq	$56, %rsp
	.cfi_remember_state
	.cfi_def_cfa_offset 24
	popq	%rbx
	.cfi_def_cfa_offset 16
	popq	%rbp
	.cfi_def_cfa_offset 8
	ret
.L36:
	.cfi_restore_state
	call	__stack_chk_fail@PLT
	.cfi_endproc
.LFE17:
	.size	good_echo, .-good_echo
	.globl	find_range
	.type	find_range, @function
find_range:
.LFB18:
	.cfi_startproc
	pxor	%xmm1, %xmm1
	comiss	%xmm0, %xmm1
	ja	.L41
	ucomiss	%xmm1, %xmm0
	jp	.L44
	jne	.L44
	movl	$1, %eax
	ret
.L44:
	pxor	%xmm1, %xmm1
	comiss	%xmm1, %xmm0
	jbe	.L46
	movl	$2, %eax
	ret
.L41:
	movl	$0, %eax
	ret
.L46:
	movl	$3, %eax
	ret
	.cfi_endproc
.LFE18:
	.size	find_range, .-find_range
	.globl	find_range1
	.type	find_range1, @function
find_range1:
.LFB19:
	.cfi_startproc
#APP
# 258 "3.c" 1
	vxorps %xmm1, %xmm1, %xmm1
	vucomiss %xmm1, %xmm0
	jp .oth
	jb .neg
	je .zer
	ja .pos
.neg:
	movl $0, %eax
	jmp .done
.zer:
	movl $1, %eax
	jmp .done
.pos:
	movl $2, %eax
	jmp .done
.oth:
	movl $3, %eax
	jmp .done
.done:
# 0 "" 2
#NO_APP
	ret
	.cfi_endproc
.LFE19:
	.size	find_range1, .-find_range1
	.globl	find_range2
	.type	find_range2, @function
find_range2:
.LFB20:
	.cfi_startproc
#APP
# 277 "3.c" 1
	vxorps %xmm1, %xmm1, %xmm1
	vucomiss %xmm1, %xmm0
	movl $0, %r8d
	movl $1, %r9d
	movl $2, %r10d
	movl $3, %r11d
	cmovb %r8d, %eax
	cmove %r9d, %eax
	cmova %r10d, %eax
	cmovp %r11d, %eax
# 0 "" 2
#NO_APP
	ret
	.cfi_endproc
.LFE20:
	.size	find_range2, .-find_range2
	.section	.rodata.str1.1
.LC4:
	.string	"%f + i%f\n"
	.section	.rodata
	.align 8
.LC3:
	.long	1374389535
	.long	1074339512
	.long	0
	.long	1075052544
	.text
	.globl	main
	.type	main, @function
main:
.LFB21:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_def_cfa_offset 16
	movsd	.LC2(%rip), %xmm2
	movsd	.LC3(%rip), %xmm0
	movsd	8+.LC3(%rip), %xmm1
	leaq	.LC4(%rip), %rdi
	movl	$3, %eax
	call	printf@PLT
	movl	$0, %eax
	addq	$8, %rsp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE21:
	.size	main, .-main
	.section	.rodata.cst8,"aM",@progbits,8
	.align 8
.LC2:
	.long	-858993459
	.long	1079577804
	.ident	"GCC: (GNU) 14.1.1 20240507"
	.section	.note.GNU-stack,"",@progbits
