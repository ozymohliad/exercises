	.file	"5.c"
	.text
	.p2align 4
	.globl	basic_memset
	.type	basic_memset, @function
basic_memset:
.LFB22:
	.cfi_startproc
	testq	%rdx, %rdx
	je	.L6
	subq	$8, %rsp
	.cfi_def_cfa_offset 16
	movzbl	%sil, %esi
	call	memset@PLT
	addq	$8, %rsp
	.cfi_def_cfa_offset 8
	ret
	.p2align 4,,10
	.p2align 3
.L6:
	movq	%rdi, %rax
	ret
	.cfi_endproc
.LFE22:
	.size	basic_memset, .-basic_memset
	.p2align 4
	.globl	fast_memset
	.type	fast_memset, @function
fast_memset:
.LFB23:
	.cfi_startproc
	testb	$7, %dil
	je	.L19
	movl	%esi, %ecx
	movq	%rdi, %rax
	.p2align 4
	.p2align 4
	.p2align 3
.L12:
	addq	$1, %rax
	movb	%cl, -1(%rax)
	testb	$7, %al
	jne	.L12
	subq	%rax, %rdx
	addq	%rdi, %rdx
.L11:
	movabsq	$72340172838076673, %rcx
	movzbl	%sil, %r11d
	movzbl	%sil, %esi
	imulq	%rcx, %rsi
	cmpq	$7, %rdx
	jbe	.L13
	leaq	-8(%rdx), %r9
	movq	%rax, %rcx
	movq	%r9, %r10
	shrq	$3, %r10
	leaq	8(%rax,%r10,8), %r8
	andl	$1, %r10d
	jne	.L14
	leaq	8(%rax), %rcx
	movq	%rsi, -8(%rcx)
	cmpq	%r8, %rcx
	je	.L29
	.p2align 4
	.p2align 4
	.p2align 3
.L14:
	movq	%rsi, (%rcx)
	addq	$16, %rcx
	movq	%rsi, -8(%rcx)
	cmpq	%r8, %rcx
	jne	.L14
.L29:
	andq	$-8, %r9
	andl	$7, %edx
	leaq	8(%rax,%r9), %rax
.L13:
	testq	%rdx, %rdx
	je	.L20
	movl	%edx, %ecx
	testl	%edx, %edx
	je	.L20
	xorl	%edx, %edx
.L16:
	movl	%edx, %esi
	addl	$1, %edx
	movb	%r11b, (%rax,%rsi)
	cmpl	%ecx, %edx
	jb	.L16
.L20:
	movq	%rdi, %rax
	ret
	.p2align 4,,10
	.p2align 3
.L19:
	movq	%rdi, %rax
	jmp	.L11
	.cfi_endproc
.LFE23:
	.size	fast_memset, .-fast_memset
	.section	.text.startup,"ax",@progbits
	.p2align 4
	.globl	main
	.type	main, @function
main:
.LFB24:
	.cfi_startproc
	subq	$8, %rsp
	.cfi_def_cfa_offset 16
	movq	stderr(%rip), %rsi
	movl	$69, %edi
	call	fputc@PLT
	xorl	%eax, %eax
	addq	$8, %rsp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE24:
	.size	main, .-main
	.ident	"GCC: (GNU) 14.2.1 20240802"
	.section	.note.GNU-stack,"",@progbits
