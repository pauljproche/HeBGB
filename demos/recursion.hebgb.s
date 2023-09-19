	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 12, 0
	.globl	_main                           ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset %rbx, -16
	movl	$16, %edi
	callq	_malloc
	movq	%rax, %rbx
	leaq	_fact(%rip), %rax
	movq	%rax, (%rbx)
	movq	%rbx, 8(%rbx)
	movl	$4, %edi
	callq	_malloc
	movl	$10, (%rax)
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	*(%rbx)
	movl	(%rax), %edi
	callq	_print_int
	movl	$4, %edi
	callq	_malloc
	movl	$0, (%rax)
	xorl	%eax, %eax
	popq	%rbx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_fact                           ## -- Begin function fact
	.p2align	4, 0x90
_fact:                                  ## @fact
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	pushq	%r15
	.cfi_def_cfa_offset 24
	pushq	%r14
	.cfi_def_cfa_offset 32
	pushq	%r12
	.cfi_def_cfa_offset 40
	pushq	%rbx
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
	.cfi_def_cfa_offset 64
	.cfi_offset %rbx, -48
	.cfi_offset %r12, -40
	.cfi_offset %r14, -32
	.cfi_offset %r15, -24
	.cfi_offset %rbp, -16
	movq	%rdi, %r14
	movq	8(%rsi), %r15
	movl	$16, %edi
	callq	_malloc
	movq	%rax, %r12
	leaq	_do_recursive_call(%rip), %rax
	movq	%rax, (%r12)
	movq	%r15, 8(%r12)
	movl	$4, %edi
	callq	_malloc
	movl	$0, (%rax)
	movl	(%r14), %ebp
	xorl	%ebx, %ebx
	testl	%ebp, %ebp
	sete	%bl
	negl	%ebx
	movl	$4, %edi
	callq	_malloc
	testl	%ebp, %ebp
	movl	%ebx, (%rax)
	jne	LBB1_2
## %bb.1:                               ## %do
	movl	$4, %edi
	callq	_malloc
	movl	$1, (%rax)
	jmp	LBB1_3
LBB1_2:                                 ## %else
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	*(%r12)
	movl	(%r14), %ebx
	imull	(%rax), %ebx
	movl	$4, %edi
	callq	_malloc
	movl	%ebx, (%rax)
LBB1_3:                                 ## %merge
	movq	%rax, 8(%rsp)
	movq	8(%rsp), %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_do_recursive_call              ## -- Begin function do_recursive_call
	.p2align	4, 0x90
_do_recursive_call:                     ## @do_recursive_call
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%r14
	.cfi_def_cfa_offset 16
	pushq	%rbx
	.cfi_def_cfa_offset 24
	pushq	%rax
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -24
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movl	$4, %edi
	callq	_malloc
	movl	$1, (%rax)
	movl	(%rbx), %ebx
	decl	%ebx
	movl	$4, %edi
	callq	_malloc
	movl	%ebx, (%rax)
	movq	8(%r14), %rsi
	movq	%rax, %rdi
	callq	*(%rsi)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
	.cfi_endproc
                                        ## -- End function
.subsections_via_symbols
