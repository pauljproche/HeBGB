	.section	__TEXT,__text,regular,pure_instructions
	.build_version macos, 12, 0
	.globl	_main                           ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%r15
	.cfi_def_cfa_offset 16
	pushq	%r14
	.cfi_def_cfa_offset 24
	pushq	%rbx
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -32
	.cfi_offset %r14, -24
	.cfi_offset %r15, -16
	movl	$8, %edi
	callq	_malloc
	movq	%rax, %rbx
	leaq	_add_three(%rip), %rax
	movq	%rax, (%rbx)
	movl	$4, %edi
	callq	_malloc
	movq	%rax, %r14
	movl	$3, (%rax)
	movl	$4, %edi
	callq	_malloc
	movq	%rax, %r15
	movl	$2, (%rax)
	movl	$4, %edi
	callq	_malloc
	movl	$1, (%rax)
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	*(%rbx)
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	*(%rax)
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	*(%rax)
	movl	(%rax), %edi
	callq	_print_int
	movl	$4, %edi
	callq	_malloc
	movl	$0, (%rax)
	xorl	%eax, %eax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_add_three                      ## -- Begin function add_three
	.p2align	4, 0x90
_add_three:                             ## @add_three
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbx
	.cfi_def_cfa_offset 16
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movl	$16, %edi
	callq	_malloc
	leaq	_add_two(%rip), %rcx
	movq	%rcx, (%rax)
	movq	%rbx, 8(%rax)
	popq	%rbx
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_add_two                        ## -- Begin function add_two
	.p2align	4, 0x90
_add_two:                               ## @add_two
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
	movq	%rdi, %rbx
	movq	8(%rsi), %r14
	movl	$24, %edi
	callq	_malloc
	leaq	_add_one(%rip), %rcx
	movq	%rcx, (%rax)
	movq	%rbx, 8(%rax)
	movq	%r14, 16(%rax)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
	.cfi_endproc
                                        ## -- End function
	.globl	_add_one                        ## -- Begin function add_one
	.p2align	4, 0x90
_add_one:                               ## @add_one
	.cfi_startproc
## %bb.0:                               ## %entry
	pushq	%rbp
	.cfi_def_cfa_offset 16
	pushq	%rbx
	.cfi_def_cfa_offset 24
	pushq	%rax
	.cfi_def_cfa_offset 32
	.cfi_offset %rbx, -24
	.cfi_offset %rbp, -16
	movq	%rdi, %rbx
	movq	8(%rsi), %rax
	movq	16(%rsi), %rcx
	movl	(%rcx), %ebp
	addl	(%rax), %ebp
	movl	$4, %edi
	callq	_malloc
	movl	%ebp, (%rax)
	addl	(%rbx), %ebp
	movl	$4, %edi
	callq	_malloc
	movl	%ebp, (%rax)
	addq	$8, %rsp
	popq	%rbx
	popq	%rbp
	retq
	.cfi_endproc
                                        ## -- End function
.subsections_via_symbols
