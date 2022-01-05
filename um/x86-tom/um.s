	.file	"um.c"
	.section	.rodata.str1.1,"aMS",@progbits,1
.LC0:
	.string	"\nusage:\num program.um\n"
.LC1:
	.string	"rb"
.LC2:
	.string	"can't open %s\n"
	.text
	.p2align 4,,15
.globl main
	.type	main, @function
main:
	leal	4(%esp), %ecx
	andl	$-16, %esp
	pushl	-4(%ecx)
	pushl	%ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	pushl	%ecx
	subl	$152, %esp
	movl	4(%ecx), %ebx
	cmpl	$1, (%ecx)
	jle	.L53
	movl	$.LC1, 4(%esp)
	movl	4(%ebx), %eax
	movl	%eax, (%esp)
	call	fopen
	testl	%eax, %eax
	movl	%eax, %esi
	je	.L54
	leal	32(%esp), %eax
	movl	%eax, 8(%esp)
	movl	4(%ebx), %eax
	movl	$3, (%esp)
	movl	%eax, 4(%esp)
	call	__xstat
	movl	$-1, %edx
	testl	%eax, %eax
	je	.L55
.L4:
	addl	$152, %esp
	movl	%edx, %eax
	popl	%ecx
	popl	%ebx
	popl	%esi
	popl	%edi
	popl	%ebp
	leal	-4(%ecx), %esp
	ret
.L55:
	movl	76(%esp), %ebx
	movl	$4, %edi
	movl	$4, 4(%esp)
	sarl	$2, %ebx
	leal	1(%ebx), %eax
	movl	%eax, (%esp)
	call	calloc
	leal	4(%eax), %ebp
	movl	%ebx, (%eax)
	movl	%ebp, %ebx
	jmp	.L9
	.p2align 4,,7
.L11:
	movl	(%ebx), %eax
	sall	$8, %eax
	orl	%edx, %eax
	movl	%eax, (%ebx)
.L9:
	movl	%esi, (%esp)
	call	fgetc
	cmpl	$-1, %eax
	movl	%eax, %edx
	je	.L56
	subl	$1, %edi
	cmpl	$-1, %edi
	jne	.L11
	addl	$4, %ebx
	movl	$3, %edi
	jmp	.L11
.L56:
	cld
	leal	120(%esp), %edi
	movl	$8, %ecx
	xorl	%eax, %eax
	xorl	%esi, %esi
	rep
	stosl

	/* skip literal for first time */
        jmp    .L50

    /* code for "literal",
       fallthrough to instruction
       decode */

.L28:
	movl	%edi, %eax
	andl	$33554431, %edi
	shrl	$25, %eax
	andl	$7, %eax
	movl	%edi, 120(%esp,%eax,4)
	/* jmp	.L50 */
        
  	.p2align 4,,7
.L50:
	movl	(%ebp,%esi,4), %edi
	addl	$1, %esi
	movl	%edi, %eax
	shrl	$28, %eax
	/* cmpl	$13, %eax */
	/* ja	.L50 */
	jmp	*.L29(,%eax,4)
	.section	.rodata
	.align 4
	.align 4
.L29:
	.long	.L15
	.long	.L16
	.long	.L17
	.long	.L18
	.long	.L19
	.long	.L20
	.long	.L21
	.long	.L22
	.long	.L23
	.long	.L24
	.long	.L25
	.long	.L26
	.long	.L27
	.long	.L28
  /*      .long   .L50  (impossible) */
	.text
.L15:
	movl	%edi, %eax
	andl	$7, %eax
	movl	120(%esp,%eax,4), %eax
	testl	%eax, %eax
	je	.L50
	movl	%edi, %edx
	shrl	$3, %edi
	andl	$7, %edi
	movl	120(%esp,%edi,4), %eax
	shrl	$6, %edx
	andl	$7, %edx
	movl	%eax, 120(%esp,%edx,4)
	jmp	.L50
.L16:
	movl	%edi, %eax
	movl	%edi, %edx
	shrl	$3, %eax
	andl	$7, %eax
	movl	120(%esp,%eax,4), %ecx
	testl	%ecx, %ecx
	cmove	%ebp, %ecx
	andl	$7, %edi
	movl	120(%esp,%edi,4), %eax
	shrl	$6, %edx
	andl	$7, %edx
	movl	(%ecx,%eax,4), %eax
	movl	%eax, 120(%esp,%edx,4)
	jmp	.L50
.L17:
	movl	%edi, %eax
	shrl	$6, %eax
	andl	$7, %eax
	movl	120(%esp,%eax,4), %edx
	movl	%edi, %eax
	testl	%edx, %edx
	cmove	%ebp, %edx
	andl	$7, %edi
	shrl	$3, %eax
	andl	$7, %eax
	movl	120(%esp,%eax,4), %ecx
	movl	120(%esp,%edi,4), %eax
	movl	%eax, (%edx,%ecx,4)
	jmp	.L50
.L18:
	movl	%edi, %edx
	movl	%edi, %ecx
	shrl	$3, %edx
	andl	$7, %edi
	movl	120(%esp,%edi,4), %eax
	andl	$7, %edx
	addl	120(%esp,%edx,4), %eax
	shrl	$6, %ecx
	andl	$7, %ecx
	movl	%eax, 120(%esp,%ecx,4)
	jmp	.L50
.L19:
	movl	%edi, %eax
	movl	%edi, %edx
	shrl	$3, %eax
	andl	$7, %edi
	andl	$7, %eax
	movl	120(%esp,%eax,4), %eax
	shrl	$6, %edx
	imull	120(%esp,%edi,4), %eax
	andl	$7, %edx
	movl	%eax, 120(%esp,%edx,4)
	jmp	.L50
.L20:
	movl	%edi, %eax
	movl	%edi, %ebx
	shrl	$3, %eax
	andl	$7, %edi
	andl	$7, %eax
	xorl	%edx, %edx
	movl	120(%esp,%eax,4), %eax
	shrl	$6, %ebx
	divl	120(%esp,%edi,4)
	andl	$7, %ebx
	movl	%eax, 120(%esp,%ebx,4)
	jmp	.L50
.L21:
	movl	%edi, %edx
	movl	%edi, %ecx
	shrl	$3, %edx
	andl	$7, %edi
	movl	120(%esp,%edi,4), %eax
	andl	$7, %edx
	andl	120(%esp,%edx,4), %eax
	shrl	$6, %ecx
	andl	$7, %ecx
	notl	%eax
	movl	%eax, 120(%esp,%ecx,4)
	jmp	.L50
.L22:
	xorl	%edx, %edx
	jmp	.L4
.L23:
	movl	%edi, %eax
	andl	$7, %eax
	movl	120(%esp,%eax,4), %ebx
	shrl	$3, %edi
	movl	$4, 4(%esp)
	andl	$7, %edi
	leal	1(%ebx), %eax
	movl	%eax, (%esp)
	call	calloc
	movl	%ebx, (%eax)
	addl	$4, %eax
	movl	%eax, 120(%esp,%edi,4)
	jmp	.L50
.L24:
	andl	$7, %edi
	movl	120(%esp,%edi,4), %eax
	subl	$4, %eax
	movl	%eax, (%esp)
	call	free
	jmp	.L50
.L25:
	movl	stdout, %eax
	andl	$7, %edi
	movl	%eax, 4(%esp)
	movl	120(%esp,%edi,4), %eax
	movl	%eax, (%esp)
	call	_IO_putc
	movl	stdout, %eax
	movl	%eax, (%esp)
	call	fflush
	jmp	.L50
.L26:
	movl	stdin, %eax
	andl	$7, %edi
	movl	%eax, (%esp)
	call	_IO_getc
	movl	%eax, 120(%esp,%edi,4)
	jmp	.L50
.L27:
	movl	%edi, %eax
	shrl	$3, %eax
	andl	$7, %eax
	movl	120(%esp,%eax,4), %esi
	testl	%esi, %esi
	jne	.L57
.L37:
	andl	$7, %edi
	movl	120(%esp,%edi,4), %esi
	jmp	.L50

.L54:
	movl	4(%ebx), %eax
	movl	$.LC2, 4(%esp)
	movl	%eax, 8(%esp)
	movl	stderr, %eax
	movl	%eax, (%esp)
	call	fprintf
	movl	$-1, %edx
	jmp	.L4
.L57:
	leal	-4(%ebp), %eax
	movl	%eax, (%esp)
	call	free
	movl	-4(%esi), %ebx
	movl	$4, 4(%esp)
	leal	1(%ebx), %eax
	movl	%eax, (%esp)
	call	calloc
	movl	%ebx, (%eax)
	leal	4(%eax), %ebp
	sall	$2, %ebx
	movl	%ebx, 8(%esp)
	movl	%esi, 4(%esp)
	movl	%ebp, (%esp)
	call	memcpy
	jmp	.L37
.L53:
	movl	stderr, %eax
	movl	$22, 8(%esp)
	movl	$1, 4(%esp)
	movl	$.LC0, (%esp)
	movl	%eax, 12(%esp)
	call	fwrite
	movl	$-1, %edx
	jmp	.L4
	.size	main, .-main
	.ident	"GCC 4.1.0 and Tom7, together at last"
	.section	.note.GNU-stack,"",@progbits
