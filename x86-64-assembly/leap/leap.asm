section .text
global leap_year

; courtesy https://exercism.org/tracks/x86-64-assembly/exercises/leap/solutions/Silva97
%macro	divisibleby 2
	mov eax, %1
	cdq		; convert double to quad, i.e. clear EDX
	mov ebx, %2
	div ebx		; quotient in EAX, remainder in EDX
	cmp edx, 0
%endmacro

leap_year:
	; argument in register edi
	divisibleby edi, 0d4
	jne false

	divisibleby edi, 0d100
	jne true

	divisibleby edi, 0d400
	jne false

true:
	mov eax, 1
	jmp return
false:
	mov eax, 0
return:
	ret

%ifidn __OUTPUT_FORMAT__,elf64
section .note.GNU-stack noalloc noexec nowrite progbits
%endif
