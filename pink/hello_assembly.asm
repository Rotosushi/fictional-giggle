
	; this is to include the legacy names for
	; the cstdlib
	; the crt is already included apparently
	; includelib ucrtlib 
	; ^^ makes linker unhappy


	extern __acrt_iob_func:near

	extern putchar:near
	extern getchar:near
	extern puts:near
	extern fgets:near


	.code
	
; ---------------------------------------------------------------------------------------------

; extern "C" int CalcSum(int a, int b, int c);
;	returns a + b + c

CalcSum proc public

; initialize the stack frame
; NOTE: since this is 64 bit, the registers
; have a prefixed 'r' instead of the 32 bit
; 'e' prefix, or the 16 bit, no prefix
	push rbp
	mov  rbp, rsp

; load the arguments
; NOTE: win64 uses the FASTCALL calling
; convention which stores the first 
; four arguments in RCX, RDX, R8, R9 respectively
; and then each subsequent parameter is stored
; on the stack. which is decremented by 8 in order
; to maintain word alignment.
; would this change per the size of the arguments?
; TODO: look up how structs are handled
	mov rax,rcx	    ; rax = a
	mov rcx,rdx	; rcx = b
	mov rdx,r8	; rdx = c

; perform the calculation
	add rax,rcx	; rax = rax + b
; the ax register is the return value of the
; function, which is why it is the accumulator
; for the addition.
	add rax,rdx	; rax = rax + c

; restore the stack frame for the caller
	pop rbp
	ret

CalcSum endp

; ---------------------------------------------------------------------------------------------

; extern "C" int IntMul(int a, int b, int* product)
IntMul proc public
; prolouge
	push rbp
	mov  rbp, rsp

; calculate the product and store the result
	imul rcx, rdx ; rcx = rcx * rdx, a *= b
	mov [r8], rcx ; *product = rcx

; epilouge
	pop rbp
	ret
IntMul endp

; ---------------------------------------------------------------------------------------------

; extern "C" int IntDiv(int a, int b, int *quotient, int *remainder)
IntDiv proc public
; prolouge
	push rbp
	mov  rbp, rsp

; ensure that the divisor is not equal to zero
	mov r10, rdx
	or  r10, r10
	jz ZeroDivisor

; calculate a / b and store quotient and remainder
	mov r10, rcx ; rcx holds a
	mov r11, rdx ; rdx holds b
	and rdx, 0   ; rdx:rax makes up the dividend
	mov rax, r11 ; store b in the dividend
	idiv r10	 ; a / b

	mov [r8], rax ; *quotient = rax
	mov [r9], rdx ; *remainder = rdx

; epilouge
ZeroDivisor:
	pop rbp
	ret
IntDiv endp

; ---------------------------------------------------------------------------------------------

; extern "C" int WriteChar(int c);
; write a single character to stdout
WriteChar proc public
; prolouge
	push rbp
	mov rbp, rsp
; x64 windows uses the __fastcall strategy for cstdlib
; functions, then the compiler, in order to call 
; WriteChar, loaded the correct registers, and we can just
; forward the call to the cstdio func 'putchar'
; NOTE:
;	despite the correct values being loaded, we still need to allocate
;	enough space for them on the stack, called the "shadow area"
;   this is true even if the callee takes zero arguments.
;	not doing this smashes the stack
; 
; despite not needing to be explicit about what is being passed
; in the actual code, does not mean it is not important.
; so:
;	rcx = the character to write
	sub rsp, 20h
	call putchar
	add rsp, 20h
;	rax = the character putchar wrote
	pop rbp
	ret
WriteChar endp

; ---------------------------------------------------------------------------------------------

; extern "C" ReadChar();
ReadChar proc public
	push rbp
	mov rbp, rsp
	; no args to getchar
	sub rsp, 20h
	call getchar
	add rsp, 20h
	; rax = the character that was read in.
	pop rbp
	ret
ReadChar endp
end
; ---------------------------------------------------------------------------------------------
;
;; extern "C" int puts (const char* str)
;WriteStr proc public
	;push rbp
	;mov rbp, rsp
	;; rcx = str
	;sub rsp, 20h
	;call puts
	;add rsp, 20h
	;; rax = return value of puts
	;pop rbp
	;ret
;WriteStr endp
;
;; ---------------------------------------------------------------------------------------------
;
	;; extern "C" ReadStr(char* buffer, int buflen)
;;	return fgets(buffer, buflen, stdin)
;ReadStr proc public
	;push rbp
	;push rcx
	;push rdx
	;push r8
	;push r13
	;push r14
	;push r15
	;sub  rsp, 20h
	;mov  rbp, rsp
;
	;; save arguments on the stack
	;mov [rbp+8],   rcx ; save buffer
	;mov [rbp+10h], rdx ; save buflen
;
	;; get a refrence to stdin
	;xor rcx, rcx
	;xor rdx, rdx
	;sub rsp, 20h
	;call __acrt_iob_func
	;add rsp, 20h
	;; 
	;mov r8, rax			; r8  = stdin
	;mov rdx, [rbp+10h]	; rdx = buflen
	;mov rcx, [rbp+8]	; rcx = &buffer
	;sub rsp, 20h
	;call fgets
	;add rsp, 20h
;
	;add rsp, 20h
	;pop r15
	;pop r14
	;pop r13
	;pop r8
	;pop rdx
	;pop rcx
	;pop rbp
	;ret
;ReadStr endp
;
	;end
;




;
;ReadStr proc public
	;; save the args into the shadow area
	;; RCX = string* buffer, RDX = BUFLEN
	;mov [rsp+10h], rdx
	;mov [rsp+8h], rcx
	;push rbp
;
	;mov rbp, rsp
	;
;
	;
;
	;; int i = 0
	;and rbx, 0
;
;ReadStrLoop:
	;; while (i < buflen) {
	;cmp rbx, rdx
	;je ReadStrBreak
;
	;push rcx
	;push rdx
	;; c = getchar
	;sub rsp, 20h
	;call getchar
	;add rsp, 20h
	;; rax = the character that was read in.
	;pop rcx
	;pop rdx
;
	;push rcx
	;push rdx
	;mov rcx, rax
	;sub rsp, 20h
	;call putchar
	;add rsp, 20h
	;; rax = the character that was written.
	;pop rcx
	;pop rdx
;
	;; if (c == '\n') break
	;cmp al, 0Ah
	;je ReadStrBreak
;
	;; if (c == EOF) break
	;cmp al, 0
	;je ReadStrBreak
;
	;; buffer[i] = c
	;; rbx = i
	;; rcx = &buffer
	;mov rcx, qword ptr [rbp+8h]
	;mov byte ptr [rbx+rcx], al
;
;
	;; i += 1
	;add rbx, 1
	;; }
	;jmp ReadStrLoop
;
;
;ReadStrBreak:
	;; always pop in reverse order of pushing
	;pop r11
	;pop rbx
	;pop rbp
	;ret
;ReadStr endp
