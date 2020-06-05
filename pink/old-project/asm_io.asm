

	global WriteStr
	global ReadStr	
	
	section .text
	
; a const char* and an int are both class INTEGER
; and will be passed in the usual argument registers
;	rdi, rsi, rdx, rcx, r8, r9
; ssize_t WriteStr (const char* buffer, int count)
WriteStr:
	push rbp
	mov  rbp, rsp
	
	; save arguments onto the stack
	mov [rbp+8],   rdi  ; store the buffer address
	mov [rbp+10h], rsi  ; store the count
	
	; ssize_t write(int fd, const void* buf, size_t count)
	mov rax, 1		    ; set syscall# = write (1) 
	mov rdi, 1			; set fd  = stdout (1)
	mov rsi, [rbp+8]	; set rsi = buffer 
	mov rdx, [rbp+10h]  ; set rdx = count
	syscall
	; rax = the return value
	pop rbp
	ret

	
; ssize_t ReadStr (const char* buffer, int count)
ReadStr:
	push rbp
	mov rbp, rsp
	
	; save args onto the stack
	mov [rbp+8],   rdi	
	mov [rbp+10h], rsi
	
	; ssize_t read(int fd, void* dest, int count)
	mov rax, 0 			; set syscall# = read (0)
	mov rdi, 0			; set fd = stdin (0)
	mov rsi, [rbp+8]    ; set rsi = buffer
	mov rdx, [rbp+10]	; set rdx = count
	syscall
	; rax = the return value
	pop rbp
	ret
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	;extern __acrt_iob_func:near
;
	;extern putchar:near
	;extern getchar:near
	;extern puts:near
	;extern fgets:near
;
	;.code
	;
;; extern "C" ReadChar();
;ReadChar proc public
	;push rbp
	;mov rbp, rsp
	;; no args to getchar
	;sub rsp, 20h
	;call getchar
	;add rsp, 20h
	;; rax = the character that was read in.
	;pop rbp
	;ret
;ReadChar endp
;
;; extern "C" int WriteChar(int c);
;; write a single character to stdout
;WriteChar proc public
;; prolouge
	;push rbp
	;mov rbp, rsp
;; NOTE:
;;	x64 windows uses the __fastcall strategy for, everything
;;	in x64, but in this case specifically cstdlib
;;	functions. so then the compiler, in order to call 
;;	WriteChar, loaded the correct registers, and we can just
;;	forward the call to the cstdio func 'putchar'
;; 
;;	despite not needing to be explicit about what is being passed
;;	in the actual code, does not mean it is not important.
;;	so:
;;		rcx = the character to write
;;
;; NOTE:
;;	despite the correct values being loaded, we still need to allocate
;;	enough space for them on the stack, in the so-called "shadow area"
;;   !!this is true even if the callee takes zero arguments!!
;;	!!not doing this smashes the stack!!
	;sub rsp, 20h
	;call putchar
	;add rsp, 20h
;;	rax = the character putchar wrote
	;pop rbp
	;ret
;WriteChar endp
;
;; ---------------------------------------------------------------------------------------------
;
;
;; ---------------------------------------------------------------------------------------------
;
;; extern "C" int puts (const char* str)
;WriteStr proc public
	;push rbp
	;mov rbp, rsp
	;; see above, as to why we don't
	;; need to mess with args
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
	;; push r13, r14, and r15 is to fix a bug where
	;; the Instruction Pointer gets misaligned after
	;; the call to fgets.
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
	;; NOTE: this is considered bad practice according to:
	;;	https://docs.microsoft.com/en-us/cpp/c-runtime-library/internal-crt-globals-and-functions?view=vs-2019
	;;   (last got: 2/26/2019)
	;; however, there really is no other way for me to get a reference to
	;; stdin in the context of x86-64 assembly. so these are the instructions
	;; we are going to go with. in order to view the sequence of x64 instructions
	;; to get a refrence to stdin, I wrote and compiled a simple c function and
	;; view'd the dissassembly.
	;; the c-func in question: 
	;;	char* ReadCStr(char* buffer, int length) {
	;;		return fgets(buffer, length, stdin);
	;;	}
	;; if microsoft changes the internal func name, and thus breaks
	;; this code. my assumption is this same method can be used in the future
	;; to fix the break.
	;xor rcx, rcx
	;xor rdx, rdx
	;sub rsp, 20h
	;call __acrt_iob_func 
	;add rsp, 20h
	;; set up regs and stack, then call fgets
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
