[org 0x0100]

    jmp start

orig_sp:    dw 0
oldtrapisr: dd 0
oldkbisr:   dd 0
oldretisr:  dd 0
filepath:   times 128 db 0
childseg:   dw 0

flag:       db 0
names:      db 'FL =CS =IP =BP =AX =BX =CX =DX =SI =DI =DS =ES ='

kbisr:
    push ax

    in al, 60h
    test al, 80h
    jnz skipflag
    add byte [cs:flag], al

skipflag:
    mov al, 20h
    out 20h, al

    pop ax
    iret

trapisr:
    push bp
    mov bp, sp

    pusha
    push ds
    push es

    sti
    push cs
    pop ds

    mov byte [flag], 0
    call clrscrn

    mov si, 6
    mov cx, 12
    mov ax, 0
    mov bx, 5

l3:
    push ax
    push bx
    mov dx, [bp + si]
    push dx
    call printnum
    sub si, 2
    inc ax
    loop l3

    mov ax, 0
    mov bx, 0
    mov cx, 12
    mov si, 4
    mov dx, names

l1:
    push ax
    push bx
    push dx
    push si
    call printstr
    add dx, 4
    inc ax
    loop l1

keywait:
    cmp byte [flag], 0
    je keywait

    pop es
    pop ds
    popa

    pop bp
    iret

hookISR: 
    push ax
    push es

    xor ax, ax
    mov es, ax

    mov ax, [es:1 * 4]
    mov [oldtrapisr], ax
    mov ax, [es:1 * 4 + 2]
    mov [oldtrapisr + 2], ax
    
    mov ax, [es:9 * 4]
    mov [oldkbisr], ax
    mov ax, [es:9 * 4 + 2]
    mov [oldkbisr + 2], ax

    mov ax, [es:22 * 4]
    mov [oldretisr], ax
    mov ax, [es:22 * 4 + 2]
    mov [oldretisr + 2], ax

    cli
    mov word [es:1 * 4], trapisr
    mov [es:1 * 4 + 2], cs

    mov word [es:9 * 4], kbisr
    mov [es:9 * 4 + 2], cs

    mov word [es:22 * 4], return_to_parent
    mov [es:22 * 4 + 2], cs
    sti

    pop es
    pop ax

    ret

unhookISR:
    push ax
    push es

    xor ax, ax
    mov es, ax

    cli
    mov ax, [oldtrapisr]
    mov [es:1 * 4], ax
    mov ax, [oldtrapisr + 2]
    mov [es:1 * 4 + 2], ax

    mov ax, [oldkbisr]
    mov [es:9 * 4], ax
    mov ax, [oldkbisr + 2]
    mov [es:9 * 4 + 2], ax
    
    mov ax, [oldretisr]
    mov [es:22 * 4], ax
    mov ax, [oldretisr + 2]
    mov [es:22 * 4 + 2], ax
    sti

    pop es
    pop ax

    ret

start:
    ;-----hook interrupt service routines-----
    call hookISR

    ;-----read file path-----
    xor cx, cx
    mov cl, [80h]
    cmp cl, 1
    jl no_filepath

    ; remove trailing spaces
    dec cl
    mov si, 82h
    mov di, filepath
    rep movsb

    ;-----resizing memory for debugger-----
    mov bx, terminate
    add bx, 20
    shr bx, 4

    mov ax, cs
    mov es, ax
    mov ah, 4Ah
    int 21h

    ;-----allocating memory for child process-----
    mov ax, 4800h
    mov bx, 1000h
    int 21h
    jc no_memory_available
    mov [childseg], ax

    ;-----creating psp for child process-----
    mov ah, 55h
    mov dx, [childseg]
    mov si, 0
    int 21h

    ;-----open .COM file-----
    mov ax, 3D00h
    mov dx, filepath
    int 21h
    jc file_not_found

    ;-----load .COM file into memory-----
    push ds

    mov bx, ax
    mov ah, 3Fh
    mov cx, 0FFFFh
    mov dx, 100h
    mov ds, [childseg]
    int 21h
    jc file_not_read

    pop ds

    ;-----close .COM file-----
    mov ah, 3Eh
    int 21h

    ;-----set up child process stack-----
    mov [orig_sp], sp

    mov ss, [childseg]
    mov sp, 0FFFEh

    pushf
    pop ax
    or ax, 0100h
    push ax
    push word [childseg]
    push word 0100h

    iret

    ;-----come back to original process-----
return_to_parent:
    ; reset parent's registers
    mov ax, cs
    mov ds, ax
    mov ss, ax
    mov sp, [orig_sp]

    call unhookISR

    jmp terminate

;-----error handling-----
no_filepath:
    jmp no_filepath

file_not_found:
    jmp file_not_found

no_memory_available:
    jmp no_memory_available

file_not_read:
    jmp file_not_read



printstr: push bp
mov bp, sp
push es
push ax
push bx
push cx
push dx
push si
push di
mov ax, 0xb800
mov es, ax ; point es to video base
mov di, 80 ; load di with columns per row
mov ax, [bp+10] ; load ax with row number
mul di ; multiply with columns per row
mov di, ax ; save result in di
add di, [bp+8] ; add column number
shl di, 1 ; turn into byte count
mov si, [bp+6] ; string to be printed
mov cx, [bp+4] ; length of string
mov ah, 0x07 ; normal attribute is fixed
nextchar: mov al, [si] ; load next char of string
mov [es:di], ax ; show next char on screen
add di, 2 ; move to next screen location
add si, 1 ; move to next char
loop nextchar ; repeat the operation cx times
pop di
pop si
pop dx
pop cx
pop bx
pop ax
pop es
pop bp
ret 8


printnum: push bp
mov bp, sp
push es
push ax
push bx
push cx
push dx
push di
mov di, 80 ; load di with columns per row
mov ax, [bp+8] ; load ax with row number
mul di ; multiply with columns per row
mov di, ax ; save result in di
add di, [bp+6] ; add column number
shl di, 1 ; turn into byte count
add di, 8 ; to end of number location
mov ax, 0xb800
mov es, ax ; point es to video base
mov ax, [bp+4] ; load number in ax
mov bx, 16 ; use base 16 for division
mov cx, 4 ; initialize count of digits
nextdigit: mov dx, 0 ; zero upper half of dividend
div bx ; divide by 10
add dl, 0x30 ; convert digit into ascii value
cmp dl, 0x39 ; is the digit an alphabet
jbe skipalpha ; no, skip addition
add dl, 7 ; yes, make in alphabet code
skipalpha: mov dh, 0x07 ; attach normal attribute
mov [es:di], dx ; print char on screen
sub di, 2 ; to previous screen location
loop nextdigit ; if no divide it again
pop di
pop dx
pop cx
pop bx
pop ax
pop es
pop bp
ret 6


clrscrn: push es
push ax
push di
mov ax, 0xb800
mov es, ax ; point es to video base
mov di, 0 ; point di to top left column
nextloc: mov word [es:di], 0x0720 ; clear next char on screen
add di, 2 ; move to next screen location
cmp di, 4000 ; has the whole screen cleared
jne nextloc ; if no clear next position
pop di
pop ax
pop es
ret



terminate:
    mov ax, 4C00h
    int 21h