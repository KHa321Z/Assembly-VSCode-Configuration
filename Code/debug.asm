[org 0x0100]

    jmp start

orig_ss:    dw 0
orig_sp:    dw 0
oldkbisr:   dd 0
oldtrapisr: dd 0
filepath:   times 128 db 0
childseg:   dw 0

kbisr:
    ret

trapisr:
    ret

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

    cli
    mov word [es:1 * 4], trapisr
    mov [es:1 * 4 + 2], cs
    mov word [es:9 * 4], kbisr
    mov [es:9 * 4 + 2], cs
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
    sti

    pop es
    pop ax

    ret

start:
    ;-----hook interrupt service routines-----
    ; call hookISR

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
    mov ah, 26h
    mov dx, [childseg]
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
    mov dx, [childseg]
    mov ds, dx
    mov dx, 100h
    int 21h
    jc file_not_read

    pop ds

    ;-----close .COM file-----
    mov ah, 3Eh
    int 21h

    ;-----set up child process environment-----
    mov ax, [childseg]
    mov es, ax

    ; setting return address for child process
    mov word [es:0Ah], return_to_parent
    mov word [es:0Ch], cs
    mov word [es:0Eh], return_to_parent
    mov word [es:10h], cs
    mov word [es:12h], return_to_parent
    mov word [es:14h], cs
    ; setting parent psp
    mov word [es:16h], cs

    ; setting child process stack
    mov [orig_ss], ss
    mov [orig_sp], sp

    mov ss, ax
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
    mov sp, [orig_sp]
    mov ax, [orig_ss]
    mov ss, ax

    jmp terminate

no_filepath:
    jmp no_filepath

file_not_found:
    jmp file_not_found

no_memory_available:
    jmp no_memory_available

file_not_read:
    jmp file_not_read



terminate:
    mov ax, 4C00h
    int 21h