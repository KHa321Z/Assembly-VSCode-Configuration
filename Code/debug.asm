; CHECK ALL PACKET INPUTS
; CHECK ALL PACKET OUTPUTS
[org 0x0100]

    jmp start

%define ARRAY_SIZE 512
%define MCR 0x03FC
%define IER 0x03F9

orig_sp:        dw 0
oldtrapisr:     dd 0
oldbrkisr:      dd 0
oldcomisr:      dd 0
oldretisr:      dd 0
filepath:       times 128 db 0
childseg:       dw 0

names:          db 'FL =CS =IP =BP =AX =BX =CX =DX =SI =DI =DS =ES ='

regs:           times 14 dw 0
chksum:         db 0
packet:         times ARRAY_SIZE db 0
packettail:     dw packet
inprocessing:   db 0

availpacks:     db 'q', '?', 's', 'c', 'p', 'g', 'Z', 'z', 'm', 'X'
addresspacks:   dw gdb_unknown, gdb_why, gdb_debugger, gdb_debugger, gdb_extract_register, gdb_send_registers, gdb_set_breakpoint, gdb_remove_breakpoint, gdb_read_memory, gdb_write_memory
packslength:    dw ($ - addresspacks) / 2

supportPack:    db 'qSupported', 0
contPack:       db 'vCont?', 0
mustreplyPack:  db 'vMustReplyEmpty', 0
multiPack:      db 'Hg0', 0
threadPack:     db 'qfThreadInfo', 0
endthreadPack:  db 'qsThreadInfo', 0
attachedPack:   db 'qAttached', 0
currthreadPack: db 'Hc-1', 0
querycurrPack:  db 'qC', 0

nothing:        db '$#00', 0
okreply:        db '$OK#9a', 0
errorreply:     db '$E01#xx', 0
stopreply:      db '$S05#b8', 0
singletreply:   db '$m1#9e', 0
endlistreply:   db '$l#6c', 0
childkillreply: db '$0#30', 0
currthreply:    db '$QC1#c5', 0

opcodesize:     dw 0
opcodes:        times ARRAY_SIZE db 0
opcodespos:     times ARRAY_SIZE dw 0


push_opcode:
    ; [bp + 4] - address of breakpoint
    push bp
    mov bp, sp

    push ax
    push cx
    push si
    push di
    push es

    push ds
    pop es

    mov ax, [bp + 4]
    mov cx, [opcodesize]
    mov di, opcodespos

    jcxz skip_push_search

    cmp cx, 256
    jae push_opcode_error

    cld
    repne scasw
    jz push_opcode_error

skip_push_search:
    mov si, ax
    mov di, [opcodesize]
    mov es, [childseg]

    ; opcode replaced for breakpoint in child process
    mov al, [es:si]
    mov byte [es:si], 0CCh
    ; opcode stored in array for reference
    mov [opcodes + di], al
    shl di, 1
    mov [opcodespos + di], si
    inc word [opcodesize]

    clc
    jmp done_push

push_opcode_error:
    stc

done_push:
    pop es
    pop di
    pop si
    pop cx
    pop ax

    pop bp
    ret 2

remove_opcode:
    ; [bp + 4] - address of breakpoint
    push bp
    mov bp, sp

    push ax
    push cx
    push si
    push di
    push es

    push ds
    pop es

    mov ax, [bp + 4]
    mov cx, [opcodesize]
    mov di, opcodespos

    jcxz remove_opcode_error

    cld
    repne scasw
    jz found_opcode_address

    jmp remove_opcode_error

found_opcode_address:
    sub di, 2
    mov si, [opcodesize]
    sub si, cx
    add si, opcodes - 1
    mov es, [childseg]

    push di
    mov di, ax
    cld

    ; opcode replaced for original
    lodsb
    stosb

    ; remove opcode and shift the array
    push ds
    pop es

    ; shift the opcodes array
    push cx

    mov di, si
    dec di
    rep movsb
    ; shift the address array
    pop cx
    pop di

    mov si, di
    add si, 2
    rep movsw

    dec word [opcodesize]
    clc
    jmp done_remove

remove_opcode_error:
    stc

done_remove:
    pop es
    pop di
    pop si
    pop cx
    pop ax

    pop bp
    ret 2


trapisr:
    push bp
    mov bp, sp

    pusha
    push ds
    push es

    sti
    push cs
    pop ds

    call printdebug
    call save_registers
    call wait_packet

    pop es
    pop ds
    popa

    pop bp
    iret

brkisr:
    push bp
    mov bp, sp

    pusha
    push ds
    push es

    sti
    push cs
    pop ds

    ; mov ax, [bp + 4]
    ; mov es, ax
    ; dec word [bp + 2]
    ; mov di, [bp + 2]
    ; mov word [opcodepos], di
    ; mov al, [opcode]
    ; mov [es:di], al

    call printdebug
    call save_registers
    call wait_packet

    pop es
    pop ds
    popa

    pop bp
    iret

save_registers:
    push ax

    mov ax, [bp - 2]
    mov [regs + 0], ax  ; AX
    mov ax, [bp - 8]
    mov [regs + 2], ax  ; BX
    mov ax, [bp - 4]
    mov [regs + 4], ax  ; CX
    mov ax, [bp - 6]
    mov [regs + 6], ax  ; DX
    mov ax, [bp - 14]
    mov [regs + 8], ax  ; SI
    mov ax, [bp - 16]
    mov [regs + 10], ax ; DI
    mov ax, [bp]
    mov [regs + 12], ax ; BP
    mov ax, [bp - 10]
    sub ax, 8
    mov [regs + 14], ax ; SP
    mov ax, [bp + 2]
    mov [regs + 16], ax ; IP
    mov ax, [bp + 6]
    mov [regs + 18], ax ; FLAGS
    mov ax, [bp + 4]
    mov [regs + 20], ax ; CS
    mov ax, [bp - 18]
    mov [regs + 22], ax ; DS
    mov ax, [bp - 20]
    mov [regs + 24], ax ; ES
    mov [regs + 26], ss ; SS

    pop ax
    ret

wait_packet:
    cmp byte [inprocessing], 0
    je wait_packet

    cmp byte [packet + 1], 's'
    je packet_step

    cmp byte [packet + 1], 'c'
    je packet_continue

    jmp wait_packet

packet_step:
    or word [bp + 6], 0100h

    jmp wait_packet_done

packet_continue:
    and word [bp + 6], 0FEFFh

wait_packet_done:
    mov byte [inprocessing], 0

    ret


send_byte:
    push bp
    mov bp, sp
    push ax
    push dx

testline:
    mov ah, 3
    xor dx, dx
    int 0x14

    and ah, 32
    jz testline

    mov al, [bp + 4]
    mov dx, 0x3F8
    out dx, al

    pop dx
    pop ax
    pop bp
    ret 2

send_hex:
    ; [bp + 4] - value to send / return value (checksum)
    push bp
    mov bp, sp
    push ax
    push dx

    xor ax, ax
    xor dx, dx

    mov al, 0F0h
    and al, [bp + 4]
    shr al, 4

    push ax
    call convert_to_ascii
    pop ax
    add dl, al
    push ax
    call send_byte

    mov ax, 0xF
    and al, [bp + 4]
    push ax
    call convert_to_ascii
    pop ax
    add dl, al
    push ax
    call send_byte

    mov [bp + 4], dl

    pop dx
    pop ax
    pop bp
    ret

send_reply:
    push bp
    mov bp, sp
    push ax
    push si
    
    xor ax, ax
    mov si, [bp + 4]

replyloop:
    mov al, [si]
    push ax
    call send_byte

    inc si
    cmp byte [si], 0
    jnz replyloop

    pop si
    pop ax
    pop bp
    ret 2

comisr:
    push bp
    mov bp, sp

    pusha
    push ds
    push es

    push cs
    push cs
    pop ds
    pop es

    mov dx, 0x3FA
    in al, dx
    and al, 0x0F

    cmp al, 4
    jne nodata

    mov dx, 0x3F8
    in al, dx

    cmp al, '$'
    jne insidepacket

    mov word [packettail], packet
    mov byte [chksum], 0

    jmp storepacket

insidepacket:
    cmp al, '#'
    je chksumstarted

    cmp byte [chksum], 0
    je storepacket

chksumstarted:
    inc byte [chksum]

storepacket:
    mov bx, [packettail]
    mov [bx], al

    inc word [packettail]

    ; check if packet is complete
    cmp byte [chksum], 3
    jne nodata

    ; send acknowledgment
    push word '+'
    call send_byte

    ; setting packet processing flag
    mov byte [inprocessing], 1

    push word 20
    push word 0
    push word packet
    push word 20
    call printstr

    ; process the received packet
    call packet_processor

nodata: 
    mov al, 0x20
    out 0x20, al

    pop es
    pop ds
    popa
    
    pop bp
    iret

packet_processor:
    mov al, [packet + 1]
    mov di, availpacks
    mov cx, [packslength]

    ; check general packets
    push word supportPack
    call check_packet
    jz gdb_unknown

    push word contPack
    call check_packet
    jz gdb_unknown

    push word mustreplyPack
    call check_packet
    jz gdb_unknown

    push word multiPack
    call check_packet
    jz gdb_ok

    push word threadPack
    call check_packet
    jz gdb_single_thread

    push word endthreadPack
    call check_packet
    jz gdb_end_list

    push word attachedPack
    call check_packet
    jz gdb_child_process

    push word currthreadPack
    call check_packet
    jz gdb_ok

    push word querycurrPack
    call check_packet
    jz gdb_curr_thread

    ; find respective packet type
    cld
    repne scasb
    jz found_packet

    jmp gdb_unknown

found_packet:
    sub di, availpacks + 1
    shl di, 1
    add di, addresspacks

    jmp [di]

terminate_packet_processing:
    ; toggle packet processing flag
    xor byte [inprocessing], 1

    ret

gdb_unknown:
    push word nothing
    call send_reply

    jmp terminate_packet_processing

gdb_ok:
    push word okreply
    call send_reply

    jmp terminate_packet_processing

gdb_single_thread:
    push word singletreply
    call send_reply

    jmp terminate_packet_processing

gdb_end_list:
    push word endlistreply
    call send_reply

    jmp terminate_packet_processing

gdb_child_process:
    push word childkillreply
    call send_reply

    jmp terminate_packet_processing

gdb_curr_thread:
    push word currthreply
    call send_reply

    jmp terminate_packet_processing

gdb_why:
    push word stopreply
    call send_reply

    jmp terminate_packet_processing

gdb_debugger:
    ; toggle flag beforehand so that it remains on and is handle by debugger isrs
    xor byte [inprocessing], 1

    jmp terminate_packet_processing

gdb_extract_register:
    push word packet + 2
    push word 1
    call extract_hex
    pop di

    shl di, 1
    xor ax, ax
    xor bx, bx

    push word '$'
    call send_byte

    mov al, [regs + di]
    push ax
    call send_hex
    pop bx

    mov al, [regs + di + 1]
    push ax
    call send_hex
    pop ax

    add al, bl 

    push word '#'
    call send_byte
    push ax
    call send_hex
    pop ax

    jmp terminate_packet_processing

gdb_send_registers:
    push word '$'
    call send_byte

    xor ax, ax
    xor bx, bx
    xor cx, cx
    xor dx, dx

nextreg:
    mov al, [regs + bx]
    push ax
    call send_hex
    pop ax
    
    add dl, al
    inc bx

    cmp bx, 14 * 2
    jne nextreg

    ; checksum
    push word '#'
    call send_byte
    push dx
    call send_hex
    pop dx

    jmp terminate_packet_processing

gdb_set_breakpoint:
    push word packet + 4
    push word 4
    call extract_hex

    call push_opcode
    jc set_breakpoint_error

    push word okreply
    call send_reply

    jmp terminate_packet_processing

set_breakpoint_error:
    push word errorreply
    call send_reply

    jmp terminate_packet_processing

gdb_remove_breakpoint:
    push word packet + 4
    push word 4
    call extract_hex

    call remove_opcode
    jc remove_breakpoint_error

    push word okreply
    call send_reply

    jmp terminate_packet_processing

remove_breakpoint_error:
    push word errorreply
    call send_reply

    jmp terminate_packet_processing

gdb_read_memory:
    mov al, '#'
    mov di, packet + 11
    mov cx, 5

    cld
    repne scasb
    sub di, packet + 12
    mov cx, di

    push word packet + 6
    push word 4
    call extract_hex
    pop di

    push word packet + 11
    push cx
    call extract_hex
    pop cx

    xor ax, ax
    xor dx, dx
    mov es, [childseg]

    push word '$'
    call send_byte

read_memory_loop:
    mov al, [es:di]
    push ax
    call send_hex
    pop ax

    add dl, al
    inc di

    loop read_memory_loop

    push word '#'
    call send_byte
    push dx
    call send_hex
    pop dx

    jmp terminate_packet_processing

gdb_write_memory:
    mov al, ':'
    mov di, packet + 7
    mov cx, 5

    cld
    repne scasb
    mov bx, di
    sub di, packet + 8
    mov cx, di

    push word packet + 2
    push word 4
    call extract_hex
    pop di

    push word packet + 7
    push cx
    call extract_hex
    pop cx

    xor ax, ax
    xor dx, dx
    mov es, [childseg]

write_memory_loop:
    push bx
    push word 2
    call extract_hex
    pop ax

    stosb
    inc bx

    loop write_memory_loop

    jmp terminate_packet_processing


hookISR: 
    push ax
    push es

    xor ax, ax
    mov es, ax

    ; saving original ISRs

    ; single step trap
    ; (int 1h)
    mov ax, [es:0x1 * 4]
    mov [oldtrapisr], ax
    mov ax, [es:0x1 * 4 + 2]
    mov [oldtrapisr + 2], ax
    ; breakpoint trap
    ; (int 3h)
    mov ax, [es:0x3 * 4]
    mov [oldbrkisr], ax
    mov ax, [es:0x3 * 4 + 2]
    mov [oldbrkisr + 2], ax
    ; COM port interrupt
    ; (int 0Ch)
    mov ax, [es:0xC * 4]
    mov [oldcomisr], ax
    mov ax, [es:0xC * 4 + 2]
    mov [oldcomisr + 2], ax
    ; return to parent process interrupt
    ; (int 22h)
    mov ax, [es:0x22 * 4]
    mov [oldretisr], ax
    mov ax, [es:0x22 * 4 + 2]
    mov [oldretisr + 2], ax

    ; hooking ISRs

    cli

    ; single step trap
    mov word [es:0x1 * 4], trapisr
    mov [es:0x1 * 4 + 2], cs
    ; breakpoint trap
    mov word [es:0x3 * 4], brkisr
    mov [es:0x3 * 4 + 2], cs
    ; COM port interrupt
    mov word [es:0xC * 4], comisr
    mov [es:0xC * 4 + 2], cs
    ; return to parent process interrupt
    mov word [es:0x22 * 4], return_to_parent
    mov [es:0x22 * 4 + 2], cs

    ; enabling interrupts

    ; enable OUT2
    mov dx, MCR
    in al, dx
    or al, 8 ; enable bit 3 (OUT2)
    out dx, al
    ; enable IER
    mov dx, IER
    in al, dx
    or al, 1
    out dx, al
    ; enable PIC 
    in al, 0x21
    and al, 0xEF
    out 0x21, al

    sti

    pop es
    pop ax

    ret

unhookISR:
    push ax
    push es

    xor ax, ax
    mov es, ax

    ; restoring original ISRs

    cli

    ; single step trap
    ; (int 1h)
    mov ax, [oldtrapisr]
    mov [es:0x1 * 4], ax
    mov ax, [oldtrapisr + 2]
    mov [es:0x1 * 4 + 2], ax
    ; breakpoint trap
    ; (int 3h)
    mov ax, [oldbrkisr]
    mov [es:0x3 * 4], ax
    mov ax, [oldbrkisr + 2]
    mov [es:0x3 * 4 + 2], ax
    ; COM port interrupt
    ; (int 0Ch)
    mov ax, [oldcomisr]
    mov [es:0xC * 4], ax
    mov ax, [oldcomisr + 2]
    mov [es:0xC * 4 + 2], ax
    ; return to parent process interrupt
    ; (int 22h)
    mov ax, [oldretisr]
    mov [es:0x22 * 4], ax
    mov ax, [oldretisr + 2]
    mov [es:0x22 * 4 + 2], ax

    ; disabling interrupts

    ; disable OUT2
    mov dx, MCR
    in al, dx
    and al, 0xF7 ; disable bit 3 (OUT2)
    out dx, al
    ; disable IER
    mov dx, IER
    xor al, al
    out dx, al
    ; disable PIC 
    in al, 0x21
    or al, 0x10
    out 0x21, al

    sti

    pop es
    pop ax

    ret

start:
    ;-----initialize COM port for debugging-----
    mov ah, 0
    mov al, 0E3h
    xor dx, dx
    int 14h

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
    cld
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


extract_hex:
    ; Parameters:
    ; [bp + 6] - address to extract
    ; [bp + 4] - length (max 8 for 32 bit address)

    ; Returns:
    ; [bp + 6] - higher word of 32 bit address
    ; [bp + 4] - lower word of 32 bit address

    push bp
    mov bp, sp

    push ax
    push cx
    push si

    mov cx, [bp + 4]
    mov si, [bp + 6]
    mov word [bp + 6], 0

extract_next_hex:
    mov al, [si]
    sub al, 0x30

    cmp al, 10
    jb skip_extract_char

    sub al, 0x27

skip_extract_char:
    shl word [bp + 6], 4
    add [bp + 6], al

    loop extract_next_hex

    pop si
    pop cx
    pop ax

    pop bp
    ret 2


convert_to_ascii:
    push bp
    mov bp, sp
    
    cmp byte [bp + 4], 10
    jl hex_digit

    add byte [bp + 4], 0x27

hex_digit:
    add byte [bp + 4], 0x30

    pop bp
    ret


; converts a 32 bit physical address to a logical address in segment:offset format
convert_physical_to_logical:
    ; Parameters:
    ; [bp + 6] - higher word of 32 bit physical address
    ; [bp + 4] - lower word of 32 bit physical address

    ; Returns:
    ; [bp + 6] - segment of logical address
    ; [bp + 4] - offset of logical address

    push bp
    mov bp, sp

    push ax
    push dx

    mov ax, [bp + 6]
    mov dx, [bp + 8]
    mov cx, 4

    ; mask to 20 bits
    and dx, 0x000F

segment_shift_loop:
    shr dx, 1
    rcr ax, 1
    loop segment_shift_loop

    mov [bp + 6], ax ; segment
    and [bp + 4], 0xFFFF ; keep only lower 4 bits of offset

    pop dx
    pop ax

    pop bp
    ret


; doesn't handle substrings
; returns answer in zero flag
check_packet:
    ; Parameters:
    ; [bp + 4] - packet to check against

    push bp
    mov bp, sp

    push ax
    push cx
    push si
    push di
    push es

    push ds
    pop es

    mov al, 0
    mov cx, ARRAY_SIZE
    mov di, [bp + 4]

    cld
    repne scasb

    mov ax, ARRAY_SIZE
    sub ax, cx
    dec ax
    mov cx, ax

    mov di, [bp + 4]
    mov si, packet + 1

    repe cmpsb

    pop es
    pop di
    pop si
    pop cx
    pop ax

    pop bp
    ret 2



printdebug:
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

    ret


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