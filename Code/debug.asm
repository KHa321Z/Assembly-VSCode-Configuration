; CHECK ALL PACKET INPUTS
; CHECK ALL PACKET OUTPUTS
; implement custom stack for parent process so that parent does not hog child process stack
; check why exit not sent (2) debugger does not exit after program exits (3) as long as connection exists gdb won't exit
; improve documentation
; remove extraneous stuff 
; wherever CS is used, compare with childseg and fix it if not equal (2) no idea where where used. 2 are in trapisr and brkisr

[org 0x0100]

    jmp start

;-----macro definitions-----
%define ARRAY_SIZE 512
%define MCR 0x03FC
%define IER 0x03F9

start:
    ;-----initialize COM port for debugging-----
    mov ah, 0
    mov al, 0xE3
    xor dx, dx
    int 0x14

    ;-----hook interrupt service routines-----
    call hookISR

    ;-----read file path-----
    xor cx, cx
    mov cl, [0x80]
    cmp cl, 1
    jl no_filepath

    ; remove trailing spaces
    dec cl
    mov si, 0x82
    mov di, filepath
    cld
    rep movsb

    ;-----resizing memory for debugger-----
    mov bx, parentstack
    add bx, 1024 + 15
    shr bx, 4

    mov ax, cs
    mov es, ax
    mov ah, 0x4A
    int 0x21

    ;-----allocating memory for child process-----
    mov ax, 0x4800
    mov bx, 0x1000
    int 0x21
    jc no_memory_available
    mov [childseg], ax

    ;-----creating psp for child process-----
    mov ah, 0x55
    mov dx, [childseg]
    mov si, 0
    int 0x21

    ;-----open .COM file-----
    mov ax, 0x3D00
    mov dx, filepath
    int 0x21
    jc file_not_found

    ;-----load .COM file into memory-----
    push ds

    mov bx, ax
    mov ah, 0x3F
    mov cx, 0xFFFF
    mov dx, 0x0100
    mov ds, [childseg]
    int 0x21
    jc file_not_read

    pop ds

    ;-----close .COM file-----
    mov ah, 0x3E
    int 0x21

    ;-----initialize registers-----
    mov [orig_sp], sp
    mov sp, 0xFFFE

    mov ax, [childseg]
    mov ds, ax
    mov ss, ax
    mov es, ax

    pushf
    push ax
    push word 0x0100

wait_for_continue:
    ; wait till first continue packet arrives
    cmp byte [cs:startprogram], 1
    jne wait_for_continue

    ; reset as program is continuing (allegedly)
    mov byte [cs:inprocessing], 0

    iret

    ;-----come back to original process-----
return_to_parent:
    ; reset parent's registers
    mov ax, cs
    mov ds, ax
    mov ss, ax
    mov es, ax
    mov sp, [orig_sp]

    ; send reply to gdb that child exitted
    cmp byte [startprogram], 0
    jz child_skipped

    push word exitreply
    call send_reply

child_skipped:
    call unhookISR

    mov ax, 0x4C00
    int 0x21


;-----error handling-----
no_filepath:
    mov dx, no_filepath_msg

    jmp handle_error

file_not_found:
    mov dx, file_not_found_msg

    jmp handle_error

no_memory_available:
    mov dx, no_mem_aval_msg

    jmp handle_error

file_not_read:
    pop ds
    mov dx, file_not_read_msg

    jmp handle_error

handle_error:
    mov ah, 0x09
    int 0x21

    mov ax, 0x4C00
    int 0x21


;-------------------------ISR functions-------------------------

; function for hooking all necessary ISRs and initializing COM ports
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


; function for unhooking all ISRs and disabling COM ports
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


; (int 0x1)
trapisr:
    push bp
    mov bp, sp

    pusha
    push ds
    push es

    push cs
    pop ds

    ; check if breakpoint was set in previous instruction
    cmp byte [reinstallbrk], 1
    jne resume_trap_activity

    mov al, 0xCC
    mov di, [tempbrkaddr]
    mov es, [childseg]
    mov byte [reinstallbrk], 0

    cld
    stosb

    ; if this is a force step for reinstalling breakpoint, skip waiting for packet
    cmp byte [forcepause], 1
    jz packet_continue

resume_trap_activity:
    call debug_activity
    
    ; wait for comisr to receive a packet
    sti

wait_packet:
    cmp byte [inprocessing], 0
    je wait_packet

    cmp byte [packet + 1], 's'
    je packet_step

    cmp byte [packet + 1], 'c'
    je packet_continue

    jmp wait_packet

packet_continue:
    and word [bp + 6], 0xFEFF

    jmp wait_packet_done

packet_step:
    or word [bp + 6], 0x0100

    call check_interrupt

wait_packet_done:
    mov byte [inprocessing], 0
    mov byte [forcepause], 0

    pop es
    pop ds
    popa

    pop bp
    iret


; (int 0x3)
brkisr:
    push bp
    mov bp, sp

    pusha
    push ds
    push es

    push cs
    pop ds

    ; adjust ip to re-execute the instruction after the breakpoint
    dec word [bp + 2]

    ; restore the opcode so that the program can continue
    ; ----- ideally check if es contains the same segment as childseg and fix ip if not
    mov es, [bp + 4]
    mov di, [bp + 2]

    ; check if normal or temporary interrupt breakpoint was set
    cmp byte [intopcode], 0xCC
    je normal_breakpoint

    mov al, [intopcode]
    ; reset with breakpoint opcode to differentiate. also helps if a breakpoint is already set
    mov byte [intopcode], 0xCC

    stosb

    jmp resume_brk_activity

normal_breakpoint:
    ; set flags to indicate re-installation of breakpoint
    mov byte [reinstallbrk], 1
    mov word [tempbrkaddr], di

    ; find the opcode in the opcodes array
    push di
    call find_opcode
    pop si

    add si, opcodes

    ; restore the original opcode
    lodsb
    stosb

resume_brk_activity:
    call debug_activity
    
    ; wait for comisr to receive a packet
    sti

trap_wait:
    cmp byte [inprocessing], 0
    je trap_wait

    cmp byte [packet + 1], 's'
    je normal_step

    cmp byte [packet + 1], 'c'
    je force_step

    jmp trap_wait

    ; step in both cases. for continue set flag so that trapisr does not wait for another packet
force_step:
    mov byte [forcepause], 1

normal_step:
    ; clear the inprocessing flag and do not wait in wait_packet
    mov byte [inprocessing], 0
    ; initialize trap flag for breakpoint restoration
    or word [bp + 6], 0x0100

    call check_interrupt

    pop es
    pop ds
    popa

    pop bp
    iret


; (int 0xC)
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
    ; skip if noise meaning packet not started
    cmp byte [packet], 0
    je nodata

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
    ; remove $ from packet to signify packet completion
    mov byte [packet], 0

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


;-------------------------debug helper functions-------------------------

; check for interrupt instruction and apply temporary breakpoint
check_interrupt:
    mov es, [bp + 4]
    mov di, [bp + 2]
    mov al, [es:di]

    ; check if next instruction is an interrupt
    cmp al, 0xCD
    jne check_interrupt_done

    ; store next instructions opcode. if already a breakpoint then will ignore it due to default value
    mov al, [es:di + 2]
    mov [intopcode], al
    ; apply temporary breakpoint and will be restored in brkisr
    mov byte [es:di + 2], 0xCC

check_interrupt_done:
    ret


; actions performed when debugger stops at instruction
debug_activity:
    ; send reply to gdb that child stopped
    push word stopreply
    call send_reply

    ; saving padded registers in 32 bit form with upper 16 bits zeroed
    mov ax, [bp - 2]
    mov [regs + 0], ax  ; AX
    mov ax, [bp - 8]
    mov [regs + 12], ax  ; BX
    mov ax, [bp - 4]
    mov [regs + 4], ax  ; CX
    mov ax, [bp - 6]
    mov [regs + 8], ax  ; DX
    mov ax, [bp - 14]
    mov [regs + 24], ax  ; SI
    mov ax, [bp - 16]
    mov [regs + 28], ax ; DI
    mov ax, [bp]
    mov [regs + 20], ax ; BP
    mov ax, [bp - 10]
    sub ax, 8
    mov [regs + 16], ax ; SP
    mov ax, [bp + 2]
    mov [regs + 32], ax ; IP
    mov ax, [bp + 6]
    mov [regs + 36], ax ; FLAGS
    mov ax, [bp + 4]
    mov [regs + 40], ax ; CS
    mov ax, [bp - 18]
    mov [regs + 48], ax ; DS
    mov ax, [bp - 20]
    mov [regs + 52], ax ; ES
    mov [regs + 44], ss ; SS

    ret


;-------------------------packet processing functions-------------------------
packet_processor:
    mov al, [packet + 1]
    mov di, availpacks
    mov cx, [packslength]

    ; check general packets
    push word supportPack
    call check_packet
    jz gdb_support

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


gdb_support:
    push word supportreply
    call send_reply

    jmp terminate_packet_processing

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


gdb_kill:
    ; no need to send reply
    mov byte [startprogram], 0

    ; terminate the program in child's context
    mov ax, 0x4c00
    int 0x21


gdb_debugger:
    ; toggle flag to start the child program's execution
    mov byte [startprogram], 1
    ; toggle flag beforehand so that it remains on and is handle by debugger isrs
    xor byte [inprocessing], 1

    jmp terminate_packet_processing


gdb_extract_register:
    push word packet + 2
    push word 1
    call extract_hex
    pop di
    pop ax

    shl di, 2
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
    mov al, ','
    mov di, packet + 4
    mov cx, 10

    cld 
    repne scasb

    sub di, packet + 5

    push word packet + 4
    push di
    call extract_hex
    call convert_physical_to_logical
    pop di
    pop ax

    shl ax, 4
    add ax, di

    push ax
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
    mov al, ','
    mov di, packet + 4
    mov cx, 10

    cld 
    repne scasb

    sub di, packet + 5

    push word packet + 4
    push di
    call extract_hex
    call convert_physical_to_logical
    pop di
    pop ax

    shl ax, 4
    add ax, di

    push ax
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
    ; extract length of address in packet
    mov al, ','
    mov di, packet + 2
    mov cx, 10

    cld
    repne scasb

    ; length in bx
    mov bx, di
    sub bx, packet + 3

    ; extract length of number of bytes to read
    mov al, '#'
    mov cx, 10

    repne scasb

    ; length in cx
    sub di, packet + 4
    sub di, bx
    mov cx, di

    ; extract and convert 32 bit address to segment:offset
    push word packet + 2
    push bx
    call extract_hex
    call convert_physical_to_logical
    pop di
    pop es

    ; extract number of bytes to read
    mov ax, packet + 3
    add ax, bx
    
    push ax
    push cx
    call extract_hex
    pop cx
    pop ax

    xor ax, ax
    xor dx, dx
    ; mov es, [childseg]        ; dunnno what to do with this

    push word '$'
    call send_byte

read_memory_loop:
    mov al, [es:di]
    push ax
    call send_hex
    pop ax

    add dl, al
    inc di

    ; handling segment wrap for large memory reads
    cmp di, 0
    jne no_wrap_in_memory

    mov ax, es
    add ax, 0x1000
    mov es, ax

no_wrap_in_memory:
    loop read_memory_loop

    push word '#'
    call send_byte
    push dx
    call send_hex
    pop dx

    jmp terminate_packet_processing


; check this
gdb_write_memory:
    ; extract length of address in packet
    mov al, ','
    mov di, packet + 2
    mov cx, 10

    cld
    repne scasb

    ; length in bx
    mov bx, di
    sub bx, packet + 3

    ; extract length of number of bytes to write
    mov al, ':'
    mov cx, 10

    repne scasb

    ; length in cx and pointer to data in si
    mov si, di
    sub di, packet + 4
    sub di, bx
    mov cx, di

    ; extract and convert 32 bit address to segment:offset
    push word packet + 2
    push bx
    call extract_hex
    call convert_physical_to_logical
    pop di
    pop es

    ; extract number of bytes to write
    mov ax, packet + 3
    add ax, bx

    push ax
    push cx
    call extract_hex
    pop cx
    pop ax

    xor ax, ax
    xor dx, dx
    mov bx, si
    ; mov es, [childseg]

write_memory_loop:
    push bx
    push word 2
    call extract_hex
    pop ax
    pop si

    stosb
    inc bx

    loop write_memory_loop

    jmp gdb_ok


;-------------------------breakpoint setting functions-------------------------

; find the index of the opcode in the opcodes array
find_opcode:
    ; Parameters:
    ; [bp + 4] - address of breakpoint
    ; Returns:
    ; [bp + 4] - index in opcodes array

    push bp
    mov bp, sp
    
    push ax
    push cx
    push di
    push es

    push ds
    pop es

    mov ax, [bp + 4]
    mov cx, [opcodearrsize]
    mov di, opcodespos

    jcxz missing_opcode

    cld
    repne scasw
    jz found_opcode_addr

missing_opcode:
    mov word [bp + 4], 0xFFFF

    jmp done_find

found_opcode_addr:
    sub di, opcodespos + 2
    shr di, 1
    mov [bp + 4], di

done_find:
    pop es
    pop di
    pop cx
    pop ax

    pop bp
    ret


; push an opcode for a breakpoint in the opcodes array
; and replace the opcode at the breakpoint address with 0xCC
push_opcode:
    ; Parameters:
    ; [bp + 4] - address of breakpoint
    ; Returns:
    ; CF - set if error

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
    mov cx, [opcodearrsize]
    mov di, opcodespos

    jcxz skip_push_search

    cmp cx, 256
    jae push_opcode_error

    cld
    repne scasw
    jz push_opcode_error

skip_push_search:
    mov si, ax
    mov di, [opcodearrsize]
    mov es, [childseg]

    ; opcode replaced for breakpoint in child process
    mov al, [es:si]
    mov byte [es:si], 0CCh
    ; opcode stored in array for reference
    mov [opcodes + di], al
    shl di, 1
    mov [opcodespos + di], si
    inc word [opcodearrsize]

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


; remove an opcode from the opcodes array
; and restore the original opcode at the breakpoint address
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

    push word [bp + 4]
    call find_opcode
    pop cx

    cmp cx, 0xFFFF
    je remove_opcode_error

    mov si, cx
    add si, opcodes
    mov es, [childseg]

    ; push di
    mov di, [bp + 4]
    cld

    ; opcode replaced for original
    lodsb
    stosb

    ; remove opcode and shift the array
    push ds
    pop es

    ; setup di index for shifting addresses
    mov di, cx
    shl di, 1
    add di, opcodespos
    push di

    ; shift the opcodes array
    mov ax, [opcodearrsize]
    sub ax, cx
    dec ax
    mov cx, ax
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

    dec word [opcodearrsize]
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


;-------------------------COM port functions-------------------------

; send a byte through COM port
send_byte:
    ; Parameters:
    ; [bp + 4] - byte to send

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


; send a hex word through COM port
send_hex:
    ; Parameters:
    ; [bp + 4] - value to send
    ; Returns:
    ; [bp + 4] - checksum value

    push bp
    mov bp, sp
    push ax
    push dx

    xor ax, ax
    xor dx, dx

    mov al, 0xF0
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


; send a reply packet through COM port
send_reply:
    ; Parameters:
    ; [bp + 4] - address of reply packet

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


;-------------------------miscellaneous functions-------------------------

; transfer to parent process stack
push_parent_stack:
    mov [cs:tempregs], ss
    mov [cs:tempregs + 2], sp
    mov [cs:tempregs + 4], bp
    mov [cs:tempregs + 6], ax
    mov [cs:tempregs + 8], bx

    pop bx

    mov bp, sp
    mov sp, [cs:parentstack + 1024]
    mov ax, cs
    mov ss, ax

    push word [bp + 4]
    push word [bp + 2]
    push word [bp + 0]

    jmp bx


pop_parent_stack:
    pop bx

    mov bp, sp
    mov sp, [cs:tempregs + 2]
    mov ss, [cs:tempregs]

    sub sp, 6
    push word [bp + 4]
    push word [bp + 2]
    push word [bp + 0]
    push bx

    mov bp, [cs:tempregs + 4]
    mov ax, [cs:tempregs + 6]
    mov bx, [cs:tempregs + 8]

    ret


; extracts hex value for equivalent ASCII string
; e.g. "1234" -> 0x1234
extract_hex:
    ; Parameters:
    ; [bp + 6] - address of value to extract
    ; [bp + 4] - length (max 8 for 32 bit address)

    ; Returns:
    ; [bp + 6] - higher word of 32 bit value
    ; [bp + 4] - lower word of 32 bit value

    push bp
    mov bp, sp

    push ax
    push cx
    push si

    mov cx, [bp + 4]
    mov si, [bp + 6]

    mov word [bp + 6], 0
    mov word [bp + 4], 0

extract_next_hex:
    mov al, [si]
    sub al, 0x30

    cmp al, 10
    jb skip_extract_char

    sub al, 0x27

skip_extract_char:
    ; shift the 32 bit value left by 4 bits
    shl word [bp + 6], 4        ; higher nibble is empty
    rol word [bp + 4], 4        ; higher nibble that is to be shifted to bp+6 is now in lower nibble

    mov ah, 0xF                 ; prepare mask
    and ah, [bp + 4]            ; extract the higher nibble
    and word [bp + 6], 0xFFF0   ; reset lower nibble of higher word
    or [bp + 6], ah             ; pasted higher nibble to the higher word

    and word [bp + 4], 0xFFF0   ; reset lower nibble of higher word
    or [bp + 4], al             ; pasted extracted lower nibble to the lower word

    inc si
    loop extract_next_hex

    pop si
    pop cx
    pop ax

    pop bp
    ret


; converts a byte value to its ASCII representation
convert_to_ascii:
    ; Parameters:
    ; [bp + 4] - byte value to convert to ASCII

    ; Returns:
    ; [bp + 4] - ASCII representation of the byte value

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
    push cx
    push dx

    mov ax, [bp + 4]
    mov dx, [bp + 6]
    mov cx, 4

    ; mask to 20 bits
    and dx, 0x000F

segment_shift_loop:
    shr dx, 1
    rcr ax, 1
    loop segment_shift_loop

    mov [bp + 6], ax ; segment
    and word [bp + 4], 0x000F ; keep only lower 4 bits of offset

    pop dx
    pop cx
    pop ax

    pop bp
    ret


; checks if the current packet matches the given packet
; doesn't handle substrings
check_packet:
    ; Parameters:
    ; [bp + 4] - packet to check against

    ; Returns:
    ; ZF - set if packet is valid

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


;--------------------------data segment-------------------------

;-----Error handling strings-----
no_filepath_msg:    db 'No File Path was Provided$'
file_not_found_msg: db 'File could not be Found$'
no_mem_aval_msg:    db 'No Memory is Available for Program$'
file_not_read_msg:  db 'File could not be Read$'

;-----GDB protocol packets-----
supportPack:    db 'qSupported', 0
contPack:       db 'vCont?', 0
mustreplyPack:  db 'vMustReplyEmpty', 0
multiPack:      db 'Hg0', 0
threadPack:     db 'qfThreadInfo', 0
endthreadPack:  db 'qsThreadInfo', 0
attachedPack:   db 'qAttached', 0
currthreadPack: db 'Hc-1', 0
querycurrPack:  db 'qC', 0

;------GDB protocol replies-----
supportreply:   db '$PacketSize=512;swbreak+;kill+;vContSupported-#67', 0
nothing:        db '$#00', 0
okreply:        db '$OK#9a', 0
errorreply:     db '$E01#xx', 0
stopreply:      db '$S05#b8', 0
singletreply:   db '$m1#9e', 0
endlistreply:   db '$l#6c', 0
childkillreply: db '$0#30', 0
currthreply:    db '$QC1#c5', 0
exitreply:      db '$W00#57', 0

;-----GDB general packets-----
availpacks:     db 'q', '?', 'k', 's', 'c', 'p', 'g', 'Z', 'z', 'm', 'X'
addresspacks:   dw gdb_unknown, gdb_why, gdb_kill, gdb_debugger, gdb_debugger, gdb_extract_register, gdb_send_registers, gdb_set_breakpoint, gdb_remove_breakpoint, gdb_read_memory, gdb_write_memory
packslength:    dw ($ - addresspacks) / 2

;-----debugger variables-----
orig_sp:        dw 0
oldtrapisr:     dd 0
oldbrkisr:      dd 0
oldcomisr:      dd 0
oldretisr:      dd 0
filepath:       times 128 db 0

;-----debugger data-----
childseg:       dw 0
regs:           times 16 dd 0
packet:         times ARRAY_SIZE db 0
packettail:     dw packet
inprocessing:   db 0
chksum:         db 0
startprogram:   db 0
forcepause:     db 0

;-----breakpoint variables-----
intopcode:      db 0xCC
reinstallbrk:   db 0
tempbrkaddr:    dw 0
opcodearrsize:  dw 0
opcodes:        times ARRAY_SIZE db 0
opcodespos:     times ARRAY_SIZE dw 0

;-----temp stack-----
tempregs:       dw 0, 0, 0, 0, 0       ; SS, SP, BP, AX, BX
parentstack:    times ARRAY_SIZE dw 0