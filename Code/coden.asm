[org 0x0100]

    start:

        MOV AX, 0xB800
        MOV ES, AX

        MOV WORD [ES:0320], 0x0742

    MOV AX, 0x4C00
    INT 0x21