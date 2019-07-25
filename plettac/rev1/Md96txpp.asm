;******************************************************************************
;*                                                                            *
;* NOMBRE       : FX919.SRC                                                   *
;* DESCRIPCION  : Controlador modem FX919                                     *
;* LENGUAJE     :                                                             *
;* EDITOR       :                                                             *
;* OBSERVACIONES: versi¢n para paridad                                        *
;* FECHA INICIO : 25/10/95                                                    *
;* TERMINADO    :                                                             *
;******************************************************************************
;
; DEFINICION DE LOS PORTS
PORTA   equ 00h
PORTB   equ 01h
PORTC   equ 02h
portd   equ 03h
PCA     equ 04h
PCB     equ 05h
PCC     equ 06h
PCD     equ 07h
mr      equ 0ah                 ;miscellaneous register


;============================= SEGMENTO DE RAM ============================

SCDAT           equ 0011h       ;sERIAL cOMUNICATIONS dATA rEGISTER
SCCR1           equ 000eh       ;sERIAL cOMUNICATION rEGISTER 1
SCCR2           equ 000fh       ;sERIAL cOMUNICATION rEGISTER 2
SCSR            equ 0010h       ;sERIAL COMUNICATION sTATUS rEGISTER
BAUD            equ 000dh       ;BAUD rATE rEGISTER
TCR             equ 0012h       ;tIMER cONTROL rEGISTER
TSR             equ 0013h       ;tIMER sTATUS rEGISTER
ENTRY           equ 0100h       ;direccion comienzo rom
optreg          equ 0fdfh       ;option register
TCRH            equ 0018h       ;timer count register (high)
TCRL            equ 0019h       ;timer count register (low)
LARGO           equ 00055
SPCR            equ   0ah               ; spcr (serial peripheral control register)
SPSR            equ   0bh       ; Serial Peripheral Status Register
SPDR            equ   0ch       ; Serial Peripheral Data I/O Register


BUFFER_LEN      equ 60
TOPE_BUFFER     equ 80
MAX_TIMERH      equ 03h
MAX_TIMERL      equ 00h


XON             equ 19
XOFF            equ 17

;---------------- Definición de los bits del COMMAND REGISTER ---------------

AQBC_AQLEV      equ 00h
TSK_SFH         equ 01h
TSK_R3H         equ 02h
TSK_RDB         equ 03h
TSK_SFS         equ 04h
TSK_RSB         equ 05h
TSK_LFSB        equ 06h

TSK_T7H         equ 01h
TSK_TDB         equ 03h
TSK_TQB         equ 04h
TSK_TSB         equ 05h
TSK_TSO         equ 06h
TSK_RESET       equ 07h


BIT_AQBC        equ 7
BIT_AQLEV       equ 6
BIT_TEST        equ 1

;---------------- Definición de los bits del CONTROL REGISTER ---------------

BIT_HI_LO       equ 5
BIT_DARA        equ 4


;---------------- Definición de los bits del MODE REGISTER ---------------

BIT_IRQEN       equ 7
BIT_INVBIT      equ 6
BIT_TXRX        equ 5
BIT_SCREN       equ 4
BIT_DQEN        equ 2

;---------------- Definición de los bits del STATUS REGISTER ---------------

BIT_IRQ         equ 7
BIT_BFREE       equ 6
BIT_IBEMPTY     equ 5
BIT_DIBOVF      equ 4
BIT_CRCFEC      equ 3
BIT_DQRDY       equ 2
BIT_MOBAN       equ 1


;---------------- Definición de los bits de control del modem  --------------

CTL_RD  equ 0
CTL_WR  equ 1

CTL_A1  equ 2
CTL_A0  equ 3
CTL_CS  equ 4


;---------------- Definición de las lineas de RS-232  --------------

RS_CTS  equ 3

;---------------- Definición de las lineas de RADIO  --------------

R_PTT   equ 7

;-------------- Definición de los bits de configuración --------------------

C_TEST          equ 5   ;Poniendo el bit 5 del PORTA a 1 se activa el test
WATCH_DOG       equ 7


;***************** CALCULADO PARA CRISTAL DE 2.4576 ***********************

UART_19200 equ 20H ;BAUD rate a 19200
UART_9600  equ 21h ;  "    "  "  9600 
UART_4800  equ 22h ;  "    "  "  4800

;================================ RAM ========================================

                                org 30h

BuffSCI   rmb TOPE_BUFFER
CntBuff   rmb   1

TotBytes  rmb   1
IrqStat   rmb   1
CtrlReg   rmb   1
CmndReg   rmb   1
ModeReg   rmb   1
StatReg   rmb   1
flag      rmb   1
CntTimer  rmb   2
status    rmb   1
sava      rmb   1
savx      rmb   1
RamHead   rmb   6
RxHeader  rmb   6
savIRQ    rmb   1
CntBytes  rmb   1
PntSCI    rmb   1
TimeBlkRx rmb   1
tmp       rmb   1
tmp2      rmb   1
tmp3      rmb   1
tope      rmb   1
TotBytesTx rmb   1

;==============================================================================

           
;========================= INICIALIZACIàN CPU ===============================

                           org ENTRY    ;comienzo rom
        lda #$82
        sta $1fdf


                lda #11011111b
        sta PCA
        sta PORTA

        clra
        sta PCB
        
        lda #$8
        sta PCC 

        lda #01011100b
        sta PCD

        clrx

Head2Ram:

        lda head,x
        sta RamHead,x
        incx
        cpx #$6
        bne Head2Ram


;==============================================================================

        clr TotBytes
        clr CntBuff
        clr IrqStat
        clr CtrlReg
        clr CmndReg
        clr ModeReg
        clr StatReg
        clr flag
        clr CntTimer
        clr status
        clr sava
        clr savx
        clr TimeBlkRx

        bclr CTL_CS,PORTA    ;/CS (Selecciona el MODEM).
        jsr IniSpi

        lda #10010100b
        sta ModeReg
        jsr P_ModeReg    ;Programamos el mode register para recepción.

        brset 0,PORTC,p19200
        brset 1,PORTC,p9600
        brset 2,PORTC,p4800
        bra beg

p19200:
        jsr b19200
        bra beg 
p9600:
        jsr b9600
        bra beg
p4800:
        jsr b4800
        bra beg

beg:
        jsr RsOff
        bset R_PTT,PORTA

        JSR SciOn       ;Activa modo sci
        jsr IniTimer
        jsr test
        cli

        jsr IniModem
main:
        jsr IniTimer
WaitB:
        bset WATCH_DOG,PORTA
        bclr WATCH_DOG,PORTA            ;Refrmbco Watch dog
        brset 2,flag,PonCts
nocts   brset 7,SPSR,HayByte

        lda CntBuff
        beq WaitB

        dec CntTimer+1
        bne WaitB
        dec CntTimer
        bne WaitB
        jsr RsOn                ;No mas car cteres
        brset 0,flag,main       ;El m¢dem est  transmitiendo, se espera
        jmp TxMsg
        bra main
 

HayByte:
        lda SPDR
        ldx CntBuff
        sta BuffSCI,x
        incx
        cpx #BUFFER_LEN
        bmi CntSCI2
        cpx #TOPE_BUFFER
        bpl desbordamiento
        jsr RsOn        ;Indica al HOST que no transmita mas bytes
        nop
CntSCI2:
        inc CntBuff
FinSCI:
        bra main


desbordamiento:

        jsr RsOn        ;Indica al HOST que no transmita mas bytes
        bra main


PonCts:
        lda #BUFFER_LEN
        sub CntBuff
        cmp #32
        bmi nocts

        jsr RsOff
        bclr 2,flag
        bra main

TxMsg:
        jsr IniTimer
        brset 0,flag,main       ;Indica que ya está transmitiendo un mensaje.
        bset 0,flag
        bclr R_PTT,PORTA        ;Pone la emisora en transmisi¢n

      
        lda CntBuff
        cmp #18
        bmi esmenor
        lda #18

esmenor:
        sta TotBytesTx
        sta RamHead+4
        sta RamHead+5
        clr PntSCI
        clr TotBytes
        clrx

TxHead:
        lda RamHead,x   ;La rutina de puesta en transmisión, sólo se ocupa
        jsr P_DataBlk   ;de transmitir el HEADER, de lo demás se ocupa
        incx            ;la rutina de interrupt, que es la encargada de 
        cpx #$6         ;procesar todas las tareas.
        bne TxHead
        lda #TSK_T7H    ;Pone la tarea de transmitir el HEADER, con lo cual
        jsr PrgTask     ;causará una interrupción de tipo BFREE cuando acabe.
        jmp main

IniModem:

        lda #TSK_RESET
        jsr PrgTask
            
        lda #10110000b
        sta ModeReg
        jsr P_ModeReg   ;Ponemos el modem en Transmisi¢n.
        jsr bucle       ;Después de poner el modem en transmisión, se ha
        jsr bucle       ;de esperar un tiempo de 2 bits mínimo, antes de
        jsr bucle       ;cargar la siguiente tarea, para que el filtro de
        jsr bucle       ;paso bajos se estabilice.
        jsr bucle       
        jsr bucle       
        jsr bucle       
        jsr bucle       
        jsr bucle       
        jsr bucle       
        jsr bucle       
        jsr bucle       
        rts

;-----------------------------------------------------------------------------
test:

        brclr C_TEST,PORTA,FinTest

        lda #TSK_RESET
        jsr PrgTask

        lda #10110000b
                sta ModeReg
        jsr P_ModeReg   ;Ponemos el modem en Transmisi¢n.

tst0:
        clrx
tst1:
        lda #$0f
        jsr P_DataBlk
        incx
        cpx #18
        bne tst1

        lda #TSK_TDB
        jsr PrgTask

        bih *
        jsr R_StatusReg
 
        lda #$cc
        jsr P_DataBlk
        lda #TSK_TSB
        jsr PrgTask

        bih *
        jsr R_StatusReg
        jmp tst0



FinTest:

        rts
;-------------------- TRANSMITE LOS BLOQUES AL HOST -------------------------
;El primer byte indica el total de bytes.
;

Blk2Host:

        ldx #$1
HTxBlk:
        
        lda BuffSCI,x
        jsr Tx2Host
        incx
        dec BuffSCI
        bne HTxBlk

FinHTxBlk:
        rts

;------------------------------------------------------------------------------

;=============================== SUBRUTINAS =================================

RsOn:
        bset RS_CTS,PORTC       ;Indica al HOST que no transmita mas bytes
        rts

RsOff:
        bclr RS_CTS,PORTC       ;Indica al HOST que transmita mas bytes
        rts

b4800:
        lda #10100101b   ;BAUD Rate a 4800
        sta CtrlReg
        jsr P_CtrlReg
        lda #UART_4800
        jsr SciOn
        rts

b9600:
        lda #01100101b   ;BAUD Rate a 9600
        sta CtrlReg
        jsr P_CtrlReg   
        lda #UART_9600
        jsr SciOn
        rts

b19200:
        lda #00110111b   ;BAUD Rate a 19200
        sta CtrlReg
        jsr P_CtrlReg   
        lda #UART_19200
        jsr SciOn
        rts


bucle:
                ldx #$ff
buc:
        decx
        cpx #$00
        bne buc
        rts

bucle1:
        ldx #$02
buc1:
        decx
        bne buc1
        rts


IniTimer:
        lda #MAX_TIMERH
        sta CntTimer
        lda #MAX_TIMERL
        sta CntTimer+1
        rts


PrgTask:
        sta sava         ;En el acumulador ha de estar la tarea.

        lda CmndReg
        and #$f8
        ora sava         ;En sava está la tarea a programar
        sta CmndReg
        jsr P_CmndReg

        lda sava
        rts


pretim:
        lda #$20
        sta TCRH
        lda #$10
        sta TCRL
        rts

;--------------------- ESCRIBE EN EL MODE REGISTER -------------------------
P_ModeReg:
                 
        sta sava
        lda #$ff
        
        sta PCB
        clra
        sta PORTB
            
        bset CTL_A0,PORTA  ;A0
        bset CTL_A1,PORTA  ;A1
        lda ModeReg
        sta PORTB
        sta PORTB
        
        bclr CTL_WR,PORTA  ;/WR
        bset CTL_WR,PORTA
        clra
        sta PCB
        lda PORTB
        lda sava
        rts

;--------------------- ESCRIBE UN DATA BLOCK BUFFER -------------------------

;--------------------- LEE EN EL STATUS REGISTER -------------------------
R_StatusReg:

        clra
    sta PCB
        lda PORTB
    bset CTL_A0,PORTA    ;A0
    bclr CTL_A1,PORTA    ;A1
    bclr CTL_RD,PORTA    ;/RD
    lda PORTB
    lda PORTB
        sta savIRQ
    bset CTL_RD,PORTA
        lda savIRQ
    rts


;--------------------- ESCRIBE EN EL COMAND REGISTER -------------------------
P_CmndReg:
        sta sava
        lda #$ff
        sta PCB
                clra
                sta PORTB

        bset CTL_A0,PORTA    ;A0
        bclr CTL_A1,PORTA    ;A1

        lda CmndReg
        sta PORTB
        sta PORTB
        bclr CTL_WR,PORTA    ;/WR
        bset CTL_WR,PORTA
                clra
        sta PCB
        lda sava
        rts

;--------------------- ESCRIBE UN DATA BLOCK BUFFER -------------------------
P_DataBlk:
;        sei
        stx savx
        sta sava

        lda #$ff
        sta PCB
                clra
                sta PORTB

        bclr CTL_A0,PORTA    ;A0
        bclr CTL_A1,PORTA    ;A1
        bclr CTL_WR,PORTA    ;/WR
                lda sava
        sta PORTB
        sta PORTB
        bset CTL_WR,PORTA
        ldx savx
                clra
        sta PCB
        lda sava
        rts

;--------------------- LEE UN DATA BLOCK BUFFER -------------------------
R_DataBlk:

        stx savx
        clra
        sta PCB
                lda PORTB

        bclr CTL_A0,PORTA    ;A0
        bclr CTL_A1,PORTA    ;A1
        bclr CTL_RD,PORTA    ;/RD
        lda PORTB
        lda PORTB
        sta sava
        bset CTL_RD,PORTA
        lda sava
        ldx savx
        rts



;----------------------- PROGRAMA EL CONTROL REGISTER -----------------------
P_CtrlReg:
        stx savx
        sta sava
        lda #$ff
        sta PCB
                clra 
                STA PORTB

        bclr CTL_A0,PORTA    ;CTL_A0
        bset CTL_A1,PORTA    ;CTL_A1

        lda CtrlReg
        sta PORTB
        sta PORTB
        bclr CTL_WR,PORTA    ;/WR
        bset CTL_WR,PORTA
        ldx savx
        clra
        sta PCB
        lda sava
        rts
;------------------------------------------------------------------------------
;------------------------------------------------------------------------------

SpiIn:                  
         lda   #$57     ;CAMBIA LA FASE DEL CLOCK
         sta   SPCR     
         clra           ;TRANSMITE 00 PARA QUE GENERE EL CLOCK
         jsr   SpiOut   ;Y ASI PERMITIR AL DEVICE QUE TRANSMITA EL BYTE.
         lda   #$53     ;CAMBIA LA FASE DEL CLOCK
         sta   SPCR     
         lda   SPDR     ;LEE EL BYTE RECIBIDO.
         RTS            

;----------------- TRANSMITE AL SPI EL CONTENIDO DE A ---------------------

SpiOut:                 

         sta   SPDR     ;Transmite el comando al 802
         brclr 7,SPSR,* ;espera que transmita el byte
         rts            


;---------------------------- inicialización spi -------------------------------
;***************************** imPORTAnte *************************************
;                                                      __
;para que el micro acepte ser master se ha de poner el ss a positivo (pata 37),

;si no es asi el micro rechaza el bit 4 del SPCR (master),
;
;******************************************************************************

IniSpi:                 
         lda   #$43     ;Serial Peripheral Interrupt Enable
         sta   SPCR     ;Serial Peripheral System Enable
                        ;Slave mode
                        ;SCK line idles in low state
                        ;When /SS is low, first edge of SCK invokes first data
                        ;sample.
                        ;Internal Processor Clock Divided by 32
         rts            

;----------------- CONFIGURACION INTERFACE DE COMUNICACIONES ------------------
SciOn:
                sta sava
        LDA #$10
        STA SCCR1

        LDA #$0C        ;- TDRE interrupt disabled
                        ;- TC interrupt disabled
                        ;- SCI interrupt enabled
                        ;- IDLE interrupt disbaled
                        ;- After the last byte is transmitted, the TDO line
                        ;  becomes a hight-impedance line.
                        ;- Receiver disabled and RDRF, IDLE, OR, NF and FE
                        ;  status bits are inhibited.

        STA SCCR2

                lda sava
                                ;BAUD rate a 9600 - #$30
                        ;            4800 - #$31
                        ;            2400 - #$32
                        ;            1200 - #$33
                        ;             600 - #$34
                        ;             300 - #$35
                        ;             150 - #$36
                        ;              75 - #$37
        sta BAUD
        RTS

;------------------------------------------------------------------------------


;-------------- TRANSMITE AL HOST EL CONTENIDO DEL REGISTRO A ------------------

Tx2Host:
        brclr 7,SCSR,Tx2Host       ;MIRA SI ESTA OCUPADO
        sta SCDAT
espera:
        brclr 6,SCSR,espera        ;ESPERA PARA ACABAR DE TRANSMITIR
        rts


;============================ RUTINAS INTERRUPION ===========================

;--------------------------------- IRQ --------------------------------------
IRQ:
        jsr R_StatusReg
        tax
        and #$80
        beq FinIrq
        stx IrqStat

        brset BIT_BFREE,IrqStat,ProcTX
irq2:   brset BIT_IBEMPTY,IrqStat,jIBempty
Irq3:
FinIrq:
        rti



jIBempty:
        jmp Irq3

; - - - - - - - - - -  PROCESO DE LAS TAREAS EN MODO TX - - - - - - - - - - - -

ProcTX:
        
        lda CmndReg
        and #$07
        ldx #$3
        fcb 42h         ;mul
        tax
        jmp ProcesTx,x


ProcesTx:
        jmp irq2
        jmp ProcT7H
        jmp FinIrq      ;Esta tarea no existe.
        jmp ProcTDB
        jmp ProcTQB
        jmp ProcTSB
        jmp ProcTSO
        jmp irq2        ;Tarea RESET


ProcT7H:
ProcTDB:

        lda TotBytesTx
        beq FinTxBlk
        sub #18
        bcc NoCero
        clra

NoCero:
        sta TotBytesTx

        clr CntBytes
        clrx

PoneBlk:
        lda BuffSCI,x
        jsr P_DataBlk
        incx
        stx PntSCI
        inc CntBytes
        lda CntBytes
        cmp #18
        bne PoneBlk

        lda #TSK_TDB
        jsr PrgTask

        ldx #18
        stx tmp
        clr tmp2

CorreBuff:

        ldx tmp
        lda BuffSCI,x
        inc tmp

        ldx tmp2
        sta BuffSCI,x
        inc tmp2

        lda tmp
        cmp #TOPE_BUFFER
        bne CorreBuff

FinCorr:
        lda CntBuff
        sub #18
        bcc NoMas

        clra
NoMas:
        sta CntBuff
        bset 2,flag
        jmp irq2

FinTxBlk:

        lda #$cc
        jsr P_DataBlk
        lda #TSK_TSB
        jsr PrgTask
        jmp irq2


ProcTSB:
;        lda #TSK_RESET
;        jsr PrgTask
        bset 2,flag
        bclr 0,flag
        jsr IniTimer
        jmp irq2


ProcTQB:
ProcTSO:
        jmp irq2

;---------------------------------- SCI -------------------------------------
;ISR para el SCI, controla el buffer BuffSCI que es de tipo anillo
;Tiene dos contadores, el buff_head, indica la próxima posición libre en el
;buffer y buff_tail, indica el primer byte a leer del buffer.

;buff_head se incrementa cada vez que llega un byte y buff_tail se incrementa
;cada vez que un byte es leido del buffer, si los dos son iguales indica que
;el buffer está lleno (en esta rutina).


SCI:
;        lda SCSR
;        brclr 3,SCSR,CntSCI
;        nop
;        nop
;        nop
;
;CntSCI:
;        lda SCDAT
;        bclr 1,status
;
;        ldx CntBuff
;        sta BuffSCI,x
;
;        incx
;        cpx #BUFFER_LEN
;        bmi CntSCI2
;        jsr RsOn        ;Indica al HOST que no transmita mas bytes
;        nop
;
;CntSCI2:
;
;        inc CntBuff
;FinSCI:
;        jsr IniTimer
;        rti

        

;------------------------------------------------------------------------------


timer:                 
SPI:
SWI:

        RTI
;==============================================================================


TstMsg:

        fcb 'test',0

head:
          fcb 0cch,0cch,0ech,0a1h,00,01
;         \_______/ \_____/   __ __ 
;             .        .       . . 
;             .        .       . .
;             .        .       . . 
;             .        .       . .....Total número de bloques
;             .        .       ...... Reservado
;             .        .............Frame  
;             ......................Bytes de sincronismo.

;======================== VECTORES INTERRUPCION ===============================
                               ORG 1FF4H

        FDB SPI
        FDB SCI
        FDB timer
        FDB IRQ
        FDB SWI     
        FDB ENTRY  
            
;------------------------------------------------------------------------------
;-------------------- CONFIGURACION REGISTROS --------------------------------

                                org 1ff0h
        fdb 00

        
                                org 1fdfh

        fcb 082h
;-----------------------------------------------------------------------------

        END

