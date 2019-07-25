;******************************************************************************
;*                                                                            *
;* NOMBRE       : UART.ASM                                                    *
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
SPCR     	equ   0ah		; spcr (serial peripheral control register)
SPSR     	equ   0bh      	; Serial Peripheral Status Register
SPDR     	equ   0ch      	; Serial Peripheral Data I/O Register
BAUD    	equ 000dh       ;BAUD rATE rEGISTER
SCDAT   	equ 0011h       ;sERIAL cOMUNICATIONS dATA rEGISTER
SCCR1   	equ 000eh       ;sERIAL cOMUNICATION rEGISTER 1
SCCR2   	equ 000fh       ;sERIAL cOMUNICATION rEGISTER 2
SCSR    	equ 0010h       ;sERIAL COMUNICATION sTATUS rEGISTER


;============================= SEGMENTO DE RAM ============================


;***************** CALCULADO PARA CRISTAL DE 2.4576 ***********************

UART_19200 equ 20h ;BAUD rate a 19200
UART_9600  equ 21h ;  "    "  "  9600 
UART_4800  equ 22h ;  "    "  "  4800

;================================ RAM ========================================

                                org 30h

sava	rmb 1
tail	rmb 1
head	rmb 1
BuffSCI	rmb 200

								
;==============================================================================

           
;========================= INICIALIZACIàN CPU ===============================

                           org 160H    ;comienzo rom
        lda #$c2
        sta $1fdf

  	
    	lda #$1
    	sta PCC	

        lda #$38
        sta PCD

		clr head
		clr tail

;==============================================================================

		
p19200:
		jsr b9600
        jsr IniSpi       ;Activa SPI

        cli        
main:       
		brset 1,PORTC,main
		lda tail
		cmp head
		beq main

		ldx tail
		lda BuffSCI,x
		jsr SpiOut
		inc tail
		
		lda tail
		cmp #200
		bne main
		
		clr tail			
		bra main
;-----------------------------------------------------------------------------

;=============================== SUBRUTINAS =================================

b4800:
    	lda #UART_4800
    	jsr SciOn
    	rts

b9600:
    	lda #UART_9600
    	jsr SciOn
    	rts

b19200:
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

	  	 bclr 0,PORTC
         sta   SPDR     ;Transmite el comando al 802
         brclr 7,SPSR,* ;espera que transmita el byte
		 bset 0,PORTC
         rts            


;---------------------------- inicialización spi -------------------------------
;***************************** imPORTAnte *************************************
;                                                      __
;para que el micro acepte ser master se ha de poner el ss a positivo (pata 37),

;si no es asi el micro rechaza el bit 4 del SPCR (master),
;
;******************************************************************************

IniSpi:                 
		 bset 0,PORTC
         lda   #$53     ;Serial Peripheral Interrupt Enable
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

        LDA #$2C        ;- TDRE interrupt disabled
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


;============================ RUTINAS INTERRUPION ===========================
SCI:
		lda SCSR
        lda SCDAT
        
        ldx head
        sta BuffSCI,x
		incx
        cpx #200
        bne CntSCI2
        clr head
        rti
CntSCI2:
        inc head
        rti

;--------------------------------- IRQ --------------------------------------

SPI:
IRQ:
timer:                 
SWI:

        RTI
;==============================================================================


;======================== VECTORES INTERRUPCION ===============================
                               ORG 1FF4H

        FDB SPI
        FDB SCI
        FDB timer
        FDB IRQ
        FDB SWI     
        FDB 160H
	    
;------------------------------------------------------------------------------
;-------------------- CONFIGURACION REGISTROS --------------------------------

                                org 1ff0h
	fdb 00

	
				org 1fdfh

	fcb 0c2h
;-----------------------------------------------------------------------------

        END

