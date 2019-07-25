///////////////////////////////////////////////////////////
//
// PROGRAMA PARA EL MODEM MDTL
//
// **** Personalizado para PLETTAC ****
////        
// Parametros para el compilador: +FM +t
//
// 
////////////////////////////////////////////////////////////

#include <y:\picc\examples\16c63a.h>
#include <y:\picc\examples\ctype.h>
#include <y:\picc\examples\stdlib.h>
#include <y:\picc\examples\string.h>

#byte port_a=5
#fuses HS,NOWDT,PROTECT

#use FIXED_IO (b_outputs = PIN_B4)
#use delay (clock=20000000)

#define BUFFER_SIZE 32
//#include "modem.h"
//#include "host.h"

//Configuración EEPROM (9346)
#define CLOCK	PIN_A0
#define bkbhit (next_in!=next_out)

#use FAST_IO (B)

//Configuración comunicación RS232
#use rs232 ( ERRORS, baud=9600, PARITY = E, BITS=8,xmit=PIN_C6, rcv=PIN_C7)
void SendByte (int data);
void serial_isr();
int bgetc();
void SendMarca ();
byte RxByte ();

int buffer[BUFFER_SIZE];
byte next_in = 0;
byte next_out = 0;

void main (void)
{
	byte bt;

	enable_interrupts(global);
	enable_interrupts(int_rda);

	set_tris_a (0x2F);
	set_tris_b (0x1F);
	output_bit (PIN_B5,0);

	while (TRUE)
	{
		SendMarca ();	//Mientras no hayan datos, enviamos 0xff para indicar
							//señal de marca.

		
		while (input (PIN_B0))	//Generamos test
		{
			SendByte ('t');	
			SendByte ('e');	
			SendByte ('s');	
			SendByte ('t');	
		}

	    while(bkbhit)
		{
	        SendByte(bgetc());
		}
	}
}
         

byte RxByte ()
{    
	int n;
	char bt;
	
	for (n=0;n<8;n++)
	{
		while (!input (PIN_A2));
		shift_right (&bt,1,input (PIN_A3));
		while (input (PIN_A2));
	}
	while (!input (PIN_A2));
	while (input (PIN_A2));
	return bt;
}

void SendMarca ()
{
	int i;
	for(i=0;i<8;i++)
	{
		while (!input(PIN_A0));
		output_bit(PIN_A1, 1);
		while (input(PIN_A0));
	}
}
void SendByte (int data)
{
	int i;
	byte shf;
	int paridad;


	while (!input(PIN_A0));
	output_bit(PIN_A1, 0);			//Generamos bit de START
	while (input(PIN_A0));

	paridad=0;
	for(i=0;i<8;i++)
	{
		while (!input(PIN_A0));
		shf=shift_right (&data,1,0);	//Enviamos bit de datos
		output_bit(PIN_A1,shf);
		
		if (shf)
			paridad = !paridad;		//Calculamos paridad
		while (input(PIN_A0));
	}

	while (!input(PIN_A0));
	output_bit(PIN_A1, shift_right (&paridad,1,0));	//Enviamos paridad
	while (input(PIN_A0));

	while (!input(PIN_A0));	
	output_bit(PIN_A1, 1);		//Enviamos bit de STOP
	while (input(PIN_A0));
}

#int_rda
void serial_isr() {
	int t;
   buffer[next_in]=getc();
   t=next_in;
   next_in=(next_in+1) % BUFFER_SIZE;
   if(next_in==next_out)
     next_in=t;           // Buffer full !!
}

int bgetc() {
   int c;

   while(!bkbhit) ;
   c=buffer[next_out];
   next_out=(next_out+1) % BUFFER_SIZE;
   return(c);
}

