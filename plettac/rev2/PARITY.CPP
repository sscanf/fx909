#include <stdio.h>

void main ()
{
	unsigned int n;
	unsigned char byte;
	unsigned char paridad=0;
	unsigned mask = 0x1;

	do
	{
		printf ("\n\nEntre byte : ");
		scanf ("%x",&byte);

		mask=1;
		paridad=0;
		for (n=0;n<8;n++)
		{
			if (mask&byte)
				paridad= !paridad;
			mask<<=1;
		}
		printf ("paridad = %d",paridad);
	}while (byte!=0);
}
