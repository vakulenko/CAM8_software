// CodeVisionAVR V2.0 C Compiler
// (C) 1998-2010 Pavel Haiduc, HP InfoTech S.R.L.

// I/O registers definitions for the ATmega328

#ifndef _MEGA328_INCLUDED_
#define _MEGA328_INCLUDED_

#pragma used+
sfrb PINB=3;
sfrb DDRB=4;
sfrb PORTB=5;
sfrb PINC=6;
sfrb DDRC=7;
sfrb PORTC=8;
sfrb PIND=9;
sfrb DDRD=0xa;
sfrb PORTD=0xb;
sfrb TIFR0=0x15;
sfrb TIFR1=0x16;
sfrb TIFR2=0x17;
sfrb PCIFR=0x1b;
sfrb EIFR=0x1c;
sfrb EIMSK=0x1d;
sfrb GPIOR0=0x1e;
sfrb EECR=0x1f;
sfrb EEDR=0x20;
sfrb EEARL=0x21;
sfrb EEARH=0x22;
sfrw EEAR=0x21;   // 16 bit access
sfrb GTCCR=0x23;
sfrb TCCR0A=0x24;
sfrb TCCR0B=0x25;
sfrb TCNT0=0x26;
sfrb OCR0A=0x27;
sfrb OCR0B=0x28;
sfrb GPIOR1=0x2a;
sfrb GPIOR2=0x2b;
sfrb SPCR=0x2c;
sfrb SPSR=0x2d;
sfrb SPDR=0x2e;
sfrb ACSR=0x30;
sfrb MONDR=0x31;
sfrb SMCR=0x33;
sfrb MCUSR=0x34;
sfrb MCUCR=0x35;
sfrb SPMCSR=0x37;
sfrb SPL=0x3d;
sfrb SPH=0x3e;
sfrb SREG=0x3f;
#pragma used-

#define WDTCSR (*(unsigned char *) 0x60)
#define CLKPR (*(unsigned char *) 0x61)
#define PRR (*(unsigned char *) 0x64)
#define OSCCAL (*(unsigned char *) 0x66)
#define PCICR (*(unsigned char *) 0x68)
#define EICRA (*(unsigned char *) 0x69)
#define PCMSK0 (*(unsigned char *) 0x6b)
#define PCMSK1 (*(unsigned char *) 0x6c)
#define PCMSK2 (*(unsigned char *) 0x6d)
#define TIMSK0 (*(unsigned char *) 0x6e)
#define TIMSK1 (*(unsigned char *) 0x6f)
#define TIMSK2 (*(unsigned char *) 0x70)
#define ADCL (*(unsigned char *) 0x78)
#define ADCH (*(unsigned char *) 0x79)
#define ADCW (*(unsigned int *) 0x78) // 16 bit access
#define ADCSRA (*(unsigned char *) 0x7a)
#define ADCSRB (*(unsigned char *) 0x7b)
#define ADMUX (*(unsigned char *) 0x7c)
#define DIDR0 (*(unsigned char *) 0x7e)
#define DIDR1 (*(unsigned char *) 0x7f)
#define TCCR1A (*(unsigned char *) 0x80)
#define TCCR1B (*(unsigned char *) 0x81)
#define TCCR1C (*(unsigned char *) 0x82)
#define TCNT1L (*(unsigned char *) 0x84)
#define TCNT1H (*(unsigned char *) 0x85)
#define ICR1L (*(unsigned char *) 0x86)
#define ICR1H (*(unsigned char *) 0x87)
#define OCR1AL (*(unsigned char *) 0x88)
#define OCR1AH (*(unsigned char *) 0x89)
#define OCR1BL (*(unsigned char *) 0x8a)
#define OCR1BH (*(unsigned char *) 0x8b)
#define TCCR2A (*(unsigned char *) 0xb0)
#define TCCR2B (*(unsigned char *) 0xb1)
#define TCNT2 (*(unsigned char *) 0xb2)
#define OCR2A (*(unsigned char *) 0xb3)
#define OCR2B (*(unsigned char *) 0xb4)
#define ASSR (*(unsigned char *) 0xb6)
#define TWBR (*(unsigned char *) 0xb8)
#define TWSR (*(unsigned char *) 0xb9)
#define TWAR (*(unsigned char *) 0xba)
#define TWDR (*(unsigned char *) 0xbb)
#define TWCR (*(unsigned char *) 0xbc)
#define TWAMR (*(unsigned char *) 0xbd)
#define UCSR0A (*(unsigned char *) 0xc0)
#define UCSR0B (*(unsigned char *) 0xc1)
#define UCSR0C (*(unsigned char *) 0xc2)
#define UBRR0L (*(unsigned char *) 0xc4)
#define UBRR0H (*(unsigned char *) 0xc5)
#define UDR0 (*(unsigned char *) 0xc6)

// Interrupt vectors definitions

#define EXT_INT0 2
#define EXT_INT1 3
#define PC_INT0 4
#define PC_INT1 5
#define PC_INT2 6
#define WDT 7
#define TIM2_COMPA 8
#define TIM2_COMPB 9
#define TIM2_OVF 10
#define TIM1_CAPT 11
#define TIM1_COMPA 12
#define TIM1_COMPB 13
#define TIM1_OVF 14
#define TIM0_COMPA 15
#define TIM0_COMPB 16
#define TIM0_OVF 17
#define SPI_STC 18
#define USART_RXC 19
#define USART_DRE 20
#define USART_TXC 21
#define ADC_INT 22
#define EE_RDY 23
#define ANA_COMP 24
#define TWI 25
#define SPM_READY 26

// Needed by the power management functions (sleep.h)
#define __SLEEP_SUPPORTED__
#define __POWERDOWN_SUPPORTED__
#define __POWERSAVE_SUPPORTED__
#define __STANDBY_SUPPORTED__
#define __EXTENDED_STANDBY_SUPPORTED__
#asm
	#ifndef __SLEEP_DEFINED__
	#define __SLEEP_DEFINED__
	.EQU __se_bit=0x01
	.EQU __sm_mask=0x0E
	.EQU __sm_adc_noise_red=0x02
	.EQU __sm_powerdown=0x04
	.EQU __sm_powersave=0x06
	.EQU __sm_standby=0x0C
	.EQU __sm_ext_standby=0x0E
	.SET power_ctrl_reg=smcr
	#endif
#endasm

#ifdef _IO_BITS_DEFINITIONS_
#include <mega328p_bits.h>
#endif

#endif

