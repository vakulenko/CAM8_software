#include <mega328.h>
#include <delay.h>

//User configurable definitions

#define KP		0.04


//System specific definitions
#define CYCLE   1020  //ms

typedef unsigned char uint8_t;
typedef unsigned int uint16_t;

#define TRUE    1
#define FALSE   0

#define TRUE_INV_PROT 0xaa55
#define FALSE_INV_PROT 0x55aa
#define HIGH_MASK_PROT 0xaa00

#define TEMP_OFFSET 1280 //128C

#define MIN_TEMP (TEMP_OFFSET-500) //-50C
#define MAX_TEMP (TEMP_OFFSET+500) //50C
#define FACTORY_TARGET_TEMP 1730   //25C

#define pixel4 PORTC = k0; \
               PORTC = k1; \
               PORTC = k2; \
               PORTC = k3; \
               PORTC = k4; \
               PORTC = k5; \
               PORTC = k6; \
               PORTC = k7; 
#define pixel8 pixel4 \
               pixel4
#define pixel40 pixel8 \
                pixel8 \
                pixel8 \
                pixel8 \
                pixel8      
#define pixel200 pixel40 \
                 pixel40 \
                 pixel40 \
                 pixel40 \
                 pixel40    
#define pixel1000 pixel200 \
                  pixel200 \
                  pixel200 \
                  pixel200 \
                  pixel200  
unsigned char k0;
unsigned char k1;
unsigned char k2;
unsigned char k3;
unsigned char k4;
unsigned char k5;
unsigned char k6;
unsigned char k7;
unsigned char k8;

uint16_t y0; 
uint16_t dy;
uint16_t expoz;
uint8_t bining;

uint8_t cmdReceived;
uint8_t command;
uint16_t parcom;

uint16_t sensorTemp;
uint16_t targetTemp;
uint8_t coolerOn;
uint8_t coolerPower;

uint16_t targetTempInBuffer;
uint8_t coolerOnInBuffer;
uint16_t sensorTempOutBuffer;
uint8_t coolerPowerOutBuffer;

/* Function prototypes */
void initDS12B20(void);
void readDS12B20(void);
void reset(void);
void wbyte(uint8_t byte);
uint8_t rbyte(void);
void PIDLoop (void);
void initCamera (void);

void frame(void);    
void shift(void);
void shift2(void);
void shift3(void);
void zad(void); 
void zad2(uint16_t expp);
void clearline(void);
void clearframe(void);
uint16_t resi(void);

/* Interrupt, triggered when new command arrives */
interrupt [PC_INT0] void pcint0(void)
{
    PCMSK0 = 0x00;
    parcom = resi();
    cmdReceived = TRUE;
    switch (command)
    {
        case 0x1b:
            frame();
            break;
        case 0x2b:
            shift3();
            break;
        case 0x3b:
            PORTD = 0x4f;
            zad2(1);
            PORTD = 0x4f;
            break;
        //ROI, StartY
        case 0x4b:
            y0 = parcom;
            break;
        //ROI, NumY
        case 0x5b:
            dy = parcom;
            break;
        //set exposure
        case 0x6b:
            expoz = parcom;
            break;
        //set binning                    
        case 0x8b:
            if ((parcom==TRUE) || (parcom==FALSE))
            {
                bining = parcom;
            }
            break;
        // on/off cooler
        case 0x9b:
            if ((parcom==TRUE) || (parcom==FALSE))
            {
                coolerOnInBuffer = parcom;
            }
            break;
        // set target temperature
        case 0xab:
            if ((parcom <= MAX_TEMP) && (parcom >= MIN_TEMP))
            {
                targetTempInBuffer = parcom;
            }
            break;
        case 0xcb:
            clearframe();
            break;
        //Init command - initialize MCU
        case 0xdb:
            initCamera();
            break;
        default:
            break;
    }     
    PCMSK0 = 0x20;
}

/* Init DS18B20 */
void initDS12B20(void)
{
	PORTB &= ~0x04;
    zad2(1);
    PORTB |= 0x04;
    DDRB = 0x13;
    zad2(1);
    DDRB = 0x17;
	//skip ROM
    wbyte(0xcc);
	// write on scratchPad
    wbyte(0x4e);
	// User byte 0 - Unused
	wbyte(0x00);
	// User byte 1 - Unused
	wbyte(0x00);
	// set up en 12 bits (0x7F)
	wbyte(0x7f);
}

/* Read sensor temperature */
void readDS12B20(void)
{
    uint16_t temperature = 0, fract = 0;
    uint8_t sign = 0;
    
    cmdReceived = FALSE;
    
    PORTB &= ~0x04;
    zad2(1);
    PORTB |= 0x04;
    DDRB = 0x13;
    zad2(1);
    DDRB = 0x17;
    //skip ROM
    wbyte(0xcc);
    //get data
    wbyte(0xbe);

    temperature = rbyte();
    temperature = temperature + 256*rbyte();
    
    if ((temperature & 0x8000) != 0x00)
	{
		sign = 1;
		temperature = 0xffff - temperature + 1;
	}
	else 
    {
        sign = 0;
    }
	fract = 0;
	if ((temperature & 0x01) != 0x00)
    {
        fract=fract+65;
    }
	if ((temperature & 0x02) != 0x00)
    {
        fract=fract+125;
    }
	if ((temperature & 0x04) != 0x00)
    {
        fract=fract+250;
    }
	if ((temperature & 0x08) != 0x00)
    {
        fract=fract+500;
    }
	temperature = (temperature >> 4) * 10 + fract / 100;
	if (sign == 1)
    {
        temperature = TEMP_OFFSET - temperature;
    }
	else
    {
        temperature = temperature + TEMP_OFFSET;
    }

    PORTB &= ~0x04;
    zad2(1);
    PORTB |= 0x04;
    DDRB = 0x13;
    zad2(1);
    DDRB = 0x17;
    //skip ROM
    wbyte(0xcc);
    //start conversion
    wbyte(0x44);
    
    //if interrupt occurs during temperature sensor reading - discard this reading
    if (cmdReceived == FALSE)
    {   
        //there is no error in temperature reading
        if ((temperature != 0x0000) && (temperature != 0xffff))
        {
            sensorTemp = temperature;
        }
    }
}

void reset(void)
{
    PORTB &= ~0x04;
    zad2(1);
    PORTB |= 0x04;
    DDRB = 0x13;
    zad2(1);
    DDRB = 0x17;
    wbyte(0xcc);
    zad2(1);
    wbyte(0x4e);
    wbyte(0x00);
    wbyte(0x00);
    wbyte(0x7f);
    zad2(1);
}

void wbyte(uint8_t byte)
{
    uint8_t i, j, buf;
    buf = byte;
    for (i = 0; i < 8; i++)
    {
        PORTB &= ~0x04;
        for (j = 0; j < 2; j++)
        {
            ;
        }
        PORTB |= ((buf & 1) << 2) & 0x04;
        for (j = 0; j < 50; j++)
        {
            ;
        }
        PORTB |= 0x04;
        for (j = 0; j < 100; j++)
        {
            ;
        }
        buf = buf >> 1;
    }
}

uint8_t rbyte(void)
{
    uint8_t i, j, buf = 0;
    for (i = 0; i < 8; i++)
    {
        buf = buf >> 1;
        PORTB &= ~0x04;
        for (j = 0; j < 2; j++)
        {
            ;
        }
        PORTB |= 0x04;
        DDRB = 0x13;
        for (j = 0; j < 12; j++)
        {
            ;
        }
        buf |= (PINB & 0x04)<< 5;
        for (j = 0; j < 30; j++)
        {
            ;
        }
        DDRB = 0x17;
        for (j = 0; j < 100; j++)
        {
            ;
        }
    }
    return(buf);
}

void PIDLoop (void)
{
    double U = 0.0;
    double E = 0.0;
    while (1)
    {
        readDS12B20();
            
        if (coolerOn == FALSE)
        {
            coolerPower=0x00;
            U=0.0;
            E=0.0;
            PORTB &=~0x01;
            delay_ms(CYCLE);
        }
        else
        {
            E = (double)sensorTemp - (double)targetTemp;
            U = U + KP * E;
            if (U > CYCLE)
            {
                U = CYCLE;
            }
            if (U <= 0.0)
            {
                U = 0.0;
            }            
            if (U > 0.0)
            {
                PORTB |= 0x01;    
            }
            delay_ms(U);                              
            if (((uint16_t) U)!=CYCLE)
            {
                PORTB &=~0x01;
            }
            delay_ms(CYCLE-U); 
            coolerPower=((uint8_t)(U/4));
        }
        //critical section
        #asm("cli");
        sensorTempOutBuffer = sensorTemp;
        coolerPowerOutBuffer = coolerPower;
        coolerOn = coolerOnInBuffer;
        targetTemp = targetTempInBuffer;
        #asm("sei");
    }
}

void initCamera (void)
{
    //8MHz
    CLKPR = 0x80; 
    CLKPR = 0x00;

    command = 0; 
    parcom = 0;
    k0 = 0x14;
    k1 = 0x2a;
    bining = 0;
    y0 = 0;
    dy = 1000;
    expoz = 0;
    cmdReceived = FALSE;

    DDRB = 0x17;
    PORTB = 0x04;
    DDRD = 0xff;
    PORTD = 0xcf;
    DDRC = 0x3f;
    PORTC = 0x00;

    reset();

    shift3();
    clearframe();
}

void main(void)
{ 
    initCamera();
    
    //init cooler basic variables
    sensorTemp = TEMP_OFFSET;
    targetTemp = FACTORY_TARGET_TEMP;   
    coolerOn = FALSE;
    coolerPower = 0;
    
    //init temperature sensor, perform first reading
    initDS12B20();
    readDS12B20();
    delay_ms(1200);
    readDS12B20();
    delay_ms(1200);
    
    //populate buffer
    sensorTempOutBuffer = sensorTemp;
    targetTempInBuffer = targetTemp;
    coolerOnInBuffer = coolerOn;
    coolerPowerOutBuffer = coolerPower;;

    // set PCIE0 to enable PCMSK0 scan
    PCICR = 0x01;
    // set PCINT5 to trigger an interrupt on state change
    PCMSK0 = 0x20;
    // turn on interrupts
    #asm("sei");

    PIDLoop();
}

void frame(void)
{
    uint16_t y;
    
    // turn off cooling
    PORTB &=~0x01;
    if (expoz > 55)
    {
        if (expoz <= 1000)
        {
            shift3();
        }
        zad2(expoz-55);
        clearline();
        clearframe();
    }
    else
    {
        clearline();
        clearframe();
        shift3();
        zad2(expoz);
    }

    shift2();

    //сброс первых 24 строк+ ROI и очистка горизонтального регистра
    y = 10+y0;
    do
    {
        shift();
    } while (--y);
    clearline();
    //для выравнивания яркости 1 строки
    shift();
    clearline();

    y = dy;
    k0 = 0x14;
    k8 = 0x28;
    do
    {
        shift();
        k0 = 0x14;
        //выключен s2, нет импульсов записи
        k1 = 0x28;
        k2 = 0x14;
        k3 = 0x28;
        k4 = 0x14; 
        k5 = 0x28;
        k6 = 0x14; 
        k7 = 0x28;
        pixel40
        pixel8

        k0 = 0x14;
        k1 = 0x2a;
        k2 = 0x14;
        k3 = 0x2a;
        k4 = 0x14;
        k5 = 0x2a;
        k6 = 0x14;
        k7 = 0x2a;
        //4 pix пустых (ad9826)
        pixel4

        if (bining == FALSE)
        {
            k0 = 0x14;
            k1 = 0x2a;
            k2 = 0x14;
            k3 = 0x2a;
            k4 = 0x14; 
            k5 = 0x2a;
            k6 = 0x14; 
            k7 = 0x2a;
        } else
        {
            k0 = 0x14;
            k1 = 0x2a;
            k2 = 0x1a;
            k3 = 0x2a;
            k4 = 0x1a; 
            k5 = 0x2a;
            k6 = 0x1a;
            k7 = 0x2a;
        }
        pixel1000
        pixel1000
        pixel1000
        pixel1000
        pixel1000
        pixel1000
        PORTC = k8; 

        k0 = 0x14;
        //выключен s2, нет импульсов записи
        k1 = 0x28;
        k2 = 0x14;
        k3 = 0x28;
        k4 = 0x14; 
        k5 = 0x28;
        k6 = 0x14; 
        k7 = 0x28;
        pixel40
        pixel40
        pixel4
    }while (--y);
}

void shift(void)
{
    PORTD = 0xcb;
    zad();
    PORTD = 0xdb;
    zad();
    PORTD = 0xda;
    zad();
    PORTD = 0xfa;
    zad();
    PORTD = 0xea;
    zad();
    PORTD = 0xee;
    zad();
    PORTD = 0xce;
    zad();
    PORTD = 0xcf;
    zad();
}

void shift2(void)
{
    shift();

    PORTD = 0xc7;zad();
    PORTD = 0xc7;zad();
    PORTD = 0xc7;zad();
    PORTD = 0xc7;zad();

    PORTD = 0xcb;zad();

    PORTD = 0xd9;zad();
    PORTD = 0xd9;zad();
    PORTD = 0xd9;zad();
    PORTD = 0xd9;zad();

    PORTD = 0xdb;zad();

    PORTD = 0xfa;zad();
    PORTD = 0xea;zad();
    PORTD = 0xee;zad();
    PORTD = 0xce;zad();
    PORTD = 0xcf;zad();
}

void shift3(void)
{
    PORTD = 0xcb;zad();
    PORTD = 0xdb;zad();
    PORTD = 0x9a;zad();
    PORTD = 0xba;zad();
    PORTD = 0xaa;zad();
    PORTD = 0xee;zad();
    PORTD = 0xce;zad();
    PORTD = 0xcf;zad();
}

void zad()
{
    uint8_t x;
    x = 7;
    do {
        
    } while(--x);
} 

void zad2(uint16_t expp)
{
    uint16_t x, y;
    for (y = 0; y < expp; y++)
    {
        x = 1347; 
        do {
            
        } while(--x);
    }
}

void clearline()
// Очистка горизонтального сдвига. Если его не очистить,
// то накопленный в нем паразитный заряд будет добавлен в первую строку изображения}
{
    uint16_t x;
    uint8_t l0,l1;

    l0 = 0x14;
    l1 = 0x20;

    x = 1600;
    do
    {
        PORTC = l0; 
        PORTC = l1;
        PORTC = l0; 
        PORTC = l1;
        PORTC = l0; 
        PORTC = l1;
        PORTC = l0; 
        PORTC = l1;
    } while (--x); 
}

void clearframe(void)
// Очистка сдвигового регистра. Если его не очистить,
// то накопленный в нем заряд будет добавлен в изображение.
// Очищается весь регистр вместе с "темными" и неиспользуемыми строками.
// Операция проводится перед "сливом" изображения в сдвиговый регистр.}
{
    uint16_t x;
    x = 1012;
    do
    {
        shift();
    } while (--x); 
    clearline();
}  
     
uint16_t resi(void)
{
    uint8_t x, count;
    uint16_t buf, buf2;
    
    buf = 0;

    for (x = 0;x < 8;x++)
    {
        count = 0;
        while ((PINB & 0x20) == 0)
        {
            count++;
            if (count > 50)
            {
                return buf;
            }
        }
        buf = buf << 1;
        if ((PINB & 0x08) == 0x08)
        {
            buf = buf + 0x0001;
        }
        count = 0;
        while ((PINB & 0x20) != 0)
        {
            count++;
            if (count > 50)
            {
                return buf;
            }
        }
    }
    command = buf;

    buf = 0;
    switch (command)
     {
        //send back current temperature
        case 0xbf:
            buf2 = sensorTempOutBuffer;
            break;
        //send back current target temperature
        case 0xbe:
            buf2 = targetTempInBuffer; 
            break;
        //send back current coolen On/Off status
        case 0xbd:
            if (coolerOnInBuffer == TRUE)   
            {
                buf2 = TRUE_INV_PROT;
            }
            else
            {
                buf2 = FALSE_INV_PROT;
            }
            break;
        //send back current cooler power percentage
        case 0xbc:
            buf2 = coolerPowerOutBuffer | HIGH_MASK_PROT;
            break;
        default:
            buf2 = 0;
            break;
     }
     
    for (x = 0;x < 16;x++)
    {
        count = 0;
        while ((PINB & 0x20) == 0)
        {
            count++;
            if (count > 50)
            {
                return buf;
            }
        }
        buf = buf << 1;
        if ((PINB & 0x08) == 0x08)
        {
            buf = buf + 0x0001;
        }
        if ((buf2 & 0x8000) == 0)
        {
            PORTB = PORTB & ~0x10;
        }
        else
        {
            PORTB = PORTB | 0x10;
        }
        buf2 = buf2 << 1;
        count = 0;
        while ((PINB & 0x20) != 0)
        {
            count++;
            if (count > 50)
            {
                return buf;
            }
        }
    }
    return buf; 
}