#include "test.h"
int foo(int a)
{
	return 2*a;
}

uint8_t myBuffer[10];
int getBuffer(uint8_t** buffer)
{
    buffer = &myBuffer;
    return 10;
}

