#include "logger/ConsoleLoggerOutput.h"
#include "logger/Logger.h"
#include "commonDebug.h"

#include <cstdarg>
extern "C"
{
#ifndef UNIT_TEST
#include "system.h"
#endif //UNIT_TEST
}
using namespace logger;

ConsoleLoggerOutput::ConsoleLoggerOutput()
{
	fLastLogTime = 0;
}

ConsoleLoggerOutput::~ConsoleLoggerOutput()
{
}

void ConsoleLoggerOutput::log(const char* component, const char* level, const char* str, va_list ap)
{
#ifdef UNIT_TEST
	uint16 length1 = sprintf(logBuffer,"%-10s:%-8s: ", component, level);
#else//UNIT_TEST
	uint16 length1 = sprintf(logBuffer,"%-10d:%-10s:%-8s: ", (uint32)getSystemTimeMs(), component, level);
#endif//UNIT_TEST
	uint16 length2 = vsprintf(logBuffer+length1, str, ap);
	length2 += length1;
	if (logBuffer[length2-1] == '\n')
	{
		logBuffer[length2-1] = '\0';
	}
	PRINTF("%s\n",logBuffer);
}
