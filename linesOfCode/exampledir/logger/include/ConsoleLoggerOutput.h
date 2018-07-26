#ifndef CONSOLELOGGER_H_
#define CONSOLELOGGER_H_

#include "commonTypes.h"
#include "ILoggerOutput.h"

namespace logger
{
class ConsoleLoggerOutput : public ILoggerOutput
{
public:
	ConsoleLoggerOutput();
	virtual ~ConsoleLoggerOutput();
	
	virtual void log(const char* component, const char* level, const char* str, va_list ap);
	
private:

	static const uint32 BUFFER_SIZE = 300;
	char logBuffer[BUFFER_SIZE];	
	uint32 fLastLogTime;
};
}
#endif /*CONSOLELOGGER_H_*/
