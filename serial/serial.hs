import System.Hardware.Serialport
import Control.Monad
import Control.Monad.Loops

main = do
  let settings = SerialPortSettings B115200 8 One NoParity NoFlowControl 100 
  s <- openSerial "/dev/tty0" settings
  -- or a little different
  s <- openSerial "/dev/tty0" settings { baudRate = B2400 }

  -- Sending
  forM_ "AT\r" $ sendChar s

  -- Receiving, using unfoldM from Control.Monad.Loops
  response <- unfoldM (recvChar s)

  print response

  closeSerial s

