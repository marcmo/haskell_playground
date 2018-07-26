ip_address="127.0.0.1"

function msg2wireFormat(a)
  assert(type(a)=="table","invalid message format")
  local s=""
  for k,v in pairs(a) do
    if string.len(s) == 0 then
      s = s..string.format("%x",v)
    else
      s = s..","..string.format("%x",v)
    end
    print("k,v was:" .. k .."," .. v)
  end
  return s
end

function foo (x)
  print ("calling foo")
  toSend={ 0xbf,0x0,0x1,0x1 }
  wireMsg = msg2wireFormat(toSend)
  print(toSend)
  print(wireMsg)
  return ("result is " .. wireMsg .. " <-")
end
