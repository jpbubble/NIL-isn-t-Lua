
local NILCode = [[
       void Hello()
          print("Hello World")
       end
       
       delegate Hi
       Hi = Hello
       Hi()
       
       Hi = void()
          print("Hi folks!")
       end
       Hi()
]]

local NIL=require "NIL"
local NILTrans = NIL.Translate(NILCode,"NILCode")

print("Original:\n"..NILCode,"\n\nTranslation:\n"..NILTrans)

print("Executing transation!")
local l = loadstring(NILTrans,"Translation")
l()

