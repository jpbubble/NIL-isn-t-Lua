--[[
***********************************************************
TestNILDelegate.lua
This particular file has been released in the public domain
and is therefore free of any restriction. You are allowed
to credit me as the original author, but this is not
required.
This file was setup/modified in:

If the law of your country does not support the concept
of a product being released in the public domain, while
the original author is still alive, or if his death was
not longer than 70 years ago, you can deem this file
"(c) Jeroen Broks - licensed under the CC0 License",
with basically comes down to the same lack of
restriction the public domain offers. (YAY!)
***********************************************************
Version 19.05.07
]]



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



