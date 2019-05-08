--[[
***********************************************************
TestSwitch.lua
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
Version 19.05.08
]]

local NILCode =
[[ // Right, let's put this to the test!

for num=0,10 do
   
   switch num
      case 1
         print("One")
      case 2
         print("Two")
      case 3
         print("Three")
      case 4
         print("Four")
      case 5 6 7 8 9
         print("More than four, less than ten")
      default
         print("Je ne sais pas!")
   end 
   
end
]]

local NIL=require "NIL"
local NILTrans = NIL.Translate(NILCode,"NILCode")

print("Original:\n"..NILCode,"\n\nTranslation:\n"..NILTrans)

print("Executing transation!")
local l = loadstring(NILTrans,"Translation")
l()

