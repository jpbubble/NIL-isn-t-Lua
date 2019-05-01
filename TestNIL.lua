--[[

   All this program does is testing NIL.
   You won't need this file in your projects, it's only for testing purposes!
   
]]

local NIL=require "NIL"

local NILCode = [[
    // Testing!
    #macro HW "Hello World!"
    print("Hallo Welt") // This German variant was to test if this worked in both WITH and WITHOUT macros
    print(HW)
]]
local NILTrans = NIL.Translate(NILCode,"NILCode")

print("Original:\n"..NILCode,"\n\nTranslation:\n"..NILTrans)