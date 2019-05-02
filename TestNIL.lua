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
    
    string Cow = "Moo"
    global int Paws = 4
    int mPaws
    mPaws = Paws * 20
    print(Paws,Cow,mPaws)
    do 
       int localint = 1
       print(localint)
       while localint<10
          localint = localint + 1
       end
    end
    if Paws==4 then print('Four paws') end
    if Cow=="Moo"
       print("The cow moos")
    elseif Cow=="woof"
       print("The cow speaks dog languages")
    else
       print("Whatever")
    end
    
    // print(localint) // should cause an error if not set as a comment!
]]
local NILTrans = NIL.Translate(NILCode,"NILCode")

print("Original:\n"..NILCode,"\n\nTranslation:\n"..NILTrans)