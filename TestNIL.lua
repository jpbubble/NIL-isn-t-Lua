--[[
***********************************************************
TestNIL.lua
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
Version 19.05.06
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
    int foreveralone=1
    repeat
       foreveralone = foreveralone + 1
       if foreveralone>10 then break end
    forever
    repeat
       foreveralone = foreveralone - 1
    until foreveralone <= 0
    while foreveralone % 5 != 0
       foreveralone=foreveralone + 3
    end
    for i = 1,10,20 
        print("Hello: "..i)
    end
    table tab
    tab[1]="Hoi"
    tab[2]="Dag"
    for i,d in ipairs(tab)
        print(i,d)
    end
    
    bool b1 = true
    boolean b2 = false
    bool b3
    // print(localint) // should cause an error if not set as a comment!
]]
local NILTrans = NIL.Translate(NILCode,"NILCode")

print("Original:\n"..NILCode,"\n\nTranslation:\n"..NILTrans)

