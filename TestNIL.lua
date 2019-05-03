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