--[[
  
  This program was set up to test a basic class.

  Please note, on the moment I wrote this the class translator was far from finished, so don't be too hasty to try this script.
  
]]


local NILCode = [[

  class MyClass
  
    int i = 2019
    string h = "Hello World
    
    void Hello()
       print(self.h)
       print("Written in "..self.i)
    end
  
  end
  
  MyClass MC
  MC = MyClass.New()
  MC.Hello()
    
]]


local NIL=require "NIL"
local NILTrans = NIL.Translate(NILCode,"NILCode")

print("Original:\n"..NILCode,"\n\nTranslation:\n"..NILTrans)

print("Executing transation!")
local l = loadstring(NILTrans,"Translation")
l()