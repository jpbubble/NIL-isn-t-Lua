--[[
  
  This program was set up to test a basic class.

  Please note, on the moment I wrote this the class translator was far from finished, so don't be too hasty to try this script.
  
]]


local NILCode = [[

  class MyClass
  
    int i = 2019
    string h = "Hello World"
    readonly string nochange="You ain't gonna change me"
    static int count = 40
    table testtable
    
//    void Hello()
//       print(self.h)
//       print("Written in "..self.i)
//    end
  
  end
    
    MyClass MC
    MyClass MC2
    MC = MyClass.NEW()
    MC.h = "Hello Earth"
    MC.count = MC.count + 1
    MC2 = MyClass.NEW()
    MC2.i = MC2.i * 2
    print(MC.count,MC.h,MC.i)
    print(MC2.count,MC2.h,MC2.i)
//  MC.Hello()
    
]]


local NIL=require "NIL"
local NILTrans = NIL.Translate(NILCode,"NILCode")

print("Original:\n"..NILCode,"\n\nTranslation:\n"..NILTrans)

print("Executing transation!")
local l = loadstring(NILTrans,"Translation")
l()