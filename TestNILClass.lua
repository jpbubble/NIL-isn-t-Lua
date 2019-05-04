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
    readonly int ro = 20
    
    void Hello()
       print(self.h)
       print("Just an integer num "..self.i)
    end
    
    void inc_i(int bi)
       self.i = self.i + bi
    end
    
    static int stic(int cu)
       print("self = ",self)
       return cu*2
    end
  
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
    MC.Hello()
    print("\n\n\nA few tests in methods")
    MC.inc_i(10)
    print(MC.i)
    print( MC.stic(50) )
    
    print(MC.ro)
    // MC.ro=20 // should cause an error!
    print(MyClass.count)
    MyClass.count = MyClass.count * 2    
    print(MyClass.count)
    
    
]]


local NIL=require "NIL"
local NILTrans = NIL.Translate(NILCode,"NILCode")

print("Original:\n"..NILCode,"\n\nTranslation:\n"..NILTrans)

print("Executing transation!")
local l = loadstring(NILTrans,"Translation")
l()