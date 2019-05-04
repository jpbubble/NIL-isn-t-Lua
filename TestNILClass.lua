--[[
***********************************************************
TestNILClass.lua
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
Version 19.05.04
]]



local NILCode = [[

  class MyClass
  
    int i = 2019
    string h = "Hello World"
    readonly string nochange="You ain't gonna change me"
    static int count = 40
    table testtable
    readonly int ro = 20
    readonly string bycons
    
    int _pi = 10
    private int pi = 20
    int pub = 30
    
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
    
    int getme()
       return 1
    end
    
    int m_pi(int v)
        self._pi = self._pi + v
        return self._pi
    end
    
    void CONSTRUCTOR(a,string b)
         print("Creating a class: ",a)
         self.bycons = b
    end
  
  end
    
    MyClass MC
    MyClass MC2
    MC = MyClass.NEW(1,"abc")
    MC.h = "Hello Earth"
    MC.count = MC.count + 1
    MC2 = MyClass.NEW(2,"def")
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
    
    print(MC.bycons,MC2.bycons)
    print("getme:",MC.getme())
    
    print("Private from method",MC.m_pi(2))
    
]]


local NIL=require "NIL"
local NILTrans = NIL.Translate(NILCode,"NILCode")

print("Original:\n"..NILCode,"\n\nTranslation:\n"..NILTrans)

print("Executing transation!")
local l = loadstring(NILTrans,"Translation")
l()
