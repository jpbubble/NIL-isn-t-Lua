--[[
***********************************************************
TestNILDump.lua
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
Version 19.05.16
]]


local print=print
local NIL = require("NIL")
local a = { 1,20,300,4000,50000,600000,7000000,subtable={"Yo! Hey what's happenin', dude!",false}, justastring="LALALA!!",justanumber=323 }
print(NIL.LuaSerialize("a",a))

local b = NIL.Load([[
  class testclass
    int i = 4
    int a = 3
    string s = "Hello World!"
    static string sts = "Static String, yeah!"
    table t
    void H()
      print(self.s)
    end
    void CONSTRUCTOR()
       self.t = {123,231,[1000]="Duizend",['nogeentafel']={1,2,3,4,1102},true,false}
    end
  end
  
  
  
  return testclass.NEW()
]])()
local d = b[".dump"]
b.i=5
b.a=10
print(b[".dump"])
-- print(b[".fulldump"])
b[".dump"]=d
print(b.i,b.a)

