--[[
NIL_Love.lua
Copyright (C)  Jeroen P. Broks
Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
Version 19.05.07
]]





assert(not NIL,"Hey! NIL may not be loaded prior to calling NIL_Love!")
NIL = require("NIL"); local NIL=NIL

local driver_source = [[

     class NIL_Love_Driver
     
         bool Exists(string filename)
            int major
            int minor
            int revision
            major, minor, revision, codename = love.getVersion( )
            if major<11
               // Deprecated for no reason at all in LOVE 11.x
               // So I had to take this in order...
               return love.filesystem.exists( filename )
            end
            return love.filesystem.getInfo(filename)!=nil
         end
         
         string Load(string filename)
            return love.filesystem.read(filename)
         end
     end
     
     return NIL_Love_Driver.NEW()    

]]

NIL.UseStuff = NIL.Load(driver_source)

