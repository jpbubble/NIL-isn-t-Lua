--[[
NIL.lua
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
Version 19.05.21
]]








-- Variables
local macros = {["!="]="~="}
local vars = {}
local functions = {}
local classes = {} -- reserved for when classes are implemented!
local luakeywords = {"if","do","for","while","then","repeat","end","until","elseif","else","return", "break", "in", "not","or","and","nil","true","false","goto",
                     "self","switch","case","default","forever","module","class","static","get","set","readonly","private", "get", "set","module"} -- please note that some keywords may still have some "different" behavior! Although 'switch' is not a Lua keyword it's listed here, as it will make my 'scope' translation easier...
local nilkeywords = {"delegate","number","int","void","string","var", "function","global","table","implementation","impl","forward","bool","boolean"} -- A few words here are actually Lua keywords, BUT NIL handles them differently in a way, and that's why they are listed here!
local operators   = {"==","~".."=",">=","<=","+","-","*","//","%","(",")","{","}","[","]",",","/","=","<",">","..",";"} -- Period is not included yet, as it's used for both decimal numbers, tables, and in the future (once that feature is implemented) classes.
local idtypes     = {"var",["variant"]="var",["int"]="number","number","string","function",["delegate"]="function","void",["bool"]="boolean","boolean"}
local globusedforuse = {}


local used = {}

local mNIL = {}

-- locals are faster than gloabls
local loadstring = loadstring or load
local io=io
local assert=assert
local load=load
local string=string
local ipairs=ipairs
local pairs=pairs
local table=table
local type=type
local tonumber=tonumber
local print=print


-- A few functions I need to get NIL to work anyway!
local replace = string.gsub
local ASC=string.byte
local sprintf = string.format


local function dbg(myvarname,myvar,level)
       local ret = ""
       for i=1,level or 1 do ret = ret .."\t" end
       ret = ret .. sprintf("%s %s: ",type(myvar),myvarname)
       -- I really NEED a case routine!
       if type(myvar)=="nil" then
          ret = ret .. "nil\n"
       elseif type(myvar) == "userdata" or type(myvar) == "function" then
          ret = ret .. "~\n"
       elseif type(myvar) == "string" or type(myvar)=="number" then
          ret = ret .. myvar .. "\n"
       elseif type(myvar) == "table" then
          ret = ret .. "\n"
          for k,v in pairs(myvar) do
              ret = ret .. dbg(k,v,(level or 1)+1)
          end
       elseif type(myvar) == "boolean" then
          if myvar then ret = ret .. "true\n" else ret = ret .. "false\n" end
       else
          ret = ret .. "WTF???\n" -- This should never be possible to happen!
       end
       return ret
end


local function split(inputstr, sep)
        if sep == nil then
                sep = "%s"
        end
        local t={} ; local i=1
        for str in string.gmatch(inputstr, "([^"..sep.."]+)") do
                t[i] = str
                i = i + 1
        end
        return t
end


local function split(inputstr, sep)
        if sep == nil then
                sep = "%s"
        end
        local t={} ; local i=1
        for str in string.gmatch(inputstr, "([^"..sep.."]+)") do
                t[i] = str
                i = i + 1
        end
        return t
end
      

local substr = string.sub
local function left(s,l)
return substr(s,1,l)
end local Left=left

local function right(s,l)
local ln = l or 1
local st = s or "nostring"
return substr(st,-ln,-1)
end local Right=right

local function mid(s,o,l)
  local ln=l or 1
  local of=o or 1
  local st=s or ""
  return substr(st,of,(of+ln)-1)
end local Mid=mid

-- slower, more amateur, but it should fix the whiteline ignore bug!
local function brutesplit(inputstr,sep)
      local s = sep or "\n"
      local t={}
      local i=1
      local ti=1
      local skip=0
      for i=1,#inputstr do
          if mid(inputstr,i,#sep)==sep then 
             ti = ti + 1 
             skip=#sep-1
          elseif skip>0 then
             skip = skip - 1 
          else
             t[ti] = t[ti] or ""
             t[ti] = t[ti] .. mid(inputstr,i,1)
          end
      end
      -- for i,v in ipairs(t) do print(i,v) end
      return t
end


local function trim(s)
  return (s:gsub("^%s*(.-)%s*$", "%1"))
end

local function prefixed(str,prefix) 
   return left(str,#prefix)==prefix 
end


local function suffixed(str,suffix) 
   return right(str,#suffix)==suffix 
end

local function itpairs(mytab)
  local i = 0
  return function()
      i = i + 1
      if mytab[i] then return i,trim(mytab[i]) end
      return nil,nil
  end
end

local function tcontains(t,v)
    for i,iv in ipairs(t) do
        if iv==v then return i end
    end
    return nil 
end

local function spairs(t, order)
    -- collect the keys
    local keys = {}
    local t2 = {}
    for k,v in pairs(t) do keys[#keys+1] = k  t2[k]=v end
    -- if order function given, sort by it by passing the table and keys a, b,
    -- otherwise just sort the keys 
    if order then
        table.sort(keys, function(a,b) return order(t, a, b) end)
    else
        table.sort(keys)
    end
    -- return the iterator function
    local i = 0
    return function()
        i = i + 1
        if keys[i] then
            return keys[i], t2[keys[i]]
        end
    end
end

local function ValidForIdentifier(str)
   -- Regular expressions may be faster, but I never mastered that, and this way I can be sure it works!
   local ret = true
   for i=1,#str do
       local c=mid(str:upper(),i,1)
       local b=ASC(c)
       if (i==1 and b>=48 and b<=57) then return false end -- Identifiers may NOT begin with a number!
       ret = ret and (c=="_" or (b>=65 and b<=90) or (b>=48 and b<=57))
   end
   return ret
end

local function chop(amystring,pure,atrack) 
  if trim(amystring)=="" then return {} end
  local track = atrack or "???"
  --[[ primitive method
  local i=0
  local wstring
  local tstring  = mystring
  repeat
     wstring = tstring
     tstring = replace(tstring,"\r", " ")
     tstring = replace(tstring,"\t", " ")
     tstring = replace(tstring,"  ", " ")
  until wstring == tstring
  local chopped=split(tstring)
  -- ]]  
  -- --[[ 'pro' method
  local chopped = {}
  local gword = ""
  local instring
  local openstring=nil
  local wt=""
  -- dirty force fix on identifiers not taking well to ) next to them (for reasons beyond me)
  -- Due to replace using 'RegEx' I'll have to do this the 'long way'
  local mystring=""
  do 
   local o = 0
   local s = nil
   for i=1,#amystring do
     if mid(amystring,i,1)=="\"" or  mid(amystring,i,1)=="'" then 
         if not s then s = mid(amystring,i,1)
         elseif mid(amystring,i,1)==s then s=nil end
     end
     -- print(i,s,amystring,mystring,mid(amystring,i,1))
     if s then mystring = mystring .. mid(amystring,i,1) end
     if not s then
      if o==0 then
        for _,op in ipairs(operators) do 
            if mid(amystring,i,#op)==op then o=#op mystring = mystring .. " "..op.." " break end
        end
      end
      if o>0 then o = o - 1 elseif mid(amystring,i,1)=="\t" or mid(amystring,i,1)=="\r" then mystring = mystring .. " " else mystring = mystring .. mid(amystring,i,1) end
     end
   end
   local changed
   repeat
     local ns = replace(mystring,"  "," ")
     changed = ns~=mystring
     mystring=ns
   until not changed
    -- print ( mystring )
  end
  for i=1,#mystring do
      local c=mid(mystring,i,1)
      local b=ASC(c)
      -- print(sprintf("%s: Character pos %4d/%4d; char %2X/%3d/%s; Word: %s; str: %s; wt: %s; << \"%s\"",track,i,#mystring,b,b,c,gword,openstring or "<< nil >>",wt,mystring)) -- debugline
      if i<#mystring and mid(mystring,i,2) == "//" and (not openstring) then
         if gword~="" then 
            chopped[#chopped+1]=gword
         end
         local ci=i+2
         gword = "// comment: "..mid(mystring,ci,#mystring-ci)
         break
      elseif (c=='"' or c=="'") and (not openstring) then 
         openstring=true
         if gword~="" then 
            chopped[#chopped+1]=gword
            gword=c
         end
         openstring=c
      elseif (c=='"' or c=="'") and openstring==c then 
         if (left(gword,1)~=c) then gword=c..gword end
         chopped[#chopped+1]=gword..c
         gword=""
         openstring=nil
      elseif openstring then
         gword=gword..c
      elseif c=="\t" or c==" " or c=="\r" or c=="\n" then -- (normally \n should be impossible, but 'just in case')
         if gword~="" then 
            chopped[#chopped+1]=gword
            gword=""
         end
         wt=""
      elseif c=="#" then
         assert(gword=="","Unexpected # in "..track)
         gword="#"
         wt="txt"
      elseif (i>1 and tcontains(operators,mid(mystring,i-1,2))) or (i<#mystring and tcontains(operators,mid(mystring,i,2))) or tcontains(operators,c) then
         if wt=="txt" then
            chopped[#chopped+1]=gword
            gword=""
         end
         gword=gword..c
         wt="op"
      elseif c=="_" or (b>=65 and b<=90) or (b>=48 and b<=57) or (b>=97 and b<=122) or (c==".") or (c==":") then
        if wt=="op" then
           chopped[#chopped+1]=gword
           gword="" 
        end
        gword = gword..c
        wt="txt"
      else 
        error("NC: I don't know character: "..c.. "; "..track)
      end
  end
  assert(not openstring,"NC: Unfinished string in "..track)
  if gword~="nil" then chopped[#chopped+1] = gword end
  --]]
  if pure then return chopped end
  local ret = {}
  for c,e in ipairs(chopped) do
      local word = {}
      ret[#ret+1]=word
      word.word=e
      word.type="Unknown"
      if     tcontains(luakeywords,word.word) then
         word.type="LuaKeyword"
      elseif left(e,1)=="#" and c==1 then
         word.type="NIL_directive"
      elseif left(e,1)=="#" then
         word.type="ElementCounter"
      elseif left(e,2) == "//" then
         word.type="Comment"
      elseif tcontains(nilkeywords,word.word) or nilkeywords[word.word] then
         word.type = "NILKeyword"
      elseif tcontains(operators,word.word) then
         word.type = "Operator"
      elseif tonumber(word.word) then -- If not a number, 'tonumber' returns 'nil' causing the boolean expression to be false.
         word.type ='number' 
      elseif (left(word.word,1)=='"' and right(word.word=='"') or (left(word.word,1)=="'" and right(word.word,1)=="'")) then
         word.type = "string"
      elseif vars[e] then
         word.type = "NIL_identifier"
         word.NILType = word.vars[e]      
      elseif _G[e] then
         word.type = "Lua_identifier"
         word.LuaType = type(_G[e])
      end
  end
  return ret
end

-- Simple serializer routine
function mNIL.LuaSerialize(name,variable,level)
    local LS=mNIL.LuaSerialize
    level = level or 0
    local function safe(str)
       local ret= ""
       for i=1,#str do
           local ch = mid(str,i,1)
           local bt = string.byte(ch) 
           if (bt>=97 and bt<=122) or (bt>=48 and bt<=57) or (bt>=65 and bt<=90) or ch==" " or ch=="_" then ret = ret .. ch else ret = ret .. "\\"..right("00"..bt,3) end
       end
       return ret
    end
    local ret = name .. " = "
    local tabs = ""
    for i=1,level do tabs = tabs .. "\t" end
    if type(variable) == "table" then
       if getmetatable(variable) then
          return tabs.."-- "..name.." is set with a meta table can therefore not be serialized. If it's a class, serializes it separately! --"
       end
       ret = ret .. "{\n"
       local comma=false
       for k,v in pairs(variable) do
           if right(ret,2)=="--" then ret = ret .."\n" else
              if comma then ret = ret .. ",\n" end
              comma=true
           end
           if     type(k) == 'number' then 
              ret = ret .. LS('['..k..']',v,level+1)
           elseif type(k) == 'boolean' then
              local bk = 'false'; if v then bk='true' end
              ret = ret .. LS('['..bk..']',v,level+1)
           elseif type(k) == 'string' then
              ret = ret .. LS('["'..safe(k)..'"]',v,level+1)
           else
              error('NSL: Table with '..type(k)..' as key cannot be serialized!');
           end
       end
       ret = ret .. "}"
    elseif type(variable) == "number" then
       ret = ret .. variable
    elseif type(variable) == "boolean" then
       if variable then ret = ret .. "true" else ret = ret .. "false" end
    elseif type(variable) == 'string' then
       ret = ret .. '"'..safe(variable)..'"'
    elseif variable==nil then
       ret = ret .. "nil"
    else
       return tabs.."-- "..name.." is a "..type(variable).." and can therefore not be serialized in any way. --"
    end
    return tabs..ret
end
local LuaSerialize=mNIL.LuaSerialize

-- Stuff that NIL will need to make classes possible.
NILClass = {}

local function NewFromClass(classname,class, callconstructor, ...)
    assert(class,"NR,NH: Class hack!")
    assert(class.classname==classname,"NR,NH: Class naming hack!")
    local locked = false
    local allowprivate = false
    local trueclass = {}
    local faketable = {} -- Only used to link the meta table to...
    local metatable = {}
    
    trueclass.fields = {}
    trueclass.where = {}
    trueclass.statics = class.statics
    trueclass.methods = class.methods
    for k,v in pairs(class.fields) do
        local wh = trueclass.fields
        
        trueclass.where[k]="fields"
        if v.static then 
           --print("GOT STATIC")
           wh=trueclass.statics 
           trueclass.where[k]="statics" 
        end
        assert(not v.abstract,"NR,NH: Abstract fields are not allowed at all, and especially not in a new defintion!")
        --if (v.name~=k) then print("Mismatch >> ",v.name,k) end
        --assert(v.name==k,"NR,NH: Field naming mismatch!")
        --print("\027[32m"..dbg('trueclass',trueclass).."\027[0m")        
        v.name=k
        if (not wh[v.name]) then
          wh[v.name] = {}
          wh[v.name].declaredata = v
          wh[v.name].value = v.default
          if v.idtype=="table" then wh[v.name].value={} end
          wh[v.name].idtype=v.idtype
        end
    end
    
    local function getmethod(func,...)
       return function(...)
           allowprivate=true
           local ret = func(faketable,...)
           allowprivate=false
           return ret
       end
    end
    
    local function Dump(nostatic)
       local wheres = {"statics","fields"}
       local ret = "-- Dump out class --\n\n"
       if nostatic then wheres={"fields"} end
       --print(LuaSerialize("THE BIG SECRET",trueclass))
       for _,where in ipairs(wheres) do
           for k,field in pairs(trueclass[where]) do
               -- print(LuaSerialize('field',field))
               if field.declaredata.ikben=="field" then 
                  ret = ret .. LuaSerialize("self."..k,field.value).."\n"
               end
           end
       end
       return ret
    end
    
    function metatable.__index(tab,key)
       assert(type(key)=="string","NR: Invalid field")
       --if key=="DUMP" then return Dump() end
       if prefixed(key,".") then
          if key==".isnilclass" then return true end
          if key==".classname" then return class.classname end
          if key==".parent" then return class.parent end
          if key==".dump" then return Dump(true) end
          if key==".fulldump" then return Dump(false) end
          error("Invalid metakey")
       end
       local where = trueclass.where[key] or trueclass.where["$get."..key]
       --print(dbg("class",trueclass))
       assert(where,"NR: Class has neither field nor method called "..key)
       if (trueclass.where["$get."..key]) then
          local dd = trueclass[where]["$get."..key].declaredata
          if dd.static then 
             allowprivate=true
             local ret= dd.func()
             allowprivate=false
             return ret
          else
             return getmethod(dd.func)()
          end
       end
       local ret = trueclass[where][key]
       assert(ret,"NR,NI/NH: Field or method could not be properly retrieved: "..class.classname.."."..key)
       assert(allowprivate or (not ret.declaredata.private),"NR: Access to private element ("..key..") denied")
       if (ret.declaredata.ikben=="method") then
          if (where=='fields') then
             return getmethod(ret.declaredata.func)
          else
             return ret.declaredata.func
          end
       else
          return ret.value
       end
    end
    
    function metatable.__newindex(tab,key,value)       
       assert(type(key)=="string","NR: Invalid field")
       if prefixed(key,".") then
          if key==".dump" then
             local l = assert((loadstring or load)("self = ... \t"..value))
             allowprivate=true
             l(faketable)
             allowprivate=false
             return
          end
       end
       local where = trueclass.where[key] or trueclass.where["$set."..key]
       --print(dbg("trueclass",trueclass))
       assert(where,"NR: Class has neither field nor method called "..key)
       assert(where=="fields" or where=="statics","NR: You can only redefine field variables, and "..key.." belongs to the "..where)
       --print("\027[36m\027[40m"..dbg('trueclass',trueclass).."\027[0m")
       if (trueclass.where["$set."..key]) then
          local dd = trueclass[where]["$set."..key].declaredata
          if dd.static then 
             return dd.func(value)
          else
             return getmethod(dd.func)(value)
          end
       end
       local field = trueclass[where][key]
       assert(not (field.declaredata.readonly and locked),"NR: Tried to reassign a read-only field")
       assert(allowprivate or (not field.declaredata.private),"NR: Access to private ("..key..") element denied")
       local idtype=field.declaredata.idtype
       if idtype=='string' then
          if type(value)=="number" then value=""..value 
          elseif value==nil then value="nil" 
          elseif value==true then value='true'
          elseif value==false then value='false' end
       end
       assert(idtype,"NI: idtype==nil "..key)
       assert(
          (idtype=="var") or
          (idtype=="string" and type(value)=="string") or
          (idtype=="number" and type(value)=="number") or
          (idtype=="boolean" and type(value)=="boolean") or
          (idtype=="table" and (type(value)=="table" or value==nil)) or
          (idtype=="userdata" and (type(value)=="userdata" or value==nil)) or
          (idtype=="function" and (type(value)=="function" or value==nil)) or
          (classes[idtype] and NILClass.BelongsToClass(value,idtype)),"NR: Value of type "..idtype.." expected for "..key.." in class "..classname
       )
       trueclass[where][key].value=value
    end
    
    function metatable.__gc(tab)
        if (trueclass.fields.DESTRUCTOR) then
           assert(trueclass.fields.DESTRUCTOR.declaredata.func and trueclass.fields.DESTRUCTOR.declaredata.idtype=="void" and (not trueclass.statics.DESTRUCTOR),"NR: Destructors may only exist as non-static 'void' functions!") 
           allowprivate=true
           trueclass.fields.DESTRUCTOR.declaredata.func(faketable)
           allowprivate=false
        end
    end
    
    setmetatable(faketable,metatable)
    if (callconstructor and trueclass.fields.CONSTRUCTOR) then
       assert(trueclass.fields.CONSTRUCTOR.declaredata.func and trueclass.fields.CONSTRUCTOR.declaredata.idtype=="void" and (not trueclass.statics.CONSTRUCTOR),"NR: Constructors may only exist as non-static 'void' functions!")
       allowprivate=true
       trueclass.fields.CONSTRUCTOR.declaredata.func(faketable,...)
       allowprivate=false
    end
    locked=true
    return faketable
end


function NILClass.DeclareClass(name,identifiers,extends)
    local reservednames = {"NEW"}
    local class = classes[name]; assert(class,"NR,NH: Class name misinformation! Has the translation been altered with?")    
    local statics = {}; class.statics = statics
    local fields = {}; class.fields = fields
    local methods = {}; class.methods = methods
    class.classname = name
    if extends then
       class.parentname = extends
       class.parent = class[extends]
       for k,v in pairs(class.parent) do
           fields[k] = {}
           local cf=fields[k]
           cf.fromparent=true
           for fk,fv in pairs(class.parent.fields) do
               if (fk=="default" and class.parent.fields.idtype=="table") then cf.default = 'new table' else cf[fk]=fv end
           end
       end
    end
    
    for k,v in pairs(identifiers) do
        for _,na in ipairs(reservednames) do assert(k~=na,"NT: Cannot use '"..k.."' as element for a class, as the name has been reserved.") end
        local old = fields[k]
        if old then -- Check if overriding old stuff is allowed!
           assert(old.fromparent,"NT: Duplicate field/method: "..k)
           assert(not old.final,"NT: Final elements cannot be overridden: "..k)
        end
        fields[k] = {}
        local cf=fields[k]
        cf.fromparent=false
        cf.name=k
        for fk,fv in pairs(identifiers[k]) do
            if (fk=="default" and identifiers=="table") then cf.default = NILClass.Emptytable() else cf[fk]=fv end
        end
    end
    
    local forstatic
    local ret = {}
    local meta = {
        __index = function(t,k)
                    if k=="NEW" or k=="NEWNOCONSTRUCTOR" then 
                       return function(...) return NewFromClass(name,class,k=='NEW',...) end
                    else
                       assert(statics[k],"NR: Non-static or non-existent field called from static call (g:"..k..")")
                       return forstatic[k]
                    end
                  end,
        __newindex = function(t,k,v)
                     if k=="NEW" or k=="NEWNOCONSTRUCTOR" or k=="DUMP" then 
                       error("NT: You cannot redefine "..k)
                     end
                       assert(statics[k],"NR: Non-static or non-existent field called from static call (s:"..k..")")
                       forstatic[k]=v
                  end
    }
    setmetatable(ret,meta)
    forstatic = ret.NEWNOCONSTRUCTOR()
    return ret
end




function NILClass.BelongsToClass(v,c) -- Will also check parent variables *if* they exist!
   if v==nil then return true end -- Seems odd, but all classes can contain 'nil' so I must keep the possibility in mind
   if type(v)~='table' then return false end
   local checkclass=v
   while checkclass do
         if not checkclass[".classname"] then return false end
         if checkclass[".classname"] == c then return true end
         checkclass = checkclass[".parent"]
   end
   return false
end

local function ch2string(ch)
   --print(ch.type,ch.word)
   if ch.type~="string" then return ch.word end
   local ret = ch.word
   if prefixed(ret,'"') or prefixed(ret,"'") then ret=right(ret,#ret-1) end
   if suffixed(ret,'"') or suffixed(ret,"'") then ret= left(ret,#ret-1) end
   return ret
end

local function usef2id(usef)
   local u = replace(usef,"\\","/")
   local s = split(u,"/")
   local r = s[#s]
   local i = split(r,".")
   return i[1]
end

-- Translator itself
function mNIL.Translate(script,chunk)
    assert(type(script)=='string',"NT: Translate wants a string for a script and not a "..type(script))
    local ret = ""
    local lines = split(replace(script,"\n\n","\n// whiteline!\n"),"\n")
    local lmacro = {}
    local amacro = {lmacro,macros}
    local scopes = {[0]={kind="Base Scope",line=0}}
    local scopelevel = function() return #scopes end
    local scopetype = function() return scopes[#scopes].kind end
    local scopestart = nil
    local allowrepeatend
    local modules = {} -- Just an array in order to return stuff properly.
    local forwards = {}
    local accepted = {}
    local function newscope(kind,ln) scopes[#scopes+1] = { kind=kind, line=ln } end
    local function buildfunction(id,chopped,tpestart,track,needself)        
          -- error(chopped[tpestart].word) --check
          assert(chopped[tpestart].word=="(","NI: Function builder has been pointed wrong! "..track)
          local params = {}
          local assertion
          local wantass = ""
		  local gotass = ""
          local i = tpestart+1
          local function addassert(a,wa,ga)
             if wantass~="" then wantass=wantass..", " end wantass=wantass..wa 
			 if gotass~="" then gotass=gotass.."..', '.." end gotass=gotass..ga
             if not a then return end
             if assertion then
                assertion = assertion.." and "..a
             else
                assertion = a
             end
          end
          if needself then params[1]="self" end -- for non-static methods.
          assert(chopped[i],"NT: Unfinished function definition in "..track)
          while (chopped[i].word~=")") do
             while (chopped[i] and chopped[i].word=="") do i=i+1 end
             assert(chopped[i] and chopped[i+1],"NT: Unfinished function definition in "..track)
             local w = chopped[i].word
             local nw = chopped[i+1].word
             if w.word=="var" then -- optional, but implemented in case people don't realize it! ^_^
                addassert(nil,"var","type("..nw..")")
                params[#params+1]=nw
                chopped[i+1].type="Function Parameter"
                i = i + 2 
             elseif w=="string" then
                params[#params+1]=nw
                addassert( "type("..nw..")=='string'","string","type("..nw..")")
                chopped[i+1].type="Function Parameter"
                i = i + 2
             elseif w=="int" or w=="number" then
                params[#params+1]=nw
                addassert( "type("..nw..")=='number'","number","type("..nw..")")
                chopped[i+1].type="Function Parameter"
                i = i + 2
             elseif w=="bool" or w=="boolean" then
                params[#params+1]=nw
                addassert("type("..nw..")=='boolean'","boolean","type("..nw..")")
                chopped[i+1].type="Function Parameter"
                i = i + 2
             elseif w=="table" then
                params[#params+1]=nw
                addassert( "(nw==nil or type(nw)=='table')","table","type("..nw..")")
                chopped[i+1].type="Function Parameter"
                i = i + 2
             elseif w=="userdata" then
                params[#params+1]=nw
                addassert( "("..nw.."==nil or type("..nw..")=='userdata')","userdata","type("..nw..")")
                chopped[i+1].type="Function Parameter"
                i = i + 2
             elseif w=="function" or w=="delegate" then
                params[#params+1]=nw
                addassert( "("..nw.."==nil or type("..nw..")=='function')","function","type("..nw..")")
                chopped[i+1].type="Function Parameter"
                i = i + 2
             elseif classes[w] then
                params[#params+1]=nw
                addassert( "("..nw.."==nil or (type("..nw..")=='table' and "..nw.."._NIL_class='"..w.."')",w,"<?>")
                chopped[i+1].type="Function Parameter"
                i = i + 2
             else
                params[#params+1]=w
                chopped[i].type="Function Parameter"
                i = i + 1
             end
             while (chopped[i] and chopped[i].word=="") do i=i+1 end
             assert(chopped[i],"NT: Unexpected end of line in "..track)
             assert(chopped[i].word=="," or chopped[i].word==")","NT: Syntax error in "..track.."\nExpected either a ',' or a ')' but not a '"..chopped[i].word.."'")
             if chopped[i].word=="," then i=i+1 end
          end          
          local ret = "("
          for i,p in ipairs(params) do
              if p~="self" or i>1 then if tcontains(nilkeywords,p) or tcontains(luakeywords,p) then error("NT: Unexpected keyword '"..p.."' in "..track) end end
              if tcontains(operators,p) then error("NT: Unexpected operator '"..p.."' in "..track) end
              assert(ValidForIdentifier(p),"NT: Invalid identifier name '"..p.."' in "..track) 
              if i>1 then ret = ret ..", " end
              ret = ret .. p
          end
          ret = ret ..")"
          return ret,params,assertion,wantass,gotass
    end
    local function StartFunctionScope(line,func,id)
          ret = ret .. "function "..(id or "")..func.head
          if func.assertion then ret = ret .." assert("..func.assertion..",'NR: Function did not receive the parameters the way it wanted!\\n\\tWant:"..(func.wantass or "?") .."\\n\\tGot: \\t'.."..(func.gotass or "'?'")..")" end
          newscope("function",line)
          local scope = scopes[#scopes]
          scope.func = func
          scope.idtype = func.idtype
		  scope.line = line
          vars[#scopes] = {}
          for _,p in ipairs(func.params) do
              --print(#scopes,p,_)
              vars[#scopes][p]={idtype='var'} -- A stricter setup can come later, but for now this will do!
          end
    end
    
    local function ClassScope(chopped,track,linenumber)
            local tpestart=1
            local dostatic=false
            local doabstract=false
            local doreadonly=false
            local doprivate=false
            local doget,doset=false,false
            local idtype
            local id
            local default = "nil"
            local wscope = #scopes
            local fields = {}
            local blst = { [true]="true", [false]="false"}
            if prefixed(chopped[1].word,"//") then ret = ret .. "-- comment line!" return end
            do local getout repeat
               getout=true
               if chopped[tpestart].word=="static" then 
                  dostatic=true getout=false tpestart = tpestart + 1 
               elseif chopped[tpestart].word=="abstract" then 
                  doabstract=true getout=false tpestart = tpestart + 1
               elseif chopped[tpestart].word=="readonly" then
                  doreadonly=true getout=false tpestart = tpestart + 1
               elseif chopped[tpestart].word=="private" then
                  doprivate=true getout=false tpestart = tpestart + 1
               elseif chopped[tpestart].word=="get" then
                  doget=true getout=false tpestart = tpestart + 1
               elseif chopped[tpestart].word=="set" then
                  doset=true getout=false tpestart = tpestart + 1
               end
            until getout end
            assert( chopped[tpestart].type=="NILKeyword" , "NT: declaration syntax error (Unexpected "..chopped[tpestart].type..">"..chopped[tpestart].word.." in "..track )
            idtype=chopped[tpestart].word
            --assert(idtype~="class","NT: Classes have not yet been supported! "..track)
            if idtypes[idtype] then idtype=idtypes[idtype] end
            id = chopped[tpestart+1].word
            assert(id,"NT: Incomplete declaration")
            assert(chopped[tpestart+1].type=="Unknown","NT: Identifier name ("..chopped[tpestart+1].word..") seems known as a "..chopped[tpestart+1].type.." in "..track)
            assert(ValidForIdentifier(id),"NT: \""..id.."\" is not a valid identifier in "..track)
            do
               local getset
               if     (fields["$get."..id]) then getset = not(doset and not(fields['$set.'..id])) 
               elseif (fields["$set."..id]) then getset = not(doget and not(fields['$get.'..id])) end
               assert(
                  not(
                    fields[id] or getset
                  ),"NT: Duplicate field identifier \""..id.."\" in "..track)
            end
            if (doget) then
                local this=''                
                if not dostatic then
                   this='self'
                end
                ret = ret .. "\t['$get."..id.."'] = { ikben='get', idtype='"..idtype.."', name='"..id.."', static="..blst[dostatic]..", private="..blst[doprivate or prefixed(id,"_")]..", func=function("..this..")"
                scopes[#scopes+1] = {
                    kind='function',
                    idtype=idtype,
                    func = { params = {}, idtype=idtype },
                    head = "("..this..")",
                    linenumber=linenumber
                }
                if not dostatic then
                   scopes[#scopes].func.params = {"self"}
                   vars[#scopes] = { self= {idtype='var', ikben='variable', name='value'}}
                end
            elseif (doset) then
                local this=''
                local thiscomma=""
                if not dostatic then
                   this='self'
                   thiscomma="self,"
                end
                ret = ret .. "\t['$set."..id.."'] = { ikben='set', idtype='void', name='"..id.."', static="..blst[dostatic]..", private="..blst[doprivate or prefixed(id,"_")]..", func=function("..thiscomma.."value)"
                local assertion
                if idtype == "var" then assertion = nil
                elseif idtype=="number"   then assertion="type(value)=='number'"
                elseif idtype=="string"   then assertion="type(value)=='string'"
                elseif idtype=="boolean"  then assertion="type(value)=='boolean'"
                elseif idtype=="table"    then assertion="type(value)=='table' or value==nil"
                elseif idtype=="userdata" then assertion="type(value)=='userdata' or value==nil"
                elseif idtype=="void"     then error("NT: void vannot be used for this!")
                else                           assertion="value=nil or NIL.BelongsToClass(value,"..idtype..")"
                end
                if assertion then ret = ret .. "assert("..assertion..", \"NR: Expected '"..idtype.."' to set '"..id.."'\")" end
                scopes[#scopes+1] = {
                    kind='function',
                    idtype='void',
                    func = { params = {'value'}, assertion = assertion },
                    head = "("..thiscomma.."value)",
                    linenumber=linenumber
                }
                vars[#scopes]={value={idtype=idtype, ikben='variabe', name='value'}}
                if not dostatic then
                   scopes[#scopes].func.params = {'self','value'}
                   vars[#scopes].self = {idtype='var', ikben='variable', name='value'}
                end
                
            elseif #chopped>tpestart+2 and chopped[tpestart+2].word=="(" then   
               --error("NT: Methods not supported yet!") --[[
               assert(
                    chopped[#chopped].word==")" or 
                    (
                       prefixed(chopped[#chopped].word,"") and 
                       chopped[#chopped-1].word==")"
               ),"NT: Incomplete function declaration")
               -- print(dbg('chopped',chopped))
               local fd,fp,fa,wa,ga = buildfunction(id,chopped,tpestart+2,track,not dostatic)
               functions[wscope][id] = { idtype=idtype, head=fd, params=fp, assertion=fa, wantass=wa, gotass=ga }
               -- print(dbg('functions',functions))
               assert(not doreadonly,"The keyword 'readonly' is not valid for methods")
               ret = ret .. "\t['"..id.."'] = { ikben='method', idtype='"..idtype.."', name='"..id.."', static="..blst[dostatic]..", private="..blst[doprivate or prefixed(id,"_")]..", "
               if doabstract then
                  ret = ret .. "abstract = true, "
               else
                  ret = ret .. "func = "
                  StartFunctionScope(linenumber,functions[wscope][id])
               end
               --]]
            else

               --print(idtype)
               assert(idtype~="void","NT: Type 'void' has been reserved for functions only! "..track)
               assert(not doabstract,"NT: Keyword 'forward' is only valid for functions/methods; "..track)
               local pdefault,psdefault
               if #chopped>tpestart+2 and chopped[tpestart+2].word=="=" then
                  assert( #chopped>=tpestart+3 , "NT: Syntax error")
                  pdefault=chopped[tpestart+3]
                  psdefault=nil; if pdefault then psdefault=pdefault.word end
               end
               if idtype=="number" then 
                 default="0"
                 if pdefault then
                    assert(tonumber(psdefault),"NT: Constant number expected in "..track)
                 end
               elseif idtype=="string" then 
                  default='""'
                  if pdefault then
                    assert(pdefault.type=="string","NT: Constant string expected in "..track.."\n")
                  end  
               elseif idtype=="table" then 
                  default="'createonthespot'" -- If I didn't do it this way, all tables in all variables of this class would get the same pointer, getting one big mess! 
                  if pdefault then
                     error("NT: Full table definition from variable declaration not yet supported in "..track)
                  end
               elseif idtype=="boolean" then
                  default="false"
                  if pdefault then
                     assert(pdefault.word=="true" or pdefault.word=="false","NT: Constant boolean expected in "..track)
                  end
               elseif idtype=="var" then
                  if pdefault then
                     error("NT: Direct, declare and define is not yet supported for variants in "..track)
                  end
               else
                  error("NT: Type not yet supported in "..track)
               end
               ret = ret .. "\t['"..id.."'] = { ikben='field', idtype='".. idtype.."', name='"..id.."', default= "..(psdefault or default)..", static="..blst[dostatic]..", readonly="..blst[doreadonly]..", private="..blst[doprivate or prefixed(id,"_")].."},"
               fields[id]=true
               
            end
    end
    
    vars.globals = vars.globals or {}
    functions.globals = functions.globals or {}
    local purelua
    for linenumber,getrawline in itpairs(lines) do
         vars[scopelevel()] = vars[scopelevel()] or {}
         functions[scopelevel()] =  functions[scopelevel()] or {}
         local track = "line #"..linenumber.. "; chunk: "..(chunk or 'He-Who-Must-Not-Be-Name')
         local line = getrawline
         -- Let's first see what macros we have
         for _,m in ipairs(amacro) do for mak,rep in spairs(m) do
             line = replace(line,mak,rep)
         end end
         -- Let's chop the line up, shall we?
         local chopped = chop(line,false,track)
         if #chopped==0 then
            -- nothing happens!
         elseif prefixed(line,"#") then
            if chopped[1].word=="#pure" then
               purelua = true
            elseif chopped[1].word=="#endpure" then
               purelua = false
            elseif chopped[1].word=="#macro" or chopped[1].word=="#localmacro" then
               assert(#chopped>=3,"NT: Invalid macro defintion in "..track)
               local rest = ""
               local wmacro
               for i,r in ipairs(chopped) do
                   if i> 3 then rest=rest.." " end
                   if i>=3 then rest=rest..chopped[i].word end
               end
               if chopped[1].word=="#macro" then wmacro=macros else wmacro=lmacro end
               assert(not wmacro[chopped[2].word] , "Duplicate macro in "..track)
               wmacro[chopped[2].word] = rest
               ret = ret .. "--[[ defined macro "..chopped[2].word.." to "..rest.." ]]\n"
            elseif chopped[1].word=="#repeatmayend" then
               local s = "yes"
               if chopped[2] then s=chopped[2].word end
               s = s:upper()
               allowrepeatend = s~="NO" and s~="FALSE" and s~="OFF"
               ret = ret .. "--[[ Autosetting changed ]]"
            elseif chopped[1].word=="#use" or chopped[1].word=="#globaluse" or chopped[1].word=="#localuse" then
               assert(chopped[2],"NT: #use/#localuse/#globaluse expects a library/module in "..track)
               local libname = ch2string(chopped[2])
               local idname  = usef2id(libname)
               local id_dat = {kind='var'}
               if chopped[3] and chopped[3].word~="" then 
                  assert(chopped[3].word:upper()=="AS","NT: 'as' expected no "..chopped[3].word.." in "..track)
                  assert(chopped[4],"NT: identifer expected after 'as'")
                  idname=chopped[4].word
               end
               mNIL.Use(libname) -- Make sure _G and NIL's global settings are updated
               assert(ValidForIdentifier(idname),"NT: Invalid identifier name ("..idname..") in "..track)
               if chopped[1].word=="#use" then
                  assert(globusedforuse[idname] or (not vars.globals[idname]),"NT: #use request leads to duping identifier in "..track)
                  globusedforuse[idname]=true
                  vars.globals[idname]=id_dat
                  vars[#scopes]=vars[#scopes] or {}
                  vars[#scopes][idname]=id_dat
                  assert(vars[#scopes][idname],"NT: #use dupes a local identifier in "..track)
                  ret = ret .. idname.." = UseNIL(\""..libname.."\") local "..idname.." = "..idname --.."\n"                  
               elseif chopped[1].word=="#localuse" then
                  vars[#scopes]=vars[#scopes] or {}
                  assert(vars[#scopes][idname],"NT: #use dupes a local identifier in "..track)
                  ret = ret .. "local "..idname.." = UseNIL(\""..libname.."\")\n"
               elseif chopped[1].word=="#globaluse" then
                  assert(globusedforuse[idname] or (not vars.globals[idname]),"NT: #use request leads to duping identifier in "..track)
                  globusedforuse[idname]=true
                  vars.globals[idname]=id_dat
                  ret = ret .. idname.." = UseNIL(\""..libname.."\")\n"
               end
            elseif chopped[1].word=="#accept" then
               assert(chopped[2],"NT: Accept without stuff in "..track)
               assert(chopped[2].type=="Unknown" or chopped[2].type=="Lua_identifier","NT: Invalid #accept! I cannot accept "..chopped[2].type.." in "..track)
               accepted[chopped[2].word]=true;
            else
               error("Unexpected/unknown directive in "..track)
            end
         elseif purelua then
             ret = ret .. getrawline
         elseif scopes[#scopes].kind=="class" or scopes[#scopes].kind=="module" then
            local scope=scopes[#scopes]
            vars[scopelevel()] = vars[scopelevel()] or {}
            functions[scopelevel()] =  functions[scopelevel()] or {}
            if #chopped == 1 and chopped[1] and chopped[1].word=="end" then
               ret = ret .. "}"
               if scope.extends then ret = ret .. ","..scope.extends end
               ret = ret .. ")"
               vars[#scopes]=nil
               functions[#scopes]=nil
               scopes[#scopes]=nil
            else
               ClassScope(chopped,track)
            end   
         elseif chopped[1].type=="NILKeyword" or classes[chopped[1].word] then -- only for declarations!
            local tpestart=1
            local doglobal=false
            local doforward=false
            local idtype
            local id
            local default = "nil"
            local wscope = #scopes
            do local getout repeat
               getout=true
               if chopped[tpestart].word=="global" then 
                  doglobal=true getout=false tpestart = tpestart + 1 
               elseif chopped[tpestart].word=="forward" then 
                  doforward=true getout=false tpestart = tpestart + 1
                  wscope='globals'
               end
            until getout end
            assert( chopped[tpestart].type=="NILKeyword" or classes[chopped[1].word], "NT: declaration syntax error in "..track )
            idtype=chopped[tpestart].word
            --assert(idtype~="class","NT: Classes have not yet been supported! "..track)
            if idtypes[idtype] then idtype=idtypes[idtype] end
            id = chopped[tpestart+1].word
            assert(id,"NT: Incomplete declaration")
            assert(chopped[tpestart+1].type=="Unknown","NT: Identifier name ("..chopped[tpestart+1].word..") seems known as a "..chopped[tpestart+1].type.." in "..track)
            assert(ValidForIdentifier(id),"NT: \""..id.."\" is not a valid identifier in "..track)
            do
               local scopetest=scopelevel()
               if doglobal then scopetest="globals" end
               -- print(scopetest,type(vars),type(vars[scopetest]))
               assert(
                  not(
                    vars[scopetest][id] or 
                    classes[id] or 
                    functions[scopetest][id]
                  ),"NT: Duplicate identifier \""..id.."\" in "..track)
            end
            if #chopped>tpestart+2 and chopped[tpestart+2].word=="(" then   
               assert(
                    chopped[#chopped].word==")" or 
                    (
                       prefixed(chopped[#chopped].word,"") and 
                       chopped[#chopped-1].word==")"
               ),"NT: Incomplete function declaration")
               -- print(dbg('chopped',chopped))
               local fd,fp,fa,wa,ga = buildfunction(id,chopped,tpestart+2,track)
               functions[wscope][id] = { idtype=idtype, head=fd, params=fp, assertion=fa, wantass=wa, gotass=ga }
               -- print(dbg('functions',functions))
               if not doglobal then ret = ret .. "local " end
               if doforward then 
                  ret = ret .. id.." = function() error('NR: Call to a foward function ("..id..") which has not yet been implemented!') end"
                  forwards[id] = functions[id]
               else
                  StartFunctionScope(linenumber,functions[wscope][id],id)
               end
            else
               --print(idtype)
               assert(idtype~="void","NT: Type 'void' has been reserved for functions only! "..track)
               assert(not doforward,"NT: Keyword 'forward' is only valid for functions; "..track)
               local pdefault,psdefault
               if #chopped>tpestart+2 and chopped[tpestart+2].word=="=" then
                  assert( #chopped>=tpestart+3 , "NT: Syntax error")
                  pdefault=chopped[tpestart+3]
                  psdefault=nil; if pdefault then psdefault=pdefault.word end
               end
               if idtype=="number" then 
                 default="0"
                 if pdefault then
                    assert(tonumber(psdefault),"NT: Constant number expected in "..track)
                 end
               elseif idtype=="string" then 
                  default='""'
                  if pdefault then
                    -- print(pdefault.type)
                    assert(pdefault.type=="string","NT: Constant string expected in "..track.."\n"..(getrawline or ""))
                  end  
               elseif idtype=="table" then 
                  default="{}" 
                  if pdefault then
                     error("NT: Full table definition from variable declaration not yet supported in "..track)
                  end
               elseif idtype=="boolean" then
                  default="false"
                  if pdefault then
                     assert(pdefault.word=="true" or pdefault.word=="false","NT: Constant boolean expected in "..track)
                  end
               elseif idtype=="function" then
                  if pdefault then
                     error("NT:Function delegates cannot yet be defined in declaration in "..track)
                  end
               elseif idtype=="var" then
                  if pdefault then
                     error("NT: Direct, declare and define is not yet supported for variants in "..track)
                  end
               else
                  assert(classes[idtype],"NT: No type nor class known as "..idtype.." in "..track)
                  default = "nil"
               end
               if not doglobal then 
                vars[scopelevel()][id] = {idtype=idtype}
                ret = ret .. "local " 
               else
                vars.globals[id] = {idtype=idtype}
               end
               ret = ret .. sprintf("%s = %s",id,psdefault or default)
               
            end               
         else
            local IgnoreUntil
            for i,v in ipairs(chopped) do
              if IgnoreUntil then
                 if v.word==IgnoreUntil then IgnoreUntil=nil end
              elseif v.word~="" then -- I don't understand how these come in, but they do!
                --[[
                if i==1 then
                  local tabs=#scopes
                  if v.word=="end" or v.word=="forever" or v.word=="until" then tabs=tabs-1 end
                  for j=1,tabs do ret = ret .. "\t" end
                end
                ]]
                -- For now the order doesn't matter. When NIL can get stricter in variable checks, the order will have to be reversed.
                
                local vword = ""
                for i=1,#v.word do
                    local vc = mid(v.word,i,1)
                    if vc=="." or vc==":" then 
                       if vword~="" then
                          local vclass
                          for si=#scopes,0,-1 do                              
                              local sid = si; if sid==0 then sid='globals' end
                              vars[sid] = vars[sid] or {}
                              local fvar = vars[sid][vword] -- found var
                              if fvar and classes[fvar.idtype] then
                                 if classes[fvar.idtype].methods[vword] then
                                    v.word = vword..":"..right(v.word,#v.word-(#vword+1))
                                 else
                                    v.word = vword.."."..right(v.word,#v.word-(#vword+1))
                                 end
                              end
                           end
                       end
                       break;
                    end
                    vword = vword .. vc
                end
                vars[#scopes] = vars[#scopes] or {}
                functions[#scopes] = functions[#scopes] or {}
                local IsVar = vars.globals[vword] or functions.globals[vword] or classes[vword] or _G[vword]
                for i=0,scopelevel() do 
                    --print('scope #'..i.."/"..#scopes)
					vars[i]=vars[i] or {}
					functions[i]=functions[i] or {}
                    IsVar = IsVar or 
                         vars[i][vword] or 
                         functions[i][vword] 
                    end
                --[[
                if not(IsVar or v.type~="Unknown") then
                   print(dbg("chopped",chopped,0))
                   print(dbg("vars",vars,0),IsVar~=nil,v.type,v.word)
                end
                --]]
                assert(IsVar or accepted[vword] or v.type~="Unknown","NT: Unknown term \""..v.word.."\" in "..track)
                -- if (v.type=="Operator") then print(v.word.." > "..dbg("chopped",chopped)) end
                -- print(dbg('v',v),"\n"..dbg('scopes',scopes))
                if i~=1 then ret = ret .. " " end
                if prefixed(v.word,"//") then 
                   ret = ret .. "--"..Right(v.word,#v.word-2)
                elseif i~=1 and (v.word=='void' or v.word=='int' or v.word=='number' or v.word=='string' or v.word=='boolean' or v.word=='table' or v.word=='function' or v.word=='delegate' or v.word=="var" or classes[v.word]) then
                       assert(chopped[i+1] and chopped[i+1].word=="(","NT: Invalid delegate definition in "..track.."\nWord"..i.."\t"..v.word.."\n"..getrawline)
                       local fd,fp,fa,wa,ga = buildfunction("",chopped,i+1,track)
                       local f = { idtype=v.word, head=fd, params=fp, assertion=fa, wantass=wa, gotass=ga }
                       StartFunctionScope(linenumber,f)
                       IgnoreUntil=")"
                       --error("DEBUG -- operation in progress!")
                elseif luakeywords[v.word] or v.type=="LuaKeyword" then
                   -- print ("KEYWORD "..v.word)
                   if scopestart==v.word and scopestart~=")" then
                      ret = ret .. " "..v.word.." "
                      scopestart=nil
                   elseif v.word=="goto" then
                      error("Jump instructions are under no circumstances allowed in NIL, not even in Lua 5.2 or later! And no, don't even request me to add it to NIL, as it ain't gonna happen, and I even put this error in to deliberately block it out!!!")
                   elseif v.word=="do" then
                      ret = ret .. " do "
                      newscope("do",linenumber)
                   elseif v.word=="if" then
                      ret = ret .. " if "
                      newscope("if",linenumber)
                      scopestart="then"
                   elseif v.word=="elseif" then
                      assert(scopes[#scopes].kind=="if" or scopes[#scopes]=="elseif","NT: 'elseif' can only be used in connection with an 'if'/'elseif' scope in "..track)
                      ret = ret .. " elseif "
                      vars[#scopes] = nil
                      scopes[#scopes] = nil
                      newscope("if",linenumber)
                      scopestart="then"
                   elseif v.word=="while" then
                      ret = ret .. " while "
                      newscope("while",linenumber)
                      scopestart="do"
                   elseif v.word=="else" then
                      assert(scopes[#scopes].kind=="if" or scopes[#scopes]=="elseif","NT: 'elseif' can only be used in connection with an 'if'/'elseif' scope in "..track)
                      ret = ret .. " else "
                      vars[#scopes] = nil
                      scopes[#scopes] = nil
                      newscope("else",linenumber)
                   elseif v.word=="repeat" then
                      ret = ret .. " repeat "
                      newscope("repeat",linenumber)
                   elseif v.word=="forever" then
                      ret = ret .. " until false "
                      vars[#scopes] = nil
                      scopes[#scopes] = nil
                   elseif v.word=="until" then
                      ret = ret .. " until "
                      vars[#scopes] = nil
                      scopes[#scopes] = nil
                   elseif v.word=="for" then
                      -- print("for!")
                      ret = ret .. " for "
                      local fi=i+1
                      newscope("for",linenumber)
                      vars[#scopes] = {}
                      scopestart="do"
                      repeat
                         local fv = chopped[fi]
                         local fn = chopped[fi+1]
                         --print("for-test",fi,fv,fn)
                         assert(fv and fn and (fn.word=="," or fn.word=="=" or fn.word=="in"),"NT: Syntax error in "..track)
                         assert(ValidForIdentifier(fv.word),"NT: For variable declarion illegal in "..track)
                         assert(not vars[#scopes][fv.word],"NT: Duplicate for variable in "..track)
                         vars[#scopes][fv.word] = {idtype='var'}
                         fi = fi + 2
                      until fn.word~=","
                   elseif v.word=="in" and scopes[#scopes].kind=="for" and scopestart=="do" then
                       ret = ret .. " in "
                   elseif v.word=="break"  then
                      ret = ret .. " break "
                   elseif v.word=="or" or v.word=="and" or v.word=="not" or v.word=="true" or v.word=="false" or v.word=="self" or v.word=="nil" then
                      ret = ret .. " "..v.word.." "
                   elseif v.word=="end" then
                      assert(scopelevel()>0,"NT: Key word 'end' encountered, without any open scope!  "..track)
                      vars[#scopes] = nil
                      functions[#scopes] = nil
                      if scopetype()=="repeat" then
                         assert(allowrepeatend,"Keyword 'end' may in this setting not be used to end a repeat. You can change that with the '#repeatmayend' directive.")
                         ret = ret .. " until false "
                         scopes[#scopes] = nil
                      elseif scopetype()=="function" then
                         scopes[scopelevel()] = nil
                         ret = ret .. "end"
                      elseif scopetype()=="case" or scopetype()=="default" then
                         ret = ret .. "end end"
                         vars[scopelevel()]   = nil
                         functions[#scopes]   = nil
                         scopes[scopelevel()] = nil
                         vars[scopelevel()]   = nil
                         functions[#scopes]   = nil
                         scopes[scopelevel()] = nil
                      else
                         scopes[scopelevel()] = nil
                         ret = ret .. "end"
                      end
                      if scopes[scopelevel()].kind=="class" or scopes[#scopes].kind=="module" then ret = ret .. "}," end
                   elseif (((v.word=="class" or v.word=="module") and i==1) or (v.word=="class" and i==2 and chopped[1].word=="private") or (v.word=="private" and i==1 and chopped[2] and chopped[2].word=="class")) and #scopes==0 then
                       assert(#chopped>=2,"NT: Class or module requires definition!")
                       --newscope("class",linenumber)
                       newscope(v.word,linenumber)
                       local cscope = scopes[#scopes]
                       cscope.classname = chopped[2].word
                       if (#chopped>=3) then
                          assert(chopped[3].word=="extends","NT: Extends expected")
                          assert(#chopped==4,"NT: class to extend from expected")
                          cscope.extends = chopped[4].word
                       end
                       if cscope.extends then assert(classes[cscope.extends],"NT: Extend request from non-existent class in "..track) end
                       assert(not classes[cscope.classname],"NT: Duplicate class")
                       assert(not vars.globals[cscope.classname],"NT: Variable name used as classname")
                       assert(not functions.globals[cscope.classname],"NT: Function name used as classname")
                       assert(not luakeywords[cscope.classname],"NT: Keyword as classname")
                       assert(not nilkeywords[cscope.classname],"NT: Classname is keyword")
                       assert(not _G[cscope.classname],"NT: Cannot use globals defined in 'pure lua' as classname")
                       classes[cscope.classname]={ name = cscope.classname }
                       if v.word=="module" or chopped[1].word=="private" then ret = ret .. "local " end
                       ret = ret .. cscope.classname .. " = NILClass.DeclareClass('"..cscope.classname.."',{\n"
                       if v.word=="module" then modules[#modules+1] = cscope.classname end
                       break
                   elseif i==1 and v.word=="switch" then
                       ret = ret .. "do local switch=("
                       newscope("switch",linenumber)
                       vars[#scopes] = {}
                       scopestart=")"
                   elseif i==1 and v.word=="case" then
                       assert(scopes[#scopes].kind=="switch" or scopes[#scopes].kind=="case","NT: Case scopes expected in "..track)
                       if scopes[#scopes].kind=="switch" then ret = ret .. " if " else ret = ret .." elseif " vars[#scopes]=nil functions[#scopes]=nil scopes[#scopes]=nil end
                       local ex 
                       for i=2,#chopped do 
                           if chopped[i].word~="" then
                              assert(chopped[2].type=="string" or chopped[2].type=="number" or chopped[2].word=="true" or chopped[2].word=="false","NT: Constant required for case in "..track)
                              if not ex then ex="" else ex = ex .. " or " end
                              ex = ex .. "switch == "..chopped[i].word
                           end
                       end
                       ret = ret .. ex .. " then --"
                       newscope("case",linenumber)
                   elseif i==1 and v.word=="default" then
                       assert(scopes[#scopes].kind=="switch" or scopes[#scopes].kind=="case","NT: Case scopes expected in "..track)
                       if scopes[#scopes].kind=="switch" then ret = ret .. " do -- " else vars[#scopes]=nil functions[#scopes]=nil scopes[#scopes]=nil  ret = ret .." else -- " end                   
                       newscope("default",linenumber)
                   elseif v.word=="return" then
                        local retscope = #scopes
                        while(retscope>0) do
                           if scopes[retscope].kind=="function" then break end
                           -- print(dbg(sprintf("Scope #%d",retscope),scopes[retscope]))
                           retscope = retscope - 1
                        end
                        if retscope==0 then 
                           if #modules>0 then error("NT: 'return' may not be used in the main scope when modules are present!") end
                           ret = ret .. "return" 
                        else
                          local scope=scopes[retscope]
                          local func = scope.func
                          -- if (not func) then print(dbg('scope',scope)) end
                          -- print(dbg("Translating return",func))
                          if func.idtype=="void" then 
                             ret = ret .. "return;"  -- This will enforce an error if people try to return values through a void, and if not an error, the value will be ignored, either way, this blocks voids from returning values! :P
                          elseif func.idtype=="var" then
                             ret = ret .. "return " -- anything goes with 'var', yes even multiple returns.
                          elseif func.idtype=="number" then
                            if i==#chopped then
                                ret = ret .. "return 0"
                             else
                                ret = ret .. "return " -- stricter checkups CAN come later
                             end   
                          elseif func.idtype=="string" then
                             if i==#chopped then
                                ret = ret .. "return ''"
                             else
                                ret = ret .. "return " -- stricter checkups CAN come later
                             end
                          elseif func.idtype=="table" then
                             if i==#chopped then
                                ret = ret .. "return nil"
                             else
                                ret = ret .. "return " -- stricter checkups CAN come later
                             end
                          elseif func.idtype=="boolean" then
                             if i==#chopped then
                                ret = ret .. "return false"
                             else
                                ret = ret .. "return " -- stricter checkups CAN come later
                             end
                          elseif func.idtype=="var" then
                             ret = ret .. "return "
                          elseif func.idtype=="function" then
                             ret = ret .. "return "
                          else
                                 error("NT: Current setup cannot return in this kind of function yet! -- "..(func.idtype or "none set").." -- "..track)
                          end
                      end
                   else 
                      error("NT: Keyword '"..v.word.."' not expected in this situation in "..track)   
                   end
                else
                   ret = ret .. v.word
                end
              end
            end
         end
      if scopestart then ret = ret .. " "..scopestart end
      scopestart = nil
      ret = ret .."\n";
    end
    if scopelevel()~=0 then
       error(
         sprintf("NT: %s-scope in line #%d not properly ended, yet the end of the chunk has been reached in %s",scopes[#scopes].kind,scopes[#scopes].line or 0,chunk or "The chunk without a name")
       )
    end
    if #modules>0 then
       ret = ret .. "\nreturn "
       for i,m in ipairs(modules) do
           if i>1 then ret = ret .. "," end
           ret = ret .. m
       end
    end
    return ret
end

function mNIL.Load(script,chunk)
	local translation=mNIL.Translate(script,chunk)
	-- nildump = (nildump or -1) +1 local bt = assert(io.open("E:\\temp\\bubdebug\\NILDUMP"..nildump..".LUA","wb")) bt:write(translation .. "\n\n-- "..chunk) bt:close() -- debug must be dummied when not in use!
    local ret,error = loadstring(translation,"Trans: "..(chunk or "?"))
	if (not ret) and CSay then CSay("Error in translation:\n"..translation) end
	return ret,error
end

mNIL.LoadString = mNIL.Load

function mNIL.LoadFile(file,chunk)
	local f = assert(io.open(file, "rb"),"NL: Reading file "..file.." failed")
	local content = f:read("*all")
	f:close()
	return mNIL.Load(content,chunk or file)
end

local UseStuffScript = mNIL.Translate([[
class NIL_BASIC_USE
    
	get string NAME
	    return 'NIL Default'
	end

    bool Exists(string file)
      var f
      f=io['open'](file,"r")
      if f~=nil then f:close() return true else return false end
      // Primitive, but unfortunately when no APIs are present this is the only way Lua can do this.
    end
    
    string Load(string file)
       var f
       string content = ""
       //f=assert(io.open(file,"rb"),"Opening "..file.." failed") 
       f = assert(io.input(file),"NL: Opening "..file.." failed")
       // please note, since f is NOT a NIL class, but a plain Lua table ":" in stead of "." is still needed!
       //print(f,type(f),"?")
       repeat
           var line
           line = io.read() 
           if not line then break end
           content = content .. line .. "\n"
       forever
       io.close(f)
       return content
    end
    
end

return NIL_BASIC_USE.NEW()

]],"Default Use Script")
-- print(UseStuffScript) -- debug
local UseStuffF = assert(loadstring(UseStuffScript,"Default Use Class"))
-- print(UseStuffF)
local UseStuff = assert(UseStuffF())

mNIL.UseStuff = UseStuff
mNIL.UseStuffRestore = function() mNIL.UseStuff=UseStuff end


function mNIL.Use(lib,...)
    local letsuse = nil
    local ulib = lib:upper()
    if used[ulib] then 
       --print("Retrieve from used")
       return used[ulib] 
       -- --[[
    else
       --print("Not yet used before so creating new")
       --]]       
    end
    local pathlesslib
    do
       local s = split(lib,"/")
       pathlesslib=s[#s]
    end
    for _,lu in ipairs({
        lib..".nil",
        lib..".nlb/"..pathlesslib..".nil",
        lib..".lua",
        lib..".nlb/"..pathlesslib..".lua",
        lib,
        lib..".nlb/"..lib
    }) do
        if mNIL.UseStuff.Exists(lu) then letsuse=lu break end
    end
    assert(letsuse,"NU: I didn't find a way to properly import a library named "..lib)
    assert(suffixed(letsuse,".lua") or suffixed(letsuse,".nil"),"NU: Inproper library name!")
    local script = mNIL.UseStuff.Load(letsuse)
    if suffixed(letsuse,".nil") then
       local ret,err = mNIL.Load(script,letsuse)
       assert(ret,"NU: Compiling NIL translation failed: "..letsuse.."\n"..(err or "-- Lua error not caught properly"))
       used[ulib]=ret(...) or true
       return used[ulib]
    else
	   -- local bt = assert(io.open("E:\\temp\\bubdebug\\NILDUMP.LUA","wb")) bt:write(script .. "\n\n-- "..letsuse) bt:close() -- debug must be dummied when not in use!
       local ret,err = loadstring(script,letsuse)
       assert(ret,"NU: Compiling Lua script failed: "..letsuse.."\n"..(err or "-- Lua error not caught properly"))
       used[ulib]=ret(...) or true
       return used[ulib]
    end
end
UseNIL = mNIL.Use -- Make sure there's always a UseNIL. Also note! NEVER replace this with something else! NIL *will* throw an error

return mNIL













