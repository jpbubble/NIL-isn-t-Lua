--[[

      NIL
 
      NIL Isn't Lua -- set up by Jeroen P. Broks

]]

-- Variables
local macros = {}
local vars = {}
local luakeywords = {"if","do","for","while","then","repeat"}
local nilkeywords = {"number","int","void","string","var","return","module","class", "function","global","end","until"} -- A few words here are actually Lua keywords, BUT NIL handles them differently in a way, and that's why they are listed here!
local operators   = {"=","==","<",">",">=","<=","+","-","*","/","%","(",")","{","}","[","]",","} -- Period is not included yet, as it's used for both decimal numbers, tables, and in the future (once that feature is implemented) classes.
local mNIL = {}

-- locals are faster than gloabls
local io=io
local assert=assert
local load=load
local string=string
local ipairs=ipairs
local pairs=pairs
local table=table
local type=type
local tonumber=tonumber

-- A few functions I need to get NIL to work anyway!
local replace = string.gsub
local ASC=string.byte

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

local function trim(s)
  return (s:gsub("^%s*(.-)%s*$", "%1"))
end

local function prefixed(str,prefix) 
   return left(str,#prefix)==prefix 
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

local function chop(mystring,pure,atrack) 
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
  for i=1,#mystring do
      local c=mid(mystring,i,1)
      local b=ASC(c)
      local wt=""
      if i<#mystring and mid(mystring,i,2) == "//" and (not openstring) then
         if gword~="" then 
            chopped[#chopped+1]=gword
         end
         local ci=i+2
         gword = "-- comment: "..mid(mystring,ci,#mystring-ci)
      elseif c=='"' and (not openstring) then 
         openstring=true
         if gword~="" then 
            chopped[#chopped+1]=gword
            gword='"'
         end
         openstring='"'
      elseif c=="'" and (not openstring) then 
         openstring=true
         if gword~="" then 
            chopped[#chopped+1]=gword
            gword="'"
         end
         openstring="'"
      elseif (c=='"' or c=="'") and openstring==c then 
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
      elseif (tcontains(operators,c) or (i>1 and tcontains(operators,mid(mystring,i-1,2))) or (i<#mystring and tcontains(operators,mid(mystring,i+1,2)))) then
         if wt=="txt" then
            chopped[#chopped+1]=gword
            gword=""
         end            
         gword=gword..c
         wt="op"
      elseif c=="_" or (b>=65 and b<=90) or (b>=48 and b<=57) or (b>=97 and b<=122) or (c==".") then
        if wt=="op" then
           chopped[#chopped+1]=gword
           gword=""
        end
        gword = gword..c
        wt="txt"
      end
  end
  assert(not openstring,"Unfinished string in "..track)
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
      elseif tcontains(nilkeywords,word.word) then
         word.type = "NILKeyword"
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

-- Translator itself
function mNIL.Translate(script,chunk)
    local ret = ""
    local lines = split(script,"\n")
    local lmacro = {}
    local amacro = {lmacro,macros}
    for linenumber,getrawline in itpairs(lines) do
         local track = "line #"..linenumber.. "; chunk: "..(chunk or 'He-Who-Must-Not-Be-Name')
         local line = getrawline
         -- Let's first see what macros we have
         for _,m in ipairs(amacro) do for mak,rep in spairs(m) do
             line = replace(line,mak,rep)
         end end
         -- Let's chop the line up, shall we?
         local chopped = chop(line)
         if prefixed(line,"#") then
            if chopped[1].word=="#macro" or chopped[1].word=="#localmacro" then
               assert(#chopped>=3,"invalid macro defintion in "..track)
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
            end
         elseif chopped[1].type=="NILKeyword" then
            -- TODO: Try to detect declarations
         else
            for i,v in ipairs(chopped) do
                assert(v.type~="Unknown","Unknown term \""..v.word.."\" in "..track)
                if i~=1 then ret = ret .. " " end
                ret = ret .. v.word
            end
         end
    end
    return ret
end

function mNIL.Load(script,chunk)
    return load(mNIL.Translate(script,chunk))
end

mNIL.LoadString = mNIL.Load

function mNIL.LoadFile(file,chunk)
	local f = assert(io.open(file, "rb"))
	local content = f:read("*all")
	f:close()
	return mNIL.Load(content,chunk or file)
end

return mNIL
