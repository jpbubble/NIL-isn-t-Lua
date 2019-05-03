--[[

      NIL
 
      NIL Isn't Lua -- set up by Jeroen P. Broks

]]

-- Variables
local macros = {["!="]="~="}
local vars = {}
local functions = {}
local classes = {} -- reserved for when classes are implemented!
local luakeywords = {"if","do","for","while","then","repeat","end","until","elseif","else","return", "break", "in", "not","or","and",
                     "switch","case","default","forever","module","class","static","get","set","readonly"} -- please note that some keywords may still have some "different" behavior! Although 'switch' is not a Lua keyword it's listed here, as it will make my 'scope' translation easier...
local nilkeywords = {"number","int","void","string","var", "function","global","table","implementation","impl","forward","bool","boolean"} -- A few words here are actually Lua keywords, BUT NIL handles them differently in a way, and that's why they are listed here!
local operators   = {":","==","~".."=",">=","<=","+","-","*","//","%","(",")","{","}","[","]",",","/","=","<",">",".."} -- Period is not included yet, as it's used for both decimal numbers, tables, and in the future (once that feature is implemented) classes.
local idtypes     = {"var",["variant"]="var",["int"]="number","number","string","function",["delegate"]="function","void",["bool"]="boolean","boolean"}
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
local print=print

-- A few functions I need to get NIL to work anyway!
local replace = string.gsub
local ASC=string.byte
local sprintf = string.format

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

local function ValidForIdentifier(str)
   -- Regular expressions may be faster, but I never mastered that, and this way I can be sure it works!
   local ret = true
   for i=1,#str do
       local c=mid(str:upper(),i,1)
       local b=ASC(c)
       if (i==1 and b>=48 and b<=57) then return false end -- Identifiers may NOT begin with a number!
       ret = ret and (b=="_" or (b>=65 and b<=90) or (b>=48 and b<=57))
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
   for i=1,#amystring do
      if o==0 then
        for _,op in ipairs(operators) do 
            if mid(amystring,i,#op)==op then o=#op mystring = mystring .. " "..op.." " break end
        end
      end
      if o>0 then o = o - 1 elseif mid(amystring,i,1)=="\t" or mid(amystring,i,1)=="\r" then mystring = mystring .. " " else mystring = mystring .. mid(amystring,i,1) end
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
      elseif c=="_" or (b>=65 and b<=90) or (b>=48 and b<=57) or (b>=97 and b<=122) or (c==".") then
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

-- Stuff that NIL will need to make classes possible.
NILClass = {}

function NILClass.DeclareClass(name,identifiers,extend)
   
end

function NILClass.NewFromClass(classname,consparam)

end


-- Translator itself
function mNIL.Translate(script,chunk)
    local ret = ""
    local lines = split(script,"\n")
    local lmacro = {}
    local amacro = {lmacro,macros}
    local scopes = {[0]={kind="Base Scope",line=0}}
    local scopelevel = function() return #scopes end
    local scopetype = function() return scopes[#scopes].kind end
    local scopestart = nil
    local allowrepeatend
    local forwards = {}
    local function newscope(kind,ln) scopes[#scopes+1] = { kind=kind, line=ln } end
    local function buildfunction(id,chopped,tpestart,track)        
          -- error(chopped[tpestart].word) --check
          assert(chopped[tpestart].word=="(","NI: Function builder has been pointed wrong! "..track)
          local params = {}
          local assertion
          local wantass = ""
          local i = tpestart+1
          local function addassert(a,wa)
             if wantass~="" then wantass=wantass..", " end wantass=wantass..wa 
             if not a then return end
             if assertion then
                assertion = assertion.." and "..a
             else
                assertion = a
             end
          end
          assert(chopped[i],"NT: Unfinished function definition in "..track)
          while (chopped[i].word~=")") do
             while (chopped[i] and chopped[i].word=="") do i=i+1 end
             assert(chopped[i] and chopped[i+1],"NT: Unfinished function definition in "..track)
             local w = chopped[i].word
             local nw = chopped[i+1].word
             if w.word=="var" then -- optional, but implemented in case people don't realize it! ^_^
                addassert(nil,"var")
                params[#params+1]=nw
                chopped[i+1].type="Function Parameter"
                i = i + 2 
             elseif w=="string" then
                params[#params+1]=nw
                addassert( "type("..nw..")=='string'","string")
                chopped[i+1].type="Function Parameter"
                i = i + 2
             elseif w=="int" or w=="number" then
                params[#params+1]=nw
                addassert( "type("..nw..")=='number'","number")
                chopped[i+1].type="Function Parameter"
                i = i + 2
             elseif w=="bool" or w=="boolean" then
                params[#params+1]=nw
                addassert("type("..nw..")=='boolean'","boolean")
                chopped[i+1].type="Function Parameter"
                i = i + 2
             elseif w=="table" then
                params[#params+1]=nw
                addassert( "(nw==nil or type(nw)=='table')","table")
                chopped[i+1].type="Function Parameter"
                i = i + 2
             elseif w=="userdata" then
                params[#params+1]=nw
                addassert( "("..nw.."==nil or type("..nw..")=='userdata')","userdata")
                chopped[i+1].type="Function Parameter"
                i = i + 2
             elseif w=="function" or w=="delegate" then
                params[#params+1]=nw
                addassert( "("..nw.."==nil or type("..nw..")=='function')","function")
                chopped[i+1].type="Function Parameter"
                i = i + 2
             elseif classes[w] then
                params[#params+1]=nw
                addassert( "("..nw.."==nil or (type("..nw..")=='table' and "..nw.."._NIL_class='"..w.."')",w)
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
              if tcontains(nilkeywords,p) or tcontains(luakeywords,p) then error("NT: Unexpected keyword '"..p.."' in "..track) end
              if tcontains(operators,p) then error("NT: Unexpected operator '"..p.."' in "..track) end
              assert(ValidForIdentifier(p),"NT: Invalid identifier name '"..p.."' in "..track) 
              if i>1 then ret = ret ..", " end
              ret = ret .. p
          end
          ret = ret ..")"
          return ret,params,assertion
    end
    local function StartFunctionScope(line,func,id)
          ret = ret .. "function "..(id or "")..func.head
          if func.assertion then ret = ret .." assert("..func.assertion..",'NR: Function did not receive the parameters the way it wanted!')" end
          newscope("function",line)
          local scope = scopes[#scopes]
          scope.func = func
          scope.idtype = func.idtype
          vars[#scopes]={}
          for _,p in ipairs(func.params) do
              --print(#scopes,p,_)
              vars[#scopes][p]={idtype='var'} -- A stricter setup can come later, but for now this will do!
          end
    end
    
    local function ClassScope(chopped,track)
            local tpestart=1
            local dostatic=false
            local doabstract=false
            local doreadonly=false
            local idtype
            local id
            local default = "nil"
            local wscope = #scopes
            local fields = {}
            if prefixed(chopped[1].word,"//") then ret = ret .. "-- comment line!" return end
            do local getout repeat
               getout=true
               if chopped[tpestart].word=="static" then 
                  dostatic=true getout=false tpestart = tpestart + 1 
               elseif chopped[tpestart].word=="abstract" then 
                  doabstract=true getout=false tpestart = tpestart + 1
               elseif chopped[tpestart].word=="readonly" then
                  doreadonly=true getout=false tpestart = tpestart + 1
               end
            until getout end
            assert( chopped[tpestart].type=="NILKeyword" , "NT: declaration syntax error in "..track )
            idtype=chopped[tpestart].word
            --assert(idtype~="class","NT: Classes have not yet been supported! "..track)
            if idtypes[idtype] then idtype=idtypes[idtype] end
            id = chopped[tpestart+1].word
            assert(id,"NT: Incomplete declaration")
            assert(chopped[tpestart+1].type=="Unknown","NT: Identifier name ("..chopped[tpestart+1].word..") seems known as a "..chopped[tpestart+1].type.." in "..track)
            assert(ValidForIdentifier(id),"NT: \""..id.."\" is not a valid identifier in "..track)
            do
               assert(
                  not(
                    fields[id]
                  ),"NT: Duplicate field identifier \""..id.."\" in "..track)
            end
            if #chopped>tpestart+2 and chopped[tpestart+2].word=="(" then   
               error("NT: Methods not supported yet!") --[[
               assert(
                    chopped[#chopped].word==")" or 
                    (
                       prefixed(chopped[#chopped].word,"") and 
                       chopped[#chopped-1].word==")"
               ),"NT: Incomplete function declaration")
               -- print(dbg('chopped',chopped))
               local fd,fp,fa = buildfunction(id,chopped,tpestart+2,track)
               functions[wscope][id] = { idtype=idtype, head=fd, params=fp, assertion=fa }
               -- print(dbg('functions',functions))
               if not doglobal then ret = ret .. "local " end
               if doforward then 
                  ret = ret .. id.." = function() error('NR: Call to a foward function ("..id..") which has not yet been implemented!') end"
                  forwards[id] = functions[id]
               else
                  StartFunctionScope(linenumber,functions[wscope][id],id)
               end
               ]]
            else
               local blst = { [true]="true", [false]="false"}
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
                    assert(pdefault.type=="string","NT: Constant string expected in "..track)
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
               elseif idtype=="var" then
                  if pdefault then
                     error("NT: Direct, declare and define is not yet supported for variants in "..track)
                  end
               else
                  error("NT: Type not yet supported in "..track)
               end
               ret = ret .. "\t['"..id.."'] = { idtype='".. idtype.."', name='"..id.."', default= "..(psdefault or default)..", static="..blst[dostatic]..", readonly="..blst[doreadonly].."},"
               fields[id]=true
               
            end
    end
    
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
       else
          ret = ret .. "WTF???\n" -- This should never be possible to happen!
       end
       return ret
    end
    vars.globals = vars.globals or {}
    functions.globals = functions.globals or {}
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
            if chopped[1].word=="#macro" or chopped[1].word=="#localmacro" then
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
            else
               error("Unexpected directive in "..track)
            end
         elseif scopes[#scopes].kind=="class" then
            local scope=scopes[#scopes]
            vars[scopelevel()] = vars[scopelevel()] or {}
            functions[scopelevel()] =  functions[scopelevel()] or {}
            if #chopped == 1 and chopped[1] and chopped[1].word=="end" then
               ret = ret .. "}"
               if scope.extends then ret = ret .. ","..scope.extends end
               ret = ret .. ")\n"
               vars[#scopes]=nil
               functions[#scopes]=nil
               scopes[#scopes]=nil
            else
               ClassScope(chopped,track)
            end   
         elseif chopped[1].type=="NILKeyword" then -- only for declarations!
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
            assert( chopped[tpestart].type=="NILKeyword" , "NT: declaration syntax error in "..track )
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
               local fd,fp,fa = buildfunction(id,chopped,tpestart+2,track)
               functions[wscope][id] = { idtype=idtype, head=fd, params=fp, assertion=fa }
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
                    assert(pdefault.type=="string","NT: Constant string expected in "..track)
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
               elseif idtype=="var" then
                  if pdefault then
                     error("NT: Direct, declare and define is not yet supported for variants in "..track)
                  end
               else
                  error("NT: Type not yet supported in "..track)
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
            for i,v in ipairs(chopped) do
              if v.word~="" then -- I don't understand how these come in, but they do!
                --[[
                if i==1 then
                  local tabs=#scopes
                  if v.word=="end" or v.word=="forever" or v.word=="until" then tabs=tabs-1 end
                  for j=1,tabs do ret = ret .. "\t" end
                end
                ]]
                -- For now the order doesn't matter. When NIL can get stricter in variable checks, the order will have to be reversed.
                vars[#scopes] = vars[#scopes] or {}
                functions[#scopes] = functions[#scopes] or {}
                local IsVar = vars.globals[v.word] or functions.globals[v.word]
                for i=0,scopelevel() do IsVar = IsVar or vars[i][v.word] or functions[i][v.word] end
                --[[
                if not(IsVar or v.type~="Unknown") then
                   print(dbg("chopped",chopped,0))
                   print(dbg("vars",vars,0),IsVar~=nil,v.type,v.word)
                end
                --]]
                assert(IsVar or v.type~="Unknown","NT: Unknown term \""..v.word.."\" in "..track)
                -- if (v.type=="Operator") then print(v.word.." > "..dbg("chopped",chopped)) end
                -- print(dbg('v',v),"\n"..dbg('scopes',scopes))
                if i~=1 then ret = ret .. " " end
                if prefixed(v.word,"//") then 
                   ret = ret .. "--"..Right(v.word,#v.word-2)
                elseif luakeywords[v.word] or v.type=="LuaKeyword" then
                   -- print ("KEYWORD "..v.word)
                   if scopestart==v.word then
                      ret = ret .. " "..v.word.." "
                      scopestart=nil                   
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
                   elseif v.word=="or" or v.word=="and" or v.word=="not" or v.word=="true" or v.word=="false" then
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
                      else
                         scopes[scopelevel()] = nil
                         ret = ret .. "end"
                      end
                      if scopes[scopelevel()].kind=="class" then ret = ret .. "," end
                   elseif v.word=="class" and i==1 and #scopes==0 then
                       assert(#chopped>=2,"NT: Class requires definition!")
                       newscope("class",linenumber)
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
                       classes[cscope.classname]=cscope.classname
                       ret = ret .. cscope.classname .. " = NILClass.DeclareClass('"..cscope.classname.."',{\n"
                       break
                   elseif v.word=="return" then
                        local retscope = #scopes
                        while(retscope>0) do
                           if scopes[retscope].kind=="function" then break end
                           -- print(dbg(sprintf("Scope #%d",retscope),scopes[retscope]))
                           retscope = retscope - 1
                        end
                        if retscope==0 then ret = ret .. "return" else
                          local scope=scopes[retscope]
                          local func = scope.func
                          if (not func) then print(dbg('scope',scope)) end
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
                          else
                                 error("Current setup cannot return in this kind of function yet! -- "..track)
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
         sprintf("NT: %s-scope in line #%d not properly ended, yet the end of the chunk has been reached in %s",scopes[#scopes].kind,scopes[#scopes].line,chunk or "The chunk without a name")
       )
    end
    return ret
end

function mNIL.Load(script,chunk)
    return load(mNIL.Translate(script,chunk))
end

mNIL.LoadString = mNIL.Load

function mNIL.LoadFile(file,chunk)
	local f = assert(io.open(file, "rb"),"NL: Reading file "..file.." failed")
	local content = f:read("*all")
	f:close()
	return mNIL.Load(content,chunk or file)
end

return mNIL
