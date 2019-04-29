--[[

      NIL
 
      NIL Isn't Lua -- set up by Jeroen P. Broks

]]


local macros = {}
local vars = {}
local luakeywords = {}
local NIL = {}

function NIL.Load(script,chunk)
end

NIL.LoadString = NIL.Load

function NIL.LoadFile(file,chunk)
	local f = assert(io.open(file, "rb"))
	local content = f:read("*all")
	f:close()
	return NIL.Load(content,chunk or file)
end

