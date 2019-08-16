NIL = require "NIL"
scriptfile = "TestNIL_QuickMeta.nil"
function gs(file)
	local f = assert(io.open(file, "rb"),"NL: Reading file "..file.." failed")
	local content = f:read("*all")
	f:close()
	return content
end
script=gs(scriptfile)
t = NIL.Translate(script)
print(t)
compiled = assert((loadstring or load)(t,"TRANSLATION"))
compiled()
