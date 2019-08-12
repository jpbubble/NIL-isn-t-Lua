NIL = require "NIL"

function gs(file)
	local f = assert(io.open(file, "rb"),"NL: Reading file "..file.." failed")
	local content = f:read("*all")
	f:close()
	return content
end

script_nil = gs("TestNIL_With.nil")

script_lua = NIL.Translate(script_nil,"TestNIL_With.nil - Test")

print("*** ORIGINAL ***\n\n"..script_nil.."\n\n\n*** TRANSLATION ***\n\n"..script_lua)

script_compiled = loadstring(script_lua)

script_compiled()

