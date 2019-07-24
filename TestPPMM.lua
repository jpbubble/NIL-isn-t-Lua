
NIL = require "NIL"

script = [[

	#pure                  
	-- comment in PURE Lua 
	#endpure               

	int a = 100
	int b = 100
	a++
	b--
	print(a,b)

	++a
	--b
	print(a,b)

	//c++
]]

translation = NIL.Translate(script)
print(translation)

