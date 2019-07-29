script = [[var hoi

group hallo
	
	link stommekoe = hoi
	link p = print
	
end

hoi = 5
print(hallo.stommekoe)

hallo.stommekoe = 255

print(hoi)

]]
ls = loadstring or load

NIL = require "NIL"
hahaha = NIL.Translate(script)
print("-- Script Translation --")
print(hahaha)
print("\n\n-- Script execution --");
exe = assert(ls(hahaha))
print(exe)
exe("huh?")
hallo.p("Hello World")
