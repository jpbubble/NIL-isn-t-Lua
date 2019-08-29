[![NIL Isn't Lua](https://raw.githubusercontent.com/jpbubble/NIL-isn-t-Lua/master/NIL.png)](https://jeroentricky.wixsite.com/nillanguage)

# NIL Isn't Lua (Yeah, that's what the acronym stands for).

Lua is a wonderful scripting language, but it does also have some downsides, which are not really a problem for small addons, and just quick game scripting (for which Lua was originally intended), but as Lua is becoming more and 
more sure for huge projects, and some of them really complex, some feature Lua lacks, and some setups in the language can become rather.... shall we say.. problematic.

To name few:
- Variables are not declared in Lua. Any variable never used before simply contains "nil". When you somewhere in you code mispel a variable in a complex piece of code, this will eventually haunt you. No problem is quick code, but 
in complex code, I don't like this. NIL does therefore require all variables to be declared before they can be used.
- NIL does therefore also have a bit of 'stronger' type setup, although this is not yet set up to be completely waterproof, The "var" type can still contain anything.
- Lua does not support constants at all, and you can either use variables, but since you can still change them, that's not a very elegant way to solve things, or just write it all out, but that can backfire if you need to change 
stuff. For NIL I will put in macros. Be careful with them, as macros will be processed before NIL translates the code to Lua and, and thus also before the Lua parser sees anything, and basically anything, even keywords can be 
turned into a macro. I am NOT planning (like C does) to allow macros to have parameters. 
- I miss the "switch" statement, so NIL will definitely support it!
- Now in Lua every variable is a global unless specifically declared as a local. For NIL I'm planning to work the other way around. First of all, locals are faster than globals in Lua, and second many programming teachers will 
teach you to avoid using globals as much as possible, so I guess it only makes sense to go for locals by default.
- Lua has no official support for classes. On this moment we can fool around with tables and maybe even metatables, but a real class environement is not yet there. NIL has its own class system and will integrate this with Lua. 

Originally NIL has been developed as part of the "BUBBLE" project, but I've decided to make this as little reliant on BUBBLE as possible, so other Lua engines (such as LOVE2D, or if you want World of Warcraft addons and so on) can 
also benefit here, so as much "pure Lua" to create NIL in as possible. This will mean that a few features BUBBLE will support won't be supported on other engines, I guess this cannot be avoided. The "#use" directive will be the 
most notable example of this. 

### Should NIL be able to interface with actual Lua code?

If you mean if you write:
~~~Lua
function Hello() 
	print("Hello world!")
end
~~~
In 100% Lua code and you load it in NIL if NIL can see the "Hello" function? Then it should be able to, but NIL is a bit dependent on the _G table for that, and it's not 100% reliable (Lua doesn't use _G itself, but _G does get 
updated every time a variabble/function is being defined). A few directives will be added to NIL to make sure NIL will pick this up.

### When I use NIL with LOVE2D, will it just respond to the callbacks?

First of all, the callbacks are done by Lua itself and not by the LOVE2D engine. In more clearer words, the callback manager for LOVE2D has been written in Lua. That alone should make it interfacable with NIL. But if you type 
something like this:
~~~
#accept love
love.draw = void()
    love.graphics.print("Hello World",10,10)
end
~~~
It should be translated to:
~~~Lua
love.draw = function()
    love.graphics.print("Hello World",10,10)
end                                         
~~~
And LOVE2D should pick that up just fine. Other engines woring with Lua will just work in similar ways.



### Is NIL slower than Lua?

Same answer as I'd always give when people ask me about languages transcompiling to C. "If you want to have the full power of C, use C." Transcompilers translating to Lua (which includes 'NIL') are not that much different. However 
I made the syntax of NIL as close to Lua as possible for the dead simple reasons that it would A) be easier to translate, and B) the translation would come as close to true Lua as possible. So in most cases, the code should be just 
as fast. 
Please note, Lua can't produce NIL code. NIL merely translates NIL code to Lua and Lua will have to compile the code generated itself like it were normal Lua code, meaning that when the code is being executed NIL does nothing at 
all. Loading the code will of course be a different story since NIL will need to translate it to Lua first, and then Lua needs to compile it, meaning a few extra stages the code comes through. Once the code is fully loaded NIL 
leaves everything to Lua, although based on your own scripting style NIL could put in some extra checks.... But you da boss on that one.

### Can NIL be easily tracebacked?

NIL has been designed to make sure that was is line #5 in NIL is also line #5 in Lua, which should not cause any traceback issues. It might be possible that in some situations the error messages may not fully make sense anymore, 
although my expectation is that these situations should be rare. 

### Is aside from some extra features the syntax exactly the same.

For moest part yes.
~~~Lua
for i=1,10 do print("Hi") end 
~~~
Should come out the same. NIL only checks if all used identifiers are known (please note that indexes, and key variables used in for commands are declared as locals by Lua for that specific for Scope, and NIL will do the same).

Typical NIL definitions, and commands require a line for themselves alone. 

