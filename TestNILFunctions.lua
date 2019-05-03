--[[

    test functions
    Since functions are BY FAR more complex than
    the stuff I've set up so far, I decided to put in the own department for functions.
    
    This is also because I want NIL to support something Lua does not support. 
    Class types. Having said that I guess it's only fitting I want to do this right
    from the start.and
    
]]



local NILCode = [[
    
    void Hello()
        print("Hello World")
    end
    
    void Hi(a)
        print("Hello "..(a or "The nameless one"))
    end
    
    void Yo(string a)
        print("Yo! Hey what's happenin', dude!")
        print(a)
    end
    
    int sum(int a, int b)
        return a+b
    end
    
    
    Hello()
    Hi("Jeroen")
    Yo("Yo")
    int a
    a = sum(4,5)
    print(a)
    
]]


local NIL=require "NIL"
local NILTrans = NIL.Translate(NILCode,"NILCode")

print("Original:\n"..NILCode,"\n\nTranslation:\n"..NILTrans)