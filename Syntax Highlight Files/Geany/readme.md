# Syntax Highlighter for Geany

This is a modified version of the Lua settings.

In order to get this to work you will have to put it into the proper 
folder Geany set for this. 

In Windows that would be:
~~~
C:\Program Files (x86)\Geany\data\filedefs
~~~

Then you'll need to add the next line to C:\Program Files (x86)\Geany\data\filetype_extensions.conf
~~~
NIL=*.nil
~~~

If I recall correctly these files/foldres can be found inside the application bundle of Geany.app
On Linux you need to take a look in ~/.config/geany to find the files and specific folders

# issue

NIL uses // in stead of -- for comments and --[[ for multi-line comments, which NIL doesn't even allow.
The syntax highlighter will still use the Lua settings for these even though NIL forbids these.
This is a Geany issue, as this file uses uses the Lua Lexer, and apparently the Lexer ignores the settings in this config file, but has these settings hard coded built in. Unfortunately, since I don't feel safe messing with other people's C++ code, there's nothing I can do about that (unless folks at Geany are gonna be helpful on that) :P
