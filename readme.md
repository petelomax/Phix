Phix is a self-hosted hybrid interpreter/compiler, developed by Pete Lomax. It is very easy to use, and similar to Euphoria.

Official site: http://phix.x10.mx/

A single 16MB download contains a pre-compiled executable, all the sources, and everything needed to recompile them, in about 15 seconds. 
The download also contains a full-featured programmer's editor and 130+ demo programs.

Perhaps the most striking feature of Phix is that it has just five builtin data types:

        <-------- object --------->
        |                |
        +-atom           +-sequence
          |                |
          +-integer        +-string

Despite such apparent simplicity, or perhaps precisely because of it, Phix programs are pretty fast - not quite achieving 
the runtime performance of C or assembly, but making up for it with a very fast edit/run cycle and proper human-readable 
messages should anything go wrong (even in shipped pre-compiled executables). Sequences are the real powerhouse of Phix. 
The one type covers lists, queues, tables, trees, and arrays, with strings being the subset that is array of character. 
They can grow and shrink automatically without any memory management overhead. 
For example if s="food" then s[2..3]="e" makes s "fed", and then s[2..1]="east" makes s "feasted".

Phix applies the principle of least surprise, for instance in some languages myproc(list) or res = myfunc(list) can mangle list, 
whereas in Phix if you actually want that to happen you would code list = myproc(list) (and myproc would need to become a function) 
or {res,list} = myfunc(list). 
Likewise 1/2 is 0.5 (not 0, unless you explicitly ask for the floor()) and 0-1 is -1 (not +MAXINT). 
A core tenet is that for any line of code there is one and only one possible interpretation of it, and said meaning is utterly intuitive.

The principle goal of Phix is to make debugging easier, a whole subject area that does not seem to get the attention it deserves. 
Not entirely sure I am succeeding, yet.

# Donate
Feel free to donate:

| METHOD        | ADDRESS                                       |
|--------       |--------------------------------------------   |
| Paypal        | paypal.me/petelomax                           |
| Bitcoin       | 1MqYnKBhvtCnknSi8GK3bnX3n5Yo4WWeDd            |

