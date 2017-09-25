# The Essence# Compiler
Currently, the Essence# compiler is implemented in C#. The plan is to eventually reimplement it in Essence# itself. 

However, thanks to Essence#'s comprehensive ability to use objects that are instances of .Net classes, it's quite possible to invoke the Essence# compiler from Essence#. Unfortunately, exposing the full breadth and depth of the functionality provided by the Essence# compilation subsystem would  require that Essence# classes would have to be defined that represent the lexical analyzer, parser, compiler, lexical token objects, parse tree nodes, abstract syntax tree nodes and [LINQ](http://msdn.microsoft.com/en-us/library/system.linq.expressions(v=vs.110).aspx) expression objects used in the Essence# compilation subsystem (as implemented in C#.) And that would include defining all the necessary _user primitives_ which would define the binding of Essence# messages to specific methods of the CLR types involved. And _that_ would be a lot of work. And no, that has not yet been done.

Of course, exposing the compiler's API is not an all-or-nothing proposition. But no work has yet been done to enable almost any use of the compiler from Essence# other than a few messages that can be sent to classes for the purpose of compiling methods to be added to the class' method dictionary. There is much other work with far higher priority that has yet to be done.

Then there's the fact that the current implementation of the compiler is not able to generate DLL or EXE files. Adding that capability to the compiler may require changing the API. And that's yet another reason to hold off on exposing the compiler for use by Essence# code.

Nevertheless, the only thing that prevents anyone who wants to do the work required to enable the compiler to be invoked from Essence# is the time it would take to read and understand the code of the compiler, and to get a working knowledge of how to create and use instances of CLR types using Essence#. 

Or you could write your own C# code to invoke the Essence# compiler directly. And then perhaps write some Essence# classes that wrap your C# code.

If you have specific questions, please [just ask](https://essencesharp.codeplex.com/discussions).

_
The essence of OOP: It's all messages, all the time._

