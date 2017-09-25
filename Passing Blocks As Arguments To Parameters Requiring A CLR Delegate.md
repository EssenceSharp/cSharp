# Passing Blocks As Arguments To Parameters Requiring A CLR Delegate
Blocks (Essence# anonymous functions which have full closure semantics) can be passed as parameters to the methods of any CLR type when the corresponding formal parameter of the method is a CLR delegate type (or of course, if the corresponding parameter type is System.Object.) But doing that so that it works correctly and without error requires that the CLR's static typing constraints must be satisfied. The Essence# dynamic binding subsystem takes care of part of that responsibility for you, but it cannot do everything. Some of the responsibility must remain with you, the programmer who would like to pass such arguments.  This article explains what you must do to make it work:

* The _arity_ (number of arguments or parameters) required by the block and that of the CLR delegate type of the corresponding parameter of the method of the CLR type which is being invoked must be the same.

* The _run time type_ of the value returned by the block must be compatible with the formal return type of the CLR delegate type. If that return type does not permit null values, then the block must not return the value **nil.** If the CLR delegate's formal return type is _void,_ then it does not matter what value the block may return.

In this context, "type compatible" means the following:


# The CLR delegate's formal return type  is assignable from the type of the value returned by the block, or

# There's an implicit or explicit type conversion operator defined from the type of the value returned by the block to the type of the CLR delegate's formal return type, or else

# The Essence# dynamic binding subsystem supports conversion from the type of the value returned by the block to the type of the CLR delegate's formal return type (which is explained in the section on [using CLR types](using-CLR-types).)

_
The essence of OOP: It's all messages, all the time._
