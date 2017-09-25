# Invoking CLR Methods That Have "Out" Or "Ref" Parameters
Conceptually, there are only two ways to pass parameters to functions: By value or by reference. Although there are (usually rather old or even ancient) programming languages that pass all parameters “by reference,” most programming languages pass parameters “by value” in the default case. And some languages, such as Essence#, pass all parameters “by value.”

The following example will be used to illustrate the difference in the semantics:

{{
var x = 3;
var y = f(x);
}}
If the variable _x_ is passed to the function **f** as a “by value” parameter, then it will be impossible for the function **f** to change the value of the variable _x._ However, if the variable _x_ is passed to the function **f** as a “by reference” parameter, then the function f will be able to change the value of the variable _x_ (although it may nevertheless refrain from doing so.)

Although there is more than one way to implement the two different ways of passing parameters, conceptually the difference is that, when arguments are passed “by value,” only the value of the argument is passed in to the function being called, whereas when arguments are passed “by reference,” the address of the argument (typically, the address of a variable) is passed in to the function being called. In the latter case, the called function may use that address to assign a new value to the variable–although it need not do so.

In fact, many languages implement “pass by value” semantics by physically passing in the address of the arguments that are passed “by value,” but nevertheless disallowing assignment to function parameters. As [Peter Deutsch](http://en.wikipedia.org/wiki/L_Peter_Deutsch) famously said, “you can cheat as long as you don’t get caught.” However, neither the CLR nor the DLR’s [LINQ](http://en.wikipedia.org/wiki/Linq) expression compiler cheat in that way: Unless a parameter is declared to require “pass by reference” semantics, arguments are passed by copying their values into the [activation frame](http://en.wikipedia.org/wiki/Call_stack) of the function being invoked.

So the problem for languages implemented on the CLR, such as Essence#, that have no syntax for specifying that parameters will be passed “by reference,” is that there’s no straightforward way to invoke the methods of CLR types that have any “pass by reference” parameters, where the intended usage is that the called method will, as part of its normal operation, assign a new value to an argument that is passed by reference.

And there’s an additional complication: Some methods that use “pass by reference” parameters will neither need nor use the initial value of the parameter, because they only set its value, and never “read” it. Conversely, others will use the initial value to compute and set a new value for the argument. And yet others may do one or the other with the same parameter, depending on the state of the receiver and the value of other parameters.

The C# language attempts to reduce the ambiguity regarding the usage model of “by reference” parameters by requiring that the method definer use different syntax to declare parameters, depending on whether the method will or will not use the initial value of a “by reference” parameter to compute and then set the argument’s new value. In the C# syntax, a parameter that is passed by reference must have [one of two keywords as a prefix](http://www.dotnetperls.com/parameter): **out** or **ref.**

The C# **out** keyword causes the compiler to require that the method that declares the parameter must first assign a value to the parameter before the parameter can be used–and before the method can return. That makes it impossible to use or access the initial value of the parameter. In contrast, the C# **ref** keyword causes the compiler to require that whoever invokes the method must first assign a value to the parameter.

But C# is not the only language used on the CLR, and the CLR itself does not enforce any constraints on the usage of “by reference” parameters. A .Net language’s compiler may do so, but such is not required by the CLR. That means there may exist .Net types that have methods that use “pass by reference” parameters that do not abide by the rules of C#.

The reflection metadata provided by the CLR does reveal, for each method parameter, whether it is a “pass by value” or a “pass by reference” parameter.  It may also reveal whether the intended usage of a “pass by reference” parameter is as a C#-style "out" parameter. But compilers are not required to provide that particular piece of metadata. The parameter may be used as an out parameter even if the reflection metadata makes no such claim. The only information provided by the reflection metadata that must be accurate is whether or not the parameter requires that its corresponding argument be passed “by value” or “by reference.”

So for now, Essence# deals with .Net method parameters that require that arguments must be passed by reference in one of two ways:

If the actual argument at runtime is anything other than a block or delegate, the dynamic binding subsystem will create and initialize a new variable that will be passed as the argument. Unless the reflection metadata claims that the parameter is an “out” parameter, the variable will be initialized to have the same value as the argument specified by the invoking code. The method will then be invoked with that new variable as the argument corresponding to the “by reference” parameter. When the method completes, the value of the created variable that was passed in as the actual argument will be assigned to the expression that represents the originally-specified argument. That assignment may cause an exception, and it may fail to update the original variable. I’m researching improvements, perhaps using the DLR’s Meta Object Protocol.

However, if the actual argument at runtime is a block or delegate, then the dynamic binding subsystem will create and initialize a new variable that will be passed as the argument (just as it does for the other case.) The method will then be invoked with that new variable as the argument corresponding to the “by reference” parameter (still essentially the same procedure as in the other case.) But when the method completes, the procedure is different: The one-argument block or delegate that was the originally-specified argument will be invoked, with the newly-created variable as its argument. That permits the caller to access the value of the “out” parameter, because its value will be the value of the block (or delegate) argument. Note, however, that the block or delegate must have an _arity_ of one, and if it’s a delegate, its parameter type must be assignable from the parameter type of the invoked method’s parameter.

## Example Usage
Let’s assume that we want to invoke the method Get as defined in the C# class named ValueModel shown below:

{{
class ValueModel {
        private static readonly Object notAValue = new Object();

        protected Object value = notAValue;

        public bool HasValue {
                get {return value != notAValue;}
        }

        public void Set(Object newValue) {
                value = newValue;
        }

        public bool Get(out Object value) {
                if (HasValue) {
                        value = this.value;
                        return true;
                }
                return false;
        }
}
}}
If we have an instance of the above ValueModel class in a variable named model (in an Essence# block or method,) we can invoke the Get method as follows:

{{
[:model |
        | value |
        (model get: [:v ](-value-_=-v))
                ifTrue: [value doSomethingUseful](value-doSomethingUseful)
]}}

Or more simply, we could just do:

{{
[:model ](-model-get_-[_value-_-value-doSomethingUseful)]
}}

**Important:** If the .Net method being invoked returns a boolean value, then the block will only be invoked if the .Net method returns true. But if the .Net method has a return type of void, or has a return type other than Boolean, then the block will always be invoked.

_
The essence of OOP: It's all messages, all the time._