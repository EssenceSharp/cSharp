# Self expression syntax
A _self expression_ is essentially a (possibly cascaded) _message_ sent to _self,_ except that the pseudo-variable "self" is omitted. The value of the "invisible self" that receives the message is established by configuring the compiler to set the pseudo-variable _self_ to the desired value during execution of the _self expression._

A _self expression_ is allowed just **one** _message chain_ having just **one** message receiver--and the receiver must not be specified syntactically. 

Conceptually, the syntactically unspecified--and therefore implied--receiver of the message(s) in the _message chain_ of a _self expression_ is the pseudo-variable _self._ That's because the same compiler infrastructure used to set the pseudo-variable _self_ to the correct value during the execution of a method or block is also used to set the value of the implied, unspecified receiver of the message(s) in the _message chain_ of a _self expression,_ and also because a _self expression_ is parsed by the parser simply by adding an actual _parse node_ for the pseudo-variable _self_ as the receiver of the message(s) in the _message chain_ of the _self expression._ That's possible because the parser implements _self expressions_ as their own "root parse node" or "grammatical start symbol."

Any syntactically-valid _expression_ which sends a message to an _operand_ can be converted into a _self expression_ simply be removing whatever syntactical construct is the receiver of the message (a.k.a, the leftmost _operand_  in an _expression.)_ The following examples show two pairs of [expressions](expressions), where the first member of the pair uses _self expression syntax_ and the second one does not:

## Configuring a class
{{"Using 'self expression' syntax:"

        superclass: Object; 
        instanceVariableNames: #(red green blue)
}}

{{"Using 'expression' syntax:"

        Color 
                superclass: Object; 
                instanceVariableNames: #(red green blue)
}}

## Adding methods to a class (using [method literals](method-literals) to define the method):
{{"Using 'self expression' syntax:"

        protocol: #accessing 
        method:
                [## red
                        ^red
                ]
}}

{{"Using 'expression' syntax:"

        aClass
                protocol: #accessing 
                method:
                        [## red
                                ^red
                        ]
}}

# Prior Art

The inspiration for _self expressions_ comes from [Tirade](http://goran.krampe.se/2009/03/16/tirade-a-file-format-for-smalltalkers/),  a data representation language invented by [Göran Krampe](https://plus.google.com/100518214657485254574/posts). The main reason that Essence# uses _self expressions_ instead of Tirade is simply because, once you have a full parser/compiler, it is significantly simpler to implement _self expressions_ than to implement Tirade. Given a full compiler, implementing _self expressions_ only involves adding a few, relatively small methods to the parser and compiler. And it also _technically_ doesn't require adding _new syntax_ to the language, since _self expressions_ only use syntactical forms that would have to exist in any case; the only innovation is to permit _simpler_ syntax that _omits_ an otherwise-required syntactical construct. So _self expressions_ were  "the simplest thing that could possibly work."

That said, [Tirade](http://goran.krampe.se/2009/03/16/tirade-a-file-format-for-smalltalkers/) would be a much superior solution to the problem of _programming-language neutral_ data interchange. But that's not the problem that _self expressions_ are intended to solve.

_
The essence of OOP: It's all messages, all the time._

