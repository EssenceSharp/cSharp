# What Are [Traits](http://scg.unibe.ch/archive/papers/Scha03aTraits.pdf)?

[Traits](http://scg.unibe.ch/archive/papers/Scha03aTraits.pdf) are composable units of behavior that can be used by classes–or by other Traits–without regard to the class inheritance hierarchy. Traits are used for the same purpose as [multiple inheritance](http://en.wikipedia.org/wiki/Multiple_inheritance), but do not suffer from the [same flaws](http://scg.unibe.ch/archive/papers/Scha03aTraits.pdf).

## [Traits](http://scg.unibe.ch/archive/papers/Scha03aTraits.pdf)–The Conceptual Model

A trait is like a class in that it defines methods, but unlike a class in that it cannot have or create instances and cannot inherit from a superclass (nor from a ‘super trait.’) Also, the current implementations of Traits in  Essence# do not allow traits to define or use instance variables–but that may change: Traits that can [define and use instance variables](http://essencesharp.wordpress.com/Documents/Computer%20Science/Stateful%20Traits.pdf) very probably will be implemented in Essence# at some point in the future.

Traits in Essence# are also [namespaces](namespaces)–just like [classes](classes) are. And they can (and normally should) be defined in an Essence# [class library](class-library)–also just like classes. 

### Basic usage

 If you want a class (or a trait) to”import” the methods defined by a trait, then send the message _uses: aTrait_ to that class (or to that trait), with the source trait (from which the methods will be imported) as the message argument. The _uses: aTrait_ operation is [transitive](http://dictionary.reference.com/browse/transitive?s=t): Except as explained below, all methods defined by the source trait will be imported by the receiver, along will all methods imported by the source trait from any other traits it uses.

Methods defined locally by the importing class (or trait) will not be overridden by the import (uses:) operation. On the other hand, any methods inherited by the importing class from its superclass which have the same names as those imported will be overridden. In other words, methods imported from a trait have higher precedence than inherited methods, but have lower precedence than locally-defined methods.

It makes no difference whether the _uses: aTrait_ message is sent to a class before or after the class defines its own methods locally: The end result will always be the same. However, sending the message _uses: aTrait_ multiple times is not additive: Each such message send has the effect of completely undoing the effects of any previous sends to the same receiver. Of course, if the same value is used as the message argument, then the end result will be the same as having sent the message only once (with that same argument value.)

With one exception, methods imported from a trait should be indistinguishable from those implemented locally by the importing class or trait. The exception is that methods defined in a trait bind to non-local variables (e.g, class variables) based on the namespace environment of the trait that defines the methods, and not based on the namespace environment of the class or trait that uses (“imports”) those methods.

In contrast, messages sent to super in a method defined in a trait will act as though they were defined locally in the importing class–in other words, the semantics of a message sent to super depend on the class that imports methods that send such messages, and does not depend on the trait where methods that send messages to super are defined. The same is true of messages sent to self.

### Trait combinatorial algebra

Semantic issues arise whenever a class (or a trait) uses two or more traits–issues that are analogous to those encountered when using multiple inheritance. Traits solve those problems by means of algebraic trait-combination expressions which disambiguate the semantics of combining two or more traits together–doing so in a way that does not degrade the robustness or the flexibility of the code, unlike multiple inheritance.

The trait combinatorial algebra has three operators: {"+"}, – and @, which are implemented in Essence# by the binary messages #+, #- and #@.

The “+” operator constructs a new trait that is the symmetric set difference of the two operands (the term ‘symmetric set difference’ will be fully explained below, but it often means the same things as the union of two sets.)

The “-” operator constructs a new trait from which one or more methods have been removed (“excluded”) with respect to the trait that is the operand on the left-hand side of the operator.

The “@” operator constructs a new trait in which one or more of the methods have been renamed (technically, aliased) with respect to the trait that is the operand on the left-hand side of the operator.

**Fundamental concept:** Regardless of the complexity of a trait combination expression, and regardless of the number of traits combined or of the trait usage graph of any traits involved in a trait combination expression, the result is always a single (usually anonymous) trait that is dynamically created by the combinatorial expression, and which contains all the methods of all the traits involved in the combinatorial expression–including those methods directly or indirectly used by the traits in the expression, and excluding only those methods which must be excluded as a function of the semantics of the combinatorial expression. In other words, the result of a trait combinatorial expression is a flattened set of the all the methods defined directly or indirectly by any trait that occurs in the expression, although some of the methods may be renamed or excluded, as required by the laws of the algebra. That means that the user of a trait defined by a trait combinatorial expression always sees itself as using a single trait, with a single, unified set of methods. It does not see an “inheritance hierarchy” or “usage graph,” and so does not (and cannot) behave differently based on how the traits it is using may be structured. The only thing that matters is the end result of the combinatorial expression, not its form or structure.

**Combination operator:** Although the result of applying the fundamental combination operator “{"+"}” to two traits is often simply a new trait that contains all the methods defined and/or imported by both operands, that is not always the case. The reason is because the semantics of the “{"+"}” operator applied to two operands is symmetric set difference, and not set union. If, and only if, each operand only contains methods whose selectors are unique to it, and which the other operand does not contain, then the result of the operation is the same as it would be for set union. Of course, that will often be the case. But when it is not, the result of the “{"+"}” operation will be a new trait that contains all those methods contained by only one of the operands, but excludes any methods contained by both operands. _That’s the semantics of symmetric set difference._

Note, however, that the symmetric set difference is computed based on all the traits in a combinatorial expression, and not computed serially between each two operands. That means that the combination operator “+” is technically not really a ‘symmetric set difference’ operator. Its actual effect is to compute a new trait that does not contain any methods defined in more than one of the traits in the combinatorial expression.

The reason that the “+” operator has that effect is simply because a) that’s the only way to make the combinatorial expression algebra [commutative](http://en.wikipedia.org/wiki/Commutative_property)–and thus order independent, and b) it’s the only way to make the resolution of such conflicts a specific and explicit responsibility of the users of traits, and to do so in a way that has a transitive closure that never goes more than one level deep.

The purpose of the other two operators is to specify how to deal with the fact that the “+” combination operator excludes any and all conflicting methods (those defined by two or more traits in the same combinatorial expression.)

**Exclusion operator:** One way to handle such conflicts is to use just one of the conflicting methods and discard the other. That’s done using the exclusion operator (“-“), as in the following example, which combines the traits TReadStream and TWriteStream, but excludes the method #on: (defined by TReadStream) from the resulting trait combination (perhaps so that the #on: method defined by TWriteStream won’t be excluded, because if the #on: method defined by TReadStream were not excluded, it would conflict with the #on: method defined by TReadStream):

{{
ReadWriteStream uses: TReadStream – #on: + TWriteStream.
}}
Multiple methods can be excluded from the same trait by using an array of method selectors as the argument to the #- message, instead of using a stand-alone symbol:

{{
ReadWriteStream uses: TReadStream – #(on: #position) + TWriteStream.
}}
**Aliasing operator:** The other way to handle such conflicts is to rename one or more of the conflicting methods, as in the following example:

{{
ReadWriteStream uses: TReadStream @ {on: -> from:. #position -> #readPosition} + TWriteStream.
}}
The example aliases the #on: method defined by TReadStream to the name #from: and aliases the #position method defined by TReadStream to the name #readPosition–but only in the anonymous trait that is the result of the combinatorial expression. The aliasing has no effect on TReadStream, nor on any other users of TReadStream. The argument of the “@” message must be an array of associations, where the association key is the name of the method in the source trait, and the value of the association is the name of the method in the trait that results from the expression.

## Defining Traits In A Class Library

The Essence# [class library](class-library) format supports the definition of traits. Note that there is a strong convention that trait names should be prefixed by a capital T, although there is nothing that enforces that convention.

To define a trait in a class library, create a folder whose name is the name of the trait to be defined, and which is located in the namespace where the trait should be declared, and create a text file in that folder named “trait.def” and/or one named “classTrait.def.”  It is an error to also have a file in the same folder named either “class.def” or “metaclass.def.” To define the instance methods of the trait, include them in a file (in the same folder) named “methods.instance.” To define the class methods of the trait, include them in a file (also in the same folder) named “methods.class.” The file format is identical to that used to define the methods of a class or metaclass (as described in the documentation for [class libraries](class-libraries).)

**Note:** Traits as implemented in Essence# are strongly congruent–but not identical–to their implementation in Pharo. 
_
The essence of OOP: It's all messages, all the time._

