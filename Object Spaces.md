# Object Spaces
An _object space_ is an object that encapsulates the _execution context_ of an Essence# program. It is also responsible for initializing and hosting the Essence# run time system, including the _dynamic binding subsystem_ that animates/reifies the _meta-object protocol_ of Microsoft's [Dynamic Language Runtime](https://dlr.codeplex.com/) (DLR.)

Any number of different _object spaces_ may be active at the same time. Each one creates and encapsulates its own, independent _execution context._ The [compiler](compiler) and the [library loader](library-loader) operate on and in a specific _object space._ _Blocks_ and _methods_ execute in the context of a specific _object space._ Essence# classes, traits and namespaces are _bound_ to a specific _object space._ Even when a class, trait or namespace is defined in the same class library and the same containing namespace, they are independent and separate from any that might have the same qualified names that are bound to a different _object space._ 

In spite of that, it is quite possible for an object bound to one _object space_ to send messages to an object bound to a different _object space._ One way to do that would be to use the DLR's _hosting_ protocol. That's because an Essence# object space is the Essence#-specific object that actually implements the bulk of the behavior required by a DLR _language context,_ which is an architectural object of the DLR's hosting protocol.  

The C# class EssenceSharp.ClientServices.ESLanguageContext subclasses the DLR class Microsoft.Scripting.Runtime.LanguageContext, and thereby is enabled to interoperate with the DLR's hosting protocol. But an instance of EssenceSharp.ClientServices.ESLanguageContext's only real job is to serve as a facade over instances of the C# class EssenceSharp.Runtime.ESObjectSpace. And EssenceSharp.Runtime.ESObjectSpace is the class that reifies an Essence# _object space._ 

So, if you are only interested in using Essence#, and have no interest in using other dynamic languages hosted on the DLR, there is no need to use a DLR _language context_ in order to invoke the Essence# compiler and run time system from your own C#, F# or Visual Basic code.  You can use instances of EssenceSharp.Runtime.ESObjectSpace directly. The only disadvantage of that would be that using other DLR-hosted languages would then require a completely different API (e.g, using an IronPython library from Essence# code requires using the DLR hosting protocol, and hence requires using a DLR _language context)._

The advantages of using instances of EssenceSharp.Runtime.ESObjectSpace directly would be a much richer API that is far more specific to Essence#.

You can get the _object space_ for the current execution environment by sending the message #objectSpace to any Essence# class (even to those that represent CLR types.) And the Essence# [Standard Library](Standard-Library) includes a definition for an Essence# class that represents the Essence#-specific behavior of instances of the C# class EssenceSharp.Runtime.ESObjectSpace. It's in the namespace CLR.EssenceSharp.Runtime, and so can be found at %EssenceSharpPath%\Source\Libraries\Standard.lib\CLR\EssenceSharp\Runtime\ObjectSpace.

There are many ways that Essence# _object spaces_ might be useful. One example would be to use one _object space_ to host programming tools such as as browsers, inspectors and debuggers, but to have the applications on which those tools operate be in their own _objects spaces._ That _architecture_ would isolate the programming tools from any misbehavior of the applications on which they operate--and vice versa.

To get additional insight into the concept of _object spaces_ and how they might be used to good effect, the paper [Virtual Smalltalk Images: Model and Applications](http://rmod.lille.inria.fr/archives/papers/Poli13a-IWST13-ObjectSpacesVirtualization.pdf) is highly recommended.
_
The essence of OOP: It's all messages, all the time._


