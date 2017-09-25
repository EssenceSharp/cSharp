# The Essence# Standard Library

The Essence# Standard Library is in most respects the same as other Essence# [class libraries](class-libraries)(class-libraries). The differences are that a) it's loaded by default; all other [class libraries](class-libraries)(class-libraries) are only loaded on demand, and b) it declares classes and methods that are foundational to the language, and which in some cases are in fact "declared" by the Essence# run time system (so for those namespaces, classes and methods, the source code in the Standard Library that formally declares them is in fact redundant.)

All versions of the source code for the Standard Library are available from the Essence# Git repository on [GitHub](https://github.com/EssenceSharp/Standard.lib)--including versions not yet published here.

The version of the Essence# Standard Library that was current when a new version of Essence# was released is installed by the {"Essence#_<VersionName>_Setup.exe"} software installer. The default location for that is %UserProfile%\My Documents\Developer\EssenceSharp\Source\Libraries\Standard.lib. However, the installer permits installation of the Essence# project at the location of your choice, and sets the environment variable %EssenceSharpPath% to whatever location you choose when you install Essence#.  

By default, shared class libraries (including the Standard Library) are located in the folder %EssenceSharpPath%\Source\Libraries, although that is not required. Any class library--including the Standard Library--can be located anywhere that is listed as a library search path by the set of [configuration profiles](configuration-profiles)(configuration-profiles) that are currently active (see the section on [configuration profiles](configuration-profiles)(configuration-profiles) for details.)

To understand what you will see when you browse the Standard Library, you will need to understand the format of an Essence# class library, which is explained in the section on [class libraries](class-libraries). But here's a synopsis:

* Every class, trait and namespace has its own folder (file directory.) 
* Containment of one folder by another indicates that the entity defined by the containing folder serves as the namespace of the entity defined by folder it contains (it does not indicate an inheritance relationship.) 
* Folders that define [classes](classes) will contain a file named _class.def_ and/or a file named _metaclass.def._ 
* Folders that define [traits](traits) will contain a file named _trait.def_ and/or a file named _classTrait.def._  
* Instance methods reside in files named _methods.instance._
* Class methods reside in files named _methods.class._
* Folders that contain a file named _namespace.def_ (or that don't contain a file named any of _class.def,_ _metaclass.def,_ _trait.def,_ _classTraid.def,_ _methods.instance_ or _methods.class)_ define [namespaces](namespaces). 
* Initializers ("do its") reside in files named _initializer._  
* Shared pools are implemented as [namespaces](namespaces).
* Class variables and namespace (shared pool) variables are declared by files with the extension ".variable". The name of the variable being defined is the filename without the extension ".variable".
* Class constants and namespace (shared pool) constants are declared by files with the extension ".constant".  The name of the constant being defined is the filename without the extension ".constant".
* The format of all the ".def" files is that of a [self expression](self-expression)--which is essentially a (possibly cascaded) message send to _self,_ except that the pseudo-variable "self" is omitted. In the case of a _class.def_ file, the value of the _invisible self_ that receives the message(s) is the class being defined by the folder. In the case of a _metaclass.def_ file, the value of the _invisible self_ that receives the message(s) is the metaclass being defined by the folder. In the case of a _trait.def_ file, the value of the _invisible self_ that receives the message(s) is the _instance trait_ being defined by the folder. In the case of a _classTrait.def_ file, the value of the _invisible self_ that receives the message(s) is the _class trait_ being defined by the folder.  In the case of a _namespace.def_ file, the value of the _invisible self_ that receives the message(s) is the _namespace_ being defined by the folder.
* The format of all _methods.instance_ and _methods.class_ files is also that of a [self expression](self-expression), which is essentially a (possibly cascaded) message send to _self,_ except that the pseudo-variable "self" is omitted. In the case of a _methods.instance_ file, the value of the _invisible self_ that receives the message(s) is the class or _instance trait_ being defined by the folder. In the case of a _methods.class_ file, the value of the _invisible self_ that receives the message(s) is the metaclass or _class trait_ being defined by the folder.
* The methods declared in "methods.instance" and in "methods.class" files are expressed as [method literals](method-literals).

_Currently,_ many of the methods of the classes in the Standard Library won't be found in the "methods.instance" or "methods.class" files of the classes in which the reside. There are three separate reasons for that:

**System primitives:** _System primitives_ are declared/defined a priori by the Essence# run time system, and so there is no need to specify them explicitly in the source code (although that can be done--and will be, eventually.) Such methods _are_ present in the method dictionaries of their respective classes, and can be accessed by sending messages such as #compiledMethodAt: and #allSelectorsAndMethodsDo: to the class where they reside, or to one which inherits them. More usefully, for each class that has _system primitives,_ the source code that explicitly declares the _system primitives_ for that class can be found in a file named "system.primitives." There is a "system.primitives" file in the class folder for each class that defines _system primitives._ If present, the [library loader](library-loader) ignores any file named "system.primitives." You can regenerate that file for all classes in the system by [running the script](https://essencesharp.codeplex.com/wikipage?title=Running%20A%20Script&referringTitle=Documentation) GeneratePrimitiveMethodSource.

**Virtual methods:** Many methods of CLR types--but by no means all of them--can be invoked as _virtual methods_ even though there is no method declaration for them in the Essence# code. In addition to that, the non-private fields and properties of CLR types can also be accessed without any need to declare them as Essence# methods. So if you browse the Essence# code in the Standard Library of Essence# classes that represent CLR types, you probably won't see an Essence# method that corresponds to every method and property of the corresponding .Net type. They are there nonetheless, and you can call them in spite of the fact that you don’t see the method definitions in the Essence# code. 

Generally, a method will only be defined in the Essence# code of an Essence# class that represents a CLR type when the corresponding .Net method requires 2 or more parameters, when the CLR type has constructors that require arguments, when the author of the Essence# class wanted to invoke a method of a CLR type using a different name than the one declared by the CLR type, or wanted to access a field or property  of a CLR type using a different name than the one declared by the CLR type. 

**Inlined methods:** In the cases of classes that are native Essence# classes--i.e, those which don't “wrap” a .Net type--almost all methods which aren't _system primitives,_ or which are not inlined by the compiler, or which are not added dynamically by sending a message such as #addMethod: to the class, must be explicitly defined. But there are  a very small number of exceptions: Messages which are inlined either by the dynamic binding subsystem or else by the compiler. Messages inlined by the dynamic binding subsystem include #doesNotUnderstand: (but only if it's not explicitly defined as a method,) #ensure:, #ifCurtailed:, #on:do:, #onEvent:do: and #onEvent:doNotDo:. And the compiler also inlines messages such as #==, #~~, #and:, #or:, #ifTrue: and #ifFalse:, as well as other messages whose semantics can and should be fixed by the system.

You can print out a list of all the methods defined by an Essence# class (including those that are imported from traits, or are inherited from superclasses) by using the [ES command line program](https://essencesharp.codeplex.com/wikipage?title=Running%20A%20Script&referringTitle=Documentation) to run code on the command line (e.g, using the Windows CommandPrompt) such as the following, which will print out the names of all the methods of class Array:

{{
Array allSelectorsDo: [:selector ](-Transcript-show_-selector;-cr)
}}

The remainder of this document is currently under construction; sorry about that.

## List of classes
* [ArithmeticValue](ArithmeticValue)
* [Array](Array)
* [ArrayedCollection](ArrayedCollection)
* [Association](Association)
* [Behavior](Behavior)
* [BindingReference](BindingReference)
* [Block](Block)
* [Boolean](Boolean)
* [ByteArray](ByteArray)
* [Character](Character)
* [Class](Class)
* [Collection](Collection)
* [CompiledCode](CompiledCode)
* [Dictionary](Dictionary)
* [Double](Double)
* [DoubleArray](DoubleArray)
* [False](False)
* [Float](Float)
* [FloatArray](FloatArray)
* [HalfWordArray](HalfWordArray)
* [IdentityDictionary](IdentityDictionary)
* [Integer](Integer)
* [InvariantPrecisionReal](InvariantPrecisionReal)
* [KeyedCollection](KeyedCollection)
* [LongWordArray](LongWordArray)
* [Magnitude](Magnitude)
* [Message](Message)
* [Metaclass](Metaclass)
* [Method](Method)
* [Namespace](Namespace)
* [Number](Number)
* [Object](Object)
* [Pathname](Pathname)
* [PrimitiveValue](PrimitiveValue) 
* [Quad](Quad)
* [QuadArray](QuadArray)
* [Rational](Rational)
* [SequenceableCollection](SequenceableCollection)
* [SmallInteger](SmallInteger)
* [String](String)
* [Symbol](Symbol)
* [True](True)
* [UndefinedObject](UndefinedObject)
* [WordArray](WordArray)

_
The essence of OOP: It's all messages, all the time._

