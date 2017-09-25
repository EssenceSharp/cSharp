# Auxilliary Tools For Developing Essence# Code
It is useful to distinguish three different types of coding/development tools that can be used in connection with developing Essence# code:

* Programs that are an integral part of the Essence# ecosystem and environment, such as the Essence# [compiler](compiler)(compiler) and the Essence# [library loader](library-loader). Currently, all such tools are implemented in C#, although the ultimate vision is that all such tools other than the run-time system will be implemented in Essence# itself. Of course, that will require a [compiler](compiler)(compiler) that can compile to DLLs and EXEs, in order to overcome the bootstrapping problem.

* Programs that are written in Essence#, and which depend upon Essence#'s metaprogramming and reflective capabilities to perform their intended functions. Currently, all such programs are implemented as Essence# scripts.

* Programs written in other languages that can either consume and/or produce Essence# code.

This article concerns itself only with the latter two categories. The [compiler](compiler) and the [library loader](library-loader) are covered elsewhere.

## Reflective Tools
Just because there is as yet no Essence# GUI library, and therefore no native Essence# code browsing tools, doesn't mean that the intrinsic reflecting capabilities of Essence# can't be used. In fact, scripts are provided in the shared scripts folder that provide at least some of the functionality traditionally provided by code browsers:

**ShowAllMethods:** The _ShowAllMethods.es_ script can be used to print out the names and _declaring class or trait_ of all the methods of a class or trait. The subject class or trait must be passed in as an argument, as in the following example which will print out the names and declaring class or trait of all the methods of class Array to the Transcript:

{{
es ShowAllMethods -a Array | more}}

**ShowAllMessagesSent:** The _ShowAllMessagesSent.es_ script can be used to print out all the messages sent by each method of a class or trait. The output is cross-referenced by the sending methods, and each such method specifies the class or trait that declares it. The subject class or trait must be passed in as an argument, as in the following example which will print out the names of all the messages sent by each method of class Array to the Transcript:

{{
es ShowAllMessagesSent -a Array | more
}}

**ShowAllSenders:** The _ShowAllSenders.es_ script can be used to print out the names and _declaring class or trait_ of all the methods in the [object space](https://essencesharp.codeplex.com/wikipage?title=Object%20Spaces&referringTitle=Documentation) that send a specified message. The subject message selector must be passed in as an argument, as in the following example which will print out to the Transcript the names and declaring class or trait of all the methods in the [object space](https://essencesharp.codeplex.com/wikipage?title=Object%20Spaces&referringTitle=Documentation) that send the message _do:_

{{
es ShowAllSenders -a #do: | more
}}

**ShowAllSendersInHierarchy:** The _ShowAllSendersInHierarchy.es_ script can be used to print out the names and _declaring class or trait_ of all the methods of a specified class that send a specified message. The subject message selector and the subject class or trait must both be passed in as  arguments, as in the following example which will print out to the Transcript the names and declaring class or trait of all the methods of OrderedCollection that send the message _do:_

{{
es ShowAllSendersInHierarchy -a #do: -a OrderedCollection | more
}}

**ShowUnimplementedMessages:** The _ShowUnimplementedMessages.es_ script can be used to print out the names of all the messages sent by the methods of a specified class or trait to the pseudo-variable _self_ for which the specified class or trait has no implementing methods. The subject class or trait must be passed in as an argument, as in the following example which will print out to the Transcript the names of any messages sent to _self_ by the class OrderedCollection that send messages for which OrderedCollection has no implementing methods:

{{
es ShowUnimplementedMessages -a OrderedCollection | more
}}
**Note:** Classes that represent CLR types typically have _virtual Essence# methods_ that don't need to be formally declared, because the Essence# dynamic binding system will automatically bind to and invoke the methods of a CLR type, provided those methods have less than two parameters. Messages sent in order to invoke such methods of CLR types will unavoidably show up as "unimplemented messages" when using the _ShowUnimplementedMessages.es_ script.

**ShowTraitUsageConflicts:** The _ShowTraitUsageConflicts.es_ script can be used to print out the name and declaring trait of all methods which were excluded from a _trait usage expression_ due to the fact that methods with the same selectors were declared by two or more of the [traits](traits)(traits) combined in a _trait usage expression._ The subject class or trait must be passed in as an argument, as in the following example which will print out to the Transcript methods excluded from the _trait usage_ of ReadStream because two or more of the [traits](traits)(traits) used by ReadStream had the same method selector:

{{
es ShowTraitUsageConflicts -a ReadStream | more
}}
## Foreign Tools
### Text Editor
The [Notepad++ text editor](http://notepad-plus-plus.org/) is highly recommended for reading/writing Essence# code on Windows.

### Browsing & Debugging
Essence# currently provides no support for any sort of GUI. Nor does it provide any code editing or code browsing tools. You might think that that means that the only way to develop code in Essence# is to use generic text editors, but such is not the case.

You could develop–and debug–your code using, for example, Squeak, Pharo or VisualWorks. That can be done because, unless your development environment’s Smalltalk compiler accepts (or worse, requires) non-standard syntax, the Essence# compiler can and will compile it. It will also accept/compile many of the common non-standard syntax extensions, such as qualified (“dotted”) names (VisualWorks, Smalltalk-X,) dynamic array literals (Squeak, Pharo) or even dynamic dictionary literals (Amber.)

So the real issue would be the differences in the standard libraries supported by other Smalltalk implementations. But that can be addressed by adding Essence# compatibility classes and methods to the environment where you develop your code, and by adding foreign-environment compatibility classes and methods to your suite of Essence# libraries/namespaces when you port your code to Essence#.

However, there is one obstacle in the path of this strategy that might immediately occur to you: The file-out formats of other Smalltalk development environments are not at all the same as the Essence# format for [class libraries](class-libraries). Fortunately, there is a solution to that problem:

We provide code that will file out a class or class category from VisualWorks, Pharo or Squeak into Essence# class library format. The code can be obtained from the [Essence# Tools Repository](https://github.com/EssenceSharp/Tools) on [GitHub](https://github.com/EssenceSharp). Although at present only those three development platforms are supported, it should not be too hard to figure out how to port one of them to another Smalltalk development platform. If you do so, please contribute it to the community.

The name of the class that implements exporting of code to Essence# class library format for each of the three development environments is _EssenceClassLibraryExporter._ The version for each of the three environments (Sqeak, VisualWorks & Pharo) has the same external API–which will be explained below. 

The Squeak version is located [here](https://github.com/EssenceSharp/Tools/tree/master/Squeak), the Pharo version is located [here](https://github.com/EssenceSharp/Tools/tree/master/Pharo), and the VisualWorks version is located [here](https://github.com/EssenceSharp/Tools/tree/master/VisualWorks). 

**Warning:** Don’t export code that you do not own, or that is not open source, unless you have the appropriate permission(s.) The VisualWorks class libraries published by Cincom are not open source, although the “Contributed” code may be (don’t assume; verify.)

After you’ve filed in EssenceClassLibraryExporter.st into Pharo, VisualWorks or Squeak, you use it by writing code in a workspace and executing it as a “do It.” Here’s what you need to know in order to "do it" correctly:

#### Select the target Essence# namespace
When exporting from VisualWorks, you probably should use the same [namespace](https://essencesharp.codeplex.com/wikipage?title=Namespaces&referringTitle=Documentation) name in Essence# that you use in VisualWorks. But when exporting from a Smalltalk development environment that doesn’t have (or doesn’t typically use) [namespaces](namespaces), you’re faced with an architectural decision that you didn’t have to make until such time as you needed to export your code to Essence#. You can, of course, simply export your code to the Smalltalk [namespace](https://essencesharp.codeplex.com/wikipage?title=Namespaces&referringTitle=Documentation). But best practice would be to define and use a [namespace](https://essencesharp.codeplex.com/wikipage?title=Namespaces&referringTitle=Documentation) in Essence# that’s specific to your project (unless you are deliberately extending a [namespace](https://essencesharp.codeplex.com/wikipage?title=Namespaces&referringTitle=Documentation) authored by someone else.)

#### Select the target Essence# [class library](https://essencesharp.codeplex.com/wikipage?title=Class%20Libraries&referringTitle=Documentation)
Conceptually, an Essence# [class library](https://essencesharp.codeplex.com/wikipage?title=Class%20Libraries&referringTitle=Documentation) is an independently-loadable code module. It roughly corresponds to what other languages/platforms call a “package,” “module,” “parcel” or “assembly.” 

It’s important to note that an Essence# [class library](https://essencesharp.codeplex.com/wikipage?title=Class%20Libraries&referringTitle=Documentation) can contain one or more Essence# [namespaces](namespaces)(namespaces)(namespaces)(namespaces), that the same Essence# [namespace](https://essencesharp.codeplex.com/wikipage?title=Namespaces&referringTitle=Documentation) can occur (be defined and/or modified by) more than one Essence# [class library](https://essencesharp.codeplex.com/wikipage?title=Class%20Libraries&referringTitle=Documentation), that [namespaces](namespaces)(namespaces)(namespaces)(namespaces) are an essential and foundational aspect of the .Net platform, and that Essence# [namespaces](namespaces)(namespaces)(namespaces)(namespaces) were designed to deal with the fact that [namespaces](namespaces)(namespaces)(namespaces)(namespaces) are so central to the way that the .Net framework operates.

You partition your code base into one or more class libraries so that your code modules can be loaded when and as needed as logically-consistent and cohesive units of functionality. You partition your code into one or more namespaces in order to isolate the named entities in your code base from unwanted name clashes with other parts of your own code base, or from unwanted name clashes with code from third parties (of which you may have no knowledge or even interest.) That’s two separate concerns solved by two separate mechanisms (“objects,”) which is proper OO design.

**Key point:**  You probably should not export your code to the Essence# [Standard Library](Standard-Library), for the reasons explained in the section on [integration issues](integration-issues). For one thing, it puts your work at risk of being overwritten when you merge with a new release of Essence# using the Essence# installation program. Using your own, private Essence# [class library](https://essencesharp.codeplex.com/wikipage?title=Class%20Libraries&referringTitle=Documentation) avoids all risk of that happening, and provides several other important benefits that I won’t go into here.

#### Using the EssenceClassLibraryExporter
The following code snippet will export the class named _MyClass_ to the Essence# [class library](https://essencesharp.codeplex.com/wikipage?title=Class%20Libraries&referringTitle=Documentation) _MyClassLibrary_ in the Essence# [namespace](https://essencesharp.codeplex.com/wikipage?title=Namespaces&referringTitle=Documentation) _MyNamespace,_ assuming that _MyClass_ is locally defined in the namespace Smalltalk (i.e., that’s the namespace in which the class is defined in the Pharo, Squeak or VisualWorks image you’re using):

{{
EssenceClassLibraryExporter libraryPath: #('MyClassLibrary').
(EssenceClassLibraryExporter 
        exportingTo: 'MyNamespace' 
        from: Smalltalk)
                exportClass: MyClass}}

By default, the exported code will be placed in the current working directory of your Smalltalk image. So in the case of the example above, the folder representing/implementing the target Essence# [class library](https://essencesharp.codeplex.com/wikipage?title=Class%20Libraries&referringTitle=Documentation) would therefore be the current working directory of your Smalltalk image, and the target [namespace](https://essencesharp.codeplex.com/wikipage?title=Namespaces&referringTitle=Documentation) would be implemented/represented by the folder MyNamespace, which would be created as a direct subfolder in the image’s current working directory.

To use a different folder as the target Essence# [class library](https://essencesharp.codeplex.com/wikipage?title=Class%20Libraries&referringTitle=Documentation), send the message #libraryPathPrefix: _aPathnameString_ to the class _EssenceClassLibraryExporter_ before exporting your code, as in the following example:

{{
EssenceClassLibraryExporter libraryPathPrefix: 
        '/Users/myUserName/Documents/Developer/EssenceSharp/Source/Libraries/'.
}}

To export to a nested Essence# [namespace](https://essencesharp.codeplex.com/wikipage?title=Namespaces&referringTitle=Documentation), use a qualified name (“dotted notation”) for the target Essence# [namespace](https://essencesharp.codeplex.com/wikipage?title=Namespaces&referringTitle=Documentation) name, as in the following example, which would export to the nested [namespace](https://essencesharp.codeplex.com/wikipage?title=Namespaces&referringTitle=Documentation) _MyRootNamespace.MyNestedNamesapce,_ at the absolute path ‘/Users/myUserName/Documents/Developer/EssenceSharp/Source/Libraries/MyClassLibrary/MyNamespace/MyNestedNamespace/':

{{
EssenceClassLibraryExporter libraryPathPrefix: 
        '/Users/myUserName/Documents/Developer/EssenceSharp/Source/Libraries/'.
EssenceClassLibraryExporter libraryPath: #('MyClassLibrary').
(EssenceClassLibraryExporter 
        exportingTo: 'MyNamespace.MyNestedNamespace' 
        from: Smalltalk)
                exportClass: MyClass
}}

To export all the classes in a class category, replace the message #exportClass: _aClass_ as in the examples above with the message #exportClassCategory: _aSymbolNamingAClassCategory,_ as in the following example:

{{
EssenceClassLibraryExporter libraryPathPrefix: 
        '/Users/myUserName/Documents/Developer/EssenceSharp/Source/Libraries/'.
EssenceClassLibraryExporter libraryPath: #('DevelopmentTools').
(EssenceClassLibraryExporter 
        exportingTo: 'SystemTesting.SUnit' 
        from: Smalltalk)
                exportClassCategory: #'SUnit-Core-Kernel'
}}

**Note:** If you use the Pharo version of _EssenceClassLibraryExporter_ (in a Pharo image, of course,) then [traits](traits) will be handled correctly.

_
The essence of OOP: It's all messages, all the time._

