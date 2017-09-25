# Namespaces 
A _namespace_ is a core concept and construct of Essence#. It is conceptually similar to a namespace in other programming languages; its primary function is to prevent name clashes when code from different sources must be integrated into an application or class library that is a client of "foreign" / "external" code libraries. But there are some important differences:

* An Essence# namespace is an object at runtime. It can be sent messages. And those messages can change the internal state of the namespace in interesting ways.

* An Essence# namespace can be created at any time during program execution.

* Because a namespace is an object, it has a class. And because it has a class, its behavior can be modified by programmers who use the language...or even by the program itself, at run time.

* An Essence# class is a type (specialization) of a namespace: All classes are namespaces, but not all namespaces are classes.  And since namespaces are nestable, so are classes. A class can contain nested namespaces. And since a class is a namespace, a class can contain other classes.

* The name of a namespace (or class) can be changed as the program runs. So can its location in the namespace hierarchy.

* A namespace can be bound to a programmer-specified .Net [assembly](http://stackoverflow.com/questions/2972732/what-is-net-assembly). Since a class is a namespace, so can a class. This binding can be changed as the program runs.

* A namespace can be bound to a programmer-specified .Net [namespace](http://broadcast.oreilly.com/2010/07/understanding-c-namespaces-and.html). So can a class. This binding can be changed as the program runs. (And an Essence# class can be bound to a .Net class--a binding which can _also_ be changed as the program runs.)

## Namespace operations
### Declaring
An entry in a namespace can be declared with any of three different "visibilities" or "access privileges": #Local, #InHierarchy or #Public. An entry with #Local visibility can only be seen/accessed from within the same namespace. An entry with #InHierarhcy visibility can be accessed from within the same namespace, or from any child namespace. An entry with #Public visibility can be accessed from anywhere (although that may require using a qualified name.)

Messages which can be sent to a namespace in order to add entries include:

* **#declareVariable:** _variableName_ **withAccess:** _accessPrivilege_ **onCollision:** _zeroArgumentBlock._ The block argument will be executed if there is already an entry in the namespace with the same name--in which case, the pre-existing entry will not have been modified.

* **#declareConstant:** _constantName_ **withValue:** _valueOfConstant_ **access:** _accessPrivilege_ **onCollision:** _zeroArgumentBlock._ The block argument will be executed if there is already an entry in the namespace with the same name--in which case, the pre-existing entry will not have been modified.

* **#defineNamespace:** _namespaceName_ **withAccess:** _accessPrivilege_ **configure:** _oneArgumentBlock._ The message _may_ result in a new namespace being created (with a default configuration) and added as an entry in the receiving namespace--but only if there was no namespace _or class_ with the same name already present. In either case, the one-argument block will be executed with the namespace (or class) that is bound to _namespaceName_ as its argument. That provides a convenient opportunity for the sender to send any desired configuration messages to the (possibly new) namespace. If such is not desired, the value **nil** may be passed as the final argument, instead of a block.

* **#defineClass:** _className_ **withAccess:** _accessPrivilege_ **configure:** _oneArgumentBlock._ The message _may_ result in a new class being created (with a default configuration) and added as an entry in the receiving namespace--but only if there was no class with the same name already present. In either case, the one-argument block will be executed with the class that is bound to _className_ as its argument. That provides a convenient opportunity for the sender to send any desired configuration messages to the (possibly new) class. If such is not desired, the value **nil** may be passed as the final argument, instead of a block.

### Importing
A namespace may import other namespaces in their entirety, or it may import specific entries from a source namespace. When importing a specific entry from a source namespace, the name of the entry in the importing namespace and the name in the source namespace may optionally be different.

Also, importing may be either transitive or intransitive. Intransitive import is the default, and its semantics corresponds more closely to the import semantics typically used in other programming languages (although it may not be identical): _Intransitive_ import means that only the entries declared locally in the source namespace, along with those locally declared in any of its parent namespaces, will be imported.

_Transitive_ import makes all names that are visible in the source namespace also visible in the importing namespace--so it transitively includes all names imported into the source namespace, or imported into any parent namespace of the source namespace.

Names may be imported with any of the three visibility/accessibility levels: #Local, #InHierarchy or #Public. So an importer can control who can see/access an imported name.

The messages that can be sent to a namespace to perform import operations include the following:

* **import:** _namespaceSpec_ **withAccess:** _accessPrivilege._

* **importTransitive:** _namespaceSpec_ **withAccess:** _accessPrivilege._

* **import:** _nameInSource_ **from:** _namespaceSpec_ **as:** _localAlias_ **withAccess:** _accessPrivilege._

* **importTransitive:** _nameInSource_ **from:** _namespaceSpec_ **as:** _localAlias_ **withAccess:** _accessPrivilege._
The value of _namespaceSpec_ may be any of:

# a Namespace instance
# a Symbol, possibly using dotted notation (e.g, #CLR.System.Random)
# A Pathname instance

## Lookup
There are two distinct ways to lookup names in a namespace: _Syntactically_ (the way most programming languages do it) and _dynamically, by sending messages to the namespace object._ Syntactic name lookup works in essentially the same way it does in C#, using dotted notation to construct a "qualified name." 

Using a qualified name to access a namespace entry _always_ counts as a _public_ access, and so will fail if the entry does not have "Public" visibility.

It is not necessary to use dotted notation when accessing a name that is visible according to the name binding rules, as explained below. Nor is it necessary to specify the Root namespace in a qualified name.

Name lookup is done based on the following binding precedence rules:

# Names locally declared in the target namespace have the highest binding precedence, so they bind before any other visible names.
# Names directly and specifically imported into the target namespace have the second highest binding precedence (this involves imports of specifically-named entries.)
# Names imported en mass into the target namespace have the third highest binding precedence (i.e., general import of an entire namespace.)
# If the target namespace is a class, then any names visible in any superclass have the fourth highest binding precedence.
# The lowest binding precedence goes to any names imported from a parent namespace that is not a class.

Note than in the case of code compiled with a class as the namespace context, there are actually _two_ inheritance paths for name lookup: The class inheritance hierarchy (the superclass chain of the class) _and also_ the namespace in which the class itself resides.

It should also be noted that the containing namespace of a metaclass is its sole (canonical) instance.

The API for dynamically performing namespace lookups includes the same messages as those used to find entries in an Essence# Dictionary. The simplest such message is **#at:** _aKey,_ although unlike a Dictionary, the key _must_ be a String or Symbol.

## Namespace structure and semantics
### BindingReferences and BindingHandles
A "[BindingReference](BindingReference)(BindingReference)" is an object that actually implements the concept of "namespace entry." In contrast, a BindingHandle is the object that directly stores the value of a namespace entry.  A namespace is a collection of [BindingReference](BindingReference)(BindingReference)s, each of which contains a key (which must be a String) and a BindingHandle. The BindingHandle directly stores the value of the binding.

When the compiler encounters a reference to a variable that is not a stack-resident variable nor an instance variable, it emits a DLR CallSite whose responsibility is to bind to the named variable _at runtime._ The first time the thread of program execution hits such a CallSite (which is .Net's implementation of a polymorphic inline cache), the CallSite attempts to find the [BindingReference](BindingReference)(BindingReference)(BindingReference) whose key is the variable's name using the namespace the compiler told it to use. If the [BindingReference](BindingReference)(BindingReference)(BindingReference) is found, the CallSite dynamically generates code that directly accesses the value of the BindingHandle contained in the [BindingReference](BindingReference)(BindingReference)(BindingReference)--which is a very efficient operation.  The logic to do the lookup of the BindingHandle happens only once, the first time the variable is accessed by that CallSite.  After that, the code generated by the CallSite to access the value of the BindingHandle is simply reused. The LINQ Expression to generate that code is literally this simple: "Expression.Field(Expression.Constant(bindingHandle), "value");"

There are two separate reasons for using BindingHandles in addition to [BindingReference](BindingReference)s: 

1) It gives the runtime system greater flexibility, because it can choose to just change the value stored by the BindingHandle (the normal case,) or it can instead give a [BindingReference](BindingReference) a new BindingHandle--which it needs to do in cases where the BindingHandle is immutable but the value bound to that variable name _must_ be changed, at least for all future lookups.

2) It makes it possible to use different types of BindingHandles for different purposes, without any effect or dependencies on the type of [BindingReference](BindingReference)(BindingReference) used. The choice of [BindingReference](BindingReference)(BindingReference) type can then be purely the responsibility of the Namespace, whereas the choice of the type of BindingHandle can be purely the responsibility of the referenced object itself.

### Namespace entry mutability
A namespace entry may be either mutable or immutable. It may also be either deletable or non-deletable. By default, a newly-added entry will be both mutable (a different value can be assigned to the name later) and deletable (the entry might later be removed.) 

An immutable namespace entry is essentially equivalent to a "constant." An entry can be made immutable at any time by sending a message to the _binding handle_ that holds it. Making an entry immutable is a one-way, permanent change. Note that it is the binding that becomes immutable here, not the object referenced by the variable name (although any Essence# object can _also_ be made actually immutable or "read only," that's not at all the same thing.)

To make an entry non-deletable, a message must be sent to its _binding reference._ 

### Special system namespaces
There is a universal root for all namespaces whose name is "Root". The Root namespace canonically has three child namespaces: The default system namespace, the "Undeclared" namespace and the "CLR" namespace, all of which have special semantics:

* The default system namespace is the namespace in which all of the core system classes and other globals reside. It is also the namespace in which code is compiled and executed by default, unless some other namespace is explicitly specified. Since a class is a type of namespace, code compiled and/or executed in the context of a class uses that class as its namespace.

* The "Undeclared" namespace is where all references to variables are put when a) the compiler is not able to find a binding for a referenced variable, and b) the compilation options don't require the compiler to abort the compilation when an undeclared variable reference is encountered.

* The "CLR" namespace is the default parent namespace for all root namespaces known to .Net and the CLR. Unless overridden by the program, all namespaces that are direct or indirect children of the CLR namespace are automatically bound to a .Net/CLR namespace with the _corresponding_ qualified path name, where "corresponding" means "without the prefix Root.CLR." **Consequently, there is a default, standard qualified name for the Essence# class that corresponds to any type (struct or class) known to the CLR.** (The runtime system makes it appear as though there is always an Essence# class that shadows/represents/corresponds to every CLR type, although what it actually does is to create any such classes dynamically, when and as needed--but only once, of course.)

## Namespaces and interoperability with other CLR languages
Essence# namespaces (including Essence# classes) play a key role in enabling and implementing interoperability with other [CLR languages](http://en.wikipedia.org/wiki/List_of_CLI_languages)--especially pervasively statically-typed languages such as C#, F# and Visual Basic.

### .Net Types and Assemblies
One key issue that must be handled in order to adroitly interoperate with CLR-hosted languages--especially those that mostly or fully depend on native CLR "types" and don't exclusively use the DLR's Meta Object Protocol--is the ability to resolve and bind to CLR types. And that includes types for which the only initially-available information is the type's name.

 **Without the CLR type object, it's not possible to create instances of the type, nor is it possible to perform operations on instances of the type.** And in general, it's not possible to obtain a CLR type object without having its assembly-qualified name. Of course, such "assembly-qualified" CLR type names are just not appropriate for use as the syntactically-required names of classes in a general-purpose programming language, regardless of the language. The following example of the "assembly qualified name" of the CLR's class "Object" demonstrates that point rather forcefully:

**System.Object,** **mscorlib,** **Version=2.0.0.0,** **Culture=neutral,** **PublicKeyToken=b77a5c561934e089**

Languages such as C# deal with this problem in two ways: 1) They require that the code one writes must explicitly list (syntactically) the namespaces from which that code intends to import types, and 2) they require that the programmer uses a development environment--such as Visual Studio--that provides mechanisms by which application developers can bind namespaces to specific assemblies (in other words, to specific .DLL files.)

The key differences between the way Essence# solves this problem and the way C# solves this problem is that Essence# a) doesn't require any special syntax to specify which namespaces are being used, b) explicitly binds namespaces to a specific assembly, instead of requiring the system to infer the assembly from the name of the namespace and type, and c) handles the binding of namespaces to assemblies dynamically in the runtime system and language library code, instead of depending on special development tools, such as VisualStudio or some equivalent development platform. 

So an Essence# program is fully capable of binding namespaces (including classes) to assemblies on its own, without any need for an external development platform. And it does so in a way that avoids the problem of types residing in different assemblies that happen to have identical fully-qualified names (same namespace and same name.)

All Essence# namespaces (and so all classes) are always bound to an assembly. By default, they are bound to the assembly to which the namespace that contains them is bound.  The Essence# Root namespace is bound to the assembly in which the Essence# runtime system resides. 

A namespace (or class) can be bound to a different assembly by sending it the message **#assemblyName:** _assemblyName,_ where the _assemblyName_ argument is a fully-qualified assembly name (String.) That is usually sufficient, but not always. There are times when the programmer (or the program) does not know the required assembly name, but only knows the pathname of the .DLL that contains the needed assembly.  To handle such cases, the message **#assemblyPathname:** _aPathNameString_ can be sent to the namespace, instead of sending **#assemblyName:**. The runtime system does the rest.

Of course, it only matters to which .Net assembly an Essence# class is bound when the class is _representing_ a CLR type. It's completely irrelevant otherwise. So if you're exclusively writing and/or using _native_ Essence# classes, none of this matters to you. But that "pure" case is rather unlikely. Generally, you will _at least_ be using Essence# classes that represent CLR types, even if the code for such classes is written and maintained by someone else.

### .Net Types and Namespaces

Although Essence# has its own namespace system, which is separate and distinct from the one used by the CLR, the Essence# architecture and runtime system are nevertheless designed so as to deterministically provide a mapping to and from Essence# fully qualified class names and CLR assembly-qualified type names. That matters because the CLR pervasively uses namespace prefixes to uniquely identify types, making it absolutely necessary to know a CLR type's namespace prefix and assembly name in order to resolve a type from its name (in the general case.)

There is both a _default mapping scheme_ between Essence# and CLR namespaces, and also an _ad hoc_ one that can be changed dynamically at runtime by the program itself. If used, the _ad hoc / dynamic_ mapping scheme supercedes/overrides the default one.

### Default mapping between CLR and Essence# namespace names

The default mapping scheme is relatively simple: To convert a CLR fully-qualified type name into an Essence# fully-qualified class name, simply prepend either "Root.CLR." or just "CLR." to the fully qualified CLR type name.  To convert a fully-qualified Essence# class name into the equivalent CLR fully-qualified type name, simply remove the prefix "CLR." or "Root.CLR.".

That means that there is no _default_ mapping form the names of Essence# classes and CLR type names in the case where the Essence# class is not in a namespace that is a child of the CLR namespace. However, such classes can nevertheless have a mapping to a CLR type if the _dynamic_ mapping scheme is used. 

Note: Just because there is a mapping defined between an Essence# class name and a CLR type name is **not** sufficient to make the Essence# class _represent_ a CLR type.  For more information on that topic, see the documentation for [classes](classes).

### Dynamic mapping between CLR and Essence# namespace names.

To dynamically create a mapping from an Essence# namespace to a CLR namespace, it is necessary to send messages to the namespace object (which could be a class object.)  There are two such messages, each dealing with a different mapping issue:

* To change the _local_ ("unqualified") CLR name of an Essence# namespace (or class,) send it the message **#hostSystemName:** with the desired _unqualified_ CLR type or namespace name as the argument. Sending this message has no effect on the name used in Essence# code to refer to the class or namespace to which this message is sent.

* To change the _qualified_ CLR namespace prefix of an Essence# namespace (or class,) send it the message **#hostSystemNamespace:** with the desired _qualified_ CLR namespace **prefix.**  Sending this message has no effect on the name used in Essence# code to refer to the class or namespace to which this message is sent.

**Important:** Changing the CLR assembly name, and/or changing the local CLR name and/or changing the CLR qualified namespace prefix of a class or namespace affects how all classes that are contained by the affected namespace bind to CLR types, unless such types have been separately and explicitly configured to use their own bindings.

_
The essence of OOP: It's all messages, all the time._
