/*
 * Copyright (c) 2014, Alan L. Lovejoy
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 * 1. Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer. 
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 * The views and conclusions contained in the software and documentation are those
 * of the authors and should not be interpreted as representing official policies, 
 * either expressed or implied, of the Essence Sharp Project.
*/

#region Using declarations
#endregion

namespace EssenceSharp.Runtime {

	/*
	 
	Object state architecture is the "shape," "format," "internal structure" or "memory layout" of an object. It's the difference between a stateless object and one with state. 
	It's the difference between a "value" type with no internal structure other than its bits and a "structured" type with individually-addressable slots ("members" or "fields" 
	or "instance variables.") It's the difference between an object whose slots are accessed by index (e.g., an array) or one whose slots are accessed by name (e.g, a struct or 
	record.)

	In essence, object state architecture defines the structure of an object and the way the bits of its value are interpreted, In other words, it defines both the syntax and 
	the semantics of the bits of the object's value.
	
	*/

	public enum ObjectStateArchitecture {

		// General Objects

		Abstract,					// A class whose instance architecture is #Abstract cannot have any instances.
		Stateless,					// The instances of a class whose instance architecture is #Stateless cannot have any state at all. For example, 
								// the class Object.
		NamedSlots,					// The instances of a class whose instance architecture is #NamedSlots can have named instance variables ("fields" 
								// in CLR-speak.) The instance variables ("fields") are dynamically typed; they work as though they had the C# type 
								// "Dynamic." (Note that there are more specific object state architectures that can also have named instance variables. 
								// So #NamedSlots is just the most abstract or general case.)

		IndexedObjectSlots,				// The instances of a class whose instance architecture is #IndexedObjectSlots can have any number of indexable 
								// slots--including none at all. They can also optionally have named instance variables. In both cases, the slots work 
								// as though they had the C# type "Dynamic." Such objects are the Essence# equivalent of C# object arrays.
		IndexedByteSlots,				// The instances of a class whose instance architecture is #IndexedByteSlots can have any number of indexable 
								// slots--including none at all--where each slot is physically stored as an unsigned 8-bit value. They can also 
								// optionally have named instance variables. Such objects are the Essence# equivalent of C# byte arrays.
		IndexedCharSlots,				// The instances of a class whose instance architecture is #IndexedCharSlots can have any number of indexable 
								// slots--including none at all--where each slot is physically stored as a Unicode character value. They can also 
								// optionally have named instance variables. Such objects are the Essence# equivalent of C# char arrays. The object 
								// state architecture of instances of the Essence# String class is #IndexedCharSlots.
		IndexedHalfWordSlots,				// The instances of a class whose instance architecture is #IndexedHalfWordSlots can have any number of indexable 
								// slots--including none at all--where each slot is physically stored as an unsigned 16-bit value. They can also 
								// optionally have named instance variables. Such objects are the Essence# equivalent of C# ushort arrays.
		IndexedWordSlots,				// The instances of a class whose instance architecture is #IndexedWordSlots can have any number of indexable 
								// slots--including none at all--where each slot is physically stored as an unsigned 32-bit value. They can also 
								// optionally have named instance variables. Such objects are the Essence# equivalent of C# uint arrays.
		IndexedLongWordSlots,				// The instances of a class whose instance architecture is #IndexedLongWordSlots can have any number of indexable 
								// slots--including none at all--where each slot is physically stored as an unsigned 64-bit value. They can also 
								// optionally have named instance variables. Such objects are the Essence# equivalent of C# ulong arrays.
		IndexedSinglePrecisionSlots,			// The instances of a class whose instance architecture is #IndexedLongWordSlots can have any number of indexable 
								// slots--including none at all--where each slot is physically stored as 32-bit IEEE floating point value. They can 
								// also optionally have named instance variables. Such objects are the Essence# equivalent of C# float arrays.
		IndexedDoublePrecisionSlots,			// The instances of a class whose instance architecture is #IndexedLongWordSlots can have any number of indexable 
								// slots--including none at all--where each slot is physically stored as 64-bit IEEE floating point value. They can 
								// also optionally have named instance variables. Such objects are the Essence# equivalent of C# double arrays.
		IndexedQuadPrecisionSlots,			// The instances of a class whose instance architecture is #IndexedLongWordSlots can have any number of indexable 
								// slots--including none at all--where each slot is physically stored as 128-bit floating point value, using a 
								// CLR-specific format. They can also optionally have named instance variables. Such objects are the Essence# 
								// equivalent of C# decimal arrays.
		
		// System Objects
		
		Symbol,						// An Essence# Symbol is an immutable character string, such that there is only ever one instance with the same 
								// characters; symbol instances also have some system reflective/introspective behaviors and usages. Instances 
								// may optionally have programmer-accessible named instance variables.
		Message,					// A Message instance specifies a message that was or could be be sent, along with the message arguments, if any. 
								// Instances are created by the run time system when and as needed, although application code may also create and 
								// use instances. Message instances cannot have programmer-accessible named instance variables.
		MessageSend,					// A MessageSend serves as a polymorphic inline cache that is directly assessible in Essence# code. It is especially
								// useful in situations where the message to be sent cannot be known at the time the code using it is compiled.
		Association,					// An Association is conceptually the same thing as a CLR KeyValuePair. Associations cannot have programmer-accessible 
								// named instance variables.
		BindingReference,				// A BindingReference is a specialized type of of Association used by Namespaces. BindingReferences cannot have 
								// programmer-accessible named instance variables.
		IdentityDictionary,				// IdentityDictionary instances act as "dictionaries" that map keys to values. An IdentityDictionary compares keys 
								// using object identity. Instances may optionally have programmer-accessible named instance variables.
		Dictionary,					// Dictionary instances act as "dictionaries" that map keys to values. A Dictionary compares keys based on the 
								// logical or conceptual value of the keys. Instances may optionally have programmer-accessible named instance 
								// variables. 
		Namespace,					// Namespace instances serve as dynamic namespaces at runtime. Instances may optionally have programmer-accessible 
								// named instance variables.
		Pathname,					// A Pathname instance serves as a hierarchical key whose elements are Strings. It's used for identifying namespaces, 
								// file pathnames, URLs, etc. Instances may optionally have programmer-accessible named instance variables.
		Block,						// A block is an anonymous function with full closure semantics. The implementation uses CLR delegates. Blocks cannot 
								// have programmer-accessible named instance variables.
		Method,						// A method is executable code that runs in the context of a specific class, with full access to the internal state 
								// of the distinguished object that receives the message that invokes the method. Methods cannot have 
								// programmer-accessible named instance variables.
		Behavior,					// A Behavior is a proto-class. There can actually be instances--it's not abstract. Instances may optionally have 
								// programmer-accessible named instance variables. 
		Class,						// A Class is a full Essence# class which is a subclass of Behavior, is an instance of a Metaclass, and whose instances 
								// (if it's allowed to have any) can be an object (value) of any type. The term 'class' is usually intended to refer 
								// to an (indirect) instance of the class Class, but technically can refer to any Object that can create instances 
								// of itself, such as a Behavior or a Metclass (i.e., any instance of Behavior or anything that inherits from Behavior.) 
								// Instances may optionally have programmer-accessible named instance variables. 
		Metaclass,					// A Metaclass is an Essence# class which is a direct subclass of the (Essence#) class Behavior. A Metaclass is an 
								// instance of the class Metaclass, and its instances must be Classes. A Metaclass can have only one instance which 
								// is called either the canonical instance or the sole instance. Note that the superclass of the Metaclass of any root 
								// Behavior (e.g., the metaclass of class Object) is (and must be) the class Class. Instances may optionally have 
								// programmer-accessible named instance variables. 
		BehavioralTrait,				// A Trait is a composable unit of behavior. Traits can be "used" by a class or by another Trait with the effect of 
								// adding the methods defined (or used) by the Trait to the method dictionary of the using class or of the using Trait. 
								// A BehavioralTrait is a Trait usable by any BehavioralTrait or by any Behavior (i.e., by any instance of the class 
								// BehavioralTrait, or by any instance of the class Behavior, or by any instance of any subclass of either the class 
								// BehavioralTrait or of the class Behavior.)
		InstanceTrait,					// An InstanceTrait is a Trait usable by any InstanceTrait or by any Class (i.e, by any instance of the class 
								// InstanceTrait or by any class that isn't a metaclass)
		ClassTrait,					// A ClassTrait is a Trait usable by any ClassTrait or by any Metaclass (i.e., by any instance of the class ClassTrait 
								// or by any instance of  the class Metaclass)
		TraitTransformation,				// A TraitTransformation acts a decorator of a Trait. It is used to exclude or rename one or more method selectors
								// of the Trait it decorates. TraitTransformations are defined by algebraic Trait expressions at run time.
		TraitComposition,				// A TraitComposition combines one or more Traits into a new Trait that is the symmetric set difference of the
								// combined Traits. TraitCompositions are defined by algebraic Trait expressions at run time.
		HostSystemObject,				// A "host system object" is simply an instance of any CLR type which is not a formal part of the Essence# runtime 
								// system. One of the requirements for an Essence# class to represent a CLR type (which may or may not be a "class" 
								// as the CLR defines that term) is that its instance type must be #HostSystemObject.
								
		// Values adopted as-is from the CLR

		Nil,						// A class whose instance architecture is #Nil governs the behavior (in Essence# code) of the value "null," which in 
								// Essence# syntax is written as nil. Nil (or "null") is technically a sentinel or metavalue whose meaning is "there 
								// is no value being referenced."
		False,						// A class whose instance architecture is #False governs the behavior (in Essence# code) of the value false.
		True,						// A class whose instance architecture is #True governs the behavior (in Essence# code) of the value true.
		Char,						// A class whose instance architecture is #Char governs the behavior (in Essence# code) of values of CLR type char 
								// (Unicode character values.)
		SmallInteger,					// A class whose instance architecture is #SmallInteger governs the behavior (in Essence# code) of all integer values 
								// that aren't BigNums. The Essence# compiler always uses Int64 values for integer literals.
		SinglePrecision,				// A class whose instance architecture is #SinglePrecision governs the behavior (in Essence# code) of all IEEE 32-bit 
								// floating point values.
		DoublePrecision,				// A class whose instance architecture is #DoublePrecision governs the behavior (in Essence# code) of all IEEE 64-bit 
								// floating point values.
		QuadPrecision,					// A class whose instance architecture is #QuadPrecision governs the behavior (in Essence# code) of all CLR values 
								// of type Decimal (128-bit floating point values that use a proprietary format.)

		// Not yet implemented
		LargeInteger,					// Support for large integers is currently in the development plan. When implemented, there will be an Essence# class 
								// that can create and govern the behavior of BigNum values. Also, Int64 and UInt64 arithmetic operations will 
								// transparently overflow into BigNum values.
		ScaledDecimal,					// When implemented, the ScaledDecimal class will provide unlimited-precision rational numbers with a fixed decimal point.

	}

	public enum PrimitiveDomainType {
		Object,
		NamedSlots,										
		IndexedSlots,
		IndexedObjectSlots,				
		IndexedByteSlots,				
		IndexedCharSlots,				
		IndexedHalfWordSlots,				
		IndexedWordSlots,				
		IndexedLongWordSlots,			
		IndexedSinglePrecisionSlots,			
		IndexedDoublePrecisionSlots,			
		IndexedQuadPrecisionSlots,			
		Pathname,
		Symbol,						
		CompiledCode,		
		Block,					
		Method,					
		Message,					
		MessageSend,					
		Association,					
		BindingReference,					
		Dictionary,					
		Namespace,					
		Behavior,					
		Class,						
		Metaclass,	
		BehavioralTrait,
		InstanceTrait,
		ClassTrait,
		TraitTransformation,
		TraitComposition,
		Nil,		 
		Boolean,
		True,
		False,
		Magnitude,
		Char,
		ArithmeticValue,
		Number,
		Integer,		
		SmallInteger,					
		LargeInteger,
		ScaledDecimal,					
		InvariantPrecisionReal,
		SinglePrecision,				
		DoublePrecision,				
		QuadPrecision,	
		CLR_System_Object,
		CLR_System_Type,
		CLR_System_Array,
		CLR_System_Delegate,
		CLR_System_Exception,
		CLR_System_Collections_Generic_List_1_Object		
	}	

	public enum CanonicalSelectorSemantics {
		None,
		DoesNotUnderstand,
		IsIdenticalTo,
		IsNotIdenticalTo,
		IdentityHash,
		IsEqualTo,
		IsNotEqualTo,
		Hash,
		Class,
		IsMemberOf,
		IsKindOf,
		AsBehavior,
		AsClass,
		AsMetaclass,
		IsNil,
		IsNotNil,
		IsBoolean,
		IsTrue,
		IsFalse,
		IsImmutable,
		Yourself,
		ShallowCopy,
		Copy,
		Coerce,
		AsImmutable,
		AsMutable,
		AsCharacter,
		AsInteger,
		AsFloat,
		AsDouble,
		AsQuad,
		AsString,
		AsSymbol,
		AsAssociationTo,
		InstVarValueAtName,
		InstVarValueAtNamePut,
		New,
		NewWithSize,
		Size,
		At,
		AtIfAbsent,
		AtPut,
		AtIfAbsentPut,
		Remove,
		RemoveIfAbsent,
		RemoveAt,
		RemoveKey,
		RemoveKeyIfAbsent,
		InvokeBlock,
		InvokeBlockWithArguments,
		Perform,
		PerformWithArguments,
		OnDo,
		Ensure,
		IfCurtailed,
		IfNil,
		IfNotNil,
		IfNilIfNotNil,
		IfNotNilIfNil,
		LogicalNot,
		LogicalAnd,
		LogicalOr,
		LogicalXor,
		ConditionalAnd,
		ConditionalOr,
		IfTrue,
		IfFalse,
		IfTrueIfFalse,
		IfFalseIfTrue,
		TimesRepeat,
		WhileNil,
		WhileNotNil,
		WhileNilDo,
		WhileNotNilDo,
		WhileTrue,
		WhileFalse,
		WhileTrueDo,
		WhileFalseDo,
		IsLessThan,
		IsGreaterThan,
		IsLessThanOrEqual,
		IsGreaterThanOrEqual,
		CompareTo,
		IsZero,
		Positive,
		StrictlyPositive,
		Negative,
		Negated,
		Reciprocal,
		Sign,
		Abs,
		Ceiling,
		Floor,
		Rounded,
		Truncated,
		RoundTo,
		TruncateTo,
		Plus,
		Minus,
		Times,
		DivideToRational,
		DivideToInteger,
		DivideFlooredToInteger,
		DivisionRemainder,
		DivisionResidual,
		RaisedToInteger,
		RaisedTo,
		SquareRoot,
		Log,
		NaturalLog,
		BitAnd,
		BitOr,
		BitXor,
		BitShift,
		ShiftLeft,
		ShiftRight,
		ToDo,
		ToByDo,
		Do,
		FromToDo,
		FromToByDo,
		KeysDo,
		AssociationsDo,
		KeysAndValuesDo,
		Concat,
	}

	public enum AccessPrivilegeLevel { 
	// Applies to "global" variable references, where "global" means a) not a temporary variable, b) not a parameter and c) not an instance var.
		Public,		// Accessible by qualified reference from anywhere, or else by "Import," if "InHierarchy," or if "Local."
		InHierarchy,	// Accessible from any inheriting scope, including the "Local" scope. However, not importable, and not accessible by qualified reference.
		Local,		// Accessible only from within the same ("Local") scope. Not accessible from inheriting scopes, not importable, and not accessible by qualified reference.
	}

	public enum ImportTransitivity {
		Intransitive,	// Does not include any imported entities in the search.	
		Transitive	// Transitively includes imported entities in the search.)
	}

}
