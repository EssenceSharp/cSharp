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

	public enum ObjectStateArchitecture {
		// General
		Abstract,					// The instance architecture of an abstract class--there can't be any instances.
		Stateless,					// The instance architectue of a class whose instances must have no instance variables (e.g. UndefinedObject)
		IndexedByteSlots,				// CLR byte[]; wrapped
		IndexedCharSlots,				// CLR char[]; wrapped; used to implement the Smalltalk (mutable) String class.
		IndexedHalfWordSlots,				// CLR ushort[]; wrapped
		IndexedWordSlots,				// CLR uint[]; wrapped
		IndexedLongWordSlots,				// CLR ulong[]; wrapped
		IndexedSinglePrecisionSlots,			// CLR float[]; wrapped
		IndexedDoublePrecisionSlots,			// CLR double[]; wrapped
		IndexedQuadPrecisionSlots,			// CLR Decimal[]; wrapped
		IndexedObjectSlots,				// CLR STObject[]; wrapped: Implements any Smalltalk class with indexed Object instance variables.
		NamedSlots,					// For any any Smalltalk class whose instances have named instance variables (generally analogous to instances 
								// of a normal C# class)
		
		// System classes: For any system class, neither adding instance variables nor changing the instance architecture is allowed.
		
		// System - Singletons or wrapped primitive values 
		Nil,						// ANSI Smalltalk: No CLR equivalent; it's a sentinel or metavalue which is semantically the same as "null"
		False,						// CLR bool (false as a singleton sentinel)
		True,						// CLR bool (true as a singleton sentinel)
		Char,						// CLR char (so it's full Unicode); wrapped
		SmallInteger,					// CLR long (so it's a 64-bit "small" integer); wrapped
		LargeInteger,					// CLR BigNum; wrapped
		ScaledDecimal,					// No direct CLR equivalent, but serves the same purpose as Decimal: It's a fixed-point fraction where the 
								// denominator is always a power of 10.
		SinglePrecision,				// CLR float; wrapped
		DoublePrecision,				// CLR double; wrapped
		QuadPrecision,					// CLR Decimal: Pretends to be a "scaled decimal" a la ANSI, but is actually just a non-standard 16-byte 
								// floating point number; wrapped
		
		// System - Internal
		Symbol,						// CLR String (immutable,) but uses the wrapped strict-flyweight pattern for correctness and performance 
								//reasons. Run time and primitives rely on it heavily.
		Message,					// No CLR equivalent: It specifies the name of a method to be invoked along with the arguments, if any. 
								// Created by the run time when needed.
		MessageSend,					// Conceptually a DLR CallSite: Same as a Message, but also specifies the receiver of the message (it caches 
								// method lookup, so it's a polymorphic inline cache)
		Association,					// Conceptually the same thing as a KeyValuePair.
		BindingReference,					// The type of Association used by a Namespace.
		IdentityDictionary,				// A Dictionary, implemented as a wrapper over a CLR Dictionary, and which uses ReferenceEqual to compare keys.
		Dictionary,					// A Dictionary, implemented as a wrapper over a CLR Dictionary
		Namespace,					// Conceptually the same thing as a CLR namespace...except it's an Object at runtime, can be sent messages,
								// be assigned to a variable and passed as an argument.
		Pathname,					// A pathname is a hierachical key whose elements are Strings. It's used for spececifing namespaces, 
								// filenames, URLs, etc.
		Block,						// CLR delegate, also known as a BlockClosure.
		Method,						// CLR delegate: But its first argument must be the reciver ('this' in C# terminology, 'self' in Smalltalk 
								// terminology); and it can access instance variables, class variables and whatever its class imports.
		Behavior,					// Basic Smalltalk Class which is a direct subclass of (Smaltalk) class Object. The instances of a Behavior 
								// may be any Smalltalk Object, including another Behavior, a Class or a Metaclass.
		Class,						// A full Smalltalk class which is a subclass of Behavior and is an instance of either a Class, a Metaclass or 
								// of a Behavior, and whose instances can be a Behavior, a Class, a Metaclass or (much more usually) a Smalltalk 
								// Object that isn't a Behavior, Class or Metaclass. The term 'class' is usually intended to refer to an 
								// instance of the class Class, but technically can refer to any Object that can create instances of itself, 
								// such as a Behavior or a Metclass (i.e., any instance of Behavior or anything that inherits from Behavior.) 
		Metaclass,					// A Smalltalk class which is a direct subclass of (Smalltalk) class Behavior. It may be an instance of either 
								// a Class or of a Behavior, and its instances must be Classes. A Metaclass can have only one non-obsolete 
								// instance which is called the canonicalInstance. Note that the superclass of the Metaclass of any root 
								// Behavior (e.g., the metaclass of class Object) is (and must be) the class Class.
		HostSystemObject				// A non-Essence Smalltalk Object. A class whose instance architecture is addMethod creates instances 
								// that are not Essence Smalltalk objects. Its CompiledMethods expect an instance of a particular non-Essence 
								// Smalltalk class as their "receiver" argument. Its purpose is to map Essence Smalltalk messages to 
								// non-Smalltalk method calls. At least some of its methods should be primitives whose primitive descriptor 
								// provides the reflection parameters necessary to invoke the methods of the non-Essence Smalltalk Object. 
								// 
								// Example: 
								//
								//		## copyFrom: startIndex for: length
								//			<context: #CSharp memberName: #Substring paramType: #Int32 paramType: #Int32>
	}

	public enum PrimitiveDomainType {
		Object,
		IndexedSlots,
		IndexedByteSlots,				
		IndexedCharSlots,				
		IndexedHalfWordSlots,				
		IndexedWordSlots,				
		IndexedLongWordSlots,			
		IndexedSinglePrecisionSlots,			
		IndexedDoublePrecisionSlots,			
		IndexedQuadPrecisionSlots,			
		IndexedObjectSlots,				
		NamedSlots,										
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
		Symbol,						
		Message,					
		MessageSend,					
		Association,					
		BindingReference,					
		Dictionary,					
		Namespace,					
		Pathname,
		CompiledCode,		
		Block,					
		Method,					
		Behavior,					
		Class,						
		Metaclass,					
		HostSystemObject				
	}	

	public enum CanonicalSelectorSemantics {
		None,
		IsIdenticalTo,
		IsNotIdenticalTo,
		IdentityHash,
		IsEqualTo,
		IsNotEqualTo,
		Hash,
		Class,
		IsMemberOf,
		IsKindOf,
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
		Perform,
		PerformWith,
		PerformWithArguments,
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
