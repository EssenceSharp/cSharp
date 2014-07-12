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
using System;
using System.Text;
using System.IO;
using System.Collections;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
#if CLR2
using FuncNs = Microsoft.Scripting.Utils;
#else
using FuncNs = System;
#endif
using EssenceSharp.ClientServices;
using EssenceSharp.UtilityServices;
using EssenceSharp.Exceptions.System;
#endregion

namespace EssenceSharp.Runtime { 

	public class ESSymbol : ESString {
		// Instances of STSymbol must not only be everything that a Smalltalk Symbol is and is required to be, 
		// they must also attempt--as much as possible--to act just like CLR Strings when used/accessed by non-Smalltalk code.
		
		#region Static variables and functions
		
		public static implicit operator String(ESSymbol value) {
			return value.PrimitiveValue;  
		}

		public static implicit operator char[](ESSymbol value) {
			return value.IndexedSlots;  
		}
		
		protected static new readonly String	empty				= "";
		protected static readonly char[]	emptyChars			= ESString.empty;
		protected static readonly char		pathElementSeparatorChar	= '.';

		public static char? PathElementSeparatorChar {
			get {return pathElementSeparatorChar;}
		}

		#endregion
		
		protected SymbolType			type				= SymbolType.String;
		protected CanonicalSelectorSemantics	canonicalSemantics		= CanonicalSelectorSemantics.None;
		protected String			stringValue;
		protected byte				numArgs				= 0;
		protected char				qualifiedNameSeparatorChar	= pathElementSeparatorChar;
		protected byte				pathElementCount		= 1;
				
		internal ESSymbol(ESBehavior stClass, String value, SymbolType type, long numArgs, char pathElementSeparatorChar, long pathElementCount) : base(stClass, String.IsNullOrEmpty(value) ? emptyChars : value.ToCharArray()) {
			this.type			= type;
			this.numArgs			= (byte)numArgs;
			this.qualifiedNameSeparatorChar	= pathElementSeparatorChar;
			this.pathElementCount		= (byte)pathElementCount;
			stringValue			= value; 		
			beImmutable();
		}
		
		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.Symbol;}
		}
		
		public override bool IsSymbol {
			get {return true;}
		}
		
		public SymbolType Type {
			get {return type;}
		}
			
		public CanonicalSelectorSemantics CanonicalSemantics {
			get {return canonicalSemantics;}
			internal set {canonicalSemantics = value;}
		}

		new public String PrimitiveValue {
			get {return stringValue;}
		}
			
		public override Object HostSystemValue {
			get {return stringValue;}
		}
		
		public long NumArgs {
			get {return numArgs;}
		}
		
		public char QualifiedNameSeparatorChar {
			get {return qualifiedNameSeparatorChar;}
		}
		
		public long PathElementCount {
			get {return pathElementCount;}
		}
		
		public override sealed ESObject shallowCopy() {
			return this;
		}
		
		public override sealed void postShallowCopy() {
		}

		public override ESSymbol asESSymbol() {
			return this;
		}
		
		public override ESString asESString() {
			return Class.ObjectSpace.newString(IndexedSlots);
		}
		
		public override ESPathname asESPathname() {
			return Class.ObjectSpace.pathnameFromString(PrimitiveValue, QualifiedNameSeparatorChar, null);
		}

		public override ESObject asMutable() {
			return asESString();
		}
				
		public override String asHostString() {
			return stringValue;
		}
		
		public String asHostSystemMemberName(CapitalizationScheme capScheme) {
			// Removes any keyword colons, and capitalizes the character following each keyword colon.
			// Does not do any semantic translation; the resulting String may not be a syntactically valid method, property or field name for a particular CLR langauge--or for any CLR language at all!

			char exclusionChar = (char)0;
			switch (Type) {
				case SymbolType.Identifier:
					return PrimitiveValue.usingCapitalizationScheme(capScheme);
				case SymbolType.BinaryMessageSelector:
					return PrimitiveValue;
				case SymbolType.Keyword:
					if (NumArgs < 2) {
						return (stringValue.Remove(stringValue.Length - 1)).usingCapitalizationScheme(capScheme);
					}
					exclusionChar = ':';
					break;
				case SymbolType.String:
					return PrimitiveValue.usingCapitalizationScheme(capScheme); // The returned String could have any Unicode character in it, without exception.
			}
			var sb = new StringBuilder();
			var ch = slots[0];
			switch (capScheme) {
				case CapitalizationScheme.AsIs:
					sb.Append(ch);
					break;
				case CapitalizationScheme.InitialCapital:
					sb.Append(Char.ToUpper(ch));
					break;
				case CapitalizationScheme.InitialLowerCase:
					sb.Append(Char.ToLower(ch));
					break;
			}
			bool capitalizeNextChar = false;
			for (var i = 1; i < slots.Length; i++) {
				ch = slots[i];
				if (ch == exclusionChar) {
					capitalizeNextChar = true;
				} else if (capitalizeNextChar) {
					sb.Append(Char.ToUpper(ch));
					capitalizeNextChar = false;
				} else {
					sb.Append(ch);
				}
			}
			return sb.ToString();
		}

		public void keywordsDo(System.Action<String> enumerator1) {
			if (Type != SymbolType.Keyword) return;
			var stream = new StringReader(PrimitiveValue);
			var keywordString = stream.nextIdentifier();
			do {
				enumerator1(keywordString);
				stream.nextMatches(':');
				keywordString = stream.nextIdentifier();
			} while (keywordString != null && keywordString.Length > 0);
		}

		public override ESIndexedSlotsObject<char> newWithSize(long size) {
			return Class.ObjectSpace.newString(size);
		}
		
		public override ESIndexedSlotsObject<char> newWith(char[] slots) {
			return Class.ObjectSpace.newString(slots);
		}
		
		public override ESIndexedSlotsObject<char> emptyCopy() {
			return Class.ObjectSpace.newString(0);
		}
		
		public override ESBindingReference bindingInNamespaceIfAbsent(NamespaceObject environment, AccessPrivilegeLevel requestorRights, ImportTransitivity importTransitivity, Functor0<ESBindingReference> ifAbsentAction) {
			if (PathElementCount > 1) return asESPathname().bindingInNamespaceIfAbsent(environment, requestorRights, importTransitivity, ifAbsentAction);
			var binding = environment.bindingAt(PrimitiveValue, requestorRights, importTransitivity, null);
			return binding == null ?
				ifAbsentAction == null ? null : ifAbsentAction() :
				binding;
		}
		
		public override Object valueInNamespaceIfAbsent(NamespaceObject environment, AccessPrivilegeLevel requestorRights, ImportTransitivity importTransitivity, FuncNs.Func<Object> ifAbsentAction) {
			var binding = bindingInNamespaceIfAbsent(environment, requestorRights, importTransitivity, null);
			return binding == null ?
				ifAbsentAction == null ? null : asFunctor0(ifAbsentAction)() :
				binding.Value.Value;
		}

		#region Foreign language interoperability
		
		public override char[] ToCharArray() {
			return stringValue.ToCharArray();
		}
		
		public override IEnumerator<char> GetEnumerator() {
			return stringValue.GetEnumerator();
		}

		#endregion

		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			append("#");
			switch (Type) {
				case SymbolType.Identifier:
				case SymbolType.Keyword:
				case SymbolType.BinaryMessageSelector:
					append(stringValue);
					return;
				default:
				case SymbolType.String:
					append("'");
					append(stringValue);
					append("'");
					return;
			}
		}

		public override T valueBy<T>(Operation<T> operation) {
		    return operation.applyToSymbol(this);
		}

		public new class Primitives : ESString.Primitives {

			protected override void bindToObjectSpace() {
				domainClass = objectSpace.SymbolClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.Symbol;}
			}
		
			#region Primitive Definitions
		
			public static Object _numArgs_(Object receiver) {
				return ((ESSymbol)receiver).NumArgs;
			}
		
			public Object _type_(Object receiver) {
				return SymbolRegistry.symbolFor(((ESSymbol)receiver).Type.ToString());
			}

			public Object _asMessageSend_(Object receiver) {
				return objectSpace.newMessageSend((ESSymbol)receiver);
			}

			public Object _asMessageSendWithReceiver_(Object receiver, Object messageSendReceiver) {
				return objectSpace.newMessageSend(messageSendReceiver, (ESSymbol)receiver);
			}

			#endregion

			public override void publishCanonicalPrimitives() {

				publishPrimitive("numArgs",						new FuncNs.Func<Object, Object>(_numArgs_));
				publishPrimitive("type",						new FuncNs.Func<Object, Object>(_type_));

				publishPrimitive("asMessageSend",					new FuncNs.Func<Object, Object>(_asMessageSend_));
				publishPrimitive("asMessageSendWithReceiver:",				new FuncNs.Func<Object, Object, Object>(_asMessageSendWithReceiver_));

			}

			public override void installPublishedPrimitivesInClass(ESSymbol protocol, ESBehavior targetClass) {
				publishedPrimitivesDo((PrimitiveDomainType domain, String name, Delegate function) => {
					targetClass.addMethod(objectSpace.newMethod(SymbolRegistry.symbolFor(name), function));
				});
			}

		}
		
	}

	public class SymbolIdentityComparator : IdentityComparator<ESSymbol> {

	}

	public class SymbolRegistry {
		
		protected static readonly String	empty				= "";
		protected static readonly char[]	emptyChars			= ESString.empty;
		protected static readonly char		pathElementSeparatorChar	= '.';

		public static char? PathElementSeparatorChar {
			get {return pathElementSeparatorChar;}
		}
		
		protected ESClass			symbolClass;
		protected Dictionary<String, ESSymbol>	symbols				= new Dictionary<String, ESSymbol>();
		protected ESSymbol			doesNotUnderstandSelector;

		public SymbolRegistry(ESClass symbolClass) {
			this.symbolClass = symbolClass;
			doesNotUnderstandSelector = symbolFor("doesNotUnderstand:");
			assignCanonicalSemanticsToSelectors();
		}
	
		public ESClass SymbolClass {
			get {return symbolClass;}
		}

		public ESSymbol DoesNotUnderstandSelector {
			get {return doesNotUnderstandSelector;}
		}
		
		public ESSymbol symbolFor(char[] charArray) {
			return symbolFor(new String(charArray));
		}
		
		public ESSymbol symbolFor(String value) {
			return symbolFor(value, PathElementSeparatorChar);
		}
		
		public ESSymbol symbolForVariableOrParameterName(String value) {
			return symbolFor(value, null);
		}
		
		public ESSymbol symbolForFilenameEncodedString(String filename) {
			return symbolFor(filename.decodedFromFilename());
		}
				
		public ESSymbol symbolFor(String value, char? qualifiedNameSeparatorChar) {
			// For general use
			ESSymbol symbol = null;
			if (symbols.TryGetValue(value, out symbol)) return symbol;
			long numArgs;
			long pathElementCount;
			SymbolType type;
			value.classifySymbol(qualifiedNameSeparatorChar, out type, out numArgs, out pathElementCount);
			return symbolFor(value, type, numArgs, qualifiedNameSeparatorChar, pathElementCount);
		}
		
		public ESSymbol symbolFor(String value, SymbolType type, long numArgs, char? qualifiedNameSeparatorChar, long pathElementCount) {
			// For use by the compiler and run time system
			value = String.IsNullOrEmpty(value) ? empty : String.Intern(value);
			ESSymbol symbol = null;
			if (symbols.TryGetValue(value, out symbol)) return symbol;
			symbol = new ESSymbol(symbolClass, value, type, numArgs, qualifiedNameSeparatorChar ?? pathElementSeparatorChar, pathElementCount);
			symbols[value] = symbol;
			return symbol;
		}

		#region Canonical Selector Semantics

		public virtual void assignCanonicalSemanticsToSelectors() {

			// Note: Generally, nothing enforces the "canonical" semantics of a Smalltalk message. Message semantics are all based on convention, and programmers are mostly free to use message selectors to mean whatever they wish.
			// HOWEVER, some Smalltalk implementations do enforce message selector semantics for a few special selectors. The most common ones are #==, #~~, #ifTrue:, #ifFalse:, #ifTrue:ifFalse, #isNil and #notNil.
			//
			// Essence# forces the semantics of #==, #~~, #isNil, #isNotNil, #not, #and: and #or: (in that those 'message sends' are ALWAYS inlined.) In the case of #not, #and: and #or:, the receivers must be Booleans at runtime.
			// Additionally, Essence# will inline (and thus force the semantics of) the following 'message sends,' provided the message arguments, and sometimes also the message receiver, are lexically block literals: 
			//
			//	#ifNil:, #ifNotNil:, #ifNil:ifNotNil:, #ifNotNil:ifNil:,	(the message receiver can be of any type or class)
			//	#whileNil, #whileNotNil, #whileNil:, #whileNotNil:,		(the message receiver must lexically be a block literal)
			//	#ifTrue:, #ifFalse:, #ifTrue:ifFalse:, #ifFalse:ifTrue:,	(the message receiver must be a Boolean at runtime)
			//	#whileTrue, #whileFalse, #whileTrue:, #whileFalse:		(the message receiver must lexically be a block literal, and must evaluate to a Boolean at runtime)
			//
			// If the receiver of any of the messages that require a Boolean receiver is not in fact a Boolean at runtime, a MustBeBoolean exception will be raised.
			//
			// A MustBeBoolean exception will also be raised if any block that receives one of the messages #whileTrue, #whileFalse, #whileTrue: or #whileFalse: does not evaluate to a Boolean.

			// Smalltalk selector characters				Semantics enumeration constant				Semantics in C# (using LINQ expressions, if possible)
			//															'x' is always the message receiver; y is the first argument, etc.

			doesNotUnderstandSelector.CanonicalSemantics			= CanonicalSelectorSemantics.DoesNotUnderstand;		// (x, y) => ..untranslatable..
			symbolFor("==").CanonicalSemantics				= CanonicalSelectorSemantics.IsIdenticalTo;		// (x, y) => ReferenceEquals(x, y)
			symbolFor("~~").CanonicalSemantics				= CanonicalSelectorSemantics.IsNotIdenticalTo;		// (x, y) => !ReferenceEquals(x, y)
			symbolFor("identityHash").CanonicalSemantics			= CanonicalSelectorSemantics.IdentityHash;		// (x)    => x.GetHashCode() -- but the hash code is based solely on the object's identity, and not based on its logical value
			symbolFor("=").CanonicalSemantics				= CanonicalSelectorSemantics.IsEqualTo;			// (x, y) => x.Equals(y)
			symbolFor("~=").CanonicalSemantics				= CanonicalSelectorSemantics.IsNotEqualTo;		// (x, y) => !x.Equals(y)
			symbolFor("hash").CanonicalSemantics				= CanonicalSelectorSemantics.Hash;			// (x)    => x.GetHashCode()
			symbolFor("class").CanonicalSemantics				= CanonicalSelectorSemantics.Class;			// (x)    => x.Class -- All CLR objects have an Essence# class (which will be created dynamically when and as necessary)
			symbolFor("isMemberOf:").CanonicalSemantics			= CanonicalSelectorSemantics.IsMemberOf;		// (object, class) => object.Class == class
			symbolFor("isKindOf:").CanonicalSemantics			= CanonicalSelectorSemantics.IsKindOf;			// (object, class) => object is class
			symbolFor("asBehavior").CanonicalSemantics			= CanonicalSelectorSemantics.AsBehavior;		// (x)    => x is ESBehavior ? x : (x is Type ? esKernel.classForHostSystemType(x): Type.GetType(x).Class)
			symbolFor("asClass").CanonicalSemantics				= CanonicalSelectorSemantics.AsClass;			// (x)    => x.asBehavior.IsMetaclass ? (x.CanonicalInstance : x) -- an Essence# Metaclass has but one instance
			symbolFor("asMetaclass").CanonicalSemantics			= CanonicalSelectorSemantics.AsClass;			// (x)    => x.asBehavior.IsMetaclass ? (x : x.Class)
			symbolFor("isNil").CanonicalSemantics				= CanonicalSelectorSemantics.IsNil;			// (x)    => ReferenceEquals(x, null)
			symbolFor("notNil").CanonicalSemantics				= CanonicalSelectorSemantics.IsNotNil;			// (x)    => !ReferenceEquals(x, null)
			symbolFor("isBoolean").CanonicalSemantics			= CanonicalSelectorSemantics.IsBoolean;			// (x)    => x is bool
			symbolFor("isTrue").CanonicalSemantics				= CanonicalSelectorSemantics.IsTrue;			// (x)    => x is bool ? (bool)x : false	
			symbolFor("isFalse").CanonicalSemantics				= CanonicalSelectorSemantics.IsFalse;			// (x)    => x is bool ? !(bool)x : false
			symbolFor("isImmutable").CanonicalSemantics			= CanonicalSelectorSemantics.IsImmutable;		// No specific equivalent, but "x.IsReadOnly" essentially expresses the concept
			symbolFor("yourself").CanonicalSemantics			= CanonicalSelectorSemantics.Yourself;			// (x)    => x -- The "identity" message. There's a good reason for it...
			symbolFor("shallowCopy").CanonicalSemantics			= CanonicalSelectorSemantics.ShallowCopy;		// (x)    => x.MemberwiseClone()
			symbolFor("copy").CanonicalSemantics				= CanonicalSelectorSemantics.Copy;			// (x)    => x.Clone()
			symbolFor("coerce:").CanonicalSemantics				= CanonicalSelectorSemantics.Coerce;			// (x, y) => Convert.ChangeType(y, x.GetType()) 
			symbolFor("asImmutable").CanonicalSemantics			= CanonicalSelectorSemantics.AsImmutable;		// (x)    => x.IsImmuable ? x : x.Clone().BeImmutable()
			symbolFor("asMutable").CanonicalSemantics			= CanonicalSelectorSemantics.AsMutable;			// (x)    => x.IsImmuable ? x.Clone() : x -- Cloned objects are initially mutable.
			symbolFor("asCharacter").CanonicalSemantics			= CanonicalSelectorSemantics.AsCharacter;		// (x)    => (char)x
			symbolFor("asInteger").CanonicalSemantics			= CanonicalSelectorSemantics.AsInteger;			// (x)    => (integer)x -- where 'integer' is a hypothetical supertype of all of the integer value types and also of BigInteger
			symbolFor("asFloat").CanonicalSemantics				= CanonicalSelectorSemantics.AsFloat;			// (x)    => (float)x
			symbolFor("asDouble").CanonicalSemantics			= CanonicalSelectorSemantics.AsDouble;			// (x)    => (double)x
			symbolFor("asQuad").CanonicalSemantics				= CanonicalSelectorSemantics.AsQuad;			// (x)    => (decimal)x -- but only in Essence Sharp 
			symbolFor("asString").CanonicalSemantics			= CanonicalSelectorSemantics.AsString;			// (x)    => x is String ? x : (x is char[] ? x : x.ToCharArray()) -- Smalltalk Strings may be either mutable or immutable
			symbolFor("asSymbol").CanonicalSemantics			= CanonicalSelectorSemantics.AsSymbol;			// (x)    => x is String ? x : (x is char[] ? new String(x) : new String(x.ToCharArray())) -- But note that a Smalltalk Symbol is more than just a String. It includes MethodInfo-like and FieldInfo-like behavior...
			symbolFor("->").CanonicalSemantics				= CanonicalSelectorSemantics.AsAssociationTo;		// (x, y) => new KeyValuePair<Object, Object>(x, y)
			symbolFor("instVarAtName:").CanonicalSemantics			= CanonicalSelectorSemantics.InstVarValueAtName;	// (x, y) => x.GetType().InvokeMember(x.GetType().GetField(y), getFieldBindingFlags, binder, x, new Object[0])
			symbolFor("instVarAtName:put:").CanonicalSemantics		= CanonicalSelectorSemantics.InstVarValueAtNamePut;	// (x, y, z) => x.GetType().InvokeMember(x.GetType().GetField(y), setFieldBindingFlags, binder, x, new object[] {z})
			symbolFor("new").CanonicalSemantics				= CanonicalSelectorSemantics.New;			// (x)    => new x() -- x must be a class
			symbolFor("new:").CanonicalSemantics				= CanonicalSelectorSemantics.NewWithSize;		// (x, y) => new x(y) -- x must be a class, y is probably a size/capacity (but doesn't have to be)
			symbolFor("size").CanonicalSemantics				= CanonicalSelectorSemantics.Size;			// (x) => x is Array ? x.Length : (x is Collection ? x.Count : 0)
			symbolFor("at:").CanonicalSemantics				= CanonicalSelectorSemantics.At;			// (x, y) => x[y] -- Array or Collection
			symbolFor("at:ifAbsent:").CanonicalSemantics			= CanonicalSelectorSemantics.AtIfAbsent;		// (x, y, z) => x.TryGetValue(y, out value) ? value : z()
			symbolFor("at:put:").CanonicalSemantics				= CanonicalSelectorSemantics.AtPut;			// (x, y, z) => x[y] = z
			symbolFor("at:ifAbsentPut:").CanonicalSemantics			= CanonicalSelectorSemantics.AtIfAbsentPut;		// (x, y, z) => x.ContainsKey(y) ? x[y] : x[y] = z()
			symbolFor("remove:").CanonicalSemantics				= CanonicalSelectorSemantics.Remove;			// (x, y) => x.Remove(y)		-- remove the value, NOT the key!
			symbolFor("remove:ifAbsent:").CanonicalSemantics		= CanonicalSelectorSemantics.RemoveIfAbsent;		// (x, y, z) => if (!x.Remove(y)) z()	-- remove the value, NOT the key!
			symbolFor("removeAt:").CanonicalSemantics			= CanonicalSelectorSemantics.RemoveAt;			// (x, y) => x.RemoveAt(y)
			symbolFor("removeAtIndex:").CanonicalSemantics			= CanonicalSelectorSemantics.RemoveAt;			// (x, y) => x.RemoveAt(y)
			symbolFor("removeKey:").CanonicalSemantics			= CanonicalSelectorSemantics.RemoveKey;			// (x, y) => x.Remove(y)
			symbolFor("removeKey:ifAbsent:").CanonicalSemantics		= CanonicalSelectorSemantics.RemoveKeyIfAbsent;		// (x, y, z) => if (!x.Remove(y)) z()
			symbolFor("ifNil:").CanonicalSemantics				= CanonicalSelectorSemantics.IfNil;			// (x, y) => x == null ? y() : x	-- Or alternatively, (x) => x ?? y()
			symbolFor("ifNotNil:").CanonicalSemantics			= CanonicalSelectorSemantics.IfNotNil;			// (x, y) => x != null ? y() : x	-- Note that the result could be null
			symbolFor("ifNil:ifNotNil:").CanonicalSemantics			= CanonicalSelectorSemantics.IfNilIfNotNil;		// (x, y, z) => x == null ? y() : z()
			symbolFor("ifNotNil:ifNil:").CanonicalSemantics			= CanonicalSelectorSemantics.IfNotNilIfNil;		// (x, y, z) => x != null ? y() : z()
			symbolFor("not").CanonicalSemantics				= CanonicalSelectorSemantics.LogicalNot;		// (x)    => !(bool)x
			symbolFor("&").CanonicalSemantics				= CanonicalSelectorSemantics.LogicalAnd;		// (x, y) => (bool)x & (bool)y
			symbolFor("|").CanonicalSemantics				= CanonicalSelectorSemantics.LogicalOr;			// (x, y) => (bool)x | (bool)y
			symbolFor("xor:").CanonicalSemantics				= CanonicalSelectorSemantics.LogicalXor;		// (x, y) => (bool)x ^ (bool)y
			symbolFor("%").CanonicalSemantics				= CanonicalSelectorSemantics.LogicalXor;		// (x, y) => (bool)x ^ (bool)y
			symbolFor("and:").CanonicalSemantics				= CanonicalSelectorSemantics.ConditionalAnd;		// (x, y) => (bool)x && (bool)y()
			symbolFor("or:").CanonicalSemantics				= CanonicalSelectorSemantics.ConditionalOr;		// (x, y) => (bool)x || (bool)y()
			symbolFor("ifTrue:").CanonicalSemantics				= CanonicalSelectorSemantics.IfTrue;			// (x, y) => (bool)x ? y() : null
			symbolFor("ifFalse:").CanonicalSemantics			= CanonicalSelectorSemantics.IfFalse;			// (x, y) => (bool)x ? null : y()
			symbolFor("ifTrue:ifFalse:").CanonicalSemantics			= CanonicalSelectorSemantics.IfTrueIfFalse;		// (x, y, z) => (bool)x ? y() : z()
			symbolFor("ifFalse:ifTrue:").CanonicalSemantics			= CanonicalSelectorSemantics.IfFalseIfTrue;		// (x, y, z) => (bool)x ? z() : y()
			symbolFor("timesRepeat:").CanonicalSemantics			= CanonicalSelectorSemantics.TimesRepeat;		// (x, y) => for (var i = 0; i < x; i++) y(); x
			symbolFor("whileNil").CanonicalSemantics			= CanonicalSelectorSemantics.WhileNil;			// (x)    => while (x() == null); x
			symbolFor("whileNotNil").CanonicalSemantics			= CanonicalSelectorSemantics.WhileNotNil;		// (x)    => while (x() != null); x
			symbolFor("whileNil:").CanonicalSemantics			= CanonicalSelectorSemantics.WhileNilDo;		// (x, y) => while (x() == null) y(); x
			symbolFor("whileNotNil:").CanonicalSemantics			= CanonicalSelectorSemantics.WhileNotNilDo;		// (x, y) => while (x() != null) y(); x
			symbolFor("whileTrue").CanonicalSemantics			= CanonicalSelectorSemantics.WhileTrue;			// (x)    => while (x()); x
			symbolFor("whileFalse").CanonicalSemantics			= CanonicalSelectorSemantics.WhileFalse;		// (x)    => while (!x()); x
			symbolFor("whileTrue:").CanonicalSemantics			= CanonicalSelectorSemantics.WhileTrueDo;		// (x, y) => while (x()) y(); x
			symbolFor("whileFalse:").CanonicalSemantics			= CanonicalSelectorSemantics.WhileFalseDo;		// (x, y) => while (!x()) y(); x
			symbolFor("<").CanonicalSemantics				= CanonicalSelectorSemantics.IsLessThan;		// (x, y) => x < y -- x and y do not have to be numbers
			symbolFor(">").CanonicalSemantics				= CanonicalSelectorSemantics.IsGreaterThan;		// (x, y) => x > y -- x and y do not have to be numbers
			symbolFor("<=").CanonicalSemantics				= CanonicalSelectorSemantics.IsLessThanOrEqual;		// (x, y) => x <= y -- x and y do not have to be numbers
			symbolFor(">=").CanonicalSemantics				= CanonicalSelectorSemantics.IsGreaterThanOrEqual;	// (x, y) => x >= y -- x and y do not have to be numbers
			symbolFor("compareTo:").CanonicalSemantics			= CanonicalSelectorSemantics.CompareTo;			// (x, y) => x.CompareTo(y)
			symbolFor("isZero").CanonicalSemantics				= CanonicalSelectorSemantics.IsZero;			// (x)    => x == 0
			symbolFor("positive").CanonicalSemantics			= CanonicalSelectorSemantics.Positive;			// (x)    => x >= 0
			symbolFor("strictlyPositive").CanonicalSemantics		= CanonicalSelectorSemantics.StrictlyPositive;		// (x)    => x > 0
			symbolFor("negative").CanonicalSemantics			= CanonicalSelectorSemantics.Negative;			// (x)    => x < 0
			symbolFor("negated").CanonicalSemantics				= CanonicalSelectorSemantics.Negated;			// (x)    => -x
			symbolFor("reciprocal").CanonicalSemantics			= CanonicalSelectorSemantics.Reciprocal;		// (x)    => 1 / x
			symbolFor("sign").CanonicalSemantics				= CanonicalSelectorSemantics.Sign;			// (x)    => x > 0 ? 1 : (x < 0 ? -1 : 0)
			symbolFor("abs").CanonicalSemantics				= CanonicalSelectorSemantics.Abs;			// (x)    => x >= 0 ? x : -x
			symbolFor("ceiling").CanonicalSemantics				= CanonicalSelectorSemantics.Ceiling;			// (x)    => Math.Ceil(x)
			symbolFor("floor").CanonicalSemantics				= CanonicalSelectorSemantics.Floor;			// (x)    => Math.Floor(x)
			symbolFor("rounded").CanonicalSemantics				= CanonicalSelectorSemantics.Rounded;			// (x)    => Math.Round(x)
			symbolFor("truncated").CanonicalSemantics			= CanonicalSelectorSemantics.Truncated;			// (x)    => x is BigInteger ? x : (long)x
			symbolFor("roundTo:").CanonicalSemantics			= CanonicalSelectorSemantics.RoundTo;			// (x, y) => Math.Round(x / y) * y
			symbolFor("truncateTo:").CanonicalSemantics			= CanonicalSelectorSemantics.TruncateTo;		// (x, y) => ((long)x / y) * y
			symbolFor("+").CanonicalSemantics				= CanonicalSelectorSemantics.Plus;			// (x, y) => x + y
			symbolFor("-").CanonicalSemantics				= CanonicalSelectorSemantics.Minus;			// (x, y) => x - y
			symbolFor("*").CanonicalSemantics				= CanonicalSelectorSemantics.Times;			// (x, y) => x * y
			symbolFor(@"/").CanonicalSemantics				= CanonicalSelectorSemantics.DivideToRational;		// (x, y) => x is integer && y is integer ? new Fraction(x, y) : x / y
			symbolFor("quo:").CanonicalSemantics				= CanonicalSelectorSemantics.DivideToInteger;		// (x, y) => (long)(x / y) -- This is called 'div' in some languages.
			symbolFor(@"//").CanonicalSemantics				= CanonicalSelectorSemantics.DivideFlooredToInteger;	// (x, y) => (long)Math.Floor((double)x / y)
			symbolFor("rem:").CanonicalSemantics				= CanonicalSelectorSemantics.DivisionRemainder;		// (x, y) => x % y
			symbolFor(@"\\").CanonicalSemantics				= CanonicalSelectorSemantics.DivisionResidual;		// (x, y) => x - ((long)Math.Floor((double)x / y) * y)
			symbolFor("raisedToInteger:").CanonicalSemantics		= CanonicalSelectorSemantics.RaisedToInteger;		// (x, y) => var result = 1; for (var i = 0; i < y; i++) result *= x; result
			symbolFor("raisedTo:").CanonicalSemantics			= CanonicalSelectorSemantics.RaisedTo;			// (x, y) => Math.Pow(x, y)
			symbolFor("**").CanonicalSemantics				= CanonicalSelectorSemantics.RaisedTo;			// (x, y) => Math.Pow(x, y)
			symbolFor("sqrt").CanonicalSemantics				= CanonicalSelectorSemantics.SquareRoot;		// (x)    => Math.Pow(x, 1 / 2.0)
			symbolFor("log:").CanonicalSemantics				= CanonicalSelectorSemantics.Log;			// (x, y) => Math.Log(x, y)
			symbolFor("ln").CanonicalSemantics				= CanonicalSelectorSemantics.NaturalLog;		// (x) => Math.Log(x, Math.E)
			symbolFor("bitShift:").CanonicalSemantics			= CanonicalSelectorSemantics.BitShift;			// (x, y) => y >= 0 ? x << y : x >> -y
			symbolFor("<<").CanonicalSemantics				= CanonicalSelectorSemantics.ShiftLeft;			// (x, y) => x << y
			symbolFor(">>").CanonicalSemantics				= CanonicalSelectorSemantics.ShiftRight;		// (x, y) => x >> y
			symbolFor("to:do:").CanonicalSemantics				= CanonicalSelectorSemantics.ToDo;			// (x, y, z) => for (var i = x; i <= y; i++) z(i); x;
			symbolFor("to:by:do:").CanonicalSemantics			= CanonicalSelectorSemantics.ToByDo;			// (x, y, z, f) => for (var i = x; i <= y; i += z) f(i); x;
			symbolFor("do:").CanonicalSemantics				= CanonicalSelectorSemantics.Do;			// (x, y) => if (x is IDictionary) {foreach (var keyValuePair in x) y(keyValuePair.Value); } else {foreach (var element in x) y(element);} x;
			symbolFor("from:to:do:").CanonicalSemantics			= CanonicalSelectorSemantics.FromToDo;			// (x, y, z, f) => for (var i = y; i <= z; i++) f(x[i]); x;
			symbolFor("from:to:by:do:").CanonicalSemantics			= CanonicalSelectorSemantics.FromToByDo;		// (x, y, z, s, f) => for (var i = y; i <= z; i += s) f(x[i]); x;
			symbolFor("keysDo:").CanonicalSemantics				= CanonicalSelectorSemantics.KeysDo;			// (x, y) => foreach (var keyValuePair in x) y(keyValuePair.Key); x;
			symbolFor("associationsDo:").CanonicalSemantics			= CanonicalSelectorSemantics.AssociationsDo;		// (x, y) => foreach (var keyValuePair in x) y(keyValuePair); x;
			symbolFor("keysAndValuesDo:").CanonicalSemantics		= CanonicalSelectorSemantics.KeysAndValuesDo;		// (x, y) => foreach (var keyValuePair in x) y(keyValuePair.Key, keyValuePair.Value); x;
			symbolFor(",").CanonicalSemantics				= CanonicalSelectorSemantics.Concat;			// (x, y) => Concat(x, y)

			symbolFor("on:do:").CanonicalSemantics				= CanonicalSelectorSemantics.OnDo;			// (x, y, z) => try {x()} catch(y) {z();}
			symbolFor("ensure:").CanonicalSemantics				= CanonicalSelectorSemantics.Ensure;			// (x, y) => try {x()} finally {y();}
			symbolFor("ifCurtailed:").CanonicalSemantics			= CanonicalSelectorSemantics.IfCurtailed;		// (x, y) => try {x()} catch {y();}

			symbolFor("onEvent:do:").CanonicalSemantics			= CanonicalSelectorSemantics.OnEventDo;			// (x, y, z) => x.GetType().GetEvent(y).AddEventHandler(x, z);
			symbolFor("onEvent:doNotDo:").CanonicalSemantics		= CanonicalSelectorSemantics.OnEventDoNotDo;		// (x, y, z) => x.GetType().GetEvent(y).RemoveEventHandler(x, z);

			symbolFor("valueWithArguments:").CanonicalSemantics		= CanonicalSelectorSemantics.InvokeWithArguments;	// (x, y) => x(y[0], y[1]..y[y.Length - 1])
			symbolFor("value").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;			// (x) => x()
			symbolFor("value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;			// (x, a1) => x(a1)
			symbolFor("value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2) => x(a1, a2)
			symbolFor("value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3) => x(a1, a2, a3)
			symbolFor("value:value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3, a4) => x(a1, a2, a3, a4)
			symbolFor("value:value:value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3, a4, a5) => x(a1, a2, a3, a4, a5)
			symbolFor("value:value:value:value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3, a4, a5, a6) => x(a1, a2, a3, a4, a5, a6)
			symbolFor("value:value:value:value:value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3, a4, a5, a6, a7) => x(a1, a2, a3, a4, a5, a6, a7)
			symbolFor("value:value:value:value:value:value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3, a4, a5, a6, a7, a8) => x(a1, a2, a3, a4, a5, a6, a7, a8)
			symbolFor("value:value:value:value:value:value:value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3, a4, a5, a6, a7, a8, a9) => x(a1, a2, a3, a4, a5, a6, a7, a8, a9)
			symbolFor("value:value:value:value:value:value:value:value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) => x(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
			symbolFor("value:value:value:value:value:value:value:value:value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11) => x(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
			symbolFor("value:value:value:value:value:value:value:value:value:value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12) => x(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
			symbolFor("value:value:value:value:value:value:value:value:value:value:value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13) => x(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
			symbolFor("value:value:value:value:value:value:value:value:value:value:value:value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14) => x(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
			symbolFor("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15) => x(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
			symbolFor("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16) => x(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)
			symbolFor("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17) => x(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)
			symbolFor("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18) => x(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)
			symbolFor("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) => x(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)
			symbolFor("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) => x(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)
			symbolFor("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21) => x(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)
			symbolFor("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22) => x(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22)
			symbolFor("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23) => x(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23)
			symbolFor("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24) => x(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24)
			symbolFor("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25) => x(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25)
			symbolFor("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26) => x(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26)
			symbolFor("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27) => x(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27)
			symbolFor("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28) => x(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28)
			symbolFor("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29) => x(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29)
			symbolFor("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30) => x(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30)
			symbolFor("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31) => x(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31)
			symbolFor("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:").CanonicalSemantics				= CanonicalSelectorSemantics.Invoke;		// (x, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32) => x(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32)

			symbolFor("perform:withArguments:").CanonicalSemantics		= CanonicalSelectorSemantics.PerformWithArguments;	// (x, y, z) => x.GetType().InvokeMember(y, bindingFlags, binder, x, (Object[])z)
			symbolFor("perform:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;			// (x, y) => x.GetType().InvokeMember(y, bindingFlags, binder, x, new Object[0])
			symbolFor("perform:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;			// (x, y, z) => x.GetType().InvokeMember(y, bindingFlags, binder, x, new object[] {z})
			symbolFor("perform:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:with:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:with:with:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:with:with:with:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:with:with:with:with:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:with:with:with:with:with:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:with:with:with:with:with:with:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:with:with:with:with:with:with:with:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:with:with:with:with:with:with:with:with:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;
			symbolFor("perform:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:with:").CanonicalSemantics			= CanonicalSelectorSemantics.Perform;

		}

		#endregion
	
	}

}




