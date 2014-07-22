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
using System.IO;
using System.Collections.Generic;
using System.Reflection;
#if CLR2
using FuncNs = Microsoft.Scripting.Utils;
#else
using FuncNs = System;
#endif
using EssenceSharp.UtilityServices;
using EssenceSharp.Exceptions.System.PrimitiveFailures;
#endregion

namespace EssenceSharp.Runtime {

	public abstract class PrimitiveDomain {
 
		public static bool asBoolean(Object value) {
			if (value is bool) return (bool)value;
			throw new MustBeBoolean();
		}

		public static FuncNs.Func<Object> asFunctor0(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (FuncNs.Func<Object>)value : esValue.F0;
		}

		public static FuncNs.Func<Object, Object> asFunctor1(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (FuncNs.Func<Object, Object>)value : esValue.F1;
		}

		public static FuncNs.Func<Object, Object, Object> asFunctor2(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (FuncNs.Func<Object, Object, Object>)value : esValue.F2;
		}

		protected ESObjectSpace				objectSpace			= null;
		protected SymbolRegistry			symbolRegistry		= null;
		protected ESBehavior				domainClass		= null;
		protected Dictionary<String, Delegate>		primitiveRegistry	= new Dictionary<String, Delegate>();

		public abstract PrimitiveDomainType Type {
			get;
		}

		protected virtual void bindToObjectSpace() {
		}

		protected virtual void unbindFromObjectSpace() {
		}

		public ESObjectSpace ObjectSpace {
			get {return objectSpace;}

			internal 
			set {	if (objectSpace == value) return;
				if (objectSpace !=null) {
					unbindFromObjectSpace();
					symbolRegistry = null;
				}
				objectSpace = value;
				if (objectSpace != null) {
					symbolRegistry = objectSpace.SymbolRegistry;
					bindToObjectSpace();
				}}

		}

		public SymbolRegistry SymbolRegistry {
			get {return symbolRegistry;}
		}

		public ESBehavior DomainClass {
			get {return domainClass;}
		}

		public ObjectStateArchitecture ClassArchitecture {
			get {return DomainClass.Architecture;}
		}

		public ObjectStateArchitecture InstanceArchitecture {
			get {return DomainClass.InstanceArchitecture;}
		}

		public ESSymbol ClassName {
			get {return DomainClass.Name;}
		}

		public virtual ESPathname Pathname {
			get {return DomainClass.pathname();}
		}

		public ESNamespace Environment {
			get {return DomainClass.Environment;}
		}

		public void publishPrimitive(String primitiveName, Delegate function) {
			primitiveRegistry[primitiveName] = function;
		}

		public bool getPrimitiveFunction(String primitiveName, out Delegate function) {
			return primitiveRegistry.TryGetValue(primitiveName, out function);
		}

		public ESMethod getPrimitiveMethod(String primitiveName, ESSymbol selector) {
			Delegate function;
			if (!getPrimitiveFunction(primitiveName, out function)) {
				return null;
			}
			return objectSpace.newMethod(selector, function);
		}

		public virtual void publishCanonicalPrimitives() {
		}

		public void publishedPrimitivesDo(System.Action<PrimitiveDomainType, String, Delegate> enumerator3) {
			foreach(var selectorFunctionAssoc in primitiveRegistry) {
				var name = selectorFunctionAssoc.Key;
				var function = selectorFunctionAssoc.Value;
				enumerator3(Type, name, function);
			}
		}

		public void installPublishedPrimitivesInDomainClass(ESSymbol protocol) {
			installPublishedPrimitivesInClass(protocol, DomainClass);
		}

		public virtual void installPublishedPrimitivesInClass(ESSymbol protocol, ESBehavior targetClass) {
			publishedPrimitivesDo((PrimitiveDomainType domain, String name, Delegate function) => {
				targetClass.addMethod(objectSpace.newMethod(SymbolRegistry.symbolFor(name), function));
			});
		}

		public void generatePrimitiveMethodSource(DirectoryInfo basePath) {

			var folderPath = basePath.FullName;
			Pathname.elementsDo(name => folderPath = Path.Combine(folderPath, name));
			var filePath = Path.Combine(folderPath, "system.primitives");
			var methodsFile = new FileInfo(filePath);
				
			using (var writeStream = methodsFile.CreateText()) {
				writeStream.Write("\t\"Published system primitives for ");
				writeStream.Write(DomainClass.NameString);
				writeStream.WriteLine("\"");
				writeStream.WriteLine("\t\"**** Code generated in order to document built-in system primitives (which may or may not be used by any classes.) So, although the library loader ignores this file, DO NOT MODIFY IT! ****\"");
				publishedPrimitivesDo((PrimitiveDomainType domain, String name, Delegate function) => {
					writeStream.WriteLine("");
					writeStream.Write("\t");
					writeStream.WriteLine("protocol: #'system primitives' method:");
					writeStream.WriteLine("");
					writeStream.Write("\t[## ");
					var selector = symbolRegistry.symbolFor(name);
					MethodInfo method;
					ParameterInfo[] parameters;
					switch (selector.Type) {
						case SymbolType.Keyword:
							method = function.Method;
							parameters = method.GetParameters();
							var parameterIndex = 1;
							selector.keywordsDo(keyword => {
								writeStream.Write(keyword);
								writeStream.Write(": ");
								writeStream.Write(parameters[parameterIndex++].Name);
								writeStream.Write(" ");
							});
							break;
						case SymbolType.BinaryMessageSelector:
							method = function.Method;
							parameters = method.GetParameters();
							writeStream.Write(name);
							writeStream.Write(" ");
							writeStream.Write(parameters[1].Name);
							break;
						default:
							writeStream.Write(name);
							break;
					}
					writeStream.WriteLine("");
					writeStream.WriteLine("");
					writeStream.Write("\t\t<primitive: ");
					if (selector.Type != SymbolType.Identifier) writeStream.Write("#");
					writeStream.Write(name);
					writeStream.Write(" domain: ");
					writeStream.Write(Type.ToString());
					writeStream.WriteLine(">");
					writeStream.WriteLine("\t];");

				});
			}

		}

	}

	public class CLR_System_Exception_Primitives : PrimitiveDomain {

		protected override void bindToObjectSpace() {
			var typeName = new TypeName(typeof(System.Exception));
			domainClass = objectSpace.classForHostSystemType(typeName);
		}

		public override PrimitiveDomainType Type {
			get {return PrimitiveDomainType.CLR_System_Exception;}
		}

		#region Primitive Definitions

		public static Object _signal_(Object receiver) {
			throw ((Exception)receiver);
		}

		#endregion

		public override void publishCanonicalPrimitives() {
			publishPrimitive("signal",					new FuncNs.Func<Object, Object>(_signal_));
		}

	}

	public class CLR_System_Collections_List_1_Object_Primitives : PrimitiveDomain {

		protected override void bindToObjectSpace() {
			var typeName = new TypeName(typeof(System.Collections.Generic.List<Object>));
			domainClass = objectSpace.classForHostSystemType(typeName);
		}

		public override PrimitiveDomainType Type {
			get {return PrimitiveDomainType.CLR_System_Collections_Generic_List_1_Object;}
		}

		public override ESPathname Pathname {
			get {return objectSpace.pathnameFromString("Smalltalk.OrderedCollection");}
		}

		#region Primitive Definitions

		public static Object _contains_(Object receiver, Object predicateObject) {
			var list = (System.Collections.Generic.List<Object>)receiver;
			var predf = asFunctor1(predicateObject);
			return list.Exists(element => (bool)predf(element));
		}

		public static Object _detect_(Object receiver, Object predicateObject, Object ifAbsentActionObject) {
			var list = (System.Collections.Generic.List<Object>)receiver;
			int mySize = list.Count;
			var predf = asFunctor1(predicateObject);
			for (var i = 0; i < mySize; i++) {
				var element = list[i];
				if ((bool)predf(element)) return element;
			}
			return ifAbsentActionObject == null ? null : asFunctor0(ifAbsentActionObject)();
		}

		public static Object _findFirst_(Object receiver, Object predicateObject, Object startIndexObject, Object endIndexObject, Object ifAbsentActionObject) {
			var list = (System.Collections.Generic.List<Object>)receiver;
			if (list.Count == 0) return 0;
			var predf = asFunctor1(predicateObject);
			var startIndex = (long)startIndexObject - 1;
			var endIndex = (long)endIndexObject - 1;
			if (startIndex <= endIndex) {
				for (var i = startIndex; i <= endIndex; i++) {
					var element = list[(int)i];
					if ((bool)predf(element)) return (long)(i + 1);
				}
			} else {
				for (var i = startIndex; i >= endIndex; i--) {
					var element = list[(int)i];
					if ((bool)predf(element)) return (long)(i + 1);
				}
			}
			return ifAbsentActionObject == null ? null : asFunctor0(ifAbsentActionObject)();
		}

		public static Object _select_(Object receiver, Object predicateObject) {
			var list = (System.Collections.Generic.List<Object>)receiver;
			int mySize = list.Count;
			var selection = new List<Object>(mySize);
			var predf = asFunctor1(predicateObject);
			for (var i = 0; i < mySize; i++) {
				var element = list[i];
				if ((bool)predf(element)) selection.Add(element);
			}
			return selection;
		}

		public static Object _removeAllSuchThat_(Object receiver, Object predicateObject) {
			var list = (System.Collections.Generic.List<Object>)receiver;
			int mySize = list.Count;
			var predf = asFunctor1(predicateObject);
			list.RemoveAll(each => (bool)predf(each));
			return list;
		}

		#endregion

		public override void publishCanonicalPrimitives() {
			publishPrimitive("contains:",					new FuncNs.Func<Object, Object, Object>(_contains_));
			publishPrimitive("detect:ifNone:",				new FuncNs.Func<Object, Object, Object, Object>(_detect_));
			publishPrimitive("findFirst:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_findFirst_));
			publishPrimitive("select:",					new FuncNs.Func<Object, Object, Object>(_select_));
			publishPrimitive("removeAllSuchThat:",				new FuncNs.Func<Object, Object, Object>(_removeAllSuchThat_));
		}

	}

	public class UndefinedObjectPrimitives : PrimitiveDomain {

		protected override void bindToObjectSpace() {
			domainClass = objectSpace.UndefinedObjectClass;
		}

		public override PrimitiveDomainType Type {
			get {return PrimitiveDomainType.Nil;}
		}

		#region Primitive Definitions

		public Object _class_(Object receiver) {
			return domainClass;
		}
		
		public Object _isMemberOf_(Object receiver, Object aBehavior) {
			return ReferenceEquals(domainClass, aBehavior);
		}
		
		public Object _isKindOf_(Object receiver, Object aBehavior) {
			ESBehavior esClass;
			try {
				esClass = (ESBehavior)aBehavior;
			} catch (InvalidCastException) {
				throw new PrimInvalidOperandException();
			}
			return domainClass.includesBehavior(esClass);
		}
		
		public Object _hasSameValueAs_(Object receiver, Object comparand) {
			return ReferenceEquals(null, comparand);
		}
		
		public Object _hash_(Object receiver) {
			return -1;
		}

		public Object _ifNil_(Object receiver, Object nilAction) {
			return ESObject.asFunctor0(nilAction)();
		}

		public Object _ifNilIfNotNil_(Object receiver, Object nilAction, Object notNilAction) {
			return ESObject.asFunctor0(nilAction)();
		}

		public Object _ifNotNilIfNil_(Object receiver, Object notNilAction, Object nilAction) {
			return ESObject.asFunctor0(nilAction)();
		}

		#endregion

		public override void publishCanonicalPrimitives() {

			publishPrimitive("class",							new FuncNs.Func<Object, Object>(_class_));
			publishPrimitive("isMemberOf:",							new FuncNs.Func<Object, Object, Object>(_isMemberOf_));
			publishPrimitive("isKindOf:",							new FuncNs.Func<Object, Object, Object>(_isKindOf_));

			publishPrimitive("=",								new FuncNs.Func<Object, Object, Object>(_hasSameValueAs_));
			publishPrimitive("hash",							new FuncNs.Func<Object, Object>(_hash_));

			publishPrimitive("ifNil:",							new FuncNs.Func<Object, Object, Object>(_ifNil_));
			publishPrimitive("ifNil:ifNotNil:",						new FuncNs.Func<Object, Object, Object, Object>(_ifNilIfNotNil_));
			publishPrimitive("ifNotNil:ifNil:",						new FuncNs.Func<Object, Object, Object, Object>(_ifNotNilIfNil_));

		}

	}

	public class FalsePrimitives : PrimitiveDomain {

		protected override void bindToObjectSpace() {
			domainClass = objectSpace.FalseClass;
		}

		public override PrimitiveDomainType Type {
			get {return PrimitiveDomainType.False;}
		}
		
		#region Primitive Definitions

		public Object _class_(Object receiver) {
			return domainClass;
		}
		
		public Object _isMemberOf_(Object receiver, Object aBehavior) {
			return ReferenceEquals(domainClass, aBehavior);
		}

		public Object _isKindOf_(Object receiver, Object aBehavior) {
			ESBehavior esClass;
			try {
				esClass = (ESBehavior)aBehavior;
			} catch (InvalidCastException) {
				throw new PrimInvalidOperandException();
			}
			return domainClass.includesBehavior(esClass);
		}
		
		public Object _hasSameValueAs_(Object receiver, Object comparand) {
			return comparand is bool ? !(bool)comparand : false;
		}
		
		public Object _hash_(Object receiver) {
			return 0;
		}

		public Object _not_(Object receiver) {
			return true;
		}

		public Object _and_(Object receiver, Object operand) {
			return receiver;
		}

		public Object _andValueOf_(Object receiver, Object operand) {
			return receiver;
		}

		public Object _or_(Object receiver, Object operand) {
			return operand;
		}

		public Object _orValueOf_(Object receiver, Object operand) {
			return asFunctor0(operand)();
		}

		public Object _xor_(Object receiver, Object operand) {
			return operand;
		}

		public Object _ifFalse_(Object receiver, Object operand) {
			return asFunctor0(operand)();
		}

		public Object _ifTrue_(Object receiver, Object operand) {
			return null;
		}

		public Object _ifFalseIfTrue_(Object receiver, Object falseAction, Object trueAction) {
			return asFunctor0(falseAction)();
		}

		public Object _ifTrueIfFalse_(Object receiver, Object trueAction, Object falseAction) {
			return asFunctor0(falseAction)();
		}
		
		#endregion

		public override void publishCanonicalPrimitives() {

			publishPrimitive("class",							new FuncNs.Func<Object, Object>(_class_));
			publishPrimitive("isMemberOf:",							new FuncNs.Func<Object, Object, Object>(_isMemberOf_));
			publishPrimitive("isKindOf:",							new FuncNs.Func<Object, Object, Object>(_isKindOf_));

			publishPrimitive("=",								new FuncNs.Func<Object, Object, Object>(_hasSameValueAs_));
			publishPrimitive("hash",							new FuncNs.Func<Object, Object>(_hash_));

			publishPrimitive("not",								new FuncNs.Func<Object, Object>(_not_));
			publishPrimitive("&",								new FuncNs.Func<Object, Object, Object>(_and_));
			publishPrimitive("and:",							new FuncNs.Func<Object, Object, Object>(_andValueOf_));
			publishPrimitive("|",								new FuncNs.Func<Object, Object, Object>(_or_));
			publishPrimitive("or:",								new FuncNs.Func<Object, Object, Object>(_orValueOf_));
			publishPrimitive("xor:",							new FuncNs.Func<Object, Object, Object>(_xor_));
			publishPrimitive("ifFalse:",							new FuncNs.Func<Object, Object, Object>(_ifFalse_));
			publishPrimitive("ifTrue:",							new FuncNs.Func<Object, Object, Object>(_ifTrue_));
			publishPrimitive("ifFalse:ifTrue:",						new FuncNs.Func<Object, Object, Object, Object>(_ifFalseIfTrue_));
			publishPrimitive("ifTrue:ifFalse:",						new FuncNs.Func<Object, Object, Object, Object>(_ifTrueIfFalse_));

		}

	}

	public class TruePrimitives : PrimitiveDomain {

		protected override void bindToObjectSpace() {
			domainClass = objectSpace.TrueClass;
		}

		public override PrimitiveDomainType Type {
			get {return PrimitiveDomainType.True;}
		}
		
		#region Primitive Definitions

		public Object _class_(Object receiver) {
			return domainClass;
		}
		
		public Object _isMemberOf_(Object receiver, Object aBehavior) {
			return ReferenceEquals(domainClass, aBehavior);
		}
		
		public Object _isKindOf_(Object receiver, Object aBehavior) {
			ESBehavior esClass;
			try {
				esClass = (ESBehavior)aBehavior;
			} catch (InvalidCastException) {
				throw new PrimInvalidOperandException();
			}
			return domainClass.includesBehavior(esClass);
		}
		
		public Object _hasSameValueAs_(Object receiver, Object comparand) {
			return comparand is bool ? (bool)comparand : false;
		}
		
		public Object _hash_(Object receiver) {
			return 1;
		}

		public Object _not_(Object receiver) {
			return false;
		}

		public Object _and_(Object receiver, Object operand) {
			return operand;
		}

		public Object _andValueOf_(Object receiver, Object operand) {
			return asFunctor0(operand)();
		}

		public Object _or_(Object receiver, Object operand) {
			return receiver;
		}

		public Object _orValueOf_(Object receiver, Object operand) {
			return receiver;
		}

		public Object _xor_(Object receiver, Object operand) {
			return asBoolean(operand) ? false : receiver;
		}

		public Object _ifFalse_(Object receiver, Object operand) {
			return null;
		}

		public Object _ifTrue_(Object receiver, Object operand) {
			return asFunctor0(operand)();
		}

		public Object _ifFalseIfTrue_(Object receiver, Object falseAction, Object trueAction) {
			return asFunctor0(trueAction)();
		}

		public Object _ifTrueIfFalse_(Object receiver, Object trueAction, Object falseAction) {
			return asFunctor0(trueAction)();
		}
		
		#endregion

		public override void publishCanonicalPrimitives() {

			publishPrimitive("class",							new FuncNs.Func<Object, Object>(_class_));
			publishPrimitive("isMemberOf:",							new FuncNs.Func<Object, Object, Object>(_isMemberOf_));
			publishPrimitive("isKindOf:",							new FuncNs.Func<Object, Object, Object>(_isKindOf_));

			publishPrimitive("=",								new FuncNs.Func<Object, Object, Object>(_hasSameValueAs_));
			publishPrimitive("hash",							new FuncNs.Func<Object, Object>(_hash_));

			publishPrimitive("not",								new FuncNs.Func<Object, Object>(_not_));
			publishPrimitive("&",								new FuncNs.Func<Object, Object, Object>(_and_));
			publishPrimitive("and:",							new FuncNs.Func<Object, Object, Object>(_andValueOf_));
			publishPrimitive("|",								new FuncNs.Func<Object, Object, Object>(_or_));
			publishPrimitive("or:",								new FuncNs.Func<Object, Object, Object>(_orValueOf_));
			publishPrimitive("xor:",							new FuncNs.Func<Object, Object, Object>(_xor_));
			publishPrimitive("ifFalse:",							new FuncNs.Func<Object, Object, Object>(_ifFalse_));
			publishPrimitive("ifTrue:",							new FuncNs.Func<Object, Object, Object>(_ifTrue_));
			publishPrimitive("ifFalse:ifTrue:",						new FuncNs.Func<Object, Object, Object, Object>(_ifFalseIfTrue_));
			publishPrimitive("ifTrue:ifFalse:",						new FuncNs.Func<Object, Object, Object, Object>(_ifTrueIfFalse_));

		}

	}

	public class CharacterPrimitives : PrimitiveDomain {

		protected override void bindToObjectSpace() {
			domainClass = objectSpace.CharacterClass;
		}

		public override PrimitiveDomainType Type {
			get {return PrimitiveDomainType.Char;}
		}
		
		#region Primitive Definitions

		public Object _class_(Object receiver) {
			return domainClass;
		}
		
		public Object _isMemberOf_(Object receiver, Object aBehavior) {
			return ReferenceEquals(domainClass, aBehavior);
		}
		
		public Object _isKindOf_(Object receiver, Object aBehavior) {
			ESBehavior esClass;
			try {
				esClass = (ESBehavior)aBehavior;
			} catch (InvalidCastException) {
				throw new PrimInvalidOperandException();
			}
			return domainClass.includesBehavior(esClass);
		}
		
		public Object _hasSameValueAs_(Object receiver, Object comparand) {
			char chComparand;
			try {
				chComparand = (char)comparand;
			} catch (InvalidCastException) {
				throw new PrimInvalidOperandException();
			}
			return (char)receiver == chComparand;
		}
		
		public Object _hash_(Object receiver) {
			return (long)(char)receiver;
		}
		
		public Object _compareTo_(Object receiver, Object comparand) {
			try {
				return (long)Math.Sign((char)receiver - (char)comparand);
			} catch (Exception ex) {
				throw new PrimitiveFailException(ex);
			}
		}
		
		public Object _isLessThan_(Object receiver, Object comparand) {
			try {
				return (char)receiver < (char)comparand;
			} catch (Exception ex) {
				throw new PrimitiveFailException(ex);
			}
		}
		
		public Object _asInteger_(Object receiver) {
			return (long)(char)receiver;
		}

		#endregion

		public override void publishCanonicalPrimitives() {

			publishPrimitive("class",							new FuncNs.Func<Object, Object>(_class_));
			publishPrimitive("isMemberOf:",							new FuncNs.Func<Object, Object, Object>(_isMemberOf_));
			publishPrimitive("isKindOf:",							new FuncNs.Func<Object, Object, Object>(_isKindOf_));

			publishPrimitive("=",								new FuncNs.Func<Object, Object, Object>(_hasSameValueAs_));
			publishPrimitive("hash",							new FuncNs.Func<Object, Object>(_hash_));

			publishPrimitive("compareTo:",							new FuncNs.Func<Object, Object, Object>(_compareTo_));
			publishPrimitive("<",								new FuncNs.Func<Object, Object, Object>(_isLessThan_));

			publishPrimitive("asInteger",							new FuncNs.Func<Object, Object>(_asInteger_));

		}

	}

	public class SmallIntegerPrimitives : PrimitiveDomain {

		protected override void bindToObjectSpace() {
			domainClass = objectSpace.SmallIntegerClass;
		}

		public override PrimitiveDomainType Type {
			get {return PrimitiveDomainType.SmallInteger;}
		}
		
		#region Primitive Definitions

		public Object _class_(Object receiver) {
			return domainClass;
		}
		
		public Object _isMemberOf_(Object receiver, Object aBehavior) {
			return ReferenceEquals(domainClass, aBehavior);
		}
		
		public Object _isKindOf_(Object receiver, Object aBehavior) {
			ESBehavior esClass;
			try {
				esClass = (ESBehavior)aBehavior;
			} catch {
				throw new PrimInvalidOperandException();
			}
			return domainClass.includesBehavior(esClass);
		}
		
		public Object _hasSameValueAs_(Object receiver, Object comparand) {
			long numComparand;
			try {
				numComparand = (long)comparand;
			} catch {
				throw new PrimInvalidOperandException();
			}
			return (long)receiver == numComparand;
		}
		
		public Object _hash_(Object receiver) {
			return (long)receiver;
		}
	
		public Object _compareTo_(Object receiver, Object comparand) {
			long numComparand;
			try {
				numComparand = (long)comparand;
			} catch {
				throw new PrimInvalidOperandException("Comparand must be a SmallInteger");
			}
			return (long)Math.Sign((long)receiver - numComparand);
		}

		public Object _lessThan_(Object receiver, Object comparand) {
			long numComparand;
			try {
				numComparand = (long)comparand;
			} catch {
				throw new PrimInvalidOperandException("Comparand must be a SmallInteger");
			}
			return ((long)receiver - numComparand) < 0;
		}

		public Object _sign_(Object receiver) {
			var value = (long)receiver;
			if (value > 0L) return 1L;
			if (value < 0L) return -1L;
			return 0L;
		}

		public Object _isZero_(Object receiver) {
			return (long)receiver == 0;
		}

		public Object _positive_(Object receiver) {
			return (long)receiver >= 0;
		}

		public Object _negative_(Object receiver) {
			return (long)receiver < 0;
		}

		public Object _abs_(Object receiver) {
			long value = (long)receiver;
			return value >= 0 ? receiver : -value;
		}

		public Object _negated_(Object receiver) {
			return -(long)receiver;
		}
		
		public Object _sumWith_(Object receiver, Object operand) {
			long numOperand;
			try {
				numOperand = (long)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			return ((long)receiver) + numOperand;
		}
		
		public Object _differenceFrom_(Object receiver, Object operand) {
			long numOperand;
			try {
				numOperand = (long)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			return ((long)receiver) - numOperand;
		}
		
		public Object _productWith_(Object receiver, Object operand) {
			long numOperand;
			try {
				numOperand = (long)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			return ((long)receiver) * numOperand;
		}
		
		public Object _dividedBy_(Object receiver, Object operand) {
			long numOperand;
			try {
				numOperand = (long)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			return ((long)receiver) / (double)numOperand; // Result SHOULD be a Fraction, but those aren't implemented yet....
		}
		
		public Object _reciprocal_(Object receiver) {
			return 1.0 / (double)(long)receiver; // Result SHOULD be a Fraction, but those aren't implemented yet....
		}
		
		public Object _asCharacter_(Object receiver) {
			return (char)(long)receiver;
		}

		public Object _asFloat_(Object receiver) {
			return (float)(long)receiver;
		}

		public Object _asDouble_(Object receiver) {
			return (double)(long)receiver;
		}

		public Object _asQuad_(Object receiver) {
			return (decimal)(long)receiver;
		}

		public Object _roundTo_(Object receiver, Object operand) {
			// Smalltalk: 53 roundTo: 5 => 55; 1263.48 roundTo: 0.25 => 1263.50.
			// C-Lang: 53.roundTo(5) => 55; 1263.48.roundTo(0.25) => 1263.50.
			long modulus;
			try {
				modulus = (long)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			double self = (double)(long)receiver;
			return (long)(Math.Round(self / modulus) * modulus);
		}
		
		public Object _truncateTo_(Object receiver, Object modulus) {
			// Smalltalk: 53 truncateTo: 5 => 50; 1263.48 truncateTo: 0.25 => 1263.25.
			// C-Lang: 53.truncateTo(5) => 50; 1263.48.truncateTo(0.25) => 1263.25.
			long intModulus;
			try {
				intModulus = (long)modulus;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			double self = (long)receiver;
			return (self / intModulus) * intModulus;
		}
		
		public Object _dividedToIntegerBy_(Object receiver, Object operand) {
			long numOperand;
			try {
				numOperand = (long)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			return ((long)receiver) / numOperand; 
		}
		
		public Object _dividedAndFlooredToIntegerBy_(Object receiver, Object operand) {
			long longOperand;
			double doubleOperand;
			try {
				longOperand = (long)operand;
				doubleOperand = (double)longOperand;
			} catch {
				try {
					doubleOperand = (double)operand;
				} catch {
					throw new PrimInvalidOperandException("Operand must be a SmallInteger");
				}
			}
			return (long)Math.Floor((long)receiver / doubleOperand); 
		}
		
		public Object _remainderFromModulus_(Object receiver, Object operand) {
			// Implements standard keyword message "rem: operand" 
			// Smalltalk: ^receiver - ((receiver quo: operand) * operand)
			// C-Lang: return receiver - ((long)(receiver / operand) * operand); -- Or just "receiver % operand" :-)
			long numOperand;
			try {
				numOperand = (long)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			return (long)receiver % numOperand;
		}
		
		public Object _flooredRemainderFromModulus_(Object receiver, Object operand) {
			// Implements standard binary message "\\ operand" 
			// Smalltalk: ^receiver - (receiver // operand * operand)
			// C-Lang: return receiver - ((long)Math.Floor(receiver / (double)operand) * operand);
			long numOperand;
			try {
				numOperand = (long)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			long self = (long)receiver;
			return self - ((long)Math.Floor(self / (double)numOperand) * numOperand);
		}
		
		public Object _raisedTo_(Object receiver, Object operand) {
			long numOperand;
			try {
				numOperand = (long)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			return (long)Math.Pow((long)receiver, numOperand);
		}
		
		public Object _log_(Object receiver, Object operand) {
			long numOperand;
			try {
				numOperand = (long)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			return Math.Log((long)receiver, numOperand);
		}
		
		public Object _ln_(Object receiver) {
			return Math.Log((long)receiver, Math.E);
		}

		public Object _timesRepeat_(Object receiver, Object action) {
			var limit = (long)receiver;
			var actionFunction = ESObject.asFunctor0(action);
			for (var count = 0; count < limit; count++) {
				actionFunction();
			}
			return receiver;
		}

		public Object _toDo_(Object receiver, Object end, Object enumerator) {
			long primEnd;
			try {
				primEnd = (long)end;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			var enumerationFunction = ESObject.asFunctor1(enumerator);
			var primStart = (long)receiver;
			for (var inductionValue = primStart; inductionValue <= primEnd; inductionValue++) {
				enumerationFunction(inductionValue);
			}
			return receiver;
		}

		public Object _toByDo_(Object receiver, Object end, Object step, Object enumerator) {
			long primEnd;
			try {
				primEnd = (long)end;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			long primStep;
			try {
				primStep = (long)step;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			var enumerationFunction = ESObject.asFunctor1(enumerator);
			var primStart = (long)receiver;
			if (primStep > 0) {
				for (var inductionValue = primStart; inductionValue <= primEnd; inductionValue += primStep) {
					enumerationFunction(inductionValue);
				}
			} else if (primStep < 0) {
				for (var inductionValue = primStart; inductionValue >= primEnd; inductionValue -= primStep) {
					enumerationFunction(inductionValue);
				}
			} else {
				throw new PrimInvalidOperandException("Step value must not be zero");
			}
			return receiver;
		}

		public Object _bitAnd_(Object receiver, Object operand) {
			long numOperand;
			try {
				numOperand = (long)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			return ((long)receiver) & numOperand;
		}
				
		public Object _bitOr_(Object receiver, Object operand) {
			long numOperand;
			try {
				numOperand = (long)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			return ((long)receiver) | numOperand;
		}
				
		public Object _bitXor_(Object receiver, Object operand) {
			long numOperand;
			try {
				numOperand = (long)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			return ((long)receiver) ^ numOperand;
		}
				
		public Object _bitShift_(Object receiver, Object operand) {
			long numOperand;
			int shiftExtent;
			try {
				numOperand = (long)operand;
				shiftExtent = (int)numOperand;
			} catch {
				try {
					shiftExtent = (int)operand;
				} catch {
					throw new PrimInvalidOperandException("Operand must be a SmallInteger");
				}
			}
			return shiftExtent >= 0 ? ((long)receiver) << shiftExtent : ((long)receiver) >> -shiftExtent;
		}
				
		public Object _bitShiftLeft_(Object receiver, Object operand) {
			long numOperand;
			int shiftExtent;
			try {
				numOperand = (long)operand;
				shiftExtent = (int)numOperand;
			} catch {
				try {
					shiftExtent = (int)operand;
				} catch {
					throw new PrimInvalidOperandException("Operand must be a SmallInteger");
				}
			}
			return ((long)receiver) << shiftExtent;
		}
				
		public Object _bitShiftRight_(Object receiver, Object operand) {
			long numOperand;
			int shiftExtent;
			try {
				numOperand = (long)operand;
				shiftExtent = (int)numOperand;
			} catch {
				try {
					shiftExtent = (int)operand;
				} catch {
					throw new PrimInvalidOperandException("Operand must be a SmallInteger");
				}
			}
			return ((long)receiver) >> shiftExtent;
		}

		#endregion

		public override void publishCanonicalPrimitives() {

			publishPrimitive("class",						new FuncNs.Func<Object, Object>(_class_));
			publishPrimitive("isMemberOf:",						new FuncNs.Func<Object, Object, Object>(_isMemberOf_));
			publishPrimitive("isKindOf:",						new FuncNs.Func<Object, Object, Object>(_isKindOf_));

			publishPrimitive("=",							new FuncNs.Func<Object, Object, Object>(_hasSameValueAs_));
			publishPrimitive("hash",						new FuncNs.Func<Object, Object>(_hash_));

			publishPrimitive("compareTo:",						new FuncNs.Func<Object, Object, Object>(_compareTo_));
			publishPrimitive("<",							new FuncNs.Func<Object, Object, Object>(_lessThan_));

			publishPrimitive("asCharacter",						new FuncNs.Func<Object, Object>(_asCharacter_));
			publishPrimitive("asFloat",						new FuncNs.Func<Object, Object>(_asFloat_));
			publishPrimitive("asDouble",						new FuncNs.Func<Object, Object>(_asDouble_));
			publishPrimitive("asQuad",						new FuncNs.Func<Object, Object>(_asQuad_));

			publishPrimitive("sign",						new FuncNs.Func<Object, Object>(_sign_));
			publishPrimitive("isZero",						new FuncNs.Func<Object, Object>(_isZero_));
			publishPrimitive("positive",						new FuncNs.Func<Object, Object>(_positive_));
			publishPrimitive("negative",						new FuncNs.Func<Object, Object>(_negative_));
			publishPrimitive("abs",							new FuncNs.Func<Object, Object>(_abs_));
			publishPrimitive("negated",						new FuncNs.Func<Object, Object>(_negated_));

			publishPrimitive("+",							new FuncNs.Func<Object, Object, Object>(_sumWith_));
			publishPrimitive("-",							new FuncNs.Func<Object, Object, Object>(_differenceFrom_));
			publishPrimitive("*",							new FuncNs.Func<Object, Object, Object>(_productWith_));
			publishPrimitive("/",							new FuncNs.Func<Object, Object, Object>(_dividedBy_));
			publishPrimitive("reciprocal",						new FuncNs.Func<Object, Object>(_reciprocal_));

			publishPrimitive("roundTo:",						new FuncNs.Func<Object, Object, Object>(_roundTo_));
			publishPrimitive("truncateTo:",						new FuncNs.Func<Object, Object, Object>(_truncateTo_));
			publishPrimitive("quo:",						new FuncNs.Func<Object, Object, Object>(_dividedToIntegerBy_));
			publishPrimitive("//",							new FuncNs.Func<Object, Object, Object>(_dividedAndFlooredToIntegerBy_));
			publishPrimitive("rem:",						new FuncNs.Func<Object, Object, Object>(_remainderFromModulus_));
			publishPrimitive(@"\\",							new FuncNs.Func<Object, Object, Object>(_flooredRemainderFromModulus_));
			publishPrimitive("**",							new FuncNs.Func<Object, Object, Object>(_raisedTo_));
			publishPrimitive("log:",						new FuncNs.Func<Object, Object, Object>(_log_));
			publishPrimitive("ln",							new FuncNs.Func<Object, Object>(_ln_));

			publishPrimitive("timesRepeat:",					new FuncNs.Func<Object, Object, Object>(_timesRepeat_));
			publishPrimitive("to:do:",						new FuncNs.Func<Object, Object, Object, Object>(_toDo_));
			publishPrimitive("to:by:do:",						new FuncNs.Func<Object, Object, Object, Object, Object>(_toByDo_));

			publishPrimitive("bitAnd:",						new FuncNs.Func<Object, Object, Object>(_bitAnd_));
			publishPrimitive("bitOr:",						new FuncNs.Func<Object, Object, Object>(_bitOr_));
			publishPrimitive("bitXor:",						new FuncNs.Func<Object, Object, Object>(_bitXor_));
			
			publishPrimitive("bitShift:",		/* bitShift: */			new FuncNs.Func<Object, Object, Object>(_bitShift_));
			publishPrimitive("<<",							new FuncNs.Func<Object, Object, Object>(_bitShiftLeft_));
			publishPrimitive(">>",							new FuncNs.Func<Object, Object, Object>(_bitShiftRight_));

		}

	}

	public class SinglePrecisionPrimitives : PrimitiveDomain {

		protected override void bindToObjectSpace() {
			domainClass = objectSpace.FloatClass;
		}

		public override PrimitiveDomainType Type {
			get {return PrimitiveDomainType.SinglePrecision;}
		}

		#region Primitives

		public Object _class_(Object receiver) {
			return domainClass;
		}
		
		public Object _isMemberOf_(Object receiver, Object aBehavior) {
			return ReferenceEquals(domainClass, aBehavior);
		}
		
		public Object _isKindOf_(Object receiver, Object aBehavior) {
			ESBehavior esClass;
			try {
				esClass = (ESBehavior)aBehavior;
			} catch (InvalidCastException) {
				throw new PrimInvalidOperandException();
			}
			return domainClass.includesBehavior(esClass);
		}
		
		public Object _hasSameValueAs_(Object receiver, Object comparand) {
			float numComparand;
			try {
				numComparand = (float)comparand;
			} catch (InvalidCastException) {
				throw new PrimInvalidOperandException();
			}
			return (float)receiver == numComparand;
		}
		
		public Object _hash_(Object receiver) {
			return (long)(float)receiver;
		}
	
		public Object _compareTo_(Object receiver, Object comparand) {
			float numComparand;
			try {
				numComparand = (float)comparand;
			} catch {
				throw new PrimInvalidOperandException("Comparand must be a Float");
			}
			return (long)Math.Sign((float)receiver - numComparand);
		}

		public Object _lessThan_(Object receiver, Object comparand) {
			float numComparand;
			try {
				numComparand = (float)comparand;
			} catch {
				throw new PrimInvalidOperandException("Comparand must be a Float");
			}
			return ((float)receiver - numComparand) < 0;
		}

		public Object _sign_(Object receiver) {
			var value = (float)receiver;
			if (value > 0.0f) return 1L;
			if (value < 0.0f) return -1L;
			return 0L;
		}

		public Object _isZero_(Object receiver) {
			return (float)receiver == 0.0f;
		}

		public Object _positive_(Object receiver) {
			return (float)receiver >= 0.0f;
		}

		public Object _negative_(Object receiver) {
			return (float)receiver < 0.0f;
		}

		public Object _abs_(Object receiver) {
			float value = (float)receiver;
			return value >= 0.0f ? receiver : -value;
		}

		public Object _negated_(Object receiver) {
			return -(float)receiver;
		}
		
		public Object _sumWith_(Object receiver, Object operand) {
			float numOperand;
			try {
				numOperand = (float)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			return ((float)receiver) + numOperand;
		}
		
		public Object _differenceFrom_(Object receiver, Object operand) {
			float numOperand;
			try {
				numOperand = (float)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			return ((float)receiver) - numOperand;
		}
		
		public Object _productWith_(Object receiver, Object operand) {
			float numOperand;
			try {
				numOperand = (float)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			return ((float)receiver) * numOperand;
		}
		
		public Object _dividedBy_(Object receiver, Object operand) {
			float numOperand;
			try {
				numOperand = (float)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			return ((float)receiver) / numOperand; 
		}
		
		public Object _reciprocal_(Object receiver) {
			return 1.0f / (float)receiver; 
		}
		
		public Object _asCharacter_(Object receiver) {
			return (char)(uint)(float)receiver;
		}

		public Object _asInteger_(Object receiver) {
			return (long)(float)receiver;
		}

		public Object _asDouble_(Object receiver) {
			return (double)(float)receiver;
		}

		public Object _asQuad_(Object receiver) {
			return (decimal)(float)receiver;
		}

		public Object _ceiling_(Object receiver) {
			return (float)Math.Ceiling((float)receiver);
		}

		public Object _floor_(Object receiver) {
			return (float)Math.Floor((float)receiver);
		}

		public Object _rounded_(Object receiver) {
			return (float)Math.Round((float)receiver);
		}

		public Object _truncated_(Object receiver) {
			return (long)(float)receiver;
		}

		public Object _roundTo_(Object receiver, Object operand) {
			// Smalltalk: 53 roundTo: 5 => 55; 1263.48 roundTo: 0.25 => 1263.50.
			// C-Lang: 53.roundTo(5) => 55; 1263.48.roundTo(0.25) => 1263.50.
			float modulus;
			try {
				modulus = (float)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			float self = (float)receiver;
			return (float)(Math.Round(self / modulus) * modulus);
		}
		
		public Object _truncateTo_(Object receiver, Object modulus) {
			// Smalltalk: 53 truncateTo: 5 => 50; 1263.48 truncateTo: 0.25 => 1263.25.
			// C-Lang: 53.truncateTo(5) => 50; 1263.48.truncateTo(0.25) => 1263.25.
			float numModulus;
			try {
				numModulus = (float)modulus;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			double self = (float)receiver;
			return ((long)(self / numModulus)) * numModulus;
		}
		
		public Object _dividedToIntegerBy_(Object receiver, Object operand) {
			float numOperand;
			try {
				numOperand = (float)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			return (long)((float)receiver / numOperand); 
		}
		
		public Object _dividedAndFlooredToIntegerBy_(Object receiver, Object operand) {
			float numOperand;
			try {
				numOperand = (float)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			return (long)Math.Floor((float)receiver / numOperand); 
		}
		
		public Object _remainderFromModulus_(Object receiver, Object operand) {
			// Implements standard keyword message "rem: operand" 
			// Smalltalk: ^receiver - ((receiver quo: operand) * operand)
			// C-Lang: return receiver - ((long)(receiver / operand) * operand); -- Or just "receiver % operand" :-)
			float numOperand;
			try {
				numOperand = (float)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			return (float)receiver % numOperand;
		}
		
		public Object _flooredRemainderFromModulus_(Object receiver, Object operand) {
			// Implements standard binary message "\\ operand" 
			// Smalltalk: ^receiver - (receiver // operand * operand)
			// C-Lang: return receiver - ((long)Math.Floor(receiver / (double)operand) * operand);
			float numOperand;
			try {
				numOperand = (float)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			float self = (float)receiver;
			return self - ((long)Math.Floor(self / numOperand) * numOperand);
		}
		
		public Object _raisedTo_(Object receiver, Object operand) {
			float numOperand;
			try {
				numOperand = (float)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			return (float)Math.Pow((float)receiver, numOperand);
		}
		
		public Object _log_(Object receiver, Object operand) {
			float numOperand;
			try {
				numOperand = (float)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			return (float)Math.Log((float)receiver, numOperand);
		}
		
		public Object _ln_(Object receiver) {
			return (float)Math.Log((float)receiver, (float)Math.E);
		}

		public Object _toDo_(Object receiver, Object end, Object enumerator) {
			float primEnd;
			try {
				primEnd = (float)end;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			var enumerationFunction = ESObject.asFunctor1(enumerator);
			var primStart = (float)receiver;
			for (var inductionValue = primStart; inductionValue <= primEnd; inductionValue++) {
				enumerationFunction(inductionValue);
			}
			return receiver;
		}

		public Object _toByDo_(Object receiver, Object end, Object step, Object enumerator) {
			float primEnd;
			try {
				primEnd = (float)end;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			float primStep;
			try {
				primStep = (float)step;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			var enumerationFunction = ESObject.asFunctor1(enumerator);
			var primStart = (float)receiver;
			if (primStep > 0) {
				for (var inductionValue = primStart; inductionValue <= primEnd; inductionValue += primStep) {
					enumerationFunction(inductionValue);
				}
			} else if (primStep < 0) {
				for (var inductionValue = primStart; inductionValue >= primEnd; inductionValue -= primStep) {
					enumerationFunction(inductionValue);
				}
			} else {
				throw new PrimInvalidOperandException("Step value must not be zero");
			}
			return receiver;
		}

		#endregion

		public override void publishCanonicalPrimitives() {

			publishPrimitive("class",					new FuncNs.Func<Object, Object>(_class_));
			publishPrimitive("isMemberOf:",					new FuncNs.Func<Object, Object, Object>(_isMemberOf_));
			publishPrimitive("isKindOf:",					new FuncNs.Func<Object, Object, Object>(_isKindOf_));

			publishPrimitive("=",						new FuncNs.Func<Object, Object, Object>(_hasSameValueAs_));
			publishPrimitive("hash",					new FuncNs.Func<Object, Object>(_hash_));

			publishPrimitive("compareTo:",					new FuncNs.Func<Object, Object, Object>(_compareTo_));
			publishPrimitive("<",						new FuncNs.Func<Object, Object, Object>(_lessThan_));

			publishPrimitive("asCharacter",					new FuncNs.Func<Object, Object>(_asCharacter_));
			publishPrimitive("asInteger",					new FuncNs.Func<Object, Object>(_asInteger_));
			publishPrimitive("asDouble",					new FuncNs.Func<Object, Object>(_asDouble_));
			publishPrimitive("asQuad",					new FuncNs.Func<Object, Object>(_asQuad_));

			publishPrimitive("sign",					new FuncNs.Func<Object, Object>(_sign_));
			publishPrimitive("isZero",					new FuncNs.Func<Object, Object>(_isZero_));
			publishPrimitive("positive",					new FuncNs.Func<Object, Object>(_positive_));
			publishPrimitive("negative",					new FuncNs.Func<Object, Object>(_negative_));
			publishPrimitive("abs",						new FuncNs.Func<Object, Object>(_abs_));
			publishPrimitive("negated",					new FuncNs.Func<Object, Object>(_negated_));

			publishPrimitive("+",						new FuncNs.Func<Object, Object, Object>(_sumWith_));
			publishPrimitive("-",						new FuncNs.Func<Object, Object, Object>(_differenceFrom_));
			publishPrimitive("*",						new FuncNs.Func<Object, Object, Object>(_productWith_));
			publishPrimitive("/",						new FuncNs.Func<Object, Object, Object>(_dividedBy_));
			publishPrimitive("reciprocal",					new FuncNs.Func<Object, Object>(_reciprocal_));

			publishPrimitive("roundTo:",					new FuncNs.Func<Object, Object, Object>(_roundTo_));
			publishPrimitive("truncateTo:",					new FuncNs.Func<Object, Object, Object>(_truncateTo_));
			publishPrimitive("quo:",					new FuncNs.Func<Object, Object, Object>(_dividedToIntegerBy_));
			publishPrimitive("//",						new FuncNs.Func<Object, Object, Object>(_dividedAndFlooredToIntegerBy_));
			publishPrimitive("rem:",					new FuncNs.Func<Object, Object, Object>(_remainderFromModulus_));
			publishPrimitive(@"\\",						new FuncNs.Func<Object, Object, Object>(_flooredRemainderFromModulus_));
			publishPrimitive("**",						new FuncNs.Func<Object, Object, Object>(_raisedTo_));
			publishPrimitive("log:",					new FuncNs.Func<Object, Object, Object>(_log_));
			publishPrimitive("ln",						new FuncNs.Func<Object, Object>(_ln_));
			publishPrimitive("to:do:",					new FuncNs.Func<Object, Object, Object, Object>(_toDo_));
			publishPrimitive("to:by:do:",					new FuncNs.Func<Object, Object, Object, Object, Object>(_toByDo_));
		}

	}

	public class DoublePrecisionPrimitives : PrimitiveDomain {

		protected override void bindToObjectSpace() {
			domainClass = objectSpace.DoubleClass;
		}

		public override PrimitiveDomainType Type {
			get {return PrimitiveDomainType.DoublePrecision;}
		}

		#region Primitive Definitions

		public Object _class_(Object receiver) {
			return domainClass;
		}
		
		public Object _isMemberOf_(Object receiver, Object aBehavior) {
			return ReferenceEquals(domainClass, aBehavior);
		}
		
		public Object _isKindOf_(Object receiver, Object aBehavior) {
			ESBehavior esClass;
			try {
				esClass = (ESBehavior)aBehavior;
			} catch (InvalidCastException) {
				throw new PrimInvalidOperandException();
			}
			return domainClass.includesBehavior(esClass);
		}
		
		public Object _hasSameValueAs_(Object receiver, Object comparand) {
			double numComparand;
			try {
				numComparand = (double)comparand;
			} catch (InvalidCastException) {
				throw new PrimInvalidOperandException();
			}
			return (double)receiver == numComparand;
		}
		
		public Object _hash_(Object receiver) {
			return (long)(double)receiver;
		}
	
		public Object _compareTo_(Object receiver, Object comparand) {
			double numComparand;
			try {
				numComparand = (double)comparand;
			} catch {
				throw new PrimInvalidOperandException("Comparand must be a Double");
			}
			return (long)Math.Sign((double)receiver - numComparand);
		}

		public Object _lessThan_(Object receiver, Object comparand) {
			double numComparand;
			try {
				numComparand = (double)comparand;
			} catch {
				throw new PrimInvalidOperandException("Comparand must be a Double");
			}
			return ((double)receiver - numComparand) < 0;
		}

		public Object _sign_(Object receiver) {
			var value = (double)receiver;
			if (value > 0.0d) return 1L;
			if (value < 0.0d) return -1L;
			return 0L;
		}

		public Object _isZero_(Object receiver) {
			return (double)receiver == 0.0d;
		}

		public Object _positive_(Object receiver) {
			return (double)receiver >= 0.0d;
		}

		public Object _negative_(Object receiver) {
			return (double)receiver < 0.0d;
		}

		public Object _abs_(Object receiver) {
			double value = (double)receiver;
			return value >= 0.0d ? receiver : -value;
		}

		public Object _negated_(Object receiver) {
			return -(double)receiver;
		}
		
		public Object _sumWith_(Object receiver, Object operand) {
			double numOperand;
			try {
				numOperand = (double)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			return ((double)receiver) + numOperand;
		}
		
		public Object _differenceFrom_(Object receiver, Object operand) {
			double numOperand;
			try {
				numOperand = (double)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			return ((double)receiver) - numOperand;
		}
		
		public Object _productWith_(Object receiver, Object operand) {
			double numOperand;
			try {
				numOperand = (double)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			return ((double)receiver) * numOperand;
		}
		
		public Object _dividedBy_(Object receiver, Object operand) {
			double numOperand;
			try {
				numOperand = (double)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			return ((double)receiver) / numOperand; 
		}
		
		public Object _reciprocal_(Object receiver) {
			return 1.0d / (double)receiver; 
		}
		
		public Object _asCharacter_(Object receiver) {
			return (char)(uint)(double)receiver;
		}

		public Object _asInteger_(Object receiver) {
			return (long)(double)receiver;
		}

		public Object _asFloat_(Object receiver) {
			return (float)(double)receiver;
		}

		public Object _asQuad_(Object receiver) {
			return (decimal)(double)receiver;
		}

		public Object _ceiling_(Object receiver) {
			return Math.Ceiling((double)receiver);
		}

		public Object _floor_(Object receiver) {
			return Math.Floor((double)receiver);
		}

		public Object _rounded_(Object receiver) {
			return Math.Round((double)receiver);
		}

		public Object _truncated_(Object receiver) {
			return (long)(double)receiver;
		}

		public Object _roundTo_(Object receiver, Object operand) {
			// Smalltalk: 53 roundTo: 5 => 55; 1263.48 roundTo: 0.25 => 1263.50.
			// C-Lang: 53.roundTo(5) => 55; 1263.48.roundTo(0.25) => 1263.50.
			double modulus;
			try {
				modulus = (double)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			double self = (double)receiver;
			return (Math.Round(self / modulus) * modulus);
		}
		
		public Object _truncateTo_(Object receiver, Object modulus) {
			// Smalltalk: 53 truncateTo: 5 => 50; 1263.48 truncateTo: 0.25 => 1263.25.
			// C-Lang: 53.truncateTo(5) => 50; 1263.48.truncateTo(0.25) => 1263.25.
			double numModulus;
			try {
				numModulus = (double)modulus;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			double self = (double)receiver;
			return ((long)(self / numModulus)) * numModulus;
		}
		
		public Object _dividedToIntegerBy_(Object receiver, Object operand) {
			double numOperand;
			try {
				numOperand = (double)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			return (long)((double)receiver / numOperand); 
		}
		
		public Object _dividedAndFlooredToIntegerBy_(Object receiver, Object operand) {
			double numOperand;
			try {
				numOperand = (double)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			return (long)Math.Floor((double)receiver / numOperand); 
		}
		
		public Object _remainderFromModulus_(Object receiver, Object operand) {
			// Implements standard keyword message "rem: operand" 
			// Smalltalk: ^receiver - ((receiver quo: operand) * operand)
			// C-Lang: return receiver - ((long)(receiver / operand) * operand); -- Or just "receiver % operand" :-)
			double numOperand;
			try {
				numOperand = (double)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			return (double)receiver % numOperand;
		}
		
		public Object _flooredRemainderFromModulus_(Object receiver, Object operand) {
			// Implements standard binary message "\\ operand" 
			// Smalltalk: ^receiver - (receiver // operand * operand)
			// C-Lang: return receiver - ((long)Math.Floor(receiver / (double)operand) * operand);
			double numOperand;
			try {
				numOperand = (double)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			double self = (double)receiver;
			return self - ((long)Math.Floor(self / numOperand) * numOperand);
		}
		
		public Object _raisedTo_(Object receiver, Object operand) {
			double numOperand;
			try {
				numOperand = (double)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			return Math.Pow((double)receiver, numOperand);
		}
		
		public Object _log_(Object receiver, Object operand) {
			double numOperand;
			try {
				numOperand = (double)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			return Math.Log((double)receiver, numOperand);
		}
		
		public Object _ln_(Object receiver) {
			return Math.Log((double)receiver, Math.E);
		}

		public Object _toDo_(Object receiver, Object end, Object enumerator) {
			double primEnd;
			try {
				primEnd = (double)end;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			var enumerationFunction = ESObject.asFunctor1(enumerator);
			var primStart = (double)receiver;
			for (var inductionValue = primStart; inductionValue <= primEnd; inductionValue++) {
				enumerationFunction(inductionValue);
			}
			return receiver;
		}

		public Object _toByDo_(Object receiver, Object end, Object step, Object enumerator) {
			double primEnd;
			try {
				primEnd = (double)end;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			double primStep;
			try {
				primStep = (double)step;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			var enumerationFunction = ESObject.asFunctor1(enumerator);
			var primStart = (double)receiver;
			if (primStep > 0) {
				for (var inductionValue = primStart; inductionValue <= primEnd; inductionValue += primStep) {
					enumerationFunction(inductionValue);
				}
			} else if (primStep < 0) {
				for (var inductionValue = primStart; inductionValue >= primEnd; inductionValue -= primStep) {
					enumerationFunction(inductionValue);
				}
			} else {
				throw new PrimInvalidOperandException("Step value must not be zero");
			}
			return receiver;
		}

		#endregion

		public override void publishCanonicalPrimitives() {

			publishPrimitive("class",					new FuncNs.Func<Object, Object>(_class_));
			publishPrimitive("isMemberOf:",					new FuncNs.Func<Object, Object, Object>(_isMemberOf_));
			publishPrimitive("isKindOf:",					new FuncNs.Func<Object, Object, Object>(_isKindOf_));

			publishPrimitive("=",						new FuncNs.Func<Object, Object, Object>(_hasSameValueAs_));
			publishPrimitive("hash",					new FuncNs.Func<Object, Object>(_hash_));

			publishPrimitive("compareTo:",					new FuncNs.Func<Object, Object, Object>(_compareTo_));
			publishPrimitive("<",						new FuncNs.Func<Object, Object, Object>(_lessThan_));

			publishPrimitive("asCharacter",					new FuncNs.Func<Object, Object>(_asCharacter_));
			publishPrimitive("asInteger",					new FuncNs.Func<Object, Object>(_asInteger_));
			publishPrimitive("asFloat",					new FuncNs.Func<Object, Object>(_asFloat_));
			publishPrimitive("asQuad",					new FuncNs.Func<Object, Object>(_asQuad_));

			publishPrimitive("sign",					new FuncNs.Func<Object, Object>(_sign_));
			publishPrimitive("isZero",					new FuncNs.Func<Object, Object>(_isZero_));
			publishPrimitive("positive",					new FuncNs.Func<Object, Object>(_positive_));
			publishPrimitive("negative",					new FuncNs.Func<Object, Object>(_negative_));
			publishPrimitive("abs",						new FuncNs.Func<Object, Object>(_abs_));
			publishPrimitive("negated",					new FuncNs.Func<Object, Object>(_negated_));

			publishPrimitive("+",						new FuncNs.Func<Object, Object, Object>(_sumWith_));
			publishPrimitive("-",						new FuncNs.Func<Object, Object, Object>(_differenceFrom_));
			publishPrimitive("*",						new FuncNs.Func<Object, Object, Object>(_productWith_));
			publishPrimitive("/",						new FuncNs.Func<Object, Object, Object>(_dividedBy_));
			publishPrimitive("reciprocal",					new FuncNs.Func<Object, Object>(_reciprocal_));

			publishPrimitive("roundTo:",					new FuncNs.Func<Object, Object, Object>(_roundTo_));
			publishPrimitive("truncateTo:",					new FuncNs.Func<Object, Object, Object>(_truncateTo_));
			publishPrimitive("quo:",					new FuncNs.Func<Object, Object, Object>(_dividedToIntegerBy_));
			publishPrimitive("//",						new FuncNs.Func<Object, Object, Object>(_dividedAndFlooredToIntegerBy_));
			publishPrimitive("rem:",					new FuncNs.Func<Object, Object, Object>(_remainderFromModulus_));
			publishPrimitive(@"\\",						new FuncNs.Func<Object, Object, Object>(_flooredRemainderFromModulus_));
			publishPrimitive("**",						new FuncNs.Func<Object, Object, Object>(_raisedTo_));
			publishPrimitive("log:",					new FuncNs.Func<Object, Object, Object>(_log_));
			publishPrimitive("ln",						new FuncNs.Func<Object, Object>(_ln_));
			publishPrimitive("to:do:",					new FuncNs.Func<Object, Object, Object, Object>(_toDo_));
			publishPrimitive("to:by:do:",					new FuncNs.Func<Object, Object, Object, Object, Object>(_toByDo_));
		}

	}

	public class QuadPrecisionPrimitives : PrimitiveDomain {

		protected override void bindToObjectSpace() {
			domainClass = objectSpace.QuadClass;
		}

		public override PrimitiveDomainType Type {
			get {return PrimitiveDomainType.QuadPrecision;}
		}

		public override void publishCanonicalPrimitives() {
		}

	}

}
