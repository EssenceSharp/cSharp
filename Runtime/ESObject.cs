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
using System.Diagnostics;
using System.Runtime.CompilerServices;
using System.Dynamic;
#if CLR2
using Microsoft.Scripting.Ast;
using FuncNs = Microsoft.Scripting.Utils;
#else
using System.Linq.Expressions;
using FuncNs = System;
#endif
using EssenceSharp.UtilityServices;
using EssenceSharp.Runtime.Binding;
#endregion

namespace EssenceSharp.Runtime {

	public class ESObject : EssenceSharpObject, IDynamicMetaObjectProvider, IEquatable<ESObject>, ICloneable {
		
		#region Static variables and methods

		#region Utilities
			
		public bool elementsAreIdentical(Object[] left, Object[] right) {
			if (ReferenceEquals(left, right)) return true;
			if (left == null) return right.Length == 0;
			if (right == null) return left.Length == 0;
			long leftLength = left.Length;
			long rightLength = right.Length;
			long sizeDiff = leftLength - rightLength;
			if (sizeDiff != 0) return false;
			for (var i = 0; i < leftLength; i++) {
				if (!ReferenceEquals(left[i], right[i])) return false;
			}
			return true;
		}
		
		public bool elementsHaveSameValue(Object[] left, Object[] right) {
			if (ReferenceEquals(left, right)) return true;
			if (left == null) return right.Length == 0;
			if (right == null) return left.Length == 0;
			long leftLength = left.Length;
			long rightLength = right.Length;
			long sizeDiff = leftLength - rightLength;
			if (sizeDiff != 0) return false;
			for (var i = 0; i < leftLength; i++) {
				Object leftObject = left[i];
				Object rightObject = right[i];
				if (leftObject == null) {
					if (rightObject != null) return false;
				} else if (rightObject == null) {
					return false;
				} else {
					ESObject leftST = leftObject as ESObject;
					ESObject rightST = rightObject as ESObject;
					if (leftST == null || rightST == null) {
						if (!leftObject.Equals(rightObject)) return false;
					} else {
						if (!leftST.hasSameValueAs(rightST)) return false;
					}
				}
			}
			return true;
		}
		
		public static int compare<ComparableElementType>(ComparableElementType[] left, ComparableElementType[] right) where ComparableElementType : ESObject, IComparable {
			if (ReferenceEquals(left, right)) return 0;
			if (left == null) return -Math.Sign(right.Length);
			if (right == null) return Math.Sign(left.Length);
			long leftLength = left.Length;
			long rightLength = right.Length;
			long sizeDiff = leftLength - rightLength;
			if (sizeDiff != 0) return Math.Sign(sizeDiff);
			for (var i = 0; i < leftLength; i++) {
				int comparison = left[i].CompareTo(right[i]);
				if (comparison != 0) return Math.Sign(comparison);
			}
			return 0;
		}
		
		public static void print(Object[] objects, uint depth, Action<String> append, Action<uint> newLine) {
			if (objects == null) return;
			uint maxPrintSize = (uint)Math.Min(50, objects.Length);
			for (long i = 0; i < maxPrintSize; i++) {
				append(" ");
				Object objectValue = objects[i];
				if (objectValue == null) {
					append("nil");
				} else {
					ESObject value = objectValue as ESObject;
					if (value == null)
						append(objectValue.ToString());
					else
						value.printUsing(depth, append, newLine);
				}
			}
			if (objects.Length > maxPrintSize) {
				newLine(depth);
				append(".....First ");
				append(maxPrintSize.ToString());
				append(" of ");
				append(objects.Length.ToString());
				append(".....");
			}
		}
		
		#endregion
		
		#endregion;
		
		private ESBehavior 									_class 				= null;
		
		public ESObject(ESBehavior esClass) {
			setClass(esClass);
		}
		
		public virtual ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.Stateless;}
		}
		
		public virtual Object HostSystemValue {
			get {return this;}
		}

		internal virtual void setClass(ESBehavior esClass) {
			_class = esClass;
		}

		public String ClassName {
			get {return _class == null ? GetType().Name : _class.Name.PrimitiveValue;}
		}

		public String QualifiedClassName {
			get {return _class == null ? GetType().FullName : _class.PathnameString;}
		}
		
		public virtual BindingHandle asBindingHandle() {
			return asMutableBindingHandle();
		}
		
		#region Conversions to primitive types

		public static bool asBoolean(Object value) {
			if (value is bool) return (bool)value;
			throw new MustBeBoolean();
		}

		public static byte asHostByte(Object value) {
			return (byte)value;
		}

		public static ushort asHostHalfWord(Object value) {
			return (ushort)value;
		}

		public static uint asHostWord(Object value) {
			return (uint)value;
		}

		public static ulong asHostLongWord(Object value) {
			return (ulong)value;
		}

		public static long asHostLong(Object value) {
			return (long)value;
		}

		public static float asHostFloat(Object value) {
			return (float)value;
		}

		public static double asHostDouble(Object value) {
			return (double)value;
		}

		public static decimal asHostDecimal(Object value) {
			return (decimal)value;
		}

		public static String asHostString(Object value) {
			ESObject esValue = value as ESObject;
			return ReferenceEquals(esValue, null) ? (String)value : esValue.asHostString();
		}
				
		public virtual String asHostString() {
			return ((ESIndexedSlotsObject<char>)this).asHostString();
		}

		public static ElementType[] asHostArray<ElementType>(Object value) {
			ESIndexedSlotsObject<ElementType> esValue = value as ESIndexedSlotsObject<ElementType>;
			return ReferenceEquals(esValue, null) ? (ElementType[] )value : ((ESIndexedSlotsObject<ElementType>)esValue).IndexedSlots;
		}

		public ElementType[] asHostArray<ElementType>() {
			return ((ESIndexedSlotsObject<ElementType>)this).IndexedSlots;
		}

		#region Functor Delegates

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

		public static FuncNs.Func<Object, Object, Object, Object> asFunctor3(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (FuncNs.Func<Object, Object, Object, Object>)value : esValue.F3;
		}

		public static FuncNs.Func<Object, Object, Object, Object, Object> asFunctor4(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (FuncNs.Func<Object, Object, Object, Object, Object>)value : esValue.F4;
		}

		public static FuncNs.Func<Object, Object, Object, Object, Object, Object> asFunctor5(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (FuncNs.Func<Object, Object, Object, Object, Object, Object>)value : esValue.F5;
		}

		public static FuncNs.Func<Object, Object, Object, Object, Object, Object, Object> asFunctor6(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object>)value : esValue.F6;
		}

		public static FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object> asFunctor7(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object>)value : esValue.F7;
		}

		public static FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object> asFunctor8(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object>)value : esValue.F8;
		}

		public static FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> asFunctor9(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value : esValue.F9;
		}

		public static FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> asFunctor10(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value : esValue.F10;
		}

		public static FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> asFunctor11(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value : esValue.F11;
		}

		public static FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> asFunctor12(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value : esValue.F12;
		}

		public static FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> asFunctor13(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value : esValue.F13;
		}

		public static FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> asFunctor14(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value : esValue.F14;
		}

		public static FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> asFunctor15(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value : esValue.F15;
		}

		public static FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> asFunctor16(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value : esValue.F16;
		}

		public static Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> asFunctor17(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value : esValue.F17;
		}

		public static Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> asFunctor18(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value : esValue.F18;
		}

		public static Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> asFunctor19(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value : esValue.F19;
		}

		public static Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> asFunctor20(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value : esValue.F20;
		}

		public static Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> asFunctor21(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value : esValue.F21;
		}

		public static Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> asFunctor22(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value : esValue.F22;
		}

		public static Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> asFunctor23(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value : esValue.F23;
		}

		public static Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> asFunctor24(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value : esValue.F24;
		}

		public static Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> asFunctor25(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value : esValue.F25;
		}

		public static Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> asFunctor26(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value : esValue.F26;
		}

		public static Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> asFunctor27(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value : esValue.F27;
		}

		public static Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> asFunctor28(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value : esValue.F28;
		}

		public static Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> asFunctor29(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value : esValue.F29;
		}

		public static Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> asFunctor30(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value : esValue.F30;
		}

		public static Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> asFunctor31(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value : esValue.F31;
		}

		public static Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> asFunctor32(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value : esValue.F32;
		}

		#endregion

		#endregion

		#region Conversions to Essence Sharp objects
		
		public virtual ESByteArray asESByteArray() {
			throw new PrimInvalidOperandException("Must be a ByteArray");
		}
		
		public virtual ESString asESString() {
			throw new PrimInvalidOperandException("Must be a CharacterArray");
		}
		
		public virtual ESHalfWordArray asESHalfWordArray() {
			throw new PrimInvalidOperandException("Must be a HalfWordArray");
		}
		
		public virtual ESWordArray asESWordArray() {
			throw new PrimInvalidOperandException("Must be a WordArray");
		}
		
		public virtual ESLongWordArray asESLongWordArray() {
			throw new PrimInvalidOperandException("Must be a LongWordArray");
		}
		
		public virtual ESFloatArray asESFloatArray() {
			throw new PrimInvalidOperandException("Must be a FloatArray");
		}
		
		public virtual ESDoubleArray asESDoubleArray() {
			throw new PrimInvalidOperandException("Must be a DoubleArray");
		}
		
		public virtual ESQuadArray asESQuadArray() {
			throw new PrimInvalidOperandException("Must be a DoubleArray");
		}
		
		public virtual ESArray asESArray() {
			throw new PrimitiveFailException("Must be an Array");
		}
		
		public virtual ESSymbol asESSymbol() {
			throw new PrimInvalidOperandException("Must be a Symbol");
		}
		
		public virtual ESPathname asESPathname() {
			throw new PrimInvalidOperandException("Must be an a String, Symbol, or Pathname");
		}
		
		public virtual ESMethod asESMethod() {
			throw new PrimInvalidOperandException("Must be a CompiledMethod");
		}
		
		public virtual ESBehavior asESBehavior() {
			throw new PrimInvalidOperandException("Must be a Behavior");
		}
		
		public virtual ESNamespace asESNamespace() {
			throw new PrimInvalidOperandException("Must be a Behavior");
		}

		public virtual ESBlock asBlock() {
			throw new PrimInvalidOperandException("Must be a Block");
		}

		#endregion

		#region Smalltalk API

		#region Core

		public ESBehavior Class {
			get {return _class;}
		}

		public BindingHandle asImmutableBindingHandle() {
			return new DirectBindingHandle(this, true);
		}
	
		public BindingHandle asMutableBindingHandle() {
			return new DirectBindingHandle(this, false);
		}

		public virtual bool IsNil {
			get {return false;}
		}
		
		public virtual bool NotNil {
			get {return true;}
		}
		
		public virtual bool IsTrue {
			get {return false;}
		}
		
		public virtual bool IsFalse {
			get {return false;}
		}
		
		public virtual bool IsNamespace {
			get {return false;}
		}
		
		public virtual bool IsBehavior {
			get {return false;}
		}
		
		public virtual bool IsClass {
			get {return false;}
		}
		
		public virtual bool IsMetaclass {
			get {return false;}
		}
		
		public virtual bool HasIndexedSlots {
			get {return false;}
		}
		
		public virtual bool IsString {
			get {return false;}
		}
		
		public virtual bool IsSymbol {
			get {return false;}
		}
		
		public virtual bool IsBlock {
			get {return false;}
		}
		
		public virtual bool IsMethod {
			get {return false;}
		}
		
		public virtual bool IsImmutable {
			get {return true;}
		}
		
		public virtual void beImmutable() {
			// By default, do nothing
		}

		public bool isMemberOf(ESBehavior aBehavior) {
			return ReferenceEquals(Class, aBehavior);
		}
		
		public bool isKindOf(ESBehavior aBehavior) {
			return Class.includesBehavior(aBehavior);
		}
		
		public long identityHash() {
			return base.GetHashCode();
		}

		public virtual long hash() {
			return base.GetHashCode();
		}
	
		public virtual bool hasSameValueAs(ESObject other) {
			return ReferenceEquals(this, other);
		}
		
		public virtual ESObject shallowCopy() {
			return (ESObject)MemberwiseClone();
		}
		
		public virtual ESObject copy() {
			// May or may NOT have the same semantics as sending the message #copy to an STObject!!!.
			ESObject copy = shallowCopy();
			copy.postCopy();
			return copy;
		}
		
		public virtual void postCopy() {
			// May or may NOT have the same semantics as sending the message #postCopy to an STObject!!!.
			// By default, do nothing
		}
		
		public virtual ESObject asMutable() {
			if (IsImmutable) return copy();
			return this;
		}
		
		public ESObject asImmutable() {
			if (IsImmutable) return this;
			ESObject immutableCopy = copy();
			immutableCopy.beImmutable();
			return immutableCopy;
		}

		#endregion

		#region Instance variable accessing

		public virtual long size() {
			return 0;
		}
		
		public virtual Object instVarValueAt(long slotIndex) {
			Class.Kernel.throwInvalidInstanceVariableAccess(Class, slotIndex);
			return null;
		}
		
		public virtual Object instVarValueAtPut(long slotIndex, Object newValue) {
			Class.Kernel.throwInvalidInstanceVariableAccess(Class, slotIndex);
			return null;
		}	
		
		public Object instVarValueAtName(ESSymbol name) {
			long slotIndex = Class.instVarIndexFor(name);
			return slotIndex >= 0 ? instVarValueAt(slotIndex) : Class.Kernel.throwInvalidInstanceVariableAccess(_class.Name, name, slotIndex);
		}
		
		public Object instVarValueAtNamePut(ESSymbol name, Object newValue) {
			long slotIndex = Class.instVarIndexFor(name);
			if (slotIndex < 0) Class.Kernel.throwInvalidInstanceVariableAccess(_class.Name, name, slotIndex);
			return instVarValueAtPut(slotIndex, newValue);
		}
		
		#endregion
		
		#region Sending messages
		
		public bool respondsTo(ESSymbol selector) {
			return Class.canUnderstand(selector);
		}
		
		public Object respondTo(ESMessage message) {
			return performWithArguments(message.Selector, message.Arguments);
		}
		
		public Object perform(ESSymbol unaryMessageSelector) {
			ESMethod method = _class.compiledMethodAt(unaryMessageSelector);
			if (method == null) {
				ESKernel kernel = _class.Kernel;
				return kernel.performDoesNotUnderstand(this, _class, kernel.newMessage(unaryMessageSelector, null));
			}
			return method.value0(this);
		}
		
		public Object performWith1(ESSymbol binaryOrKeywordMessageSelector, Object a1) {
			ESMethod method = _class.compiledMethodAt(binaryOrKeywordMessageSelector);
			if (method == null) {
				ESKernel kernel = _class.Kernel;
				return kernel.performDoesNotUnderstand(this, _class, kernel.newMessage(binaryOrKeywordMessageSelector, new Object[]{a1}));
			}
			return method.value1(this, a1);
		}
		
		public Object performWith2(ESSymbol keywordMessageSelector, Object a1, Object a2) {
			ESMethod method = _class.compiledMethodAt(keywordMessageSelector);
			if (method == null) {
				ESKernel kernel = _class.Kernel;
				return kernel.performDoesNotUnderstand(this, _class, kernel.newMessage(keywordMessageSelector, new Object[]{a1, a2}));
			}
			return method.value2(this, a1, a2);
		}
		
		public Object performWith3(ESSymbol keywordMessageSelector, Object a1, Object a2, Object a3) {
			ESMethod method = _class.compiledMethodAt(keywordMessageSelector);
			if (method == null) {
				ESKernel kernel = _class.Kernel;
				return kernel.performDoesNotUnderstand(this, _class, kernel.newMessage(keywordMessageSelector, new Object[]{a1, a2, a3}));
			}
			return method.value3(this, a1, a2, a3);
		}
		
		public Object performWith4(ESSymbol keywordMessageSelector, Object a1, Object a2, Object a3, Object a4) {
			ESMethod method = _class.compiledMethodAt(keywordMessageSelector);
			if (method == null) {
				ESKernel kernel = _class.Kernel;
				return kernel.performDoesNotUnderstand(this, _class, kernel.newMessage(keywordMessageSelector, new Object[]{a1, a2, a3, a4}));
			}
			return method.value4(this, a1, a2, a3, a4);
		}
		
		public Object performWithArguments(ESSymbol selector, Object[] arguments) {
			ESMethod method = _class.compiledMethodAt(selector);
			if (method == null) {
				ESKernel kernel = _class.Kernel;
				return kernel.performDoesNotUnderstand(this, _class, kernel.newMessage(selector, arguments));
			}
			return method.valueWithReceiverWithArguments(this, arguments);
		}
		
		#endregion

		#region Debugging

		public void halt() {
			Debugger.Break();
		}

		public void show() {
			Console.Write(ToString());
		}

		public void crShow() {
			Console.WriteLine("");
			Console.Write(ToString());
		}

		public void showCr() {
			Console.WriteLine(ToString());
		}

		#endregion

		#endregion

		#region Foreign language interoperability

		public virtual DynamicMetaObject GetMetaObject(Expression parameter) {
			return new ESDynamicMetaObject(parameter, BindingRestrictions.Empty, this);
		}
		
		public Object Clone() {
			return shallowCopy();
		}
		
		public override int GetHashCode() {
			return (int)hash();
		}

		public override bool Equals(Object generalOther) {
			// Not used by compiled Smalltalk code. The intent is to enable interoperability of STObjects with non-Smalltalk code.
			if (ReferenceEquals(this, generalOther)) return true;
			ESObject other = generalOther as ESObject;
			if (other == null) return false;
			return other.hasSameValueAs(this);
		}      
		
		public bool Equals(ESObject other) {
			return other.hasSameValueAs(this);
		}      
		
 		public static bool operator false(ESObject receiver) {
       			return receiver == null ? false :  receiver.IsFalse;
		}
		
 		public static bool operator true(ESObject receiver) {
       			return receiver == null ? false :  receiver.IsTrue;
		}
		
		public override void printTypeUsing(Action<String> append) {
			append(QualifiedClassName);
		}
		
		public override void printUsing(uint depth, Action<String> append, Action<uint> newLine) {
			append("{<");
			printTypeUsing(append);
			append("> ");
			printElementsUsing(depth+1, append, newLine);
			append("}");
		}
		
		#endregion

		public virtual T valueBy<T>(Operation<T> operation) {
			return operation.applyToESObject(this);
		}

		public class Primitives : PrimitiveDomain {

			protected override void bindToKernel() {
				domainClass = kernel.ObjectClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.Object;}
			}
		
			#region Primitive Definitions
			// As an invariant of the Essence Sharp Smalltalk implementation, all primitives must:
			//		a) return type Object, 
			//		b) have all parameters typed as Object, and
			//		c) have at least one parameter which provides the receiver of the message--and that parameter must be the first one.

			#region Core

			public Object _class_(Object receiver) {
				return kernel.classOf(receiver);
			}
		
			public Object _isMemberOf_(Object receiver, Object aBehavior) {
				return ReferenceEquals(kernel.classOf(receiver), aBehavior);
			}
		
			public Object _isKindOf_(Object receiver, Object aBehavior) {
				return kernel.classOf(receiver).includesBehavior((ESBehavior)aBehavior);
			}
		
			public static Object _hasSameIdentityAs_(Object receiver, Object other) {
				return ReferenceEquals(receiver, other);
			}
		
			public static Object _hasSameValueAs_(Object receiver, Object comparand) {
				return ((ESObject)receiver).hasSameValueAs((ESObject)comparand);
			}
		
			public static Object _identityHash_(Object receiver) {
				return RuntimeHelpers.GetHashCode(receiver);
			}
		
			public static Object _hash_(Object receiver) {
				return ((ESObject)receiver).hash();
			}

			public static Object _ifNotNil_(Object receiver, Object notNilAction) {
				return asFunctor0(notNilAction)();
			}

			public static Object _ifNilIfNotNil_(Object receiver, Object nilAction, Object notNilAction) {
				return asFunctor0(notNilAction)();
			}

			public static Object _ifNotNilIfNil_(Object receiver, Object notNilAction, Object nilAction) {
				return asFunctor0(notNilAction)();
			}

			public static Object _isImmutable_(Object receiver) {
				return ((ESObject)receiver).IsImmutable;
			}
		
			public static Object _beImmutable_(Object receiver) {
				((ESObject)receiver).beImmutable();
				return receiver;
			}
		
			public static Object _shallowCopy_(Object receiver) {
				return ((ESObject)receiver).shallowCopy();
			}
		
			public static Object _asMutable_(Object receiver) {
				return ((ESObject)receiver).asMutable();
			}
		
			public static Object _asImmutable_(Object receiver) {
				return ((ESObject)receiver).asImmutable();
			}
		
			public Object _asAssociationKeyWithValue_ (Object receiver, Object value) {
				return kernel.newAssociation(receiver, value);
			}

			public static Object _asNamespace_(Object receiver) {
				return ((ESObject)receiver).asESNamespace();
			}
		
			public static Object _halt_ (Object receiver) {
				Debugger.Break();
				return receiver;
			}
		
			public static Object _show_ (Object receiver) {
				((ESObject)receiver).show();
				return receiver;
			}
		
			public static Object _crShow_ (Object receiver) {
				((ESObject)receiver).crShow();
				return receiver;
			}
		
			public static Object _showCr_ (Object receiver) {
				((ESObject)receiver).showCr();
				return receiver;
			}

			#endregion

			#region Instance variable accessing

			public static Object _size_(Object receiver) {
				return ((ESObject)receiver).size();
			}
		
			public static Object _instVarValueAt_(Object receiver, Object slotIndex) {
				return ((ESObject)receiver).instVarValueAt(asHostLong(slotIndex) - 1);
			}
		
			public static Object _instVarValueAtName_(Object receiver, Object name) {
				return ((ESObject)receiver).instVarValueAtName((ESSymbol)name);
			}
		
			public static Object _instVarValueAtPut_(Object receiver, Object slotIndex, Object newValue) {
				return ((ESObject)receiver).instVarValueAtPut(asHostLong(slotIndex) - 1, newValue);
			}	
		
			public static Object _instVarValueAtNamePut_(Object receiver, Object name, Object newValue) {
				return ((ESObject)receiver).instVarValueAtNamePut((ESSymbol)name, newValue);
			}	
		
			#endregion
		
			#region Sending messages
		
			public Object _respondsTo_(Object receiver, Object selector) {
				return kernel.classOf(receiver).canUnderstand(kernel.asESSymbol(selector));
			}
		
			public Object _respondTo_(Object receiver, Object message) {
				ESMessage stMessage;
				try {
					stMessage = (ESMessage)message;
				} catch {
					throw new PrimInvalidOperandException();
				}
				ESBehavior esClass = kernel.classOf(receiver);
				ESMethod method = esClass.compiledMethodAt(stMessage.Selector);
				if (method == null) return kernel.performDoesNotUnderstand(receiver, esClass, stMessage);
				return method.valueWithReceiverWithArguments(receiver, stMessage.Arguments);
			}
		
			public Object _perform_(Object receiver, Object selector) {
				ESSymbol stSelector;
				try {
					stSelector = (ESSymbol)selector;
				} catch {
					throw new PrimInvalidOperandException();
				}
				ESBehavior esClass = kernel.classOf(receiver);
				ESMethod method = esClass.compiledMethodAt(stSelector);
				if (method == null) return kernel.performDoesNotUnderstand(receiver, esClass, kernel.newMessage(stSelector, null));
				return method.value0(receiver);
			}
		
			public Object _performWith1_(Object receiver, Object selector, Object a1) {
				ESSymbol stSelector;
				try {
					stSelector = (ESSymbol)selector;
				} catch {
					throw new PrimInvalidOperandException();
				}
				ESBehavior esClass = kernel.classOf(receiver);
				ESMethod method = esClass.compiledMethodAt(stSelector);
				if (method == null) return kernel.performDoesNotUnderstand(receiver, esClass, kernel.newMessage(stSelector, new Object[]{a1}));
				return method.value1(receiver, a1);
			}
		
			public Object _performWith2_(Object receiver, Object selector, Object a1, Object a2) {
				ESSymbol stSelector;
				try {
					stSelector = (ESSymbol)selector;
				} catch {
					throw new PrimInvalidOperandException();
				}
				ESBehavior esClass = kernel.classOf(receiver);
				ESMethod method = esClass.compiledMethodAt(stSelector);
				if (method == null) return kernel.performDoesNotUnderstand(receiver, esClass, kernel.newMessage(stSelector, new Object[]{a1, a2}));
				return method.value2(receiver, a1, a2);
			}
		
			public Object _performWith3_(Object receiver, Object selector, Object a1, Object a2, Object a3) {
				ESSymbol stSelector;
				try {
					stSelector = (ESSymbol)selector;
				} catch {
					throw new PrimInvalidOperandException();
				}
				ESBehavior esClass = kernel.classOf(receiver);
				ESMethod method = esClass.compiledMethodAt(stSelector);
				if (method == null) return kernel.performDoesNotUnderstand(receiver, esClass, kernel.newMessage(stSelector, new Object[]{a1, a2, a3}));
				return method.value3(receiver, a1, a2, a3);
			}
		
			public Object _performWith4_(Object receiver, Object selector, Object a1, Object a2, Object a3, Object a4) {
				ESSymbol stSelector;
				try {
					stSelector = (ESSymbol)selector;
				} catch {
					throw new PrimInvalidOperandException();
				}
				ESBehavior esClass = kernel.classOf(receiver);
				ESMethod method = esClass.compiledMethodAt(stSelector);
				if (method == null) return kernel.performDoesNotUnderstand(receiver, esClass, kernel.newMessage(stSelector, new Object[]{a1, a2, a3, a4}));
				return method.value4(receiver, a1, a2, a3, a4);
			}
		
			public Object _performWithArguments_(Object receiver, Object selector, Object arguments) {
				ESSymbol stSelector;
				try {
					stSelector = (ESSymbol)selector;
				} catch {
					throw new PrimInvalidOperandException();
				}
				Object[] argArray;
				try {
					argArray = asHostArray<Object>(arguments);
				} catch {
					throw new PrimInvalidOperandException();
				}
				ESBehavior esClass = kernel.classOf(receiver);
				ESMethod method = esClass.compiledMethodAt(stSelector);
				if (method == null) return kernel.performDoesNotUnderstand(receiver, esClass, kernel.newMessage(stSelector, argArray));
				return method.valueWithReceiverWithArguments(receiver, argArray);
			}
		
			#endregion
		
			#endregion

			public override void publishCanonicalPrimitives() {

				publishPrimitive("class",						new FuncNs.Func<Object, Object>(_class_));
				publishPrimitive("isMemberOf:",						new FuncNs.Func<Object, Object, Object>(_isMemberOf_));
				publishPrimitive("isKindOf:",						new FuncNs.Func<Object, Object, Object>(_isKindOf_));
				publishPrimitive("==",							new FuncNs.Func<Object, Object, Object>(_hasSameIdentityAs_));
				publishPrimitive("=",							new FuncNs.Func<Object, Object, Object>(_hasSameValueAs_));
				publishPrimitive("identityHash",					new FuncNs.Func<Object, Object>(_identityHash_));
				publishPrimitive("hash",						new FuncNs.Func<Object, Object>(_hash_));
				publishPrimitive("ifNotNil:",						new FuncNs.Func<Object, Object, Object>(_ifNotNil_));
				publishPrimitive("ifNil:ifNotNil:",					new FuncNs.Func<Object, Object, Object, Object>(_ifNilIfNotNil_));
				publishPrimitive("ifNotNil:ifNil:",					new FuncNs.Func<Object, Object, Object, Object>(_ifNotNilIfNil_));
				publishPrimitive("isImmutable",						new FuncNs.Func<Object, Object>(_isImmutable_));
				publishPrimitive("beImmutable",						new FuncNs.Func<Object, Object>(_beImmutable_));
				publishPrimitive("shallowCopy",						new FuncNs.Func<Object, Object>(_shallowCopy_));
				publishPrimitive("asMutable",						new FuncNs.Func<Object, Object>(_asMutable_));
				publishPrimitive("asImmutable",						new FuncNs.Func<Object, Object>(_asImmutable_));
				publishPrimitive("->",							new FuncNs.Func<Object, Object, Object>(_asAssociationKeyWithValue_));
				publishPrimitive("size",						new FuncNs.Func<Object, Object>(_size_));
				publishPrimitive("instVarValueAt:",					new FuncNs.Func<Object, Object, Object>(_instVarValueAt_));
				publishPrimitive("instVarValueAtName:",					new FuncNs.Func<Object, Object, Object>(_instVarValueAtName_));
				publishPrimitive("instVarValueAt:put:",					new FuncNs.Func<Object, Object, Object, Object>(_instVarValueAtPut_));
				publishPrimitive("instVarValueAtName:put:",				new FuncNs.Func<Object, Object, Object, Object>(_instVarValueAtNamePut_));
				publishPrimitive("respondsTo:",						new FuncNs.Func<Object, Object, Object>(_respondsTo_));
				publishPrimitive("respondTo:",						new FuncNs.Func<Object, Object, Object>(_respondTo_));
				publishPrimitive("perform:",						new FuncNs.Func<Object, Object, Object>(_perform_));
				publishPrimitive("perform:with:",					new FuncNs.Func<Object, Object, Object, Object>(_performWith1_));
				publishPrimitive("perform:with:with:",					new FuncNs.Func<Object, Object, Object, Object, Object>(_performWith2_));
				publishPrimitive("perform:with:with:with:",				new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_performWith3_));
				publishPrimitive("perform:with:with:with:with:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object, Object>(_performWith4_));
				publishPrimitive("perform:withArguments:",				new FuncNs.Func<Object, Object, Object, Object>(_performWithArguments_));
				publishPrimitive("halt",						new FuncNs.Func<Object, Object>(_halt_));
				publishPrimitive("show",						new FuncNs.Func<Object, Object>(_show_));
				publishPrimitive("crShow",						new FuncNs.Func<Object, Object>(_crShow_));
				publishPrimitive("showCr",						new FuncNs.Func<Object, Object>(_showCr_));

			}

		}
		
	}
		
}
