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
using System.Text;
using System.Collections;
using System.Collections.Generic;
#if CLR2
using FuncNs = Microsoft.Scripting.Utils;
#else
using FuncNs = System;
#endif
using EssenceSharp.UtilityServices;
using EssenceSharp.Exceptions.System;
using EssenceSharp.Exceptions.System.PrimitiveFailures;
#endregion

namespace EssenceSharp.Runtime {
	
	public abstract class ESIndexedComparableSlotsObject<ValueType> : ESIndexedSlotsObject<ValueType>, IComparable, IEnumerable, IEnumerable<ValueType> where ValueType : IEquatable<ValueType>, IComparable {
				
		public ESIndexedComparableSlotsObject(ESBehavior esClass, long size) : base(esClass, size) {
		}
		
		public ESIndexedComparableSlotsObject(ESBehavior esClass, ValueType[] slots) : base(esClass, slots) {
		}

		protected override bool eachHasSameValue(ValueType left, ValueType right) {
			return left.Equals(right);
		}
		
		public virtual int compareTo(ESIndexedSlotsObject<ValueType> comparand) {
			return ESIndexedComparableSlotsObject<ValueType>.compare(IndexedSlots, comparand.IndexedSlots);
		}

		protected abstract int foreignCompareTo(Object comparandObject);
		
		public virtual int CompareTo(Object comparandObject) {
			if (comparandObject == null) {
				Class.Kernel.throwInvalidArgumentException(Class, "CompareTo", "comparand", comparandObject);
			}
			if (this == comparandObject) return 0;
			var comparand = comparandObject as ESIndexedComparableSlotsObject<ValueType>;
			if (comparand == null) {
				return foreignCompareTo(comparandObject);
			} else {
				return compareTo(comparand);
			}
		}	
		
		IEnumerator IEnumerable.GetEnumerator() {
			return slots.GetEnumerator();
		}
		
		public virtual IEnumerator<ValueType> GetEnumerator() {
			return new ESIndexedComparableSlotsObjectEnumerator(this);
		}

		public class ESIndexedComparableSlotsObjectEnumerator : IEnumerator<ValueType> {
			private ESIndexedComparableSlotsObject<ValueType> esArray;
			private int index;

			internal ESIndexedComparableSlotsObjectEnumerator(ESIndexedComparableSlotsObject<ValueType> esArray) {
				this.esArray = esArray;
				index = -1;
			}

			public bool MoveNext() {
				if (esArray.Count - index <= 1) return false;
				index++;
				return true;
			}

			public void Reset() {
				index = -1;
			}

			void IDisposable.Dispose() {
				esArray = null;
			}

			public ValueType Current {
				get {return esArray[index];}
			}

			Object IEnumerator.Current {
				get {return Current;}
			}

		}

		public override bool Equals(Object comparand) {
			return (bool)_hasSameValueAs_(this, comparand);
		}

		public override bool Equals(ESObject comparand) {
			return (bool)_hasSameValueAs_(this, comparand);
		}

		public static Object _hasSameValueAs_(Object receiver, Object comparandObject) {
			if (receiver == comparandObject) return true;
			var esArray = (ESIndexedComparableSlotsObject<ValueType>)receiver;
			var comparand = comparandObject as ESIndexedComparableSlotsObject<ValueType>;
			if (comparand == null) {
				return esArray.foreignCompareTo(comparandObject) == 0;
			} else {
				return esArray.compareTo(comparand) == 0;
			}
		}

		public static Object _compareTo_(Object receiver, Object comparandObject) {
			if (receiver == comparandObject) return 0;
			var esArray = (ESIndexedComparableSlotsObject<ValueType>)receiver;
			if (comparandObject == null) {
				esArray.Class.Kernel.throwInvalidArgumentException(esArray.Class, "CompareTo", "comparand", comparandObject);
			}
			var comparand = comparandObject as ESIndexedComparableSlotsObject<ValueType>;
			if (comparand == null) {
				return esArray.foreignCompareTo(comparandObject);
			} else {
				return esArray.compareTo(comparand);
			}
		}

		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (Class.InstSize > 0) {
				base.printElementsUsing(depth, append, newLine);
				newLine(0);
			}
			printNamedInstanceVariablesUsing(depth, append, newLine);
			append(" size: ");
			append(size().ToString());
			append(" |");
			uint maxPrintSize = (uint)Math.Min(80, size());
			for (long i = 0; i < maxPrintSize; i++) {
				append(" ");
				var value = slots[i];
				append(value.ToString());
			}
			if (size() > maxPrintSize) {
				append(" ");
				append(".....First ");
				append(maxPrintSize.ToString());
				append(" of ");
				append(size().ToString());
				append(".....");
			}
		}

	}
	
	public class ESByteArray : ESIndexedComparableSlotsObject<byte> {
		
		#region Static variables and functions
		
		public static implicit operator byte[](ESByteArray array) {
			return array.IndexedSlots;  
		}
		
		#endregion
				
		public ESByteArray(ESBehavior esClass) : base(esClass, 0) {
		}
				
		public ESByteArray(ESBehavior esClass, long size) : base(esClass, size) {
		}
		
		public ESByteArray(ESBehavior esClass, byte[] slots) : base(esClass, slots) {
		}

		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.IndexedByteSlots;}
		}
		
		public override ESArray asESArray() {
			return Class.Kernel.newArray(Array.ConvertAll<byte, Object>(IndexedSlots, value => (Object)value));
		}
		
		public override ESByteArray asESByteArray() {
			return this;
		}

		protected override int foreignCompareTo(Object comparand) {
			byte[] primArray = comparand as byte[];
			if (primArray == null) {
				IComparable comparable = comparand as IComparable;
				if (comparable == null) Class.Kernel.throwInvalidArgumentException(Class, "foreignCompareTo", "comparand", comparand);
				return -Math.Sign(comparable.CompareTo(IndexedSlots));
			}
			return ESIndexedComparableSlotsObject<byte>.compare(IndexedSlots, primArray);
		}

		public override T valueBy<T>(Operation<T> operation) {
		    return operation.applyToIndexedByteSlotsObject(this);
		}

		public new class Primitives : ESIndexedSlotsObject<byte>.Primitives {

			protected override void bindToKernel() {
				domainClass = kernel.ByteArrayClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.IndexedByteSlots;}
			}

			#region Primitive Definitions

			public Object _at_(Object receiver, Object slotIndexObject) {
				var array = (ESIndexedComparableSlotsObject<byte>)receiver;
				var slots = array.IndexedSlots;
				var slotIndex = ((long)slotIndexObject) - 1;
				return (long)slots[slotIndex];
			}

			public Object _atPut_(Object receiver, Object slotIndexObject, Object newValue) {
				var array = (ESIndexedComparableSlotsObject<byte>)receiver;
				if (array.IsImmutable) throw new ImmutableObjectException();
				var slots = array.IndexedSlots;
				var slotIndex = ((long)slotIndexObject) - 1;
				slots[slotIndex] = (byte)newValue;
				return newValue;
			}
		
			public Object _atFirstPutOrPrepend_(Object receiver, Object newValue) {
				return ((ESIndexedComparableSlotsObject<byte>)receiver).atFirstPutOrPrepend(asHostByte(newValue));
			}
		
			public Object _atLastPutOrAppend_(Object receiver, Object newValue) {
				return ((ESIndexedComparableSlotsObject<byte>)receiver).atLastPutOrAppend(asHostByte(newValue));
			}
		
			public Object _elementsDo_(Object receiver, Object enumerator) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator);
				((ESIndexedComparableSlotsObject<byte>)receiver).elementsDo(value => f1((long)value));
				return receiver;
			}

			public Object _elementsFromToDo_(Object receiver, Object startIndex, Object endIndex, Object enumerator) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator);
				((ESIndexedComparableSlotsObject<byte>)receiver).elementsFromToDo((long)startIndex - 1, (long)endIndex - 1, value => f1((long)value));
				return receiver;
			}
		
			public Object _elementsFromToByDo_(Object receiver, Object startIndex, Object endIndex, Object step, Object enumerator) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator);
				((ESIndexedComparableSlotsObject<byte>)receiver).elementsFromToByDo((long)startIndex - 1, (long)endIndex - 1, asHostLong(step), value => f1((long)value));
				return receiver;
			}

			#endregion

			public override void publishCanonicalPrimitives() {

				publishPrimitive("hash",						new FuncNs.Func<Object, Object>(_hash_));
				publishPrimitive("=",							new FuncNs.Func<Object, Object, Object>(_hasSameValueAs_));
				publishPrimitive("compareTo:",						new FuncNs.Func<Object, Object, Object>(_compareTo_));

				publishPrimitive("at:",							new FuncNs.Func<Object, Object, Object>(_at_));

				publishPrimitive("do:",							new FuncNs.Func<Object, Object, Object>(_elementsDo_));
				publishPrimitive("from:to:do:",						new FuncNs.Func<Object, Object, Object, Object, Object>(_elementsFromToDo_));
				publishPrimitive("from:to:by:do:",					new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_elementsFromToByDo_));

				publishPrimitive("at:put:",						new FuncNs.Func<Object, Object, Object, Object>(_atPut_));
				publishPrimitive("nextIdentityIndexOf:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIdentityIndexOfIfAbsent_<byte>));
				publishPrimitive("identityIncludes:",					new FuncNs.Func<Object, Object, Object>(_identityIncludes_<byte>));
				publishPrimitive("add:",						new FuncNs.Func<Object, Object, Object>(_appendElement_<byte>));
				publishPrimitive("copyWith:",						new FuncNs.Func<Object, Object, Object>(_copyAppendingElement_<byte>));
				publishPrimitive("identityRemoveNext:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_identityRemoveNextIfAbsent_<byte>));	
				publishPrimitive("copyIdentityRemovingNext:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyIdentityRemovingNextIfAbsent_<byte>));	
				publishPrimitive("identityRemoveAll:",					new FuncNs.Func<Object, Object, Object>(_identityRemoveAllOccurrencesOf_<byte>));	
				publishPrimitive("copyIdentityRemovingAll:",				new FuncNs.Func<Object, Object, Object>(_identityRemoveAllOccurrencesOf_<byte>));	
				publishPrimitive("add:beforeIndex:",					new FuncNs.Func<Object, Object, Object, Object>(_insertElementAt_<byte>));
				publishPrimitive("copyAdding:beforeIndex:",				new FuncNs.Func<Object, Object, Object, Object>(_copyInstertingElementAt_<byte>));

				publishPrimitive("nextIndexOf:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIndexOfIfAbsent_<byte>));
				publishPrimitive("includes:",						new FuncNs.Func<Object, Object, Object>(_includes_<byte>));
				publishPrimitive("removeNext:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_removeNextIfAbsent_<byte>));	
				publishPrimitive("copyRemovingNext:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyRemovingNextIfAbsent_<byte>));	
				publishPrimitive("removeAll:",						new FuncNs.Func<Object, Object, Object>(_removeAllOccurrencesOf_<byte>));
				publishPrimitive("copyRemovingAll:", /* #copyWithout: */		new FuncNs.Func<Object, Object, Object>(_copyRemovingAllOccurrencesOf_<byte>));

				publishPrimitive("atFirstPutOrAdd:",					new FuncNs.Func<Object, Object, Object>(_atFirstPutOrPrepend_));
				publishPrimitive("atLastPutOrAdd:",					new FuncNs.Func<Object, Object, Object>(_atLastPutOrAppend_));

				publishPrimitive("setSize:",						new FuncNs.Func<Object, Object, Object>(_setSize_));
				publishPrimitive("copyWithSize:",					new FuncNs.Func<Object, Object, Object>(_copyWithSize_));
				publishPrimitive("firstIfNone:",					new FuncNs.Func<Object, Object, Object>(_firstIfNone_));
				publishPrimitive("lastIfNone:",						new FuncNs.Func<Object, Object, Object>(_lastIfNone_));
				publishPrimitive("findtFirst:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIndexSuchThatIfAbsent_));
				publishPrimitive("contains:",						new FuncNs.Func<Object, Object, Object>(_contains_));
				publishPrimitive("addAll:",						new FuncNs.Func<Object, Object, Object>(_appendAll_));
				publishPrimitive(",",							new FuncNs.Func<Object, Object, Object>(_copyAppendingAll_));
				publishPrimitive("copyFrom:to:",					new FuncNs.Func<Object, Object, Object, Object>(_copyFromTo_));
				publishPrimitive("copyTo:",						new FuncNs.Func<Object, Object, Object>(_prefixTo_));
				publishPrimitive("copyFrom:",						new FuncNs.Func<Object, Object, Object>(_suffixFrom_));
				publishPrimitive("withAllButFirst",					new FuncNs.Func<Object, Object>(_withAllButFirst_));
				publishPrimitive("withAllButLast",					new FuncNs.Func<Object, Object>(_withAllButLast_));
				publishPrimitive("withFirst",						new FuncNs.Func<Object, Object>(_withFirst_));
				publishPrimitive("withLast",						new FuncNs.Func<Object, Object>(_withLast_));

				publishPrimitive("removeAt:",						new FuncNs.Func<Object, Object, Object>(_removeAt_));		// "Delete index"
				publishPrimitive("copyRemovingAt:",					new FuncNs.Func<Object, Object, Object>(_copyRemovingAt_));	// "Copy without index"
				publishPrimitive("copyAndRemoveFrom:to:",				new FuncNs.Func<Object, Object, Object, Object>(_copyAndRemoveFromTo_));	// "Cut range"
				publishPrimitive("copyRemovingFrom:to:",				new FuncNs.Func<Object, Object, Object, Object>(_copyRemovingFromTo_));	// "Copy without range"
				publishPrimitive("removeNextSuchThat:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_removeNextSuchThatIfAbsent_));
				publishPrimitive("copyRemovingNextSuchThat:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyRemovingNextSuchThatIfAbsent_));
				publishPrimitive("removeAllSuchThat:",					new FuncNs.Func<Object, Object, Object>(_removeAllOccurrencesSuchThat_));
				publishPrimitive("copyRemovingAllSuchThat:",				new FuncNs.Func<Object, Object, Object>(_copyRemovingAllSuchThat_));
				publishPrimitive("addAll:beforeIndex:",					new FuncNs.Func<Object, Object, Object, Object>(_insertAllAt_));
				publishPrimitive("copyAddingAll:beforeIndex:",				new FuncNs.Func<Object, Object, Object, Object>(_copyInsertingAllAt_));
				publishPrimitive("moveFrom:to:by:",					new FuncNs.Func<Object, Object, Object, Object, Object>(_moveFromToBy_)); // Cut then paste range
				publishPrimitive("copyMovingFrom:to:by:",				new FuncNs.Func<Object, Object, Object, Object, Object>(_copyMovingFromToBy_)); // Cut then paste range
				publishPrimitive("replaceFrom:to:with:startingAt:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_replaceFromToWithStartingAt_)); 
				publishPrimitive("copyReplacingFrom:to:with:startingAt:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyReplacingFromToWithStartingAt_)); 
				publishPrimitive("reverse",						new FuncNs.Func<Object, Object>(_reverse_)); 
				publishPrimitive("copyReversed",					new FuncNs.Func<Object, Object>(_copyReversed_)); 
				publishPrimitive("reverseFrom:to:",					new FuncNs.Func<Object, Object, Object, Object>(_reverseFromTo_)); 
				publishPrimitive("copyReversedFrom:to:",				new FuncNs.Func<Object, Object, Object, Object>(_copyReversedFromTo_));

			}

		}

	}
	
	public class ESString : ESIndexedComparableSlotsObject<char> {
		
		#region Static variables and functions
		
		public static implicit operator char[](ESString array) {
			return array.IndexedSlots;  
		}
		
		public static implicit operator String(ESString array) {
			return new String(array.IndexedSlots);  
		}

		#endregion

		public ESString(ESBehavior esClass) : base(esClass, 0) {
		}

		public ESString(ESBehavior esClass, long size) : base(esClass, size) {
		}
		
		public ESString(ESBehavior esClass, char[] slots) : base(esClass, slots) {
		}
		
		public ESString(ESBehavior esClass, String value) : base(esClass, value.ToCharArray()) {
		}

		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.IndexedCharSlots;}
		}
		
		public override bool IsString {
			get {return true;}
		}
		
		public override String asHostString() {
			return new String(slots);
		}
		
		public String encodedAsFilename() {
			return asHostString().encodedAsFilename();
		}
		
		public override ESArray asESArray() {
			return Class.Kernel.newArray(Array.ConvertAll<char, Object>(IndexedSlots, value => (Object)value));
		}
		
		public override ESPathname asESPathname() {
			return Class.Kernel.pathnameFromString(asHostString());
		}
		
		public virtual ESPathname asESPathnameUsingSeparator(char separatorChar) {
			return Class.Kernel.pathnameFromString(asHostString(), separatorChar, null);
		}
		
		public virtual ESPathname asESPathnameUsingSeparators(char[] separatorChars) {
			return Class.Kernel.pathnameFromString(asHostString(), separatorChars, null);
		}
		
		public override ESString asESString() {
			return this;
		}

		public virtual char[] ToCharArray() {
			return slots;
		}
		
		public override ESSymbol asESSymbol() {
			return Class.Kernel.symbolFor(asHostString());
		}

		public override ESNamespace asESNamespace() {
			var kernel = Class.Kernel;
			return kernel.asESNamespace(
				valueInNamespaceIfAbsent(
					kernel.RootNamespace, 
					AccessPrivilegeLevel.Public, 
					ImportTransitivity.Transitive, 
					delegate() {throw new PrimitiveFailException("Specified namespace is not accessible");}));
		}

		protected override int foreignCompareTo(Object comparand) {
			var stringComparand = comparand as String;
			if (stringComparand == null) {
				char[] primArray = comparand as char[];
				if (primArray == null) {
					IComparable comparable = comparand as IComparable;
					if (comparable == null) Class.Kernel.throwInvalidArgumentException(Class, "CompareTo", "comparand", comparand);
					return -Math.Sign(comparable.CompareTo(IndexedSlots));
				} else {
					return compare<char>(IndexedSlots, primArray);
				}
			} else {
				return asHostString().CompareTo(stringComparand);
			}
		}
		
		public virtual ESBindingReference bindingInNamespaceIfAbsent(NamespaceObject environment, AccessPrivilegeLevel requestorRights, ImportTransitivity importTransitivity, Functor0<ESBindingReference> ifAbsentAction) {
			var binding = environment.bindingAt(asHostString(), requestorRights, importTransitivity, null);
			return binding == null ?
				ifAbsentAction == null ? null : ifAbsentAction() :
				binding;
		}
		
		public virtual Object valueInNamespaceIfAbsent(NamespaceObject environment, AccessPrivilegeLevel requestorRights, ImportTransitivity importTransitivity, FuncNs.Func<Object> ifAbsentAction) {
			var binding = environment.bindingAt(asHostString(), requestorRights, importTransitivity, null);
			return binding == null ?
				ifAbsentAction == null ? null : ifAbsentAction() :
				binding.Value.Value;
		}

		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			printNamedInstanceVariablesUsing(depth, append, newLine);
			append(asHostString());
		}

		public override T valueBy<T>(Operation<T> operation) {
		    return operation.applyToIndexedCharSlotsObject(this);
		}

		public new class Primitives : ESIndexedSlotsObject<char>.Primitives {

			protected override void bindToKernel() {
				domainClass = kernel.StringClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.IndexedCharSlots;}
			}

			#region Primitive Definitions

			public Object _at_(Object receiver, Object slotIndexObject) {
				var array = (ESString)receiver;
				var slots = array.IndexedSlots;
				var slotIndex = ((long)slotIndexObject) - 1;
				return (char)slots[slotIndex];
			}

			public Object _atPut_(Object receiver, Object slotIndexObject, Object newValue) {
				var array = (ESIndexedComparableSlotsObject<char>)receiver;
				if (array.IsImmutable) throw new ImmutableObjectException();
				var slots = array.IndexedSlots;
				var slotIndex = ((long)slotIndexObject) - 1;
				slots[slotIndex] = (char)newValue;
				return newValue;
			}
		
			public Object _atFirstPutOrPrepend_(Object receiver, Object newValue) {
				return ((ESString)receiver).atFirstPutOrPrepend((char)newValue);
			}
		
			public Object _atLastPutOrAppend_(Object receiver, Object newValue) {
				return ((ESString)receiver).atLastPutOrAppend((char)newValue);
			}
		
			public Object _elementsDo_(Object receiver, Object enumerator) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator);
				((ESString)receiver).elementsDo(value => f1(value));
				return receiver;
			}

			public Object _elementsFromToDo_(Object receiver, Object startIndex, Object endIndex, Object enumerator) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator);
				((ESString)receiver).elementsFromToDo((long)startIndex - 1, (long)endIndex - 1, value => f1(value));
				return receiver;
			}
		
			public Object _elementsFromToByDo_(Object receiver, Object startIndex, Object endIndex, Object step, Object enumerator) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator);
				((ESString)receiver).elementsFromToByDo((long)startIndex - 1, (long)endIndex - 1, asHostLong(step), value => f1(value));
				return receiver;
			}
		
			public Object _asNamespace_ (Object receiver) {
				return ((ESString)receiver).asESNamespace();
			}
		
			public Object _asPathname_ (Object receiver) {
				return ((ESString)receiver).asESPathname();
			}
		
			public Object _asPathnameUsingSeparator_ (Object receiver, Object separatorObject) {
				if (separatorObject is char) {
					var separatorCharacter = (char)separatorObject;
					return ((ESString)receiver).asESPathnameUsingSeparator(separatorCharacter);
				} else {
					var separatorString = asHostCharArray(separatorObject);
					return ((ESString)receiver).asESPathnameUsingSeparators(separatorString);
				}
			}
		
			public Object _bindingInNamespaceIfAbsent_ (Object receiver, Object environment, Object importTransitivity, Object ifAbsentAction) {
				ImportTransitivity transitivity;
				try {
					transitivity = (ImportTransitivity)Enum.Parse(typeof(ImportTransitivity), kernel.asESSymbol(importTransitivity));
				} catch {
					throw new PrimInvalidOperandException("valueInNamespaceIfAbsent: <importTransitivity> must be a Symbol or String identifying a valid import transitivity.");
				}
				var binding = ((ESString)receiver).bindingInNamespaceIfAbsent((NamespaceObject)environment, AccessPrivilegeLevel.Public, transitivity, null);
				if (binding == null) return asFunctor0(ifAbsentAction)();
				return binding;
			}
		
			public Object _valueInNamespaceIfAbsent_ (Object receiver, Object environment, Object importTransitivity, Object ifAbsentAction) {
				ImportTransitivity transitivity;
				try {
					transitivity = (ImportTransitivity)Enum.Parse(typeof(ImportTransitivity), kernel.asESSymbol(importTransitivity));
				} catch {
					throw new PrimInvalidOperandException("valueInNamespaceIfAbsent: <importTransitivity> must be a Symbol or String identifying a valid import transitivity.");
				}
				return ((ESString)receiver).valueInNamespaceIfAbsent((NamespaceObject)environment, AccessPrivilegeLevel.Public, transitivity, asFunctor0(ifAbsentAction));
			}
		
			public Object _asBindingReferenceKeyWithValue_ (Object receiver, Object value) {
				return kernel.newBindingReference(asHostString(receiver), value);
			}


					
			public Object _asHostSystemType_(Object receiver) {
				var esString = (ESString)receiver;
				var stringValue = esString.asHostString();
				Type hostSystemType;
				try {
					hostSystemType = ESBehavior.typeFromAssemblyQualifiedName(stringValue, true);
				} catch (Exception ex) {
					throw new PrimitiveFailException(ex);
				}
				return hostSystemType;
			}

			#endregion

			public override void publishCanonicalPrimitives() {

				publishPrimitive("hash",						new FuncNs.Func<Object, Object>(_hash_));
				publishPrimitive("=",								new FuncNs.Func<Object, Object, Object>(_hasSameValueAs_));
				publishPrimitive("compareTo:",							new FuncNs.Func<Object, Object, Object>(_compareTo_));

				publishPrimitive("at:",								new FuncNs.Func<Object, Object, Object>(_at_));

				publishPrimitive("do:",								new FuncNs.Func<Object, Object, Object>(_elementsDo_));
				publishPrimitive("from:to:do:",							new FuncNs.Func<Object, Object, Object, Object, Object>(_elementsFromToDo_));
				publishPrimitive("from:to:by:do:",						new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_elementsFromToByDo_));

				publishPrimitive("at:put:",							new FuncNs.Func<Object, Object, Object, Object>(_atPut_));
				publishPrimitive("nextIdentityIndexOf:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIdentityIndexOfIfAbsent_<char>));
				publishPrimitive("identityIncludes:",						new FuncNs.Func<Object, Object, Object>(_identityIncludes_<char>));
				publishPrimitive("add:",							new FuncNs.Func<Object, Object, Object>(_appendElement_<char>));
				publishPrimitive("copyWith:",							new FuncNs.Func<Object, Object, Object>(_copyAppendingElement_<char>));
				publishPrimitive("identityRemoveNext:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_identityRemoveNextIfAbsent_<char>));	
				publishPrimitive("copyIdentityRemovingNext:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyIdentityRemovingNextIfAbsent_<char>));	
				publishPrimitive("identityRemoveAll:",						new FuncNs.Func<Object, Object, Object>(_identityRemoveAllOccurrencesOf_<char>));	
				publishPrimitive("copyIdentityRemovingAll:",					new FuncNs.Func<Object, Object, Object>(_identityRemoveAllOccurrencesOf_<char>));	
				publishPrimitive("add:beforeIndex:",						new FuncNs.Func<Object, Object, Object, Object>(_insertElementAt_<char>));
				publishPrimitive("copyAdding:beforeIndex:",					new FuncNs.Func<Object, Object, Object, Object>(_copyInstertingElementAt_<char>));

				publishPrimitive("nextIndexOf:from:to:ifAbsent:",				new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIndexOfIfAbsent_<char>));
				publishPrimitive("includes:",							new FuncNs.Func<Object, Object, Object>(_includes_<char>));
				publishPrimitive("removeNext:from:to:ifAbsent:",				new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_removeNextIfAbsent_<char>));	
				publishPrimitive("copyRemovingNext:from:to:ifAbsent:",				new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyRemovingNextIfAbsent_<char>));	
				publishPrimitive("removeAll:",							new FuncNs.Func<Object, Object, Object>(_removeAllOccurrencesOf_<char>));
				publishPrimitive("copyRemovingAll:", /* #copyWithout: */			new FuncNs.Func<Object, Object, Object>(_copyRemovingAllOccurrencesOf_<char>));

				publishPrimitive("atFirstPutOrAdd:",						new FuncNs.Func<Object, Object, Object>(_atFirstPutOrPrepend_));
				publishPrimitive("atLastPutOrAdd:",						new FuncNs.Func<Object, Object, Object>(_atLastPutOrAppend_));

				publishPrimitive("asPathname",							new FuncNs.Func<Object, Object>(_asPathname_));
				publishPrimitive("asPathname:",							new FuncNs.Func<Object, Object, Object>(_asPathnameUsingSeparator_));
				publishPrimitive("asNamespace",							new FuncNs.Func<Object, Object>(_asNamespace_));
				publishPrimitive("asHostSystemType",						new FuncNs.Func<Object, Object>(_asHostSystemType_));

				publishPrimitive("=>",								new FuncNs.Func<Object, Object, Object>(_asBindingReferenceKeyWithValue_));
				publishPrimitive("bindingInNamespace:transitivity:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object>(_bindingInNamespaceIfAbsent_));
				publishPrimitive("valueInNamespace:transitivity:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object>(_valueInNamespaceIfAbsent_));

				publishPrimitive("setSize:",							new FuncNs.Func<Object, Object, Object>(_setSize_));
				publishPrimitive("copyWithSize:",						new FuncNs.Func<Object, Object, Object>(_copyWithSize_));
				publishPrimitive("firstIfNone:",						new FuncNs.Func<Object, Object, Object>(_firstIfNone_));
				publishPrimitive("lastIfNone:",							new FuncNs.Func<Object, Object, Object>(_lastIfNone_));
				publishPrimitive("findtFirst:from:to:ifAbsent:",				new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIndexSuchThatIfAbsent_));
				publishPrimitive("contains:",							new FuncNs.Func<Object, Object, Object>(_contains_));
				publishPrimitive("addAll:",							new FuncNs.Func<Object, Object, Object>(_appendAll_));
				publishPrimitive(",",								new FuncNs.Func<Object, Object, Object>(_copyAppendingAll_));
				publishPrimitive("copyFrom:to:",						new FuncNs.Func<Object, Object, Object, Object>(_copyFromTo_));
				publishPrimitive("copyTo:",							new FuncNs.Func<Object, Object, Object>(_prefixTo_));
				publishPrimitive("copyFrom:",							new FuncNs.Func<Object, Object, Object>(_suffixFrom_));
				publishPrimitive("withAllButFirst",						new FuncNs.Func<Object, Object>(_withAllButFirst_));
				publishPrimitive("withAllButLast",						new FuncNs.Func<Object, Object>(_withAllButLast_));
				publishPrimitive("withFirst",							new FuncNs.Func<Object, Object>(_withFirst_));
				publishPrimitive("withLast",							new FuncNs.Func<Object, Object>(_withLast_));

				publishPrimitive("removeAt:",							new FuncNs.Func<Object, Object, Object>(_removeAt_));		// "Delete index"
				publishPrimitive("copyRemovingAt:",						new FuncNs.Func<Object, Object, Object>(_copyRemovingAt_));	// "Copy without index"
				publishPrimitive("copyAndRemoveFrom:to:",					new FuncNs.Func<Object, Object, Object, Object>(_copyAndRemoveFromTo_));	// "Cut range"
				publishPrimitive("copyRemovingFrom:to:",					new FuncNs.Func<Object, Object, Object, Object>(_copyRemovingFromTo_));	// "Copy without range"
				publishPrimitive("removeNextSuchThat:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_removeNextSuchThatIfAbsent_));
				publishPrimitive("copyRemovingNextSuchThat:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyRemovingNextSuchThatIfAbsent_));
				publishPrimitive("removeAllSuchThat:",						new FuncNs.Func<Object, Object, Object>(_removeAllOccurrencesSuchThat_));
				publishPrimitive("copyRemovingAllSuchThat:",					new FuncNs.Func<Object, Object, Object>(_copyRemovingAllSuchThat_));
				publishPrimitive("addAll:beforeIndex:",						new FuncNs.Func<Object, Object, Object, Object>(_insertAllAt_));
				publishPrimitive("copyAddingAll:beforeIndex:",					new FuncNs.Func<Object, Object, Object, Object>(_copyInsertingAllAt_));
				publishPrimitive("moveFrom:to:by:",						new FuncNs.Func<Object, Object, Object, Object, Object>(_moveFromToBy_)); // Cut then paste range
				publishPrimitive("copyMovingFrom:to:by:",					new FuncNs.Func<Object, Object, Object, Object, Object>(_copyMovingFromToBy_)); // Cut then paste range
				publishPrimitive("replaceFrom:to:with:startingAt:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_replaceFromToWithStartingAt_)); 
				publishPrimitive("copyReplacingFrom:to:with:startingAt:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyReplacingFromToWithStartingAt_)); 
				publishPrimitive("reverse",							new FuncNs.Func<Object, Object>(_reverse_)); 
				publishPrimitive("copyReversed",						new FuncNs.Func<Object, Object>(_copyReversed_)); 
				publishPrimitive("reverseFrom:to:",						new FuncNs.Func<Object, Object, Object, Object>(_reverseFromTo_)); 
				publishPrimitive("copyReversedFrom:to:",					new FuncNs.Func<Object, Object, Object, Object>(_copyReversedFromTo_));

			}

		}
		
	}
	
	public class ESHalfWordArray : ESIndexedComparableSlotsObject<ushort> {
		
		#region Static variables and functions
		
		public static implicit operator ushort[](ESHalfWordArray array) {
			return array.IndexedSlots;  
		}
		
		#endregion
		
		public ESHalfWordArray(ESBehavior esClass) : base(esClass, 0) {
		}
		
		public ESHalfWordArray(ESBehavior esClass, long size) : base(esClass, size) {
		}
		
		public ESHalfWordArray(ESBehavior esClass, ushort[] slots) : base(esClass, slots) {
		}
		
		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.IndexedHalfWordSlots;}
		}
		
		public override ESArray asESArray() {
			return Class.Kernel.newArray(Array.ConvertAll<ushort, Object>(IndexedSlots, value => (long)value));
		}
		
		public override ESHalfWordArray asESHalfWordArray() {
			return this;
		}

		protected override int foreignCompareTo(Object comparand) {
			ushort[] primArray = comparand as ushort[];
			if (primArray == null) {
				IComparable comparable = comparand as IComparable;
				if (comparable == null) Class.Kernel.throwInvalidArgumentException(Class, "foreignCompareTo", "comparand", comparand);
				return -Math.Sign(comparable.CompareTo(IndexedSlots));
			}
			return ESIndexedComparableSlotsObject<ushort>.compare(IndexedSlots, primArray);
		}

		public override T valueBy<T>(Operation<T> operation) {
		    return operation.applyToIndexedHalfWordSlotsObject(this);
		}

		public new class Primitives : ESIndexedSlotsObject<ushort>.Primitives {

			protected override void bindToKernel() {
				domainClass = kernel.HalfWordArrayClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.IndexedHalfWordSlots;}
			}

			#region Primitive Definitions

			public Object _at_(Object receiver, Object slotIndexObject) {
				var array = (ESIndexedComparableSlotsObject<ushort>)receiver;
				var slots = array.IndexedSlots;
				var slotIndex = ((long)slotIndexObject) - 1;
				return (long)slots[slotIndex];
			}

			public Object _atPut_(Object receiver, Object slotIndexObject, Object newValue) {
				var array = (ESIndexedComparableSlotsObject<ushort>)receiver;
				if (array.IsImmutable) throw new ImmutableObjectException();
				var slots = array.IndexedSlots;
				var slotIndex = ((long)slotIndexObject) - 1;
				slots[slotIndex] = (ushort)newValue;
				return newValue;
			}
		
			public Object _atFirstPutOrPrepend_(Object receiver, Object newValue) {
				return ((ESIndexedComparableSlotsObject<ushort>)receiver).atFirstPutOrPrepend(asHostHalfWord(newValue));
			}
		
			public Object _atLastPutOrAppend_(Object receiver, Object newValue) {
				return ((ESIndexedComparableSlotsObject<ushort>)receiver).atLastPutOrAppend(asHostHalfWord(newValue));
			}
		
			public Object _elementsDo_(Object receiver, Object enumerator) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator);
				((ESIndexedComparableSlotsObject<ushort>)receiver).elementsDo(value => f1((long)value));
				return receiver;
			}

			public Object _elementsFromToDo_(Object receiver, Object startIndex, Object endIndex, Object enumerator) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator);
				((ESIndexedComparableSlotsObject<ushort>)receiver).elementsFromToDo((long)startIndex - 1, (long)endIndex - 1, value => f1((long)value));
				return receiver;
			}
		
			public Object _elementsFromToByDo_(Object receiver, Object startIndex, Object endIndex, Object step, Object enumerator) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator);
				((ESIndexedComparableSlotsObject<ushort>)receiver).elementsFromToByDo((long)startIndex - 1, (long)endIndex - 1, asHostLong(step), value => f1((long)value));
				return receiver;
			}

			#endregion

			public override void publishCanonicalPrimitives() {

				publishPrimitive("hash",						new FuncNs.Func<Object, Object>(_hash_));
				publishPrimitive("=",							new FuncNs.Func<Object, Object, Object>(_hasSameValueAs_));
				publishPrimitive("compareTo:",						new FuncNs.Func<Object, Object, Object>(_compareTo_));

				publishPrimitive("at:",							new FuncNs.Func<Object, Object, Object>(_at_));

				publishPrimitive("do:",							new FuncNs.Func<Object, Object, Object>(_elementsDo_));
				publishPrimitive("from:to:do:",						new FuncNs.Func<Object, Object, Object, Object, Object>(_elementsFromToDo_));
				publishPrimitive("from:to:by:do:",					new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_elementsFromToByDo_));

				publishPrimitive("at:put:",						new FuncNs.Func<Object, Object, Object, Object>(_atPut_));
				publishPrimitive("nextIdentityIndexOf:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIdentityIndexOfIfAbsent_<ushort>));
				publishPrimitive("identityIncludes:",					new FuncNs.Func<Object, Object, Object>(_identityIncludes_<ushort>));
				publishPrimitive("add:",						new FuncNs.Func<Object, Object, Object>(_appendElement_<ushort>));
				publishPrimitive("copyWith:",						new FuncNs.Func<Object, Object, Object>(_copyAppendingElement_<ushort>));
				publishPrimitive("identityRemoveNext:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_identityRemoveNextIfAbsent_<ushort>));	
				publishPrimitive("copyIdentityRemovingNext:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyIdentityRemovingNextIfAbsent_<ushort>));	
				publishPrimitive("identityRemoveAll:",					new FuncNs.Func<Object, Object, Object>(_identityRemoveAllOccurrencesOf_<ushort>));	
				publishPrimitive("copyIdentityRemovingAll:",				new FuncNs.Func<Object, Object, Object>(_identityRemoveAllOccurrencesOf_<ushort>));	
				publishPrimitive("add:beforeIndex:",					new FuncNs.Func<Object, Object, Object, Object>(_insertElementAt_<ushort>));
				publishPrimitive("copyAdding:beforeIndex:",				new FuncNs.Func<Object, Object, Object, Object>(_copyInstertingElementAt_<ushort>));

				publishPrimitive("nextIndexOf:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIndexOfIfAbsent_<ushort>));
				publishPrimitive("includes:",						new FuncNs.Func<Object, Object, Object>(_includes_<ushort>));
				publishPrimitive("removeNext:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_removeNextIfAbsent_<ushort>));	
				publishPrimitive("copyRemovingNext:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyRemovingNextIfAbsent_<ushort>));	
				publishPrimitive("removeAll:",						new FuncNs.Func<Object, Object, Object>(_removeAllOccurrencesOf_<ushort>));
				publishPrimitive("copyRemovingAll:", /* #copyWithout: */		new FuncNs.Func<Object, Object, Object>(_copyRemovingAllOccurrencesOf_<ushort>));

				publishPrimitive("atFirstPutOrAdd:",					new FuncNs.Func<Object, Object, Object>(_atFirstPutOrPrepend_));
				publishPrimitive("atLastPutOrAdd:",					new FuncNs.Func<Object, Object, Object>(_atLastPutOrAppend_));

				publishPrimitive("setSize:",						new FuncNs.Func<Object, Object, Object>(_setSize_));
				publishPrimitive("copyWithSize:",					new FuncNs.Func<Object, Object, Object>(_copyWithSize_));
				publishPrimitive("firstIfNone:",					new FuncNs.Func<Object, Object, Object>(_firstIfNone_));
				publishPrimitive("lastIfNone:",						new FuncNs.Func<Object, Object, Object>(_lastIfNone_));
				publishPrimitive("findtFirst:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIndexSuchThatIfAbsent_));
				publishPrimitive("contains:",						new FuncNs.Func<Object, Object, Object>(_contains_));
				publishPrimitive("addAll:",						new FuncNs.Func<Object, Object, Object>(_appendAll_));
				publishPrimitive(",",							new FuncNs.Func<Object, Object, Object>(_copyAppendingAll_));
				publishPrimitive("copyFrom:to:",					new FuncNs.Func<Object, Object, Object, Object>(_copyFromTo_));
				publishPrimitive("copyTo:",						new FuncNs.Func<Object, Object, Object>(_prefixTo_));
				publishPrimitive("copyFrom:",						new FuncNs.Func<Object, Object, Object>(_suffixFrom_));
				publishPrimitive("withAllButFirst",					new FuncNs.Func<Object, Object>(_withAllButFirst_));
				publishPrimitive("withAllButLast",					new FuncNs.Func<Object, Object>(_withAllButLast_));
				publishPrimitive("withFirst",						new FuncNs.Func<Object, Object>(_withFirst_));
				publishPrimitive("withLast",						new FuncNs.Func<Object, Object>(_withLast_));

				publishPrimitive("removeAt:",						new FuncNs.Func<Object, Object, Object>(_removeAt_));		// "Delete index"
				publishPrimitive("copyRemovingAt:",					new FuncNs.Func<Object, Object, Object>(_copyRemovingAt_));	// "Copy without index"
				publishPrimitive("copyAndRemoveFrom:to:",				new FuncNs.Func<Object, Object, Object, Object>(_copyAndRemoveFromTo_));	// "Cut range"
				publishPrimitive("copyRemovingFrom:to:",				new FuncNs.Func<Object, Object, Object, Object>(_copyRemovingFromTo_));	// "Copy without range"
				publishPrimitive("removeNextSuchThat:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_removeNextSuchThatIfAbsent_));
				publishPrimitive("copyRemovingNextSuchThat:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyRemovingNextSuchThatIfAbsent_));
				publishPrimitive("removeAllSuchThat:",					new FuncNs.Func<Object, Object, Object>(_removeAllOccurrencesSuchThat_));
				publishPrimitive("copyRemovingAllSuchThat:",				new FuncNs.Func<Object, Object, Object>(_copyRemovingAllSuchThat_));
				publishPrimitive("addAll:beforeIndex:",					new FuncNs.Func<Object, Object, Object, Object>(_insertAllAt_));
				publishPrimitive("copyAddingAll:beforeIndex:",				new FuncNs.Func<Object, Object, Object, Object>(_copyInsertingAllAt_));
				publishPrimitive("moveFrom:to:by:",					new FuncNs.Func<Object, Object, Object, Object, Object>(_moveFromToBy_)); // Cut then paste range
				publishPrimitive("copyMovingFrom:to:by:",				new FuncNs.Func<Object, Object, Object, Object, Object>(_copyMovingFromToBy_)); // Cut then paste range
				publishPrimitive("replaceFrom:to:with:startingAt:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_replaceFromToWithStartingAt_)); 
				publishPrimitive("copyReplacingFrom:to:with:startingAt:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyReplacingFromToWithStartingAt_)); 
				publishPrimitive("reverse",						new FuncNs.Func<Object, Object>(_reverse_)); 
				publishPrimitive("copyReversed",					new FuncNs.Func<Object, Object>(_copyReversed_)); 
				publishPrimitive("reverseFrom:to:",					new FuncNs.Func<Object, Object, Object, Object>(_reverseFromTo_)); 
				publishPrimitive("copyReversedFrom:to:",				new FuncNs.Func<Object, Object, Object, Object>(_copyReversedFromTo_));

			}

		}
		
	}
	
	public class ESWordArray : ESIndexedComparableSlotsObject<uint> {
		
		#region Static variables and functions
		
		public static implicit operator uint[](ESWordArray array) {
			return array.IndexedSlots;  
		}
		
		#endregion

		public ESWordArray(ESBehavior esClass) : base(esClass, 0) {
		}

		public ESWordArray(ESBehavior esClass, long size) : base(esClass, size) {
		}
		
		public ESWordArray(ESBehavior esClass, uint[] slots) : base(esClass, slots) {
		}
		
		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.IndexedWordSlots;}
		}
		
		public override ESArray asESArray() {
			return Class.Kernel.newArray(Array.ConvertAll<uint, Object>(IndexedSlots, value => (long)value));
		}
		
		public override ESWordArray asESWordArray() {
			return this;
		}

		protected override int foreignCompareTo(Object comparand) {
			uint[] primArray = comparand as uint[];
			if (primArray == null) {
				IComparable comparable = comparand as IComparable;
				if (comparable == null) Class.Kernel.throwInvalidArgumentException(Class, "foreignCompareTo", "comparand", comparand);
				return -Math.Sign(comparable.CompareTo(IndexedSlots));
			}
			return ESIndexedComparableSlotsObject<uint>.compare(IndexedSlots, primArray);
		}

		public override T valueBy<T>(Operation<T> operation) {
		    return operation.applyToIndexedWordSlotsObject(this);
		}

		public new class Primitives : ESIndexedSlotsObject<uint>.Primitives {

			protected override void bindToKernel() {
				domainClass = kernel.WordArrayClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.IndexedWordSlots;}
			}

			#region Primitive Definitions

			public Object _at_(Object receiver, Object slotIndexObject) {
				var array = (ESIndexedComparableSlotsObject<uint>)receiver;
				var slots = array.IndexedSlots;
				var slotIndex = ((long)slotIndexObject) - 1;
				return (long)slots[slotIndex];
			}

			public Object _atPut_(Object receiver, Object slotIndexObject, Object newValue) {
				var array = (ESIndexedComparableSlotsObject<uint>)receiver;
				if (array.IsImmutable) throw new ImmutableObjectException();
				var slots = array.IndexedSlots;
				var slotIndex = ((long)slotIndexObject) - 1;
				slots[slotIndex] = (uint)newValue;
				return newValue;
			}
		
			public Object _atFirstPutOrPrepend_(Object receiver, Object newValue) {
				return ((ESIndexedComparableSlotsObject<uint>)receiver).atFirstPutOrPrepend(asHostWord(newValue));
			}
		
			public Object _atLastPutOrAppend_(Object receiver, Object newValue) {
				return ((ESIndexedComparableSlotsObject<uint>)receiver).atLastPutOrAppend(asHostWord(newValue));
			}
		
			public Object _elementsDo_(Object receiver, Object enumerator) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator);
				((ESIndexedComparableSlotsObject<uint>)receiver).elementsDo(value => f1((long)value));
				return receiver;
			}

			public Object _elementsFromToDo_(Object receiver, Object startIndex, Object endIndex, Object enumerator) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator);
				((ESIndexedComparableSlotsObject<uint>)receiver).elementsFromToDo((long)startIndex - 1, (long)endIndex - 1, value => f1((long)value));
				return receiver;
			}
		
			public Object _elementsFromToByDo_(Object receiver, Object startIndex, Object endIndex, Object step, Object enumerator) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator);
				((ESIndexedComparableSlotsObject<uint>)receiver).elementsFromToByDo((long)startIndex - 1, (long)endIndex - 1, asHostLong(step), value => f1((long)value));
				return receiver;
			}

			#endregion

			public override void publishCanonicalPrimitives() {

				publishPrimitive("hash",						new FuncNs.Func<Object, Object>(_hash_));
				publishPrimitive("=",							new FuncNs.Func<Object, Object, Object>(_hasSameValueAs_));
				publishPrimitive("compareTo:",						new FuncNs.Func<Object, Object, Object>(_compareTo_));

				publishPrimitive("at:",							new FuncNs.Func<Object, Object, Object>(_at_));

				publishPrimitive("do:",							new FuncNs.Func<Object, Object, Object>(_elementsDo_));
				publishPrimitive("from:to:do:",						new FuncNs.Func<Object, Object, Object, Object, Object>(_elementsFromToDo_));
				publishPrimitive("from:to:by:do:",					new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_elementsFromToByDo_));

				publishPrimitive("at:put:",						new FuncNs.Func<Object, Object, Object, Object>(_atPut_));
				publishPrimitive("nextIdentityIndexOf:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIdentityIndexOfIfAbsent_<uint>));
				publishPrimitive("identityIncludes:",					new FuncNs.Func<Object, Object, Object>(_identityIncludes_<uint>));
				publishPrimitive("add:",						new FuncNs.Func<Object, Object, Object>(_appendElement_<uint>));
				publishPrimitive("copyWith:",						new FuncNs.Func<Object, Object, Object>(_copyAppendingElement_<uint>));
				publishPrimitive("identityRemoveNext:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_identityRemoveNextIfAbsent_<uint>));	
				publishPrimitive("copyIdentityRemovingNext:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyIdentityRemovingNextIfAbsent_<uint>));	
				publishPrimitive("identityRemoveAll:",					new FuncNs.Func<Object, Object, Object>(_identityRemoveAllOccurrencesOf_<uint>));	
				publishPrimitive("copyIdentityRemovingAll:",				new FuncNs.Func<Object, Object, Object>(_identityRemoveAllOccurrencesOf_<uint>));	
				publishPrimitive("add:beforeIndex:",					new FuncNs.Func<Object, Object, Object, Object>(_insertElementAt_<uint>));
				publishPrimitive("copyAdding:beforeIndex:",				new FuncNs.Func<Object, Object, Object, Object>(_copyInstertingElementAt_<uint>));

				publishPrimitive("nextIndexOf:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIndexOfIfAbsent_<uint>));
				publishPrimitive("includes:",						new FuncNs.Func<Object, Object, Object>(_includes_<uint>));
				publishPrimitive("removeNext:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_removeNextIfAbsent_<uint>));	
				publishPrimitive("copyRemovingNext:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyRemovingNextIfAbsent_<uint>));	
				publishPrimitive("removeAll:",						new FuncNs.Func<Object, Object, Object>(_removeAllOccurrencesOf_<uint>));
				publishPrimitive("copyRemovingAll:", /* #copyWithout: */		new FuncNs.Func<Object, Object, Object>(_copyRemovingAllOccurrencesOf_<uint>));

				publishPrimitive("atFirstPutOrAdd:",					new FuncNs.Func<Object, Object, Object>(_atFirstPutOrPrepend_));
				publishPrimitive("atLastPutOrAdd:",					new FuncNs.Func<Object, Object, Object>(_atLastPutOrAppend_));

				publishPrimitive("setSize:",						new FuncNs.Func<Object, Object, Object>(_setSize_));
				publishPrimitive("copyWithSize:",					new FuncNs.Func<Object, Object, Object>(_copyWithSize_));
				publishPrimitive("firstIfNone:",					new FuncNs.Func<Object, Object, Object>(_firstIfNone_));
				publishPrimitive("lastIfNone:",						new FuncNs.Func<Object, Object, Object>(_lastIfNone_));
				publishPrimitive("findtFirst:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIndexSuchThatIfAbsent_));
				publishPrimitive("contains:",						new FuncNs.Func<Object, Object, Object>(_contains_));
				publishPrimitive("addAll:",						new FuncNs.Func<Object, Object, Object>(_appendAll_));
				publishPrimitive(",",							new FuncNs.Func<Object, Object, Object>(_copyAppendingAll_));
				publishPrimitive("copyFrom:to:",					new FuncNs.Func<Object, Object, Object, Object>(_copyFromTo_));
				publishPrimitive("copyTo:",						new FuncNs.Func<Object, Object, Object>(_prefixTo_));
				publishPrimitive("copyFrom:",						new FuncNs.Func<Object, Object, Object>(_suffixFrom_));
				publishPrimitive("withAllButFirst",					new FuncNs.Func<Object, Object>(_withAllButFirst_));
				publishPrimitive("withAllButLast",					new FuncNs.Func<Object, Object>(_withAllButLast_));
				publishPrimitive("withFirst",						new FuncNs.Func<Object, Object>(_withFirst_));
				publishPrimitive("withLast",						new FuncNs.Func<Object, Object>(_withLast_));

				publishPrimitive("removeAt:",						new FuncNs.Func<Object, Object, Object>(_removeAt_));		// "Delete index"
				publishPrimitive("copyRemovingAt:",					new FuncNs.Func<Object, Object, Object>(_copyRemovingAt_));	// "Copy without index"
				publishPrimitive("copyAndRemoveFrom:to:",				new FuncNs.Func<Object, Object, Object, Object>(_copyAndRemoveFromTo_));	// "Cut range"
				publishPrimitive("copyRemovingFrom:to:",				new FuncNs.Func<Object, Object, Object, Object>(_copyRemovingFromTo_));	// "Copy without range"
				publishPrimitive("removeNextSuchThat:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_removeNextSuchThatIfAbsent_));
				publishPrimitive("copyRemovingNextSuchThat:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyRemovingNextSuchThatIfAbsent_));
				publishPrimitive("removeAllSuchThat:",					new FuncNs.Func<Object, Object, Object>(_removeAllOccurrencesSuchThat_));
				publishPrimitive("copyRemovingAllSuchThat:",				new FuncNs.Func<Object, Object, Object>(_copyRemovingAllSuchThat_));
				publishPrimitive("addAll:beforeIndex:",					new FuncNs.Func<Object, Object, Object, Object>(_insertAllAt_));
				publishPrimitive("copyAddingAll:beforeIndex:",				new FuncNs.Func<Object, Object, Object, Object>(_copyInsertingAllAt_));
				publishPrimitive("moveFrom:to:by:",					new FuncNs.Func<Object, Object, Object, Object, Object>(_moveFromToBy_)); // Cut then paste range
				publishPrimitive("copyMovingFrom:to:by:",				new FuncNs.Func<Object, Object, Object, Object, Object>(_copyMovingFromToBy_)); // Cut then paste range
				publishPrimitive("replaceFrom:to:with:startingAt:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_replaceFromToWithStartingAt_)); 
				publishPrimitive("copyReplacingFrom:to:with:startingAt:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyReplacingFromToWithStartingAt_)); 
				publishPrimitive("reverse",						new FuncNs.Func<Object, Object>(_reverse_)); 
				publishPrimitive("copyReversed",					new FuncNs.Func<Object, Object>(_copyReversed_)); 
				publishPrimitive("reverseFrom:to:",					new FuncNs.Func<Object, Object, Object, Object>(_reverseFromTo_)); 
				publishPrimitive("copyReversedFrom:to:",				new FuncNs.Func<Object, Object, Object, Object>(_copyReversedFromTo_));

			}

		}
		
	}
	
	public class ESLongWordArray : ESIndexedComparableSlotsObject<ulong> {
		
		#region Static variables and functions

		public static implicit operator ulong[](ESLongWordArray array) {
			return array.IndexedSlots;  
		}
		
		#endregion

		public ESLongWordArray(ESBehavior esClass) : base(esClass, 0) {
		}

		public ESLongWordArray(ESBehavior esClass, long size) : base(esClass, size) {
		}
		
		public ESLongWordArray(ESBehavior esClass, ulong[] slots) : base(esClass, slots) {
		}
		
		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.IndexedLongWordSlots;}
		}
		
		public override ESArray asESArray() {
			return Class.Kernel.newArray(Array.ConvertAll<ulong, Object>(IndexedSlots, value => (long)value));
		}
		
		public override ESLongWordArray asESLongWordArray() {
			return this;
		}

		protected override int foreignCompareTo(Object comparand) {
			ulong[] primArray = comparand as ulong[];
			if (primArray == null) {
				IComparable comparable = comparand as IComparable;
				if (comparable == null) Class.Kernel.throwInvalidArgumentException(Class, "foreignCompareTo", "comparand", comparand);
				return -Math.Sign(comparable.CompareTo(IndexedSlots));
			}
			return ESIndexedComparableSlotsObject<ulong>.compare(IndexedSlots, primArray);
		}

		public override T valueBy<T>(Operation<T> operation) {
		    return operation.applyToIndexedLongWordSlotsObject(this);
		}

		public new class Primitives : ESIndexedSlotsObject<ulong>.Primitives {

			protected override void bindToKernel() {
				domainClass = kernel.LongWordArrayClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.IndexedLongWordSlots;}
			}

			#region Primitive Definitions

			public Object _at_(Object receiver, Object slotIndexObject) {
				var array = (ESIndexedComparableSlotsObject<ulong>)receiver;
				var slots = array.IndexedSlots;
				var slotIndex = ((long)slotIndexObject) - 1;
				return (long)slots[slotIndex];
			}

			public Object _atPut_(Object receiver, Object slotIndexObject, Object newValue) {
				var array = (ESIndexedComparableSlotsObject<ulong>)receiver;
				if (array.IsImmutable) throw new ImmutableObjectException();
				var slots = array.IndexedSlots;
				var slotIndex = ((long)slotIndexObject) - 1;
				slots[slotIndex] = (ulong)newValue;
				return newValue;
			}
		
			public Object _atFirstPutOrPrepend_(Object receiver, Object newValue) {
				return ((ESIndexedComparableSlotsObject<ulong>)receiver).atFirstPutOrPrepend(asHostLongWord(newValue));
			}
		
			public Object _atLastPutOrAppend_(Object receiver, Object newValue) {
				return ((ESIndexedComparableSlotsObject<ulong>)receiver).atLastPutOrAppend(asHostLongWord(newValue));
			}
		
			public Object _elementsDo_(Object receiver, Object enumerator) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator);
				((ESIndexedComparableSlotsObject<ulong>)receiver).elementsDo(value => f1((long)value));
				return receiver;
			}

			public Object _elementsFromToDo_(Object receiver, Object startIndex, Object endIndex, Object enumerator) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator);
				((ESIndexedComparableSlotsObject<ulong>)receiver).elementsFromToDo((long)startIndex - 1, (long)endIndex - 1, value => f1((long)value));
				return receiver;
			}
		
			public Object _elementsFromToByDo_(Object receiver, Object startIndex, Object endIndex, Object step, Object enumerator) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator);
				((ESIndexedComparableSlotsObject<ulong>)receiver).elementsFromToByDo((long)startIndex - 1, (long)endIndex - 1, asHostLong(step), value => f1((long)value));
				return receiver;
			}

			#endregion	

			public override void publishCanonicalPrimitives() {

				publishPrimitive("hash",						new FuncNs.Func<Object, Object>(_hash_));
				publishPrimitive("=",							new FuncNs.Func<Object, Object, Object>(_hasSameValueAs_));
				publishPrimitive("compareTo:",						new FuncNs.Func<Object, Object, Object>(_compareTo_));

				publishPrimitive("at:",							new FuncNs.Func<Object, Object, Object>(_at_));

				publishPrimitive("do:",							new FuncNs.Func<Object, Object, Object>(_elementsDo_));
				publishPrimitive("from:to:do:",						new FuncNs.Func<Object, Object, Object, Object, Object>(_elementsFromToDo_));
				publishPrimitive("from:to:by:do:",					new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_elementsFromToByDo_));

				publishPrimitive("at:put:",						new FuncNs.Func<Object, Object, Object, Object>(_atPut_));
				publishPrimitive("nextIdentityIndexOf:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIdentityIndexOfIfAbsent_<ulong>));
				publishPrimitive("identityIncludes:",					new FuncNs.Func<Object, Object, Object>(_identityIncludes_<ulong>));
				publishPrimitive("add:",						new FuncNs.Func<Object, Object, Object>(_appendElement_<ulong>));
				publishPrimitive("copyWith:",						new FuncNs.Func<Object, Object, Object>(_copyAppendingElement_<ulong>));
				publishPrimitive("identityRemoveNext:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_identityRemoveNextIfAbsent_<ulong>));	
				publishPrimitive("copyIdentityRemovingNext:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyIdentityRemovingNextIfAbsent_<ulong>));	
				publishPrimitive("identityRemoveAll:",					new FuncNs.Func<Object, Object, Object>(_identityRemoveAllOccurrencesOf_<ulong>));	
				publishPrimitive("copyIdentityRemovingAll:",				new FuncNs.Func<Object, Object, Object>(_identityRemoveAllOccurrencesOf_<ulong>));	
				publishPrimitive("add:beforeIndex:",					new FuncNs.Func<Object, Object, Object, Object>(_insertElementAt_<ulong>));
				publishPrimitive("copyAdding:beforeIndex:",				new FuncNs.Func<Object, Object, Object, Object>(_copyInstertingElementAt_<uint>));

				publishPrimitive("nextIndexOf:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIndexOfIfAbsent_<ulong>));
				publishPrimitive("includes:",						new FuncNs.Func<Object, Object, Object>(_includes_<ulong>));
				publishPrimitive("removeNext:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_removeNextIfAbsent_<ulong>));	
				publishPrimitive("copyRemovingNext:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyRemovingNextIfAbsent_<ulong>));	
				publishPrimitive("removeAll:",						new FuncNs.Func<Object, Object, Object>(_removeAllOccurrencesOf_<ulong>));
				publishPrimitive("copyRemovingAll:", /* #copyWithout: */		new FuncNs.Func<Object, Object, Object>(_copyRemovingAllOccurrencesOf_<ulong>));

				publishPrimitive("atFirstPutOrAdd:",					new FuncNs.Func<Object, Object, Object>(_atFirstPutOrPrepend_));
				publishPrimitive("atLastPutOrAdd:",					new FuncNs.Func<Object, Object, Object>(_atLastPutOrAppend_));

				publishPrimitive("setSize:",						new FuncNs.Func<Object, Object, Object>(_setSize_));
				publishPrimitive("copyWithSize:",					new FuncNs.Func<Object, Object, Object>(_copyWithSize_));
				publishPrimitive("firstIfNone:",					new FuncNs.Func<Object, Object, Object>(_firstIfNone_));
				publishPrimitive("lastIfNone:",						new FuncNs.Func<Object, Object, Object>(_lastIfNone_));
				publishPrimitive("findtFirst:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIndexSuchThatIfAbsent_));
				publishPrimitive("contains:",						new FuncNs.Func<Object, Object, Object>(_contains_));
				publishPrimitive("addAll:",						new FuncNs.Func<Object, Object, Object>(_appendAll_));
				publishPrimitive(",",							new FuncNs.Func<Object, Object, Object>(_copyAppendingAll_));
				publishPrimitive("copyFrom:to:",					new FuncNs.Func<Object, Object, Object, Object>(_copyFromTo_));
				publishPrimitive("copyTo:",						new FuncNs.Func<Object, Object, Object>(_prefixTo_));
				publishPrimitive("copyFrom:",						new FuncNs.Func<Object, Object, Object>(_suffixFrom_));
				publishPrimitive("withAllButFirst",					new FuncNs.Func<Object, Object>(_withAllButFirst_));
				publishPrimitive("withAllButLast",					new FuncNs.Func<Object, Object>(_withAllButLast_));
				publishPrimitive("withFirst",						new FuncNs.Func<Object, Object>(_withFirst_));
				publishPrimitive("withLast",						new FuncNs.Func<Object, Object>(_withLast_));

				publishPrimitive("removeAt:",						new FuncNs.Func<Object, Object, Object>(_removeAt_));		// "Delete index"
				publishPrimitive("copyRemovingAt:",					new FuncNs.Func<Object, Object, Object>(_copyRemovingAt_));	// "Copy without index"
				publishPrimitive("copyAndRemoveFrom:to:",				new FuncNs.Func<Object, Object, Object, Object>(_copyAndRemoveFromTo_));	// "Cut range"
				publishPrimitive("copyRemovingFrom:to:",				new FuncNs.Func<Object, Object, Object, Object>(_copyRemovingFromTo_));	// "Copy without range"
				publishPrimitive("removeNextSuchThat:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_removeNextSuchThatIfAbsent_));
				publishPrimitive("copyRemovingNextSuchThat:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyRemovingNextSuchThatIfAbsent_));
				publishPrimitive("removeAllSuchThat:",					new FuncNs.Func<Object, Object, Object>(_removeAllOccurrencesSuchThat_));
				publishPrimitive("copyRemovingAllSuchThat:",				new FuncNs.Func<Object, Object, Object>(_copyRemovingAllSuchThat_));
				publishPrimitive("addAll:beforeIndex:",					new FuncNs.Func<Object, Object, Object, Object>(_insertAllAt_));
				publishPrimitive("copyAddingAll:beforeIndex:",				new FuncNs.Func<Object, Object, Object, Object>(_copyInsertingAllAt_));
				publishPrimitive("moveFrom:to:by:",					new FuncNs.Func<Object, Object, Object, Object, Object>(_moveFromToBy_)); // Cut then paste range
				publishPrimitive("copyMovingFrom:to:by:",				new FuncNs.Func<Object, Object, Object, Object, Object>(_copyMovingFromToBy_)); // Cut then paste range
				publishPrimitive("replaceFrom:to:with:startingAt:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_replaceFromToWithStartingAt_)); 
				publishPrimitive("copyReplacingFrom:to:with:startingAt:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyReplacingFromToWithStartingAt_)); 
				publishPrimitive("reverse",						new FuncNs.Func<Object, Object>(_reverse_)); 
				publishPrimitive("copyReversed",					new FuncNs.Func<Object, Object>(_copyReversed_)); 
				publishPrimitive("reverseFrom:to:",					new FuncNs.Func<Object, Object, Object, Object>(_reverseFromTo_)); 
				publishPrimitive("copyReversedFrom:to:",				new FuncNs.Func<Object, Object, Object, Object>(_copyReversedFromTo_));

			}

		}
	
	}
	
	public class ESFloatArray : ESIndexedComparableSlotsObject<float> {
		
		#region Static variables and functions
		
		public static implicit operator float[](ESFloatArray array) {
			return array.IndexedSlots;  
		}
		
		#endregion

		public ESFloatArray(ESBehavior esClass) : base(esClass, 0) {
		}

		public ESFloatArray(ESBehavior esClass, long size) : base(esClass, size) {
		}
		
		public ESFloatArray(ESBehavior esClass, float[] slots) : base(esClass, slots) {
		}
		
		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.IndexedSinglePrecisionSlots;}
		}
		
		public override ESArray asESArray() {
			return Class.Kernel.newArray(Array.ConvertAll<float, Object>(IndexedSlots, value => (float)value));
		}
		
		public override ESFloatArray asESFloatArray() {
			return this;
		}
		
		protected override int foreignCompareTo(Object comparand) {
			float[] primArray = comparand as float[];
			if (primArray == null) {
				IComparable comparable = comparand as IComparable;
				if (comparable == null) Class.Kernel.throwInvalidArgumentException(Class, "foreignCompareTo", "comparand", comparand);
				return -Math.Sign(comparable.CompareTo(IndexedSlots));
			}
			return ESIndexedComparableSlotsObject<float>.compare(IndexedSlots, primArray);
		}

		public override T valueBy<T>(Operation<T> operation) {
		    return operation.applyToIndexedSinglePrecisionSlotsObject(this);
		}

		public new class Primitives : ESIndexedSlotsObject<float>.Primitives {

			protected override void bindToKernel() {
				domainClass = kernel.FloatArrayClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.IndexedSinglePrecisionSlots;}
			}

			#region Primitive Definitions

			public Object _at_(Object receiver, Object slotIndexObject) {
				var array = (ESIndexedComparableSlotsObject<float>)receiver;
				var slots = array.IndexedSlots;
				var slotIndex = ((long)slotIndexObject) - 1;
				return slots[slotIndex];
			}

			public Object _atPut_(Object receiver, Object slotIndexObject, Object newValue) {
				var array = (ESIndexedComparableSlotsObject<float>)receiver;
				if (array.IsImmutable) throw new ImmutableObjectException();
				var slots = array.IndexedSlots;
				var slotIndex = ((long)slotIndexObject) - 1;
				slots[slotIndex] = (float)newValue;
				return newValue;
			}
		
			public Object _atFirstPutOrPrepend_(Object receiver, Object newValue) {
				return ((ESIndexedComparableSlotsObject<float>)receiver).atFirstPutOrPrepend(asHostFloat(newValue));
			}
		
			public Object _atLastPutOrAppend_(Object receiver, Object newValue) {
				return ((ESIndexedComparableSlotsObject<float>)receiver).atLastPutOrAppend(asHostFloat(newValue));
			}
		
			public Object _elementsDo_(Object receiver, Object enumerator) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator);
				((ESIndexedComparableSlotsObject<float>)receiver).elementsDo(value => f1((float)value));
				return receiver;
			}

			public Object _elementsFromToDo_(Object receiver, Object startIndex, Object endIndex, Object enumerator) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator);
				((ESIndexedComparableSlotsObject<float>)receiver).elementsFromToDo((long)startIndex - 1, (long)endIndex - 1, value => f1((float)value));
				return receiver;
			}
		
			public Object _elementsFromToByDo_(Object receiver, Object startIndex, Object endIndex, Object step, Object enumerator) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator);
				((ESIndexedComparableSlotsObject<float>)receiver).elementsFromToByDo((long)startIndex - 1, (long)endIndex - 1, asHostLong(step), value => f1((float)value));
				return receiver;
			}

			#endregion

			public override void publishCanonicalPrimitives() {

				publishPrimitive("hash",						new FuncNs.Func<Object, Object>(_hash_));
				publishPrimitive("=",							new FuncNs.Func<Object, Object, Object>(_hasSameValueAs_));
				publishPrimitive("compareTo:",						new FuncNs.Func<Object, Object, Object>(_compareTo_));

				publishPrimitive("at:",							new FuncNs.Func<Object, Object, Object>(_at_));

				publishPrimitive("do:",							new FuncNs.Func<Object, Object, Object>(_elementsDo_));
				publishPrimitive("from:to:do:",						new FuncNs.Func<Object, Object, Object, Object, Object>(_elementsFromToDo_));
				publishPrimitive("from:to:by:do:",					new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_elementsFromToByDo_));

				publishPrimitive("at:put:",						new FuncNs.Func<Object, Object, Object, Object>(_atPut_));
				publishPrimitive("nextIdentityIndexOf:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIdentityIndexOfIfAbsent_<float>));
				publishPrimitive("identityIncludes:",					new FuncNs.Func<Object, Object, Object>(_identityIncludes_<float>));
				publishPrimitive("add:",						new FuncNs.Func<Object, Object, Object>(_appendElement_<float>));
				publishPrimitive("copyWith:",						new FuncNs.Func<Object, Object, Object>(_copyAppendingElement_<float>));
				publishPrimitive("identityRemoveNext:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_identityRemoveNextIfAbsent_<float>));	
				publishPrimitive("copyIdentityRemovingNext:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyIdentityRemovingNextIfAbsent_<float>));	
				publishPrimitive("identityRemoveAll:",					new FuncNs.Func<Object, Object, Object>(_identityRemoveAllOccurrencesOf_<float>));	
				publishPrimitive("copyIdentityRemovingAll:",				new FuncNs.Func<Object, Object, Object>(_identityRemoveAllOccurrencesOf_<float>));	
				publishPrimitive("add:beforeIndex:",					new FuncNs.Func<Object, Object, Object, Object>(_insertElementAt_<float>));
				publishPrimitive("copyAdding:beforeIndex:",				new FuncNs.Func<Object, Object, Object, Object>(_copyInstertingElementAt_<uint>));

				publishPrimitive("nextIndexOf:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIndexOfIfAbsent_<float>));
				publishPrimitive("includes:",						new FuncNs.Func<Object, Object, Object>(_includes_<float>));
				publishPrimitive("removeNext:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_removeNextIfAbsent_<float>));	
				publishPrimitive("copyRemovingNext:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyRemovingNextIfAbsent_<float>));	
				publishPrimitive("removeAll:",						new FuncNs.Func<Object, Object, Object>(_removeAllOccurrencesOf_<float>));
				publishPrimitive("copyRemovingAll:", /* #copyWithout: */		new FuncNs.Func<Object, Object, Object>(_copyRemovingAllOccurrencesOf_<float>));

				publishPrimitive("atFirstPutOrAdd:",					new FuncNs.Func<Object, Object, Object>(_atFirstPutOrPrepend_));
				publishPrimitive("atLastPutOrAdd:",					new FuncNs.Func<Object, Object, Object>(_atLastPutOrAppend_));

				publishPrimitive("setSize:",						new FuncNs.Func<Object, Object, Object>(_setSize_));
				publishPrimitive("copyWithSize:",					new FuncNs.Func<Object, Object, Object>(_copyWithSize_));
				publishPrimitive("firstIfNone:",					new FuncNs.Func<Object, Object, Object>(_firstIfNone_));
				publishPrimitive("lastIfNone:",						new FuncNs.Func<Object, Object, Object>(_lastIfNone_));
				publishPrimitive("findtFirst:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIndexSuchThatIfAbsent_));
				publishPrimitive("contains:",						new FuncNs.Func<Object, Object, Object>(_contains_));
				publishPrimitive("addAll:",						new FuncNs.Func<Object, Object, Object>(_appendAll_));
				publishPrimitive(",",							new FuncNs.Func<Object, Object, Object>(_copyAppendingAll_));
				publishPrimitive("copyFrom:to:",					new FuncNs.Func<Object, Object, Object, Object>(_copyFromTo_));
				publishPrimitive("copyTo:",						new FuncNs.Func<Object, Object, Object>(_prefixTo_));
				publishPrimitive("copyFrom:",						new FuncNs.Func<Object, Object, Object>(_suffixFrom_));
				publishPrimitive("withAllButFirst",					new FuncNs.Func<Object, Object>(_withAllButFirst_));
				publishPrimitive("withAllButLast",					new FuncNs.Func<Object, Object>(_withAllButLast_));
				publishPrimitive("withFirst",						new FuncNs.Func<Object, Object>(_withFirst_));
				publishPrimitive("withLast",						new FuncNs.Func<Object, Object>(_withLast_));

				publishPrimitive("removeAt:",						new FuncNs.Func<Object, Object, Object>(_removeAt_));		// "Delete index"
				publishPrimitive("copyRemovingAt:",					new FuncNs.Func<Object, Object, Object>(_copyRemovingAt_));	// "Copy without index"
				publishPrimitive("copyAndRemoveFrom:to:",				new FuncNs.Func<Object, Object, Object, Object>(_copyAndRemoveFromTo_));	// "Cut range"
				publishPrimitive("copyRemovingFrom:to:",				new FuncNs.Func<Object, Object, Object, Object>(_copyRemovingFromTo_));	// "Copy without range"
				publishPrimitive("removeNextSuchThat:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_removeNextSuchThatIfAbsent_));
				publishPrimitive("copyRemovingNextSuchThat:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyRemovingNextSuchThatIfAbsent_));
				publishPrimitive("removeAllSuchThat:",					new FuncNs.Func<Object, Object, Object>(_removeAllOccurrencesSuchThat_));
				publishPrimitive("copyRemovingAllSuchThat:",				new FuncNs.Func<Object, Object, Object>(_copyRemovingAllSuchThat_));
				publishPrimitive("addAll:beforeIndex:",					new FuncNs.Func<Object, Object, Object, Object>(_insertAllAt_));
				publishPrimitive("copyAddingAll:beforeIndex:",				new FuncNs.Func<Object, Object, Object, Object>(_copyInsertingAllAt_));
				publishPrimitive("moveFrom:to:by:",					new FuncNs.Func<Object, Object, Object, Object, Object>(_moveFromToBy_)); // Cut then paste range
				publishPrimitive("copyMovingFrom:to:by:",				new FuncNs.Func<Object, Object, Object, Object, Object>(_copyMovingFromToBy_)); // Cut then paste range
				publishPrimitive("replaceFrom:to:with:startingAt:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_replaceFromToWithStartingAt_)); 
				publishPrimitive("copyReplacingFrom:to:with:startingAt:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyReplacingFromToWithStartingAt_)); 
				publishPrimitive("reverse",						new FuncNs.Func<Object, Object>(_reverse_)); 
				publishPrimitive("copyReversed",					new FuncNs.Func<Object, Object>(_copyReversed_)); 
				publishPrimitive("reverseFrom:to:",					new FuncNs.Func<Object, Object, Object, Object>(_reverseFromTo_)); 
				publishPrimitive("copyReversedFrom:to:",				new FuncNs.Func<Object, Object, Object, Object>(_copyReversedFromTo_));

			}

		}
		
	}
	
	public class ESDoubleArray : ESIndexedComparableSlotsObject<double> {
		
		#region Static variables and functions
		
		public static implicit operator double[](ESDoubleArray array) {
			return array.IndexedSlots;  
		}
		
		#endregion

		public ESDoubleArray(ESBehavior esClass) : base(esClass, 0) {
		}

		public ESDoubleArray(ESBehavior esClass, long size) : base(esClass, size) {
		}
		
		public ESDoubleArray(ESBehavior esClass, double[] slots) : base(esClass, slots) {
		}
		
		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.IndexedDoublePrecisionSlots;}
		}
		
		public override ESArray asESArray() {
			return Class.Kernel.newArray(Array.ConvertAll<double, Object>(IndexedSlots, value => (double)value));
		}
		
		public override ESDoubleArray asESDoubleArray() {
			return this;
		}

		protected override int foreignCompareTo(Object comparand) {
			double[] primArray = comparand as double[];
			if (primArray == null) {
				IComparable comparable = comparand as IComparable;
				if (comparable == null) Class.Kernel.throwInvalidArgumentException(Class, "foreignCompareTo", "comparand", comparand);
				return -Math.Sign(comparable.CompareTo(IndexedSlots));
			}
			return ESIndexedComparableSlotsObject<double>.compare(IndexedSlots, primArray);
		}

		public override T valueBy<T>(Operation<T> operation) {
		    return operation.applyToIndexedDoublePrecisionSlotsObject(this);
		}

		public new class Primitives : ESIndexedSlotsObject<double>.Primitives {

			protected override void bindToKernel() {
				domainClass = kernel.DoubleArrayClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.IndexedDoublePrecisionSlots;}
			}

			#region Primitive Definitions

			public Object _at_(Object receiver, Object slotIndexObject) {
				var array = (ESIndexedComparableSlotsObject<double>)receiver;
				var slots = array.IndexedSlots;
				var slotIndex = ((long)slotIndexObject) - 1;
				return slots[slotIndex];
			}

			public Object _atPut_(Object receiver, Object slotIndexObject, Object newValue) {
				var array = (ESIndexedComparableSlotsObject<double>)receiver;
				if (array.IsImmutable) throw new ImmutableObjectException();
				var slots = array.IndexedSlots;
				var slotIndex = ((long)slotIndexObject) - 1;
				slots[slotIndex] = (double)newValue;
				return newValue;
			}
		
			public Object _atFirstPutOrPrepend_(Object receiver, Object newValue) {
				return ((ESIndexedComparableSlotsObject<double>)receiver).atFirstPutOrPrepend(asHostDouble(newValue));
			}
		
			public Object _atLastPutOrAppend_(Object receiver, Object newValue) {
				return ((ESIndexedComparableSlotsObject<double>)receiver).atLastPutOrAppend(asHostDouble(newValue));
			}
		
			public Object _elementsDo_(Object receiver, Object enumerator) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator);
				((ESIndexedComparableSlotsObject<double>)receiver).elementsDo(value => f1((double)value));
				return receiver;
			}

			public Object _elementsFromToDo_(Object receiver, Object startIndex, Object endIndex, Object enumerator) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator);
				((ESIndexedComparableSlotsObject<double>)receiver).elementsFromToDo((long)startIndex - 1, (long)endIndex - 1, value => f1((double)value));
				return receiver;
			}
		
			public Object _elementsFromToByDo_(Object receiver, Object startIndex, Object endIndex, Object step, Object enumerator) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator);
				((ESIndexedComparableSlotsObject<double>)receiver).elementsFromToByDo((long)startIndex - 1, (long)endIndex - 1, asHostLong(step), value => f1((double)value));
				return receiver;
			}

			#endregion

			public override void publishCanonicalPrimitives() {

				publishPrimitive("hash",						new FuncNs.Func<Object, Object>(_hash_));
				publishPrimitive("=",							new FuncNs.Func<Object, Object, Object>(_hasSameValueAs_));
				publishPrimitive("compareTo:",						new FuncNs.Func<Object, Object, Object>(_compareTo_));

				publishPrimitive("at:",							new FuncNs.Func<Object, Object, Object>(_at_));

				publishPrimitive("do:",							new FuncNs.Func<Object, Object, Object>(_elementsDo_));
				publishPrimitive("from:to:do:",						new FuncNs.Func<Object, Object, Object, Object, Object>(_elementsFromToDo_));
				publishPrimitive("from:to:by:do:",					new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_elementsFromToByDo_));

				publishPrimitive("at:put:",						new FuncNs.Func<Object, Object, Object, Object>(_atPut_));
				publishPrimitive("nextIdentityIndexOf:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIdentityIndexOfIfAbsent_<double>));
				publishPrimitive("identityIncludes:",					new FuncNs.Func<Object, Object, Object>(_identityIncludes_<double>));
				publishPrimitive("add:",						new FuncNs.Func<Object, Object, Object>(_appendElement_<double>));
				publishPrimitive("copyWith:",						new FuncNs.Func<Object, Object, Object>(_copyAppendingElement_<double>));
				publishPrimitive("identityRemoveNext:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_identityRemoveNextIfAbsent_<double>));	
				publishPrimitive("copyIdentityRemovingNext:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyIdentityRemovingNextIfAbsent_<double>));	
				publishPrimitive("identityRemoveAll:",					new FuncNs.Func<Object, Object, Object>(_identityRemoveAllOccurrencesOf_<double>));	
				publishPrimitive("copyIdentityRemovingAll:",				new FuncNs.Func<Object, Object, Object>(_identityRemoveAllOccurrencesOf_<double>));	
				publishPrimitive("add:beforeIndex:",					new FuncNs.Func<Object, Object, Object, Object>(_insertElementAt_<double>));
				publishPrimitive("copyAdding:beforeIndex:",				new FuncNs.Func<Object, Object, Object, Object>(_copyInstertingElementAt_<double>));

				publishPrimitive("nextIndexOf:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIndexOfIfAbsent_<double>));
				publishPrimitive("includes:",						new FuncNs.Func<Object, Object, Object>(_includes_<double>));
				publishPrimitive("removeNext:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_removeNextIfAbsent_<double>));	
				publishPrimitive("copyRemovingNext:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyRemovingNextIfAbsent_<double>));	
				publishPrimitive("removeAll:",						new FuncNs.Func<Object, Object, Object>(_removeAllOccurrencesOf_<double>));
				publishPrimitive("copyRemovingAll:", /* #copyWithout: */		new FuncNs.Func<Object, Object, Object>(_copyRemovingAllOccurrencesOf_<double>));

				publishPrimitive("atFirstPutOrAdd:",					new FuncNs.Func<Object, Object, Object>(_atFirstPutOrPrepend_));
				publishPrimitive("atLastPutOrAdd:",					new FuncNs.Func<Object, Object, Object>(_atLastPutOrAppend_));

				publishPrimitive("setSize:",						new FuncNs.Func<Object, Object, Object>(_setSize_));
				publishPrimitive("copyWithSize:",					new FuncNs.Func<Object, Object, Object>(_copyWithSize_));
				publishPrimitive("firstIfNone:",					new FuncNs.Func<Object, Object, Object>(_firstIfNone_));
				publishPrimitive("lastIfNone:",						new FuncNs.Func<Object, Object, Object>(_lastIfNone_));
				publishPrimitive("findtFirst:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIndexSuchThatIfAbsent_));
				publishPrimitive("contains:",						new FuncNs.Func<Object, Object, Object>(_contains_));
				publishPrimitive("addAll:",						new FuncNs.Func<Object, Object, Object>(_appendAll_));
				publishPrimitive(",",							new FuncNs.Func<Object, Object, Object>(_copyAppendingAll_));
				publishPrimitive("copyFrom:to:",					new FuncNs.Func<Object, Object, Object, Object>(_copyFromTo_));
				publishPrimitive("copyTo:",						new FuncNs.Func<Object, Object, Object>(_prefixTo_));
				publishPrimitive("copyFrom:",						new FuncNs.Func<Object, Object, Object>(_suffixFrom_));
				publishPrimitive("withAllButFirst",					new FuncNs.Func<Object, Object>(_withAllButFirst_));
				publishPrimitive("withAllButLast",					new FuncNs.Func<Object, Object>(_withAllButLast_));
				publishPrimitive("withFirst",						new FuncNs.Func<Object, Object>(_withFirst_));
				publishPrimitive("withLast",						new FuncNs.Func<Object, Object>(_withLast_));

				publishPrimitive("removeAt:",						new FuncNs.Func<Object, Object, Object>(_removeAt_));		// "Delete index"
				publishPrimitive("copyRemovingAt:",					new FuncNs.Func<Object, Object, Object>(_copyRemovingAt_));	// "Copy without index"
				publishPrimitive("copyAndRemoveFrom:to:",				new FuncNs.Func<Object, Object, Object, Object>(_copyAndRemoveFromTo_));	// "Cut range"
				publishPrimitive("copyRemovingFrom:to:",				new FuncNs.Func<Object, Object, Object, Object>(_copyRemovingFromTo_));	// "Copy without range"
				publishPrimitive("removeNextSuchThat:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_removeNextSuchThatIfAbsent_));
				publishPrimitive("copyRemovingNextSuchThat:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyRemovingNextSuchThatIfAbsent_));
				publishPrimitive("removeAllSuchThat:",					new FuncNs.Func<Object, Object, Object>(_removeAllOccurrencesSuchThat_));
				publishPrimitive("copyRemovingAllSuchThat:",				new FuncNs.Func<Object, Object, Object>(_copyRemovingAllSuchThat_));
				publishPrimitive("addAll:beforeIndex:",					new FuncNs.Func<Object, Object, Object, Object>(_insertAllAt_));
				publishPrimitive("copyAddingAll:beforeIndex:",				new FuncNs.Func<Object, Object, Object, Object>(_copyInsertingAllAt_));
				publishPrimitive("moveFrom:to:by:",					new FuncNs.Func<Object, Object, Object, Object, Object>(_moveFromToBy_)); // Cut then paste range
				publishPrimitive("copyMovingFrom:to:by:",				new FuncNs.Func<Object, Object, Object, Object, Object>(_copyMovingFromToBy_)); // Cut then paste range
				publishPrimitive("replaceFrom:to:with:startingAt:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_replaceFromToWithStartingAt_)); 
				publishPrimitive("copyReplacingFrom:to:with:startingAt:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyReplacingFromToWithStartingAt_)); 
				publishPrimitive("reverse",						new FuncNs.Func<Object, Object>(_reverse_)); 
				publishPrimitive("copyReversed",					new FuncNs.Func<Object, Object>(_copyReversed_)); 
				publishPrimitive("reverseFrom:to:",					new FuncNs.Func<Object, Object, Object, Object>(_reverseFromTo_)); 
				publishPrimitive("copyReversedFrom:to:",				new FuncNs.Func<Object, Object, Object, Object>(_copyReversedFromTo_));

			}

		}
				
	}
	
	public class ESQuadArray : ESIndexedComparableSlotsObject<decimal> {
		
		#region Static variables and functions
		
		public static implicit operator decimal[](ESQuadArray array) {
			return array.IndexedSlots;  
		}
		
		#endregion

		public ESQuadArray(ESBehavior esClass) : base(esClass, 0) {
		}

		public ESQuadArray(ESBehavior esClass, long size) : base(esClass, size) {
		}
		
		public ESQuadArray(ESBehavior esClass, decimal[] slots) : base(esClass, slots) {
		}
		
		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.IndexedQuadPrecisionSlots;}
		}
		
		public override ESArray asESArray() {
			return Class.Kernel.newArray(Array.ConvertAll<decimal, Object>(IndexedSlots, value => (double)value));
		}
		
		public override ESQuadArray asESQuadArray() {
			return this;
		}

		protected override int foreignCompareTo(Object comparand) {
			decimal[] primArray = comparand as decimal[];
			if (primArray == null) {
				IComparable comparable = comparand as IComparable;
				if (comparable == null) Class.Kernel.throwInvalidArgumentException(Class, "foreignCompareTo", "comparand", comparand);
				return -Math.Sign(comparable.CompareTo(IndexedSlots));
			}
			return ESIndexedComparableSlotsObject<decimal>.compare(IndexedSlots, primArray);
		}

		public override T valueBy<T>(Operation<T> operation) {
		    return operation.applyToIndexedQuadPrecisionSlotsObject(this);
		}

		public new class Primitives : ESIndexedSlotsObject<decimal>.Primitives {

			protected override void bindToKernel() {
				domainClass = kernel.QuadArrayClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.IndexedQuadPrecisionSlots;}
			}

			#region Primitive Definitions

			public Object _at_(Object receiver, Object slotIndexObject) {
				var array = (ESIndexedComparableSlotsObject<decimal>)receiver;
				var slots = array.IndexedSlots;
				var slotIndex = ((long)slotIndexObject) - 1;
				return slots[slotIndex];
			}

			public Object _atPut_(Object receiver, Object slotIndexObject, Object newValue) {
				var array = (ESIndexedComparableSlotsObject<decimal>)receiver;
				if (array.IsImmutable) throw new ImmutableObjectException();
				var slots = array.IndexedSlots;
				var slotIndex = ((long)slotIndexObject) - 1;
				slots[slotIndex] = (decimal)newValue;
				return newValue;
			}
		
			public Object _atFirstPutOrPrepend_(Object receiver, Object newValue) {
				return ((ESIndexedComparableSlotsObject<decimal>)receiver).atFirstPutOrPrepend(asHostDecimal(newValue));
			}
		
			public Object _atLastPutOrAppend_(Object receiver, Object newValue) {
				return ((ESIndexedComparableSlotsObject<decimal>)receiver).atLastPutOrAppend(asHostDecimal(newValue));
			}
		
			public Object _elementsDo_(Object receiver, Object enumerator) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator);
				((ESIndexedComparableSlotsObject<decimal>)receiver).elementsDo(value => f1((double)value));
				return receiver;
			}

			public Object _elementsFromToDo_(Object receiver, Object startIndex, Object endIndex, Object enumerator) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator);
				((ESIndexedComparableSlotsObject<decimal>)receiver).elementsFromToDo((long)startIndex - 1, (long)endIndex - 1, value => f1((double)value));
				return receiver;
			}
		
			public Object _elementsFromToByDo_(Object receiver, Object startIndex, Object endIndex, Object step, Object enumerator) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator);
				((ESIndexedComparableSlotsObject<decimal>)receiver).elementsFromToByDo((long)startIndex - 1, (long)endIndex - 1, asHostLong(step), value => f1((double)value));
				return receiver;
			}

			#endregion

			public override void publishCanonicalPrimitives() {

				publishPrimitive("hash",						new FuncNs.Func<Object, Object>(_hash_));
				publishPrimitive("=",							new FuncNs.Func<Object, Object, Object>(_hasSameValueAs_));
				publishPrimitive("compareTo:",						new FuncNs.Func<Object, Object, Object>(_compareTo_));

				publishPrimitive("at:",							new FuncNs.Func<Object, Object, Object>(_at_));

				publishPrimitive("do:",							new FuncNs.Func<Object, Object, Object>(_elementsDo_));
				publishPrimitive("from:to:do:",						new FuncNs.Func<Object, Object, Object, Object, Object>(_elementsFromToDo_));
				publishPrimitive("from:to:by:do:",					new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_elementsFromToByDo_));

				publishPrimitive("at:put:",						new FuncNs.Func<Object, Object, Object, Object>(_atPut_));
				publishPrimitive("nextIdentityIndexOf:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIdentityIndexOfIfAbsent_<decimal>));
				publishPrimitive("identityIncludes:",					new FuncNs.Func<Object, Object, Object>(_identityIncludes_<decimal>));
				publishPrimitive("add:",						new FuncNs.Func<Object, Object, Object>(_appendElement_<decimal>));
				publishPrimitive("copyWith:",						new FuncNs.Func<Object, Object, Object>(_copyAppendingElement_<decimal>));
				publishPrimitive("identityRemoveNext:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_identityRemoveNextIfAbsent_<decimal>));	
				publishPrimitive("copyIdentityRemovingNext:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyIdentityRemovingNextIfAbsent_<decimal>));	
				publishPrimitive("identityRemoveAll:",					new FuncNs.Func<Object, Object, Object>(_identityRemoveAllOccurrencesOf_<decimal>));	
				publishPrimitive("copyIdentityRemovingAll:",				new FuncNs.Func<Object, Object, Object>(_identityRemoveAllOccurrencesOf_<decimal>));	
				publishPrimitive("add:beforeIndex:",					new FuncNs.Func<Object, Object, Object, Object>(_insertElementAt_<decimal>));
				publishPrimitive("copyAdding:beforeIndex:",				new FuncNs.Func<Object, Object, Object, Object>(_copyInstertingElementAt_<decimal>));

				publishPrimitive("nextIndexOf:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIndexOfIfAbsent_<decimal>));
				publishPrimitive("includes:",						new FuncNs.Func<Object, Object, Object>(_includes_<decimal>));
				publishPrimitive("removeNext:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_removeNextIfAbsent_<decimal>));	
				publishPrimitive("copyRemovingNext:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyRemovingNextIfAbsent_<decimal>));	
				publishPrimitive("removeAll:",						new FuncNs.Func<Object, Object, Object>(_removeAllOccurrencesOf_<decimal>));
				publishPrimitive("copyRemovingAll:", /* #copyWithout: */		new FuncNs.Func<Object, Object, Object>(_copyRemovingAllOccurrencesOf_<decimal>));

				publishPrimitive("atFirstPutOrAdd:",					new FuncNs.Func<Object, Object, Object>(_atFirstPutOrPrepend_));
				publishPrimitive("atLastPutOrAdd:",					new FuncNs.Func<Object, Object, Object>(_atLastPutOrAppend_));

				publishPrimitive("setSize:",						new FuncNs.Func<Object, Object, Object>(_setSize_));
				publishPrimitive("copyWithSize:",					new FuncNs.Func<Object, Object, Object>(_copyWithSize_));
				publishPrimitive("firstIfNone:",					new FuncNs.Func<Object, Object, Object>(_firstIfNone_));
				publishPrimitive("lastIfNone:",						new FuncNs.Func<Object, Object, Object>(_lastIfNone_));
				publishPrimitive("findtFirst:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIndexSuchThatIfAbsent_));
				publishPrimitive("contains:",						new FuncNs.Func<Object, Object, Object>(_contains_));
				publishPrimitive("addAll:",						new FuncNs.Func<Object, Object, Object>(_appendAll_));
				publishPrimitive(",",							new FuncNs.Func<Object, Object, Object>(_copyAppendingAll_));
				publishPrimitive("copyFrom:to:",					new FuncNs.Func<Object, Object, Object, Object>(_copyFromTo_));
				publishPrimitive("copyTo:",						new FuncNs.Func<Object, Object, Object>(_prefixTo_));
				publishPrimitive("copyFrom:",						new FuncNs.Func<Object, Object, Object>(_suffixFrom_));
				publishPrimitive("withAllButFirst",					new FuncNs.Func<Object, Object>(_withAllButFirst_));
				publishPrimitive("withAllButLast",					new FuncNs.Func<Object, Object>(_withAllButLast_));
				publishPrimitive("withFirst",						new FuncNs.Func<Object, Object>(_withFirst_));
				publishPrimitive("withLast",						new FuncNs.Func<Object, Object>(_withLast_));

				publishPrimitive("removeAt:",						new FuncNs.Func<Object, Object, Object>(_removeAt_));		// "Delete index"
				publishPrimitive("copyRemovingAt:",					new FuncNs.Func<Object, Object, Object>(_copyRemovingAt_));	// "Copy without index"
				publishPrimitive("copyAndRemoveFrom:to:",				new FuncNs.Func<Object, Object, Object, Object>(_copyAndRemoveFromTo_));	// "Cut range"
				publishPrimitive("copyRemovingFrom:to:",				new FuncNs.Func<Object, Object, Object, Object>(_copyRemovingFromTo_));	// "Copy without range"
				publishPrimitive("removeNextSuchThat:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_removeNextSuchThatIfAbsent_));
				publishPrimitive("copyRemovingNextSuchThat:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyRemovingNextSuchThatIfAbsent_));
				publishPrimitive("removeAllSuchThat:",					new FuncNs.Func<Object, Object, Object>(_removeAllOccurrencesSuchThat_));
				publishPrimitive("copyRemovingAllSuchThat:",				new FuncNs.Func<Object, Object, Object>(_copyRemovingAllSuchThat_));
				publishPrimitive("addAll:beforeIndex:",					new FuncNs.Func<Object, Object, Object, Object>(_insertAllAt_));
				publishPrimitive("copyAddingAll:beforeIndex:",				new FuncNs.Func<Object, Object, Object, Object>(_copyInsertingAllAt_));
				publishPrimitive("moveFrom:to:by:",					new FuncNs.Func<Object, Object, Object, Object, Object>(_moveFromToBy_)); // Cut then paste range
				publishPrimitive("copyMovingFrom:to:by:",				new FuncNs.Func<Object, Object, Object, Object, Object>(_copyMovingFromToBy_)); // Cut then paste range
				publishPrimitive("replaceFrom:to:with:startingAt:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_replaceFromToWithStartingAt_)); 
				publishPrimitive("copyReplacingFrom:to:with:startingAt:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyReplacingFromToWithStartingAt_)); 
				publishPrimitive("reverse",						new FuncNs.Func<Object, Object>(_reverse_)); 
				publishPrimitive("copyReversed",					new FuncNs.Func<Object, Object>(_copyReversed_)); 
				publishPrimitive("reverseFrom:to:",					new FuncNs.Func<Object, Object, Object, Object>(_reverseFromTo_)); 
				publishPrimitive("copyReversedFrom:to:",				new FuncNs.Func<Object, Object, Object, Object>(_copyReversedFromTo_));

			}

		}
				
	}

	public class ESPathname : ESIndexedComparableSlotsObject<String> {

		#region Static variables and functions
		
		public static implicit operator String[](ESPathname array) {
			return array.IndexedSlots;  
		}

		public static readonly String									defaultSeparatorString			= ".";
		public static readonly char									defaultSeparator			= (char)ESSymbol.PathElementSeparatorChar;
		public static readonly char									defaultExtensionSeparator		= ' ';

		#endregion

		public ESPathname(ESBehavior esClass) : this(esClass, 0) {}

		public ESPathname(ESBehavior esClass, long size) : base(esClass, size) {}

		public ESPathname(ESBehavior esClass, string[] slots) : base(esClass, slots) {}

		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.Pathname;}
		}
		
		public void initializeFromString(String pathString, char separatorChar, FuncNs.Func<Object, Object> transformer) {
			initializeFromStream(new StringReader(pathString), separatorChar, transformer);
		}
		
		public void initializeFromStream(TextReader stream, char separatorChar, FuncNs.Func<Object, Object> transformer) {
			if (IsImmutable) throw new ImmutableObjectException();
			slots = stream.elementsFromStream(separatorChar, transformer);
		}

		public override void normalizeIndexedSlots() {
			if (IsImmutable) throw new ImmutableObjectException();
			if (slots == null) {
				slots = empty;
				return;
			}
			if (slots.Length == 0) return;
			int count = 0;
			for (var i = 0; i < slots.Length; i++) {
				String value = slots[i];
				if (String.IsNullOrEmpty(value)) count++;
			}
			if (count > 0) {
				String[] newSlots = new string[slots.Length - count];
				int j= 0;
				for (var i = 0; i < slots.Length; i++) {
					String value = slots[i];
					if (!String.IsNullOrEmpty(value)) newSlots[j++] = slots[i];
				}
				slots = newSlots;
			}

		}

		protected override bool eachHasSameValue(String left, String right) {
			return left.Equals(right);
		}
		
		public override ESPathname asESPathname() {
			return this;
		}
		
		public override ESArray asESArray() {
			var kernel = Class.Kernel;
			return kernel.newArray(Array.ConvertAll<String, Object>(IndexedSlots, each => kernel.newString(each.ToCharArray())));
		}

		public Object extensionUsingSeparatorIfNone(char extensionSeparator, FuncNs.Func<Object>  notFoundAction) {
			long mySize = slots.Length;
			if (mySize < 1) return asFunctor0(notFoundAction)();
			String lastElement = slots[mySize - 1];
			if (lastElement == null) return asFunctor0(notFoundAction)();
			int index = lastElement.LastIndexOf(extensionSeparator);
			if (index <= 0) return asFunctor0(notFoundAction)(); // It is best if filenames of the form 'foo' or '.foo' are considered to have no extension
			return lastElement.Substring(index, lastElement.Length - index);
		}

		public bool hasExtensionUsingSeparator(char extensionSeparator) {
			long mySize = slots.Length;
			if (mySize < 1) return false;
			String lastElement = slots[mySize - 1];
			if (lastElement == null) return false;
			long index = lastElement.LastIndexOf(extensionSeparator);
			if (index <= 0) return false; // It is best if filenames of the form 'foo' or '.foo' are considered to have no extension
			return true;
		}

		public void appendExtension(char extensionSeparator, String extension) {
			if (IsImmutable) throw new ImmutableObjectException();
			if (slots.Length < 1) setSize(1);
			long mySize = slots.Length;
			String lastElement = slots[mySize - 1];
			if (String.IsNullOrEmpty(lastElement)) {
				lastElement = extensionSeparator + extension;
			} else {
				lastElement = lastElement + extensionSeparator + extension;
			}
			slots[mySize - 1] = lastElement;
		}

		public ESPathname appendingExtension(char extensionSeparator, String extension) {
			ESPathname copyAppendingExtension = (ESPathname)copy();
			copyAppendingExtension.appendExtension(extensionSeparator, extension);
			return copyAppendingExtension;
		}

		public void replaceOrAppendExtension(char extensionSeparator, String extension) {
			if (IsImmutable) throw new ImmutableObjectException();
			if (slots.Length < 1) setSize(1);
			long mySize = slots.Length;
			String lastElement = slots[mySize - 1];
			if (String.IsNullOrEmpty(lastElement)) {
				lastElement = extensionSeparator + extension;
			} else {
				int index = lastElement.LastIndexOf(extensionSeparator);
				if (index > 0) lastElement = lastElement.Substring(0, index + 1);
				lastElement = lastElement + extensionSeparator + extension;
			}
			slots[mySize - 1] = lastElement;
		}

		public ESPathname withExtension(char extensionSeparator, String extension) {
			ESPathname copyWithExtension = (ESPathname)copy();
			copyWithExtension.replaceOrAppendExtension(extensionSeparator, extension);
			return copyWithExtension;
		}

		public void removeExtension(char extensionSeparator) {
			long mySize = slots.Length;
			if (mySize < 1) return;
			String lastElement = slots[mySize - 1];
			if (!String.IsNullOrEmpty(lastElement)) {
				int index = lastElement.LastIndexOf(extensionSeparator);
				if (index > 0) {
					if (IsImmutable) throw new ImmutableObjectException();
					slots[mySize - 1] = lastElement.Substring(0, index + 1);
				} else if (index == 0) {
					setSize(mySize - 1);
				}
			}
		}

		public ESPathname withoutExtension(char extensionSeparator) {
			ESPathname copyWithoutExtension;
			long mySize = slots.Length;
			if (mySize < 1) {
				copyWithoutExtension = (ESPathname)copy();
				if (IsImmutable) copyWithoutExtension.beImmutable();
				return copyWithoutExtension;
			}
			String lastElement = slots[mySize - 1];
			if (String.IsNullOrEmpty(lastElement)) {
				copyWithoutExtension = (ESPathname)copy();
				if (IsImmutable) copyWithoutExtension.beImmutable();
				return copyWithoutExtension;
			}
			int index = lastElement.LastIndexOf(extensionSeparator);
			if (index < 0) {
				copyWithoutExtension = (ESPathname)copy();
				if (IsImmutable) copyWithoutExtension.beImmutable();
				return copyWithoutExtension;
			}
			if (index > 0) {
				copyWithoutExtension = (ESPathname)copy();
				copyWithoutExtension.IndexedSlots[mySize - 1] = lastElement.Substring(0, index + 1);
			} else if (index == 0) {
				copyWithoutExtension = (ESPathname)copyWithSize(mySize - 1);
				copyWithoutExtension.setSize(mySize - 1);
			} else {
				copyWithoutExtension = (ESPathname)copy();
			}
			return copyWithoutExtension;
		}

		public ESBindingReference bindingInNamespaceIfAbsent(NamespaceObject environment, AccessPrivilegeLevel requestorRights, ImportTransitivity importTransitivity, Functor0<ESBindingReference> ifAbsentAction) {
			long mySize = IndexedSlots.Length;
			if (mySize < 1) return ifAbsentAction == null ? null : ifAbsentAction();
			var ns = environment;
			ESBindingReference binding = null;
			var index = 0;
			String key;
			while (ns != null && index < mySize) {
				key = slots[index++];
				binding = ns.bindingAt(key, requestorRights, importTransitivity, null);
				if (binding == null) return ifAbsentAction == null ? null : ifAbsentAction();
				ns = binding.Value.Value as ESNamespace;
				requestorRights = AccessPrivilegeLevel.Public;
			}
			if (index != mySize) return ifAbsentAction == null ? null : ifAbsentAction();
			return binding;
		}

		public Object valueInNamespaceIfAbsent(NamespaceObject environment, AccessPrivilegeLevel requestorRights, ImportTransitivity importTransitivity, FuncNs.Func<Object> ifAbsentAction) {
			var binding = bindingInNamespaceIfAbsent(environment, requestorRights, importTransitivity, null);
			return binding == null ?
				(ifAbsentAction == null ? null : asFunctor0(ifAbsentAction)()) :
				binding.Value.Value;
		}

		public override ESNamespace asESNamespace() {
			var kernel = Class.Kernel;
			return kernel.asESNamespace(
				valueInNamespaceIfAbsent(
					kernel.RootNamespace, 
					AccessPrivilegeLevel.Public, 
					ImportTransitivity.Transitive, 
					delegate() {throw new PrimitiveFailException("Specified namespace is not accessible");}));
		}

		protected override int foreignCompareTo(object comparand) {
			String[] primArray = comparand as String[];
			if (primArray == null) {
				IComparable comparable = comparand as IComparable;
				if (comparable == null) Class.Kernel.throwInvalidArgumentException(Class, "foreignCompareTo", "comparand", comparand);
				return -Math.Sign(comparable.CompareTo(IndexedSlots));
			}
			return ESIndexedComparableSlotsObject<String>.compare(IndexedSlots, primArray);
		}

		#region Printing
		// These methods are for use by the runtime system, or by non-Smalltalk code; they aren't directly callable from Essence Sharp

		public override String ToString() {
			StringBuilder sb = new StringBuilder();
			printOn(sb, defaultSeparatorString);
			return sb.ToString();
		}

		public String ToString(String separatorString) {
			StringBuilder sb = new StringBuilder();
			printOn(sb, separatorString);
			return sb.ToString();
		}

		public String ToString(String separatorString, Functor1<String> transformer) {
			StringBuilder sb = new StringBuilder();
			printOn(sb, separatorString, transformer);
			return sb.ToString();
		}

		public void printOn(StringBuilder sb, String separatorString) {
			printOn(sb, separatorString, null);
		}

		public void printOn(StringBuilder sb, String separatorString, Functor1<String> transformer) {
			long mySize = slots.Length;
			if (mySize < 1) return;
			long lastIndex = mySize - 1;
			String element;
			if (transformer == null) {
				for (var i = 0; i < lastIndex; i++) {
					element = slots[i];
					if (!String.IsNullOrEmpty(element)) {
						sb.Append(element);
						if (!String.IsNullOrEmpty(separatorString)) sb.Append(separatorString);
					}
				}
				element = slots[lastIndex];
				if (!String.IsNullOrEmpty(element)) sb.Append(element);
			} else {
				for (var i = 0; i < lastIndex; i++) {
					element = slots[i];
					if (!String.IsNullOrEmpty(element)) {
						sb.Append(transformer(element));
						if (!String.IsNullOrEmpty(separatorString)) sb.Append(separatorString);
					}
				}
				element = slots[lastIndex];
				if (!String.IsNullOrEmpty(element)) sb.Append(transformer(element));
			}
		}

		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			printNamedInstanceVariablesUsing(depth, append, newLine);
			append(ToString());
		}

		#endregion

		public override T valueBy<T>(Operation<T> operation) {
		    return operation.applyToPathname(this);
		}

		public new class Primitives : ESIndexedSlotsObject<String>.Primitives {

			protected override void bindToKernel() {
				domainClass = kernel.PathnameClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.Pathname;}
			}

			#region Primitive Definitions

			public Object _asNamespace_(Object receiver) {
				return ((ESPathname)receiver).asESNamespace();
			}

			public Object _at_(Object receiver, Object slotIndexObject) {
				var array = (ESIndexedComparableSlotsObject<String>)receiver;
				var slots = array.IndexedSlots;
				var slotIndex = ((long)slotIndexObject) - 1;
				return slots[slotIndex];
			}

			public Object _atPut_(Object receiver, Object slotIndexObject, Object newValue) {
				var array = (ESIndexedComparableSlotsObject<String>)receiver;
				if (array.IsImmutable) throw new ImmutableObjectException();
				var slots = array.IndexedSlots;
				var slotIndex = ((long)slotIndexObject) - 1;
				slots[slotIndex] = (String)newValue;
				return newValue;
			}

			public Object _atFirstPutOrPrepend_(Object receiver, Object newValue) {
				return ((ESPathname)receiver).atFirstPutOrPrepend(asHostString(newValue));
			}
		
			public Object _atLastPutOrAppend_(Object receiver, Object newValue) {
				return ((ESPathname)receiver).atLastPutOrAppend(asHostString(newValue));
			}
		
			public Object _elementsDo_(Object receiver, Object enumerator) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator);
				((ESPathname)receiver).elementsDo(value => f1(kernel.asESSymbol(value)));
				return receiver;
			}

			public Object _elementsFromToDo_(Object receiver, Object startIndex, Object endIndex, Object enumerator) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator);
				((ESPathname)receiver).elementsFromToDo((long)startIndex - 1, (long)endIndex - 1, value => f1(kernel.asESSymbol(value)));
				return receiver;
			}
		
			public Object _elementsFromToByDo_(Object receiver, Object startIndex, Object endIndex, Object step, Object enumerator) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator);
				((ESPathname)receiver).elementsFromToByDo((long)startIndex - 1, (long)endIndex - 1, asHostLong(step), value => f1(kernel.asESSymbol(value)));
				return receiver;
			}
		
			public Object _bindingInNamespaceIfAbsent_ (Object receiver, Object environment, Object importTransitivity, Object ifAbsentAction) {
				ImportTransitivity transitivity;
				try {
					transitivity = (ImportTransitivity)Enum.Parse(typeof(ImportTransitivity), kernel.asESSymbol(importTransitivity));
				} catch {
					throw new PrimInvalidOperandException("valueInNamespaceIfAbsent: <importTransitivity> must be a Symbol or String identifying a valid import transitivity.");
				}
				var binding = ((ESPathname)receiver).bindingInNamespaceIfAbsent((ESNamespace)environment, AccessPrivilegeLevel.Public, transitivity, null);
				return binding == null ?
					asFunctor0(ifAbsentAction)() :
					binding;
			}
		
			public Object _valueInNamespaceIfAbsent_ (Object receiver, Object environment, Object importTransitivity, Object ifAbsentAction) {
				ImportTransitivity transitivity;
				try {
					transitivity = (ImportTransitivity)Enum.Parse(typeof(ImportTransitivity), kernel.asESSymbol(importTransitivity));
				} catch {
					throw new PrimInvalidOperandException("valueInNamespaceIfAbsent: <importTransitivity> must be a Symbol or String identifying a valid import transitivity.");
				}
				return ((ESPathname)receiver).valueInNamespaceIfAbsent((ESNamespace)environment, AccessPrivilegeLevel.Public, transitivity, asFunctor0(ifAbsentAction));
			}

			#endregion

			public override void publishCanonicalPrimitives() {

				publishPrimitive("hash",						new FuncNs.Func<Object, Object>(_hash_));
				publishPrimitive("=",							new FuncNs.Func<Object, Object, Object>(_hasSameValueAs_));
				publishPrimitive("compareTo:",						new FuncNs.Func<Object, Object, Object>(_compareTo_));

				publishPrimitive("at:",							new FuncNs.Func<Object, Object, Object>(_at_));

				publishPrimitive("do:",							new FuncNs.Func<Object, Object, Object>(_elementsDo_));
				publishPrimitive("from:to:do:",						new FuncNs.Func<Object, Object, Object, Object, Object>(_elementsFromToDo_));
				publishPrimitive("from:to:by:do:",					new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_elementsFromToByDo_));

				publishPrimitive("at:put:",						new FuncNs.Func<Object, Object, Object, Object>(_atPut_));
				publishPrimitive("nextIdentityIndexOf:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIdentityIndexOfIfAbsent_<String>));
				publishPrimitive("identityIncludes:",					new FuncNs.Func<Object, Object, Object>(_identityIncludes_<String>));
				publishPrimitive("add:",						new FuncNs.Func<Object, Object, Object>(_appendElement_<String>));
				publishPrimitive("copyWith:",						new FuncNs.Func<Object, Object, Object>(_copyAppendingElement_<String>));
				publishPrimitive("identityRemoveNext:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_identityRemoveNextIfAbsent_<String>));	
				publishPrimitive("copyIdentityRemovingNext:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyIdentityRemovingNextIfAbsent_<String>));	
				publishPrimitive("identityRemoveAll:",					new FuncNs.Func<Object, Object, Object>(_identityRemoveAllOccurrencesOf_<String>));	
				publishPrimitive("copyIdentityRemovingAll:",				new FuncNs.Func<Object, Object, Object>(_identityRemoveAllOccurrencesOf_<String>));	
				publishPrimitive("add:beforeIndex:",					new FuncNs.Func<Object, Object, Object, Object>(_insertElementAt_<String>));
				publishPrimitive("copyAdding:beforeIndex:",				new FuncNs.Func<Object, Object, Object, Object>(_copyInstertingElementAt_<String>));

				publishPrimitive("nextIndexOf:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIndexOfIfAbsent_<String>));
				publishPrimitive("includes:",						new FuncNs.Func<Object, Object, Object>(_includes_<String>));
				publishPrimitive("removeNext:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_removeNextIfAbsent_<String>));	
				publishPrimitive("copyRemovingNext:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyRemovingNextIfAbsent_<String>));	
				publishPrimitive("removeAll:",						new FuncNs.Func<Object, Object, Object>(_removeAllOccurrencesOf_<String>));
				publishPrimitive("copyRemovingAll:", /* #copyWithout: */		new FuncNs.Func<Object, Object, Object>(_copyRemovingAllOccurrencesOf_<String>));

				publishPrimitive("atFirstPutOrAdd:",					new FuncNs.Func<Object, Object, Object>(_atFirstPutOrPrepend_));
				publishPrimitive("atLastPutOrAdd:",					new FuncNs.Func<Object, Object, Object>(_atLastPutOrAppend_));

				publishPrimitive("setSize:",						new FuncNs.Func<Object, Object, Object>(_setSize_));
				publishPrimitive("copyWithSize:",					new FuncNs.Func<Object, Object, Object>(_copyWithSize_));
				publishPrimitive("firstIfNone:",					new FuncNs.Func<Object, Object, Object>(_firstIfNone_));
				publishPrimitive("lastIfNone:",						new FuncNs.Func<Object, Object, Object>(_lastIfNone_));
				publishPrimitive("findtFirst:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIndexSuchThatIfAbsent_));
				publishPrimitive("contains:",						new FuncNs.Func<Object, Object, Object>(_contains_));
				publishPrimitive("addAll:",						new FuncNs.Func<Object, Object, Object>(_appendAll_));
				publishPrimitive(",",							new FuncNs.Func<Object, Object, Object>(_copyAppendingAll_));
				publishPrimitive("copyFrom:to:",					new FuncNs.Func<Object, Object, Object, Object>(_copyFromTo_));
				publishPrimitive("copyTo:",						new FuncNs.Func<Object, Object, Object>(_prefixTo_));
				publishPrimitive("copyFrom:",						new FuncNs.Func<Object, Object, Object>(_suffixFrom_));
				publishPrimitive("withAllButFirst",					new FuncNs.Func<Object, Object>(_withAllButFirst_));
				publishPrimitive("withAllButLast",					new FuncNs.Func<Object, Object>(_withAllButLast_));
				publishPrimitive("withFirst",						new FuncNs.Func<Object, Object>(_withFirst_));
				publishPrimitive("withLast",						new FuncNs.Func<Object, Object>(_withLast_));

				publishPrimitive("removeAt:",						new FuncNs.Func<Object, Object, Object>(_removeAt_));		// "Delete index"
				publishPrimitive("copyRemovingAt:",					new FuncNs.Func<Object, Object, Object>(_copyRemovingAt_));	// "Copy without index"
				publishPrimitive("copyAndRemoveFrom:to:",				new FuncNs.Func<Object, Object, Object, Object>(_copyAndRemoveFromTo_));	// "Cut range"
				publishPrimitive("copyRemovingFrom:to:",				new FuncNs.Func<Object, Object, Object, Object>(_copyRemovingFromTo_));	// "Copy without range"
				publishPrimitive("removeNextSuchThat:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_removeNextSuchThatIfAbsent_));
				publishPrimitive("copyRemovingNextSuchThat:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyRemovingNextSuchThatIfAbsent_));
				publishPrimitive("removeAllSuchThat:",					new FuncNs.Func<Object, Object, Object>(_removeAllOccurrencesSuchThat_));
				publishPrimitive("copyRemovingAllSuchThat:",				new FuncNs.Func<Object, Object, Object>(_copyRemovingAllSuchThat_));
				publishPrimitive("addAll:beforeIndex:",					new FuncNs.Func<Object, Object, Object, Object>(_insertAllAt_));
				publishPrimitive("copyAddingAll:beforeIndex:",				new FuncNs.Func<Object, Object, Object, Object>(_copyInsertingAllAt_));
				publishPrimitive("moveFrom:to:by:",					new FuncNs.Func<Object, Object, Object, Object, Object>(_moveFromToBy_)); // Cut then paste range
				publishPrimitive("copyMovingFrom:to:by:",				new FuncNs.Func<Object, Object, Object, Object, Object>(_copyMovingFromToBy_)); // Cut then paste range
				publishPrimitive("replaceFrom:to:with:startingAt:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_replaceFromToWithStartingAt_)); 
				publishPrimitive("copyReplacingFrom:to:with:startingAt:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyReplacingFromToWithStartingAt_)); 
				publishPrimitive("reverse",						new FuncNs.Func<Object, Object>(_reverse_)); 
				publishPrimitive("copyReversed",					new FuncNs.Func<Object, Object>(_copyReversed_)); 
				publishPrimitive("reverseFrom:to:",					new FuncNs.Func<Object, Object, Object, Object>(_reverseFromTo_)); 
				publishPrimitive("copyReversedFrom:to:",				new FuncNs.Func<Object, Object, Object, Object>(_copyReversedFromTo_));

				publishPrimitive("bindingInNamespace:transitivity:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object>(_bindingInNamespaceIfAbsent_));
				publishPrimitive("valueInNamespace:transitivity:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object>(_valueInNamespaceIfAbsent_));

				publishPrimitive("asNamespace",						new FuncNs.Func<Object, Object>(_asNamespace_));

			}

		}

	}

}
