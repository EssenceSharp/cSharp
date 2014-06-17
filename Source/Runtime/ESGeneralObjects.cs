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
using System.Collections;
using System.Collections.Generic;
using System.IO;
#if CLR2
using FuncNs = Microsoft.Scripting.Utils;
#else
using FuncNs = System;
#endif
using EssenceSharp.Exceptions.System;
using EssenceSharp.Exceptions.System.PrimitiveFailures;
#endregion

namespace EssenceSharp.Runtime {

	public abstract class ESInitiallyMutableObject : ESObject {
		
		public static readonly byte mutabilityFlagBit = 1;

		protected byte statusFlags = 0;

		public ESInitiallyMutableObject(ESBehavior esClass) : base(esClass) {
		}
		
		public override bool IsImmutable {
			get {return (statusFlags & mutabilityFlagBit) != 0;}
		}
		
		public override void beImmutable() {
			statusFlags |= mutabilityFlagBit;
		}
		
		private void beMutable() {
			statusFlags ^= mutabilityFlagBit;
		}
		
		public override ESObject shallowCopy() {
			var copy = (ESInitiallyMutableObject)base.shallowCopy();
			copy.beMutable();
			return copy;
		}
		
	}

	public interface NamedSlotsObject {

		Object[] NamedSlots {
			get;
		}

		Object instVarValueAt(long slotIndex);
		
		Object instVarValueAtPut(long slotIndex, Object newValue);
		
		Object instVarValueAtName(ESSymbol name);
		
		Object instVarValueAtNamePut(ESSymbol name, Object newValue);

	}

	public class ESNamedSlotsObject : ESInitiallyMutableObject, NamedSlotsObject {
		
		#region Static variables and functions
				
		protected static Object[]					emptyNamedSlots				= new Object[0];
		
		#endregion
		
		protected Object[] 						namedSlots 				= null;	
		
		public ESNamedSlotsObject(ESBehavior esClass) : base(esClass) {
		}
		
		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.NamedSlots;}
		}
		
		internal override void setClass(ESBehavior esClass) {
			base.setClass(esClass);
			invalidateNamedSlots();
		}

		internal void invalidateNamedSlots() {
			long instSize = Class == null ? 0 : Class.InstSize;
			if (instSize > 0) {
				if (namedSlots != null && namedSlots.Length > 0) {
					if (namedSlots.Length != instSize) {
						Object[] newSlots = new Object[instSize];
						long limit = Math.Min(namedSlots.Length, instSize);
						for (long i = 0; i < limit; i++) {
							newSlots[i] = namedSlots[i];
						}
						namedSlots = newSlots;
					} 
				} else {
					namedSlots = new Object[instSize];
				}
			} else {
				namedSlots = emptyNamedSlots;
			}
		}
		
		public Object[] NamedSlots {
			get {return namedSlots;}
		}
		
		public override void postCopy() {
			base.postCopy();
			long instSize = namedSlots.Length;
			if (instSize > 0) {
				Object[] newSlots = instSize > 0 ? new Object[instSize] : emptyNamedSlots;
				for (var i = 0; i < instSize; i++) {
					newSlots[i] = namedSlots[i];
				}
				namedSlots = newSlots;
			}
			
		}
		
		public override Object instVarValueAt(long slotIndex) {
			if (slotIndex >= namedSlots.Length || slotIndex < 0) {
				return base.instVarValueAt(slotIndex);
			}
			return namedSlots[slotIndex];
		}
		
		public override Object instVarValueAtPut(long slotIndex, Object newValue) {
			if (slotIndex >= namedSlots.Length || slotIndex < 0) {
				return base.instVarValueAt(slotIndex);
			}
			if (IsImmutable) throw new ImmutableObjectException();
			namedSlots[slotIndex] = newValue;
			return this;
		}	
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			printNamedInstanceVariablesUsing(depth, append, newLine);
		}

		public virtual void printNamedInstanceVariablesUsing(uint depth, Action<String> append, Action<uint> newLine) {
			var namedSlotCount = namedSlots.Length;
			if (namedSlotCount < 1) return;
			uint maxPrintSize = (uint)Math.Min(80, namedSlotCount);
			for (long i = 0; i < maxPrintSize; i++) {
				newLine(depth);
				Object value = namedSlots[i];
				ESBehavior myClass = Class;
				if (myClass == null) {
					append(i.ToString());
				} else {
					ESSymbol name = Class.instVarNameAt(i);
					append(name.PrimitiveValue);
					value = instVarValueAt(i);
				}
				append(": ");
				if (value == null) {
					append("<null>");
				} else {
					ESObject esValue = value as ESObject;
					if (esValue == null) {
						append(value.ToString());
					} else {
						esValue.printUsing(depth, append, newLine);
					}
				}
			}
			if (size() > maxPrintSize) {
				newLine(depth);
				append(".....First ");
				append(maxPrintSize.ToString());
				append(" of ");
				append(namedSlots.Length.ToString());
				append(".....");
			}
		}

		public override T valueBy<T>(Operation<T> operation) {
		    return operation.applyToNamedSlotsObject(this);
		}
		
	}

	#region Indexed Slots Abstraction

	public abstract class ESIndexedSlotsObject<ElementType> : ESNamedSlotsObject {
		
		#region Static variables and functions
			
		public bool elementsAreIdentical(ElementType[] left, ElementType[] right) {
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
		
		public static bool equatableElementsHaveSameValue<EquatableElementType>(EquatableElementType[] left, EquatableElementType[] right) where EquatableElementType : ElementType, IEquatable<ElementType> {
			if (ReferenceEquals(left, right)) return true;
			if (left == null) return right.Length <= 0;
			if (right == null) return left.Length <= 0;
			long leftLength = left.Length;
			long rightLength = right.Length;
			long sizeDiff = leftLength - rightLength;
			if (sizeDiff != 0) return false;
			for (var i = 0; i < leftLength; i++) {
				if (left[i].Equals(right[i])) return false;
			}
			return true;
		}
		
		new public static int compare<ComparableElementType>(ComparableElementType[] left, ComparableElementType[] right) where ComparableElementType : ElementType, IComparable {
			if (ReferenceEquals(left, right)) return 0;
			if (left == null) return right.Length > 0 ? -1 : 0;
			if (right == null) return left.Length > 0 ? 1 : 0;
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
			
		public static readonly ElementType					nullElement				= default(ElementType);
		public static readonly ElementType[]					empty					= new ElementType[0];
				
		#endregion
		
		protected ElementType[] 						slots 					= null;
		
		public ESIndexedSlotsObject(ESBehavior esClass, long size) : base(esClass) {
			initialize(size > 0 ? new ElementType[size] : empty);
		}
		
		public ESIndexedSlotsObject(ESBehavior esClass, ElementType[] slots) : base(esClass) {
			initialize(slots);
		}
		
		protected virtual void initialize(ElementType[] slots) {
			this.slots = slots == null ? empty : (slots.Length < 1 ? empty : slots);
		}

		public override bool HasIndexedSlots {
			get {return true;}
		}
		
		public override Object HostSystemValue {
			get {return slots;}
		}

		public ElementType[] PrimitiveValue {
			get {return slots;}
		}
		
		public ElementType[] IndexedSlots {
			get {return slots;}
			protected set {slots = value ?? empty;}
		}
		
		public override void beImmutable() {
			if (!IsImmutable) {
				normalizeIndexedSlots();
				base.beImmutable();
			}
		}
		
		public virtual void normalizeIndexedSlots() {
			// By default, do nothing
		}

		public virtual ESIndexedSlotsObject<ElementType> newWithSize(long size) {
			ESIndexedSlotsObject<ElementType> copy = (ESIndexedSlotsObject<ElementType>)shallowCopy();
			copy.setSize(0);
			copy.postCopy();
			copy.setSize(size);
			return copy;
		}
		
		public virtual ESIndexedSlotsObject<ElementType> newWith(ElementType[] slots) {
			ESIndexedSlotsObject<ElementType> copy = (ESIndexedSlotsObject<ElementType>)shallowCopy();
			copy.setSize(0);
			copy.postCopy();
			copy.IndexedSlots = slots;
			return copy;
		}
		
		public override void postCopy() {
			base.postCopy();
			long mySize = size();
			if (mySize > 0) {
				ElementType[] newSlots = mySize > 0 ? new ElementType[mySize] : empty;
				Array.Copy(slots, newSlots, mySize);
				slots = newSlots;
			}
		}
		
		public virtual ESObject emptyCopy() {
			// May or may NOT have the same semantics as sending the message #copy to an ESObject!!!.
			ESIndexedSlotsObject<ElementType> copy = (ESIndexedSlotsObject<ElementType>)shallowCopy();
			copy.setSize(0);
			copy.postCopy();
			return copy;
		}

		protected abstract bool eachHasSameValue(ElementType left, ElementType right);
		
		#region Smalltalk API

		public override long hash() {
			long mySize = size();
			long code = mySize;
			if (mySize > 0) {
				code += slots[mySize - 1].GetHashCode();
				if (mySize > 1) {
					code += slots[0].GetHashCode();
					if (mySize > 2) {
						code += slots[mySize / 2].GetHashCode();
					}
				}
			}
			return code;
		}

		public override long size() {
			return slots.Length;
		}
		
		public ESIndexedSlotsObject<ElementType> setSize(long unconstrainedNewSize) {
			if (IsImmutable) throw new ImmutableObjectException();
			long newSize = Math.Max(0, unconstrainedNewSize);
			if (newSize > 0) {
				Array.Resize<ElementType>(ref slots, (int)newSize);
			} else {
				slots = empty;
			}
			return this;
		}
		
		public ESIndexedSlotsObject<ElementType> copyWithSize(long unconstrainedNewSize) {
			long oldSize = slots.Length;
			long newSize = Math.Max(0, unconstrainedNewSize);
			var newSlots = new ElementType[newSize];
			var copy = newWith(newSlots);
			if (newSize > 0) {
				if (oldSize > 0) {
					Array.Copy(
						slots, 				// sourceArray
						0,				// sourceIndex
						copy.IndexedSlots, 		// destinationArray
						0,				// destinationIndex
						Math.Min(oldSize, newSize));	// length
				}
			}
			return copy;
		}

		public void elementsDo(Functor1<Object, ElementType> enumerator) {
			long mySize = slots.Length;
			for (long slotIndex = 0; slotIndex < mySize; slotIndex++) {
				enumerator((slots[slotIndex]));
			}
		}
			
		public void elementsFromToDo(long startIndex, long endIndex, Functor1<Object, ElementType> enumerator) {
			long mySize = slots.Length;
			startIndex = Math.Min(Math.Max(0, startIndex), mySize);
			endIndex = Math.Min(Math.Max(0, endIndex), mySize);
			for (long slotIndex = startIndex; slotIndex <= endIndex; slotIndex++) {
				enumerator((slots[slotIndex]));
			}
		}
			
		public void elementsFromToByDo(long startIndex, long endIndex, long step, Functor1<Object, ElementType> enumerator) {
			long mySize = slots.Length;
			startIndex = Math.Min(Math.Max(0, startIndex), mySize);
			endIndex = Math.Min(Math.Max(0, endIndex), mySize);
			if (step > 0) {
				for (long slotIndex = startIndex; slotIndex <= endIndex; slotIndex += step) {
					enumerator((slots[slotIndex]));
				}
			} else if (step < 0) {
				for (long slotIndex = startIndex; slotIndex >= endIndex; slotIndex += step) {
					enumerator((slots[slotIndex]));
				}
			} else {
				throw new PrimInvalidOperandException("Step value must not be zero");
			}
		}
		
		public Object firstIfNone(FuncNs.Func<Object> noSuchElementAction) {
			long mySize = slots.Length;
			return mySize > 0 ? slots[0] : noSuchElementAction();
		}

		public Object lastIfNone(FuncNs.Func<Object> noSuchElementAction) {
			long mySize = slots.Length;
			return mySize > 0 ? slots[mySize - 1] : noSuchElementAction();
		}
		
		public ElementType atFirstPutOrPrepend(ElementType newValue) {
			if (IsImmutable) throw new ImmutableObjectException();
			long mySize = slots.Length;
			if (mySize > 0) {
				slots[0] = newValue;
			} else {
				slots = new ElementType[]{newValue};
			}
			return newValue;
		}
		
		public ElementType atLastPutOrAppend(ElementType newValue) {
			if (IsImmutable) throw new ImmutableObjectException();
			long mySize = slots.Length;
			if (mySize > 0) {
				slots[mySize - 1] = newValue;
			} else {
				slots = new ElementType[]{newValue};
			}
			return newValue;
		}
		
		public long nextIdentityIndexOf(ElementType element, long startIndex, long endIndex) {
			if (slots.Length < 1) return -1;
			if (startIndex <= endIndex) { 
				for (var i = startIndex; i <= endIndex; i++) {
					if (ReferenceEquals(element, slots[i])) return i;
				}
			} else {
				for (var i = startIndex; i >= endIndex; i--) {
					if (ReferenceEquals(element, slots[i])) return i;
				}
			}
			return -1;
		}
		
		public bool identityIncludes(ElementType element) {
			long mySize = slots.Length;
			for (var i = 0; i < mySize; i++) {
				if (ReferenceEquals(element, slots[i])) return true;
			}
			return false;
		}
		
		public long nextIndexOf(ElementType element, long startIndex, long endIndex) {
			if (slots.Length < 1) return -1;
			if (startIndex <= endIndex) { 
				for (var i = startIndex; i <= endIndex; i++) {
					if (eachHasSameValue(element, slots[i])) return i;
				}
			} else {
				for (var i = startIndex; i >= endIndex; i--) {
					if (eachHasSameValue(element, slots[i])) return i;
				}
			}
			return -1;
		}
		
		public bool includes(ElementType element) {
			long mySize = slots.Length;
			for (var i = 0; i < mySize; i++) {
				if (eachHasSameValue(element, slots[i])) return true;
			}
			return false;
		}
		
		public long nextIndexSuchThat(FuncNs.Func<Object, Object> predicate, long startIndex, long endIndex) {
			if (slots.Length < 1) return -1;
			if (startIndex <= endIndex) { 
				for (var i = startIndex; i <= endIndex; i++) {
					if ((bool)(predicate(slots[i]))) return i;
				}
			} else {
				for (var i = startIndex; i >= endIndex; i--) {
					if ((bool)(predicate(slots[i]))) return i;
				}
			}
			return -1;
		}
		
		public bool contains(FuncNs.Func<Object, Object> predicate) {
			long mySize = slots.Length;
			for (var i = 0; i < mySize; i++) {
				if ((bool)(predicate(slots[i]))) return true;
			}
			return false;
		}
		
		public ESIndexedSlotsObject<ElementType> appendElement(ElementType suffix) {
			if (IsImmutable) throw new ImmutableObjectException();
			long oldLength = slots.Length;
			long newLength = oldLength + 1;
			var newSlots = new ElementType[newLength];
			if (oldLength > 0) {
				Array.Copy(
					slots, 							// sourceArray
					0,							// sourceIndex
					newSlots, 						// destinationArray
					0,							// destinationIndex
					oldLength);						// length
			}
			newSlots[oldLength] = suffix;
			slots = newSlots;
			return this;
		}
			
		public ESIndexedSlotsObject<ElementType> copyAppendingElement(ElementType suffix) {
			long oldLength = slots.Length;
			long newLength = oldLength + 1;
			var newSlots = new ElementType[newLength];
			var copy = newWith(newSlots);
			if (oldLength > 0) {
				Array.Copy(
					slots, 							// sourceArray
					0,							// sourceIndex
					newSlots, 						// destinationArray
					0,							// destinationIndex
					oldLength);						// length
			}
			newSlots[oldLength] = suffix;
			return copy;
		}
			
		public ESIndexedSlotsObject<ElementType> appendAll(ElementType[] suffix) {
			long suffixLength = suffix.Length;
			if (suffixLength == 0) return this;
			if (IsImmutable) throw new ImmutableObjectException();
			long oldLength = slots.Length;
			long newLength = oldLength + suffixLength;
			var newSlots = new ElementType[newLength];
			if (oldLength > 0) {
				Array.Copy(
					slots, 							// sourceArray
					0,							// sourceIndex
					newSlots, 						// destinationArray
					0,							// destinationIndex
					oldLength);						// length
			}
			Array.Copy(
				suffix, 							// sourceArray
				0,								// sourceIndex
				newSlots, 							// destinationArray
				oldLength,							// destinationIndex
				suffixLength);							// length
			slots = newSlots;
			return this;
		}			
			
		public ESIndexedSlotsObject<ElementType> copyAppendingAll(ElementType[] suffix) {
			long suffixLength = suffix.Length;
			if (suffixLength == 0) return this;
			long oldLength = slots.Length;
			long newLength = oldLength + suffixLength;
			var newSlots = new ElementType[newLength];
			var copy = newWith(newSlots);
			if (oldLength > 0) {
				Array.Copy(
					slots, 							// sourceArray
					0,							// sourceIndex
					newSlots, 						// destinationArray
					0,							// destinationIndex
					oldLength);						// length
			}
			Array.Copy(
				suffix, 							// sourceArray
				0,								// sourceIndex
				newSlots, 							// destinationArray
				oldLength,							// destinationIndex
				suffixLength);							// length
			return copy;
		}

		public ESIndexedSlotsObject<ElementType> insertElementAt(ElementType infix, long insertionIndex) {
			if (IsImmutable) throw new ImmutableObjectException();
			long oldLength = slots.Length;
			long newLength = oldLength + 1;
			insertionIndex = Math.Min(Math.Max(0, insertionIndex), oldLength);
			long prefixLength = insertionIndex;
			long suffixLength = oldLength - prefixLength - 1;
			var newSlots = new ElementType[newLength];
			if (oldLength > 0) {
				if (prefixLength > 0) {
					Array.Copy(
						slots, 						// sourceArray
						0,						// sourceIndex
						newSlots, 					// destinationArray
						0,						// destinationIndex
						prefixLength);					// length
				}
			}
			newSlots[insertionIndex] = infix;
			if (suffixLength > 0) {
				Array.Copy(
					slots, 							// sourceArray
					prefixLength,						// sourceIndex
					newSlots, 						// destinationArray
					prefixLength + 1,					// destinationIndex
					suffixLength);						// length
			}
			slots = newSlots;
			return this;
		}

		public ESIndexedSlotsObject<ElementType> copyInstertingElementAt(ElementType infix, long insertionIndex) {
			long oldLength = slots.Length;
			long newLength = oldLength + 1;
			insertionIndex = Math.Min(Math.Max(0, insertionIndex), oldLength);
			long prefixLength = insertionIndex;
			long suffixLength = oldLength - prefixLength - 1;
			var newSlots = new ElementType[newLength];
			var copy = newWith(newSlots);
			if (oldLength > 0) {
				if (prefixLength > 0) {
					Array.Copy(
						slots, 						// sourceArray
						0,						// sourceIndex
						newSlots, 					// destinationArray
						0,						// destinationIndex
						prefixLength);					// length
				}
			}
			newSlots[insertionIndex] = infix;
			if (suffixLength > 0) {
				Array.Copy(
					slots, 							// sourceArray
					prefixLength,						// sourceIndex
					newSlots, 						// destinationArray
					prefixLength + 1,					// destinationIndex
					suffixLength);						// length
			}
			return copy;
		}
		
		public ESIndexedSlotsObject<ElementType> insertAllAt(ElementType[] infix, long insertionIndex) {
			long infixLength = infix.Length;
			if (infixLength == 0) return this;
			if (IsImmutable) throw new ImmutableObjectException();
			long oldLength = slots.Length;
			long newLength = oldLength + infixLength;
			insertionIndex = Math.Min(Math.Max(0, insertionIndex), oldLength);
			long prefixLength = insertionIndex;
			long suffixLength = oldLength - prefixLength - 1;
			var newSlots = new ElementType[newLength];
			if (oldLength > 0) {
				if (prefixLength > 0) {
					Array.Copy(
						slots, 						// sourceArray
						0,						// sourceIndex
						newSlots, 					// destinationArray
						0,						// destinationIndex
						prefixLength);					// length
				}
			}
			Array.Copy(
				infix, 								// sourceArray
				0,								// sourceIndex
				newSlots, 							// destinationArray
				insertionIndex,							// destinationIndex
				infixLength);							// length
			if (suffixLength > 0) {
				Array.Copy(
					slots, 							// sourceArray
					prefixLength,						// sourceIndex
					newSlots, 						// destinationArray
					prefixLength + infixLength,				// destinationIndex
					suffixLength);						// length
			}
			slots = newSlots;
			return this;
		}
		
		public ESIndexedSlotsObject<ElementType> copyInsertingAllAt(ElementType[] infix, long insertionIndex) {
			long infixLength = infix.Length;
			if (infixLength == 0) return this;
			long oldLength = slots.Length;
			long newLength = oldLength + infixLength;
			insertionIndex = Math.Min(Math.Max(0, insertionIndex), oldLength);
			long prefixLength = insertionIndex;
			long suffixLength = oldLength - prefixLength - 1;
			var newSlots = new ElementType[newLength];
			var copy = newWith(newSlots);
			if (oldLength > 0) {
				if (prefixLength > 0) {
					Array.Copy(
						slots, 						// sourceArray
						0,						// sourceIndex
						newSlots, 					// destinationArray
						0,						// destinationIndex
						prefixLength);					// length
				}
			}
			Array.Copy(
				infix, 								// sourceArray
				0,								// sourceIndex
				newSlots, 							// destinationArray
				insertionIndex,							// destinationIndex
				infixLength);							// length
			if (suffixLength > 0) {
				Array.Copy(
					slots, 							// sourceArray
					prefixLength,						// sourceIndex
					newSlots, 						// destinationArray
					prefixLength + infixLength,				// destinationIndex
					suffixLength);						// length
			}
			return copy;
		}
		
		public ESIndexedSlotsObject<ElementType> copyFromTo(long startIndex, long endIndex) {
			long mySize = slots.Length;
			startIndex = Math.Min(Math.Max(0, startIndex), mySize);
			endIndex = Math.Min(Math.Max(0, endIndex), mySize);
			long length = Math.Min(Math.Max(0, endIndex - startIndex + 1), mySize - startIndex);
			var newSlots = new ElementType[length];
			ESIndexedSlotsObject<ElementType> copy = newWith(newSlots);
			if (length > 0) {
				Array.Copy(
					slots, 							// sourceArray
					startIndex,						// sourceIndex
					copy.IndexedSlots, 					// destinationArray
					0,							// destinationIndex
					length);						// length
			} else if (length < 0) {
				throw new PrimInvalidOperandException("Invalid index(es)");
			}
			return copy;
		}

		public ESIndexedSlotsObject<ElementType> prefixTo(long endIndex) {
			return copyFromTo(0,  endIndex);
		}

		public ESIndexedSlotsObject<ElementType> suffixFrom(long startIndex) {
			return copyFromTo(startIndex,  slots.Length);
		}

		public ESIndexedSlotsObject<ElementType> withAllButFirst() {
			long mySize = slots.Length;
			return mySize > 1 ? copyFromTo(1,  mySize - 1) : (ESIndexedSlotsObject<ElementType>)copy();
		}

		public ESIndexedSlotsObject<ElementType> withAllButLast() {
			long mySize = slots.Length;
			return mySize > 1 ? copyFromTo(0,  mySize - 2) : (ESIndexedSlotsObject<ElementType>)copy();
		}

		public ESIndexedSlotsObject<ElementType> withFirst() {
			long mySize = slots.Length;
			return mySize > 1 ? copyFromTo(0,  0) : (ESIndexedSlotsObject<ElementType>)copy();
		}

		public ESIndexedSlotsObject<ElementType> withLast() {
			long mySize = slots.Length;
			long lastIndex = mySize - 1;
			return mySize > 1 ? copyFromTo(lastIndex,  lastIndex) : (ESIndexedSlotsObject<ElementType>)copy();
		}
		
		public ESIndexedSlotsObject<ElementType> copyAndRemoveFromTo(long startIndex, long endIndex) {
			long oldLength = slots.Length;
			startIndex = Math.Min(Math.Max(0, startIndex), oldLength);
			endIndex = Math.Min(Math.Max(0, endIndex), oldLength);
			long length = Math.Min(Math.Max(0, endIndex - startIndex + 1), oldLength - startIndex);
			ESIndexedSlotsObject<ElementType> deletion;
			if (length > 0) {
				long newLength = oldLength - length;
				long prefixLength = startIndex;
				long suffixLength = oldLength - prefixLength - 1;
				var deletionSlots = new ElementType[length];
				deletion = newWith(deletionSlots);
				var newSlots = new ElementType[newLength];
				if (oldLength > 0) {
					if (prefixLength > 0) {
						Array.Copy(
							slots, 					// sourceArray
							0,					// sourceIndex
							newSlots, 				// destinationArray
							0,					// destinationIndex
							prefixLength);				// length
					}
				}
				if (suffixLength > 0) {
					Array.Copy(
						slots, 						// sourceArray
						endIndex + 1,					// sourceIndex
						newSlots, 					// destinationArray
						startIndex,					// destinationIndex
						suffixLength);					// length
				}
				if (length > 0) {
					Array.Copy(
						slots, 						// sourceArray
						startIndex,					// sourceIndex
						deletionSlots, 					// destinationArray
						0,						// destinationIndex
						length);					// length
				}
				slots = newSlots;
			} else if (length < 0) {
				throw new PrimInvalidOperandException("Invalid index(es)");
			} else {
				deletion = (ESIndexedSlotsObject<ElementType>)shallowCopy();
			}
			return deletion;
		}
		
		public ESIndexedSlotsObject<ElementType> removeAt(long index) {
			long oldLength = slots.Length;
			if (index < 0) throw new PrimInvalidOperandException("Invalid index: Must be greater than 0.");
			if (index >= oldLength) throw new PrimInvalidOperandException("Invalid index: Must be less than or equal to receiver size.");
			if (IsImmutable) throw new ImmutableObjectException();
			long newLength = oldLength - 1;
			long prefixLength = index;
			long suffixLength = oldLength - prefixLength - 1;
			var newSlots = new ElementType[newLength];
			if (oldLength > 0) {
				if (prefixLength > 0) {
					Array.Copy(
						slots, 					// sourceArray
						0,					// sourceIndex
						newSlots, 				// destinationArray
						0,					// destinationIndex
						prefixLength);				// length
				}
			}
			if (suffixLength > 0) {
				Array.Copy(
					slots, 						// sourceArray
					index + 1,					// sourceIndex
					newSlots, 					// destinationArray
					index,						// destinationIndex
					suffixLength);					// length
			}
			slots = newSlots;
			return this;
		}
		
		public ESIndexedSlotsObject<ElementType> removeFromTo(long startIndex, long endIndex) {
			long oldLength = slots.Length;
			startIndex = Math.Min(Math.Max(0, startIndex), oldLength);
			endIndex = Math.Min(Math.Max(0, endIndex), oldLength);
			long length = Math.Min(Math.Max(0, endIndex - startIndex + 1), oldLength - startIndex);
			if (length > 0) {
				if (IsImmutable) throw new ImmutableObjectException();
				long newLength = oldLength - length;
				long prefixLength = startIndex;
				long suffixLength = oldLength - prefixLength - 1;
				var newSlots = new ElementType[newLength];
				if (oldLength > 0) {
					if (prefixLength > 0) {
						Array.Copy(
							slots, 					// sourceArray
							0,					// sourceIndex
							newSlots, 				// destinationArray
							0,					// destinationIndex
							prefixLength);				// length
					}
				}
				if (suffixLength > 0) {
					Array.Copy(
						slots, 						// sourceArray
						endIndex + 1,					// sourceIndex
						newSlots, 					// destinationArray
						startIndex,					// destinationIndex
						suffixLength);					// length
				}
				slots = newSlots;
			} else if (length < 0) {
				throw new PrimInvalidOperandException("Invalid index(es)");
			} 
			return this;
		}
		
		public ESIndexedSlotsObject<ElementType> copyRemovingAt(long index) {
			long oldLength = slots.Length;
			if (index < 0) throw new PrimInvalidOperandException("Invalid index: Must be greater than 0.");
			if (index >= oldLength) throw new PrimInvalidOperandException("Invalid index: Must be less than or equal to receiver size.");
			long newLength = oldLength - 1;
			long prefixLength = index;
			long suffixLength = oldLength - prefixLength - 1;
			ElementType[] newSlots = new ElementType[newLength];
			var copy = newWith(newSlots);
			if (oldLength > 0) {
				if (prefixLength > 0) {
					Array.Copy(
						slots, 					// sourceArray
						0,					// sourceIndex
						newSlots, 				// destinationArray
						0,					// destinationIndex
						prefixLength);				// length
				}
			}
			if (suffixLength > 0) {
				Array.Copy(
					slots, 						// sourceArray
					index + 1,					// sourceIndex
					newSlots, 					// destinationArray
					index,						// destinationIndex
					suffixLength);					// length
			}
			return copy;
		}
		
		public ESIndexedSlotsObject<ElementType> copyRemovingFromTo(long startIndex, long endIndex) {
			long oldLength = slots.Length;
			ESIndexedSlotsObject<ElementType> copy;
			startIndex = Math.Min(Math.Max(0, startIndex), oldLength);
			endIndex = Math.Min(Math.Max(0, endIndex), oldLength);
			long length = Math.Min(Math.Max(0, endIndex - startIndex + 1), oldLength - startIndex);
			if (length > 0) {
				long newLength = oldLength - length;
				long prefixLength = startIndex;
				long suffixLength = oldLength - endIndex - 1;
				var newSlots = new ElementType[length];
				copy = newWith(newSlots);
				if (oldLength > 0) {
					if (prefixLength > 0) {
						Array.Copy(
							slots, 					// sourceArray
							0,					// sourceIndex
							newSlots, 				// destinationArray
							0,					// destinationIndex
							prefixLength);				// length
					}
				}
				if (suffixLength > 0) {
					Array.Copy(
						slots, 						// sourceArray
						endIndex + 1,					// sourceIndex
						newSlots, 					// destinationArray
						startIndex,					// destinationIndex
						suffixLength);					// length
				}
			} else if (length < 0) {
				throw new PrimInvalidOperandException("Invalid index(es)");
			} else {
				copy = (ESIndexedSlotsObject<ElementType>)shallowCopy();
			}
			return copy;
		}

		public long identityRemoveNext(ElementType removal, long startIndex, long endIndex) {
			if (slots.Length < 1) return -1;
			if (startIndex <= endIndex) { 
				for (var i = startIndex; i <= endIndex; i++) {
					var element = slots[i];
					if (ReferenceEquals(removal, element))  {
						removeFromTo(i, i);
						return i;
					}
				}
			} else {
				for (var i = startIndex; i >= endIndex; i--) {
					var element = slots[i];
					if (ReferenceEquals(removal, element))  {
						removeFromTo(i, i);
						return i;
					}
				}
			}
			return -1;
		}

		public Object copyIdentityRemovingNext(ElementType removal, long startIndex, long endIndex, FuncNs.Func<Object> notFoundAction) {
			if (slots.Length < 1) return notFoundAction == null ? null : notFoundAction();
			var mySize = slots.Length;
			if (startIndex <= endIndex) { 
				for (var i = startIndex; i <= endIndex; i++) {
					var element = slots[i];
					if (ReferenceEquals(removal, element)) {
						var newSlots = new ElementType[mySize - 1];
						var copy = newWith(newSlots);
						long prefixLength = i;
						long suffixLength = mySize - i - 1;
						if (prefixLength > 0) {
							Array.Copy(
								slots, 						// sourceArray
								0,						// sourceIndex
								newSlots, 					// destinationArray
								0,						// destinationIndex
								prefixLength);					// length
						}
						if (suffixLength > 0) {
							Array.Copy(
								slots, 						// sourceArray
								i + 1,						// sourceIndex
								newSlots, 					// destinationArray
								i,						// destinationIndex
								suffixLength);					// length
						}					
						return copy;
					}
				} 
			} else {
				for (var i = startIndex; i >= endIndex; i--) {
					var element = slots[i];
					if (ReferenceEquals(removal, element)) {
						var newSlots = new ElementType[mySize - 1];
						var copy = newWith(newSlots);
						long prefixLength = i;
						long suffixLength = mySize - i - 1;
						if (prefixLength > 0) {
							Array.Copy(
								slots, 						// sourceArray
								0,						// sourceIndex
								newSlots, 					// destinationArray
								0,						// destinationIndex
								prefixLength);					// length
						}
						if (suffixLength > 0) {
							Array.Copy(
								slots, 						// sourceArray
								i + 1,						// sourceIndex
								newSlots, 					// destinationArray
								i,						// destinationIndex
								suffixLength);					// length
						}					
						return copy;
					}
				} 
			}
			return notFoundAction == null ? null : notFoundAction();
		}

		public long identityRemoveAllOccurrencesOf(ElementType removal) {
			var elementsRemaining = new List<ElementType>();
			long mySize = slots.Length;
			long removalCount = 0;
			for (var i = 0; i < mySize; i++) {
				var element = slots[i];
				if (ReferenceEquals(removal, element)) {
					removalCount++;
				} else {
					elementsRemaining.Add(element);
				}
			}
			if (removalCount > 0) {
				if (IsImmutable) throw new ImmutableObjectException();
				slots = elementsRemaining.ToArray();
			}
			return removalCount;
		}

		public ESIndexedSlotsObject<ElementType> copyIdentityRemovingAllOccurrencesOf(ElementType removal) {
			var elementsRemaining = new List<ElementType>();
			long mySize = slots.Length;
			long removalCount = 0;
			for (var i = 0; i < mySize; i++) {
				var element = slots[i];
				if (ReferenceEquals(removal, element)) {
					removalCount++;
				} else {
					elementsRemaining.Add(element);
				}
			}
			if (removalCount > 0) {
				return newWith(elementsRemaining.ToArray());
			} else {
				return (ESIndexedSlotsObject<ElementType>)copy();
			}
		}

		public long removeNext(ElementType removal, long startIndex, long endIndex) {
			if (slots.Length < 1) return -1;
			if (startIndex <= endIndex) { 
				for (var i = startIndex; i <= endIndex; i++) {
					var element = slots[i];
					if (eachHasSameValue(removal, element))  {
						removeFromTo(i, i);
						return i;
					}
				}
			} else {
				for (var i = startIndex; i >= endIndex; i--) {
					var element = slots[i];
					if (eachHasSameValue(removal, element))  {
						removeFromTo(i, i);
						return i;
					}
				}
			}
			return -1;
		}

		public Object copyRemovingNext(ElementType removal, long startIndex, long endIndex, FuncNs.Func<Object> notFoundAction) {
			if (slots.Length < 1) return notFoundAction == null ? null : notFoundAction();
			var mySize = slots.Length;
			if (startIndex <= endIndex) { 
				for (var i = startIndex; i <= endIndex; i++) {
					ElementType element = slots[i];
					if (eachHasSameValue(removal, element)) {
						ElementType[] newSlots = new ElementType[mySize - 1];
						var copy = newWith(newSlots);
						long prefixLength = i;
						long suffixLength = mySize - i - 1;
						if (prefixLength > 0) {
							Array.Copy(
								slots, 						// sourceArray
								0,						// sourceIndex
								newSlots, 					// destinationArray
								0,						// destinationIndex
								prefixLength);					// length
						}
						if (suffixLength > 0) {
							Array.Copy(
								slots, 						// sourceArray
								i + 1,						// sourceIndex
								newSlots, 					// destinationArray
								i,						// destinationIndex
								suffixLength);					// length
						}					
						return copy;
					}
				} 
			} else {
				for (var i = startIndex; i >= endIndex; i--) {
					var element = slots[i];
					if (eachHasSameValue(removal, element)) {
						var newSlots = new ElementType[mySize - 1];
						var copy = newWith(newSlots);
						long prefixLength = i;
						long suffixLength = mySize - i - 1;
						if (prefixLength > 0) {
							Array.Copy(
								slots, 						// sourceArray
								0,						// sourceIndex
								newSlots, 					// destinationArray
								0,						// destinationIndex
								prefixLength);					// length
						}
						if (suffixLength > 0) {
							Array.Copy(
								slots, 						// sourceArray
								i + 1,						// sourceIndex
								newSlots, 					// destinationArray
								i,						// destinationIndex
								suffixLength);					// length
						}					
						return copy;
					}
				} 
			}
			return notFoundAction == null ? null : notFoundAction();
		}

		public long removeAllOccurrencesOf(ElementType removal) {
			var elementsRemaining = new List<ElementType>();
			long mySize = slots.Length;
			long removalCount = 0;
			for (var i = 0; i < mySize; i++) {
				var element = slots[i];
				if (eachHasSameValue(removal, element)) {
					removalCount++;
				} else {
					elementsRemaining.Add(element);
				}
			}
			if (removalCount > 0) {
				if (IsImmutable) throw new ImmutableObjectException();
				slots = elementsRemaining.ToArray();
			}
			return removalCount;
		}

		public ESIndexedSlotsObject<ElementType> copyRemovingAllOccurrencesOf(ElementType removal) {
			var elementsRemaining = new List<ElementType>();
			long mySize = slots.Length;
			long removalCount = 0;
			for (var i = 0; i < mySize; i++) {
				var element = slots[i];
				if (eachHasSameValue(removal, element)) {
					removalCount++;
				} else {
					elementsRemaining.Add(element);
				}
			}
			if (removalCount > 0) {
				return newWith(elementsRemaining.ToArray());
			} else {
				return (ESIndexedSlotsObject<ElementType>)copy();
			}
		}

		public long removeNextSuchThat(FuncNs.Func<Object, Object> predicate, long startIndex, long endIndex) {
			if (slots.Length < 1) return -1;
			if (startIndex <= endIndex) { 
				for (var i = startIndex; i <= endIndex; i++) {
					var element = slots[i];
					if ((bool)(predicate(element)))  {
						removeFromTo(i, i);
						return i;
					}
				}
			} else {
				for (var i = startIndex; i >= endIndex; i--) {
					var element = slots[i];
					if ((bool)(predicate(element)))  {
						removeFromTo(i, i);
						return i;
					}
				}
			}
			return -1;
		}

		public Object copyRemovingNextSuchThat(FuncNs.Func<Object, Object> predicate, long startIndex, long endIndex, FuncNs.Func<Object> notFoundAction) {
			if (slots.Length < 1) return notFoundAction == null ? null : notFoundAction();
			var mySize = slots.Length;
			if (startIndex <= endIndex) { 
				for (var i = startIndex; i <= endIndex; i++) {
					var element = slots[i];
					if ((bool)(predicate(element))) {
						var newSlots = new ElementType[mySize - 1];
						var copy = newWith(newSlots);
						long prefixLength = i;
						long suffixLength = mySize - i - 1;
						if (prefixLength > 0) {
							Array.Copy(
								slots, 						// sourceArray
								0,						// sourceIndex
								newSlots, 					// destinationArray
								0,						// destinationIndex
								prefixLength);					// length
						}
						if (suffixLength > 0) {
							Array.Copy(
								slots, 						// sourceArray
								i + 1,						// sourceIndex
								newSlots, 					// destinationArray
								i,						// destinationIndex
								suffixLength);					// length
						}					
						return copy;
					}
				} 
			} else {
				for (var i = startIndex; i >= endIndex; i--) {
					var element = slots[i];
					if ((bool)(predicate(element))) {
						var newSlots = new ElementType[mySize - 1];
						var copy = newWith(newSlots);
						long prefixLength = i;
						long suffixLength = mySize - i - 1;
						if (prefixLength > 0) {
							Array.Copy(
								slots, 						// sourceArray
								0,						// sourceIndex
								newSlots, 					// destinationArray
								0,						// destinationIndex
								prefixLength);					// length
						}
						if (suffixLength > 0) {
							Array.Copy(
								slots, 						// sourceArray
								i + 1,						// sourceIndex
								newSlots, 					// destinationArray
								i,						// destinationIndex
								suffixLength);					// length
						}					
						return copy;
					}
				} 
			}
			return notFoundAction == null ? null : notFoundAction();
		}

		public long removeAllOccurrencesSuchThat(FuncNs.Func<Object, Object> predicate) {
			List<ElementType> elementsRemaining = new List<ElementType>();
			long mySize = slots.Length;
			long removalCount = 0;
			for (var i = 0; i < mySize; i++) {
				var element = slots[i];
				if ((bool)(predicate(element))) {
					removalCount++;
				} else {
					elementsRemaining.Add(element);
				}
			}
			if (removalCount > 0) {
				if (IsImmutable) throw new ImmutableObjectException();
				slots = elementsRemaining.ToArray();
			}
			return removalCount;
		}		

		public ESIndexedSlotsObject<ElementType> copyRemovingAllSuchThat(FuncNs.Func<Object, Object> predicate) {
			List<ElementType> elementsRemaining = new List<ElementType>();
			long mySize = slots.Length;
			long removalCount = 0;
			for (var i = 0; i < mySize; i++) {
				ElementType element = slots[i];
				if ((bool)(predicate(element))) {
					removalCount++;
				} else {
					elementsRemaining.Add(element);
				}
			}
			if (removalCount > 0) {
				return newWith(elementsRemaining.ToArray());
			} else {
				return (ESIndexedSlotsObject<ElementType>)copy();
			}
		}

		public ESIndexedSlotsObject<ElementType> moveFromToBy(long startIndex, long stopIndex, long displacement) {
			if (displacement == 0) return this;
			long mySize = slots.Length;
			startIndex = Math.Min(Math.Max(0, startIndex), mySize);
			stopIndex = Math.Min(Math.Max(0, stopIndex), mySize);
			long sectionSize = stopIndex - startIndex + 1;
			if (sectionSize > 0) {
				ElementType[] newSlots = new ElementType[mySize];
				long movedSectionStart = startIndex + displacement;
				long movedSectionEnd = stopIndex  + displacement;
				if (movedSectionStart < 0 || movedSectionEnd >= mySize) {
					throw new PrimInvalidOperandException("Invalid displacement");
				}
				if (IsImmutable) throw new ImmutableObjectException();
				long prefixLength = movedSectionStart;
				long suffixLength = slots.Length - movedSectionEnd - 1;
				if (prefixLength > 0) {
					Array.Copy(							
						slots, 							// sourceArray
						0,							// sourceIndex
						newSlots, 						// destinationArray
						0,							// destinationIndex
						prefixLength);						// length
				}
				Array.Copy(								
					slots, 								// sourceArray
					startIndex,							// sourceIndex
					newSlots, 							// destinationArray
					movedSectionStart,						// destinationIndex
					sectionSize);							// length
				if (suffixLength > 0) {
					Array.Copy(							
						slots, 							// sourceArray
						movedSectionEnd + 1,					// sourceIndex
						newSlots, 						// destinationArray
						movedSectionEnd + 1,					// destinationIndex
						suffixLength);						// length
				}
				if (displacement > 0) {
					Array.Copy(								
						slots, 							// sourceArray
						stopIndex + 1,						// sourceIndex
						newSlots, 						// destinationArray
						startIndex,						// destinationIndex
						displacement);						// length
				} else {
					Array.Copy(								
						slots, 							// sourceArray
						movedSectionStart,					// sourceIndex
						newSlots, 						// destinationArray
						movedSectionEnd + 1,					// destinationIndex
						-displacement);						// length
				}
				slots = newSlots;
			} else if (sectionSize < 0) {
				throw new PrimInvalidOperandException("Invalid index(es)");
			}
			return this;
		}
		
		public ESIndexedSlotsObject<ElementType> copyMovingFromToBy(long startIndex, long stopIndex, long displacement) {
			if (displacement == 0) return (ESIndexedSlotsObject<ElementType>)this.shallowCopy();
			long mySize = slots.Length;
			startIndex = Math.Min(Math.Max(0, startIndex), mySize);
			stopIndex = Math.Min(Math.Max(0, stopIndex), mySize);
			long sectionSize = stopIndex - startIndex + 1;
			ESIndexedSlotsObject<ElementType> copy;
			if (sectionSize > 0) {
				ElementType[] newSlots = new ElementType[mySize];
				copy = newWith(newSlots);
				long movedSectionStart = startIndex + displacement;
				long movedSectionEnd = stopIndex  + displacement;
				if (movedSectionStart < 0 || movedSectionEnd >= mySize) {
					throw new PrimInvalidOperandException("Invalid displacement");
				}
				long prefixLength = movedSectionStart;
				long suffixLength = slots.Length - movedSectionEnd - 1;
				if (prefixLength > 0) {
					Array.Copy(							
						slots, 							// sourceArray
						0,							// sourceIndex
						newSlots, 						// destinationArray
						0,							// destinationIndex
						prefixLength);						// length
				}
				Array.Copy(								
					slots, 								// sourceArray
					startIndex,							// sourceIndex
					newSlots, 							// destinationArray
					movedSectionStart,						// destinationIndex
					sectionSize);							// length
				if (suffixLength > 0) {
					Array.Copy(							
						slots, 							// sourceArray
						movedSectionEnd + 1,					// sourceIndex
						newSlots, 						// destinationArray
						movedSectionEnd + 1,					// destinationIndex
						suffixLength);						// length
				}
				if (displacement > 0) {
					Array.Copy(								
						slots, 							// sourceArray
						stopIndex + 1,						// sourceIndex
						newSlots, 						// destinationArray
						startIndex,						// destinationIndex
						displacement);						// length
				} else {
					Array.Copy(								
						slots, 							// sourceArray
						movedSectionStart,					// sourceIndex
						newSlots, 						// destinationArray
						movedSectionEnd + 1,					// destinationIndex
						-displacement);						// length
				}
			} else if (sectionSize < 0) {
				throw new PrimInvalidOperandException("Invalid index(es)");
			} else {
				copy = (ESIndexedSlotsObject<ElementType>)this.shallowCopy();
			}
			return copy;
		}
			
		public ESIndexedSlotsObject<ElementType> replaceFromToWithStartingAt(long startIndex, long stopIndex, ElementType[] replacementSource, long replacementSourceStartIndex) {
			long mySize = slots.Length;
			startIndex = Math.Min(Math.Max(0, startIndex), mySize);
			stopIndex = Math.Min(Math.Max(0, stopIndex), mySize);
			long length = stopIndex - startIndex + 1;
			long replacementSourceSize = replacementSource.Length;
			replacementSourceStartIndex = Math.Min(Math.Max(0, replacementSourceStartIndex), replacementSourceSize);
			long maxLength = replacementSourceSize - replacementSourceStartIndex;
			length = Math.Min(length, maxLength);
			if (length > 0) {
				if (IsImmutable) throw new ImmutableObjectException();
				Array.Copy(								// The CLR documentation claims that Array.Copy handles the case where the source and destination array is the same array, including any overlapping regions.
					replacementSource, 						// sourceArray
					replacementSourceStartIndex,					// sourceIndex
					slots, 								// destinationArray
					startIndex,							// destinationIndex
					length);							// length
			} else if (length < 0) {
				throw new PrimInvalidOperandException("Invalid destination index(es)");
			}
			return this;
		}
			
		public ESIndexedSlotsObject<ElementType> copyReplacingFromToWithStartingAt(long startIndex, long stopIndex, ElementType[] replacementSource, long replacementSourceStartIndex) {
			ESIndexedSlotsObject<ElementType> copy;
			long ourSize = slots.Length;
			startIndex = Math.Min(Math.Max(0, startIndex), ourSize);
			stopIndex = Math.Min(Math.Max(0, stopIndex), ourSize);
			long length = stopIndex - startIndex + 1;
			long replacementSourceSize = replacementSource.Length;
			replacementSourceStartIndex = Math.Min(Math.Max(0, replacementSourceStartIndex), replacementSourceSize);
			long maxLength = replacementSourceSize - replacementSourceStartIndex;
			length = Math.Min(length, maxLength);
			if (length >= 0) {
				long prefixLength = startIndex;
				long suffixLength = ourSize - stopIndex - 1;
				ElementType[] newSlots = new ElementType[ourSize];
				copy = newWith(newSlots);
				if (prefixLength > 0) {
					Array.Copy(							
						slots, 							// sourceArray
						0,							// sourceIndex
						newSlots, 						// destinationArray
						0,							// destinationIndex
						prefixLength);						// length
				}
				if (length > 0) {
					Array.Copy(							
						replacementSource, 					// sourceArray
						replacementSourceStartIndex,				// sourceIndex
						newSlots, 						// destinationArray
						startIndex,						// destinationIndex
						length);						// length
				}
				if (suffixLength > 0) {
					Array.Copy(							
						slots, 							// sourceArray
						stopIndex + 1,						// sourceIndex
						newSlots, 						// destinationArray
						stopIndex + 1,						// destinationIndex
						suffixLength);						// length
				}
			} else {
				throw new PrimInvalidOperandException("Invalid destination index(es)");
			}
			return copy;
		}
		
		public ESIndexedSlotsObject<ElementType> reverse() {
			long ourSize = slots.Length;
			if (ourSize > 1) {
				if (IsImmutable) throw new ImmutableObjectException();
				Array.Reverse(slots);
			}
			return this;
		}
		
		public ESIndexedSlotsObject<ElementType> copyReversed() {
			long ourSize = slots.Length;
			if (ourSize <= 1) return (ESIndexedSlotsObject<ElementType>)shallowCopy();
			ElementType[] newSlots = new ElementType[ourSize];
			ESIndexedSlotsObject<ElementType> copy = newWith(newSlots);
			long j = ourSize - 1;
			for (var i = 0; i < ourSize; i++) {
				newSlots[j--] = slots[i];
			}
			return copy;
		}
			
		public ESIndexedSlotsObject<ElementType> reverseFromTo(long startIndex, long endIndex) {
			long ourSize = slots.Length;
			if (ourSize <= 1) return this;
			startIndex = Math.Min(Math.Max(0, startIndex), ourSize);
			endIndex = Math.Min(Math.Max(0, endIndex), ourSize);
			long length = endIndex - startIndex + 1;
			if (length > 1) {
				if (IsImmutable) throw new ImmutableObjectException();
				Array.Reverse(slots, (int)startIndex, (int)length);
			} else if (length < 0) {
				throw new PrimInvalidOperandException("Indexes invalid");
			}
			return this;
		}
			
		public ESIndexedSlotsObject<ElementType> copyReversedFromTo(long startIndex, long endIndex) {
			long ourSize = slots.Length;
			if (ourSize <= 1) return (ESIndexedSlotsObject<ElementType>)shallowCopy();
			ESIndexedSlotsObject<ElementType> copy;
			startIndex = Math.Min(Math.Max(0, startIndex), ourSize - 1);
			endIndex = Math.Min(Math.Max(0, endIndex), ourSize - 1);
			long span = endIndex - startIndex + 1;
			if (span > 1) {
				long suffixLength = ourSize - endIndex - 1;
				ElementType[] newSlots = new ElementType[ourSize];
				copy = newWith(newSlots);
				if (startIndex > 0) {
					Array.Copy(							
						slots, 							// sourceArray
						0,							// sourceIndex
						newSlots, 						// destinationArray
						0,							// destinationIndex
						startIndex);						// length
				}
				long j = endIndex;
				for (var i = startIndex; i <= endIndex; i++) {
					newSlots[j--] = slots[i];
				}
				if (suffixLength > 0) {
					Array.Copy(							
						slots, 							// sourceArray
						endIndex + 1,						// sourceIndex
						newSlots, 						// destinationArray
						endIndex + 1,						// destinationIndex
						suffixLength);						// length
				}
			} else if (span < 0) {
				throw new PrimInvalidOperandException("Invalid index(es)");
			} else {
				copy = (ESIndexedSlotsObject<ElementType>)shallowCopy();
			}
			return copy;
		}

		#endregion
		
		#region Foreign language interoperability
		
		public long Length {
			get {return slots.Length;}
		}
		
		public long Count {
			get {return slots.Length;}
		}
		
		public ElementType this[long slotIndex] {
			get {return slots[slotIndex];}
			set {slots[slotIndex] = value;}
		}
		
		#endregion
		
		public new abstract class Primitives : PrimitiveDomain {
		
			#region Primitives

			#region Non-generic primitives

			public Object _setSize_(Object receiver, Object newSize) {
				return ((ESIndexedSlotsObject<ElementType>)receiver).setSize(asHostLong(newSize));
			}
		
			public Object _copyWithSize_(Object receiver, Object newSize) {
				return ((ESIndexedSlotsObject<ElementType>)receiver).copyWithSize(asHostLong(newSize));
			}
		
			public Object _firstIfNone_(Object receiver, Object noSuchElementAction) {
				return ((ESIndexedSlotsObject<ElementType>)receiver).firstIfNone(asFunctor0(noSuchElementAction));
			}
		
			public Object _lastIfNone_(Object receiver, Object noSuchElementAction) {
				return ((ESIndexedSlotsObject<ElementType>)receiver).lastIfNone(asFunctor0(noSuchElementAction));
			}

			public Object _nextIndexSuchThatIfAbsent_(Object receiver, Object predicate, Object startIndex, Object endIndex, Object noSuchElementAction) {
				long index = ((ESIndexedSlotsObject<ElementType>)receiver).nextIndexSuchThat(asFunctor1(predicate), asHostLong(startIndex) - 1, asHostLong(endIndex) - 1);
				return index >= 0 ? index + 1 : asFunctor0(noSuchElementAction)();
			}

			public Object _contains_(Object receiver, Object predicate) {
				return ((ESIndexedSlotsObject<ElementType>)receiver).contains(asFunctor1(predicate));
			}
		
			public Object _appendAll_(Object receiver, Object suffix) {
				try {
					return ((ESIndexedSlotsObject<ElementType>)receiver).appendAll(((ESIndexedSlotsObject<ElementType>)suffix).IndexedSlots);
				} catch (InvalidCastException castEx) {
					if (suffix.GetType() == castEx.TargetSite.DeclaringType) {
						throw new PrimInvalidOperandException("Invalid suffix", castEx);
					} else {
						throw castEx;
					}
				}
			}

			public Object _copyAppendingAll_(Object receiver, Object suffix) {
				try {
					return ((ESIndexedSlotsObject<ElementType>)receiver).copyAppendingAll(((ESIndexedSlotsObject<ElementType>)suffix).IndexedSlots);
				} catch (InvalidCastException castEx) {
					if (suffix.GetType() == castEx.TargetSite.DeclaringType) {
						throw new PrimInvalidOperandException("Invalid suffix", castEx);
					} else {
						throw castEx;
					}
				}
			}
		
			public Object _copyFromTo_(Object receiver, Object startIndex, Object endIndex) {
				return ((ESIndexedSlotsObject<ElementType>)receiver).copyFromTo(asHostLong(startIndex) - 1, asHostLong(endIndex) - 1);
			}
		
			public Object _prefixTo_(Object receiver, Object endIndex) {
				return ((ESIndexedSlotsObject<ElementType>)receiver).prefixTo(asHostLong(endIndex) - 1);
			}

			public Object _suffixFrom_(Object receiver, Object startIndex) {
				return ((ESIndexedSlotsObject<ElementType>)receiver).suffixFrom(asHostLong(startIndex) - 1);
			}
		
			public Object _withAllButFirst_(Object receiver) {
				return ((ESIndexedSlotsObject<ElementType>)receiver).withAllButFirst();
			}
		
			public Object _withAllButLast_(Object receiver) {
				return ((ESIndexedSlotsObject<ElementType>)receiver).withAllButLast();
			}
		
			public Object _withFirst_(Object receiver) {
				return ((ESIndexedSlotsObject<ElementType>)receiver).withFirst();
			}
		
			public Object _withLast_(Object receiver) {
				return ((ESIndexedSlotsObject<ElementType>)receiver).withLast();
			}
		
			public Object _copyAndRemoveFromTo_(Object receiver, Object startIndex, Object endIndex) {
				return ((ESIndexedSlotsObject<ElementType>)receiver).copyAndRemoveFromTo(asHostLong(startIndex) - 1, asHostLong(endIndex) - 1);
			}
		
			public Object _removeAt_(Object receiver, Object index) {
				return ((ESIndexedSlotsObject<ElementType>)receiver).removeAt(asHostLong(index) - 1);
			}
		
			public Object _copyRemovingAt_(Object receiver, Object index) {
				return ((ESIndexedSlotsObject<ElementType>)receiver).copyRemovingAt(asHostLong(index) - 1);
			}
		
			public Object _removeFromTo_(Object receiver, Object startIndex, Object endIndex) {
				return ((ESIndexedSlotsObject<ElementType>)receiver).removeFromTo(asHostLong(startIndex) - 1, asHostLong(endIndex) - 1);
			}
		
			public Object _copyRemovingFromTo_(Object receiver, Object startIndex, Object endIndex) {
				return ((ESIndexedSlotsObject<ElementType>)receiver).copyRemovingFromTo(asHostLong(startIndex) - 1, asHostLong(endIndex) - 1);
			}
		
			public Object _removeNextSuchThatIfAbsent_(Object receiver, Object predicate, Object startIndex, Object endIndex, Object notFoundAction) {
				long index = ((ESIndexedSlotsObject<ElementType>)receiver).removeNextSuchThat(asFunctor1(predicate), (long)startIndex - 1, (long)endIndex - 1);
				return index >= 0 ? index + 1 : asFunctor0(notFoundAction)();
			}
		
			public Object _copyRemovingNextSuchThatIfAbsent_(Object receiver, Object predicate, Object startIndex, Object endIndex, Object notFoundAction) {
				return ((ESIndexedSlotsObject<ElementType>)receiver).copyRemovingNextSuchThat(asFunctor1(predicate), (long)startIndex - 1, (long)endIndex - 1, asFunctor0(notFoundAction));
			}
		
			public Object _removeAllOccurrencesSuchThat_(Object receiver, Object predicate) {
				return ((ESIndexedSlotsObject<ElementType>)receiver).removeAllOccurrencesSuchThat(asFunctor1(predicate));
			}
		
			public Object _copyRemovingAllSuchThat_(Object receiver, Object predicate) {
				// Note: Same functionality as reject: predicate
				return ((ESIndexedSlotsObject<ElementType>)receiver).copyRemovingAllSuchThat(asFunctor1(predicate));
			}

			public Object _insertAllAt_(Object receiver, Object infix, Object insertionIndex) {
				try {
					return ((ESIndexedSlotsObject<ElementType>)receiver).insertAllAt(((ESIndexedSlotsObject<ElementType>)infix).IndexedSlots, asHostLong(insertionIndex) - 1);
				} catch (InvalidCastException castEx) {
					if (infix.GetType() == castEx.TargetSite.DeclaringType) {
						throw new PrimInvalidOperandException("Invalid suffix", castEx);
					} else {
						throw castEx;
					}
				}
			}
		
			public Object _copyInsertingAllAt_(Object receiver, Object infix, Object insertionIndex) {
				try {
					return ((ESIndexedSlotsObject<ElementType>)receiver).copyInsertingAllAt(((ESIndexedSlotsObject<ElementType>)infix).IndexedSlots, asHostLong(insertionIndex) - 1);
				} catch (InvalidCastException castEx) {
					if (infix.GetType() == castEx.TargetSite.DeclaringType) {
						throw new PrimInvalidOperandException("Invalid suffix", castEx);
					} else {
						throw castEx;
					}
				}
			}

			public Object _moveFromToBy_(Object receiver, Object startIndex, Object stopIndex, Object displacement) {
				return ((ESIndexedSlotsObject<ElementType>)receiver).moveFromToBy(asHostLong(startIndex) - 1, asHostLong(stopIndex) - 1, asHostLong(displacement));
			}
		
			public Object _copyMovingFromToBy_(Object receiver, Object startIndex, Object stopIndex, Object displacement) {
				return ((ESIndexedSlotsObject<ElementType>)receiver).copyMovingFromToBy(asHostLong(startIndex) - 1, asHostLong(stopIndex) - 1, asHostLong(displacement));
			}
		
			public Object _replaceFromToWithStartingAt_(Object receiver, Object startIndex, Object stopIndex, Object replacementSource, Object replacementSourceStartIndex) {
				try {
					return ((ESIndexedSlotsObject<ElementType>)receiver).replaceFromToWithStartingAt(asHostLong(startIndex) - 1, asHostLong(stopIndex) - 1, ((ESIndexedSlotsObject<ElementType>)replacementSource).IndexedSlots, asHostLong(replacementSourceStartIndex) - 1);
				} catch (InvalidCastException castEx) {
					if (replacementSource.GetType() == castEx.TargetSite.DeclaringType) {
						throw new PrimInvalidOperandException("Invalid replacementSource", castEx);
					} else {
						throw castEx;
					}
				}
			}
		
			public Object _copyReplacingFromToWithStartingAt_(Object receiver, Object startIndex, Object stopIndex, Object replacementSource, Object replacementSourceStartIndex) {
				try {
					return ((ESIndexedSlotsObject<ElementType>)receiver).copyReplacingFromToWithStartingAt(asHostLong(startIndex) - 1, asHostLong(stopIndex) - 1, ((ESIndexedSlotsObject<ElementType>)replacementSource).IndexedSlots, asHostLong(replacementSourceStartIndex) - 1);
				} catch (InvalidCastException castEx) {
					if (replacementSource.GetType() == castEx.TargetSite.DeclaringType) {
						throw new PrimInvalidOperandException("Invalid replacementSource", castEx);
					} else {
						throw castEx;
					}
				}
			}

			public Object _reverse_(Object receiver) {
				return ((ESIndexedSlotsObject<ElementType>)receiver).reverse();
			}

			public Object _copyReversed_(Object receiver) {
				return ((ESIndexedSlotsObject<ElementType>)receiver).copyReversed();
			}
		
			public Object _reverseFromTo_(Object receiver, Object startIndex, Object endIndex) {
				return ((ESIndexedSlotsObject<ElementType>)receiver).reverseFromTo(asHostLong(startIndex) - 1, asHostLong(endIndex) - 1);
			}
		
			public Object _copyReversedFromTo_(Object receiver, Object startIndex, Object endIndex) {
				return ((ESIndexedSlotsObject<ElementType>)receiver).copyReversedFromTo(asHostLong(startIndex) - 1, asHostLong(endIndex) - 1);
			}

			#endregion

			#region Generic Primitives
			
			public Object _nextIdentityIndexOfIfAbsent_<ElementTypeP>(Object receiver, Object element, Object startIndex, Object endIndex, Object noSuchElementAction) 
				where ElementTypeP : ElementType {
				long index = ((ESIndexedSlotsObject<ElementType>)receiver).nextIdentityIndexOf((ElementTypeP)element, asHostLong(startIndex) - 1, asHostLong(endIndex) - 1);
				return index >= 0 ? index + 1 : asFunctor0(noSuchElementAction)();
			}

			public Object _identityIncludes_<ElementTypeP>(Object receiver, Object element) 
				where ElementTypeP : ElementType {
				return ((ESIndexedSlotsObject<ElementType>)receiver).identityIncludes((ElementTypeP)element);
			}
		
			public Object _appendElement_<ElementTypeP>(Object receiver, Object suffix) 
				where ElementTypeP : ElementType {
				return ((ESIndexedSlotsObject<ElementType>)receiver).appendElement((ElementTypeP)suffix);
			}		
		
			public Object _copyAppendingElement_<ElementTypeP>(Object receiver, Object suffix) 
				where ElementTypeP : ElementType {
				return ((ESIndexedSlotsObject<ElementType>)receiver).copyAppendingElement((ElementTypeP)suffix);
			}
				
			public Object _identityRemoveNextIfAbsent_<ElementTypeP>(Object receiver, Object removal, Object startIndex, Object endIndex, Object notFoundAction) 
				where ElementTypeP : ElementType {
				long index = ((ESIndexedSlotsObject<ElementType>)receiver).identityRemoveNext((ElementTypeP)removal, (long)startIndex - 1, (long)endIndex - 1);
				return index >= 0 ? index + 1 : asFunctor0(notFoundAction)();
			}
		
			public Object _copyIdentityRemovingNextIfAbsent_<ElementTypeP>(Object receiver, Object removal, Object startIndex, Object endIndex, Object notFoundAction) 
				where ElementTypeP : ElementType {
				return ((ESIndexedSlotsObject<ElementType>)receiver).copyIdentityRemovingNext((ElementTypeP)removal, (long)startIndex - 1, (long)endIndex - 1, asFunctor0(notFoundAction));
			}
		
			public Object _identityRemoveAllOccurrencesOf_<ElementTypeP>(Object receiver, Object removal) 
				where ElementTypeP : ElementType {
				return ((ESIndexedSlotsObject<ElementType>)receiver).identityRemoveAllOccurrencesOf((ElementTypeP)removal);
			}
		
			public Object _copyIdentityRemovingAllOccurrencesOf_<ElementTypeP>(Object receiver, Object removal) 
				where ElementTypeP : ElementType {
				return ((ESIndexedSlotsObject<ElementType>)receiver).copyIdentityRemovingAllOccurrencesOf((ElementTypeP)removal);
			}
		
			public Object _insertElementAt_<ElementTypeP>(Object receiver, Object infix, Object insertionIndex) 
				where ElementTypeP : ElementType {
				try {
					return ((ESIndexedSlotsObject<ElementType>)receiver).insertElementAt((ElementTypeP)infix, asHostLong(insertionIndex) - 1);
				} catch (InvalidCastException castEx) {
					if (infix.GetType() == castEx.TargetSite.DeclaringType) {
						throw new PrimInvalidOperandException("Invalid suffix", castEx);
					} else {
						throw castEx;
					}
				}
			}
		
			public Object _copyInstertingElementAt_<ElementTypeP>(Object receiver, Object infix, Object insertionIndex) {
				try {
					return ((ESIndexedSlotsObject<ElementType>)receiver).copyInstertingElementAt((ElementType)infix, asHostLong(insertionIndex) - 1);
				} catch (InvalidCastException castEx) {
					if (infix.GetType() == castEx.TargetSite.DeclaringType) {
						throw new PrimInvalidOperandException("Invalid suffix", castEx);
					} else {
						throw castEx;
					}
				}
			}

			public Object _nextIndexOfIfAbsent_<ElementTypeP>(Object receiver, Object element, Object startIndex, Object endIndex, Object noSuchElementAction)
				where ElementTypeP : ElementType {
				long index = ((ESIndexedSlotsObject<ElementType>)receiver).nextIndexOf((ElementTypeP)element, asHostLong(startIndex) - 1, asHostLong(endIndex) - 1);
				return index >= 0 ? index + 1 : asFunctor0(noSuchElementAction)();
			}

			public Object _includes_<ElementTypeP>(Object receiver, Object element)  
				where ElementTypeP : ElementType {
				return ((ESIndexedSlotsObject<ElementType>)receiver).includes((ElementTypeP)element);
			}
		
			public Object _removeNextIfAbsent_<ElementTypeP>(Object receiver, Object removal, Object startIndex, Object endIndex, Object notFoundAction)  
				where ElementTypeP : ElementType {
				long index = ((ESIndexedSlotsObject<ElementType>)receiver).removeNext((ElementTypeP)removal, (long)startIndex - 1, (long)endIndex - 1);
				return index >= 0 ? index + 1 : asFunctor0(notFoundAction)();
			}
		
			public Object _copyRemovingNextIfAbsent_<ElementTypeP>(Object receiver, Object removal, Object startIndex, Object endIndex, Object notFoundAction)  
				where ElementTypeP : ElementType {
				return ((ESIndexedSlotsObject<ElementType>)receiver).copyRemovingNext((ElementTypeP)removal, (long)startIndex - 1, (long)endIndex - 1, asFunctor0(notFoundAction));
			}
		
			public Object _removeAllOccurrencesOf_<ElementTypeP>(Object receiver, Object removal)  
				where ElementTypeP : ElementType {
				return ((ESIndexedSlotsObject<ElementType>)receiver).removeAllOccurrencesOf((ElementTypeP)removal);
			}
		
			public Object _copyRemovingAllOccurrencesOf_<ElementTypeP>(Object receiver, Object removal)  
				where ElementTypeP : ElementType {
				return ((ESIndexedSlotsObject<ElementType>)receiver).copyRemovingAllOccurrencesOf((ElementTypeP)removal);
			}

			#endregion

			#endregion

		}
		
	}

	#endregion

	public class ESArray : ESIndexedSlotsObject<Object>, IEnumerable, IEnumerable<Object> {
		
		#region Static variables and functions
		
		public static implicit operator Object[](ESArray array) {
			return array.IndexedSlots;  
		}
						
		#endregion
				
		protected FuncNs.Func<Object, Object, Object> areEqual;

		public ESArray(ESBehavior esClass) : base(esClass, 0) {
		}
				
		public ESArray(ESBehavior esClass, long size) : base(esClass, size) {
		}
		
		public ESArray(ESBehavior esClass, Object[] slots) : base(esClass, slots) {
		}
		
		protected override void initialize(Object[] slots) {
			base.initialize(slots);

			ESBlock equalsBlock;
			var kernel = Class.Kernel;
			kernel.compile(new StringReader(":left :right | left = right"), kernel.SmalltalkNamespace, null, out equalsBlock);
			areEqual = equalsBlock.F2;

		}

		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.IndexedObjectSlots;}
		}
		
		public override ESArray asESArray() {
			return this;
		}

		protected override bool eachHasSameValue(Object left, Object right) {
			if (left == null) return right == null;
			if (right == null) return false;
			return (bool)areEqual(left, right);
		}
		
		protected bool elementsHaveSameValue(Object[] left, Object[] right) {
			if (ReferenceEquals(left, right)) return true;
			if (left == null) return right.Length == 0;
			if (right == null) return left.Length == 0;
			long leftLength = left.Length;
			long rightLength = right.Length;
			long sizeDiff = leftLength - rightLength;
			if (sizeDiff != 0) return false;
			for (var i = 0; i < leftLength; i++) {
				var leftObject = left[i];
				var rightObject = right[i];
				if (leftObject == null) {
					if (rightObject != null) return false;
				} else if (rightObject == null) {
					return false;
				} else if (!(bool)areEqual(leftObject, rightObject)) {
					return false;
				}
			}
			return true;
		}
		
		public override bool hasSameValueAs(ESObject other) {
			if (ReferenceEquals(this, other)) return true;
			ESIndexedSlotsObject<Object> esArray = other as ESIndexedSlotsObject<Object>;
			if (esArray == null) return false;
			return elementsHaveSameValue(IndexedSlots, esArray.IndexedSlots);
		}

		public void elementsDo(FuncNs.Func<Object, Object> enumerator) {
			long mySize = slots.Length;
			for (long slotIndex = 0; slotIndex < mySize; slotIndex++) {
				enumerator((slots[slotIndex]));
			}
		}
			
		public void elementsFromToDo(long startIndex, long endIndex, FuncNs.Func<Object, Object> enumerator) {
			long mySize = slots.Length;
			startIndex = Math.Min(Math.Max(0, startIndex), mySize);
			endIndex = Math.Min(Math.Max(0, endIndex), mySize);
			for (long slotIndex = startIndex; slotIndex <= endIndex; slotIndex++) {
				enumerator((slots[slotIndex]));
			}
		}
			
		public void elementsFromToByDo(long startIndex, long endIndex, long step, FuncNs.Func<Object, Object> enumerator) {
			long mySize = slots.Length;
			startIndex = Math.Min(Math.Max(0, startIndex), mySize);
			endIndex = Math.Min(Math.Max(0, endIndex), mySize);
			if (step > 0) {
				for (long slotIndex = startIndex; slotIndex <= endIndex; slotIndex += step) {
					enumerator((slots[slotIndex]));
				}
			} else if (step < 0) {
				for (long slotIndex = startIndex; slotIndex >= endIndex; slotIndex += step) {
					enumerator((slots[slotIndex]));
				}
			} else {
				throw new PrimInvalidOperandException("Step value must not be zero");
			}
		}
		
		IEnumerator IEnumerable.GetEnumerator() {
			return slots.GetEnumerator();
		}
		
		public IEnumerator<Object> GetEnumerator() {
			return new ESIndexedObjectSlotsObjectEnumerator(this);
		}

		public class ESIndexedObjectSlotsObjectEnumerator : IEnumerator<Object> {
			private ESIndexedSlotsObject<Object> esArray;
			private int index;

			internal ESIndexedObjectSlotsObjectEnumerator(ESIndexedSlotsObject<Object> esArray) {
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

			public Object Current {
				get {return esArray[index];}
			}

			Object IEnumerator.Current {
				get {return Current;}
			}

		}		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			printNamedInstanceVariablesUsing(depth, append, newLine);
			append(" size: ");
			append(size().ToString());
			append(" |");
			print(slots, depth, append, newLine);
		}

		public override T valueBy<T>(Operation<T> operation) {
		    return operation.applyToIndexedObjectSlotsObject(this);
		}

		public new class Primitives : ESIndexedSlotsObject<Object>.Primitives {

			protected override void bindToKernel() {
				domainClass = kernel.ArrayClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.IndexedObjectSlots;}
			}

			#region Primitive Definitions
		
			public Object _at_(Object receiver, Object slotIndexObject) {
				var array = (ESIndexedSlotsObject<Object>)receiver;
				var slots = array.IndexedSlots;
				var slotIndex = ((long)slotIndexObject) - 1;
				return slots[slotIndex];
			}

			public Object _atPut_(Object receiver, Object slotIndexObject, Object newValue) {
				var array = (ESIndexedSlotsObject<Object>)receiver;
				if (array.IsImmutable) throw new ImmutableObjectException();
				var slots = array.IndexedSlots;
				var slotIndex = ((long)slotIndexObject) - 1;
				slots[slotIndex] = newValue;
				return newValue;
			}
		
			public Object _atFirstPutOrPrepend_(Object receiver, Object newValue) {
				return ((ESIndexedSlotsObject<Object>)receiver).atFirstPutOrPrepend(newValue);
			}
		
			public Object _atLastPutOrAppend_(Object receiver, Object newValue) {
				return ((ESIndexedSlotsObject<Object>)receiver).atLastPutOrAppend(newValue);
			}
		
			public Object _elementsDo_(Object receiver, Object enumerator) {
				((ESArray)receiver).elementsDo(asFunctor1(enumerator));
				return receiver;
			}

			public Object _elementsFromToDo_(Object receiver, Object startIndex, Object endIndex, Object enumerator) {
				((ESArray)receiver).elementsFromToDo((long)startIndex - 1, (long)endIndex - 1, asFunctor1(enumerator));
				return receiver;
			}
		
			public Object _elementsFromToByDo_(Object receiver, Object startIndex, Object endIndex, Object step, Object enumerator) {
				((ESArray)receiver).elementsFromToByDo((long)startIndex - 1, (long)endIndex - 1, (long)step, asFunctor1(enumerator));
				return receiver;
			}

			#endregion

			public override void publishCanonicalPrimitives() {

				publishPrimitive("at:",							new FuncNs.Func<Object, Object, Object>(_at_));

				publishPrimitive("do:",							new FuncNs.Func<Object, Object, Object>(_elementsDo_));
				publishPrimitive("from:to:do:",						new FuncNs.Func<Object, Object, Object, Object, Object>(_elementsFromToDo_));
				publishPrimitive("from:to:by:do:",					new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_elementsFromToByDo_));

				publishPrimitive("atFirstPutOrAdd:",					new FuncNs.Func<Object, Object, Object>(_atFirstPutOrPrepend_));
				publishPrimitive("atLastPutOrAdd:",					new FuncNs.Func<Object, Object, Object>(_atLastPutOrAppend_));

				publishPrimitive("at:put:",						new FuncNs.Func<Object, Object, Object, Object>(_atPut_));
				publishPrimitive("nextIdentityIndexOf:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIdentityIndexOfIfAbsent_<Object>));
				publishPrimitive("identityIncludes:",					new FuncNs.Func<Object, Object, Object>(_identityIncludes_<Object>));
				publishPrimitive("add:",						new FuncNs.Func<Object, Object, Object>(_appendElement_<Object>));
				publishPrimitive("copyWith:",						new FuncNs.Func<Object, Object, Object>(_copyAppendingElement_<Object>));
				publishPrimitive("identityRemoveNext:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_identityRemoveNextIfAbsent_<Object>));	
				publishPrimitive("copyIdentityRemovingNext:from:to:ifAbsent:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyIdentityRemovingNextIfAbsent_<Object>));	
				publishPrimitive("identityRemoveAll:",					new FuncNs.Func<Object, Object, Object>(_identityRemoveAllOccurrencesOf_<Object>));	
				publishPrimitive("copyIdentityRemovingAll:",				new FuncNs.Func<Object, Object, Object>(_identityRemoveAllOccurrencesOf_<Object>));	
				publishPrimitive("add:beforeIndex:",					new FuncNs.Func<Object, Object, Object, Object>(_insertElementAt_<Object>));
				publishPrimitive("copyAdding:beforeIndex:",				new FuncNs.Func<Object, Object, Object, Object>(_copyInstertingElementAt_<Object>));

				publishPrimitive("nextIndexOf:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIndexOfIfAbsent_<Object>));
				publishPrimitive("includes:",						new FuncNs.Func<Object, Object, Object>(_includes_<Object>));
				publishPrimitive("removeNext:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_removeNextIfAbsent_<Object>));	
				publishPrimitive("copyRemovingNext:from:to:ifAbsent:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_copyRemovingNextIfAbsent_<Object>));	
				publishPrimitive("removeAll:",						new FuncNs.Func<Object, Object, Object>(_removeAllOccurrencesOf_<Object>));
				publishPrimitive("copyRemovingAll:", /* #copyWithout: */		new FuncNs.Func<Object, Object, Object>(_copyRemovingAllOccurrencesOf_<Object>));

				publishPrimitive("setSize:",						new FuncNs.Func<Object, Object, Object>(_setSize_));
				publishPrimitive("copyWithSize:",					new FuncNs.Func<Object, Object, Object>(_copyWithSize_));
				publishPrimitive("firstIfNone:",					new FuncNs.Func<Object, Object, Object>(_firstIfNone_));
				publishPrimitive("lastIfNone:",						new FuncNs.Func<Object, Object, Object>(_lastIfNone_));
				publishPrimitive("findFirst:from:to:ifAbsent:",				new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_nextIndexSuchThatIfAbsent_));
				publishPrimitive("contains:",						new FuncNs.Func<Object, Object, Object>(_contains_));
				publishPrimitive("addAll:",						new FuncNs.Func<Object, Object, Object>(_appendAll_));
				publishPrimitive("copyAddingAll:",					new FuncNs.Func<Object, Object, Object>(_copyAppendingAll_));
				publishPrimitive("copyFrom:to:",					new FuncNs.Func<Object, Object, Object, Object>(_copyFromTo_));
				publishPrimitive("copyTo:",						new FuncNs.Func<Object, Object, Object>(_prefixTo_));
				publishPrimitive("copyFrom:",						new FuncNs.Func<Object, Object, Object>(_suffixFrom_));
				publishPrimitive("withAllButFirst",					new FuncNs.Func<Object, Object>(_withAllButFirst_));
				publishPrimitive("withAllButLast",					new FuncNs.Func<Object, Object>(_withAllButLast_));
				publishPrimitive("withFirst",						new FuncNs.Func<Object, Object>(_withFirst_));
				publishPrimitive("withLast",						new FuncNs.Func<Object, Object>(_withLast_));

				publishPrimitive("removeAt:",						new FuncNs.Func<Object, Object, Object>(_removeAt_));		// "Delete index"
				publishPrimitive("copyRemovingAt:",					new FuncNs.Func<Object, Object, Object>(_copyRemovingAt_));	// "Copy without index"
				publishPrimitive("removeFrom:to:",					new FuncNs.Func<Object, Object, Object, Object>(_removeFromTo_));		// "Delete range"
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

}
