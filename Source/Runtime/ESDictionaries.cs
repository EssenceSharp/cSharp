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
#if CLR2
using FuncNs = Microsoft.Scripting.Utils;
#else
using FuncNs = System;
#endif
using EssenceSharp.Exceptions.System;
using EssenceSharp.Exceptions.System.PrimitiveFailures;
#endregion

namespace EssenceSharp.Runtime {

	public abstract class ESAbstractAssociation<KeyType, ValueType> : ESInitiallyMutableObject 
		where KeyType : class
		where ValueType : class {

		#region Static variables and functions

		public static readonly byte									keyMutabilityFlagBit			= 2;

		#endregion
 
		protected KeyType										key					= default(KeyType);
		protected ValueType										value					= default(ValueType);

		protected ESAbstractAssociation(ESBehavior esClass) : base(esClass) {
		}
 
		protected ESAbstractAssociation(ESBehavior esClass, KeyType key, ValueType value) : base(esClass) {
			this.key = key;
			this.value = value;
		}

		public virtual bool IsDeletable {
			get {return true;}
		}

		public bool IsKeyImmutable {
			get {return (statusFlags & keyMutabilityFlagBit) != 0;}
		}

		public virtual KeyType Key {
			get {return key;}
			set {if (IsKeyImmutable) throw new ImmutableObjectException();
			     key = value;}
		}

		public virtual ValueType Value {
			get {return value;}
			set {if (IsImmutable) throw new ImmutableObjectException();
			     this.value = value;}
		}

		public override void beImmutable() {
			statusFlags |= keyMutabilityFlagBit;
			base.beImmutable();
		}

		public void keyBeImmutable() {
			statusFlags |= keyMutabilityFlagBit;
		}
		
		public virtual void setKeyAndValue(KeyType newKey, ValueType newValue) {
			if (IsImmutable || IsKeyImmutable) throw new ImmutableObjectException();
			key = newKey;
			value = newValue;
		}

		public virtual void setKey(KeyType newKey) {
			if (IsImmutable || IsKeyImmutable) throw new ImmutableObjectException();
			key = newKey;
		}

		public virtual void setKeyImmutably(KeyType newKey) {
			if (IsImmutable || IsKeyImmutable) throw new ImmutableObjectException();
			key = newKey;
			statusFlags |= keyMutabilityFlagBit;
		}

		public virtual void setValue(ValueType newValue) {
			if (IsImmutable) throw new ImmutableObjectException();
			value = newValue;
		}

		public virtual void setValueImmutably(Object newValue) {
			if (IsImmutable) throw new ImmutableObjectException();
			value = (ValueType)newValue;
			base.beImmutable();
		}

		public ESAbstractAssociation<KeyType, ValueType> withKey(KeyType newKey) {
			ESAbstractAssociation<KeyType, ValueType> mutableCopy = (ESAbstractAssociation<KeyType, ValueType>)base.copy();
			mutableCopy.Key = newKey;
			if (IsImmutable) {
				mutableCopy.beImmutable();
			} else if (IsKeyImmutable) {
				mutableCopy.keyBeImmutable();
			}
			return mutableCopy;
		}

		public virtual ESAbstractAssociation<KeyType, ValueType> withValue(ValueType newValue) {
			ESAbstractAssociation<KeyType, ValueType> mutableCopy = (ESAbstractAssociation<KeyType, ValueType>)base.copy();
			mutableCopy.Value = newValue;
			if (IsImmutable) {
				mutableCopy.beImmutable();
			} else if (IsKeyImmutable) {
				mutableCopy.keyBeImmutable();
			}
			return mutableCopy;
		}

		public virtual ESAbstractAssociation<KeyType, ValueType> withKeyAndValue(KeyType newKey, ValueType newValue) {
			ESAbstractAssociation<KeyType, ValueType> mutableCopy = (ESAbstractAssociation<KeyType, ValueType>)base.copy();
			mutableCopy.setKeyAndValue(newKey, newValue);
			if (IsImmutable) {
				mutableCopy.beImmutable();
			} else if (IsKeyImmutable) {
				mutableCopy.keyBeImmutable();
			}
			return mutableCopy;
		}

		public override void postCopy() {
			base. postCopy();
			statusFlags |= keyMutabilityFlagBit;
		}

		public KeyValuePair<KeyType, ValueType> asKeyValuePair() {
			return new KeyValuePair<KeyType, ValueType>(key, value);
		}

	}

	public class ESAssociation : ESAbstractAssociation<Object, Object> {
		
		public static implicit operator KeyValuePair<Object, Object>(ESAssociation association) {
			return new KeyValuePair<Object, Object>(association.Key, association.Value);  
		}
	
		public ESAssociation(ESBehavior esClass) : base(esClass) {}

		public ESAssociation(ESBehavior esClass, Object key, Object value) : base(esClass, key, value) {}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (key == null) {
				append("nil");
			} else {
				ESObject esValue = key as ESObject;
				if (esValue == null) {
					append(key.ToString());
				} else {
					esValue.printUsing(depth, append, newLine);
				}
			}
			append(" -> ");
			if (value == null) {
				append("nil");
			} else {
				ESObject esValue = value as ESObject;
				if (esValue == null) {
					append(value.ToString());
				} else {
					esValue.printUsing(depth, append, newLine);
				}
			}
		}

		public override T valueBy<T>(Operation<T> operation) {
		    return operation.applyToAssociation(this);
		}

		public new class Primitives : PrimitiveDomain {

			protected override void bindToKernel() {
				domainClass = kernel.AssociationClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.Association;}
			}

			#region Primitive Definitions
		
			public Object _isDeletable_(Object receiver) {
				return ((ESAssociation)receiver).IsDeletable;
			}
		
			public Object _isKeyImmutable_(Object receiver) {
				return ((ESAssociation)receiver).IsKeyImmutable;
			}
		
			public Object _keyBeImmutable_(Object receiver) {
				((ESAssociation)receiver).keyBeImmutable();
				return receiver;
			}
		
			public Object _key_(Object receiver) {
				return ((ESAssociation)receiver).Key;
			}
		
			public Object _value_(Object receiver) {
				return ((ESAssociation)receiver).Value;
			}
		
			public Object _setKeyAndValue_(Object receiver, Object key, Object value) {
				((ESAssociation)receiver).setKeyAndValue(key, value);
				return receiver;
			}
		
			public Object _setKey_(Object receiver, Object key) {
				((ESAssociation)receiver).setKey(key);
				return receiver;
			}
		
			public Object _setKeyImmutably_(Object receiver, Object key) {
				((ESAssociation)receiver).setKeyImmutably(key);
				return receiver;
			}
		
			public Object _setValue_(Object receiver, Object value) {
				((ESAssociation)receiver).setValue(value);
				return receiver;
			}
		
			public Object _setValueImmutably_(Object receiver, Object value) {
				((ESAssociation)receiver).setValueImmutably(value);
				return receiver;
			}
		
			public Object _withKey_(Object receiver, Object newKey) {
				return ((ESAssociation)receiver).withKey(newKey);
			}
		
			public Object _withValue_(Object receiver, Object newValue) {
				return ((ESAssociation)receiver).withValue(newValue);
			}
		
			public Object _withKeyAndValue_(Object receiver, Object newKey, Object newValue) {
				return ((ESAssociation)receiver).withKeyAndValue(newKey, newValue);
			}

			#endregion

			public override void publishCanonicalPrimitives() {

				publishPrimitive("isDeletable",							new FuncNs.Func<Object, Object>(_isDeletable_));
				publishPrimitive("isKeyImmutable",						new FuncNs.Func<Object, Object>(_isKeyImmutable_));
				publishPrimitive("keyBeImmutable",						new FuncNs.Func<Object, Object>(_keyBeImmutable_));
				publishPrimitive("key",								new FuncNs.Func<Object, Object>(_key_));
				publishPrimitive("value",							new FuncNs.Func<Object, Object>(_value_));
				publishPrimitive("key:value:",							new FuncNs.Func<Object, Object, Object, Object>(_setKeyAndValue_));
				publishPrimitive("key:",							new FuncNs.Func<Object, Object, Object>(_setKey_));
				publishPrimitive("immutableKey:",						new FuncNs.Func<Object, Object, Object>(_setKeyImmutably_));
				publishPrimitive("value:",							new FuncNs.Func<Object, Object, Object>(_setValue_));
				publishPrimitive("immutableValue:",						new FuncNs.Func<Object, Object, Object>(_setValueImmutably_));
				publishPrimitive("withKey:",							new FuncNs.Func<Object, Object, Object>(_withKey_));
				publishPrimitive("withValue:",							new FuncNs.Func<Object, Object, Object>(_withValue_));
				publishPrimitive("withKey:value",						new FuncNs.Func<Object, Object, Object, Object>(_withKeyAndValue_));

			}

		}

	}

	public interface ESGenericDictionary<KeyType, ValueType, AssociationType> : NamedSlotsObject, IDictionary<KeyType, ValueType> 
		where AssociationType : ESAbstractAssociation<KeyType, ValueType> 
		where KeyType : class
		where ValueType : class {

		bool isEmpty {get;}

		AssociationType associationAt(KeyType key);
		AssociationType associationAtIfAbsent(KeyType key, FuncNs.Func<AssociationType> notFoundAction);
		ValueType at(KeyType key);
		ValueType atIfAbsent(KeyType key, FuncNs.Func<ValueType> notFoundAction);
		ValueType atIfAbsentPut(KeyType key, FuncNs.Func<ValueType> computeValueToBeAdded);
		void add(AssociationType newAssociation);
		void atPut(KeyType key, ValueType value);
		void atImmutablyPut(KeyType key, ValueType value);
		bool includesKey(KeyType key);
		Object removeKey(KeyType key);
		Object removeKeyIfAbsent(KeyType key, FuncNs.Func<Object> notFoundAction);
		void removeAll();
		void associationsDo(FuncNs.Func<AssociationType, Object> enumerator1);
		void keysDo(FuncNs.Func<KeyType, Object> enumerator1);
		void valuesDo(FuncNs.Func<ValueType, Object> enumerator1);
		void keysAndValuesDo(FuncNs.Func<KeyType, ValueType, Object> enumerator2);

	}

	public abstract class ESAbstractDictionary<KeyType, ValueType, AssociationType> 
		: ESNamedSlotsObject, ESGenericDictionary<KeyType, ValueType, AssociationType> 
		where AssociationType : ESAbstractAssociation<KeyType, ValueType> 
		where KeyType : class
		where ValueType : class {

		protected IDictionary<KeyType, AssociationType>							bindings				= null;
 
		protected ESAbstractDictionary(ESBehavior esClass) : base(esClass) {
			bindings = newBindings(3, null);
		}
 
		protected ESAbstractDictionary(ESBehavior esClass, long capacity) : base(esClass) {
			bindings = newBindings(capacity, null);
		}

		protected ESAbstractDictionary(ESBehavior esClass, AssociationType[] associations) :this(esClass, associations == null ? 3 : associations.Length * 2) {
			if (associations != null) foreach (var a in associations) add(a);
		}

		protected abstract IDictionary<KeyType, AssociationType> newBindings(long capacity, IEqualityComparer<KeyType> keyComparator);

		protected abstract AssociationType newAssociation(KeyType key, ValueType value);

		#region ES Dictionary protocol

		public override long size() {
			return bindings.Count;
		}

		public bool isEmpty {
			get {return size() == 0;}
		}

		public ValueType this[KeyType key] {
			get {return associationAtIfAbsent(key, delegate() {throw new KeyNotFoundException(key.ToString());}).Value;}
			set {atPut(key, value);}
		}

		public AssociationType associationAt(KeyType key) {
			return associationAtIfAbsent(key, delegate() {throw new KeyNotFoundException(key.ToString());});
		}

		public virtual AssociationType associationAtIfAbsent(KeyType key, FuncNs.Func<AssociationType> notFoundAction) {
			AssociationType association;
			if (bindings.TryGetValue(key, out association)) return association;
			return notFoundAction == null ? null : notFoundAction();
		}

		public ValueType at(KeyType key) {
			return associationAtIfAbsent(key, delegate() {throw new KeyNotFoundException(key.ToString());}).Value;
		}

		public virtual ValueType atIfAbsent(KeyType key, FuncNs.Func<ValueType> notFoundAction) {
			var association = associationAtIfAbsent(key, null);
			return association == null ? (notFoundAction == null ? null : notFoundAction()) : association.Value;
		}

		public ValueType atIfAbsentPut(KeyType key, FuncNs.Func<ValueType> computeValueToBeAdded) {
			return atIfAbsent(
					key, 
					delegate() {
						ValueType value = computeValueToBeAdded();
						AssociationType association = newAssociation(key, value);
						association.keyBeImmutable();
						bindings[key] = association;
						return value;
					});
		}

		public void add(AssociationType newAssociation) {
			AssociationType prevAssociation;
			if (bindings.TryGetValue(newAssociation.Key, out prevAssociation)) {
				prevAssociation.Value = newAssociation.Value;
			} else {
				newAssociation.keyBeImmutable();
				bindings[newAssociation.Key] = newAssociation;
			}
		}

		public void atPut(KeyType key, ValueType value) {
			AssociationType association;
			if (bindings.TryGetValue(key, out association)) {
				association.setValue(value);
			} else {
				association = newAssociation(key, value);
				association.keyBeImmutable();
				bindings[key] = association;
			}
		}

		public void atImmutablyPut(KeyType key, ValueType value) {
			AssociationType association;
			if (bindings.TryGetValue(key, out association)) {
				association.setValueImmutably(value);
			} else {
				association = newAssociation(key, value);
				association.beImmutable();
			}
		}

		public bool includesKey(KeyType key) {
			return bindings.ContainsKey(key);
		}

		public Object removeKey(KeyType key) {
			return removeKeyIfAbsent(key, delegate() {throw new KeyNotFoundException(key.ToString());});
		}

		public Object removeKeyIfAbsent(KeyType key, FuncNs.Func<Object> notFoundAction) {
			AssociationType association;
			if (bindings.TryGetValue(key, out association)) {
				if (!association.IsDeletable) throw new PrimNonDeletableKeyException("The association with the key " + key.ToString() + " is not removable from the KeyedCollection.");
				bindings.Remove(key);
				return association.Value;
			} else if (notFoundAction == null) {
				return null;
			} else {
				return notFoundAction();
			}
		}

		public virtual void removeAll() {
			bindings = new Dictionary<KeyType, AssociationType>();
		}

		public void associationsDo(FuncNs.Func<AssociationType, Object> enumerator1) {
			foreach (var kvp in bindings) {
				enumerator1(kvp.Value);
			}
		}

		public void keysDo(FuncNs.Func<KeyType, Object> enumerator1) {
			foreach (var kvp in bindings) {
				AssociationType association = kvp.Value;
				enumerator1(association.Key);
			}
		}

		public virtual void valuesDo(FuncNs.Func<ValueType, Object> enumerator1) {
			foreach (var kvp in bindings) {
				AssociationType association = kvp.Value;
				enumerator1(association.Value);
			}
		}

		public virtual void keysAndValuesDo(FuncNs.Func<KeyType, ValueType, Object> enumerator2) {
			foreach (var kvp in bindings) {
				AssociationType association = kvp.Value;
				enumerator2(association.Key, association.Value);
			}
		}

		#endregion

		#region Host System IDictionary protocol (interoperability)

		public bool IsReadOnly {
			get {return IsImmutable;}
		}

		public int Count {
			get {return (int)size();}
		}

		public ICollection<KeyType> Keys {
			get {return bindings.Keys;}
		}

		public ICollection<ValueType> Values {
			get {
				var list = new List<ValueType>();
				valuesDo(delegate(ValueType value) {list.Add(value); return value;});
				return list;}
		}

		public void Add(KeyValuePair<KeyType, ValueType> item) {
			atPut(item.Key, item.Value);
		}

		public void Add(KeyType key, ValueType value) {
			atPut(key, value);
		}

		public void Clear() {
			removeAll();
		}

		public bool Contains(KeyValuePair<KeyType, ValueType> item) {
			var association = associationAtIfAbsent(item.Key, null);
			return Equals(item.Value, association.Value);
		}

		public bool ContainsKey(KeyType key) {
			return includesKey(key);
		}

		public void CopyTo(KeyValuePair<KeyType, ValueType>[] array, int arrayIndex) {
			if (array == null) throw new ArgumentNullException();
			if (arrayIndex < 0) throw new ArgumentOutOfRangeException();
			var limit = arrayIndex + size() - 1;
			if (limit >= array.Length) throw new ArgumentException();
			long index = arrayIndex;
			foreach (var kvp in bindings) {
				array[index++] = new KeyValuePair<KeyType, ValueType>(kvp.Key, kvp.Value.Value);
			}
		}

		public bool Remove(KeyValuePair<KeyType, ValueType> item) {
			var value = removeKeyIfAbsent(item.Key, null);
			return value != null;
		}

		public bool Remove(KeyType key) {
			var value = removeKeyIfAbsent(key, null);
			return value != null;
		}

		public bool TryGetValue(KeyType key, out ValueType value) {
			var association = associationAtIfAbsent(key, null);
			if (association == null) {
				value = null;
				return false;
			}
			value = association.Value;
			return true;
		}

		public IEnumerator<KeyValuePair<KeyType, ValueType>> GetEnumerator() {
			return new ESDictionaryEnumerator(bindings.GetEnumerator());
		}

		IEnumerator IEnumerable.GetEnumerator() {
			return new ESDictionaryEnumerator(bindings.GetEnumerator());
		}

		public class ESDictionaryEnumerator : IEnumerator<KeyValuePair<KeyType, ValueType>> {
			private IEnumerator<KeyValuePair<KeyType, AssociationType>> bindingsEnumerator;

			internal ESDictionaryEnumerator(IEnumerator<KeyValuePair<KeyType, AssociationType>> bindingsEnumerator) {
				this.bindingsEnumerator = bindingsEnumerator;
			}

			public bool MoveNext() {
				return bindingsEnumerator.MoveNext();
			}

			public void Reset() {
				bindingsEnumerator.Reset();
			}

			void IDisposable.Dispose() {
				bindingsEnumerator.Dispose();
			}

			public KeyValuePair<KeyType, ValueType> Current {
				get {	var currentBaseKvp = bindingsEnumerator.Current;
					return currentBaseKvp.Value.asKeyValuePair();}
			}

			Object IEnumerator.Current {
				get {return Current;}
			}

		}

		#endregion

		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			foreach (var kvp in bindings) {
				newLine(depth);
				AssociationType association = kvp.Value;
				if (association == null) {
					append("nil");
				} else {
					association.printUsing(depth + 1, append, newLine);
				}
			}

		}

	}

	public class ESIdentityDictionary : ESAbstractDictionary<Object, Object, ESAssociation> {

		public ESIdentityDictionary(ESBehavior esClass) : base(esClass) {}

		public ESIdentityDictionary(ESBehavior esClass, long capacity) : base(esClass, capacity) {}

		public ESIdentityDictionary(ESBehavior esClass, ESAssociation[] associations) :base(esClass, associations) {
		}

		protected virtual IEqualityComparer<Object> defaultKeyComparator() {
			return Class.Kernel.ObjectIdentityComparator;
		}

		protected override IDictionary<Object, ESAssociation> newBindings(long capacity, IEqualityComparer<Object> keyComparator) {
			return new Dictionary<Object, ESAssociation>((int)capacity, keyComparator ?? defaultKeyComparator());
		}

		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.IdentityDictionary;}
		}

		protected override ESAssociation newAssociation(Object key, Object value) {
			return Class.Kernel.newAssociation(key, value);
		}

		public override T valueBy<T>(Operation<T> operation) {
		    return operation.applyToIdentityDictionary(this);
		}

		public new class Primitives : PrimitiveDomain {

			protected override void bindToKernel() {
				domainClass = kernel.IdentityDictionaryClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.Dictionary;}
			}

			#region Primitive Definitions

			public Object _size_(Object receiver) {
				return ((ESDictionary)receiver).size();
			}

			public Object _isEmpty_(Object receiver) {
				return ((ESDictionary)receiver).isEmpty;
			}

			public Object _associationAt_(Object receiver, Object key) {
				return ((ESDictionary)receiver).associationAt(key);
			}

			public Object _associationAtIfAbsent_(Object receiver, Object key, Object absentAction) {
				ESAssociation association = ((ESDictionary)receiver).associationAtIfAbsent(key, null);
				return association ?? (absentAction == null ? null : asFunctor0(absentAction)());
			}

			public Object _at_(Object receiver, Object key) {
				return ((ESDictionary)receiver).at(key);
			}

			public Object _atIfAbsent_(Object receiver, Object key, Object absentAction) {
				return ((ESDictionary)receiver).atIfAbsent(key, asFunctor0(absentAction));
			}

			public Object _atIfAbsentPut_(Object receiver, Object key, Object computeValueToBeAdded) {
				return ((ESDictionary)receiver).atIfAbsentPut(key, asFunctor0(computeValueToBeAdded));
			}

			public Object _add_(Object receiver, Object newAssociation) {
				((ESDictionary)receiver).add((ESAssociation)newAssociation);
				return receiver;
			}

			public Object _atPut_(Object receiver, Object key, Object newValue) {
				((ESDictionary)receiver).atPut(key, newValue);
				return newValue;
			}

			public Object _atImmutablyPut_(Object receiver, Object key, Object newValue) {
				((ESDictionary)receiver).atImmutablyPut(key, newValue);
				return newValue;
			}

			public Object _includesKey_(Object receiver, Object key) {
				return ((ESDictionary)receiver).includesKey(key);
			}

			public Object _removeKey_(Object receiver, Object key) {
				return ((ESDictionary)receiver).removeKey(key);
			}

			public Object _removeKeyIfAbsent_(Object receiver, Object key, Object notFoundAction) {
				return ((ESDictionary)receiver).removeKeyIfAbsent(key, asFunctor0(notFoundAction));
			}

			public Object _associationsDo_(Object receiver, Object enumerator1) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator1);
				((ESDictionary)receiver).associationsDo(association => f1(association));
				return receiver;
			}

			public Object _keysDo_(Object receiver, Object enumerator1) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator1);
				((ESDictionary)receiver).keysDo(key => f1(key));
				return receiver;
			}

			public Object _valuesDo_(Object receiver, Object enumerator1) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator1);
				((ESDictionary)receiver).valuesDo(key => f1(key));
				return receiver;
			}

			public Object _keysAndValuesDo_(Object receiver, Object enumerator2) {
				FuncNs.Func<Object, Object, Object> f2 = asFunctor2(enumerator2);
				((ESDictionary)receiver).keysAndValuesDo((key, value) => f2(key, value));
				return receiver;
			}

			#endregion

			public override void publishCanonicalPrimitives() {

				publishPrimitive("size",							new FuncNs.Func<Object, Object>(_size_));
				publishPrimitive("isEmpty",							new FuncNs.Func<Object, Object>(_isEmpty_));
				publishPrimitive("associationAt:",						new FuncNs.Func<Object, Object, Object>(_associationAt_));
				publishPrimitive("associationAt:ifAbsent:",					new FuncNs.Func<Object, Object, Object, Object>(_associationAtIfAbsent_));
				publishPrimitive("at:",								new FuncNs.Func<Object, Object, Object>(_at_));
				publishPrimitive("at:ifAbsent:",						new FuncNs.Func<Object, Object, Object, Object>(_atIfAbsent_));
				publishPrimitive("at:ifAbsentPut:",						new FuncNs.Func<Object, Object, Object, Object>(_atIfAbsentPut_));
				publishPrimitive("add:",							new FuncNs.Func<Object, Object, Object>(_add_));
				publishPrimitive("at:put:",							new FuncNs.Func<Object, Object, Object, Object>(_atPut_));
				publishPrimitive("at:immutablyPut:",						new FuncNs.Func<Object, Object, Object, Object>(_atImmutablyPut_));
				publishPrimitive("includesKey:",						new FuncNs.Func<Object, Object, Object>(_includesKey_));
				publishPrimitive("removeKey:",							new FuncNs.Func<Object, Object, Object>(_removeKey_));
				publishPrimitive("removeKey:ifAbsent:",						new FuncNs.Func<Object, Object, Object, Object>(_removeKeyIfAbsent_));
				publishPrimitive("associationsDo:",						new FuncNs.Func<Object, Object, Object>(_associationsDo_));
				publishPrimitive("keysDo:",							new FuncNs.Func<Object, Object, Object>(_keysDo_));
				publishPrimitive("valuesDo:",		/* do: */				new FuncNs.Func<Object, Object, Object>(_valuesDo_));
				publishPrimitive("keysAndValuesDo:",						new FuncNs.Func<Object, Object, Object>(_keysAndValuesDo_));

			}

		}

	}

	public class ESDictionary : ESIdentityDictionary {

		public ESDictionary(ESBehavior esClass) : base(esClass) {}

		public ESDictionary(ESBehavior esClass, long capacity) : base(esClass, capacity) {}

		public ESDictionary(ESBehavior esClass, ESAssociation[] associations) :base(esClass, associations) {
		}

		protected override IEqualityComparer<Object> defaultKeyComparator() {
			return Class.Kernel.newObjectEqualityComparator();
		}

		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.Dictionary;}
		}

		public override T valueBy<T>(Operation<T> operation) {
		    return operation.applyToDictionary(this);
		}

	}

}
