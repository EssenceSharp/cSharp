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
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Dynamic;
#if CLR2
using Microsoft.Scripting.Ast;
using FuncNs = Microsoft.Scripting.Utils;
#else
using System.Linq.Expressions;
using FuncNs = System;
#endif
using EssenceSharp.ClientServices;
using EssenceSharp.UtilityServices;
using EssenceSharp.Exceptions.System;
using EssenceSharp.Runtime.Binding;
using EssenceSharp.Exceptions.System.PrimitiveFailures;
#endregion

namespace EssenceSharp.Runtime {

	#region Interfaces

	public interface MethodSource {
		
		#region Methods -- Smalltalk Protocol
		
		void selectorsDo(FuncNs.Func<Object, Object> enumerator1);
		void selectorsDo(FuncNs.Func<Object, Object> enumerator1, HashSet<ESSymbol> exclusionSet);
		void selectorsAndMethodsDo(FuncNs.Func<Object, Object, Object> enumerator2);
		void selectorsAndMethodsDo(FuncNs.Func<Object, Object, Object> enumerator2, HashSet<ESSymbol> exclusionSet);
		HashSet<ESSymbol> selectors();
		bool includesSelector(ESSymbol selector);

		void allSelectorsDo(FuncNs.Func<Object, Object> enumerator1);
		void allSelectorsDo(FuncNs.Func<Object, Object> enumerator1, HashSet<ESSymbol> exclusionSet);
		void allSelectorsAndMethodsDo(FuncNs.Func<Object, Object, Object> enumerator2);
		void allSelectorsAndMethodsDo(FuncNs.Func<Object, Object, Object> enumerator2, HashSet<ESSymbol> exclusionSet);
		HashSet<ESSymbol> allSelectors();
		bool canUnderstand(ESSymbol selector);
		ESMethod compiledMethodAt(ESSymbol selector);

		void withUnimplementedMessagesSentToSelfDo(Action<ESSymbol> enumerator1);

		#endregion
		
		#region Methods -- Host System Protocol

		// Operations on methods whose selectors conform to host system (CLR) naming conventions

		void systemSelectorsDo(FuncNs.Func<Object, Object, Object> enumerator2);
		void systemSelectorsDo(FuncNs.Func<Object, Object, Object> enumerator2, HashSet<String> exclusionSet);
		void systemSelectorsAndMethodsDo(FuncNs.Func<Object, Object, Object, Object> enumerator3);
		void systemSelectorsAndMethodsDo(FuncNs.Func<Object, Object, Object, Object> enumerator3, HashSet<String> exclusionSet);
		bool includesSystemSelector(String systemSelector, long numArgs);

		bool canUnderstandSystemMessage(String systemSelector, long numArgs);
		ESMethod compiledMethodAtSystemSelector(String systemSelector, long numArgs);
		
		#endregion

	}

	public interface MethodBinder : MethodSource {
		
		#region Methods -- Smalltalk Protocol
		
		ESMethod addMethod(ESMethod newMethod);
		bool removeSelector(ESSymbol selector);
		
		#endregion

		ESMethod addMethodBoundToSystemSelector(ESMethod newMethod, String systemSelector);
		
		#region Methods -- Host System Protocol

		// Operations on methods whose selectors conform to host system (CLR) naming conventions

		ESMethod bindMethodToSystemSelector(ESSymbol essenceSelector, String systemSelector);
		bool unbindMethodFromSystemSelector(String systemSelector, long numArgs);
		
		#endregion

	}

	public interface TraitUser {

		bool UsesTraits {get;}
		TraitUsageExpression TraitUsage {get;}
		TraitUser uses(TraitUsageExpression newUsedTraits);
		void invalidateUsedTraits(TraitUsageExpression source);
		void usedTraitsDo(Action<TraitUsageExpression> enumerator1);
		void withUsedTraitUnimplementedMessagesSentToSelfDo(Action<ESSymbol> enumerator1);
		void usedTraitExcludedSelectorsDo(Action<ESSymbol> enumerator1);
		void usedTraitAliasedSelectorsDo(Action<ESSymbol, ESSymbol> enumerator2);
		void usedTraitMethodConflictsDo(Action<ESSymbol, ESMethod> enumerator2);
		void usedTraitHostSystemMethodConflictsDo(Action<long, String, ESMethod> enumerator3);

	}

	public interface BehavioralObject : MethodBinder, TraitUser, NamespaceObject {

		ESObjectSpace ObjectSpace {get;}
 		bool HasSuperclass {get;}
		ESBehavior Superclass {get;}

		ESMethod localCompiledMethodAt(ESSymbol selector);
		bool localIncludesSelector(ESSymbol selector);
		void localMethodsDo(Action<Object> enumerator1);
		void localSelectorsDo(FuncNs.Func<Object, Object> enumerator1);
		void localSelectorsDo(FuncNs.Func<Object, Object> enumerator1, HashSet<ESSymbol> exclusionSet);
		void localSelectorsAndMethodsDo(FuncNs.Func<Object, Object, Object> enumerator2);
		void localSelectorsAndMethodsDo(FuncNs.Func<Object, Object, Object> enumerator2, HashSet<ESSymbol> exclusionSet);
		long localSelectorCount();
		HashSet<ESSymbol> localSelectors();

		long selectorCount();

		void withUnimplementedMessagesSentToSelfDo(Action<ESSymbol> enumerator1);

		ESMethod localCompiledMethodAtSystemSelector(String systemSelector, long numArgs);
		bool localIncludesSystemSelector(String systemSelector, long numArgs);
		void localSystemSelectorsDo(FuncNs.Func<Object, Object, Object> enumerator2);
		void localSystemSelectorsDo(FuncNs.Func<Object, Object, Object> enumerator2, HashSet<String> exclusionSet);
		void localSystemSelectorsAndMethodsDo(FuncNs.Func<Object, Object, Object, Object> enumerator3);
		void localSystemSelectorsAndMethodsDo(FuncNs.Func<Object, Object, Object, Object> enumerator3, HashSet<String> exclusionSet);
		long localSystemSelectorCount();

		long systemSelectorCount();

		#region Compiling Methods

		ESMethod compileMethod(ESSymbol protocol, TextReader sourceStream);

		#endregion

		#region Named instance variables

		ESSymbol[] InstanceVariableNames {get;}
		ESSymbol localInstVarNameAt(long index);
		ESSymbol instVarNameAt(long index);
		long instVarIndexFor(ESSymbol instanceVariableName);
		long BasicInstSize {get;}
		long SuperInstSize {get;}
		long InstSize {get;}
		void allInstVarNamesAndIndexesDo(System.Action<ESSymbol, long> enumerator2);

		#endregion

	}

	public interface TraitUsageExpression : NamedSlotsObject, TraitUser, MethodSource {

		ESObjectSpace  ObjectSpace {get;}
		TraitUsageExpression IdentityObject {get;}
		TraitUsageExpression Reduced {get;}

		void addUser(TraitUser aUser);
		void removeUser(TraitUser aUser);

		TraitUsageExpression combinedWith(TraitUsageExpression operand);
		TraitUsageExpression combinedWithTrait(Trait operand);
		TraitUsageExpression combinedWithTransformation(ESTraitTransformation operand);
		TraitUsageExpression combinedWithComposition(ESTraitComposition operand);

		TraitUsageExpression excluding(ESSymbol selector);
		TraitUsageExpression aliasing(ESSymbol sourceSelector, ESSymbol selectorAlias);

		void excludedSelectorsDo(Action<ESSymbol> enumerator1);
		void aliasedSelectorsDo(Action<ESSymbol, ESSymbol> enumerator2);
		void methodConflictsDo(Action<ESSymbol, ESMethod> enumerator2);
		void hostSystemMethodConflictsDo(Action<long, String, ESMethod> enumerator3);

		void handleSelectorAliasingCollision(ESSymbol sourceSelector, ESSymbol previousAlias, ESSymbol selectorAlias);

	}

	public interface Trait : BehavioralObject, TraitUsageExpression {
	}

	#endregion

	#region Comparators

	// The following is only necessary due to static typing:

	public class BehavioralObjectIdentityComparator : IdentityComparator<BehavioralObject> {}

	public class BehavorIdentityComparator : IdentityComparator<ESBehavior> {}

	public class TraitIUserIdentityComparator : IdentityComparator<TraitUser> {}

	public class TraitExpressionIdentityComparator : IdentityComparator<TraitUsageExpression> {}

	public class TraitIdentityComparator : IdentityComparator<Trait> {}

	public class BehavioralTraitIdentityComparator : IdentityComparator<ESBehavioralTrait> {}

	#endregion

	public abstract class ESBehavioralObject  : ESNamespace, BehavioralObject {

		#region Static variables and methods

		internal static readonly ESSymbol[]						emptyInstanceVariableNames	 	= new ESSymbol[0];
		internal static readonly Dictionary<ESSymbol, long>				emptyInstanceVariableIndexes 		= new Dictionary<ESSymbol, long>();

		#endregion

		#region Instance variables

		protected ESObjectSpace								objectSpace; 
		protected bool									constraintsMustBeSatisfied		= false;
		protected IDictionary<ESSymbol, ESMethod> 					methodDictionary; 
		protected IDictionary<long, IDictionary<String, ESMethod>>			hostSystemMethodDictionary; 
		protected TraitUsageExpression							traitUsage;	

		#endregion

		#region Constructors

		internal ESBehavioralObject() : base(null) {
			initialize();
		}

		protected ESBehavioralObject(ESBehavior metaClass) : base(metaClass) {
			initialize();
		}

		public ESBehavioralObject(ESBehavior metaClass, ESObjectSpace objectSpace) : this(metaClass) {
			setObjectSpace(objectSpace);
		}

		#endregion

		#region Internal Protocol

		protected HashSet<ESSymbol> newSelectorSet() {
			return new HashSet<ESSymbol>(new SymbolIdentityComparator());
		}

		protected HashSet<String> newHostSystemSelectorSet() {
			return new HashSet<String>();
		}
		
		protected IDictionary<ESSymbol, ESMethod> newMethodDictionary() {
			return new Dictionary<ESSymbol, ESMethod>();
		}

		protected IDictionary<long, IDictionary<String, ESMethod>> newHostSystemMethodDictionary() {
			return new Dictionary<long, IDictionary<String, ESMethod>>();
		}

		protected virtual void initialize() {
			methodDictionary = newMethodDictionary();
			hostSystemMethodDictionary = newHostSystemMethodDictionary();
		}

		public ESObjectSpace ObjectSpace {
			get {return objectSpace;}
		}

		protected void setObjectSpace(ESObjectSpace newKernel) {
			if (objectSpace == newKernel) return;
			if (objectSpace != null) unbindFromObjectSpace();
			objectSpace = newKernel;
			if (objectSpace != null) bindToObjectSpace();
		}

		protected virtual void unbindFromObjectSpace() {
			// By default, do nothing
		}

		protected virtual void bindToObjectSpace() {
			// By default, do nothing
		}

		protected IDictionary<String, ESMethod> newHostSystemMethodNameDictionary() {
			return new Dictionary<String, ESMethod>();
		}

		public IEqualityComparer<Object> ObjectIdentityComparator {
			get {return objectSpace.ObjectIdentityComparator;}
		}

		public virtual bool InstancesCanHaveNamedSlots {
			get {return false;}
		}

		public virtual void validate() {
			constraintsMustBeSatisfied = true;
		}

		#endregion

		#region General protocol

		public virtual bool IsHostSystemMetaclass {
			get {return false;}
		}

		public override void postCopy() {
			base.postCopy();
			constraintsMustBeSatisfied		= false;
			var oldMethodDictionary			= methodDictionary;
			methodDictionary = newMethodDictionary();
			foreach (var kvp in oldMethodDictionary) addMethod(kvp.Value);
			var oldHostSystemMethodDictionary	= hostSystemMethodDictionary;
			hostSystemMethodDictionary		= newHostSystemMethodDictionary();
			foreach (var arityKvp in oldHostSystemMethodDictionary) {
				var arity = arityKvp.Key;
				var hsmnd = arityKvp.Value;
				if (hsmnd.Count > 0) { 
					var newHsmnd = newHostSystemMethodNameDictionary();
					hostSystemMethodDictionary[arity] = newHsmnd;
					foreach (var hsMethodNameKvp in hsmnd) {
						var hostSystemMethodName = hsMethodNameKvp.Key;
						var method = hsMethodNameKvp.Value;
						var essenceSelector = method.Selector;
						newHsmnd[hostSystemMethodName] = compiledMethodAt(essenceSelector);
					}
				}
			}
			if (traitUsage != null) traitUsage.addUser(this);
		}

		#endregion
		
		#region Methods -- Smalltalk Protocol

		#region Methods defined locally--methods from Traits and/or superclasses excluded

		public void localMethodsDo(Action<Object> enumerator1) {
			foreach (var assoc in methodDictionary) enumerator1(assoc.Value);
		}
		
		public void localSelectorsDo(FuncNs.Func<Object, Object> enumerator1) {
			localSelectorsDo(enumerator1, null);
		}
		
		public void localSelectorsDo(FuncNs.Func<Object, Object> enumerator1, HashSet<ESSymbol> exclusionSet) {
			foreach (var assoc in methodDictionary) {
				var selector = assoc.Key;
				if (exclusionSet == null) { 
					enumerator1(selector);
				} else if (!exclusionSet.Contains(selector)) {
					exclusionSet.Add(selector);
					enumerator1(selector);
				}
			}
		}

		public void localSelectorsAndMethodsDo(FuncNs.Func<Object, Object, Object> enumerator2) {
			localSelectorsAndMethodsDo(enumerator2, null);
		}

		public void localSelectorsAndMethodsDo(FuncNs.Func<Object, Object, Object> enumerator2, HashSet<ESSymbol> exclusionSet) {
			foreach (var assoc in methodDictionary) {
				var selector = assoc.Key;
				if (exclusionSet == null) { 
					enumerator2(selector, assoc.Value);
				} else if (!exclusionSet.Contains(selector)) {
					exclusionSet.Add(selector);
					enumerator2(selector, assoc.Value);
				}
			}
		}
		
		public long localSelectorCount() {
			return methodDictionary.Count;
		}
		
		public HashSet<ESSymbol> localSelectors() {
			var selectors = newSelectorSet();
			localSelectorsDo(selectorObject => selectors.Add((ESSymbol)selectorObject), null);
			return selectors;
		}
		
		public bool localIncludesSelector(ESSymbol selector) {
			return methodDictionary.ContainsKey(selector);
		}

		protected ESMethod localCompiledMethodAt(ESSymbol selector, bool addIfAbsent) {
			ESMethod method;
			if (methodDictionary.TryGetValue(selector, out method)) {
				return method;
			} else if (addIfAbsent) {
				method = objectSpace.newMethod(selector, null, this, this);
				methodDictionary[selector] = method;
				return method;
			} else {
				return null;
			}
		}
		
		public ESMethod localCompiledMethodAt(ESSymbol selector) {
			return localCompiledMethodAt(selector, false);
		}


		#endregion

		#region Methods--non-inherited only (includes methods from Traits)

		public void selectorsDo(FuncNs.Func<Object, Object> enumerator1) {
			selectorsDo(enumerator1, newSelectorSet());
		}

		public void selectorsDo(FuncNs.Func<Object, Object> enumerator1, HashSet<ESSymbol> exclusionSet) {
			localSelectorsDo(enumerator1, exclusionSet);
			if (traitUsage != null) traitUsage.selectorsDo(enumerator1, exclusionSet);
		}

		public void selectorsAndMethodsDo(FuncNs.Func<Object, Object, Object> enumerator2) {
			selectorsAndMethodsDo(enumerator2, newSelectorSet());
		}

		public void selectorsAndMethodsDo(FuncNs.Func<Object, Object, Object> enumerator2, HashSet<ESSymbol> exclusionSet) {
			localSelectorsAndMethodsDo(enumerator2, exclusionSet);
			if (traitUsage != null) traitUsage.selectorsAndMethodsDo(enumerator2, exclusionSet);
		}

		public long selectorCount() {
			return localSelectorCount();
		}
		
		public HashSet<ESSymbol> selectors() {
			var selectors = newSelectorSet();
			selectorsDo(selectorObject => selectors.Add((ESSymbol)selectorObject), null);
			return selectors;
		}
		
		public bool includesSelector(ESSymbol selector) {
			return localIncludesSelector(selector) || (traitUsage != null && traitUsage.includesSelector(selector));
		}

		#endregion

		#region All methods--from whatever source derived
		
		public virtual void allSelectorsDo(FuncNs.Func<Object, Object> enumerator1) {
			allSelectorsDo(enumerator1, newSelectorSet());
		}
		
		public virtual void allSelectorsDo(FuncNs.Func<Object, Object> enumerator1, HashSet<ESSymbol> exclusionSet) {
			selectorsDo(enumerator1, exclusionSet);
		}

		public virtual void allSelectorsAndMethodsDo(FuncNs.Func<Object, Object, Object> enumerator2) {
			allSelectorsAndMethodsDo(enumerator2, newSelectorSet());
		}

		public virtual void allSelectorsAndMethodsDo(FuncNs.Func<Object, Object, Object> enumerator2, HashSet<ESSymbol> exclusionSet) {
			selectorsAndMethodsDo(enumerator2, exclusionSet);
		}
		
		public HashSet<ESSymbol> allSelectors() {
			var selectors = newSelectorSet();
			allSelectorsDo(selectorObject => selectors.Add((ESSymbol)selectorObject), null);
			return selectors;
		}

		public virtual bool canUnderstand(ESSymbol selector) {
			return includesSelector(selector);
		}
		
		public virtual ESMethod compiledMethodAt(ESSymbol selector) {
			ESMethod method;
			if (methodDictionary.TryGetValue(selector, out method)) return method;
			return traitUsage == null ? 
				null :
				traitUsage.compiledMethodAt(selector);
		}

		public void withUnimplementedMessagesSentToSelfDo(Action<ESSymbol> enumerator1) {
			var implementedSelectors = newSelectorSet();
			var allMessagesSentToSelf = newSelectorSet();
			allSelectorsAndMethodsDo((selectorObject, methodObject) =>  {
				var selector = (ESSymbol)selectorObject;
				implementedSelectors.Add(selector);
				var method = (ESMethod)methodObject;
				var messagesSentToSelf = method.MessagesSentToSelf;
				if (messagesSentToSelf != null) {
					foreach (var message in messagesSentToSelf) allMessagesSentToSelf.Add(message);
				}
				return null;
			}, null);
			foreach (var selectorSentToSelf in allMessagesSentToSelf) {
				if (!implementedSelectors.Contains(selectorSentToSelf)) enumerator1(selectorSentToSelf);
			}
		}

		#endregion

		#region Adding/Removing methods

		public ESMethod addMethod(ESMethod newMethod) {
			return addMethodBoundToSystemSelector(newMethod, null);
		}
		
		public ESMethod addMethodBoundToSystemSelector(ESMethod newMethod, String systemSelector) {
			if (newMethod == null) {
				objectSpace.throwInvalidArgumentException(Class, "addMethod:", "newMethod", newMethod);
				return null;
			}
			ESSymbol selector = newMethod.Selector;
			if (selector == null) {
				objectSpace.throwInvalidArgumentException(Class, "addMethod:", "newMethod", newMethod);
				return null;
			}
			ESMethod residentMethod;
			if (methodDictionary.TryGetValue(selector, out residentMethod)) {
				residentMethod.become(newMethod);				
			} else {
				residentMethod = newMethod.newCopyIn(this);
				methodDictionary[selector] = residentMethod;
			}
			changedBehavior();

			if (systemSelector == null) return residentMethod;
			systemSelector = String.Intern(systemSelector);

			long numArgs = residentMethod.NumArgs;
			IDictionary<String, ESMethod> hostMethodDict;
			if (!hostSystemMethodDictionary.TryGetValue(numArgs, out hostMethodDict)) {
				hostMethodDict = newHostSystemMethodNameDictionary();
				hostSystemMethodDictionary[numArgs] = hostMethodDict;
			}
			hostMethodDict[systemSelector] = residentMethod;
			residentMethod.addToProtocol(objectSpace.symbolFor("host system API"));
			return residentMethod;
		}

		public bool removeSelector(ESSymbol selector) {
			if (methodDictionary.Remove(selector)) {
				changedBehavior();
				return true;
			}
			return false;
		}

		protected virtual void changedBehavior() {
			incrementVersion();
		}

		#endregion
		
		#endregion
		
		#region Methods -- Host System Protocol

		// Operations on methods whose selectors conform to host system (CLR) naming conventions

		#region Methods defined locally--methods from Traits and/or superclasses excluded
		
		public void localSystemSelectorsDo(FuncNs.Func<Object, Object, Object> enumerator2) {
			localSystemSelectorsDo(enumerator2, null);	
		}
		
		public void localSystemSelectorsDo(FuncNs.Func<Object, Object, Object> enumerator2, HashSet<String> exclusionSet) {
			foreach (var numArgsAssoc in hostSystemMethodDictionary) {
				var arity = numArgsAssoc.Key;
				foreach (var selectorAssoc in numArgsAssoc.Value) { 
					var selector = selectorAssoc.Key;
					if (exclusionSet == null) {
						enumerator2(selector, arity);
					} else if (!exclusionSet.Contains(selector)) {
						exclusionSet.Add(selector);
						enumerator2(selector, arity);
					}
				}
			}		
		}	

		public void localSystemSelectorsAndMethodsDo(FuncNs.Func<Object, Object, Object, Object> enumerator3) {
			localSystemSelectorsAndMethodsDo(enumerator3, null);
		}

		public void localSystemSelectorsAndMethodsDo(FuncNs.Func<Object, Object, Object, Object> enumerator3, HashSet<String> exclusionSet) {
			foreach (var numArgsAssoc in hostSystemMethodDictionary) {
				var arity = numArgsAssoc.Key;
				foreach (var selectorAssoc in numArgsAssoc.Value) { 
					var selector = selectorAssoc.Key;
					if (exclusionSet == null) {
						enumerator3(selector, arity, selectorAssoc.Value);
					} else if (!exclusionSet.Contains(selector)) {
						exclusionSet.Add(selector);
						enumerator3(selector, arity, selectorAssoc.Value);
					}
				}
			}
		}
		
		public long localSystemSelectorCount() {
			long count = 0;
			foreach (var hostSysMethodDict in hostSystemMethodDictionary) count += hostSysMethodDict.Value.Count;
			return count;
		}
		
		public bool localIncludesSystemSelector(String systemSelector, long numArgs) {
			IDictionary<String, ESMethod> hostMethodDict;
			if (!hostSystemMethodDictionary.TryGetValue(numArgs, out hostMethodDict)) return false;
			return hostMethodDict.ContainsKey(systemSelector);		
		}

		public ESMethod localCompiledMethodAtSystemSelector(String systemSelector, long numArgs) {
			IDictionary<String, ESMethod> hostSysMethodDict;
			if (hostSystemMethodDictionary.TryGetValue(numArgs, out hostSysMethodDict)) {
				ESMethod method;
				if (hostSysMethodDict.TryGetValue(systemSelector, out method)) {
					return method;
				}
			}
			return null;
		}
	
		#endregion

		#region Methods--non-inherited only (includes methods from Traits)

		public void systemSelectorsDo(FuncNs.Func<Object, Object, Object> enumerator2) {
			systemSelectorsDo(enumerator2, newHostSystemSelectorSet());	
		}
		
		public void systemSelectorsDo(FuncNs.Func<Object, Object, Object> enumerator2, HashSet<String> exclusionSet) {
			localSystemSelectorsDo(enumerator2, exclusionSet);	
			if (traitUsage != null) traitUsage.systemSelectorsDo(enumerator2, exclusionSet);
		}

		public void systemSelectorsAndMethodsDo(FuncNs.Func<Object, Object, Object, Object> enumerator3) {
			systemSelectorsAndMethodsDo(enumerator3, newHostSystemSelectorSet());
		}

		public void systemSelectorsAndMethodsDo(FuncNs.Func<Object, Object, Object, Object> enumerator3, HashSet<String> exclusionSet) {
			localSystemSelectorsAndMethodsDo(enumerator3, exclusionSet);	
			if (traitUsage != null) traitUsage.systemSelectorsAndMethodsDo(enumerator3, exclusionSet);
		}
		
		public long systemSelectorCount() {
			return localSystemSelectorCount();
		}
		
		public bool includesSystemSelector(String systemSelector, long numArgs) {
			return localIncludesSystemSelector(systemSelector, numArgs) 
				|| (traitUsage != null && traitUsage.includesSystemSelector(systemSelector, numArgs));		
		}

		#endregion

		#region All methods--from whatever source derived

		public virtual ESMethod compiledMethodAtSystemSelector(String systemSelector, long numArgs) {
			var method = localCompiledMethodAtSystemSelector(systemSelector, numArgs);
			if (method != null) return method;
			return traitUsage == null ? null : traitUsage.compiledMethodAtSystemSelector(systemSelector, numArgs);
		}
		
		public virtual bool canUnderstandSystemMessage(String systemSelector, long numArgs) {
			return includesSystemSelector(systemSelector, numArgs);	
		}

		#endregion

		#region Adding/Removing methods
		
		public ESMethod bindMethodToSystemSelector(ESSymbol essenceSelector, String systemSelector) {
			if (essenceSelector == null) {
				objectSpace.throwInvalidArgumentException(Class, "bindMethod:toSystemSelector:", "essenceSelector", essenceSelector);
				return null;
			}
			if (systemSelector == null) {
				objectSpace.throwInvalidArgumentException(Class, "bindMethod:toSystemSelector:", "systemSelector", systemSelector);
				return null;
			}
			systemSelector = String.Intern(systemSelector);
			ESMethod mappedMethod;
			if (methodDictionary.TryGetValue(essenceSelector, out mappedMethod)) return addMethodBoundToSystemSelector(mappedMethod, systemSelector);
			mappedMethod = objectSpace.newMethodToSendDoesNotUnderstand(this, essenceSelector);
			return addMethodBoundToSystemSelector(mappedMethod, systemSelector);
		}
		
		public bool unbindMethodFromSystemSelector(String systemSelector, long numArgs) {			
			if (systemSelector == null) return false;
			IDictionary<String, ESMethod> hostSysMethodDict;
			if (!hostSystemMethodDictionary.TryGetValue(numArgs, out hostSysMethodDict)) return false;
			if (hostSysMethodDict.Remove(systemSelector)) {
				changedBehavior();
				return true;
			}
			return false;		
		}

		#endregion
		
		#endregion

		#region Compiling Methods

		public ESMethod compileMethod(ESSymbol protocol, TextReader sourceStream) {
			ESMethod method;
			if (objectSpace.compileMethod(sourceStream, this, protocol, out method)) {
				addMethod(method);
				return method;
			} else {
				return null;
			}
		}

		public void recompile() {
			foreach (var kvp in methodDictionary) kvp.Value.recompile();
		}

		public virtual void recompileAll() {
			recompile();
		}

		#endregion

		#region TraitUser Protocol

		public bool UsesTraits {
			get {return traitUsage != null;}
		}

		public TraitUsageExpression TraitUsage {
			get {return traitUsage;}
		}

		public TraitUser uses(TraitUsageExpression newTraitUsage) {
			if (newTraitUsage != null) newTraitUsage = newTraitUsage.Reduced;
			if (traitUsage == newTraitUsage) return this;
			if (traitUsage != null) unbindFromTraitUsage();
			traitUsage = newTraitUsage;
			if (traitUsage != null) bindToTraitUsage();
			changedBehavior();
			return this;
		}

		protected virtual void unbindFromTraitUsage() {
			traitUsage.removeUser(this);
		}

		protected virtual void bindToTraitUsage() {
			traitUsage.addUser(this);
		}

		public virtual void invalidateUsedTraits(TraitUsageExpression source) {
			if (traitUsage != null) {
				traitUsage.invalidateUsedTraits(source);
				changedBehavior();
			}
		}

		public void usedTraitsDo(Action<TraitUsageExpression> enumerator1) {
			if (traitUsage == null) return;
			if (traitUsage is Trait) {
				enumerator1(traitUsage);
			} else { 
				traitUsage.usedTraitsDo(enumerator1);
			}
		}

		public void withUsedTraitUnimplementedMessagesSentToSelfDo(Action<ESSymbol> enumerator1) {
			if (traitUsage == null) return;
			traitUsage.withUnimplementedMessagesSentToSelfDo(enumerator1);
		}

		public void usedTraitMethodConflictsDo(Action<ESSymbol, ESMethod> enumerator2) {
			if (traitUsage == null) return;
			traitUsage.methodConflictsDo(enumerator2);
		}

		public void usedTraitHostSystemMethodConflictsDo(Action<long, String, ESMethod> enumerator3) {
			if (traitUsage == null) return;
			traitUsage.hostSystemMethodConflictsDo(enumerator3);
		}

		public void usedTraitExcludedSelectorsDo(Action<ESSymbol> enumerator1) {
			if (traitUsage == null) return;
			traitUsage.excludedSelectorsDo(enumerator1);
		}

		public void usedTraitAliasedSelectorsDo(Action<ESSymbol, ESSymbol> enumerator2) {
			if (traitUsage == null) return;
			traitUsage.aliasedSelectorsDo(enumerator2);
		}

		#endregion

		#region Inheritance hierarchy

 		public virtual bool HasSuperclass {
			get {return false;}
		}
		
		public virtual ESBehavior Superclass {
			get {return null;}
		}

		public virtual bool canInheritFrom(ESBehavioralObject aSuperclass) {
			return false;
		}

		public virtual bool includesBehavior(ESBehavioralObject aBehavior) {
			return this == aBehavior;
		}
		
		public virtual bool inheritsFrom(ESBehavioralObject aBehavior) {
			return false;
		}
		
		#endregion

		#region Named instance variables

		public virtual ESSymbol[] InstanceVariableNames {
			get {return emptyInstanceVariableNames;}
		}

		protected virtual void invalidateInstanceVariableNames() {
			recompile();
			base.incrementVersion();
		}

		public virtual ESSymbol localInstVarNameAt(long index) {
			return null;
		}
		
		public virtual ESSymbol instVarNameAt(long index) {
			if (index < 0) return null;
			long superInstSize = SuperInstSize;
			if (index < superInstSize) return superInstSize > 0 ? Superclass.instVarNameAt(index) : null;
			return null;			
		}
		
		protected virtual Dictionary<ESSymbol, long> InstanceVariableIndexes {
			get {return emptyInstanceVariableIndexes;}
		}
		
		public virtual long instVarIndexFor(ESSymbol instanceVariableName) {
			return HasSuperclass ? Superclass.instVarIndexFor(instanceVariableName) : -1;
		}
		
		public virtual long BasicInstSize {
			get {return 0;}
		}
		
		public long SuperInstSize {
			get {return HasSuperclass ? Superclass.InstSize : 0;}
		}
		
		public long InstSize {
			get {return SuperInstSize + BasicInstSize;}
		}
		
		public virtual void allInstVarNamesAndIndexesDo(System.Action<ESSymbol, long> enumerator2) {
			if (HasSuperclass) Superclass.allInstVarNamesAndIndexesDo(enumerator2);
			
		}

		#endregion

		public new abstract class Primitives : PrimitiveDomain {

			#region Primitive Definitions

			public Object _objectSpace_(Object receiver) {
				return ((ESBehavioralObject)receiver).ObjectSpace;
			}

			public Object _localSelectorsDo_(Object receiver, Object enumerator1) {
				((ESBehavioralObject)receiver).localSelectorsDo(asFunctor1(enumerator1));
				return receiver;
			}

			public Object _localSelectorsAndMethodsDo_(Object receiver, Object enumerator2) {
				((ESBehavioralObject)receiver).localSelectorsAndMethodsDo(asFunctor2(enumerator2));
				return receiver;
			}

			public Object _localSelectorCount_(Object receiver) {
				var theReceiver = (ESBehavioralObject)receiver;
				return theReceiver.localSelectorCount();
			}

			public Object _localSelectors_(Object receiver) {
				return ((ESBehavioralObject)receiver).localSelectors();
			}

			public Object _localIncludesSelector_(Object receiver, Object selector) {
				return ((ESBehavioralObject)receiver).localIncludesSelector(objectSpace.asESSymbol(selector));
			}

			public Object _localCompiledMethodAt_(Object receiver, Object selector) {
				return ((ESBehavioralObject)receiver).localCompiledMethodAt(objectSpace.asESSymbol(selector));
			}

			public Object _selectorsDo_(Object receiver, Object enumerator1) {
				((ESBehavioralObject)receiver).selectorsDo(asFunctor1(enumerator1));
				return receiver;
			}

			public Object _selectorsAndMethodsDo_(Object receiver, Object enumerator2) {
				((ESBehavioralObject)receiver).selectorsAndMethodsDo(asFunctor2(enumerator2));
				return receiver;
			}

			public Object _selectorCount_(Object receiver) {
				var theReceiver = (ESBehavioralObject)receiver;
				return theReceiver.selectorCount();
			}

			public Object _selectors_(Object receiver) {
				return ((ESBehavioralObject)receiver).selectors();
			}

			public Object _includesSelector_(Object receiver, Object selector) {
				return ((ESBehavioralObject)receiver).includesSelector(objectSpace.asESSymbol(selector));
			}

			public Object _allSelectorsDo_(Object receiver, Object enumerator1) {
				((ESBehavioralObject)receiver).allSelectorsDo(asFunctor1(enumerator1));
				return receiver;
			}

			public Object _allSelectorsAndMethodsDo_(Object receiver, Object enumerator2) {
				((ESBehavioralObject)receiver).allSelectorsAndMethodsDo(asFunctor2(enumerator2));
				return receiver;
			}

			public Object _allSelectors_(Object receiver) {
				return ((ESBehavioralObject)receiver).allSelectors();
			}

			public Object _canUnderstand_(Object receiver, Object selector) {
				return ((ESBehavioralObject)receiver).canUnderstand(objectSpace.asESSymbol(selector));
			}

			public Object _compiledMethodAt_(Object receiver, Object selector) {
				return ((ESBehavioralObject)receiver).compiledMethodAt(objectSpace.asESSymbol(selector));
			}

			public Object _withUnimplementedMessagesSentToSelfDo_(Object receiver, Object enumerator1) {
				var theReceiver = (ESBehavioralObject)receiver;
				var functor1 = asFunctor1(enumerator1);
				theReceiver.withUnimplementedMessagesSentToSelfDo(selector => functor1(selector));
				return receiver;
			}


			public Object _localSystemSelectorsDo_(Object receiver, Object enumerator2) {
				((ESBehavioralObject)receiver).localSystemSelectorsDo(asFunctor2(enumerator2));
				return receiver;
			}

			public Object _localSystemSelectorsAndMethodsDo_(Object receiver, Object enumerator3) {
				((ESBehavioralObject)receiver).localSystemSelectorsAndMethodsDo(asFunctor3(enumerator3));
				return receiver;
			}

			public Object _localSystemSelectorCount_(Object receiver) {
				return ((ESBehavioralObject)receiver).localSystemSelectorCount();
			}

			public Object _localIncludesSystemSelector_(Object receiver, Object systemSelector, Object numArgs) {
				return ((ESBehavioralObject)receiver).localIncludesSystemSelector(asHostString(systemSelector), asHostLong(numArgs));
			}

			public Object _localCompiledMethodAtSystemSelector_(Object receiver, Object systemSelector, Object numArgs) {
				return ((ESBehavioralObject)receiver).localCompiledMethodAtSystemSelector(asHostString(systemSelector), asHostLong(numArgs));
			}


			public Object _systemSelectorsDo_(Object receiver, Object enumerator2) {
				((ESBehavioralObject)receiver).systemSelectorsDo(asFunctor2(enumerator2));
				return receiver;
			}

			public Object _systemSelectorsAndMethodsDo_(Object receiver, Object enumerator3) {
				((ESBehavioralObject)receiver).systemSelectorsAndMethodsDo(asFunctor3(enumerator3));
				return receiver;
			}

			public Object _systemSelectorCount_(Object receiver) {
				return ((ESBehavioralObject)receiver).systemSelectorCount();
			}

			public Object _includesSystemSelector_(Object receiver, Object systemSelector, Object numArgs) {
				return ((ESBehavioralObject)receiver).includesSystemSelector(asHostString(systemSelector), asHostLong(numArgs));
			}


			public Object _canUnderstandSystemMessage_(Object receiver, Object selector, Object numArgs) {
				var arity = (long)numArgs;
				return ((ESBehavioralObject)receiver).canUnderstandSystemMessage(objectSpace.asESSymbol(selector), arity);
			}

			public Object _compiledMethodAtSystemSelector_(Object receiver, Object systemSelector, Object numArgs) {
				return ((ESBehavioralObject)receiver).compiledMethodAtSystemSelector(asHostString(systemSelector), asHostLong(numArgs));
			}


			public Object _addMethod_(Object receiver, Object method) {
				return ((ESBehavioralObject)receiver).addMethod((ESMethod)method);
			}	

			public Object _protocolMethod_(Object receiver, Object protocol, Object method) {
				var compiledMethod = (ESMethod)method;
				compiledMethod.addToProtocol(objectSpace.asESSymbol(protocol));
				return ((ESBehavioralObject)receiver).addMethod(compiledMethod);
			}	

			public Object _removeSelector_(Object receiver, Object selector) {
				return ((ESBehavioralObject)receiver).removeSelector(objectSpace.asESSymbol(selector));
			}

			public Object _addMethodBoundToSystemSelector_(Object receiver, Object method, Object systemSelector) {
				return ((ESBehavioralObject)receiver).addMethodBoundToSystemSelector((ESMethod)method, asHostString(systemSelector));
			}	

			public Object _bindMethodAtToSystemSelector_(Object receiver, Object essenceSelector, Object systemSelector) {
				return ((ESBehavioralObject)receiver).bindMethodToSystemSelector(objectSpace.asESSymbol(essenceSelector), asHostString(systemSelector));
			}

			public Object _unbindMethodFromSystemSelector_(Object receiver, Object systemSelector, Object numArgs) {
				return ((ESBehavioralObject)receiver).unbindMethodFromSystemSelector(asHostString(systemSelector), asHostLong(numArgs));
			}

			public Object _protocolSystemSelectorMethod_(Object receiver, Object protocol, Object systemSelector, Object method) {
				var compiledMethod = (ESMethod)method;
				compiledMethod.addToProtocol(objectSpace.asESSymbol(protocol));
				return ((ESBehavioralObject)receiver).addMethodBoundToSystemSelector(compiledMethod, asHostString(systemSelector));
			}	

			public Object _compileMethodFromString_(Object receiver, Object protocol, Object methodText) {
				var methodString = asHostString(methodText);
				return ((ESBehavioralObject)receiver).compileMethod(objectSpace.asESSymbol(protocol), new StringReader(methodString));
			}


			public Object _usesTraits_(Object receiver) {
				return ((ESBehavioralObject)receiver).UsesTraits;
			}

			public Object _traitUsage_(Object receiver) {
				return ((ESBehavioralObject)receiver).TraitUsage;
			}

			public Object _uses_(Object receiver, Object traitUsageExpression) {
				return ((ESBehavioralObject)receiver).uses((TraitUsageExpression)traitUsageExpression);
			}

			public Object _invalidateUsedTraits_(Object receiver, Object source) {
				((ESBehavioralObject)receiver).invalidateUsedTraits((TraitUsageExpression)source);
				return receiver;
			}

			public Object _usedTraitsDo_(Object receiver, Object enumerator1) {
				var functor1 = asFunctor1(enumerator1);
				((ESBehavioralObject)receiver).usedTraitsDo(trait => functor1(trait));
				return receiver;
			}

			public Object _withUsedTraitUnimplementedMessagesSentToSelfDo_(Object receiver, Object enumerator1) {
				var functor1 = asFunctor1(enumerator1);
				((ESBehavioralObject)receiver).withUsedTraitUnimplementedMessagesSentToSelfDo(trait => functor1(trait));
				return receiver;
			}

			public Object _usedTraitMethodConflictsDo_(Object receiver, Object enumerator2) {
				var functor2 = asFunctor2(enumerator2);
				((ESBehavioralObject)receiver).usedTraitMethodConflictsDo((selector, method) => functor2(selector, method));
				return receiver;
			}

			public Object _usedTraitHostSystemMethodConflictsDo_(Object receiver, Object enumerator3) {
				var functor3 = asFunctor3(enumerator3);
				((ESBehavioralObject)receiver).usedTraitHostSystemMethodConflictsDo((arity, selector, method) => functor3(arity, selector, method));
				return receiver;
			}

			public Object _usedTraitExcludedSelectorsDo_(Object receiver, Object enumerator1) {
				var functor1 = asFunctor1(enumerator1);
				((ESBehavioralObject)receiver).usedTraitExcludedSelectorsDo(selector => functor1(selector));
				return receiver;
			}

			public Object _usedTraitAliasedSelectorsDo_(Object receiver, Object enumerator2) {
				var functor2 = asFunctor2(enumerator2);
				((ESBehavioralObject)receiver).usedTraitAliasedSelectorsDo((originalSelector, selectorAlias) => functor2(originalSelector, selectorAlias));
				return receiver;
			}


			public Object _superclass_(Object receiver) {
				return ((ESBehavioralObject)receiver).Superclass;
			}

			public Object _includesBehavior_(Object receiver, Object aBehavior) {
				return ((ESBehavioralObject)receiver).includesBehavior((ESBehavioralObject)aBehavior);
			}

			public Object _inheritsFrom_(Object receiver, Object aBehavior) {
				return ((ESBehavioralObject)receiver).inheritsFrom((ESBehavioralObject)aBehavior);
			}
		
			public Object _instanceVariableNames_(Object receiver) {
				return objectSpace.newArray(Array.ConvertAll<ESSymbol, Object>(((ESBehavioralObject)receiver).InstanceVariableNames, symbol => symbol));
			}
		
			public Object _localInstVarNameAt_(Object receiver, Object index) {
				return ((ESBehavioralObject)receiver).localInstVarNameAt(asHostLong(index) - 1);
			}
		
			public Object _instVarNameAt_(Object receiver, Object index) {
				return ((ESBehavioralObject)receiver).instVarNameAt(asHostLong(index) - 1);
			}

			public Object _instVarIndexFor_(Object receiver, Object instanceVariableName) {
				return ((ESBehavioralObject)receiver).instVarIndexFor(objectSpace.asESSymbol(instanceVariableName)) + 1;
			}
		
			public Object _basicInstSize_(Object receiver) {
				return ((ESBehavioralObject)receiver).BasicInstSize;
			}
		
			public Object _instSize_(Object receiver) {
				return ((ESBehavioralObject)receiver).InstSize;
			}

			public Object _allInstVarNamesAndIndexesDo_(Object receiver, Object enumerator2) {
				var functor2 = asFunctor2(enumerator2);
				((ESBehavioralObject)receiver).allInstVarNamesAndIndexesDo((instVarName, index) => functor2(instVarName, index));
				return receiver;
			}

			#endregion

			public override void publishCanonicalPrimitives() {

				publishPrimitive("objectSpace",						new FuncNs.Func<Object, Object>(_objectSpace_));

				publishPrimitive("localSelectorsDo:",					new FuncNs.Func<Object, Object, Object>(_localSelectorsDo_));
				publishPrimitive("localSelectorsAndMethodsDo:",				new FuncNs.Func<Object, Object, Object>(_localSelectorsAndMethodsDo_));
				publishPrimitive("localSelectorCount",					new FuncNs.Func<Object, Object>(_localSelectorCount_));
				publishPrimitive("localSelectors",					new FuncNs.Func<Object, Object>(_localSelectors_));
				publishPrimitive("localIncludesSelector:",				new FuncNs.Func<Object, Object, Object>(_localIncludesSelector_));
				publishPrimitive("localCompiledMethodAt:",				new FuncNs.Func<Object, Object, Object>(_localCompiledMethodAt_));

				publishPrimitive("selectorsDo:",					new FuncNs.Func<Object, Object, Object>(_selectorsDo_));
				publishPrimitive("selectorsAndMethodsDo:",				new FuncNs.Func<Object, Object, Object>(_selectorsAndMethodsDo_));
				publishPrimitive("selectorCount",					new FuncNs.Func<Object, Object>(_selectorCount_));
				publishPrimitive("selectors",						new FuncNs.Func<Object, Object>(_selectors_));
				publishPrimitive("includesSelector:",					new FuncNs.Func<Object, Object, Object>(_includesSelector_));

				publishPrimitive("allSelectorsDo:",					new FuncNs.Func<Object, Object, Object>(_allSelectorsDo_));
				publishPrimitive("allSelectorsAndMethodsDo:",				new FuncNs.Func<Object, Object, Object>(_allSelectorsAndMethodsDo_));
				publishPrimitive("allSelectors",					new FuncNs.Func<Object, Object>(_allSelectors_));
				publishPrimitive("canUnderstand:",					new FuncNs.Func<Object, Object, Object>(_canUnderstand_));
				publishPrimitive("compiledMethodAt:",					new FuncNs.Func<Object, Object, Object>(_compiledMethodAt_));

				publishPrimitive("addMethod:",						new FuncNs.Func<Object, Object, Object>(_addMethod_));
				publishPrimitive("protocol:method:",					new FuncNs.Func<Object, Object, Object, Object>(_protocolMethod_));
				publishPrimitive("removeSelector:",					new FuncNs.Func<Object, Object, Object>(_removeSelector_));

				publishPrimitive("localSystemSelectorsDo:",				new FuncNs.Func<Object, Object, Object>(_localSystemSelectorsDo_));
				publishPrimitive("localSystemSelectorsAndMethodsDo:",			new FuncNs.Func<Object, Object, Object>(_localSystemSelectorsAndMethodsDo_));
				publishPrimitive("localSystemSelectorCount",				new FuncNs.Func<Object, Object>(_localSystemSelectorCount_));
				publishPrimitive("includesSystemSelector:numArgs:",			new FuncNs.Func<Object, Object, Object, Object>(_includesSystemSelector_));

				publishPrimitive("systemSelectorsDo:",					new FuncNs.Func<Object, Object, Object>(_systemSelectorsDo_));
				publishPrimitive("systemSelectorsAndMethodsDo:",			new FuncNs.Func<Object, Object, Object>(_systemSelectorsAndMethodsDo_));
				publishPrimitive("systemSelectorCount",					new FuncNs.Func<Object, Object>(_systemSelectorCount_));
				publishPrimitive("includesSystemSelector:numArgs:",			new FuncNs.Func<Object, Object, Object, Object>(_includesSystemSelector_));

				publishPrimitive("canUnderstandSystemMessage:",				new FuncNs.Func<Object, Object, Object, Object>(_canUnderstandSystemMessage_));
				publishPrimitive("compiledMethodAtSystemSelector:numArgs:",		new FuncNs.Func<Object, Object, Object, Object>(_compiledMethodAtSystemSelector_));

				publishPrimitive("withUnimplementedMessagesSentToSelfDo:",		new FuncNs.Func<Object, Object, Object>(_withUnimplementedMessagesSentToSelfDo_));

				publishPrimitive("addMethod:systemSelector:",				new FuncNs.Func<Object, Object, Object, Object>(_addMethodBoundToSystemSelector_));
				publishPrimitive("protocol:systemSelector:method:",			new FuncNs.Func<Object, Object, Object, Object, Object>(_protocolSystemSelectorMethod_));
				publishPrimitive("bindMethodAt:toSystemSelector:",			new FuncNs.Func<Object, Object, Object, Object>(_bindMethodAtToSystemSelector_));
				publishPrimitive("unbindindMethodFromSystemSelector:numArgs:",		new FuncNs.Func<Object, Object, Object, Object>(_unbindMethodFromSystemSelector_));

				publishPrimitive("compileMethodInProtocol:fromString:",			new FuncNs.Func<Object, Object, Object, Object>(_compileMethodFromString_));

				publishPrimitive("usesTraits",						new FuncNs.Func<Object, Object>(_usesTraits_));
				publishPrimitive("traitUsage",						new FuncNs.Func<Object, Object>(_traitUsage_));
				publishPrimitive("uses:",						new FuncNs.Func<Object, Object, Object>(_uses_));

				publishPrimitive("invalidateUsedTraitsFrom:",				new FuncNs.Func<Object, Object, Object>(_invalidateUsedTraits_));
				publishPrimitive("usedTraitsDo:",					new FuncNs.Func<Object, Object, Object>(_usedTraitsDo_));
				publishPrimitive("withUsedTraitUnimplementedMessagesSentToSelfDo:",	new FuncNs.Func<Object, Object, Object>(_withUsedTraitUnimplementedMessagesSentToSelfDo_));
				publishPrimitive("usedTraitMethodConflictsDo:",				new FuncNs.Func<Object, Object, Object>(_usedTraitMethodConflictsDo_));
				publishPrimitive("usedTraitHostSystemMethodConflictsDo:",		new FuncNs.Func<Object, Object, Object>(_usedTraitHostSystemMethodConflictsDo_));
				publishPrimitive("usedTraitExcludedSelectorsDo:",			new FuncNs.Func<Object, Object, Object>(_usedTraitExcludedSelectorsDo_));
				publishPrimitive("usedTraitAliasedSelectorsDo:",			new FuncNs.Func<Object, Object, Object>(_usedTraitAliasedSelectorsDo_));

				publishPrimitive("superclass",						new FuncNs.Func<Object, Object>(_superclass_));
				publishPrimitive("includesBehavior:",					new FuncNs.Func<Object, Object, Object>(_includesBehavior_));
				publishPrimitive("inheritsFrom:",					new FuncNs.Func<Object, Object, Object>(_inheritsFrom_));

				publishPrimitive("instanceVariableNames",				new FuncNs.Func<Object, Object>(_instanceVariableNames_));
				publishPrimitive("localInstVarNameAt:",					new FuncNs.Func<Object, Object, Object>(_localInstVarNameAt_));
				publishPrimitive("instVarNameAt:",					new FuncNs.Func<Object, Object, Object>(_instVarNameAt_));
				publishPrimitive("instVarIndexFor:",					new FuncNs.Func<Object, Object, Object>(_instVarIndexFor_));
				publishPrimitive("basicInstSize",					new FuncNs.Func<Object, Object>(_basicInstSize_));
				publishPrimitive("instSize",						new FuncNs.Func<Object, Object>(_instSize_));

				publishPrimitive("allInstVarNamesAndIndexesDo:",			new FuncNs.Func<Object, Object, Object>(_allInstVarNamesAndIndexesDo_));

			}

		}

	}

	#region Behaviors (BehavioralObjects that can have superclasses, can have instances, and can define the instance variables of their instances)

	public class ESBehavior : ESBehavioralObject {

		#region Static variables and methods

		#region Instance Architecture Queries

		public static bool instancesArchitectureForbidsNamedSlots(ObjectStateArchitecture instanceArchitecture) {
			switch (instanceArchitecture) {
				case ObjectStateArchitecture.Stateless:
				case ObjectStateArchitecture.Association:
				case ObjectStateArchitecture.BindingReference:
				case ObjectStateArchitecture.Message:
				case ObjectStateArchitecture.Block:
				case ObjectStateArchitecture.Method:
				case ObjectStateArchitecture.HostSystemObject:
					return true;
				default:
					return false;
			}
		}

		public static bool isValidInheritanceRelationship(ObjectStateArchitecture inheritingInstanceArchitecture, ObjectStateArchitecture superclassInstanceArchitecture) {
			switch (inheritingInstanceArchitecture) {

				case ObjectStateArchitecture.Abstract:
					return true;

				case ObjectStateArchitecture.Stateless:
					switch (superclassInstanceArchitecture) {
						case ObjectStateArchitecture.Abstract:
						case ObjectStateArchitecture.Stateless:
							return true;
						default:
							return false;
					}

				case ObjectStateArchitecture.NamedSlots:
					switch (superclassInstanceArchitecture) {
						case ObjectStateArchitecture.Abstract:
						case ObjectStateArchitecture.Stateless:
						case ObjectStateArchitecture.NamedSlots:
							return true;
						default:
							return false;
					}

				case ObjectStateArchitecture.IdentityDictionary:
				case ObjectStateArchitecture.Dictionary:
				case ObjectStateArchitecture.Namespace:
				case ObjectStateArchitecture.TraitTransformation:
				case ObjectStateArchitecture.TraitComposition:
				case ObjectStateArchitecture.IndexedObjectSlots:
				case ObjectStateArchitecture.IndexedByteSlots:
				case ObjectStateArchitecture.IndexedCharSlots:
				case ObjectStateArchitecture.IndexedHalfWordSlots:
				case ObjectStateArchitecture.IndexedWordSlots:
				case ObjectStateArchitecture.IndexedLongWordSlots:
				case ObjectStateArchitecture.IndexedSinglePrecisionSlots:
				case ObjectStateArchitecture.IndexedDoublePrecisionSlots:
				case ObjectStateArchitecture.IndexedQuadPrecisionSlots:
				case ObjectStateArchitecture.Symbol:
				case ObjectStateArchitecture.Pathname:
					switch (superclassInstanceArchitecture) {
						case ObjectStateArchitecture.Abstract:
						case ObjectStateArchitecture.Stateless:
						case ObjectStateArchitecture.NamedSlots:
							return true;
						default:
							return inheritingInstanceArchitecture == superclassInstanceArchitecture;
					}

				case ObjectStateArchitecture.BehavioralTrait:
				case ObjectStateArchitecture.InstanceTrait:
				case ObjectStateArchitecture.ClassTrait:
					switch (superclassInstanceArchitecture) {
						case ObjectStateArchitecture.Abstract:
						case ObjectStateArchitecture.Stateless:
						case ObjectStateArchitecture.NamedSlots:
						case ObjectStateArchitecture.Namespace:
						case ObjectStateArchitecture.BehavioralTrait:
							return true;
						default:
							return inheritingInstanceArchitecture == superclassInstanceArchitecture;
					}

				case ObjectStateArchitecture.Behavior:
				case ObjectStateArchitecture.Class:
				case ObjectStateArchitecture.Metaclass:
					switch (superclassInstanceArchitecture) {
						case ObjectStateArchitecture.Abstract:
						case ObjectStateArchitecture.Stateless:
						case ObjectStateArchitecture.NamedSlots:
						case ObjectStateArchitecture.Namespace:
						case ObjectStateArchitecture.Behavior:
							return true;
						default:
							return inheritingInstanceArchitecture == superclassInstanceArchitecture;
					}

				case ObjectStateArchitecture.Association:
				case ObjectStateArchitecture.BindingReference:
				case ObjectStateArchitecture.Message:
				case ObjectStateArchitecture.Block:
				case ObjectStateArchitecture.Method:
					switch (superclassInstanceArchitecture) {
						case ObjectStateArchitecture.Abstract:
						case ObjectStateArchitecture.Stateless:
							return true;
						default:
							return inheritingInstanceArchitecture == superclassInstanceArchitecture;
					}

				case ObjectStateArchitecture.Nil:
				case ObjectStateArchitecture.False:
				case ObjectStateArchitecture.True:
				case ObjectStateArchitecture.Char:
				case ObjectStateArchitecture.SmallInteger:
				case ObjectStateArchitecture.LargeInteger:
				case ObjectStateArchitecture.SinglePrecision:
				case ObjectStateArchitecture.DoublePrecision:
				case ObjectStateArchitecture.QuadPrecision:
					switch (superclassInstanceArchitecture) {
						case ObjectStateArchitecture.Abstract:
						case ObjectStateArchitecture.HostSystemObject:
							return true;
						default:
							return inheritingInstanceArchitecture == superclassInstanceArchitecture;
					}

				case ObjectStateArchitecture.HostSystemObject:
					switch (superclassInstanceArchitecture) {
						case ObjectStateArchitecture.Abstract:
						case ObjectStateArchitecture.HostSystemObject:
							return true;
						default:
							return false;
					}

				default:
					return false;

			}
		}

		public static bool isClassOfAdoptedType(ObjectStateArchitecture instanceArchitecture) {
			switch (instanceArchitecture) {

				case ObjectStateArchitecture.Nil:
				case ObjectStateArchitecture.False:
				case ObjectStateArchitecture.True:
				case ObjectStateArchitecture.Char:
				case ObjectStateArchitecture.SmallInteger:
				case ObjectStateArchitecture.LargeInteger:
				case ObjectStateArchitecture.SinglePrecision:
				case ObjectStateArchitecture.DoublePrecision:
				case ObjectStateArchitecture.QuadPrecision:
					return true;

				default:
					return false;

			}
		}

		public static bool isClassOfHostSystemType(ObjectStateArchitecture instanceArchitecture) {
			return instanceArchitecture == ObjectStateArchitecture.HostSystemObject || isClassOfAdoptedType(instanceArchitecture);
		}

		#endregion

		#region Host System Reflection

		public static readonly BindingFlags						instanceCreationBindingFlags		= BindingFlags.Instance | BindingFlags.CreateInstance | BindingFlags.Public | BindingFlags.NonPublic;
		public static readonly BindingFlags						instanceMethodInvokeBindingFlags	= BindingFlags.Instance | BindingFlags.InvokeMethod | BindingFlags.Public | BindingFlags.NonPublic;
		public static readonly BindingFlags						instanceFieldGetBindingFlags		= BindingFlags.Instance | BindingFlags.GetField | BindingFlags.Public | BindingFlags.NonPublic;
		public static readonly BindingFlags						instanceFieldSetBindingFlags		= BindingFlags.Instance | BindingFlags.SetField | BindingFlags.Public | BindingFlags.NonPublic;
		public static readonly BindingFlags						instancePropertyGetBindingFlags		= BindingFlags.Instance | BindingFlags.GetProperty | BindingFlags.Public | BindingFlags.NonPublic;
		public static readonly BindingFlags						instancePropertySetBindingFlags		= BindingFlags.Instance | BindingFlags.SetProperty | BindingFlags.Public | BindingFlags.NonPublic;

		public static readonly BindingFlags						staticMethodInvokeBindingFlags		= BindingFlags.Static | BindingFlags.InvokeMethod | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.FlattenHierarchy;
		public static readonly BindingFlags						staticFieldGetBindingFlags		= BindingFlags.Static | BindingFlags.GetField | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.FlattenHierarchy;
		public static readonly BindingFlags						staticFieldSetBindingFlags		= BindingFlags.Static | BindingFlags.SetField | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.FlattenHierarchy;
		public static readonly BindingFlags						staticPropertyGetBindingFlags		= BindingFlags.Static | BindingFlags.GetProperty | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.FlattenHierarchy;
		public static readonly BindingFlags						staticPropertySetBindingFlags		= BindingFlags.Static | BindingFlags.SetProperty | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.FlattenHierarchy;

		public static Type typeFromAssemblyQualifiedName(String assemblyQualifiedTypeName, bool raiseExceptionOnErrorOrNotFound) {
			var typeName = TypeName.fromString(assemblyQualifiedTypeName);
			return typeName.getType(raiseExceptionOnErrorOrNotFound);
		}

		public static ConstructorInfo getHostConstructor(Type instanceType, Type[] signature) {
			return instanceType.GetConstructor(
					instanceCreationBindingFlags, 
					Type.DefaultBinder, 
					signature, 
					null);
		}

		public static MethodInfo getHostInstanceMethod(Type receiverType, String methodName) {
			return receiverType.GetMethod(methodName, instanceMethodInvokeBindingFlags, Type.DefaultBinder, TypeGuru.emptyTypeArray, null);
		}

		public static MethodInfo getHostInstanceMethod(Type receiverType, String methodName, Type[] signature) {
			return receiverType.GetMethod(methodName, instanceMethodInvokeBindingFlags, Type.DefaultBinder, signature, null);
		}

		public static MethodInfo getHostClassMethod(Type receiverType, String methodName) {
			return receiverType.GetMethod(methodName, staticMethodInvokeBindingFlags, Type.DefaultBinder, TypeGuru.emptyTypeArray, null);
		}

		public static MethodInfo getHostClassMethod(Type receiverType, String methodName, Type[] signature) {
			return receiverType.GetMethod(methodName, staticMethodInvokeBindingFlags, Type.DefaultBinder, signature, null);
		}

		public static Object newInstanceOf(Type hostType) {
			return hostType.InvokeMember(hostType.FullName, instanceCreationBindingFlags, null, null, emtpyObjArray);
		}

		public static Object newInstanceOf(Type hostType, Object[] args) {
			return hostType.InvokeMember(hostType.FullName, instanceCreationBindingFlags, null, null, args ?? emtpyObjArray);
		}

		public static Object sendHostMessage(Object receiver, String messageName, BindingFlags bindingFlags) {
			Type receiverType = receiver.GetType();
			return receiverType.InvokeMember(messageName, bindingFlags | BindingFlags.InvokeMethod, Type.DefaultBinder, receiver, emtpyObjArray);
		}

		public static Object sendHostClassMessage(Type classType, String messageName, BindingFlags bindingFlags) {
			return classType.InvokeMember(messageName, (bindingFlags ^ BindingFlags.Instance) | BindingFlags.InvokeMethod | BindingFlags.Static, Type.DefaultBinder, null, emtpyObjArray);
		}

		public static Object sendHostMessage(Object receiver, String messageName, Object[] args, BindingFlags bindingFlags) {
			Type receiverType = receiver.GetType();
			return receiverType.InvokeMember(messageName, bindingFlags | BindingFlags.InvokeMethod, Type.DefaultBinder, receiver, args ?? emtpyObjArray);
		}

		public static Object sendHostClassMessage(Type classType, String messageName, Object[] args, BindingFlags bindingFlags) {
			return classType.InvokeMember(messageName, (bindingFlags ^ BindingFlags.Instance) | BindingFlags.InvokeMethod | BindingFlags.Static, Type.DefaultBinder, null, args ?? emtpyObjArray);
		}

		public static Object valueOfHostObjectField(Object receiver, String fieldName, BindingFlags bindingFlags) {
			Type receiverType = receiver.GetType();
			return receiverType.InvokeMember(fieldName, bindingFlags | BindingFlags.GetField, Type.DefaultBinder, receiver, emtpyObjArray);
		}

		public static Object setValueOfHostObjectField(Object receiver, String fieldName, Object newValue, BindingFlags bindingFlags) {
			Type receiverType = receiver.GetType();
			return receiverType.InvokeMember(fieldName, bindingFlags | BindingFlags.SetField, Type.DefaultBinder, receiver, new object[] {newValue});
		}

		public static Object valueOfHostObjectProperty(Object receiver, String fieldName, BindingFlags bindingFlags) {
			Type receiverType = receiver.GetType();
			return receiverType.InvokeMember(fieldName, bindingFlags | BindingFlags.GetProperty, Type.DefaultBinder, receiver, emtpyObjArray);
		}

		public static Object setValueOfHostObjectProperty(Object receiver, String fieldName, Object newValue, BindingFlags bindingFlags) {
			Type receiverType = receiver.GetType();
			return receiverType.InvokeMember(fieldName, bindingFlags | BindingFlags.SetProperty, Type.DefaultBinder, receiver, new object[] {newValue});
		}

		#endregion

		#endregion

		protected ESBehavior								superclass; 
		protected HashSet<ESBehavior>							subclasses;

		protected Type									instanceType; 
		protected bool									isInstanceTypeLocked			= false;
		protected ObjectStateArchitecture 						instanceArchitecture			= ObjectStateArchitecture.NamedSlots;
		protected bool									isInstanceArchitectureLocked		= false;

		protected ObjectEqualityComparator						instanceEqualityComparator; 
		protected FuncNs.Func<Object, Object, Object>					instanceEqualityFunctor; 
		protected FuncNs.Func<Object, Object>						instanceHashFunctor; 

		#region Constructors

		protected ESBehavior(ESBehavior metaClass) : base(metaClass) {
		}

		internal ESBehavior(ObjectStateArchitecture instanceArchitecture) : base() {
			this.instanceArchitecture = instanceArchitecture;
		}
			
		protected ESBehavior(ESBehavior metaClass, ObjectStateArchitecture instanceArchitecture) : base(metaClass) {
			this.instanceArchitecture = instanceArchitecture;
		}
			
		public ESBehavior(ESBehavior metaClass, ESObjectSpace objectSpace, ObjectStateArchitecture instanceArchitecture) : base(metaClass, objectSpace) {
			this.instanceArchitecture = instanceArchitecture;
		}

		public ESBehavior(ESBehavior metaClass, ESObjectSpace objectSpace) : base(metaClass, objectSpace) {
		}
			
		protected ESBehavior(ESBehavior metaClass, ObjectStateArchitecture instanceArchitecture, ESBehavior superclass) 
					: this(metaClass, instanceArchitecture) {
			setSuperclass(superclass);
		}
				
		public ESBehavior(ESBehavior metaClass, ESObjectSpace objectSpace, ObjectStateArchitecture instanceArchitecture, ESBehavior superclass) 
					: this(metaClass, objectSpace, instanceArchitecture) {
			setSuperclass(superclass);
		}

		#endregion

		#region Internal operations

		protected HashSet<ESBehavior> newSubclassesSet() {
			return new HashSet<ESBehavior>(new BehavorIdentityComparator());
		}

		protected override void initialize() {
			base.initialize();
			subclasses = newSubclassesSet();
		}
	
		protected override void bindToObjectSpace() {
			base.bindToObjectSpace();
			instanceEqualityComparator = objectSpace.newObjectEqualityComparator();
			instanceEqualityFunctor = instanceEqualityComparator.EqualityFunctor;
			instanceHashFunctor = instanceEqualityComparator.HashFunctor;
		}

		public override void validate() {
			base.validate();
			assertValidInheritanceStructure(Superclass);
		}

		public virtual ObjectStateArchitecture InstanceArchitecture {
			get {return instanceArchitecture;}
			set {	if (instanceArchitecture == value) return;
				if (isInstanceArchitectureLocked) throw new PrimitiveFailException("A Behavior's instance architecture cannot be changed after it has created instances.");
				if (constraintsMustBeSatisfied && !canInheritFrom(value, Superclass)) throwIncompatibleSuperclassException(value, Superclass);
				instanceArchitecture = value;
				isBoundToHostSystemNamespace = instanceArchitecture == ObjectStateArchitecture.HostSystemObject;
				invalidateInstanceType(); 
			}
		}

		public override bool InstancesCanHaveNamedSlots {
			get {
				switch (InstanceArchitecture) {
					case ObjectStateArchitecture.Stateless:
					case ObjectStateArchitecture.Association:
					case ObjectStateArchitecture.BindingReference:
					case ObjectStateArchitecture.Message:
					case ObjectStateArchitecture.Block:
					case ObjectStateArchitecture.Method:
					case ObjectStateArchitecture.HostSystemObject:
						return false;
					case ObjectStateArchitecture.Abstract:
						return HasSuperclass ? Superclass.SubclassInstancesCanHaveNamedSlots : false;
					default:
						return true;
				}
			}
		}

		public bool SubclassInstancesCanHaveNamedSlots {
			get {
				switch (InstanceArchitecture) {
					case ObjectStateArchitecture.Association:
					case ObjectStateArchitecture.BindingReference:
					case ObjectStateArchitecture.Message:
					case ObjectStateArchitecture.Block:
					case ObjectStateArchitecture.Method:
					case ObjectStateArchitecture.HostSystemObject:
						return false;
					case ObjectStateArchitecture.Abstract:
					case ObjectStateArchitecture.Stateless:
						return HasSuperclass ? Superclass.SubclassInstancesCanHaveNamedSlots : true;
					default:
						return true;
				}
			}
		}

		public IEqualityComparer<Object> InstanceEqualityComparator {
			get {return instanceEqualityComparator;}
		}

		public bool instanceHasSameValueAs(Object instance, Object comparand) {
			if (instanceEqualityFunctor == null) return ReferenceEquals(instance, comparand);
			return (bool)instanceEqualityFunctor(instance, comparand);
		}

		public int instanceHashCode(Object instance) {
			if (instanceHashFunctor == null) return RuntimeHelpers.GetHashCode(instance);
			return (int)instanceHashFunctor(instance);
		}

		public Type InstanceType {
			get {	if (instanceType == null) invalidateInstanceType();
				return instanceType;}
			set {	if (instanceType == value) return;
				setInstanceType(value);}
		}

		protected void setInstanceType(Type newInstanceType) {
			if (isInstanceTypeLocked)  throw new PrimitiveFailException("The instance type of classes representing open generic types cannot be changed.");
			if (instanceType == newInstanceType) return;
			if (instanceType != null) unbindFromInstanceType();
			instanceType = newInstanceType;
			if (instanceType != null) bindToInstanceType();
			changedInstanceType();
		}

		protected virtual void changedInstanceType() {
			incrementVersion();
		}

		protected virtual void unbindFromInstanceType() {
			if (InstanceArchitecture == ObjectStateArchitecture.HostSystemObject && instanceType.IsGenericType) {
				methodDictionary = newMethodDictionary();
				hostSystemMethodDictionary = newHostSystemMethodDictionary();
			}
		}

		protected void basicBindToInstanceType() {
			if (instanceType == null) return;
			if (InstanceArchitecture == ObjectStateArchitecture.HostSystemObject) {
				var namespacePrefix = instanceType.Namespace;
				if (namespacePrefix != HostSystemNamespace) {
					hostSystemNamespace = namespacePrefix;
				}
				var typeName = instanceType.Name;
				if (typeName != HostSystemName) {
					hostSystemName = typeName;
				}
			}
		}

		protected virtual void bindToInstanceType() {
			isBoundToHostSystemNamespace = instanceArchitecture == ObjectStateArchitecture.HostSystemObject;
			if (instanceType == null) return;
			assembly = instanceType.Assembly;
			if (!isClassOfAdoptedType(InstanceArchitecture) && !instanceType.isEssenceSharpType()) InstanceArchitecture = ObjectStateArchitecture.HostSystemObject;
			basicBindToInstanceType();
		}

		protected void invalidateInstanceType() {
			setInstanceType(getInstanceType());
		}

		public Type typeFromQualifiedName(String qualifiedTypeName, bool raiseExceptionOnErrorOrNotFound) {
			Type type = null;
			var assembly = Assembly;
			if (assembly != null) {
				type = assembly.GetType(qualifiedTypeName, false);
				if (type != null) return type;
			}
			return Type.GetType(qualifiedTypeName, raiseExceptionOnErrorOrNotFound);
		}

		protected virtual Type ReflectionType {
			get { return InstanceType;}
		}

		public ESSymbol nameInEnvironmentFor(Type hostSystemType) {
			return objectSpace.symbolFor(new TypeName(hostSystemType).NameWithGenericArguments);
		}

		public override Assembly Assembly {
			get {	if (assembly == null) {
					if (instanceType == null) return base.Assembly;
					assembly = instanceType.Assembly;
				}
				return assembly;
			}
			set {	if (assembly == value) return;
				assembly = value;
				if (instanceType != null && assembly != instanceType.Assembly) invalidateInstanceType();}
		}

		#region getInstanceType()

		protected Type getInstanceType() {
			switch (InstanceArchitecture) {
				case ObjectStateArchitecture.Abstract:
					return typeof(ESInitiallyMutableObject);		
				case ObjectStateArchitecture.Stateless:
					return TypeGuru.esObjectType;		
				case ObjectStateArchitecture.NamedSlots:
					return TypeGuru.esNamedSlotsObjectType;		
				case ObjectStateArchitecture.IndexedByteSlots:
					return TypeGuru.esByteArrayType;		
				case ObjectStateArchitecture.IndexedCharSlots:
					return TypeGuru.esStringType;		
				case ObjectStateArchitecture.IndexedHalfWordSlots:
					return TypeGuru.esHalfWordArrayType;		
				case ObjectStateArchitecture.IndexedWordSlots:
					return TypeGuru.esWordArrayType;		
				case ObjectStateArchitecture.IndexedLongWordSlots:
					return TypeGuru.esLongWordArrayType;	
				case ObjectStateArchitecture.IndexedSinglePrecisionSlots:
					return TypeGuru.esFloatArrayType;		
				case ObjectStateArchitecture.IndexedDoublePrecisionSlots:
					return TypeGuru.esDoubleArrayType;		
				case ObjectStateArchitecture.IndexedQuadPrecisionSlots:
					return TypeGuru.esQuadArrayType;		
				case ObjectStateArchitecture.Pathname:
					return TypeGuru.esPathnameType;		
				case ObjectStateArchitecture.IndexedObjectSlots:
					return TypeGuru.esObjectArrayType;	
				case ObjectStateArchitecture.Symbol:
					return TypeGuru.esSymbolType;		
				case ObjectStateArchitecture.Message:
					return TypeGuru.esMessageType;		
				case ObjectStateArchitecture.MessageSend:
					// return TypeGuru.esMessageSendType;		
					return null;
				case ObjectStateArchitecture.Association:
					return TypeGuru.esAssociationType;		
				case ObjectStateArchitecture.BindingReference:
					return TypeGuru.esBindingReferenceType;		
				case ObjectStateArchitecture.Dictionary:
					return TypeGuru.esDictionaryType;			
				case ObjectStateArchitecture.Namespace:
					return TypeGuru.esNamespaceType;		
				case ObjectStateArchitecture.Behavior:
					return TypeGuru.esBehaviorType;		
				case ObjectStateArchitecture.Class:
					return TypeGuru.esClassType;	
				case ObjectStateArchitecture.Metaclass:
					return TypeGuru.esMetaclassType;
				case ObjectStateArchitecture.BehavioralTrait:
					return TypeGuru.esBehavioralTraitType;
				case ObjectStateArchitecture.InstanceTrait:
					return TypeGuru.esInstanceTraitType;
				case ObjectStateArchitecture.ClassTrait:
					return TypeGuru.esClassTraitType;
				case ObjectStateArchitecture.TraitTransformation:
					return TypeGuru.esTraitTransformationType;
				case ObjectStateArchitecture.TraitComposition:
					return TypeGuru.esTraitCompositionType;
				case ObjectStateArchitecture.Block:
					return TypeGuru.esBlockType;		
				case ObjectStateArchitecture.Method:
					return TypeGuru.esMethodType;		
				case ObjectStateArchitecture.Nil:
					return TypeGuru.nullType;		
				case ObjectStateArchitecture.False:
					return TypeGuru.boolType;	
				case ObjectStateArchitecture.True:
					return TypeGuru.boolType;	
				case ObjectStateArchitecture.Char:
					return TypeGuru.charType;		
				case ObjectStateArchitecture.SmallInteger:
					return TypeGuru.longType;		
				case ObjectStateArchitecture.LargeInteger:
					// return TypeGuru.bigIntegerType;		
					return null;		
				case ObjectStateArchitecture.ScaledDecimal:
					// return TypeGuru.esScaledDecimalType;	
					return null;		
				case ObjectStateArchitecture.SinglePrecision:
					return TypeGuru.floatType;		
				case ObjectStateArchitecture.DoublePrecision:
					return TypeGuru.doubleType;		
				case ObjectStateArchitecture.QuadPrecision:
					return TypeGuru.decimalType;		
				case ObjectStateArchitecture.HostSystemObject:
					return typeFromQualifiedName(QualifiedHostSystemName, false);
				default:
					return null;
			}
		}

		#endregion

		#endregion

		#region General protocol

		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.Behavior;}
		}
		
		public override bool IsBehavior {
			get {return true;}
		}
	
		public override ESBehavior asESBehavior() {
			return this;
		}

		protected override void incrementVersion() {
			base.incrementVersion();
			foreach (var subclass in subclasses) subclass.incrementVersion();
		}

		public override void postCopy() {
			base.postCopy();
			subclasses = newSubclassesSet();
			if (superclass != null) superclass.basicAddSubclass(this);
		}

		#endregion

		#region Compiling methods

		public override void recompileAll() {
			base.recompileAll();
			foreach (var subclass in subclasses) subclass.recompileAll();			
		}

		#endregion

		#region Inheritance hierarchy

		protected void throwIncompatibleSuperclassException(ObjectStateArchitecture instanceArchitecture, ESBehavior incompatibleSuperclass) {
			throw new PrimInvalidOperandException("A class whose instance architecture is " + instanceArchitecture + " cannot have a superclass whose instance architecture is " + incompatibleSuperclass.InstanceArchitecture + ".");
		}

		protected void throwIncompatibleSuperclassException(ESBehavior incompatibleSuperclass) {
			throwIncompatibleSuperclassException(InstanceArchitecture, incompatibleSuperclass);
		}

 		public override bool HasSuperclass {
			get {return superclass != null;}
		}
		
		public override ESBehavior Superclass {
			get {return superclass;}
		}

		protected bool canInheritFrom(ObjectStateArchitecture instanceArchitecture, ESBehavioralObject aSuperclass) {
			if (aSuperclass == null) return true;
			var superclass = aSuperclass as ESBehavior;
			if (superclass == null) return false;
			if (!isValidInheritanceRelationship(instanceArchitecture, superclass.InstanceArchitecture)) { 
				return false;
			}
			return canInheritFrom(instanceArchitecture, superclass.Superclass);
		}

		public override bool canInheritFrom(ESBehavioralObject aSuperclass) {
			return canInheritFrom(InstanceArchitecture, aSuperclass);
		}

		public void assertValidInheritanceStructure(ESBehavior superclass) {
			if (!constraintsMustBeSatisfied) return;
			if (!canInheritFrom(superclass)) {
				if (isInstanceArchitectureLocked) throwIncompatibleSuperclassException(superclass);
				switch (superclass.InstanceArchitecture) {
					case ObjectStateArchitecture.Abstract:
					case ObjectStateArchitecture.Stateless:
						InstanceArchitecture = ObjectStateArchitecture.NamedSlots;
						break;
					default:
						InstanceArchitecture = superclass.InstanceArchitecture;
						break;
				}			
			}
		}

		public void setSuperclass(ESBehavior newSuperclass) {
			if (superclass == newSuperclass) return;
			if (newSuperclass != null) {
				assertValidInheritanceStructure(newSuperclass);
				var wouldCreateCycle = newSuperclass.includesBehavior(this);
				if (wouldCreateCycle) {
					throw new PrimInvalidOperandException("A superclass must not also inherit from a subclass");
				}
			}
			unbindFromSuperclass();
			superclass = newSuperclass;
			bindToSuperclass();		
		}

		protected virtual void unbindFromSuperclass() {
			if (superclass != null) superclass.basicRemoveSubclass(this);
		}

		protected virtual void bindToSuperclass() {
			if (superclass != null) superclass.basicAddSubclass(this);
			changedSuperclass();
		}

		protected virtual void changedSuperclass() {
			recompile();
			incrementVersion();
		}

		protected void basicAddSubclass(ESBehavior subclass) {
			if (subclass == null) return;
			subclasses.Add(subclass);
		}

		protected void basicRemoveSubclass(ESBehavior subclass) {
			if (subclass == null) return;
			subclasses.Remove(subclass);
		}

		public void addSubclass(ESBehavior subclass) {
			if (subclass == null) return;
			var wouldCreateCycle = this.includesBehavior(subclass);
			if (wouldCreateCycle) {
				throw new PrimInvalidOperandException("A superclass must not also inherit from a subclass");
			}
			int prevSize = subclasses.Count;
			subclasses.Add(subclass);
			subclass.setSuperclass(this);
		}

		public void removeSubclass(ESBehavior subclass) {
			if (subclass == null) return;
			if (subclasses.Remove(subclass)) subclass.setSuperclass(null);
		}

		internal void bindToHostSystemSuperclasses() {
			if (superclass != null || InstanceArchitecture != ObjectStateArchitecture.HostSystemObject) return;
			Type systemType = InstanceType;
			if (systemType == null) return;
			Type systemSupertype;
			if (systemType.IsGenericTypeDefinition || !systemType.IsGenericType) {
				systemSupertype = systemType.BaseType;
			} else {
				systemSupertype = systemType.GetGenericTypeDefinition();
			}
			if (systemSupertype == null) return;
			var newSuperclass = objectSpace.classForHostSystemType(systemSupertype);
			setSuperclass(newSuperclass);
		}

		public override bool includesBehavior(ESBehavioralObject aBehavior) {
			// The receiver may either be a subclass of <aBehavior>, or it may be identical to <aBehavior>
			if (this == aBehavior) return true;
			ESBehavior mySuperclass = Superclass;
			return mySuperclass == null ?
				false :
				mySuperclass.includesBehavior(aBehavior);
		}
		
		public override bool inheritsFrom(ESBehavioralObject aBehavior) {
			// The receiver must be a SUBCLASS of <aBehavior>
			if (aBehavior == null) return true;
			ESBehavior mySuperclass = Superclass;
			return mySuperclass == null ?
				false :
				mySuperclass.includesBehavior(aBehavior);
		}

		#endregion

		#region Methods -- Smalltalk Protocol

		public override void allSelectorsDo(FuncNs.Func<Object, Object> enumerator1, HashSet<ESSymbol> exclusionSet) {
			selectorsDo(enumerator1, exclusionSet);
			if (HasSuperclass) Superclass.allSelectorsDo(enumerator1, exclusionSet);
		}

		public override void allSelectorsAndMethodsDo(FuncNs.Func<Object, Object, Object> enumerator2, HashSet<ESSymbol> exclusionSet) {
			selectorsAndMethodsDo(enumerator2, exclusionSet);
			if (HasSuperclass) Superclass.allSelectorsAndMethodsDo(enumerator2, exclusionSet);
		}
		
		public override ESMethod compiledMethodAt(ESSymbol selector) {
			var method = base.compiledMethodAt(selector);
			if (method != null) return method;
			return HasSuperclass ? Superclass.compiledMethodAt(selector) : null;
		}

		public override bool canUnderstand(ESSymbol selector) {
			return includesSelector(selector) || (HasSuperclass && Superclass.canUnderstand(selector));		
		}

		#endregion

		#region Methods -- Host System Protocol

		public override ESMethod compiledMethodAtSystemSelector(String systemSelector, long numArgs) {
			var method = base.compiledMethodAtSystemSelector(systemSelector, numArgs);
			if (method != null) return method;
			return HasSuperclass ? Superclass.compiledMethodAtSystemSelector(systemSelector, numArgs) : null;
		}
		
		public override bool canUnderstandSystemMessage(String systemSelector, long numArgs) {
			return includesSystemSelector(systemSelector, numArgs) || HasSuperclass && Superclass.canUnderstandSystemMessage(systemSelector, numArgs);	
		}

		#endregion

		#region Namespace protocol

		protected override String AnonymousName {
			get {return "AnAnonymousBehavior";}
		}
		
		internal override void nameChanged() {
			if (hostSystemName == null && InstanceArchitecture == ObjectStateArchitecture.HostSystemObject) invalidateInstanceType();
		}
		
		internal override void hostSystemNameChanged() {
			if (hostSystemName == null || InstanceArchitecture == ObjectStateArchitecture.HostSystemObject) {
				invalidateInstanceType();
			} else {
				InstanceArchitecture = ObjectStateArchitecture.HostSystemObject;
			}
		}
		
		internal override void hostSystemNamespaceChanged() {
			if (hostSystemNamespace == null || InstanceArchitecture == ObjectStateArchitecture.HostSystemObject) {
				invalidateInstanceType();
			} else {
				InstanceArchitecture = ObjectStateArchitecture.HostSystemObject;
			}
		}

		protected ESBindingReference bindingForSubclassAt(String key, AccessPrivilegeLevel requestorPrivilege, ImportTransitivity importTransitivity, HashSet<ESNamespace> transitiveClosure) {
			ESBindingReference binding = localBindingAt(key, requestorPrivilege);
			if (binding != null) return binding;
			binding = importedBindingAt(key, (AccessPrivilegeLevel)Math.Min((int)requestorPrivilege, (int)AccessPrivilegeLevel.InHierarchy), transitiveClosure);
			if (binding != null) return binding;
			return superclass == null ? 
				null : 
				superclass.bindingForSubclassAt(key, 
					(AccessPrivilegeLevel)Math.Min((int)requestorPrivilege, (int)AccessPrivilegeLevel.InHierarchy), 
					importTransitivity, 
					transitiveClosure);
		}

		protected override ESBindingReference inheritedBindingAt(String key, AccessPrivilegeLevel requestorPrivilege, ImportTransitivity importTransitivity, HashSet<ESNamespace> transitiveClosure) {
			ESBindingReference binding = null;
			if (superclass != null) {
				binding = superclass.bindingForSubclassAt(
						key, 
						(AccessPrivilegeLevel)Math.Min((int)requestorPrivilege, (int)AccessPrivilegeLevel.InHierarchy), 
						importTransitivity, 
						transitiveClosure);
			}
			return binding ?? base.inheritedBindingAt(key, requestorPrivilege, importTransitivity, transitiveClosure);

		}

		#endregion
		
		#region Instance creation
		
		public Object newInstance() {
			switch (InstanceArchitecture) {	
				case ObjectStateArchitecture.Abstract:
					throw new PrimitiveFailException("A class whose instance architecture is #Abstract cannot have any instances.");
				case ObjectStateArchitecture.Stateless:
					isInstanceArchitectureLocked = true;
					return new ESObject(this);
				case ObjectStateArchitecture.NamedSlots:
					isInstanceArchitectureLocked = true;
					return new ESNamedSlotsObject(this);
				case ObjectStateArchitecture.IndexedObjectSlots:
					isInstanceArchitectureLocked = true;
					return new ESArray(this, 0);
				case ObjectStateArchitecture.IndexedByteSlots:
					isInstanceArchitectureLocked = true;
					return new ESByteArray(this, 0);
				case ObjectStateArchitecture.IndexedCharSlots:
					isInstanceArchitectureLocked = true;
					return new ESString(this, 0);
				case ObjectStateArchitecture.IndexedHalfWordSlots:
					isInstanceArchitectureLocked = true;
					return new ESHalfWordArray(this, 0);
				case ObjectStateArchitecture.IndexedWordSlots:
					isInstanceArchitectureLocked = true;
					return new ESWordArray(this, 0);
				case ObjectStateArchitecture.IndexedLongWordSlots:
					isInstanceArchitectureLocked = true;
					return new ESLongWordArray(this, 0);
				case ObjectStateArchitecture.IndexedSinglePrecisionSlots:
					isInstanceArchitectureLocked = true;
					return new ESFloatArray(this, 0);
				case ObjectStateArchitecture.IndexedDoublePrecisionSlots:
					isInstanceArchitectureLocked = true;
					return new ESDoubleArray(this, 0);
				case ObjectStateArchitecture.IndexedQuadPrecisionSlots:
					isInstanceArchitectureLocked = true;
					return new ESQuadArray(this, 0);
				case ObjectStateArchitecture.Behavior:
					isInstanceArchitectureLocked = true;
					return new ESBehavior(this, objectSpace);
				case ObjectStateArchitecture.Class:
					isInstanceArchitectureLocked = true;
					return new ESClass(Architecture == ObjectStateArchitecture.Metaclass ? this : objectSpace.newMetaclass());
				case ObjectStateArchitecture.Metaclass:
					isInstanceArchitectureLocked = true;
					return new ESMetaclass(this, objectSpace);

				case ObjectStateArchitecture.BehavioralTrait:
					return new ESBehavioralTrait(this);
				case ObjectStateArchitecture.InstanceTrait:
					return new ESInstanceTrait(this);
				case ObjectStateArchitecture.ClassTrait:
					return new ESClassTrait(this);
				case ObjectStateArchitecture.TraitComposition:
					return new ESTraitComposition(this);
										
				case ObjectStateArchitecture.Message:
					isInstanceArchitectureLocked = true;
					return new ESMessage(this);

				case ObjectStateArchitecture.MessageSend:
					throw new UnimplementedPrimitiveException("MessageSend objects are not yet implemented.");

				case ObjectStateArchitecture.Association:
					isInstanceArchitectureLocked = true;
					return new ESAssociation(this);

				case ObjectStateArchitecture.BindingReference:
					isInstanceArchitectureLocked = true;
					return new ESBindingReference(this);

				case ObjectStateArchitecture.IdentityDictionary:
					isInstanceArchitectureLocked = true;
					return new ESIdentityDictionary(this);

				case ObjectStateArchitecture.Dictionary:
					isInstanceArchitectureLocked = true;
					return new ESDictionary(this);

				case ObjectStateArchitecture.Namespace:
					isInstanceArchitectureLocked = true;
					return new ESNamespace(this);

				case ObjectStateArchitecture.Pathname:
					isInstanceArchitectureLocked = true;
					return new ESPathname(this);

				case ObjectStateArchitecture.Block:
					isInstanceArchitectureLocked = true;
					return new ESBlock(this);
				case ObjectStateArchitecture.Method:
					isInstanceArchitectureLocked = true;
					return new ESMethod(this);

				case ObjectStateArchitecture.HostSystemObject:
					Type hostType = InstanceType;
					if (hostType == null) throw new PrimitiveFailException("Named host type not resident.");
					return newInstanceOf(hostType);
					
				default:
					throw new PrimitiveFailException("The #new primitive cannot be used to create an instance with this class' instance architecture (#" + InstanceArchitecture + ".");

			}
			
		}
		
		public Object newWithSize(long size) {
			switch (InstanceArchitecture) {
				case ObjectStateArchitecture.IndexedByteSlots:
					isInstanceArchitectureLocked = true;
					return new ESByteArray(this, size);
				case ObjectStateArchitecture.IndexedCharSlots:
					isInstanceArchitectureLocked = true;
					return new ESString(this, size);
				case ObjectStateArchitecture.IndexedHalfWordSlots:
					isInstanceArchitectureLocked = true;
					return new ESHalfWordArray(this, size);
				case ObjectStateArchitecture.IndexedWordSlots:
					isInstanceArchitectureLocked = true;
					return new ESWordArray(this, size);
				case ObjectStateArchitecture.IndexedLongWordSlots:
					isInstanceArchitectureLocked = true;
					return new ESLongWordArray(this, size);
				case ObjectStateArchitecture.IndexedSinglePrecisionSlots:
					isInstanceArchitectureLocked = true;
					return new ESFloatArray(this, size);
				case ObjectStateArchitecture.IndexedDoublePrecisionSlots:
					isInstanceArchitectureLocked = true;
					return new ESDoubleArray(this, size);
				case ObjectStateArchitecture.IndexedQuadPrecisionSlots:
					isInstanceArchitectureLocked = true;
					return new ESQuadArray(this, size);
				case ObjectStateArchitecture.IndexedObjectSlots:
					isInstanceArchitectureLocked = true;
					return new ESArray(this, size);

				case ObjectStateArchitecture.IdentityDictionary:
					isInstanceArchitectureLocked = true;
					return new ESIdentityDictionary(this, size);

				case ObjectStateArchitecture.Dictionary:
					isInstanceArchitectureLocked = true;
					return new ESDictionary(this, size);

				case ObjectStateArchitecture.Namespace:
					isInstanceArchitectureLocked = true;
					return new ESNamespace(this, size);

				case ObjectStateArchitecture.Pathname:
					isInstanceArchitectureLocked = true;
					return new ESPathname(this, size);

				case ObjectStateArchitecture.HostSystemObject:
					Type hostType = InstanceType;
					if (hostType == null) throw new PrimitiveFailException("Named host type is not resolvable: " + QualifiedHostSystemName);
					return newInstanceOf(hostType, new object[]{size});
					
				case ObjectStateArchitecture.Abstract:
					throw new PrimitiveFailException("A class whose instance architecture is #Abstract cannot have any instances.");
					
				default:
					throw new PrimitiveFailException("The #new: primitive cannot be used to create an instance with this class' instance architecture (#" + InstanceArchitecture + ".");
			}

		}
		
		public Object newWithValue(Object arg) {
			try {
				switch (InstanceArchitecture) {
					case ObjectStateArchitecture.IndexedByteSlots:
						isInstanceArchitectureLocked = true;
						return new ESByteArray(this, asHostArray<byte>(arg));
					case ObjectStateArchitecture.IndexedCharSlots:
						isInstanceArchitectureLocked = true;
						return new ESString(this, asHostArray<char>(arg));
					case ObjectStateArchitecture.IndexedHalfWordSlots:
						isInstanceArchitectureLocked = true;
						return new ESHalfWordArray(this, asHostArray<ushort>(arg));
					case ObjectStateArchitecture.IndexedWordSlots:
						isInstanceArchitectureLocked = true;
						return new ESWordArray(this, asHostArray<uint>(arg));
					case ObjectStateArchitecture.IndexedLongWordSlots:
						isInstanceArchitectureLocked = true;
						return new ESLongWordArray(this, asHostArray<ulong>(arg));
					case ObjectStateArchitecture.IndexedSinglePrecisionSlots:
						isInstanceArchitectureLocked = true;
						return new ESFloatArray(this, asHostArray<float>(arg));
					case ObjectStateArchitecture.IndexedDoublePrecisionSlots:
						isInstanceArchitectureLocked = true;
						return new ESDoubleArray(this, asHostArray<double>(arg));
					case ObjectStateArchitecture.IndexedQuadPrecisionSlots:
						isInstanceArchitectureLocked = true;
						return new ESQuadArray(this, asHostArray<decimal>(arg));
					case ObjectStateArchitecture.IndexedObjectSlots:
						isInstanceArchitectureLocked = true;
						return new ESArray(this, asHostArray<Object>(arg));
					case ObjectStateArchitecture.ScaledDecimal:
						throw new UnimplementedPrimitiveException("ScaledDecimal objects are not yet implemented.");
					case ObjectStateArchitecture.QuadPrecision:
						throw new UnimplementedPrimitiveException("QuadPrecision objects are not yet implemented.");

					case ObjectStateArchitecture.TraitTransformation:
						try {
							return new ESTraitTransformation(this, (Trait)arg);
						} catch (InvalidCastException ex) {
							throw new PrimitiveFailException("The subject of a TraitTransformation must be a Trait.", ex);
						}

					case ObjectStateArchitecture.Symbol:
						isInstanceArchitectureLocked = true;
						return objectSpace.symbolFor(asHostString(arg));

					case ObjectStateArchitecture.HostSystemObject:
					Type hostType = InstanceType;
						if (hostType == null) throw new PrimitiveFailException("Named host type not resident.");
						return newInstanceOf(hostType, new Object[]{arg});

					case ObjectStateArchitecture.Abstract:
						throw new PrimitiveFailException("A class whose instance architecture is #Abstract cannot have any instances.");

					default:
						throw new PrimitiveFailException(
							"The Behavior>>value: primitive cannot be used to create an instance with this class' instance architecture (#" +
							InstanceArchitecture + ".");
				}
			} catch (InvalidCastException ex) {
				throw new PrimitiveFailException("Invalid arguemnt", ex);
				
			}
	
		}
		
		#endregion

		#region Named instance variables

		protected override void invalidateInstanceVariableNames() {
			base.invalidateInstanceVariableNames();
			foreach (var subclass in subclasses) subclass.invalidateInstanceVariableNames();	
		}

		#endregion

		#region Host System Object Reflection

		public virtual BindingFlags HostObjectMethodInvokeBindingFlags {
			get { return instanceMethodInvokeBindingFlags; }
		}

		public virtual BindingFlags HostObjectFieldGetBindingFlags {
			get { return instanceFieldGetBindingFlags; }
		}

		public virtual BindingFlags HostObjectFieldSetBindingFlags {
			get { return instanceFieldSetBindingFlags; }
		}

		public virtual BindingFlags HostObjectPropertyGetBindingFlags {
			get { return instancePropertyGetBindingFlags; }
		}

		public virtual BindingFlags HostObjectPropertySetBindingFlags {
			get { return instancePropertySetBindingFlags; }
		}

		public ConstructorInfo getHostConstructor() {
			return getHostConstructor(ReflectionType, TypeGuru.emptyTypeArray);
		}

		public ConstructorInfo getHostConstructor(Type[] signature) {
			return getHostConstructor(ReflectionType, signature);
		}

		public virtual List<ConstructorInfo> getHostConstructors(long arity) {
			var matchingConstructors = new List<ConstructorInfo>();
			var type = ReflectionType;
			var constructors = type.GetConstructors(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance);
			foreach (var ci in constructors) {
				var parameters = ci.GetParameters();
				if (parameters.Length != arity) continue;
				matchingConstructors.Add(ci);
			}
			return matchingConstructors;
		}

		public virtual MethodInfo getHostMethod(String methodName) {
			return getHostInstanceMethod(ReflectionType, methodName);
		}

		public virtual MethodInfo getHostMethod(String methodName, Type[] signature) {
			return getHostInstanceMethod(ReflectionType, methodName, signature);
		}

		public virtual List<MethodInfo> getHostMethodsMatching(String methodName, long arity) {
			var matchingMethods = new List<MethodInfo>();
			var methods = ReflectionType.GetMethods(HostObjectMethodInvokeBindingFlags);
			foreach (var mi in methods) {
				var parameters = mi.GetParameters();
				if (parameters.Length != arity) continue;
				if (mi.Name != methodName) continue;
				matchingMethods.Add(mi);
			}
			return matchingMethods;
		}

		public PropertyInfo getReadableProperty(String name) {
			var property = ReflectionType.GetProperty(name, HostObjectPropertyGetBindingFlags);
			if (property == null) return null;
			return property.CanRead ? property : null;
		}

		public PropertyInfo getWritableProperty(String name) {
			var property = ReflectionType.GetProperty(name, HostObjectPropertySetBindingFlags);
			if (property == null) return null;
			return property.CanWrite ? property : null;
		}

		public FieldInfo getField(String name) {
			return InstanceType.GetField(name, HostObjectFieldGetBindingFlags);
		}

		public bool getReadablePropertyOrElseField(String name, Action<PropertyInfo> propertyAction, Action<FieldInfo> fieldAction) {
			String propertyName = name.usingCapitalizationScheme(CapitalizationScheme.InitialCapital);
			var property = getReadableProperty(propertyName);
			if (property != null) {
				propertyAction(property);
				return true;
			}
			String fieldName = name.usingCapitalizationScheme(CapitalizationScheme.InitialLowerCase);
			var field = getField(fieldName);
			if (field != null) {
				fieldAction(field);
				return true;
			}
			return false;
		}

		public bool getWritablePropertyOrElseField(String name, Action<PropertyInfo> propertyAction, Action<FieldInfo> fieldAction) {
			String propertyName = name.usingCapitalizationScheme(CapitalizationScheme.InitialCapital);
			var property = getWritableProperty(propertyName);
			if (property != null) {
				propertyAction(property);
				return true;
			}
			String fieldName = name.usingCapitalizationScheme(CapitalizationScheme.InitialLowerCase);
			var field = getField(fieldName);
			if (field != null) {
				fieldAction(field);
				return true;
			}
			return false;
		}

		public Object newHostObjectInstance() {
			return newInstanceOf(ReflectionType);
		}

		public Object newHostObjectInstance(Object[] args) {
			return newInstanceOf(ReflectionType, args);
		}

		public virtual Object sendHostMessage(Object receiver, String messageName) {
			return sendHostMessage(receiver, messageName, HostObjectMethodInvokeBindingFlags);
		}

		public virtual Object sendHostMessage(Object receiver, String messageName, Object[] args) {
			return sendHostMessage(receiver, messageName, args, HostObjectMethodInvokeBindingFlags);
		}

		public Object valueOfHostObjectField(Object receiver, String fieldName) {
			return valueOfHostObjectField(receiver, fieldName, HostObjectFieldGetBindingFlags);
		}

		public Object setValueOfHostObjectField(Object receiver, String fieldName, Object newValue) {
			return setValueOfHostObjectField(receiver, fieldName, newValue, HostObjectFieldGetBindingFlags);
		}

		public Object valueOfHostObjectProperty(Object receiver, String fieldName) {
			return valueOfHostObjectProperty(receiver, fieldName, HostObjectPropertyGetBindingFlags);
		}

		public Object setValueOfHostObjectProperty(Object receiver, String fieldName, Object newValue) {
			return setValueOfHostObjectProperty(receiver, fieldName, newValue, HostObjectPropertySetBindingFlags);
		}

		#endregion

		#region Interoperability

		public override DynamicMetaObject GetMetaObject(Expression parameter) {
			return new ESBehaviorDynamicMetaObject(parameter, BindingRestrictions.Empty, this, Class);
		}

		#endregion

		public override T valueBy<T>(Operation<T> operation) {
		    return operation.applyToBehavior(this);
		}

		public new class Primitives : ESBehavioralObject.Primitives {

			protected override void bindToObjectSpace() {
				domainClass = objectSpace.BehaviorClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.Behavior;}
			}

			#region Primitive Definitions

			public Object _setSuperclass_(Object receiver, Object superclass) {
				((ESBehavior)receiver).setSuperclass((ESBehavior)superclass);
				return receiver;
			}
		
			public Object _addSubclass_(Object receiver, Object subclass) {
				((ESBehavior)receiver).addSubclass((ESBehavior)subclass);
				return receiver;
			}
		
			public Object _removeSubclass_(Object receiver, Object subclass) {
				((ESBehavior)receiver).removeSubclass((ESBehavior)subclass);
				return receiver;
			}

			public Object _instanceEqualityComparer_(Object receiver) {
				return ((ESBehavior)receiver).InstanceEqualityComparator;
			}

			public Object _newObjectEqualityComparer_(Object receiver) {
				return ((ESBehavior)receiver).ObjectSpace.newObjectEqualityComparator();
			}

			public Object _instanceArchitecture_(Object receiver) {
				return objectSpace.asESSymbol(((ESBehavior)receiver).InstanceArchitecture.ToString());
			}
		
			public Object _setInstanceArchitecture_(Object receiver, Object instanceArchitecture) {
				ObjectStateArchitecture architecture;
				try {
					architecture = (ObjectStateArchitecture)Enum.Parse(typeof(ObjectStateArchitecture), objectSpace.asESSymbol(instanceArchitecture));
				} catch {
					throw new PrimitiveFailException("instanceArchitecture: <instanceArchitecture> must be a Symbol or String identifying a valid object state architecture.");
				}
				((ESBehavior)receiver).InstanceArchitecture = architecture;
				return receiver;
			}
		
			public Object _instanceType_(Object receiver) {
				return ((ESBehavior)receiver).InstanceType;
			}
		
			public Object _setInstanceType_(Object receiver, Object instanceTypeObject) {
				Type instanceType;
				try {
					instanceType = (Type)instanceTypeObject;
				} catch (InvalidCastException ex) {
					throw new PrimitiveFailException(ex);
				}
				return ((ESBehavior)receiver).InstanceType = (Type)instanceType;
			}
		
			public Object _new_(Object receiver) {
				return ((ESBehavior)receiver).newInstance();
			}
		
			public Object _newWithSize_(Object receiver, Object size) {
				return ((ESBehavior)receiver).newWithSize(asHostLong(size));
			}
		
			public Object _newWithValue_(Object receiver, Object initializingValue) {
				return ((ESBehavior)receiver).newWithValue(initializingValue);
			}
		
			#endregion

			public override void publishCanonicalPrimitives() {

				base.publishCanonicalPrimitives();

				publishPrimitive("superclass:",						new FuncNs.Func<Object, Object, Object>(_setSuperclass_));
				publishPrimitive("addSubclass:",					new FuncNs.Func<Object, Object, Object>(_addSubclass_));
				publishPrimitive("removeSubclass:",					new FuncNs.Func<Object, Object, Object>(_removeSubclass_));

				publishPrimitive("instanceEqualityComparer",				new FuncNs.Func<Object, Object>(_instanceEqualityComparer_));
				publishPrimitive("newObjectEqualityComparer",				new FuncNs.Func<Object, Object>(_newObjectEqualityComparer_));

				publishPrimitive("instanceArchitecture",				new FuncNs.Func<Object, Object>(_instanceArchitecture_));
				publishPrimitive("instanceArchitecture:",				new FuncNs.Func<Object, Object, Object>(_setInstanceArchitecture_));
				publishPrimitive("instanceType",					new FuncNs.Func<Object, Object>(_instanceType_));
				publishPrimitive("instanceType:",					new FuncNs.Func<Object, Object, Object>(_setInstanceType_));

				publishPrimitive("new",							new FuncNs.Func<Object, Object>(_new_));
				publishPrimitive("newWithSize:",					new FuncNs.Func<Object, Object, Object>(_newWithSize_));
				publishPrimitive("newWithValue:",					new FuncNs.Func<Object, Object, Object>(_newWithValue_));

			}

		}

	}

	public abstract class ESAbstractClass : ESBehavior {

		#region Instance variables

		protected ESSymbol[]								instanceVariableNames			= emptyInstanceVariableNames;
		protected Dictionary<ESSymbol, long>						instanceVariableIndexes			= emptyInstanceVariableIndexes;

		#endregion

		#region Constructors

		internal ESAbstractClass(ObjectStateArchitecture instanceArchitecture) : base(instanceArchitecture) {
		}

		protected ESAbstractClass(ESBehavior metaClass) : base(metaClass) {
			instanceVariableNames = emptyInstanceVariableNames;
		}

		public ESAbstractClass(ESBehavior metaClass, ESObjectSpace objectSpace) : base(metaClass, objectSpace) {
		}
			
		protected ESAbstractClass(ESBehavior metaClass, ObjectStateArchitecture instanceArchitecture, ESBehavior superclass) 
					: base(metaClass, instanceArchitecture, superclass) {
		}
			
		public ESAbstractClass(ESBehavior metaClass, ESObjectSpace objectSpace, ObjectStateArchitecture instanceArchitecture, ESBehavior superclass) 
					: base(metaClass, objectSpace, instanceArchitecture, superclass) {
		}
			
		protected ESAbstractClass(ESBehavior metaClass, ObjectStateArchitecture instanceArchitecture, ESSymbol[] instanceVarnames, ESBehavior superclass) 
					: base(metaClass, instanceArchitecture, superclass) {
			setInstanceVariableNames(instanceVarnames);
		}
			
		public ESAbstractClass(ESBehavior metaClass, ESObjectSpace objectSpace, ObjectStateArchitecture instanceArchitecture, ESSymbol[] instanceVarnames, ESBehavior superclass) 
					: base(metaClass, objectSpace, instanceArchitecture, superclass) {
			setInstanceVariableNames(instanceVarnames);
		}

		#endregion

		#region General protocol

		public override void postCopy() {
			base.postCopy();
			instanceVariableIndexes = null;
			if (instanceVariableNames.Length > 0) instanceVariableNames = (ESSymbol[])instanceVariableNames.Clone();
		}

		#endregion

		#region Named instance variables

		public override ESSymbol[] InstanceVariableNames {
			get {return instanceVariableNames;}
		}
		
		public void setInstanceVariableNames(ESSymbol[] instanceVarNames) {
			ESSymbol[] prevInstVarNames = instanceVariableNames;
			if (InstancesCanHaveNamedSlots) {
				instanceVariableNames = (instanceVarNames == null || instanceVarNames.Length < 1) ? emptyInstanceVariableNames : instanceVarNames;
				if (!elementsAreIdentical(prevInstVarNames, instanceVariableNames)) {
					invalidateInstanceVariableNames();
				}
			} else {
				if (instanceVarNames == null || instanceVarNames.Length < 1) {
					instanceVariableNames = emptyInstanceVariableNames;
					if (prevInstVarNames.Length > 0) {
						invalidateInstanceVariableNames();
					}
				} else if (isInstanceArchitectureLocked) {
					throw new PrimInvalidOperandException("A Behavior with instance architecture " + instanceArchitecture + " cannot have any instance variables.");
				} else {
					InstanceArchitecture = ObjectStateArchitecture.NamedSlots;
					setInstanceVariableNames(instanceVarNames);
					return;
				}
			}
			instanceVariableIndexes = null;
		}

		public override ESSymbol localInstVarNameAt(long index) {
			if (index >= instanceVariableNames.Length || index < 0) return null;
			return instanceVariableNames[index];
		}
		
		public override ESSymbol instVarNameAt(long index) {
			if (index < 0) return null;
			long superInstSize = SuperInstSize;
			if (index < superInstSize) return superInstSize > 0 ? superclass.instVarNameAt(index) : null;
			index -= superInstSize;
			if (index >= instanceVariableNames.Length) return null;
			return instanceVariableNames[index];
		}
		
		protected override Dictionary<ESSymbol, long> InstanceVariableIndexes {
			get {if (instanceVariableIndexes == null) mapInstanceVariableNamesToIndexes();
				return instanceVariableIndexes;}
		}

		protected Dictionary<ESSymbol, long> newInstanceVariableIndexMap() {
			return new Dictionary<ESSymbol, long>();
		}

		protected void mapInstanceVariableNamesToIndexes() {
			if (instanceVariableNames.Length > 0) {
				instanceVariableIndexes = newInstanceVariableIndexMap();
				for (uint index = 0; index < instanceVariableNames.Length; index++) {
					instanceVariableIndexes[instanceVariableNames[index]] = index;
				}
			} else {
				instanceVariableIndexes = emptyInstanceVariableIndexes;
			}
		}
		
		public override long instVarIndexFor(ESSymbol instanceVariableName) {
			if (instanceVariableIndexes == null) mapInstanceVariableNamesToIndexes();
			if (instanceVariableIndexes.Count < 1) return superclass == null ? -1 : superclass.instVarIndexFor(instanceVariableName);
			long index;
			if (instanceVariableIndexes.TryGetValue(instanceVariableName, out index)) {
				return index + SuperInstSize;
			} else {
				return superclass == null ? -1 : superclass.instVarIndexFor(instanceVariableName);
			}
		}
		
		public override long BasicInstSize {
			get {return instanceVariableNames.Length;}
		}
		
		public override void allInstVarNamesAndIndexesDo(System.Action<ESSymbol, long> enumerator2) {
			long baseIndex = 0;
			if (superclass != null) {
				superclass.allInstVarNamesAndIndexesDo(enumerator2);
				baseIndex = SuperInstSize;
			}
			if (instanceVariableNames.Length < 1) return;
			for (var i = 0; i < instanceVariableNames.Length; i++) {
				var name = instanceVariableNames[i];
				enumerator2(name, baseIndex + i);
			}
		}

		#endregion

		public new abstract class Primitives : PrimitiveDomain {
		
			public Object _setInstanceVariableNames_(Object receiver, Object instanceVariableNames) {
				Object[] namesArray = asHostArray<Object>(instanceVariableNames);
				ESSymbol[] symbolArray = Array.ConvertAll<Object, ESSymbol>(namesArray, new Converter<Object, ESSymbol>(objectSpace.asESSymbol));
				((ESAbstractClass)receiver).setInstanceVariableNames(symbolArray);
				return receiver;
			}

			public override void publishCanonicalPrimitives() {
				publishPrimitive("instanceVariableNames:",				new FuncNs.Func<Object, Object, Object>(_setInstanceVariableNames_));
			}

		}

	}

	public class ESClass : ESAbstractClass {

		internal ESClass(ObjectStateArchitecture instanceArchitecture) : base(instanceArchitecture) {
		}
						
		public ESClass(ESBehavior metaClass) : base(metaClass) {
		}
			
		internal ESClass(ESBehavior metaClass, Type hostSystemType) : base(metaClass) {
			instanceArchitecture = ObjectStateArchitecture.HostSystemObject;
			setName(nameInEnvironmentFor(hostSystemType));
			setInstanceType(hostSystemType);
			isBoundToHostSystemNamespace = true;
			bindToHostSystemSuperclasses();
		}
			
		public ESClass(ESBehavior metaClass, ESSymbol name) : base(metaClass) {
			setName(name);
		}
			
		public ESClass(ESBehavior metaClass, ESSymbol name, ObjectStateArchitecture instanceArchitecture) 
						: this(metaClass, name, instanceArchitecture, null) {
		}
			
		public ESClass(ESBehavior metaClass, ESSymbol name, ObjectStateArchitecture instanceArchitecture, ESBehavior superclass) 
						: this(metaClass, name, instanceArchitecture, null, superclass) {
		}
						
		public ESClass(ESBehavior metaClass, ESSymbol name, ObjectStateArchitecture instanceArchitecture, ESSymbol[] instanceVarnames, ESBehavior superclass) 
						: base(metaClass, instanceArchitecture, instanceVarnames, superclass) {
			setName(name);
		}

		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.Class;}
		}

		public override void postCopy() {
			base.postCopy();
			var metaclass = Class;
			if (metaclass != null) setClass((ESBehavior)metaclass.copy());
		}
		
		public override bool IsClass {
			get {return true;}
		}
		
		public ESMetaclass Metaclass {
			get {return (ESMetaclass)Class;}
		}
		
		protected override String AnonymousName {
			get {return "AnAnonymousClass";}
		}
		
		internal override void nameChanged() {
			base.nameChanged();
			var metaClass = Metaclass;
			if (metaClass != null) metaClass.nameChanged();
		}
		
		internal override void setClass(ESBehavior metaClass) {
			if (metaClass == null) return;
			var metaclass = metaClass as ESMetaclass;
			if (metaclass == null) throw new PrimInvalidOperandException("The class of a Class must be a Metaclass.");
			setObjectSpace(metaclass.ObjectSpace);
			base.setClass(metaclass);
			metaclass.adopt(this);
		}
		
		protected override void bindToSuperclass() {
			base.bindToSuperclass();
			var metaClass = Metaclass;
			if (metaClass != null) metaClass.bindToCanonicalInstance();			
		}

		protected override void bindToInstanceType() {
			base.bindToInstanceType();
			if (InstanceArchitecture == ObjectStateArchitecture.HostSystemObject) {
				if (instanceType.IsGenericTypeDefinition) {
					isInstanceTypeLocked = true;
					isInstanceArchitectureLocked = true;
				}
				objectSpace.bindHostSystemTypeTo(instanceType, this);
				bindToHostSystemSuperclasses();
			}
		}

		public override T valueBy<T>(Operation<T> operation) {
		    return operation.applyToClass(this);
		}

		public new class Primitives : ESAbstractClass.Primitives {

			protected override void bindToObjectSpace() {
				domainClass = objectSpace.ClassClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.Class;}
			}

		}
		
	}
	
	public class ESMetaclass : ESAbstractClass {
			
		internal ESMetaclass(ESBehavior metaClass, ESObjectSpace objectSpace) : this(metaClass, objectSpace, null, null) {
		}
			
		internal ESMetaclass(ESBehavior metaClass, ESObjectSpace objectSpace, ESBehavior superclass) : this(metaClass, objectSpace, null, superclass) {
		}

		internal ESMetaclass(ESBehavior metaClass, ESObjectSpace objectSpace, ESSymbol[] instanceVarnames, ESBehavior superclass) : base(metaClass, objectSpace, ObjectStateArchitecture.Class, instanceVarnames, superclass) {
		}

		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.Metaclass;}
		}

		public override ObjectStateArchitecture InstanceArchitecture {
			get {return ObjectStateArchitecture.Class;}
			set {	if (instanceArchitecture == value) return;
				instanceArchitecture = ObjectStateArchitecture.Class;
				isInstanceArchitectureLocked = true;
				invalidateInstanceType(); 
			}
		}

		public override void postCopy() {
			base.postCopy();
			environment = null;
			setSuperclass(objectSpace == null ? null : objectSpace.ClassClass);
		}
		
		public override bool IsMetaclass {
			get {return true;}
		}
		
		public override bool IsHostSystemMetaclass {
			get {	var baseClass = CanonicalInstance;
				if (baseClass == null) return false;
				return isClassOfHostSystemType(baseClass.InstanceArchitecture);}
		}
		
		public ESClass CanonicalInstance {
			get {return (ESClass)environment;}
		}
		
		public bool HasCanonicalInstance {
			get {return environment != null;}
		}
		
		internal ESObject adopt(ESClass canonicalInstance) {
			if (canonicalInstance != null && ReferenceEquals(canonicalInstance.Class, this)) {
				environment = (ESClass)canonicalInstance;
				bindToCanonicalInstance();
			} else {
				throw new PrimInvalidOperandException("The class of the canonical instance of a Metaclass must be identical to the Metaclass.");
			}
			return CanonicalInstance;
		}
		
		internal void bindToCanonicalInstance() {
			setSuperclass(CanonicalInstance.HasSuperclass ? CanonicalInstance.Superclass.Class : objectSpace.ClassClass);
			name = null;
		}

		protected override void changedSuperclass() {
			invalidateInstanceVariableNames();
		}
		
		protected override String AnonymousName {
			get {return "AnAnonymousMetaclass";}
		}

		public override ESSymbol Name {
			get {if (name == null) {
					name = HasCanonicalInstance ?
								objectSpace.symbolFor(CanonicalInstance.Name.PrimitiveValue + " class") :
								objectSpace.symbolFor(AnonymousName);
				}
				return name;}
		}
		
		internal override void nameChanged() {
			name = null;
		}
		
		internal override void hostSystemNameChanged() {
			// Do nothing
		}
		
		internal override void hostSystemNamespaceChanged() {
			// Do nothing
		}

		protected override void basicSetName(ESSymbol newName) {
			nameChanged();
		}

		public override void setName(ESSymbol newName) {
			nameChanged();
		}

		public override void renameFromTo(ESSymbol prevName, ESSymbol newName) {
			nameChanged();
		}

		public override ESPathname pathname() {
			ESPathname pn = CanonicalInstance.pathname();
			pn.appendExtension(' ', "class");
			return pn;
		}

		public override void setEnvironment(ESNamespace newEnvironment) {
			// Should not implement
		}

		protected override void unbindFromEnvironment() {
			// Should not implement
		}

		protected override void bindToEnvironment() {
			// Should not implement
		}

		protected override void invalidateInstanceVariableNames() {
			var soleInstance = CanonicalInstance;
			if (soleInstance != null) soleInstance.invalidateNamedSlots();
			base.invalidateInstanceVariableNames();
		}

		protected override Type ReflectionType {
			get {	var baseClass = CanonicalInstance;
				if (baseClass == null) return InstanceType;
				return isClassOfHostSystemType(baseClass.InstanceArchitecture) ?
					baseClass.InstanceType :
					InstanceType;}
		}

		public override BindingFlags HostObjectMethodInvokeBindingFlags {
			get { return staticMethodInvokeBindingFlags; }
		}

		public override BindingFlags HostObjectFieldGetBindingFlags {
			get { return staticFieldGetBindingFlags; }
		}

		public override BindingFlags HostObjectFieldSetBindingFlags {
			get { return staticFieldSetBindingFlags; }
		}

		public override BindingFlags HostObjectPropertyGetBindingFlags {
			get { return staticPropertyGetBindingFlags; }
		}

		public override BindingFlags HostObjectPropertySetBindingFlags {
			get { return staticPropertySetBindingFlags; }
		}

		public override MethodInfo getHostMethod(String methodName) {
			return getHostClassMethod(ReflectionType, methodName);
		}

		public override MethodInfo getHostMethod(String methodName, Type[] signature) {
			return getHostClassMethod(ReflectionType, methodName, signature);
		}

		public override Object sendHostMessage(Object receiver, String messageName) {
			return sendHostClassMessage(receiver.GetType(), messageName, HostObjectMethodInvokeBindingFlags);
		}

		public override Object sendHostMessage(Object receiver, String messageName, Object[] args) {
			return sendHostClassMessage(receiver.GetType(), messageName, args, HostObjectMethodInvokeBindingFlags);
		}

		public override T valueBy<T>(Operation<T> operation) {
		    return operation.applyToMetaclass(this);
		}

		public new class Primitives : ESAbstractClass.Primitives {

			protected override void bindToObjectSpace() {
				domainClass = objectSpace.MetaclassClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.Metaclass;}
			}

			#region Primitive Definitions
		
			public Object _canonicalInstance_(Object receiver) {
				return ((ESMetaclass)receiver).CanonicalInstance;
			}

			#endregion

			public override void publishCanonicalPrimitives() {
				base.publishCanonicalPrimitives();
				publishPrimitive("canonicalInstance",				new FuncNs.Func<Object, Object>(_canonicalInstance_));
			}

		}

	}

	#endregion

	#region Traits (A Trait is a composable unit of behavior that defines a set of methods that classes or other Traits can "use" (in the sense of "import"))

	public class ESBehavioralTrait : ESBehavioralObject, Trait {
		
		protected HashSet<TraitUser> users;

		public ESBehavioralTrait(ESBehavior esClass) : base(esClass) {
		}

		#region Internal operations

		protected HashSet<TraitUser> newUserSet() {
			return new HashSet<TraitUser>(new TraitIUserIdentityComparator());
		}

		public void traitUsersDo(Action<TraitUser> enumerator1) {
			if (users == null) return;
			foreach (var user in users) enumerator1(user);
		}

		#endregion

		#region Internal operations

		internal override void setClass(ESBehavior metaClass) {
			if (metaClass == null) return;
			setObjectSpace(metaClass.ObjectSpace);
			base.setClass(metaClass);
		}

		#endregion

		#region General protocol

		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.BehavioralTrait;}
		}

		public override void postCopy() {
			base.postCopy();
			users = newUserSet();
		}

		protected override void initialize() {
			base.initialize();
			users = newUserSet();
		}

		#endregion

		#region Namespace protocol
		
		protected override String AnonymousName {
			get {return "AnAnonymousTrait";}
		}

		#endregion

		#region Methods

		protected override void changedBehavior() {
			base.changedBehavior();
			traitUsersDo(user => user.invalidateUsedTraits(this));
		}

		#endregion

		#region TraitUser protocol

		public override void invalidateUsedTraits(TraitUsageExpression source) {
			if (source != this) base.invalidateUsedTraits(source);
		}

		#endregion

		#region TraitUsageExpression protocol

		protected ESTraitComposition newComposition() {
			return ObjectSpace.newTraitComposition();
		}

		protected ESTraitTransformation asTransformation() {
			return ObjectSpace.newTraitTransformation(this);
		}

		public TraitUsageExpression IdentityObject {
			get {return this;}
		}

		public TraitUsageExpression Reduced {
			get {return this;}
		}

		public void addUser(TraitUser aUser) {
			if (users == null) users = newUserSet();
			users.Add(aUser);
		}

		public void removeUser(TraitUser aUser) {
			if (users == null) return;
			users.Remove(aUser);
			if (users.Count < 1) users = null;
		}

		public TraitUsageExpression combinedWith(TraitUsageExpression operand) {
			return operand.combinedWithTrait(this);
		}

		public TraitUsageExpression combinedWithTrait(Trait operand) {
			return this == operand ? this : newComposition().combinedWith(operand).combinedWithTrait(this);
		}

		public TraitUsageExpression combinedWithTransformation(ESTraitTransformation operand) {
			return this == operand.IdentityObject ? operand : newComposition().combinedWithTransformation(operand).combinedWithTrait(this);
		}

		public TraitUsageExpression combinedWithComposition(ESTraitComposition operand) {
			return operand.combinedWithTrait(this);
		}

		public TraitUsageExpression excluding(ESSymbol selector) {
			return asTransformation().excluding(selector);
		}

		public TraitUsageExpression aliasing(ESSymbol sourceSelector, ESSymbol selectorAlias) {
			return asTransformation().aliasing(sourceSelector, selectorAlias);
		}

		public void handleSelectorAliasingCollision(ESSymbol sourceSelector, ESSymbol previousAlias, ESSymbol selectorAlias) {
			ObjectSpace.throwSelectorAliasingCollisionException(sourceSelector, previousAlias, selectorAlias);
		}

		public void methodConflictsDo(Action<ESSymbol, ESMethod> enumerator2) {
			// I have none
		}

		public void hostSystemMethodConflictsDo(Action<long, String, ESMethod> enumerator3) {
			// I have none
		}

		public void excludedSelectorsDo(Action<ESSymbol> enumerator1) {
			// I have none
		}

		public void aliasedSelectorsDo(Action<ESSymbol, ESSymbol> enumerator2) {
			// I have none
		}

		#endregion

		public override T valueBy<T>(Operation<T> operation) {
		    return operation.applyToBehavioralTrait(this);
		}

		public new class Primitives : ESBehavioralObject.Primitives {

			protected override void bindToObjectSpace() {
				domainClass = objectSpace.TraitBehaviorClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.BehavioralTrait;}
			}

			#region Primitive Definitions

			public Object _identityObject_(Object receiver) {
				return ((ESBehavioralTrait)receiver).IdentityObject;
			}

			public Object _reduced_(Object receiver) {
				return ((ESBehavioralTrait)receiver).Reduced;
			}

			public Object _addUser_(Object receiver, Object user) {
				((ESBehavioralTrait)receiver).addUser((TraitUser)user);
				return receiver;
			}

			public Object _removeUser_(Object receiver, Object user) {
				((ESBehavioralTrait)receiver).removeUser((TraitUser)user);
				return receiver;
			}

			public Object _combinedWith_(Object receiver, Object usageExpression) {
				return ((ESBehavioralTrait)receiver).combinedWith((TraitUsageExpression)usageExpression);
				return receiver;
			}

			public Object _combinedWithTrait_(Object receiver, Object trait) {
				return ((ESBehavioralTrait)receiver).combinedWithTrait((Trait)trait);
				
			}

			public Object _combinedWithTransformation_(Object receiver, Object traitTransformation) {
				return ((ESBehavioralTrait)receiver).combinedWithTransformation((ESTraitTransformation)traitTransformation);
			}

			public Object _combinedWithComposition_(Object receiver, Object traitComposition) {
				return ((ESBehavioralTrait)receiver).combinedWithComposition((ESTraitComposition)traitComposition);
			}

			public Object _excluding_(Object receiverObject, Object selectorObject) {
				var receiver = (ESBehavioralTrait)receiverObject;
				var selector = objectSpace.asESSymbol(selectorObject);
				return receiver.excluding(selector);
			}

			public Object _excludingAll_(Object receiverObject, Object selectorExclusionSpec) {
				var receiver = (ESBehavioralTrait)receiverObject;
				var selector = selectorExclusionSpec as ESSymbol;
				if (selector != null) return receiver.excluding(selector);
				var selectorArray = objectSpace.asESArray(selectorExclusionSpec);
				var slots = selectorArray.IndexedSlots;
				TraitUsageExpression usage = receiver;
				for (var i = 0; i < slots.Length; i++) {
					selector = objectSpace.asESSymbol(slots[i]);
					usage = usage.excluding(selector);
				}
				return usage;
			}

			public Object _aliasing_(Object receiverObject, Object sourceSelector, Object selectorAlias) {
				var receiver = (ESBehavioralTrait)receiverObject;
				return receiver.aliasing(objectSpace.asESSymbol(sourceSelector), objectSpace.asESSymbol(selectorAlias));
			}

			public Object _aliasingAll_(Object receiverObject, Object selectorAliasingSpec) {
				var receiver = (ESBehavioralTrait)receiverObject;
				var selectorArray = objectSpace.asESArray(selectorAliasingSpec);
				var slots = selectorArray.IndexedSlots;
				TraitUsageExpression usage = receiver;
				for (var i = 0; i < slots.Length; i++) {
					var association = (ESAssociation)slots[i];
					usage = usage.aliasing(objectSpace.asESSymbol(association.Key), objectSpace.asESSymbol(association.Value));
				}
				return usage;
			}

			public Object _excludedSelectorsDo_(Object receiver, Object enumerator1) {
				var functor1 = asFunctor1(enumerator1);
				((ESBehavioralTrait)receiver).excludedSelectorsDo(selector => functor1(selector));
				return receiver;
			}

			public Object _aliasedSelectorsDo_(Object receiver, Object enumerator2) {
				var functor2 = asFunctor2(enumerator2);
				((ESBehavioralTrait)receiver).aliasedSelectorsDo((sourceSelector, selectorAlias) => functor2(sourceSelector, selectorAlias));
				return receiver;
			}

			public Object _methodConflictsDo_(Object receiver, Object enumerator2) {
				var functor2 = asFunctor2(enumerator2);
				((ESBehavioralTrait)receiver).methodConflictsDo((selector, method) => functor2(selector, method));
				return receiver;
			}

			public Object _hostSystemMethodConflictsDo_(Object receiver, Object enumerator3) {
				var functor3 = asFunctor3(enumerator3);
				((ESBehavioralTrait)receiver).hostSystemMethodConflictsDo((arity, selector, method) => functor3(arity, selector, method));
				return receiver;
			}

			#endregion

			public override void publishCanonicalPrimitives() {

				base.publishCanonicalPrimitives();

				publishPrimitive("identityObject",					new FuncNs.Func<Object, Object>(_identityObject_));
				publishPrimitive("reduced",						new FuncNs.Func<Object, Object>(_reduced_));
				publishPrimitive("addUser:",						new FuncNs.Func<Object, Object, Object>(_addUser_));
				publishPrimitive("removeUser:",						new FuncNs.Func<Object, Object, Object>(_removeUser_));
				publishPrimitive("+",							new FuncNs.Func<Object, Object, Object>(_combinedWith_));
				publishPrimitive("combinedWithTrait:",					new FuncNs.Func<Object, Object, Object>(_combinedWithTrait_));
				publishPrimitive("combinedWithTransformation:",				new FuncNs.Func<Object, Object, Object>(_combinedWithTransformation_));
				publishPrimitive("combinedWithComposition:",				new FuncNs.Func<Object, Object, Object>(_combinedWithComposition_));
				publishPrimitive("excluding:",						new FuncNs.Func<Object, Object, Object>(_excluding_));
				publishPrimitive("-",							new FuncNs.Func<Object, Object, Object>(_excludingAll_));
				publishPrimitive("aliasing:as:",					new FuncNs.Func<Object, Object, Object, Object>(_aliasing_));
				publishPrimitive("@",							new FuncNs.Func<Object, Object, Object>(_aliasingAll_));
				publishPrimitive("excludedSelectorsDo:",				new FuncNs.Func<Object, Object, Object>(_excludedSelectorsDo_));
				publishPrimitive("aliasedSelectorsDo:",					new FuncNs.Func<Object, Object, Object>(_aliasedSelectorsDo_));
				publishPrimitive("methodConflictsDo:",					new FuncNs.Func<Object, Object, Object>(_methodConflictsDo_));
				publishPrimitive("hostSystemMethodConflictsDo:",			new FuncNs.Func<Object, Object, Object>(_hostSystemMethodConflictsDo_));

			}

		}

	}

	public class ESInstanceTrait : ESBehavioralTrait {

		protected ESClassTrait classTrait;

		public ESInstanceTrait(ESBehavior esClass) : base(esClass) {
			setClassTrait(esClass.ObjectSpace.newClassTrait());
		}
			
		public ESInstanceTrait(ESBehavior metaClass, ESSymbol name) : this(metaClass) {
			setName(name);
		}

		public ESClassTrait ClassTrait {
			get {return classTrait;}
			set {setClassTrait(value);}
		}

		protected void setClassTrait(ESClassTrait newClassTrait) {
			if (classTrait == newClassTrait) return;
			if (classTrait != null) unbindFromClassTrait();
			classTrait = newClassTrait;
			if (classTrait != null) bindToClassTrait();
		}

		protected void unbindFromClassTrait() {
			classTrait.InstanceTrait = null;
		}

		protected void bindToClassTrait() {
			classTrait.InstanceTrait = this;
		}

		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.InstanceTrait;}
		}

		public override T valueBy<T>(Operation<T> operation) {
		    return operation.applyToInstanceTrait(this);
		}

		public new class Primitives : PrimitiveDomain {

			protected override void bindToObjectSpace() {
				domainClass = objectSpace.TraitClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.InstanceTrait;}
			}

			#region Primitive Definitions

			#endregion

			public override void publishCanonicalPrimitives() {

				// publishPrimitive("superclass:",						new FuncNs.Func<Object, Object, Object>(_setSuperclass_));

			}

		}

	}

	public class ESClassTrait : ESBehavioralTrait {

		protected ESInstanceTrait instanceTrait;

		public ESClassTrait(ESBehavior esClass) : base(esClass) {
		}

		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.ClassTrait;}
		}
		public ESInstanceTrait InstanceTrait {
			get {return instanceTrait;}
			set {setInstanceTrait(value);}
		}

		protected void setInstanceTrait(ESInstanceTrait newInstanceTrait) {
			if (instanceTrait == newInstanceTrait) return;
			if (instanceTrait != null) unbindFromInstanceTrait();
			instanceTrait = newInstanceTrait;
			if (instanceTrait != null) bindToInstanceTrait();
		}

		protected void unbindFromInstanceTrait() {
			instanceTrait.ClassTrait = null;
		}

		protected void bindToInstanceTrait() {
			instanceTrait.ClassTrait = this;
		}

		public override T valueBy<T>(Operation<T> operation) {
		    return operation.applyToClassTrait(this);
		}

		public new class Primitives : PrimitiveDomain {

			protected override void bindToObjectSpace() {
				domainClass = objectSpace.ClassTraitClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.ClassTrait;}
			}

			#region Primitive Definitions

			#endregion

			public override void publishCanonicalPrimitives() {

				// publishPrimitive("superclass:",						new FuncNs.Func<Object, Object, Object>(_setSuperclass_));

			}

		}

	}

	public abstract class ESAbstractTraitUsageExpression : ESNamedSlotsObject, TraitUsageExpression {
		
		protected ESAbstractTraitUsageExpression(ESBehavior esClass) : base(esClass) {
		}

		protected HashSet<ESSymbol> newSelectorSet() {
			return new HashSet<ESSymbol>(new SymbolIdentityComparator());
		}

		protected ESTraitComposition newComposition() {
			return ObjectSpace.newTraitComposition();
		}

		public abstract TraitUsageExpression IdentityObject {get;}
		public abstract ESObjectSpace  ObjectSpace {get;}

		public abstract TraitUsageExpression Reduced {get;}

		public abstract void addUser(TraitUser aUser);
		public abstract void removeUser(TraitUser aUser);

		public abstract void invalidateUsedTraits(TraitUsageExpression source);

		public abstract TraitUsageExpression combinedWith(TraitUsageExpression operand);
		public abstract TraitUsageExpression combinedWithTrait(Trait operand);
		public abstract TraitUsageExpression combinedWithTransformation(ESTraitTransformation operand);
		public abstract TraitUsageExpression combinedWithComposition(ESTraitComposition operand);
		public abstract TraitUsageExpression excluding(ESSymbol selector);
		public abstract TraitUsageExpression aliasing(ESSymbol sourceSelector, ESSymbol selectorAlias);
		public abstract void withUnimplementedMessagesSentToSelfDo(Action<ESSymbol> enumerator1);
		public abstract void methodConflictsDo(Action<ESSymbol, ESMethod> enumerator2);
		public abstract void hostSystemMethodConflictsDo(Action<long, String, ESMethod> enumerator3);

		
		public abstract bool UsesTraits {get;}
		public abstract TraitUsageExpression TraitUsage {get;}

		public TraitUser uses(TraitUsageExpression newUsedTraits) {
			return newUsedTraits;
		}

		public abstract void usedTraitsDo(Action<TraitUsageExpression> enumerator1);
		public abstract void withUsedTraitUnimplementedMessagesSentToSelfDo(Action<ESSymbol> enumerator1);
		public abstract void usedTraitExcludedSelectorsDo(Action<ESSymbol> enumerator1);
		public abstract void usedTraitAliasedSelectorsDo(Action<ESSymbol, ESSymbol> enumerator2);
		public abstract void usedTraitMethodConflictsDo(Action<ESSymbol, ESMethod> enumerator2);
		public abstract void usedTraitHostSystemMethodConflictsDo(Action<long, String, ESMethod> enumerator3);


		public void selectorsDo(FuncNs.Func<object, object> enumerator1) {
			selectorsDo(enumerator1, null);
		}

		public abstract void selectorsDo(FuncNs.Func<object, object> enumerator1, HashSet<ESSymbol> exclusionSet);

		public void selectorsAndMethodsDo(FuncNs.Func<object, object, object> enumerator2) {
			selectorsAndMethodsDo(enumerator2, null);
		}

		public abstract void selectorsAndMethodsDo(FuncNs.Func<object, object, object> enumerator2, HashSet<ESSymbol> exclusionSet);
		public abstract HashSet<ESSymbol> selectors();
		public abstract bool includesSelector(ESSymbol selector);


		public void allSelectorsDo(FuncNs.Func<object, object> enumerator1) {
			selectorsDo(enumerator1, null);
		}

		public void allSelectorsDo(FuncNs.Func<object, object> enumerator1, HashSet<ESSymbol> exclusionSet) {
			selectorsDo(enumerator1, exclusionSet);
		}

		public void allSelectorsAndMethodsDo(FuncNs.Func<object, object, object> enumerator2) {
			selectorsAndMethodsDo(enumerator2, null);
		}

		public void allSelectorsAndMethodsDo(FuncNs.Func<object, object, object> enumerator2, HashSet<ESSymbol> exclusionSet) {
			selectorsAndMethodsDo(enumerator2, exclusionSet);
		}

		public HashSet<ESSymbol> allSelectors() {
			return selectors();
		}

		public abstract ESMethod compiledMethodAt(ESSymbol selector);

		public bool canUnderstand(ESSymbol selector) {
			return includesSelector(selector);
		}


		public void systemSelectorsDo(FuncNs.Func<object, object, object> enumerator2) {
			systemSelectorsDo(enumerator2, null);
		}

		public abstract void systemSelectorsDo(FuncNs.Func<object, object, object> enumerator2, HashSet<String> exclusionSet);


		public void systemSelectorsAndMethodsDo(FuncNs.Func<object, object, object, object> enumerator3) {
			systemSelectorsAndMethodsDo(enumerator3, null);
		}

		public abstract void systemSelectorsAndMethodsDo(FuncNs.Func<object, object, object, object> enumerator3, HashSet<String> exclusionSet);
		public abstract bool includesSystemSelector(string systemSelector, long numArgs);

		public bool canUnderstandSystemMessage(String systemSelector, long numArgs) {
			return includesSystemSelector(systemSelector, numArgs);
		}

		public abstract ESMethod compiledMethodAtSystemSelector(string systemSelector, long numArgs);

		public virtual void handleSelectorAliasingCollision(ESSymbol sourceSelector, ESSymbol previousAlias, ESSymbol selectorAlias) {
			ObjectSpace.throwSelectorAliasingCollisionException(sourceSelector, previousAlias, selectorAlias);
		}

		public virtual void excludedSelectorsDo(Action<ESSymbol> enumerator1) {
		}

		public virtual void aliasedSelectorsDo(Action<ESSymbol, ESSymbol> enumerator2) {
		}

		public new abstract class Primitives : PrimitiveDomain {

			#region Primitive Definitions

			public Object _objectSpace_(Object receiver) {
				return ((ESAbstractTraitUsageExpression)receiver).ObjectSpace;
			}

			public Object _identityObject_(Object receiver) {
				return ((ESAbstractTraitUsageExpression)receiver).IdentityObject;
			}

			public Object _reduced_(Object receiver) {
				return ((ESAbstractTraitUsageExpression)receiver).Reduced;
			}

			public Object _addUser_(Object receiver, Object user) {
				((ESAbstractTraitUsageExpression)receiver).addUser((TraitUser)user);
				return receiver;
			}

			public Object _removeUser_(Object receiver, Object user) {
				((ESAbstractTraitUsageExpression)receiver).removeUser((TraitUser)user);
				return receiver;
			}

			public Object _combinedWith_(Object receiver, Object usageExpression) {
				return ((ESAbstractTraitUsageExpression)receiver).combinedWith((TraitUsageExpression)usageExpression);
				return receiver;
			}

			public Object _combinedWithTrait_(Object receiver, Object trait) {
				return ((ESAbstractTraitUsageExpression)receiver).combinedWithTrait((Trait)trait);
				
			}

			public Object _combinedWithTransformation_(Object receiver, Object traitTransformation) {
				return ((ESAbstractTraitUsageExpression)receiver).combinedWithTransformation((ESTraitTransformation)traitTransformation);
			}

			public Object _combinedWithComposition_(Object receiver, Object traitComposition) {
				return ((ESAbstractTraitUsageExpression)receiver).combinedWithComposition((ESTraitComposition)traitComposition);
			}

			public Object _excluding_(Object receiverObject, Object selectorObject) {
				var receiver = (ESAbstractTraitUsageExpression)receiverObject;
				var selector = objectSpace.asESSymbol(selectorObject);
				return receiver.excluding(selector);
			}

			public Object _excludingAll_(Object receiverObject, Object selectorExclusionSpec) {
				var receiver = (ESAbstractTraitUsageExpression)receiverObject;
				var selector = selectorExclusionSpec as ESSymbol;
				if (selector != null) return receiver.excluding(selector);
				var selectorArray = objectSpace.asESArray(selectorExclusionSpec);
				var slots = selectorArray.IndexedSlots;
				TraitUsageExpression usage = receiver;
				for (var i = 0; i < slots.Length; i++) {
					selector = objectSpace.asESSymbol(slots[i]);
					usage = usage.excluding(selector);
				}
				return usage;
			}

			public Object _aliasing_(Object receiverObject, Object sourceSelector, Object selectorAlias) {
				var receiver = (ESAbstractTraitUsageExpression)receiverObject;
				return receiver.aliasing(objectSpace.asESSymbol(sourceSelector), objectSpace.asESSymbol(selectorAlias));
			}

			public Object _aliasingAll_(Object receiverObject, Object selectorAliasingSpec) {
				var receiver = (ESAbstractTraitUsageExpression)receiverObject;
				var selectorArray = objectSpace.asESArray(selectorAliasingSpec);
				var slots = selectorArray.IndexedSlots;
				TraitUsageExpression usage = receiver;
				for (var i = 0; i < slots.Length; i++) {
					var association = (ESAssociation)slots[i];
					usage = usage.aliasing(objectSpace.asESSymbol(association.Key), objectSpace.asESSymbol(association.Value));
				}
				return usage;
			}

			public Object _excludedSelectorsDo_(Object receiver, Object enumerator1) {
				var functor1 = asFunctor1(enumerator1);
				((ESAbstractTraitUsageExpression)receiver).excludedSelectorsDo(selector => functor1(selector));
				return receiver;
			}

			public Object _aliasedSelectorsDo_(Object receiver, Object enumerator2) {
				var functor2 = asFunctor2(enumerator2);
				((ESAbstractTraitUsageExpression)receiver).aliasedSelectorsDo((sourceSelector, selectorAlias) => functor2(sourceSelector, selectorAlias));
				return receiver;
			}

			public Object _methodConflictsDo_(Object receiver, Object enumerator2) {
				var functor2 = asFunctor2(enumerator2);
				((ESAbstractTraitUsageExpression)receiver).methodConflictsDo((selector, method) => functor2(selector, method));
				return receiver;
			}

			public Object _hostSystemMethodConflictsDo_(Object receiver, Object enumerator3) {
				var functor3 = asFunctor3(enumerator3);
				((ESAbstractTraitUsageExpression)receiver).hostSystemMethodConflictsDo((arity, selector, method) => functor3(arity, selector, method));
				return receiver;
			}


			public Object _usesTraits_(Object receiver) {
				return ((ESAbstractTraitUsageExpression)receiver).UsesTraits;
			}

			public Object _traitUsage_(Object receiver) {
				return ((ESAbstractTraitUsageExpression)receiver).TraitUsage;
			}

			public Object _invalidateUsedTraits_(Object receiver, Object source) {
				((ESAbstractTraitUsageExpression)receiver).invalidateUsedTraits((TraitUsageExpression)source);
				return receiver;
			}

			public Object _usedTraitsDo_(Object receiver, Object enumerator1) {
				var functor1 = asFunctor1(enumerator1);
				((ESAbstractTraitUsageExpression)receiver).usedTraitsDo(trait => functor1(trait));
				return receiver;
			}

			public Object _withUsedTraitUnimplementedMessagesSentToSelfDo_(Object receiver, Object enumerator1) {
				var functor1 = asFunctor1(enumerator1);
				((ESAbstractTraitUsageExpression)receiver).withUsedTraitUnimplementedMessagesSentToSelfDo(trait => functor1(trait));
				return receiver;
			}

			public Object _usedTraitMethodConflictsDo_(Object receiver, Object enumerator2) {
				var functor2 = asFunctor2(enumerator2);
				((ESAbstractTraitUsageExpression)receiver).usedTraitMethodConflictsDo((selector, method) => functor2(selector, method));
				return receiver;
			}

			public Object _usedTraitHostSystemMethodConflictsDo_(Object receiver, Object enumerator3) {
				var functor3 = asFunctor3(enumerator3);
				((ESAbstractTraitUsageExpression)receiver).usedTraitHostSystemMethodConflictsDo((arity, selector, method) => functor3(arity, selector, method));
				return receiver;
			}

			public Object _usedTraitExcludedSelectorsDo_(Object receiver, Object enumerator1) {
				var functor1 = asFunctor1(enumerator1);
				((ESAbstractTraitUsageExpression)receiver).usedTraitExcludedSelectorsDo(selector => functor1(selector));
				return receiver;
			}

			public Object _usedTraitAliasedSelectorsDo_(Object receiver, Object enumerator2) {
				var functor2 = asFunctor2(enumerator2);
				((ESAbstractTraitUsageExpression)receiver).usedTraitAliasedSelectorsDo((originalSelector, selectorAlias) => functor2(originalSelector, selectorAlias));
				return receiver;
			}
		

			public Object _selectorsDo_(Object receiver, Object enumerator1) {
				((ESAbstractTraitUsageExpression)receiver).selectorsDo(asFunctor1(enumerator1));
				return receiver;
			}

			public Object _selectorsAndMethodsDo_(Object receiver, Object enumerator2) {
				((ESAbstractTraitUsageExpression)receiver).selectorsAndMethodsDo(asFunctor2(enumerator2));
				return receiver;
			}

			public Object _selectors_(Object receiver) {
				return ((ESAbstractTraitUsageExpression)receiver).selectors();
			}

			public Object _includesSelector_(Object receiver, Object selector) {
				return ((ESAbstractTraitUsageExpression)receiver).includesSelector(objectSpace.asESSymbol(selector));
			}

			public Object _allSelectorsDo_(Object receiver, Object enumerator1) {
				((ESAbstractTraitUsageExpression)receiver).allSelectorsDo(asFunctor1(enumerator1));
				return receiver;
			}

			public Object _allSelectorsAndMethodsDo_(Object receiver, Object enumerator2) {
				((ESAbstractTraitUsageExpression)receiver).allSelectorsAndMethodsDo(asFunctor2(enumerator2));
				return receiver;
			}

			public Object _allSelectors_(Object receiver) {
				return ((ESAbstractTraitUsageExpression)receiver).allSelectors();
			}

			public Object _canUnderstand_(Object receiver, Object selector) {
				return ((ESAbstractTraitUsageExpression)receiver).canUnderstand(objectSpace.asESSymbol(selector));
			}

			public Object _compiledMethodAt_(Object receiver, Object selector) {
				return ((ESAbstractTraitUsageExpression)receiver).compiledMethodAt(objectSpace.asESSymbol(selector));
			}

			public Object _withUnimplementedMessagesSentToSelfDo_(Object receiver, Object enumerator1) {
				var theReceiver = (ESAbstractTraitUsageExpression)receiver;
				var functor1 = asFunctor1(enumerator1);
				theReceiver.withUnimplementedMessagesSentToSelfDo(selector => functor1(selector));
				return receiver;
			}

			public Object _systemSelectorsDo_(Object receiver, Object enumerator2) {
				((ESBehavioralObject)receiver).systemSelectorsDo(asFunctor2(enumerator2));
				return receiver;
			}

			public Object _systemSelectorsAndMethodsDo_(Object receiver, Object enumerator3) {
				((ESBehavioralObject)receiver).systemSelectorsAndMethodsDo(asFunctor3(enumerator3));
				return receiver;
			}

			public Object _systemSelectorCount_(Object receiver) {
				return ((ESBehavioralObject)receiver).systemSelectorCount();
			}

			public Object _includesSystemSelector_(Object receiver, Object systemSelector, Object numArgs) {
				return ((ESBehavioralObject)receiver).includesSystemSelector(asHostString(systemSelector), asHostLong(numArgs));
			}


			public Object _canUnderstandSystemMessage_(Object receiver, Object selector, Object numArgs) {
				var arity = (long)numArgs;
				return ((ESBehavioralObject)receiver).canUnderstandSystemMessage(objectSpace.asESSymbol(selector), arity);
			}

			public Object _compiledMethodAtSystemSelector_(Object receiver, Object systemSelector, Object numArgs) {
				return ((ESBehavioralObject)receiver).compiledMethodAtSystemSelector(asHostString(systemSelector), asHostLong(numArgs));
			}

			#endregion

			public override void publishCanonicalPrimitives() {

				publishPrimitive("objectSpace",						new FuncNs.Func<Object, Object>(_objectSpace_));
				publishPrimitive("identityObject",					new FuncNs.Func<Object, Object>(_identityObject_));
				publishPrimitive("reduced",						new FuncNs.Func<Object, Object>(_reduced_));
				publishPrimitive("addUser:",						new FuncNs.Func<Object, Object, Object>(_addUser_));
				publishPrimitive("removeUser:",						new FuncNs.Func<Object, Object, Object>(_removeUser_));
				publishPrimitive("+",							new FuncNs.Func<Object, Object, Object>(_combinedWith_));
				publishPrimitive("combinedWithTrait:",					new FuncNs.Func<Object, Object, Object>(_combinedWithTrait_));
				publishPrimitive("combinedWithTransformation:",				new FuncNs.Func<Object, Object, Object>(_combinedWithTransformation_));
				publishPrimitive("combinedWithComposition:",				new FuncNs.Func<Object, Object, Object>(_combinedWithComposition_));
				publishPrimitive("excluding:",						new FuncNs.Func<Object, Object, Object>(_excluding_));
				publishPrimitive("-",							new FuncNs.Func<Object, Object, Object>(_excludingAll_));
				publishPrimitive("aliasing:as:",					new FuncNs.Func<Object, Object, Object, Object>(_aliasing_));
				publishPrimitive("@",							new FuncNs.Func<Object, Object, Object>(_aliasingAll_));
				publishPrimitive("excludedSelectorsDo:",				new FuncNs.Func<Object, Object, Object>(_excludedSelectorsDo_));
				publishPrimitive("aliasedSelectorsDo:",					new FuncNs.Func<Object, Object, Object>(_aliasedSelectorsDo_));
				publishPrimitive("methodConflictsDo:",					new FuncNs.Func<Object, Object, Object>(_methodConflictsDo_));
				publishPrimitive("hostSystemMethodConflictsDo:",			new FuncNs.Func<Object, Object, Object>(_hostSystemMethodConflictsDo_));

				publishPrimitive("usesTraits",						new FuncNs.Func<Object, Object>(_usesTraits_));
				publishPrimitive("traitUsage",						new FuncNs.Func<Object, Object>(_traitUsage_));
				publishPrimitive("invalidateUsedTraitsFrom:",				new FuncNs.Func<Object, Object, Object>(_invalidateUsedTraits_));
				publishPrimitive("usedTraitsDo:",					new FuncNs.Func<Object, Object, Object>(_usedTraitsDo_));
				publishPrimitive("withUsedTraitUnimplementedMessagesSentToSelfDo:",	new FuncNs.Func<Object, Object, Object>(_withUsedTraitUnimplementedMessagesSentToSelfDo_));
				publishPrimitive("usedTraitMethodConflictsDo:",				new FuncNs.Func<Object, Object, Object>(_usedTraitMethodConflictsDo_));
				publishPrimitive("usedTraitHostSystemMethodConflictsDo:",		new FuncNs.Func<Object, Object, Object>(_usedTraitHostSystemMethodConflictsDo_));
				publishPrimitive("usedTraitExcludedSelectorsDo:",			new FuncNs.Func<Object, Object, Object>(_usedTraitExcludedSelectorsDo_));
				publishPrimitive("usedTraitAliasedSelectorsDo:",			new FuncNs.Func<Object, Object, Object>(_usedTraitAliasedSelectorsDo_));

				publishPrimitive("selectorsDo:",					new FuncNs.Func<Object, Object, Object>(_selectorsDo_));
				publishPrimitive("selectorsAndMethodsDo:",				new FuncNs.Func<Object, Object, Object>(_selectorsAndMethodsDo_));
				publishPrimitive("selectors",						new FuncNs.Func<Object, Object>(_selectors_));
				publishPrimitive("includesSelector:",					new FuncNs.Func<Object, Object, Object>(_includesSelector_));

				publishPrimitive("allSelectorsDo:",					new FuncNs.Func<Object, Object, Object>(_allSelectorsDo_));
				publishPrimitive("allSelectorsAndMethodsDo:",				new FuncNs.Func<Object, Object, Object>(_allSelectorsAndMethodsDo_));
				publishPrimitive("allSelectors",					new FuncNs.Func<Object, Object>(_allSelectors_));
				publishPrimitive("canUnderstand:",					new FuncNs.Func<Object, Object, Object>(_canUnderstand_));
				publishPrimitive("compiledMethodAt:",					new FuncNs.Func<Object, Object, Object>(_compiledMethodAt_));

				publishPrimitive("systemSelectorsDo:",					new FuncNs.Func<Object, Object, Object>(_systemSelectorsDo_));
				publishPrimitive("systemSelectorsAndMethodsDo:",			new FuncNs.Func<Object, Object, Object>(_systemSelectorsAndMethodsDo_));
				publishPrimitive("systemSelectorCount",					new FuncNs.Func<Object, Object>(_systemSelectorCount_));
				publishPrimitive("includesSystemSelector:numArgs:",			new FuncNs.Func<Object, Object, Object, Object>(_includesSystemSelector_));

				publishPrimitive("canUnderstandSystemMessage:",				new FuncNs.Func<Object, Object, Object, Object>(_canUnderstandSystemMessage_));
				publishPrimitive("compiledMethodAtSystemSelector:numArgs:",		new FuncNs.Func<Object, Object, Object, Object>(_compiledMethodAtSystemSelector_));

			}

		}

	}

	public class ESTraitTransformation : ESAbstractTraitUsageExpression {

		protected Trait subject;
		protected HashSet<ESSymbol> excludedSelectors;
		protected Dictionary<ESSymbol, ESSymbol> selectorAliases;

		public ESTraitTransformation(ESBehavior esClass, Trait subject) : base(esClass) {
			this.subject = subject;
		}
		
		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.TraitTransformation;}
		}

		#region Internal operations

		protected Dictionary<ESSymbol, ESSymbol> newSelectorAliases() {
			return new Dictionary<ESSymbol, ESSymbol>(new SymbolIdentityComparator());
		}

		protected bool getTargetSelector(ESSymbol sourceSelector, out ESSymbol targetSelector) {
			if (selectorAliases == null || !selectorAliases.TryGetValue(sourceSelector, out targetSelector)) targetSelector = sourceSelector;
			if (excludedSelectors == null) return true;
			return !excludedSelectors.Contains(targetSelector);
		}

		#endregion

		#region TraitUser protocol

		public override bool UsesTraits {
			get {return subject.UsesTraits;}
		}

		public override TraitUsageExpression TraitUsage {
			get {return subject.TraitUsage;}
		}

		public override void invalidateUsedTraits(TraitUsageExpression source) {
			subject.invalidateUsedTraits(source);
		}

		public override void usedTraitsDo(Action<TraitUsageExpression> enumerator1) {
			subject.usedTraitsDo(enumerator1);
		}

		public override void withUsedTraitUnimplementedMessagesSentToSelfDo(Action<ESSymbol> enumerator1) {
			subject.withUsedTraitUnimplementedMessagesSentToSelfDo(enumerator1);
		}

		public override void usedTraitMethodConflictsDo(Action<ESSymbol, ESMethod> enumerator2) {
			subject.usedTraitMethodConflictsDo(enumerator2);
		}

		public override void usedTraitHostSystemMethodConflictsDo(Action<long, String, ESMethod> enumerator3) {
			subject.usedTraitHostSystemMethodConflictsDo(enumerator3);
		}

		public override void usedTraitExcludedSelectorsDo(Action<ESSymbol> enumerator1) {
			excludedSelectorsDo(enumerator1);
			subject.usedTraitExcludedSelectorsDo(enumerator1);
		}

		public override void usedTraitAliasedSelectorsDo(Action<ESSymbol, ESSymbol> enumerator2) {
			aliasedSelectorsDo(enumerator2);
			subject.usedTraitAliasedSelectorsDo(enumerator2);
		}

		#endregion

		#region TraitTransformation protocol

		public Trait Subject {
			get {return subject;}
		}

		#endregion

		#region TraitUsageExpression protocol

		public override TraitUsageExpression IdentityObject {
			get {return subject.IdentityObject;}
		}

		public override ESObjectSpace ObjectSpace {
			get {return Class.ObjectSpace;}
		}

		public override TraitUsageExpression Reduced {
			get {return newComposition().combinedWithTransformation(this);}
		}

		public override void addUser(TraitUser aUser) {
			subject.addUser(aUser);
		}

		public override void removeUser(TraitUser aUser) {
			subject.removeUser(aUser);
		}

		public override TraitUsageExpression combinedWith(TraitUsageExpression operand) {
			return operand.combinedWithTransformation(this);
		}

		public override TraitUsageExpression combinedWithTrait(Trait operand) {
			return subject.IdentityObject == operand.IdentityObject ?
				this :
				newComposition().combinedWithTrait(operand).combinedWithTransformation(this);
		}

		public override TraitUsageExpression combinedWithTransformation(ESTraitTransformation operand) {
			if (this == operand) return this;
			if (subject.IdentityObject == operand.IdentityObject) { 
				usedTraitExcludedSelectorsDo(selector => operand.excluding(selector));
				usedTraitAliasedSelectorsDo((sourceSelector, alias) => operand.aliasing(sourceSelector, alias));
				return operand;
			} else {
				return newComposition().combinedWithTransformation(operand).combinedWithTransformation(this);
			}
		}

		public override TraitUsageExpression combinedWithComposition(ESTraitComposition operand) {
			return operand.combinedWithTransformation(this);
		}

		public override TraitUsageExpression excluding(ESSymbol selector) {
			if (excludedSelectors == null) excludedSelectors = newSelectorSet();
			excludedSelectors.Add(selector);
			return this;
		}

		public override TraitUsageExpression aliasing(ESSymbol sourceSelector, ESSymbol selectorAlias) {
			if (sourceSelector.NumArgs != selectorAlias.NumArgs) ObjectSpace.throwSelectorAliasingArityMismatchException(sourceSelector,  selectorAlias);
			if (selectorAliases == null) selectorAliases = newSelectorAliases();
			ESSymbol previousAlias;
			if (selectorAliases.TryGetValue(sourceSelector, out previousAlias)) {
				if (selectorAlias == previousAlias) {
					return this;
				} else {
					subject.handleSelectorAliasingCollision(sourceSelector, previousAlias, selectorAlias);
				}
			}
			selectorAliases[sourceSelector] = selectorAlias;
			return this;
		}

		public override void handleSelectorAliasingCollision(ESSymbol sourceSelector, ESSymbol previousAlias, ESSymbol selectorAlias) {
			subject.handleSelectorAliasingCollision(sourceSelector, previousAlias, selectorAlias);
		}

		public override void withUnimplementedMessagesSentToSelfDo(Action<ESSymbol> enumerator1) {
			subject.withUnimplementedMessagesSentToSelfDo(enumerator1);
		}

		public override void methodConflictsDo(Action<ESSymbol, ESMethod> enumerator2) {
			subject.methodConflictsDo(enumerator2);
		}

		public override void hostSystemMethodConflictsDo(Action<long, String, ESMethod> enumerator3) {
			subject.hostSystemMethodConflictsDo(enumerator3);
		}

		public override void excludedSelectorsDo(Action<ESSymbol> enumerator1) {
			if (excludedSelectors != null) foreach (var symbol in excludedSelectors) enumerator1(symbol);
		}

		public override void aliasedSelectorsDo(Action<ESSymbol, ESSymbol> enumerator2) {
			if (selectorAliases != null) foreach (var kvp in selectorAliases) enumerator2(kvp.Key, kvp.Value);
		}

		#endregion

		#region Method accessing/enumeration protocol

		public override void selectorsDo(FuncNs.Func<object, object> enumerator1, HashSet<ESSymbol> exclusionSet) {
			subject.selectorsDo((Object sourceSelectorObject) => {
				var sourceSelector = (ESSymbol)sourceSelectorObject;
				ESSymbol targetSelector;
				if (getTargetSelector(sourceSelector, out targetSelector)) {
					if (exclusionSet == null) {
						enumerator1(targetSelector);
					} else if (!exclusionSet.Contains(targetSelector)) {
						enumerator1(targetSelector);
						exclusionSet.Add(targetSelector);
					}
				}
				return subject;
			}, null);
		}

		public override void selectorsAndMethodsDo(FuncNs.Func<object, object, object> enumerator2, HashSet<ESSymbol> exclusionSet) {
			subject.selectorsAndMethodsDo((Object sourceSelectorObject, Object methodObject) => {
				var sourceSelector = (ESSymbol)sourceSelectorObject;
				ESSymbol targetSelector;
				if (getTargetSelector(sourceSelector, out targetSelector)) {
					if (exclusionSet == null) {
						enumerator2(targetSelector, methodObject);
					} else if (!exclusionSet.Contains(targetSelector)) {	
						enumerator2(targetSelector, methodObject);
						exclusionSet.Add(targetSelector);
					}
				}
				return subject;
			}, null);
		}

		public override HashSet<ESSymbol> selectors() {
			var selectors = newSelectorSet();
			selectorsDo(selectorObject => selectors.Add((ESSymbol)selectorObject), null);
			return selectors;
		}

		public override bool includesSelector(ESSymbol sourceSelector) {
			ESSymbol targetSelector;
			return getTargetSelector(sourceSelector, out targetSelector) ? subject.includesSelector(targetSelector) : false;
		}

		public override ESMethod compiledMethodAt(ESSymbol sourceSelector) {
			ESSymbol targetSelector;
			return getTargetSelector(sourceSelector, out targetSelector) ? subject.compiledMethodAt(targetSelector) : null;
		}


		public override void systemSelectorsDo(FuncNs.Func<object, object, object> enumerator2, HashSet<String> exclusionSet) {
			subject.systemSelectorsDo(enumerator2, exclusionSet);
		}

		public override void systemSelectorsAndMethodsDo(FuncNs.Func<object, object, object, object> enumerator3, HashSet<String> exclusionSet) {
			subject.systemSelectorsAndMethodsDo(enumerator3, exclusionSet);
		}

		public override ESMethod compiledMethodAtSystemSelector(string systemSelector, long numArgs) {
			return subject.compiledMethodAtSystemSelector(systemSelector, numArgs);
		}

		public override bool includesSystemSelector(string systemSelector, long numArgs) {
			return subject.includesSystemSelector(systemSelector, numArgs);
		}

		#endregion

		public override T valueBy<T>(Operation<T> operation) {
		    return operation.applyToTraitTransformation(this);
		}

		public new class Primitives : ESAbstractTraitUsageExpression.Primitives {

			protected override void bindToObjectSpace() {
				domainClass = objectSpace.TraitTransformationClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.TraitTransformation;}
			}

			#region Primitive Definitions

			#endregion

			public override void publishCanonicalPrimitives() {

				base.publishCanonicalPrimitives();

				// publishPrimitive("superclass:",						new FuncNs.Func<Object, Object, Object>(_setSuperclass_));

			}

		}

	}

	public class ESTraitComposition : ESAbstractTraitUsageExpression {

		protected ESObjectSpace							objectSpace;
		protected Dictionary<TraitUsageExpression, TraitUsageExpression>	elements;
		protected bool								isReduced = false;
		protected IDictionary<ESSymbol, ESMethod> 				methodDictionary; 
		protected HashSet<ESSymbol>						unimplementedMessagesSentToSelf;
		protected IDictionary<ESSymbol, List<ESMethod>>				methodConflicts;
		protected IDictionary<long, IDictionary<String, ESMethod>>		hostSystemMethodDictionary; 
		protected IDictionary<long, IDictionary<String, List<ESMethod>>>	hostSystemMethodConflicts;

		public ESTraitComposition(ESBehavior esClass) : base(esClass) {
			objectSpace = esClass.ObjectSpace;
			elements = newElements();
		}
		
		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.TraitComposition;}
		}

		#region Internal operations

		protected void reset() {
			lock (this) { 
				elements = newElements();
				invalidateUsedTraits(this);
			}
		}

		protected Dictionary<TraitUsageExpression, TraitUsageExpression> newElements() {
			return new Dictionary<TraitUsageExpression, TraitUsageExpression>(new TraitExpressionIdentityComparator());
		}

		protected IDictionary<ESSymbol, List<ESMethod>> newConflictsDictionary() {
			return new Dictionary<ESSymbol, List<ESMethod>>(new SymbolIdentityComparator());
		}
		
		protected IDictionary<ESSymbol, ESMethod> newMethodDictionary() {
			return new Dictionary<ESSymbol, ESMethod>();
		}

		protected IDictionary<long, IDictionary<String, ESMethod>> newHostSystemMethodDictionary() {
			return new Dictionary<long, IDictionary<String, ESMethod>>();
		}

		protected IDictionary<long, IDictionary<String, List<ESMethod>>> newHostSystemMethodConflictsDictionary() {
			return new Dictionary<long, IDictionary<String, List<ESMethod>>>();
		}

		protected void composeMethodDictionary() {

			var newMethodDict = newMethodDictionary();
			var newConflicts = newConflictsDictionary();
			var allMessagesSentToSelf = newSelectorSet();
			
			foreach (var kvp in elements) {
				var element = kvp.Value;
				element.selectorsAndMethodsDo((selectorObject, methodObject) => {
					var selector = (ESSymbol)selectorObject;
					var newMethod = (ESMethod)methodObject;
					ESMethod currentMethod;
					if (newMethodDict.TryGetValue(selector, out currentMethod)) {
						if (newMethod.Identity == currentMethod.Identity) return null;
						List<ESMethod> conflictingMethods;
						if (!newConflicts.TryGetValue(selector, out conflictingMethods)) {
							conflictingMethods = new List<ESMethod>();
							conflictingMethods.Add(currentMethod);
							newConflicts[selector] = conflictingMethods;
						}
						conflictingMethods.Add(newMethod);
					} else { 
						newMethodDict[selector] = newMethod;
						var messagesSentToSelf = newMethod.MessagesSentToSelf;
						if (messagesSentToSelf != null) {
							foreach (var message in messagesSentToSelf) 
								allMessagesSentToSelf.Add(message);
						}
					}
					return null;
				}, null);
			}

			foreach (var kvp in newConflicts) newMethodDict.Remove(kvp.Key);
			foreach (var kvp in newMethodDict) allMessagesSentToSelf.Remove(kvp.Key);

			unimplementedMessagesSentToSelf = allMessagesSentToSelf;
			methodConflicts = newConflicts;
			methodDictionary = newMethodDict;

		}

		protected void composeHostSystemMethodDictionary() {

			var newMethodDict = newHostSystemMethodDictionary();
			var newConflicts = newHostSystemMethodConflictsDictionary();
			
			foreach (var kvp in elements) {
				var element = kvp.Value;
				element.systemSelectorsAndMethodsDo((selectorObject, arityObject, methodObject) => {
					var selector = (ESSymbol)selectorObject;
					var arity = (long)arityObject;
					var newMethod = (ESMethod)methodObject;
					IDictionary<String, ESMethod> arityDict;
					var arityDictHasElements = true;
					if (!newMethodDict.TryGetValue(arity, out arityDict)) {
						arityDict = new Dictionary<String, ESMethod>();
						newMethodDict[arity] = arityDict;
						arityDictHasElements = false;
					}
					ESMethod currentMethod;
					if (arityDictHasElements && arityDict.TryGetValue(selector, out currentMethod)) {
						if (newMethod.Identity == currentMethod.Identity) return null;
						IDictionary<String, List<ESMethod>> arityConflictsDict;
						if (!newConflicts.TryGetValue(arity, out arityConflictsDict)) {
							arityConflictsDict = new Dictionary<String, List<ESMethod>>();
							newConflicts[arity] = arityConflictsDict;
						}
						List<ESMethod> conflictingMethods;
						if (!arityConflictsDict.TryGetValue(selector, out conflictingMethods)) {
							conflictingMethods = new List<ESMethod>();
							conflictingMethods.Add(currentMethod);
							arityConflictsDict[selector] = conflictingMethods;
						}
						conflictingMethods.Add(newMethod);
					} else { 
						arityDict[selector] = newMethod;
					}
					return null;
				}, null);
			}

			foreach (var kvp in newConflicts) {
				var arity = kvp.Key;
				var arityConflictsDict = kvp.Value;
				var arityDict = newMethodDict[arity];
				foreach (var arityKvp in arityDict) { 
					arityDict.Remove(arityKvp.Key);
				}
				if (arityDict.Count < 1) newMethodDict.Remove(arity);
			}

			hostSystemMethodConflicts = newConflicts;
			hostSystemMethodDictionary = newMethodDict;

		}

		protected void reduce() {
			lock (this) { 
				composeMethodDictionary();
				composeHostSystemMethodDictionary();
				isReduced = true;
			}
		}

		protected bool IsReduced {
			get {return isReduced;}
		}

		#endregion

		#region TraitUser Protocol

		public override bool UsesTraits {
			get {return elements != null && elements.Count > 0;}
		}

		public override TraitUsageExpression TraitUsage {
			get {return UsesTraits ? this : null;}
		}

		public override void invalidateUsedTraits(TraitUsageExpression source) {
			isReduced = false;
			methodDictionary = null;
			hostSystemMethodDictionary = null;
			unimplementedMessagesSentToSelf = null;
			methodConflicts = null;
			hostSystemMethodConflicts = null;
		}

		public override void usedTraitsDo(Action<TraitUsageExpression> enumerator1) {
			foreach (var kvp in elements) enumerator1(kvp.Value);
		}

		public override void withUsedTraitUnimplementedMessagesSentToSelfDo(Action<ESSymbol> enumerator1) {
			withUnimplementedMessagesSentToSelfDo(enumerator1);
		}

		public override void usedTraitMethodConflictsDo(Action<ESSymbol, ESMethod> enumerator2) {
			methodConflictsDo(enumerator2);
		}

		public override void usedTraitHostSystemMethodConflictsDo(Action<long, String, ESMethod> enumerator3) {
			hostSystemMethodConflictsDo(enumerator3);
		}

		public override void usedTraitExcludedSelectorsDo(Action<ESSymbol> enumerator1) {
			var excludedSelectors = newSelectorSet();
			foreach (var kvp in elements) kvp.Value.excludedSelectorsDo(selector => excludedSelectors.Add(selector));
			foreach (var selector in excludedSelectors) enumerator1(selector);
		}

		public override void usedTraitAliasedSelectorsDo(Action<ESSymbol, ESSymbol> enumerator2) {
			var aliasedSelectors = new Dictionary<ESSymbol, HashSet<ESSymbol>>();
			foreach (var kvp in elements) kvp.Value.aliasedSelectorsDo((originalSelector, selectorAlias) => {
				HashSet<ESSymbol> selectorAliases;
				if (!aliasedSelectors.TryGetValue(originalSelector, out selectorAliases)) {
					selectorAliases = newSelectorSet();
					aliasedSelectors[originalSelector] = selectorAliases;
				}
				selectorAliases.Add(selectorAlias);
			});
			foreach (var kvp in aliasedSelectors) {
				var orignalSelector = kvp.Key;
				var selectorAliases = kvp.Value;
				foreach (var selectorAlias in selectorAliases) enumerator2(orignalSelector, selectorAlias);
			}
		}

		#endregion

		#region TraitUsageExpression protocol

		public override TraitUsageExpression IdentityObject {
			get {return this;}
		}

		public override ESObjectSpace ObjectSpace {
			get {return objectSpace;}
		}

		public override TraitUsageExpression Reduced {
			get {	
				reduce();
				return this;
			}
		}

		public override void addUser(TraitUser aUser) {
		}

		public override void removeUser(TraitUser aUser) {
			usedTraitsDo(usedTrait => usedTrait.removeUser(this));
		}

		public override TraitUsageExpression combinedWith(TraitUsageExpression operand) {
			return operand.combinedWithComposition(this);
		}

		public override TraitUsageExpression combinedWithTrait(Trait operand) {
			add(operand);
			return this;
		}

		public override TraitUsageExpression combinedWithTransformation(ESTraitTransformation operand) {
			add(operand);
			return this;
		}

		public override TraitUsageExpression combinedWithComposition(ESTraitComposition operand) {
			if (this == operand) return this;
			usedTraitsDo(element => operand.add(element));
			return operand;
		}

		protected void add(TraitUsageExpression addend) {
			var identity = addend.IdentityObject;
			TraitUsageExpression current;
			if (elements.TryGetValue(identity, out current)) {
				if (current == addend) return;
				addend = current.combinedWith(addend);
			}
			lock (this) { 
				identity.addUser(this);
				elements[identity] = addend;
				isReduced = false;
			}
		}

		public override TraitUsageExpression excluding(ESSymbol selector) {
			lock (this) { 
				foreach (var kvp in elements) {
					var identity = kvp.Key;
					var element = kvp.Value;
					elements[identity] = element.excluding(selector);
					isReduced = false;
				}
			}
			return this;
		}

		public override TraitUsageExpression aliasing(ESSymbol sourceSelector, ESSymbol selectorAlias) {
			lock (this) {
				foreach (var kvp in elements) {
					var identity = kvp.Key;
					var element = kvp.Value;
					elements[identity] = element.aliasing(sourceSelector, selectorAlias);
					isReduced = false;
				}
			}
			return this;
		}

		public override void withUnimplementedMessagesSentToSelfDo(Action<ESSymbol> enumerator1) {
			if (!isReduced) reduce();
			foreach (var selector in unimplementedMessagesSentToSelf) enumerator1(selector);
		}

		public override void methodConflictsDo(Action<ESSymbol, ESMethod> enumerator2) {
			if (!isReduced) reduce();
			foreach (var kvp in methodConflicts) {
				var selector = kvp.Key;
				foreach (var method in kvp.Value) enumerator2(selector, method);
			}
		}

		public override void hostSystemMethodConflictsDo(Action<long, String, ESMethod> enumerator3) {
			if (!isReduced) reduce();
			foreach (var kvp in hostSystemMethodConflicts) {
				var arity = kvp.Key;
				var arityDict = kvp.Value;
				foreach (var arityKvp in arityDict) { 
					var selector = arityKvp.Key;
					foreach (var method in arityKvp.Value) enumerator3(arity, selector, method);
				}
			}
		}

		#endregion

		#region Method accessing/enumeration protocol

		public override void selectorsDo(FuncNs.Func<object, object> enumerator1, HashSet<ESSymbol> exclusionSet) {
			if (!isReduced) reduce();
			foreach (var assoc in methodDictionary) {
				var selector = assoc.Key;
				if (exclusionSet == null) {
					enumerator1(selector);
				} else if (!exclusionSet.Contains(selector)) {
					exclusionSet.Add(selector);
					enumerator1(selector);
				}
			}
		}

		public override void selectorsAndMethodsDo(FuncNs.Func<object, object, object> enumerator2, HashSet<ESSymbol> exclusionSet) {
			if (!isReduced) reduce();
			foreach (var assoc in methodDictionary) {
				var selector = assoc.Key;
				if (exclusionSet == null) {
					enumerator2(assoc.Key, assoc.Value);
				} else if (!exclusionSet.Contains(selector)) {
					exclusionSet.Add(selector);
					enumerator2(assoc.Key, assoc.Value);
				}
			}
		}

		public override HashSet<ESSymbol> selectors() {
			var symbols = newSelectorSet();
			selectorsDo(selector => symbols.Add((ESSymbol)selector), null);
			return symbols;
		}

		public override ESMethod compiledMethodAt(ESSymbol selector) {
			if (!isReduced) reduce();
			ESMethod method;
			if (methodDictionary.TryGetValue(selector, out method)) return method;
			return null;
		}

		public override bool includesSelector(ESSymbol selector) {
			if (!isReduced) reduce();
			return methodDictionary.ContainsKey(selector);
		}

		public override void systemSelectorsDo(FuncNs.Func<object, object, object> enumerator2, HashSet<String> exclusionSet) {
			if (!isReduced) reduce();
			foreach (var numArgsAssoc in hostSystemMethodDictionary) {
				var arity = numArgsAssoc.Key;
				foreach (var selectorAssoc in numArgsAssoc.Value) { 
					var hostSystemSelector = selectorAssoc.Key;
					if (exclusionSet == null) { 
						enumerator2(hostSystemSelector, arity);
					} else if (!exclusionSet.Contains(hostSystemSelector)) {
						exclusionSet.Add(hostSystemSelector);
						enumerator2(hostSystemSelector, arity);
					}
				}
			}		
		}

		public override void systemSelectorsAndMethodsDo(FuncNs.Func<object, object, object, object> enumerator3, HashSet<String> exclusionSet) {
			if (!isReduced) reduce();
			foreach (var numArgsAssoc in hostSystemMethodDictionary) {
				var arity = numArgsAssoc.Key;
				foreach (var selectorAssoc in numArgsAssoc.Value) { 
					var hostSystemSelector = selectorAssoc.Key;
					if (exclusionSet == null) { 
						enumerator3(hostSystemSelector, arity, selectorAssoc.Value);
					} else if (!exclusionSet.Contains(hostSystemSelector)) {
						exclusionSet.Add(hostSystemSelector);
						enumerator3(hostSystemSelector, arity, selectorAssoc.Value);
					}
				}
			}
		}

		public override ESMethod compiledMethodAtSystemSelector(string systemSelector, long numArgs) {
			if (!isReduced) reduce();
			IDictionary<String, ESMethod> hostSysMethodDict;
			if (hostSystemMethodDictionary.TryGetValue(numArgs, out hostSysMethodDict)) {
				ESMethod method;
				if (hostSysMethodDict.TryGetValue(systemSelector, out method)) {
					return method;
				}
			}
			return null;
		}

		public override bool includesSystemSelector(string systemSelector, long numArgs) {
			if (!isReduced) reduce();
			IDictionary<String, ESMethod> hostMethodDict;
			if (!hostSystemMethodDictionary.TryGetValue(numArgs, out hostMethodDict)) return false;
			return hostMethodDict.ContainsKey(systemSelector);		
		}

		#endregion

		public override T valueBy<T>(Operation<T> operation) {
		    return operation.applyToTraitComposition(this);
		}

		public new class Primitives : ESAbstractTraitUsageExpression.Primitives {

			protected override void bindToObjectSpace() {
				domainClass = objectSpace.TraitCompositionClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.TraitComposition;}
			}

			#region Primitive Definitions

			#endregion

			public override void publishCanonicalPrimitives() {

				base.publishCanonicalPrimitives();

				// publishPrimitive("superclass:",						new FuncNs.Func<Object, Object, Object>(_setSuperclass_));

			}

		}

	}

	#endregion

}
