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
#if CLR2
using FuncNs = Microsoft.Scripting.Utils;
#else
using FuncNs = System;
#endif
using Microsoft.Scripting;
using Microsoft.Scripting.Runtime;
using EssenceSharp.Exceptions.System;
using EssenceSharp.Exceptions.System.PrimitiveFailures;
#endregion

namespace EssenceSharp.Runtime {

	public interface BindingHandle {

		bool IsDirect {
			get;
		}

		bool IsScopeVariableHandle {
			get;
		}

		bool IsImmutable {
			get;
		}

		void beImmutable();

		Object Value {
			get;
			set;
		}

	}

	public abstract class AbstractBindingHandle : EssenceSharpObject, BindingHandle {

		protected bool											isImmutable				= false;

		protected AbstractBindingHandle() {
		}

		protected AbstractBindingHandle(bool isImmutable) {
			this.isImmutable = isImmutable;
		}

		public virtual bool IsDirect {
			get {return false;}
		}

		public virtual bool IsScopeVariableHandle {
			get {return false;}
		}

		public bool IsImmutable {
			get {return isImmutable;}
		}

		public void beImmutable() {
			isImmutable = true;
		}

		public abstract Object Value {
			get;
			set;
		}

		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			var value = Value;
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

	}

	public class DirectBindingHandle : AbstractBindingHandle {

		protected Object										value;

		public DirectBindingHandle() {
		}

		public DirectBindingHandle(Object value) : base() {
			this.value = value;
		}

		public DirectBindingHandle(Object value, bool isImmutable) :base(isImmutable) {
			this.value = value;
		}

		public override bool IsDirect {
			get {return true;}
		}

		public override Object Value {
			get {return value;}
			set {if (IsImmutable) throw new ImmutableObjectException();
			     this.value = value;}
		}

	}

	public class ScopeVariableBindingHandle : AbstractBindingHandle {

		protected IScopeVariable									scopeVariable;

		public ScopeVariableBindingHandle(IScopeVariable scopeVariable) : base() {
			this.scopeVariable = scopeVariable;
		}

		public ScopeVariableBindingHandle(IScopeVariable scopeVariable, bool isImmutable) :base(isImmutable) {
			this.scopeVariable = scopeVariable;
		}

		public override bool IsScopeVariableHandle {
			get {return true;}
		}

		public IScopeVariable ScopeVariable {
			get {return scopeVariable;}
			set {scopeVariable = value;}
		}

		public override Object Value {
			get {	Object value;
				return (scopeVariable.TryGetValue(out value)) ? value : null;}
			set {if (IsImmutable) throw new ImmutableObjectException();
			     scopeVariable.SetValue(value);}
		}

	}

	public class ESBindingReference : ESAbstractAssociation<String, BindingHandle> {
		
		public static readonly byte								nonDeletabilityFlagBit			= 4;

		public static implicit operator KeyValuePair<String, BindingHandle>(ESBindingReference association) {
			return new KeyValuePair<String, BindingHandle>(association.Key, association.Value);  
		}

		protected AccessPrivilegeLevel								accessPrivilegeLevel			= AccessPrivilegeLevel.Public;

		public ESBindingReference(ESBehavior esClass) : base(esClass) {}


		public ESBindingReference(ESBehavior esClass, String key, BindingHandle value) : base(esClass, key, value) {}

		public ESBindingReference(ESBehavior esClass, String key, Object value) : base(esClass, key, new DirectBindingHandle(value)) {}

		public ESBindingReference(ESBehavior esClass, String key, BindingHandle value, AccessPrivilegeLevel accessPrivilegeLevel) : base(esClass, key, value) {
			this.accessPrivilegeLevel = accessPrivilegeLevel;
		}

		public ESBindingReference(ESBehavior esClass, String key, Object value, AccessPrivilegeLevel accessPrivilegeLevel) : base(esClass, key, new DirectBindingHandle(value)) {
			this.accessPrivilegeLevel = accessPrivilegeLevel;
		}

		public override bool IsDeletable {
			get {return (statusFlags & nonDeletabilityFlagBit) == 0;}
		}

		public void beUndeletable() {
			if (IsImmutable) throw new ImmutableObjectException();
			statusFlags |= nonDeletabilityFlagBit;
		}

		public AccessPrivilegeLevel AccessPrivilegeLevel { 
			get {return accessPrivilegeLevel;}
			set {accessPrivilegeLevel = value;}
		}
		
		public override void beImmutable() {
			base.beImmutable();
			value.beImmutable();
		}
		
		public void setKeyAndValue(String newKey, Object newValue) {
			if (IsImmutable || IsKeyImmutable) throw new ImmutableObjectException();
			key = newKey;
			if (value == null) {
				value = new DirectBindingHandle(newValue);
			} else {
				value.Value = newValue;
			}
		}

		public void setValue(Object newValue) {
			if (IsImmutable) throw new ImmutableObjectException();
			if (value == null) {
				value = new DirectBindingHandle(newValue);
			} else {
				value.Value = newValue;
			}
		}

		public override void setValueImmutably(Object newValue) {
			if (IsImmutable) throw new ImmutableObjectException();
			if (value == null) {
				value = new DirectBindingHandle(newValue);
			} else {
				value.Value = newValue;
			}
			beImmutable();
		}
		
		public ESBindingReference withValue(Object newValue) {
			var mutableCopy = (ESBindingReference)base.shallowCopy();
			mutableCopy.setValue(newValue);
			if (IsImmutable) {
				mutableCopy.beImmutable();
			} else if (IsKeyImmutable) {
				mutableCopy.keyBeImmutable();
			}
			return mutableCopy;
		}
		
		public ESBindingReference withKeyAndValue(String newKey, Object newValue) {
			var mutableCopy = (ESBindingReference)base.shallowCopy();
			mutableCopy.setKeyAndValue(newKey, newValue);
			if (IsImmutable) {
				mutableCopy.beImmutable();
			} else if (IsKeyImmutable) {
				mutableCopy.keyBeImmutable();
			}
			return mutableCopy;
		}

		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (key == null) {
				append("nil");
			} else {
				append(key);
			}
			append(" -> ");
			if (value == null) {
				append("nil");
			} else {
				Object valueValue = value.Value;
				ESObject esValue = valueValue as ESObject;
				if (esValue == null) {
					append(valueValue.ToString());
				} else {
					esValue.printUsing(depth, append, newLine);
				}
			}
		}

		public override T valueBy<T>(Operation<T> operation) {
		    return operation.applyToBindingReference(this);
		}

		public new class Primitives : PrimitiveDomain {

			protected override void bindToObjectSpace() {
				domainClass = objectSpace.BindingReferenceClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.BindingReference;}
			}

			#region Primitive Definitions
		
			public Object _isDeletable_(Object receiver) {
				return ((ESBindingReference)receiver).IsDeletable;
			}
		
			public Object _beUndeletable_(Object receiver) {
				((ESBindingReference)receiver).beUndeletable();
				return receiver;
			}
		
			public Object _isKeyImmutable_(Object receiver) {
				return ((ESBindingReference)receiver).IsKeyImmutable;
			}
		
			public Object _keyBeImmutable_(Object receiver) {
				((ESBindingReference)receiver).keyBeImmutable();
				return receiver;
			}
		
			public Object _accessPrivilege_(Object receiver) {
				return SymbolRegistry.symbolFor(((ESBindingReference)receiver).AccessPrivilegeLevel.ToString());
			}
		
			public Object _setAccessPrivilege_(Object receiver, Object accessPrivilege) {
				AccessPrivilegeLevel accessPrivilegeLevel;
				try {
					accessPrivilegeLevel = (AccessPrivilegeLevel)Enum.Parse(typeof(AccessPrivilegeLevel), objectSpace.asESSymbol(accessPrivilege));
				} catch {
					throw new PrimInvalidOperandException("accessPrivilege: <accessPrivilege> must be a Symbol or String identifying a valid access privilege level.");
				}
				((ESBindingReference)receiver).AccessPrivilegeLevel = accessPrivilegeLevel;
				return receiver;
			}
		
			public Object _key_(Object receiver) {
				return objectSpace.symbolFor(((ESBindingReference)receiver).Key);
			}
		
			public Object _value_(Object receiver) {
				return ((ESBindingReference)receiver).Value.Value;
			}
		
			public Object _setKeyAndValue_(Object receiver, Object key, Object value) {
				((ESBindingReference)receiver).setKeyAndValue(asHostString(key), value);
				return receiver;
			}
		
			public Object _setKey_(Object receiver, Object key) {
				((ESBindingReference)receiver).setKey(asHostString(key));
				return receiver;
			}
		
			public Object _setKeyImmutably_(Object receiver, Object key) {
				((ESBindingReference)receiver).setKeyImmutably(asHostString(key));
				return receiver;
			}
		
			public Object _setValue_(Object receiver, Object value) {
				((ESBindingReference)receiver).setValue(value);
				return receiver;
			}
		
			public Object _setValueImmutably_(Object receiver, Object value) {
				((ESBindingReference)receiver).setValueImmutably(value);
				return receiver;
			}
		
			public Object _withKey_(Object receiver, Object newKey) {
				return ((ESBindingReference)receiver).withKey(asHostString(newKey));
			}
		
			public Object _withValue_(Object receiver, Object newValue) {
				return ((ESBindingReference)receiver).withValue(newValue);
			}
		
			public Object _withKeyAndValue_(Object receiver, Object newKey, Object newValue) {
				return ((ESBindingReference)receiver).withKeyAndValue(asHostString(newKey), newValue);
			}

			#endregion

			public override void publishCanonicalPrimitives() {

				publishPrimitive("isDeletable",						new FuncNs.Func<Object, Object>(_isDeletable_));
				publishPrimitive("beUndeletable",					new FuncNs.Func<Object, Object>(_beUndeletable_));
				publishPrimitive("isKeyImmutable",					new FuncNs.Func<Object, Object>(_isKeyImmutable_));
				publishPrimitive("keyBeImmutable",					new FuncNs.Func<Object, Object>(_keyBeImmutable_));
				publishPrimitive("key",							new FuncNs.Func<Object, Object>(_key_));
				publishPrimitive("value",						new FuncNs.Func<Object, Object>(_value_));
				publishPrimitive("key:value:",						new FuncNs.Func<Object, Object, Object, Object>(_setKeyAndValue_));
				publishPrimitive("key:",						new FuncNs.Func<Object, Object, Object>(_setKey_));
				publishPrimitive("immutableKey:",					new FuncNs.Func<Object, Object, Object>(_setKeyImmutably_));
				publishPrimitive("value:",						new FuncNs.Func<Object, Object, Object>(_setValue_));
				publishPrimitive("immutableValue:",					new FuncNs.Func<Object, Object, Object>(_setValueImmutably_));
				publishPrimitive("withKey:",						new FuncNs.Func<Object, Object, Object>(_withKey_));
				publishPrimitive("withValue:",						new FuncNs.Func<Object, Object, Object>(_withValue_));
				publishPrimitive("withKey:value",					new FuncNs.Func<Object, Object, Object, Object>(_withKeyAndValue_));

				publishPrimitive("accessPrivilege",					new FuncNs.Func<Object, Object>(_accessPrivilege_));
				publishPrimitive("accessPrivilege:",					new FuncNs.Func<Object, Object, Object>(_setAccessPrivilege_));

			}

		}

	}

	public class ESImportSpec : IEquatable<ESImportSpec> {

		protected NamespaceObject										source;
		protected AccessPrivilegeLevel									accessPrivilegeLevel			= Runtime.AccessPrivilegeLevel.Local;
		protected ImportTransitivity									transitivity				= ImportTransitivity.Intransitive;

		public ESImportSpec(NamespaceObject source, AccessPrivilegeLevel accessPrivilegeLevel, ImportTransitivity transitivity) {
			this.source = source;
			this.accessPrivilegeLevel = accessPrivilegeLevel;
			this.transitivity = transitivity;
		}

		public NamespaceObject Source {
			get {return source;}
		}

		public AccessPrivilegeLevel AccessPrivilegeLevel {
			get {return accessPrivilegeLevel;}
		}

		public ImportTransitivity Transitivity {
			get {return transitivity;}
		}

		public virtual bool Equals(ESImportSpec other) {
			if (this == other) return true;
			if (other == null) return false;
			if (GetType() != other.GetType()) return false;
			if (source != other.Source) return false;
			if (AccessPrivilegeLevel != other.AccessPrivilegeLevel) return false;
			return Transitivity == other.Transitivity;
		}

		public override bool Equals(Object other) {
			if (this == other) return true;
			if (other == null) return false;
			var comparand = other as ESImportSpec;
			if (comparand == null) return false;
			return comparand.Equals(this);
		}

		public override int GetHashCode() {
			return source.GetHashCode();
		}

	}

	public class ESSpecifImportSpec : ESImportSpec {

		protected String /* The public name to which the entity is bound in the source namespace */	nameInSource;

		public ESSpecifImportSpec(NamespaceObject source, AccessPrivilegeLevel accessPrivilegeLevel, ImportTransitivity transitivity, String nameInSource) : base(source, accessPrivilegeLevel, transitivity) {
			this.nameInSource = nameInSource;
		}

		public String NameInSource {
			get {return nameInSource;}
		}

		public override bool Equals(ESImportSpec other) {
			if (!base.Equals(other)) return false;
			return NameInSource == ((ESSpecifImportSpec)other).NameInSource;
		}

	}

	public interface NamespaceObject : ESGenericDictionary<String, BindingHandle, ESBindingReference> {

		long Identity {get;}
		long VersionId {get;}
		bool IsClrNamespace {get;}
		ESSymbol Name {get;}
		String NameString {get;}
		ESPathname pathname();
		String PathnameString {get;}
		String HostSystemNamespace {get;}
		String HostSystemName {get;}
		String QualifiedHostSystemName {get;}
		void setName(ESSymbol newName);
		void renameFromTo(ESSymbol prevName, ESSymbol newName);
		String AssemblyNameString {get;set;}
		String AssemblyPathname {get;set;}
		void setAssemblyPath(FileInfo assemblyPath);
		Assembly Assembly {get;set;}
		ESNamespace Environment {get;}
		void setEnvironment(ESNamespace newEnvironment);
		void fromRootDo(System.Action<long, ESNamespace> enumerator2);

		ESBindingReference declareConstant(String key, Object constantValue, AccessPrivilegeLevel accessPrivilegeLevel, Functor0<ESBindingReference> onCollisionAction);
		ESBindingReference declareVariable(String key, AccessPrivilegeLevel accessPrivilegeLevel, Functor0<ESBindingReference> onCollisionAction);
		ESBindingReference importScopeVariableFrom(Scope scope, String key, AccessPrivilegeLevel accessPrivilegeLevel, Functor0<ESBindingReference> onCollisionAction);
		bool declareInSelf();
		bool declareInSelf(bool overridePreviousBinding);
		bool declareInSelfAs(String alias, bool overridePreviousBinding);
		ESNamespace defineNamespace(String nsName, AccessPrivilegeLevel accessPrivilegeLevel, FuncNs.Func<Object, Object> configureNamespace);
		ESClass defineClass(String className, AccessPrivilegeLevel accessPrivilegeLevel, FuncNs.Func<Object, Object> configureClass);
		ESInstanceTrait defineTrait(String traitName, AccessPrivilegeLevel accessPrivilegeLevel, FuncNs.Func<Object, Object> configureTrait);

		ESBindingReference localBindingAt(String key, AccessPrivilegeLevel requestorPrivilege);
		ESBindingReference bindingAt(String key, AccessPrivilegeLevel requestorRights, ImportTransitivity importTransitivity, FuncNs.Func<ESBindingReference> notFoundAction);

		void initializeSpecificImports();
		void initializeGeneralImports();
		void initializeImports();
		void addImport(ESImportSpec importSpec);
		void addImports(List<ESImportSpec> importSpecs);
		void importAs(String localName, ESSpecifImportSpec specificImport);

	}

	public class ESNamesapceIdentityComparator : IdentityComparator<ESNamespace> {}

	public class ESNamespace : ESAbstractDictionary<String, BindingHandle, ESBindingReference>, NamespaceObject {

		#region Static variables and functions

		private static long										identityGenerator			= 0;
		private static long										versionIdGenerator			= 0;

		#endregion

		protected long											identity				= identityGenerator++;
		protected long											versionId				= versionIdGenerator++;
		protected ESSymbol										name;
		protected bool											isBoundToHostSystemNamespace;
		protected String										hostSystemNamespace;
		protected String										hostSystemName;
		protected Assembly										assembly;
		protected ESNamespace	/* The "parent" or "containing" namespace */				environment;
		protected Dictionary<String, ESSpecifImportSpec>						specificImports;
		protected List<ESImportSpec>									generalImports;

		public ESNamespace(ESBehavior esClass) : base(esClass) {
			initializeImports();
		}

		public ESNamespace(ESBehavior esClass, bool isBoundToHostNamespace) : this(esClass) {
			this.isBoundToHostSystemNamespace = isBoundToHostNamespace;
		}

		public ESNamespace(ESBehavior esClass, ESNamespace environment, ESSymbol name) : base(esClass) {
			initializeImports();
			this.name = name;
			setEnvironment(environment);
		}

		public ESNamespace(ESBehavior esClass, ESNamespace environment, ESSymbol name, bool isBoundToHostNamespace) : this(esClass, environment, name) {
			this.isBoundToHostSystemNamespace = isBoundToHostNamespace;
		}

		public ESNamespace(ESBehavior esClass, long capacity) : base(esClass, capacity) {
			initializeImports();
		}

		public ESNamespace(ESBehavior esClass, long capacity, ESNamespace environment, ESSymbol name) : base(esClass, capacity) {
			initializeImports();
			this.name = name;
			setEnvironment(environment);
		}

		protected override Dictionary<String, ESBindingReference> newBindings(long capacity, IEqualityComparer<String> keyComparator) {
			return new Dictionary<String, ESBindingReference>((int)capacity, keyComparator);
		}

		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.Namespace;}
		}
		
		public override bool IsNamespace {
			get {return true;}
		}
		
		public override ESNamespace asESNamespace() {
			return this;
		}

		public override BindingHandle asBindingHandle() {
			return new DirectBindingHandle(this);
		}

		protected override ESBindingReference newAssociation(String key, BindingHandle value) {
			return Class.ObjectSpace.newBindingReference(key, value);
		}

		public override void postCopy() {
			base.postCopy();
			identity		= identityGenerator++;
			versionId		= versionIdGenerator++;
			var oldSpecificImports	= specificImports;
			var oldSGeneralImports	= generalImports;
			initializeImports();
			foreach (var kvp in oldSpecificImports) specificImports[kvp.Key] = kvp.Value;
			generalImports.AddRange(oldSGeneralImports);
		}

		#region Dictionary protocol

		public override ESBindingReference associationAtIfAbsent(String key, FuncNs.Func<ESBindingReference> notFoundAction) {
			return bindingAt(key, AccessPrivilegeLevel.Public, ImportTransitivity.Transitive, notFoundAction);
		}
		
		#endregion

		#region Namespace protocol: Core

		public long Identity {
			get {return identity;}
		}

		public long VersionId {
			get { return versionId; }
		}

		protected virtual void incrementVersion() {
			versionId = versionIdGenerator++;
		}

		public bool IsClrNamespace {
			get {return this == Class.ObjectSpace.ClrNamespace;}
		}

		protected virtual String AnonymousName {
			get {return "AnAnonymousNamespace";}
		}

		public virtual ESSymbol Name {
			get {return name ?? Class.ObjectSpace.symbolFor(AnonymousName);}
		}

		public String NameString {
			get {return Name.PrimitiveValue;}
		}

		public virtual ESPathname pathname() {
			if (environment == null) return Class.ObjectSpace.pathnameFromString(NameString);
			ESPathname pn = null;
			int index = 0;
			fromRootDo((long depth, ESNamespace pathElement) => {
				if (pn == null) {
					pn = Class.ObjectSpace.newPathname(depth - 1);
				} else {
					pn[index++] = pathElement.NameString;
				}
			});
			return pn;
		}

		public String PathnameString {
			get {return pathname().ToString();}
		}

		public String HostSystemNamespace {
			get {
				if (hostSystemNamespace == null) {
					var qualifiedNameBuilder = new StringBuilder();
					buildQualifiedHostSystemNamePrefix(qualifiedNameBuilder, 0);
					return qualifiedNameBuilder.ToString();
				};
				return hostSystemNamespace;}
		}

		public String HostSystemName {
			get {return hostSystemName ?? NameString;}
		}

		public String QualifiedHostSystemName {
			get {	var qualifiedNameBuilder = new StringBuilder();
				buildQualifiedHostSystemNamePrefix(qualifiedNameBuilder, 0);
				qualifiedNameBuilder.Append(HostSystemName);
				return qualifiedNameBuilder.ToString();}
		}

		protected void buildQualifiedHostSystemNamePrefix(StringBuilder qualifiedNameBuilder, int depth) {
			if (environment == null) return;
			if (hostSystemNamespace == null) {
				if (IsClrNamespace || !IsBoundToHostSystemNamespace) return;
				environment.buildQualifiedHostSystemNamePrefix(qualifiedNameBuilder, depth + 1);
			} else {
				qualifiedNameBuilder.Append(hostSystemNamespace);
				qualifiedNameBuilder.Append('.');
			}
			if (depth > 0) {
				qualifiedNameBuilder.Append(HostSystemName);
				if (IsBoundToHostSystemNamespace && IsBehavior) {
					qualifiedNameBuilder.Append('+');
				} else {
					qualifiedNameBuilder.Append('.');
				}
			}
		}
		
		internal virtual void nameChanged() {
			// By default, do nothing
		}

		protected virtual void basicSetName(ESSymbol newName) {
			if (name == newName) return;
			name = newName;
			nameChanged();
		}

		public virtual void setName(ESSymbol newName) {
			if (ReferenceEquals(name, newName)) return;
			if (IsImmutable) throw new ImmutableObjectException();
			ESSymbol prevName = name;
			if (environment == null) {
				basicSetName(newName);
			} else if (prevName == null) {
				environment.atPut(newName.PrimitiveValue, this.asBindingHandle());
			} else {
				environment.renameFromTo(prevName, newName);
			}
		}

		public virtual void renameFromTo(ESSymbol prevName, ESSymbol newName) {
			if (bindings.ContainsKey(newName.PrimitiveValue)) throw new NamespaceEntryNameConflictException();
			ESBindingReference prevAssociation;
			if (bindings.TryGetValue(prevName.PrimitiveValue, out prevAssociation)) {
				ESBindingReference newAssociation= (ESBindingReference)prevAssociation.withKey(newName.PrimitiveValue);
				bindings[newName.PrimitiveValue] = newAssociation;
				bindings.Remove(prevName.PrimitiveValue);
				BindingHandle nsReference = newAssociation.Value;
				ESNamespace localNS = nsReference.Value as ESNamespace;
				if (localNS != null) localNS.basicSetName(newName);
			} else { 
				throw new KeyNotFoundException(prevName.PrimitiveValue);
			}
		}

		protected virtual void setHostSystemName(String newName) {
			if (newName == null) {
				if (hostSystemName == null) return;
				hostSystemName = null;
			} else if (newName == NameString) {
				if (hostSystemName == null) return;
				hostSystemName = null;
			} else {
				isBoundToHostSystemNamespace = true;
				if (hostSystemName == newName) return;
				hostSystemName = newName;
			}
			hostSystemNameChanged();
		}
		
		internal virtual void hostSystemNameChanged() {
			// By default, do nothing
		}

		public bool IsBoundToHostSystemNamespace {
			get {return isBoundToHostSystemNamespace;}
		}

		protected virtual void setHostSystemNamespace(String newName) {
			if (newName == null) {
				if (hostSystemNamespace == null) return;
				hostSystemNamespace = null;
			} else {
				isBoundToHostSystemNamespace = true;
				if (hostSystemNamespace == newName) return;
				hostSystemNamespace = newName;
			}
			hostSystemNamespaceChanged();
		}
		
		internal virtual void hostSystemNamespaceChanged() {
			// By default, do nothing
		}
		
		public virtual String AssemblyNameString {
			get {return Assembly.FullName;}
			set {	Class.ObjectSpace.bindNamespaceToAssemblyNamed(this, new AssemblyName(value));
				Assembly = Class.ObjectSpace.assemblyFor(this, true);}
		}

		public String AssemblyPathname {
			get {	var path = Class.ObjectSpace.assemblyPathFor(this);
				return path == null ? null : path.FullName;}
			set {setAssemblyPath(new FileInfo(value));}
		}

		public void setAssemblyPath(FileInfo assemblyPath) {
			Class.ObjectSpace.bindNamespaceToAssemblyAt(this, assemblyPath);
			Assembly = Class.ObjectSpace.assemblyFor(this, true);
		}

		public virtual Assembly Assembly {
			get {	if (assembly == null) {
					var assm = Class.ObjectSpace.assemblyFor(this, true);
					if (assm == null) {
						if (environment != null) {
							assm = environment.Assembly;
						} 
					}
					assembly = assm ?? typeof(ESObjectSpace).Assembly;
				}
				return assembly;}
			set {assembly = value;}
		}

		public ESNamespace Environment {
			get {return environment;}
		}

		public virtual void setEnvironment(ESNamespace newEnvironment) {
			if (ReferenceEquals(environment, newEnvironment)) return;
			if (IsImmutable) throw new ImmutableObjectException();
			unbindFromEnvironment();
			environment = newEnvironment;
			bindToEnvironment();
		}

		protected virtual void unbindFromEnvironment() {
			if (environment == null || name == null) return;
			environment.removeKeyIfAbsent(Name.PrimitiveValue, null);
		}

		protected virtual void bindToEnvironment() {
			if (environment == null) return;
			isBoundToHostSystemNamespace = isBoundToHostSystemNamespace || environment.IsBoundToHostSystemNamespace;
			if (name != null) {
				environment.atPut(Name.PrimitiveValue, this.asBindingHandle());
			}
		}

		protected void fromRootDo(long depth, System.Action<long, ESNamespace> enumerator2) {
			if (environment != null) environment.fromRootDo(depth + 1, enumerator2);
			enumerator2(depth, this);
		}

		public void fromRootDo(System.Action<long, ESNamespace> enumerator2) {
			if (environment != null) environment.fromRootDo(2, enumerator2);
			enumerator2(0, this);
		}

		#endregion

		#region Namespace protocol: Declaring/Defining

		public ESBindingReference declareConstant(String key, Object constantValue, AccessPrivilegeLevel accessPrivilegeLevel, Functor0<ESBindingReference> onCollisionAction) {
			ESBindingReference binding;
			if (bindings.TryGetValue(key, out binding)) return onCollisionAction == null ? null : onCollisionAction();
			binding = Class.ObjectSpace.newBindingReference(key, constantValue, accessPrivilegeLevel);
			binding.beImmutable();
			bindings[key] = binding;
			return binding;
		}

		public ESBindingReference declareVariable(String key, AccessPrivilegeLevel accessPrivilegeLevel, Functor0<ESBindingReference> onCollisionAction) {
			ESBindingReference binding;
			if (bindings.TryGetValue(key, out binding)) return onCollisionAction == null ? null : onCollisionAction();
			binding = Class.ObjectSpace.newBindingReference(key, new DirectBindingHandle(null), accessPrivilegeLevel);
			binding.keyBeImmutable();
			bindings[key] = binding;
			return binding;
		}

		public ESBindingReference importScopeVariableFrom(Scope scope, String key, AccessPrivilegeLevel accessPrivilegeLevel, Functor0<ESBindingReference> onCollisionAction) {
			if (scope == null) return null;
			var storage = scope.Storage as ScopeStorage;
			if (storage == null) return null;
			var variable = storage.GetVariable(key);
			if (variable == null) return null;
			ESBindingReference binding;
			if (bindings.TryGetValue(key, out binding)) {
				var handle = binding.Value;
				if (!handle.IsScopeVariableHandle) return onCollisionAction == null ? null : onCollisionAction();
				var curentScopeVar = ((ScopeVariableBindingHandle)handle).ScopeVariable;
				if (!ReferenceEquals(variable, curentScopeVar)) {
					((ScopeVariableBindingHandle)handle).ScopeVariable = variable;
				}
			} else {
				binding = Class.ObjectSpace.newBindingReference(key, new ScopeVariableBindingHandle(variable), accessPrivilegeLevel);
				binding.keyBeImmutable();
				bindings[key] = binding;
			}
			return binding;
		}

		public bool declareInSelf() {
			return declareInSelf(false);
		}

		public bool declareInSelf(bool overridePreviousBinding) {
			return declareInSelfAs(Name, overridePreviousBinding);
		}

		public bool declareInSelfAs(String alias, bool overridePreviousBinding) {
			if (!overridePreviousBinding) if (bindings.ContainsKey(alias)) return false;
			atPut(alias, this.asBindingHandle());
			return true;
		}

		public ESNamespace defineNamespace(String nsName, AccessPrivilegeLevel accessPrivilegeLevel, FuncNs.Func<Object, Object> configureNamespace) {
			ESBindingReference binding;
			ESNamespace theNamespace = null;
			if (bindings.TryGetValue(nsName, out binding)) {
				var value = binding.Value.Value;
				if (value != null) {
					theNamespace = value as ESNamespace;
					if (theNamespace == null && binding.IsImmutable) throw new ImmutableBindingException("Cannot change the value of the binding named " + binding.Key);
				}
				if (binding.AccessPrivilegeLevel != accessPrivilegeLevel) binding.AccessPrivilegeLevel = accessPrivilegeLevel;
			} 
			if (theNamespace == null) {
				var objectSpace = Class.ObjectSpace;
				theNamespace = objectSpace.newNamespace(this, null);
				if (binding == null) {
					binding = objectSpace.newBindingReference(nsName, theNamespace, accessPrivilegeLevel);
					bindings[nsName] = binding;
				} else {
					binding.setValue(theNamespace);
					if (binding.AccessPrivilegeLevel != accessPrivilegeLevel) binding.AccessPrivilegeLevel = accessPrivilegeLevel;
				}
				theNamespace.basicSetName(objectSpace.symbolFor(nsName));
			}
			if (configureNamespace != null) configureNamespace(theNamespace);
			return theNamespace;
		}

		public ESClass defineClass(String className, AccessPrivilegeLevel accessPrivilegeLevel, FuncNs.Func<Object, Object> configureClass) {
			ESBindingReference binding;
			ESClass theClass = null;
			if (bindings.TryGetValue(className, out binding)) {
				var value = binding.Value.Value;
				if (value != null) {
					theClass = value as ESClass;
					if (theClass == null && binding.IsImmutable) throw new ImmutableBindingException("Cannot change the value of the binding named " + binding.Key);
				}
				if (binding.AccessPrivilegeLevel != accessPrivilegeLevel) {
					binding.AccessPrivilegeLevel = accessPrivilegeLevel;
				}
			} 
			if (theClass == null) {
				var objectSpace = Class.ObjectSpace;
				theClass = objectSpace.newClass();
				if (binding == null) {
					binding = objectSpace.newBindingReference(className, theClass, accessPrivilegeLevel);
					bindings[className] = binding;
				} else {
					binding.setValue(theClass);
				}
				theClass.setEnvironment(this);
				theClass.basicSetName(objectSpace.symbolFor(className));
			}
			if (configureClass != null) configureClass(theClass);
			return theClass;
		}

		public ESInstanceTrait defineTrait(String traitName, AccessPrivilegeLevel accessPrivilegeLevel, FuncNs.Func<Object, Object> configureTrait) {
			ESBindingReference binding;
			ESInstanceTrait theTrait = null;
			if (bindings.TryGetValue(traitName, out binding)) {
				var value = binding.Value.Value;
				if (value != null) {
					theTrait = value as ESInstanceTrait;
					if (theTrait == null && binding.IsImmutable) throw new ImmutableBindingException("Cannot change the value of the binding named " + binding.Key);
				}
				if (binding.AccessPrivilegeLevel != accessPrivilegeLevel) {
					binding.AccessPrivilegeLevel = accessPrivilegeLevel;
				}
			} 
			if (theTrait == null) {
				var objectSpace = Class.ObjectSpace;
				theTrait = objectSpace.newTrait();
				if (binding == null) {
					binding = objectSpace.newBindingReference(traitName, theTrait, accessPrivilegeLevel);
					bindings[traitName] = binding;
				} else {
					binding.setValue(theTrait);
				}
				theTrait.setEnvironment(this);
				theTrait.basicSetName(objectSpace.symbolFor(traitName));
			}
			if (configureTrait != null) configureTrait(theTrait);
			return theTrait;
		}

		#endregion

		#region Namespace protocol: Import Specification

		protected Dictionary<String, ESSpecifImportSpec> SpecificImports {
			get {return specificImports;}
		}

		protected List<ESImportSpec>  GeneralImports {
			get {return generalImports;}
		}

		public void initializeSpecificImports() {
			specificImports = new Dictionary<String, ESSpecifImportSpec>();
		}

		public void initializeGeneralImports() {
			generalImports = new List<ESImportSpec>();
		}

		public void initializeImports() {
			initializeSpecificImports();
			initializeGeneralImports();
		}

		public void addImport(ESImportSpec importSpec) {
			if (!GeneralImports.Contains(importSpec)) GeneralImports.Add(importSpec);
		}

		public void addImports(List<ESImportSpec> importSpecs) {
			foreach (var importSpec in importSpecs) addImport(importSpec);
		}

		public void importAs(String localName, ESSpecifImportSpec specificImport) {
			SpecificImports[localName] = specificImport;
		}

		#endregion

		#region Namespace protocol: Lookup/Search

		protected ESBindingReference importedBindingAt(String key, AccessPrivilegeLevel requestorPrivilege, HashSet<ESNamespace> transitiveClosure) {
			ESBindingReference binding = null;
			ESSpecifImportSpec specificImportSpec;
			if (specificImports.TryGetValue(key, out specificImportSpec)) {
				if ((int)requestorPrivilege >= (int)specificImportSpec.AccessPrivilegeLevel) {
					var source = specificImportSpec.Source as ESNamespace;
					binding = source.searchForBindingAt(
							specificImportSpec.NameInSource, 
							AccessPrivilegeLevel.Public,
							specificImportSpec.Transitivity,
							transitiveClosure);
				}
			}
			if (binding == null) {
				foreach (var importSpec in generalImports) {
					if ((int)requestorPrivilege >= (int)importSpec.AccessPrivilegeLevel) {
						var source = importSpec.Source as ESNamespace;
						binding = source.searchForBindingAt(
								key, 
								AccessPrivilegeLevel.Public,
								importSpec.Transitivity,
								transitiveClosure);
						if (binding != null) return binding;
					}
				}
			}
			return binding;
		}

		protected virtual ESBindingReference inheritedBindingAt(String key, AccessPrivilegeLevel requestorPrivilege, ImportTransitivity importTransitivity, HashSet<ESNamespace> transitiveClosure) {
			return environment == null ? 
				null : 
				environment.searchForBindingAt(
						key, 
						(AccessPrivilegeLevel)Math.Min((int)requestorPrivilege, (int)AccessPrivilegeLevel.InHierarchy), 
						importTransitivity,
						transitiveClosure);
		}

		protected ESBindingReference searchForBindingAt(String key, AccessPrivilegeLevel requestorPrivilege, ImportTransitivity importTransitivity, HashSet<ESNamespace> transitiveClosure) {
			if (ReferenceEquals(transitiveClosure,	null)) {
				transitiveClosure = new HashSet<ESNamespace>(new ESNamesapceIdentityComparator());
				transitiveClosure.Add(this);
			} else if (transitiveClosure.Contains(this)) {
				return null;
			} else {
				transitiveClosure.Add(this);
			}
			ESBindingReference binding = localBindingAt(key, requestorPrivilege);
			if (binding != null) return binding;
			if (importTransitivity == ImportTransitivity.Transitive) {
				binding = importedBindingAt(key, (AccessPrivilegeLevel)Math.Min((int)requestorPrivilege, (int)AccessPrivilegeLevel.InHierarchy), transitiveClosure);
				if (binding != null) return binding;
			}
			return inheritedBindingAt(key, requestorPrivilege, ImportTransitivity.Transitive, transitiveClosure);
		}

		public ESBindingReference localBindingAt(String key, AccessPrivilegeLevel requestorPrivilege) {
			ESBindingReference association;
			if (bindings.TryGetValue(key, out association)) {
				if ((int)requestorPrivilege >= (int)association.AccessPrivilegeLevel) return association;
			}
			return null;
		}

		public ESBindingReference bindingAt(String key, AccessPrivilegeLevel requestorRights, ImportTransitivity importTransitivity, FuncNs.Func<ESBindingReference> notFoundAction) {
			ESBindingReference binding = searchForBindingAt(key, requestorRights, importTransitivity, null);
			return binding ?? (notFoundAction == null ? null : notFoundAction());
		}

		#endregion

		#region Interoperability
		
		public override int GetHashCode() {
			return RuntimeHelpers.GetHashCode(this);
		}

		public override bool Equals(Object comparand) {
			return this == comparand;
		}      
		
		public override bool Equals(ESObject comparand) {
			return this == comparand;
		}    

		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			append(NameString);
		}

		#endregion

		public override T valueBy<T>(Operation<T> operation) {
		    return operation.applyToNamespace(this);
		}

		public new class Primitives : PrimitiveDomain {

			protected override void bindToObjectSpace() {
				domainClass = objectSpace.NamespaceClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.Namespace;}
			}

			#region Primitive Definitions
		
			public Object _asNamespace_(Object receiver) {
				return (ESNamespace)receiver;
			}
		
			public Object _name_(Object receiver) {
				return ((ESNamespace)receiver).Name;
			}
		
			public Object _setName_(Object receiver, Object name) {
				((ESNamespace)receiver).setName(objectSpace.asESSymbol(name));
				return receiver;
			}
		
			public Object _renameFromTo_(Object receiver, Object prevName, Object newName) {
				((ESNamespace)receiver).renameFromTo(objectSpace.asESSymbol(prevName), objectSpace.asESSymbol(newName));
				return receiver;
			}
		
			public Object _hostSystemName_ (Object receiver) {
				return objectSpace.symbolFor(((ESNamespace)receiver).HostSystemName);
			}
		
			public Object _setHostSystemName_ (Object receiver, Object hostSystemName) {
				((ESNamespace)receiver).setHostSystemName(asHostString(hostSystemName));
				return receiver;
			}
		
			public Object _hostSystemNamespace_ (Object receiver) {
				return objectSpace.symbolFor(((ESNamespace)receiver).HostSystemNamespace);
			}
		
			public Object _setHostSystemNamespace_ (Object receiver, Object hostSystemNamespace) {
				((ESNamespace)receiver).setHostSystemNamespace(asHostString(hostSystemNamespace));
				return receiver;
			}
		
			public Object _assemblyPathname_ (Object receiver) {
				return objectSpace.symbolFor(((ESNamespace)receiver).AssemblyPathname);
			}
		
			public Object _setAssemblyPathname_ (Object receiver, Object assemblyPathname) {
				((ESNamespace)receiver).AssemblyPathname = asHostString(assemblyPathname);
				return receiver;
			}
		
			public Object _assemblyName_ (Object receiver) {
				return objectSpace.symbolFor(((ESNamespace)receiver).AssemblyNameString);
			}
		
			public Object _setAssemblyName_ (Object receiver, Object assemblyName) {
				((ESNamespace)receiver).AssemblyNameString = asHostString(assemblyName);
				return receiver;
			}
		
			public Object _pathname_(Object receiver) {
				return ((ESNamespace)receiver).pathname();
			}

			public Object _qualifiedHostSystemName_(Object receiver) {
				return ((ESNamespace)receiver).QualifiedHostSystemName;
			}

			public Object _environment_(Object receiver) {
				return ((ESNamespace)receiver).Environment;
			}
		
			public Object _setEnvironment_(Object receiver, Object environment) {
				((ESNamespace)receiver).setEnvironment((ESNamespace)environment);
				return receiver;
			}

			public Object _size_(Object receiver) {
				return ((ESNamespace)receiver).size();
			}

			public Object _isEmpty_(Object receiver) {
				return ((ESNamespace)receiver).isEmpty;
			}

			public Object _associationAt_(Object receiver, Object key) {
				return ((ESNamespace)receiver).associationAt(asHostString(key));
			}

			public Object _associationAtIfAbsent_(Object receiver, Object key, Object absentAction) {
				ESBindingReference association = ((ESNamespace)receiver).associationAtIfAbsent(asHostString(key), null);
				return association ?? (absentAction == null ? null : asFunctor0(absentAction)());
			}

			public Object _at_(Object receiver, Object key) {
				return ((ESNamespace)receiver).at(asHostString(key)).Value;
			}

			public Object _atIfAbsent_(Object receiver, Object key, Object absentAction) {
				BindingHandle reference = ((ESNamespace)receiver).atIfAbsent(asHostString(key), null);
				return reference == null ? asFunctor0(absentAction)() : reference.Value;
			}

			public Object _atIfAbsentPut_(Object receiver, Object key, Object computeValueToBeAdded) {
				FuncNs.Func<Object> f0 = asFunctor0(computeValueToBeAdded);
				return ((ESNamespace)receiver).atIfAbsentPut(asHostString(key), () => new DirectBindingHandle(f0()));
			}

			public Object _add_(Object receiver, Object newAssociation) {
				((ESNamespace)receiver).add((ESBindingReference)newAssociation);
				return receiver;
			}

			public Object _atPut_(Object receiver, Object key, Object newValue) {
				((ESNamespace)receiver).atPut(asHostString(key), new DirectBindingHandle(newValue));
				return newValue;
			}

			public Object _atImmutablyPut_(Object receiver, Object key, Object newValue) {
				((ESNamespace)receiver).atImmutablyPut(asHostString(key), new DirectBindingHandle(newValue));
				return newValue;
			}

			public Object _includesKey_(Object receiver, Object key) {
				return ((ESNamespace)receiver).includesKey(asHostString(key));
			}

			public Object _removeKey_(Object receiver, Object key) {
				return ((ESNamespace)receiver).removeKey(asHostString(key));
			}

			public Object _removeKeyIfAbsent_(Object receiver, Object key, Object notFoundAction) {
				return ((ESNamespace)receiver).removeKeyIfAbsent(asHostString(key), asFunctor0(notFoundAction));
			}

			public Object _associationsDo_(Object receiver, Object enumerator1) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator1);
				((ESNamespace)receiver).associationsDo(association => f1(association));
				return receiver;
			}

			public Object _keysDo_(Object receiver, Object enumerator1) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator1);
				((ESNamespace)receiver).keysDo(key => f1(objectSpace.asESSymbol(key)));
				return receiver;
			}

			public Object _valuesDo_(Object receiver, Object enumerator1) {
				FuncNs.Func<Object, Object> f1 = asFunctor1(enumerator1);
				((ESNamespace)receiver).valuesDo(reference => f1(reference.Value));
				return receiver;
			}

			public Object _keysAndValuesDo_(Object receiver, Object enumerator2) {
				FuncNs.Func<Object, Object, Object> f2 = asFunctor2(enumerator2);
				((ESNamespace)receiver).keysAndValuesDo((key, reference) => f2(objectSpace.asESSymbol(key), reference.Value));
				return receiver;
			}

			public Object _declareVariable_(Object receiver, Object key, Object accessPrivilegeLevel, Object onCollisionAction) {
				AccessPrivilegeLevel access;
				try {
					access = (AccessPrivilegeLevel)Enum.Parse(typeof(AccessPrivilegeLevel), asHostString(accessPrivilegeLevel));
				} catch {
					throw new PrimInvalidOperandException("declare:withAccess:onCollision: <accessPrivilegeLevel> must be a Symbol or String identifying a valid access privilege level.");
				}
				var binding = ((ESNamespace)receiver).declareVariable(asHostString(key), access, null);
				return binding == null ?
					asFunctor0(onCollisionAction)() :
					binding;
			}

			public Object _declareConstant_(Object receiver, Object key, Object constantValue, Object accessPrivilegeLevel, Object onCollisionAction) {
				AccessPrivilegeLevel access;
				try {
					access = (AccessPrivilegeLevel)Enum.Parse(typeof(AccessPrivilegeLevel), asHostString(accessPrivilegeLevel));
				} catch {
					throw new PrimInvalidOperandException("declareConstant:withValue:access:onCollision: <accessPrivilegeLevel> must be a Symbol or String identifying a valid access privilege level.");
				}
				var binding = ((ESNamespace)receiver).declareConstant(asHostString(key), constantValue, access, null);
				return binding == null ?
					asFunctor0(onCollisionAction)() :
					binding;
			}

			public Object _defineNamespace_(Object receiver, Object nsName, Object accessPrivilegeLevel, Object configureClass) {
				AccessPrivilegeLevel access;
				try {
					access = (AccessPrivilegeLevel)Enum.Parse(typeof(AccessPrivilegeLevel), asHostString(accessPrivilegeLevel));
				} catch {
					throw new PrimInvalidOperandException("defineClass:withAccess:configure: <accessPrivilegeLevel> must be a Symbol or String identifying a valid access privilege level.");
				}
				return ((ESNamespace)receiver).defineNamespace(asHostString(nsName), access, asFunctor1(configureClass));
			}

			public Object _defineClass_(Object receiver, Object className, Object accessPrivilegeLevel, Object configureClass) {
				AccessPrivilegeLevel access;
				try {
					access = (AccessPrivilegeLevel)Enum.Parse(typeof(AccessPrivilegeLevel), asHostString(accessPrivilegeLevel));
				} catch {
					throw new PrimInvalidOperandException("defineClass:withAccess:configure: <accessPrivilegeLevel> must be a Symbol or String identifying a valid access privilege level.");
				}
				return ((ESNamespace)receiver).defineClass(asHostString(className), access, asFunctor1(configureClass));
			}

			public Object _defineTrait_(Object receiver, Object traitName, Object accessPrivilegeLevel, Object configureTrait) {
				AccessPrivilegeLevel access;
				try {
					access = (AccessPrivilegeLevel)Enum.Parse(typeof(AccessPrivilegeLevel), asHostString(accessPrivilegeLevel));
				} catch {
					throw new PrimInvalidOperandException("defineTrait:withAccess:configure: <accessPrivilegeLevel> must be a Symbol or String identifying a valid access privilege level.");
				}
				return ((ESNamespace)receiver).defineTrait(asHostString(traitName), access, asFunctor1(configureTrait));
			}

			public Object _initializeSpecificImports_(Object receiver) {
				((ESNamespace)receiver).initializeSpecificImports();
				return receiver;
			}

			public Object _initializeGeneralImports_(Object receiver) {
				((ESNamespace)receiver).initializeGeneralImports();
				return receiver;
			}

			public Object _importWithAccess_(Object receiver, Object sourceNsSpecification, Object accessPrivilegeLevel) {
				AccessPrivilegeLevel access;
				try {
					access = (AccessPrivilegeLevel)Enum.Parse(typeof(AccessPrivilegeLevel), asHostString(accessPrivilegeLevel));
				} catch {
					throw new PrimInvalidOperandException("import:withAccess: <accessPrivilegeLevel> must be a Symbol or String identifying a valid access privilege level.");
				}
				((ESNamespace)receiver).addImport(new ESImportSpec(objectSpace.asESNamespace(sourceNsSpecification), access, ImportTransitivity.Intransitive));
				return receiver;
			}

			public Object _importTransitiveWithAccess_(Object receiver, Object sourceNsSpecification, Object accessPrivilegeLevel) {
				AccessPrivilegeLevel access;
				try {
					access = (AccessPrivilegeLevel)Enum.Parse(typeof(AccessPrivilegeLevel), asHostString(accessPrivilegeLevel));
				} catch {
					throw new PrimInvalidOperandException("importTransitive:withAccess: <accessPrivilegeLevel> must be a Symbol or String identifying a valid access privilege level.");
				}
				((ESNamespace)receiver).addImport(new ESImportSpec(objectSpace.asESNamespace(sourceNsSpecification), access, ImportTransitivity.Transitive));
				return receiver;
			}

			public Object _importFromAsWithAccess_(Object receiver, Object nameInSource, Object sourceNsSpecification, Object localName, Object accessPrivilegeLevel) {
				AccessPrivilegeLevel access;
				try {
					access = (AccessPrivilegeLevel)Enum.Parse(typeof(AccessPrivilegeLevel), asHostString(accessPrivilegeLevel));
				} catch {
					throw new PrimInvalidOperandException("import:withAccess: <accessPrivilegeLevel> must be a Symbol or String identifying a valid access privilege level.");
				}
				((ESNamespace)receiver).importAs(asHostString(localName), new ESSpecifImportSpec(objectSpace.asESNamespace(sourceNsSpecification), access, ImportTransitivity.Intransitive, objectSpace.asESSymbol(nameInSource)));
				return receiver;
			}

			public Object _importTransitiveFromAsWithAccess_(Object receiver, Object nameInSource, Object sourceNsSpecification, Object localName, Object accessPrivilegeLevel) {
				AccessPrivilegeLevel access;
				try {
					access = (AccessPrivilegeLevel)Enum.Parse(typeof(AccessPrivilegeLevel), asHostString(accessPrivilegeLevel));
				} catch {
					throw new PrimInvalidOperandException("import:withAccess: <accessPrivilegeLevel> must be a Symbol or String identifying a valid access privilege level.");
				}
				((ESNamespace)receiver).importAs(asHostString(localName), new ESSpecifImportSpec(objectSpace.asESNamespace(sourceNsSpecification), access, ImportTransitivity.Transitive, objectSpace.asESSymbol(nameInSource)));
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

				publishPrimitive("asNamespace",							new FuncNs.Func<Object, Object>(_asNamespace_));

				publishPrimitive("name",							new FuncNs.Func<Object, Object>(_name_));
				publishPrimitive("name:",							new FuncNs.Func<Object, Object, Object>(_setName_));
				publishPrimitive("renameFrom:to:",						new FuncNs.Func<Object, Object, Object, Object>(_renameFromTo_));

				publishPrimitive("hostSystemName",						new FuncNs.Func<Object, Object>(_hostSystemName_));
				publishPrimitive("hostSystemName:",						new FuncNs.Func<Object, Object, Object>(_setHostSystemName_));
				publishPrimitive("hostSystemNamespace",						new FuncNs.Func<Object, Object>(_hostSystemNamespace_));
				publishPrimitive("hostSystemNamespace:",					new FuncNs.Func<Object, Object, Object>(_setHostSystemNamespace_));
				publishPrimitive("assemblyPathname",						new FuncNs.Func<Object, Object>(_assemblyPathname_));
				publishPrimitive("assemblyPathname:",						new FuncNs.Func<Object, Object, Object>(_setAssemblyPathname_));
				publishPrimitive("assemblyName",						new FuncNs.Func<Object, Object>(_assemblyName_));
				publishPrimitive("assemblyName:",						new FuncNs.Func<Object, Object, Object>(_setAssemblyName_));

				publishPrimitive("pathname",							new FuncNs.Func<Object, Object>(_pathname_));
				publishPrimitive("qualifiedHostSystemName",					new FuncNs.Func<Object, Object>(_qualifiedHostSystemName_));

				publishPrimitive("environment",							new FuncNs.Func<Object, Object>(_environment_));
				publishPrimitive("environment:",						new FuncNs.Func<Object, Object, Object>(_setEnvironment_));

				publishPrimitive("declareVariable:withAccess:onCollision:",			new FuncNs.Func<Object, Object, Object, Object, Object>(_declareVariable_));
				publishPrimitive("declareConstant:withValue:access:onCollision:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_declareConstant_));
				publishPrimitive("defineNamespace:withAccess:configure:",			new FuncNs.Func<Object, Object, Object, Object, Object>(_defineNamespace_));
				publishPrimitive("defineClass:withAccess:configure:",				new FuncNs.Func<Object, Object, Object, Object, Object>(_defineClass_));
				publishPrimitive("defineTrait:withAccess:configure:",				new FuncNs.Func<Object, Object, Object, Object, Object>(_defineTrait_));

				publishPrimitive("initializeSpecificImports",					new FuncNs.Func<Object, Object>(_initializeSpecificImports_));
				publishPrimitive("initializeGeneralImports",					new FuncNs.Func<Object, Object>(_initializeGeneralImports_));
				publishPrimitive("import:withAccess:",						new FuncNs.Func<Object, Object, Object, Object>(_importWithAccess_));
				publishPrimitive("importTransitive:withAccess:",				new FuncNs.Func<Object, Object, Object, Object>(_importTransitiveWithAccess_));
				publishPrimitive("import:from:as:withAccess:",					new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_importFromAsWithAccess_));
				publishPrimitive("importTransitive:from:as:withAccess:",			new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_importTransitiveFromAsWithAccess_));

			}

		}

	}

}
