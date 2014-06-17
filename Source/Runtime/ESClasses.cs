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
	
	public class ESBehavior : ESNamespace {

		#region Static variables and methods

		#region InstanceArchitecture Constraints

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
				case ObjectStateArchitecture.Behavior:
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

				case ObjectStateArchitecture.Class:
				case ObjectStateArchitecture.Metaclass:
					switch (superclassInstanceArchitecture) {
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

		public static bool isAdoptedType(ObjectStateArchitecture instanceArchitecture) {
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

		internal static readonly ESSymbol[]						emptyInstanceVariableNames	 	= new ESSymbol[0];
		internal static readonly Dictionary<ESSymbol, long>				emptyInstanceVariableIndexes 		= new Dictionary<ESSymbol, long>();

		protected static readonly Object[]						emtpyObjArray	 			= new Object[0];

		#endregion

		protected ESKernel								kernel					= null; 
		protected ESBehavior								superclass				= null;
		protected System.Collections.Generic.HashSet<ESBehavior>			subclasses				= null;
		protected IDictionary<ESSymbol, ESMethod> 					methodDictionary 			= null;
		protected IDictionary<long, IDictionary<String, ESMethod>>			hostSystemMethodDictionary		= null;
		protected ObjectEqualityComparator						instanceEqualityComparator		= null;
		protected FuncNs.Func<Object, Object, Object>					instanceEqualityFunctor			= null;
		protected FuncNs.Func<Object, Object>						instanceHashFunctor			= null;
		protected Type									instanceType				= null;
		protected bool									isInstanceTypeLocked			= false;

		protected ObjectStateArchitecture 						instanceArchitecture			= ObjectStateArchitecture.NamedSlots;
		protected bool									constraintsMustBeSatisfied		= false;
		protected bool									isInstanceArchitectureLocked		= false;
		protected ESSymbol[]								instanceVariableNames			= emptyInstanceVariableNames;
		protected Dictionary<ESSymbol, long>						instanceVariableIndexes			= emptyInstanceVariableIndexes;
			
		internal ESBehavior(ObjectStateArchitecture instanceArchitecture) : base(null) {
			this.instanceArchitecture = instanceArchitecture;
			initialize();
		}

		protected ESBehavior(ESBehavior metaClass) : base(metaClass) {
			initialize();
			instanceVariableNames = emptyInstanceVariableNames;
		}

		public ESBehavior(ESBehavior metaClass, ESKernel kernel) : this(metaClass) {
			Kernel = kernel;
		}
			
		protected ESBehavior(ESBehavior metaClass, ObjectStateArchitecture instanceArchitecture, ESBehavior superclass) 
					: base(metaClass) {
			this.instanceArchitecture = instanceArchitecture;
			initialize();
			Superclass = superclass;
		}
			
		public ESBehavior(ESBehavior metaClass, ESKernel kernel, ObjectStateArchitecture instanceArchitecture, ESBehavior superclass) 
					: this(metaClass, instanceArchitecture, superclass) {
			Kernel = kernel;
		}
			
		protected ESBehavior(ESBehavior metaClass, ObjectStateArchitecture instanceArchitecture, ESSymbol[] instanceVarnames, ESBehavior superclass) 
					: this(metaClass, instanceArchitecture, superclass) {
			setInstanceVariableNames(instanceVarnames);
		}
			
		public ESBehavior(ESBehavior metaClass, ESKernel kernel, ObjectStateArchitecture instanceArchitecture, ESSymbol[] instanceVarnames, ESBehavior superclass) 
					: this(metaClass, kernel, instanceArchitecture, superclass) {
			setInstanceVariableNames(instanceVarnames);
		}
		
		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.Behavior;}
		}
		
		public override bool IsBehavior {
			get {return true;}
		}
		
		public override ESBehavior asESBehavior() {
			return this;
		}

		public virtual bool IsHostSystemMetaclass {
			get {return false;}
		}

		#region Internal Protocol

		protected void initialize() {
			methodDictionary = newMethodDictionary();
			hostSystemMethodDictionary = newHostSystemMethodDictionary();
			subclasses = new System.Collections.Generic.HashSet<ESBehavior>();
		}

		internal ESKernel Kernel {
			get {return kernel;}
			set {
				if (kernel == value) return;
				if (kernel != null) unbindFromKernel();
				kernel = value;
				if (kernel != null) bindToKernel();
			}
		}

		protected virtual void unbindFromKernel() {
			// By default, do nothing
		}

		protected virtual void bindToKernel() {
			instanceEqualityComparator = kernel.newObjectEqualityComparator();
			instanceEqualityFunctor = instanceEqualityComparator.EqualityFunctor;
			instanceHashFunctor = instanceEqualityComparator.HashFunctor;
		}
		
		protected IDictionary<ESSymbol, ESMethod> newMethodDictionary() {
			return new Dictionary<ESSymbol, ESMethod>();
		}

		protected IDictionary<long, IDictionary<String, ESMethod>> newHostSystemMethodDictionary() {
			return new Dictionary<long, IDictionary<String, ESMethod>>();
		}

		public IEqualityComparer<Object> ObjectIdentityComparator {
			get {return kernel.ObjectIdentityComparator;}
		}

		public IEqualityComparer<Object> InstanceEqualityComparator {
			get {return instanceEqualityComparator;}
		}

		protected override String AnonymousName {
			get {return "AnAnonymousBehavior";}
		}

		protected override void incrementVersion() {
			base.incrementVersion();
			foreach (var subclass in subclasses) subclass.incrementVersion();
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

		public bool InstancesCanHaveNamedSlots {
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
						return superclass ==  null ? false : superclass.SubclassInstancesCanHaveNamedSlots;
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
						return superclass == null ? true : superclass.SubclassInstancesCanHaveNamedSlots;
					default:
						return true;
				}
			}
		}

		protected virtual Type ReflectionType {
			get { return InstanceType;}
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
			if (!isAdoptedType(InstanceArchitecture) && !instanceType.isEssenceSharpType()) InstanceArchitecture = ObjectStateArchitecture.HostSystemObject;
			basicBindToInstanceType();
		}

		protected void invalidateInstanceType() {
			setInstanceType(getInstanceType());
		}

		#region getInstanceType()

		protected Type getInstanceType() {
			switch (InstanceArchitecture) {
				case ObjectStateArchitecture.Abstract:
					return typeof(ESInitiallyMutableObject);		
				case ObjectStateArchitecture.Stateless:
					return TypeGuru.esObjectType;		
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
				case ObjectStateArchitecture.NamedSlots:
					return TypeGuru.esNamedSlotsObjectType;		
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
				case ObjectStateArchitecture.Block:
					return TypeGuru.esBlockType;		
				case ObjectStateArchitecture.Method:
					return TypeGuru.esMethodType;		
				case ObjectStateArchitecture.Behavior:
					return TypeGuru.esBehaviorType;		
				case ObjectStateArchitecture.Class:
					return TypeGuru.esClassType;	
				case ObjectStateArchitecture.Metaclass:
					return TypeGuru.esMetaclassType;		
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

		public Type typeFromQualifiedName(String qualifiedTypeName, bool raiseExceptionOnErrorOrNotFound) {
			Type type = null;
			var assembly = Assembly;
			if (assembly != null) {
				type = assembly.GetType(qualifiedTypeName, false);
				if (type != null) return type;
			}
			return Type.GetType(qualifiedTypeName, raiseExceptionOnErrorOrNotFound);
		}

		public ESSymbol nameInEnvironmentFor(Type hostSystemType) {
			return kernel.symbolFor(new TypeName(hostSystemType).NameWithGenericArguments);
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

		public bool instanceHasSameValueAs(Object instance, Object comparand) {
			if (instanceEqualityFunctor == null) return ReferenceEquals(instance, comparand);
			return (bool)instanceEqualityFunctor(instance, comparand);
		}

		public int instanceHashCode(Object instance) {
			if (instanceHashFunctor == null) return RuntimeHelpers.GetHashCode(instance);
			return (int)instanceHashFunctor(instance);
		}

		public void validate() {
			constraintsMustBeSatisfied = true;
			assertValidInheritanceStructure(Superclass);
		}

		#endregion

		#region Namespace Protocol
		
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

		protected ESBindingReference bindingForSubclassAt(String key, AccessPrivilegeLevel requestorPrivilege, ImportTransitivity importTransitivity, System.Collections.Generic.HashSet<ESNamespace> transitiveClosure) {
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

		protected override ESBindingReference inheritedBindingAt(String key, AccessPrivilegeLevel requestorPrivilege, ImportTransitivity importTransitivity, System.Collections.Generic.HashSet<ESNamespace> transitiveClosure) {
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

		#region Inheritance hierarchy

		protected void throwIncompatibleSuperclassException(ObjectStateArchitecture instanceArchitecture, ESBehavior incompatibleSuperclass) {
			throw new PrimInvalidOperandException("A class whose instance architecture is " + instanceArchitecture + " cannot have a superclass whose instance architecture is " + incompatibleSuperclass.InstanceArchitecture + ".");
		}

		protected void throwIncompatibleSuperclassException(ESBehavior incompatibleSuperclass) {
			throwIncompatibleSuperclassException(InstanceArchitecture, incompatibleSuperclass);
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

 		public bool HasSuperclass {
			get {return superclass != null;}
		}
		
		public ESBehavior Superclass {
			get {return superclass;}
			set {setSuperclass(value);}
		}

		protected bool canInheritFrom(ObjectStateArchitecture instanceArchitecture, ESBehavior aSuperclass) {
			if (aSuperclass == null) return true;
			if (!isValidInheritanceRelationship(instanceArchitecture, aSuperclass.InstanceArchitecture)) return false;
			return canInheritFrom(instanceArchitecture, aSuperclass.Superclass);
		}

		public bool canInheritFrom(ESBehavior aSuperclass) {
			return canInheritFrom(InstanceArchitecture, aSuperclass);
		}

		protected void setSuperclass(ESBehavior newSuperclass) {
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
			invalidateSuperclass();
		}

		protected virtual void invalidateSuperclass() {
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
			subclass.Superclass = this;
		}

		public void removeSubclass(ESBehavior subclass) {
			if (subclass == null) return;
			if (subclasses.Remove(subclass)) subclass.Superclass = null;
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
			var newSuperclass = kernel.classForHostSystemType(systemSupertype);
			Superclass = newSuperclass;
		}

		public bool includesBehavior(ESBehavior aBehavior) {
			// The receiver may either be a subclass of <aBehavior>, or it may be identical to <aBehavior>
			if (ReferenceEquals(this, aBehavior)) return true;
			ESBehavior mySuperclass = Superclass;
			return mySuperclass == null ?
				false :
				mySuperclass.includesBehavior(aBehavior);
		}
		
		public bool inheritsFrom(ESBehavior aBehavior) {
			// The receiver must be a SUBCLASS of <aBehavior>
			if (aBehavior == null) return true;
			ESBehavior mySuperclass = Superclass;
			return mySuperclass == null ?
				false :
				mySuperclass.includesBehavior(aBehavior);
		}
		
		#endregion

		#region Named instance variables

		public ESSymbol[] InstanceVariableNames {
			get {return instanceVariableNames;}
			set {setInstanceVariableNames(value);}
		}
		
		protected void setInstanceVariableNames(ESSymbol[] instanceVarNames) {
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

		protected virtual void invalidateInstanceVariableNames() {
			recompile();
			base.incrementVersion();
			foreach (var subclass in subclasses) subclass.invalidateInstanceVariableNames();	
		}

		public ESSymbol basicInstVarNameAt(long index) {
			if (index >= instanceVariableNames.Length || index < 0) return null;
			return instanceVariableNames[index];
		}
		
		public ESSymbol instVarNameAt(long index) {
			if (index < 0) return null;
			long superInstSize = SuperInstSize;
			if (index < superInstSize) return superInstSize > 0 ? superclass.instVarNameAt(index) : null;
			index -= superInstSize;
			if (index >= instanceVariableNames.Length) return null;
			return instanceVariableNames[index];
		}
		
		public Dictionary<ESSymbol, long> InstanceVariableIndexes {
			get {if (instanceVariableIndexes == null) mapInstanceVariableNamesToIndexes();
				return instanceVariableIndexes;}
		}
		
		protected void mapInstanceVariableNamesToIndexes() {
			if (instanceVariableNames.Length > 0) {
				instanceVariableIndexes = new Dictionary<ESSymbol, long>();
				for (uint index = 0; index < instanceVariableNames.Length; index++) {
					instanceVariableIndexes[instanceVariableNames[index]] = index;
				}
			} else {
				instanceVariableIndexes = emptyInstanceVariableIndexes;
			}
		}
		
		public long instVarIndexFor(ESSymbol instanceVariableName) {
			if (instanceVariableIndexes == null) mapInstanceVariableNamesToIndexes();
			if (instanceVariableIndexes.Count < 1) return superclass == null ? -1 : superclass.instVarIndexFor(instanceVariableName);
			long index;
			if (instanceVariableIndexes.TryGetValue(instanceVariableName, out index)) {
				return index + SuperInstSize;
			} else {
				return superclass == null ? -1 : superclass.instVarIndexFor(instanceVariableName);
			}
		}
		
		public long BasicInstSize {
			get {return instanceVariableNames.Length;}
		}
		
		public long SuperInstSize {
			get {return superclass == null ? 0 : superclass.InstSize;}
		}
		
		public long InstSize {
			get {return SuperInstSize + BasicInstSize;}
		}
		
		public void allInstVarNamesAndIndexesDo(System.Action<ESSymbol, long> enumerator2) {
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
		
		#region Compiled Methods -- Smalltalk Protocol
		
		protected ESMethod basicCompiledMethodAt(ESSymbol selector, bool addIfAbsent) {
			ESMethod method;
			if (methodDictionary.TryGetValue(selector, out method)) {
				return method;
			} else if (addIfAbsent) {
				method = kernel.newMethod(selector, null, this);
				methodDictionary[selector] = method;
				return method;
			} else {
				return null;
			}
		}
		
		public ESMethod basicCompiledMethodAt(ESSymbol selector) {
			return basicCompiledMethodAt(selector, false);
		}
		
		public ESMethod compiledMethodAt(ESSymbol selector) {
			ESMethod method;
			if (methodDictionary.TryGetValue(selector, out method)) {
				return method;
			} else {
				return superclass == null ? null : superclass.compiledMethodAt(selector);
			}
		}

		public ESMethod addMethod(ESMethod newMethod) {
			return addMethodBoundToSystemSelector(newMethod, null);
		}
		
		public ESMethod addMethodBoundToSystemSelector(ESMethod newMethod, String systemSelector) {
			if (newMethod == null) {
				kernel.throwInvalidArgumentException(Class, "addMethod:", "newMethod", newMethod);
				return null;
			}
			ESSymbol selector = newMethod.Selector;
			if (selector == null) {
				kernel.throwInvalidArgumentException(Class, "addMethod:", "newMethod", newMethod);
				return null;
			}
			ESMethod residentMethod;
			if (methodDictionary.TryGetValue(selector, out residentMethod)) {
				residentMethod.become(newMethod);				
			} else {
				residentMethod = newMethod.newCopyIn(this);
				methodDictionary[selector] = residentMethod;
			}
			incrementVersion();

			if (systemSelector == null) return residentMethod;
			systemSelector = String.Intern(systemSelector);

			long numArgs = residentMethod.NumArgs;
			IDictionary<String, ESMethod> hostMethodDict;
			if (!hostSystemMethodDictionary.TryGetValue(numArgs, out hostMethodDict)) {
				hostMethodDict = new Dictionary<String, ESMethod>();
				hostSystemMethodDictionary[numArgs] = hostMethodDict;
			}
			hostMethodDict[systemSelector] = residentMethod;
			residentMethod.addToProtocol(kernel.symbolFor("host system API"));
			return residentMethod;
		}

		public bool removeSelector(ESSymbol selector) {
			if (methodDictionary.Remove(selector)) {
				incrementVersion();
				return true;
			}
			return false;
		}
		
		public bool includesSelector(ESSymbol selector) {
			return methodDictionary.ContainsKey(selector);
		}
		
		public bool canUnderstand(ESSymbol selector) {
			if (methodDictionary.ContainsKey(selector)) return true;
			ESBehavior mySuperclass = Superclass;
			if (mySuperclass == null) return false;
			return mySuperclass.canUnderstand(selector);
		}
		
		public long selectorCount() {
			return methodDictionary.Count;
		}
		
		public ESSymbol[] selectors() {
			ESSymbol[] symbols = new ESSymbol[methodDictionary.Count];
			int index = 0;
			foreach (var assoc in methodDictionary) {
				symbols[index++] = assoc.Key;
			}
			return symbols;
		}
		
		public void selectorsDo(FuncNs.Func<Object, Object> enumerator1) {
			foreach (var assoc in methodDictionary) {
				enumerator1(assoc.Key);
			}
		}

		public void selectorsAndMethodsDo(FuncNs.Func<Object, Object, Object> enumerator2) {
			foreach (var assoc in methodDictionary) {
				enumerator2(assoc.Key, assoc.Value);
			}
		}
		
		#endregion
		
		#region Compiled Methods -- Host System Protocol

		// Operations on methods whose selectors conform to host system (CLR) naming conventions

		public ESMethod compiledMethodAtSystemSelector(String systemSelector, long numArgs) {
			IDictionary<String, ESMethod> hostSysMethodDict;
			if (hostSystemMethodDictionary.TryGetValue(numArgs, out hostSysMethodDict)) {
				ESMethod method;
				if (hostSysMethodDict.TryGetValue(systemSelector, out method)) {
					return method;
				} else {
					return superclass == null ? null : superclass.compiledMethodAtSystemSelector(systemSelector, numArgs);
				}
			} else {
				return superclass == null ? null : superclass.compiledMethodAtSystemSelector(systemSelector, numArgs);
			}
		}
		
		public ESMethod bindMethodToSystemSelector(ESSymbol essenceSelector, String systemSelector) {
			if (essenceSelector == null) {
				kernel.throwInvalidArgumentException(Class, "bindMethod:toSystemSelector:", "essenceSelector", essenceSelector);
				return null;
			}
			if (systemSelector == null) {
				kernel.throwInvalidArgumentException(Class, "bindMethod:toSystemSelector:", "systemSelector", systemSelector);
				return null;
			}
			systemSelector = String.Intern(systemSelector);
			ESMethod mappedMethod;
			if (methodDictionary.TryGetValue(essenceSelector, out mappedMethod)) return addMethodBoundToSystemSelector(mappedMethod, systemSelector);

			var methodHeaderBuilder = new StringBuilder();
			long arity = 0;
			switch (essenceSelector.Type) {
				case SymbolType.Identifier:
					methodHeaderBuilder.Append(essenceSelector.PrimitiveValue);
					break;
				case SymbolType.BinaryMessageSelector:
					arity = 1;
					methodHeaderBuilder.Append(essenceSelector.PrimitiveValue);
					methodHeaderBuilder.Append(" a1");
					break;
				case SymbolType.Keyword:
					arity = essenceSelector.NumArgs;
					var argIndex = 1;
					essenceSelector.keywordsDo(keyword => {
						methodHeaderBuilder.Append(keyword); 
						methodHeaderBuilder.Append(": a"); 
						methodHeaderBuilder.Append(argIndex++); 
						methodHeaderBuilder.Append(" ");});
					break;
				default:
					kernel.throwInvalidArgumentException(Class, "bindMethod:toSystemSelector:", "essenceSelector", essenceSelector);
					break;
			}
			var methodDeclarationBuilder = new StringBuilder();
			methodDeclarationBuilder.AppendLine(methodHeaderBuilder.ToString());
			methodDeclarationBuilder.AppendLine();
			methodDeclarationBuilder.Append("        ^self doesNotUnderstand: (Message selector: #");
			methodDeclarationBuilder.Append(essenceSelector.PrimitiveValue);
			methodDeclarationBuilder.Append(" arguments: {");
			for (var i = 0; i < arity; i++) {
				methodDeclarationBuilder.Append("a");
				methodDeclarationBuilder.Append(i + 1);
				if (arity - i > 1) methodDeclarationBuilder.Append(". ");
			}
			methodDeclarationBuilder.AppendLine("})");
			if (!kernel.compileMethod(new StringReader(methodDeclarationBuilder.ToString()), this, kernel.symbolFor("error handling"), out mappedMethod)) {
				throw new InternalSystemError("Unexpected compilation error in ESBehavior.bindMethodToSystemSelector() -- probably not user or programmer error");
			}
			return addMethodBoundToSystemSelector(mappedMethod, systemSelector);
		}
		
		public bool unbindMethodFromSystemSelector(String systemSelector, long numArgs) {
			if (systemSelector == null) return false;
			IDictionary<String, ESMethod> hostSysMethodDict;
			if (!hostSystemMethodDictionary.TryGetValue(numArgs, out hostSysMethodDict)) return false;
			if (hostSysMethodDict.Remove(systemSelector)) {
				incrementVersion();
				return true;
			}
			return false;
		}
		
		public bool includesSystemSelector(String systemSelector, long numArgs) {
			IDictionary<String, ESMethod> hostMethodDict;
			if (!hostSystemMethodDictionary.TryGetValue(numArgs, out hostMethodDict)) return false;
			return hostMethodDict.ContainsKey(systemSelector);
		}
		
		public bool canUnderstandSystemMessage(String systemSelector, long numArgs) {
			IDictionary<String, ESMethod> hostSysMethodDict;
			if (hostSystemMethodDictionary.TryGetValue(numArgs, out hostSysMethodDict)) {
				if (hostSysMethodDict.ContainsKey(systemSelector)) return true;
			}
			ESBehavior mySuperclass = Superclass;
			if (mySuperclass == null) return false;
			return mySuperclass.canUnderstandSystemMessage(systemSelector, numArgs);
		}
		
		public long systemSelectorCount() {
			long count = 0;
			foreach (var hostSysMethodDict in hostSystemMethodDictionary) count += hostSysMethodDict.Value.Count;
			return count;
		}
		
		public void systemSelectorsDo(FuncNs.Func<Object, Object, Object> enumerator2) {
			foreach (var numArgsAssoc in hostSystemMethodDictionary) {
				foreach (var selectorAssoc in numArgsAssoc.Value)
				enumerator2(selectorAssoc.Key, numArgsAssoc.Key);
			}
		}

		public void systemSelectorsAndMethodsDo(FuncNs.Func<Object, Object, Object, Object> enumerator3) {
			foreach (var numArgsAssoc in hostSystemMethodDictionary) 
				foreach (var selectorAssoc in numArgsAssoc.Value)
					enumerator3(selectorAssoc.Key, numArgsAssoc.Key, selectorAssoc.Value);
		}
		
		#endregion

		#region Compiling Methods

		public ESMethod compileMethod(ESSymbol protocol, TextReader sourceStream) {
			ESMethod method;
			if (kernel.compileMethod(sourceStream, this, protocol, out method)) {
				addMethod(method);
				return method;
			} else {
				return null;
			}
		}

		public void recompile() {
			foreach (var kvp in methodDictionary) kvp.Value.recompile();
		}

		public void recompileAll() {
			recompile();
			foreach (var subclass in subclasses) subclass.recompileAll();			
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
				case ObjectStateArchitecture.IndexedObjectSlots:
					isInstanceArchitectureLocked = true;
					return new ESArray(this, 0);
				case ObjectStateArchitecture.NamedSlots:
					isInstanceArchitectureLocked = true;
					return new ESNamedSlotsObject(this);
				case ObjectStateArchitecture.Behavior:
					isInstanceArchitectureLocked = true;
					return new ESBehavior(this, kernel);
				case ObjectStateArchitecture.Class:
					isInstanceArchitectureLocked = true;
					return new ESClass(Architecture == ObjectStateArchitecture.Metaclass ? this : kernel.newMetaclass());
				case ObjectStateArchitecture.Metaclass:
					isInstanceArchitectureLocked = true;
					return new ESMetaclass(this, kernel);
										
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

					case ObjectStateArchitecture.Symbol:
						isInstanceArchitectureLocked = true;
						return kernel.symbolFor(asHostString(arg));

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

		public override DynamicMetaObject GetMetaObject(Expression parameter) {
			return new ESBehaviorDynamicMetaObject(parameter, BindingRestrictions.Empty, this, Class);
		}
		
		public override int GetHashCode() {
			return RuntimeHelpers.GetHashCode(this);
		}

		public override bool Equals(Object comparand) {
			return this == comparand;
		}      
		
		public override bool Equals(ESObject comparand) {
			return this == comparand;
		}    
  
		public override T valueBy<T>(Operation<T> operation) {
		    return operation.applyToBehavior(this);
		}

		public new class Primitives : PrimitiveDomain {

			protected override void bindToKernel() {
				domainClass = kernel.BehaviorClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.Behavior;}
			}

			#region Primitive Definitions
		
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
		
			public Object _superclass_(Object receiver) {
				return ((ESBehavior)receiver).Superclass;
			}
		
			public Object _setSuperclass_(Object receiver, Object superclass) {
				((ESBehavior)receiver).Superclass = (ESBehavior)superclass;
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

			public Object _includesBehavior_(Object receiver, Object aBehavior) {
				return ((ESBehavior)receiver).includesBehavior((ESBehavior)aBehavior);
			}

			public Object _inheritsFrom_(Object receiver, Object aBehavior) {
				return ((ESBehavior)receiver).inheritsFrom((ESBehavior)aBehavior);
			}

			public Object _instanceEqualityComparer_(Object receiver) {
				return ((ESBehavior)receiver).InstanceEqualityComparator;
			}

			public Object _newObjectEqualityComparer_(Object receiver) {
				return ((ESBehavior)receiver).Kernel.newObjectEqualityComparator();
			}

			public Object _basicCompiledMethodAt_(Object receiver, Object selector) {
				return ((ESBehavior)receiver).basicCompiledMethodAt(kernel.asESSymbol(selector));
			}

			public Object _compiledMethodAt_(Object receiver, Object selector) {
				return ((ESBehavior)receiver).compiledMethodAt(kernel.asESSymbol(selector));
			}

			public Object _addMethod_(Object receiver, Object method) {
				return ((ESBehavior)receiver).addMethod((ESMethod)method);
			}	

			public Object _addMethodBoundToSystemSelector_(Object receiver, Object method, Object systemSelector) {
				return ((ESBehavior)receiver).addMethodBoundToSystemSelector((ESMethod)method, asHostString(systemSelector));
			}	

			public Object _protocolMethod_(Object receiver, Object protocol, Object method) {
				var compiledMethod = (ESMethod)method;
				compiledMethod.addToProtocol(kernel.asESSymbol(protocol));
				return ((ESBehavior)receiver).addMethod(compiledMethod);
			}	

			public Object _protocolSystemSelectorMethod_(Object receiver, Object protocol, Object systemSelector, Object method) {
				var compiledMethod = (ESMethod)method;
				compiledMethod.addToProtocol(kernel.asESSymbol(protocol));
				return ((ESBehavior)receiver).addMethodBoundToSystemSelector(compiledMethod, asHostString(systemSelector));
			}	

			public Object _compileMethodFromString_(Object receiver, Object protocol, Object methodText) {
				var methodString = asHostString(methodText);
				return ((ESBehavior)receiver).compileMethod(kernel.asESSymbol(protocol), new StringReader(methodString));
			}

			public Object _removeSelector_(Object receiver, Object selector) {
				return ((ESBehavior)receiver).removeSelector(kernel.asESSymbol(selector));
			}

			public Object _includesSelector_(Object receiver, Object selector) {
				return ((ESBehavior)receiver).includesSelector(kernel.asESSymbol(selector));
			}

			public Object _canUnderstand_(Object receiver, Object selector) {
				return ((ESBehavior)receiver).canUnderstand(kernel.asESSymbol(selector));
			}

			public Object _selectorCount_(Object receiver) {
				ESBehavior theReceiver = (ESBehavior)receiver;
				return theReceiver.selectorCount();
			}

			public Object _selectors_(Object receiver) {
				ESBehavior theReceiver = (ESBehavior)receiver;
				return kernel.instanceFrom(theReceiver.selectors());
			}

			public Object _selectorsDo_(Object receiver, Object enumerator1) {
				((ESBehavior)receiver).selectorsDo(asFunctor1(enumerator1));
				return receiver;
			}

			public Object _selectorsAndMethodsDo_(Object receiver, Object enumerator2) {
				((ESBehavior)receiver).selectorsAndMethodsDo(asFunctor2(enumerator2));
				return receiver;
			}

			public Object _compiledMethodAtSystemSelector_(Object receiver, Object systemSelector, Object numArgs) {
				return ((ESBehavior)receiver).compiledMethodAtSystemSelector(asHostString(systemSelector), asHostLong(numArgs));
			}

			public Object _bindMethodAtToSystemSelector_(Object receiver, Object essenceSelector, Object systemSelector) {
				return ((ESBehavior)receiver).bindMethodToSystemSelector(kernel.asESSymbol(essenceSelector), asHostString(systemSelector));
			}

			public Object _unbindMethodFromSystemSelector_(Object receiver, Object systemSelector, Object numArgs) {
				return ((ESBehavior)receiver).unbindMethodFromSystemSelector(asHostString(systemSelector), asHostLong(numArgs));
			}

			public Object _includesSystemSelector_(Object receiver, Object systemSelector, Object numArgs) {
				return ((ESBehavior)receiver).includesSystemSelector(asHostString(systemSelector), asHostLong(numArgs));
			}

			public Object _systemSelectorCount_(Object receiver) {
				return ((ESBehavior)receiver).systemSelectorCount();
			}

			public Object _systemSelectorsDo_(Object receiver, Object enumerator2) {
				((ESBehavior)receiver).systemSelectorsDo(asFunctor2(enumerator2));
				return receiver;
			}

			public Object _systemSelectorsAndMethodsDo_(Object receiver, Object enumerator3) {
				((ESBehavior)receiver).systemSelectorsAndMethodsDo(asFunctor3(enumerator3));
				return receiver;
			}

			public Object _instanceArchitecture_(Object receiver) {
				return kernel.asESSymbol(((ESBehavior)receiver).InstanceArchitecture.ToString());
			}
		
			public Object _setInstanceArchitecture_(Object receiver, Object instanceArchitecture) {
				ObjectStateArchitecture architecture;
				try {
					architecture = (ObjectStateArchitecture)Enum.Parse(typeof(ObjectStateArchitecture), kernel.asESSymbol(instanceArchitecture));
				} catch {
					throw new PrimitiveFailException("instanceArchitecture: <instanceArchitecture> must be a Symbol or String identifying a valid object state architecture.");
				}
				((ESBehavior)receiver).InstanceArchitecture = architecture;
				return receiver;
			}
		
			public Object _instanceVariableNames_(Object receiver) {
				return kernel.newArray(Array.ConvertAll<ESSymbol, Object>(((ESBehavior)receiver).InstanceVariableNames, symbol => symbol));
			}
		
			public Object _setInstanceVariableNames_(Object receiver, Object instanceVariableNames) {
				Object[] namesArray = asHostArray<Object>(instanceVariableNames);
				ESSymbol[] symbolArray = Array.ConvertAll<Object, ESSymbol>(namesArray, new Converter<Object, ESSymbol>(kernel.asESSymbol));
				((ESBehavior)receiver).InstanceVariableNames = symbolArray;
				return receiver;
			}
		
			public Object _basicInstVarNameAt_(Object receiver, Object index) {
				return ((ESBehavior)receiver).basicInstVarNameAt(asHostLong(index) - 1);
			}
		
			public Object _instVarNameAt_(Object receiver, Object index) {
				return ((ESBehavior)receiver).instVarNameAt(asHostLong(index) - 1);
			}

			public Object _instVarIndexFor_(Object receiver, Object instanceVariableName) {
				return ((ESBehavior)receiver).instVarIndexFor(kernel.asESSymbol(instanceVariableName)) + 1;
			}
		
			public Object _basicInstSize_(Object receiver) {
				return ((ESBehavior)receiver).BasicInstSize;
			}
		
			public Object _instSize_(Object receiver) {
				return ((ESBehavior)receiver).InstSize;
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

				publishPrimitive("instanceType",					new FuncNs.Func<Object, Object>(_instanceType_));
				publishPrimitive("instanceType:",					new FuncNs.Func<Object, Object, Object>(_setInstanceType_));
				publishPrimitive("superclass",						new FuncNs.Func<Object, Object>(_superclass_));
				publishPrimitive("superclass:",						new FuncNs.Func<Object, Object, Object>(_setSuperclass_));
				publishPrimitive("addSubclass:",					new FuncNs.Func<Object, Object, Object>(_addSubclass_));
				publishPrimitive("removeSubclass:",					new FuncNs.Func<Object, Object, Object>(_removeSubclass_));
				publishPrimitive("includesBehavior:",					new FuncNs.Func<Object, Object, Object>(_includesBehavior_));
				publishPrimitive("inheritsFrom:",					new FuncNs.Func<Object, Object, Object>(_inheritsFrom_));
				publishPrimitive("instanceEqualityComparer",				new FuncNs.Func<Object, Object>(_instanceEqualityComparer_));
				publishPrimitive("newObjectEqualityComparer",				new FuncNs.Func<Object, Object>(_newObjectEqualityComparer_));

				publishPrimitive("basicCompiledMethodAt:",				new FuncNs.Func<Object, Object, Object>(_basicCompiledMethodAt_));
				publishPrimitive("compiledMethodAt:",					new FuncNs.Func<Object, Object, Object>(_compiledMethodAt_));
				publishPrimitive("addMethod:",						new FuncNs.Func<Object, Object, Object>(_addMethod_));
				publishPrimitive("addMethod:systemSelector:",				new FuncNs.Func<Object, Object, Object, Object>(_addMethodBoundToSystemSelector_));
				publishPrimitive("protocol:method:",					new FuncNs.Func<Object, Object, Object, Object>(_protocolMethod_));
				publishPrimitive("protocol:systemSelector:method:",			new FuncNs.Func<Object, Object, Object, Object, Object>(_protocolSystemSelectorMethod_));
				publishPrimitive("compileMethodInProtocol:fromString:",			new FuncNs.Func<Object, Object, Object, Object>(_compileMethodFromString_));
				publishPrimitive("removeSelector:",					new FuncNs.Func<Object, Object, Object>(_removeSelector_));
				publishPrimitive("includesSelector:",					new FuncNs.Func<Object, Object, Object>(_includesSelector_));
				publishPrimitive("canUnderstand:",					new FuncNs.Func<Object, Object, Object>(_canUnderstand_));
				publishPrimitive("selectorCount",					new FuncNs.Func<Object, Object>(_selectorCount_));
				publishPrimitive("selectors",						new FuncNs.Func<Object, Object>(_selectors_));
				publishPrimitive("selectorsDo:",					new FuncNs.Func<Object, Object, Object>(_selectorsDo_));
				publishPrimitive("selectorsAndMethodsDo:",				new FuncNs.Func<Object, Object, Object>(_selectorsAndMethodsDo_));

				publishPrimitive("compiledMethodAtSystemSelector:numArgs:",		new FuncNs.Func<Object, Object, Object, Object>(_compiledMethodAtSystemSelector_));
				publishPrimitive("bindMethodAt:toSystemSelector:",			new FuncNs.Func<Object, Object, Object, Object>(_bindMethodAtToSystemSelector_));
				publishPrimitive("unbindindMethodFromSystemSelector:numArgs:",		new FuncNs.Func<Object, Object, Object, Object>(_unbindMethodFromSystemSelector_));
				publishPrimitive("includesSystemSelector:numArgs:",			new FuncNs.Func<Object, Object, Object, Object>(_includesSystemSelector_));
				publishPrimitive("systemSelectorCount",					new FuncNs.Func<Object, Object>(_systemSelectorCount_));
				publishPrimitive("systemSelectorsDo:",					new FuncNs.Func<Object, Object, Object>(_systemSelectorsDo_));
				publishPrimitive("systemSelectorsAndMethodsDo:",			new FuncNs.Func<Object, Object, Object>(_systemSelectorsAndMethodsDo_));

				publishPrimitive("instanceArchitecture",				new FuncNs.Func<Object, Object>(_instanceArchitecture_));
				publishPrimitive("instanceArchitecture:",				new FuncNs.Func<Object, Object, Object>(_setInstanceArchitecture_));
				publishPrimitive("instanceVariableNames",				new FuncNs.Func<Object, Object>(_instanceVariableNames_));
				publishPrimitive("instanceVariableNames:",				new FuncNs.Func<Object, Object, Object>(_setInstanceVariableNames_));
				publishPrimitive("basictInstVarNameAt:",				new FuncNs.Func<Object, Object, Object>(_basicInstVarNameAt_));
				publishPrimitive("instVarNameAt:",					new FuncNs.Func<Object, Object, Object>(_instVarNameAt_));
				publishPrimitive("instVarIndexFor:",					new FuncNs.Func<Object, Object, Object>(_instVarIndexFor_));
				publishPrimitive("basicInstSize",					new FuncNs.Func<Object, Object>(_basicInstSize_));
				publishPrimitive("instSize",						new FuncNs.Func<Object, Object>(_instSize_));

				publishPrimitive("new",							new FuncNs.Func<Object, Object>(_new_));
				publishPrimitive("newWithSize:",					new FuncNs.Func<Object, Object, Object>(_newWithSize_));
				publishPrimitive("newWithValue:",					new FuncNs.Func<Object, Object, Object>(_newWithValue_));

			}

		}
		
	}
	
	public class ESClass : ESBehavior {

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
			var metalass = metaClass as ESMetaclass;
			if (metalass == null) throw new PrimInvalidOperandException("The class of a Class must be a Metaclass.");
			Kernel = metalass.Kernel;
			base.setClass(metalass);
			metalass.adopt(this);
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
				kernel.bindHostSystemTypeTo(instanceType, this);
				bindToHostSystemSuperclasses();
			}
		}

		public override T valueBy<T>(Operation<T> operation) {
		    return operation.applyToClass(this);
		}

		public new class Primitives : PrimitiveDomain {

			protected override void bindToKernel() {
				domainClass = kernel.ClassClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.Class;}
			}

			public override void publishCanonicalPrimitives() {
			}

		}
		
	}
	
	public class ESMetaclass : ESBehavior {
			
		internal ESMetaclass(ESBehavior metaClass, ESKernel kernel) : this(metaClass, kernel, null, null) {
		}
			
		internal ESMetaclass(ESBehavior metaClass, ESKernel kernel, ESBehavior superclass) : this(metaClass, kernel, null, superclass) {
		}

		internal ESMetaclass(ESBehavior metaClass, ESKernel kernel, ESSymbol[] instanceVarnames, ESBehavior superclass) : this(metaClass, kernel, ObjectStateArchitecture.Class, instanceVarnames, superclass) {
		}

		private ESMetaclass(ESBehavior metaClass, ESKernel kernel, ObjectStateArchitecture instanceArchitecture, ESSymbol[] instanceVarnames, ESBehavior superclass) : base(metaClass, kernel, instanceArchitecture, instanceVarnames, superclass) {
		}

		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.Metaclass;}
		}

		public override ObjectStateArchitecture InstanceArchitecture {
			get {return ObjectStateArchitecture.Class;}
			set {	if (instanceArchitecture == value) return;
				instanceArchitecture = ObjectStateArchitecture.Class;
				invalidateInstanceType(); 
			}
		}
		
		public override bool IsMetaclass {
			get {return true;}
		}
		
		public override bool IsHostSystemMetaclass {
			get {	var baseClass = CanonicalInstance;
				if (baseClass == null) return false;
				return baseClass.InstanceArchitecture == ObjectStateArchitecture.HostSystemObject;}
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
			Superclass = CanonicalInstance.HasSuperclass ? CanonicalInstance.Superclass.Class : kernel.ClassClass;
			name = null;
		}

		protected override void invalidateSuperclass() {
			invalidateInstanceVariableNames();
		}
		
		protected override String AnonymousName {
			get {return "AnAnonymousMetaclass";}
		}

		public override ESSymbol Name {
			get {if (name == null) {
					name = HasCanonicalInstance ?
								kernel.symbolFor(CanonicalInstance.Name.PrimitiveValue + " class") :
								kernel.symbolFor(AnonymousName);
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
				return baseClass.InstanceArchitecture == ObjectStateArchitecture.HostSystemObject ?
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

		public new class Primitives : PrimitiveDomain {

			protected override void bindToKernel() {
				domainClass = kernel.MetaclassClass;
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
				publishPrimitive("canonicalInstance",				new FuncNs.Func<Object, Object>(_canonicalInstance_));
			}

		}

	}

}
