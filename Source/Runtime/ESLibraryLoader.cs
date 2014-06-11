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
using System.Collections.Generic;
using System.IO;
using EssenceSharp.UtilityServices;
#endregion

namespace EssenceSharp.Runtime {

	public class ESLibraryLoader {

		public static bool load(ESKernel kernel, DirectoryInfo baseDirectory, bool beVerbose, out List<ESNamespace> rootNamespaces) {
			var libraryLoader = new ESLibraryLoader(kernel, baseDirectory);
			libraryLoader.IsVerbose = beVerbose;
			return libraryLoader.load(out rootNamespaces);
		}

		public static bool load(ESKernel kernel, DirectoryInfo baseDirectory, bool beVerbose, bool recurseIntoNestedNamespaces, out List<ESNamespace> rootNamespaces) {
			var libraryLoader = new ESLibraryLoader(kernel, baseDirectory, recurseIntoNestedNamespaces);
			libraryLoader.IsVerbose = beVerbose;
			return libraryLoader.load(out rootNamespaces);
		}

		public static bool load(ESKernel kernel, ESNamespace baseEnvironment, DirectoryInfo baseDirectory, bool beVerbose, bool recurseIntoNestedNamespaces, out List<ESNamespace> rootNamespaces) {
			var libraryLoader = new ESLibraryLoader(kernel, baseEnvironment, baseDirectory, recurseIntoNestedNamespaces);
			libraryLoader.IsVerbose = beVerbose;
			return libraryLoader.load(out rootNamespaces);
		}

		protected bool						isVerbose			= false;
		protected ESKernel					kernel				= null;
		protected ESNamespace					baseEnvironment			= null;
		protected DirectoryInfo					baseDirectory			= null;
		protected bool						recurseIntoNestedNamespaces	= false;
		protected List<NamespaceFactory>			namespaceFactories		= null; 
		protected List<BehaviorFactory>				behaviorFactories		= null; 


		public ESLibraryLoader(ESKernel kernel) {
			this.kernel	= kernel;
			baseEnvironment	= kernel.RootNamespace;
		}

		public ESLibraryLoader(ESKernel kernel, DirectoryInfo baseDirectory) : this(kernel) {
			this.baseDirectory				= baseDirectory;
		}

		public ESLibraryLoader(ESKernel kernel, DirectoryInfo baseDirectory, bool recurseIntoNestedNamespaces) : this(kernel, baseDirectory) {
			this.baseDirectory				= baseDirectory;
			this.recurseIntoNestedNamespaces		= recurseIntoNestedNamespaces;
		}

		public ESLibraryLoader(ESKernel kernel, ESNamespace baseEnvironment, DirectoryInfo baseDirectory, bool recurseIntoNestedNamespaces) : this(kernel, baseDirectory, recurseIntoNestedNamespaces) {
			this.baseEnvironment				= baseEnvironment ?? kernel.RootNamespace;
		}

		#region Public protocol

		public bool IsVerbose {
			get { return isVerbose;}
			set { isVerbose = value;}
		}

		public ESKernel Kernel {
			get {return kernel;}
		}

		public ESNamespace BaseEnvironment {
			get {return baseEnvironment;}
			set {baseEnvironment = value ?? kernel.RootNamespace;}
		}

		public DirectoryInfo BaseDirectory {
			get {return baseDirectory;}
			set {baseDirectory = value;}
		}

		public bool RecurseIntoNestedNamespaces {
			get {return recurseIntoNestedNamespaces;}
			set {recurseIntoNestedNamespaces = value;}
		}

		public virtual bool load(out List<ESNamespace> rootNamespaces) {

			rootNamespaces = null;
			if (baseDirectory == null || !baseDirectory.Exists) {
				Console.WriteLine("Library directory '" + baseDirectory.FullName + "' is not accessible.");
				return false;
			}

			FileSystemInfo[] files = baseDirectory.GetFileSystemInfos();
			if (files.Length < 1) return true;
			var rootNamespaceDirectories = new List<DirectoryInfo>();
			foreach (var file in files) {
				var fileName = file.Name;
				if ((file.Attributes & FileAttributes.Directory) == FileAttributes.Directory ) {
					rootNamespaceDirectories.Add((DirectoryInfo)file);
				}
			}

			rootNamespaces = new List<ESNamespace>();
			namespaceFactories = new List<NamespaceFactory>(); 
			behaviorFactories = new List<BehaviorFactory>(); 

			foreach (var rootNamespaceDirectory in rootNamespaceDirectories) {
				ESNamespace rootNamespace;
				if (!loadNamespace(baseEnvironment, rootNamespaceDirectory, out rootNamespace)) return false;
				rootNamespaces.Add(rootNamespace);
			}

			foreach (var factory in behaviorFactories) 
				if (!factory.declareAll()) return false;
			foreach (var factory in namespaceFactories) 
				if (!factory.declareAll()) return false;
			foreach (var factory in behaviorFactories) 
				if (!factory.configureAll()) return false;
			foreach (var factory in namespaceFactories) 
				if (!factory.configureAll()) return false;
			foreach (var factory in behaviorFactories) 
				if (!factory.compileAll()) return false;
			foreach (var factory in behaviorFactories) 
				if (!factory.initializeAll()) return false;
			foreach (var factory in namespaceFactories) 
				if (!factory.initializeAll()) return false;
			return true;

		}

		#endregion

		#region Internal operations

		protected virtual bool loadNamespace(ESNamespace baseEnvironment, DirectoryInfo baseDirectory, out ESNamespace loadedNamespace) {
			loadedNamespace = null;
			if (baseDirectory == null || !baseDirectory.Exists) {
				Console.WriteLine("Namespace directory '" + baseDirectory.FullName + "' is not accessible.");
				return false;
			}
			var name = baseDirectory.Name;
			String namePrefix;
			int genericArity;
			if (!TypeName.parseUnqualifiedName(name, out namePrefix, out genericArity, (prefix, errorDescription) => {})) return true;
			if (genericArity > 0) {
				name = namePrefix + "`" + genericArity.ToString();
			}
			FileSystemInfo[] files = baseDirectory.GetFileSystemInfos();
			FileSystemInfo namespaceConfigurationFile = null;
			FileSystemInfo classConfigurationFile = null;
			FileSystemInfo metaclassConfigurationFile = null;
			FileSystemInfo classInitializationFile = null;
			FileSystemInfo metaclassInitializationFile = null;
			FileSystemInfo instanceMethodsFile = null;
			FileSystemInfo classMethodsFile = null;
			var variables = new List<FileSystemInfo>();
			var constants = new List<FileSystemInfo>();
			var nestedNamespaces = recurseIntoNestedNamespaces ? new List<DirectoryInfo>() : null;
			bool isClass = false;
			NamespaceFactory namespaceFactory;
			BehaviorFactory behaviorFactory;

			foreach (var file in files) {
				var fileName = file.Name;
				if ((file.Attributes & FileAttributes.Directory) == FileAttributes.Directory ) {
					if (recurseIntoNestedNamespaces) nestedNamespaces.Add((DirectoryInfo)file);
				} else {
					switch (fileName) {
						case "configure.namespace":
							namespaceConfigurationFile = file;
							break;
						case "configure.class":
							classConfigurationFile = file;
							isClass = true;
							break;
						case "configure.metaclass":
							metaclassConfigurationFile = file;
							isClass = true;
							break;
						case "initialize.class":
							classInitializationFile = file;
							isClass = true;
							break;
						case "initialize.metaclass":
							metaclassInitializationFile = file;
							isClass = true;
							break;
						case "instance.methods":
							isClass = true;
							instanceMethodsFile = file;
							break;
						case "class.methods":
							isClass = true;
							classMethodsFile = file;
							break;
						default:
							var extension = file.Extension;
							switch (extension) {
								case ".variable":
									variables.Add(file);
									break;
								case ".constant":
									constants.Add(file);
									break;
							}
							break;
					}
				}
			}

			bool success = true;
			var nameSymbol = kernel.SymbolRegistry.symbolFor(name);
			if (isClass) {
				namespaceFactory = behaviorFactory = new BehaviorFactory(kernel, baseEnvironment, nameSymbol);
				behaviorFactories.Add(behaviorFactory);
				behaviorFactory.ClassConfigurationFile = (FileInfo)classConfigurationFile;
				behaviorFactory.MetaclassConfigurationFile = (FileInfo)metaclassConfigurationFile;
				behaviorFactory.ClassInitializationFile = (FileInfo)classInitializationFile;
				behaviorFactory.MetaclassInitializationFile = (FileInfo)metaclassInitializationFile;
				behaviorFactory.InstanceMethodsFile = (FileInfo)instanceMethodsFile;
				behaviorFactory.ClassMethodsFile = (FileInfo)classMethodsFile;
			} else {
				namespaceFactory = new NamespaceFactory(kernel, baseEnvironment, nameSymbol);
				namespaceFactories.Add(namespaceFactory);
			}
			namespaceFactory.IsVerbose = IsVerbose;
			if (!namespaceFactory.declareNamespace()) return false;
			loadedNamespace = namespaceFactory.ThisNamespace;
			namespaceFactory.NamespaceConfigurationFile = (FileInfo)namespaceConfigurationFile;
			foreach (var constFile in constants) {
				namespaceFactory.addConstantInitializerFile((FileInfo)constFile);
			}
			foreach (var varFile in variables) {
				namespaceFactory.addVariableInitializerFile((FileInfo)varFile);
			}
			if (recurseIntoNestedNamespaces) {
				ESNamespace childNamespace;
				foreach (var nsFile in nestedNamespaces) {
					if (!loadNamespace(loadedNamespace, nsFile, out childNamespace)) return false;
				}
			}
			return success;
		}

		#endregion

	}

	public class NamespaceFactory {
	
		public class Association<KeyType, ValueType> {
			protected KeyType key;
			protected ValueType value;

			public Association(ValueType value) {
				this.value = value;
			}

			public Association(KeyType key) {
				this.key = key;
			}

			public Association(KeyType key, ValueType value) {
				this.key = key;
				this.value = value;
			}

			public KeyType Key {
				get {return key;}
				set {key = value;}
			}

			public ValueType Value {
				get {return value;}
				set {this.value = value;}
			}

		}

		protected bool							isVerbose			= false;
		protected ESKernel						kernel				= null;
		protected ESNamespace						baseEnvironment;
		protected ESSymbol						name;
		protected FileInfo						namespaceConfigurationFile;
		protected ESNamespace						thisNamespace;
		protected List<Association<ESBindingReference, FileInfo>>	constants			= new List<Association<ESBindingReference, FileInfo>>();
		protected List<Association<ESBindingReference, FileInfo>>	variables			= new List<Association<ESBindingReference, FileInfo>>();


		public NamespaceFactory(ESKernel kernel, ESNamespace baseEnvironment, ESSymbol name) {
			this.kernel		= kernel;
			this.baseEnvironment	= baseEnvironment;
			this.name		= name;
		}

		public bool IsVerbose {
			get { return isVerbose;}
			set { isVerbose = value;}
		}

		public ESKernel Kernel {
			get {return kernel;}
		}

		public ESNamespace BaseEnvironment {
			get {return baseEnvironment;}
		}

		public ESSymbol Name {
			get {return name;}
		}

		public ESNamespace ThisNamespace {
			get {return thisNamespace;}
		}

		public FileInfo NamespaceConfigurationFile {
			get {return namespaceConfigurationFile;}
			set {namespaceConfigurationFile = value;}
		}

		public void addConstantInitializerFile(FileInfo constantInitializationFile) {
			constants.Add(new Association<ESBindingReference, FileInfo>(null, constantInitializationFile));
		}

		public void addVariableInitializerFile(FileInfo variableInitializationFile) {
			variables.Add(new Association<ESBindingReference, FileInfo>(null, variableInitializationFile));
		}

		public virtual bool declareNamespace() {
			if (IsVerbose) Console.WriteLine("Declaring namespace: #" + Name);
			if (baseEnvironment == null) {
				thisNamespace = kernel.newNamespace();
				thisNamespace.setName(Name);
			} else {
				thisNamespace = baseEnvironment.defineNamespace(Name, AccessPrivilegeLevel.Public, null);
			}
			return true;
		}

		public virtual bool declareAll() {
			if (!declareConstants()) return false;
			if (!declareVariables()) return false;
			return true;
		}

		public virtual bool declareConstants() {
			foreach (var constantAssoc in constants) {
				if (!declareConstant(constantAssoc)) return false;
			}
			return true;
		}

		public virtual bool declareVariables() {
			foreach (var variableAssoc in variables) {
				if (!declareVariable(variableAssoc)) return false;
			}
			return true;
		}

		public virtual bool configureAll() {
			configureNamespace();
			return true;
		}

		public virtual bool initializeAll() {
			initializeConstants();
			initializeVariables();
			return true;
		}

		public virtual bool initializeConstants() {
			foreach (var constantAssoc in constants) {
				if (!initializeConstant(constantAssoc)) return false;
			}
			return true;
		}

		public virtual bool initializeVariables() {
			foreach (var variableAssoc in variables) {
				if (!initializeVariable(variableAssoc)) return false;
			}
			return true;
		}

		protected virtual AccessPrivilegeLevel DefaultConstantAccessPrivilege {
			get {return AccessPrivilegeLevel.Public;}
		}

		protected virtual AccessPrivilegeLevel DefaultVariableAccessPrivilege {
			get {return AccessPrivilegeLevel.Public;}
		}

		protected virtual bool declareConstant(Association<ESBindingReference, FileInfo> association) {
			var file = association.Value;
			var name = ESFileUtility.filenameWithoutExtensionsFrom(file);
			if (IsVerbose) Console.WriteLine("Declaring constant: " + name + " in: " + ThisNamespace.NameString);
			var binding = ThisNamespace.declareVariable(name, DefaultConstantAccessPrivilege, null);
			if (binding == null) {
				Console.WriteLine("Name binding collision: " + name + " in: " + ThisNamespace.PathnameString);
				return false;
			}
			association.Key = binding;
			return true;
		}

		protected virtual bool declareVariable(Association<ESBindingReference, FileInfo> association) {
			var file = association.Value;
			var name = ESFileUtility.filenameWithoutExtensionsFrom(file);
			if (IsVerbose) Console.WriteLine("Declaring variable: " + name + " in: " + ThisNamespace.NameString);
			var binding = ThisNamespace.declareVariable(name, DefaultVariableAccessPrivilege, null);
			if (binding == null) {
				Console.WriteLine("Name binding collision: " + name + " in: " + ThisNamespace.PathnameString);
				return false;
			}
			association.Key = binding;
			return true;
		}

		protected virtual bool configureNamespace() {
			if (NamespaceConfigurationFile == null) return true;
			if (IsVerbose) Console.WriteLine("Configuring namespace: " + ThisNamespace.PathnameString);
			return evaluateAsSelfExpression(ThisNamespace, ThisNamespace, NamespaceConfigurationFile);
		}

		protected virtual bool initializeConstant(Association<ESBindingReference, FileInfo> association) {
			var binding = association.Key;
			var file = association.Value;
			if (IsVerbose) Console.WriteLine("Initializing constant: " + binding.Key + " in: " + ThisNamespace.PathnameString);
			var success = evaluateAsSelfExpression(ThisNamespace, binding, file);
			if (success) binding.beImmutable();
			return success;
		}

		protected virtual bool initializeVariable(Association<ESBindingReference, FileInfo> association) {
			var binding = association.Key;
			var file = association.Value;
			if (IsVerbose) Console.WriteLine("Initializing variable: " + binding.Key + " in: " + ThisNamespace.PathnameString);
			var success = evaluateAsSelfExpression(ThisNamespace, binding, file);
			return success;
		}

		protected virtual bool evaluateAsSelfExpression(ESNamespace environment, Object selfValue, FileInfo file) {
			Object value;
			return kernel.evaluateAsSelfExpression(file, environment, selfValue, out value);
		}

	}

	public class BehaviorFactory : NamespaceFactory {
		protected ESClass thisClass;
		protected ESMetaclass thisMetaclass;
		protected FileInfo classConfigurationFile;
		protected FileInfo metaclassConfigurationFile;
		protected FileInfo instanceMethodsFile;
		protected FileInfo classMethodsFile;
		protected FileInfo classInitializationFile;
		protected FileInfo metaclassInitializationFile;

		public BehaviorFactory(ESKernel kernel, ESNamespace baseEnvironment, ESSymbol name) : base(kernel, baseEnvironment, name) {
			this.baseEnvironment = baseEnvironment;
		}

		public ESClass ThisClass {
			get {return thisClass;}
		}

		public ESMetaclass ThisMetaclass {
			get {return thisMetaclass;}
		}
		public FileInfo ClassConfigurationFile {
			get {return classConfigurationFile;}
			set {classConfigurationFile = value;}
		}

		public FileInfo MetaclassConfigurationFile {
			get {return metaclassConfigurationFile;}
			set {metaclassConfigurationFile = value;}
		}

		public FileInfo InstanceMethodsFile {
			get {return instanceMethodsFile;}
			set {instanceMethodsFile = value;}
		}

		public FileInfo ClassMethodsFile {
			get {return classMethodsFile;}
			set {classMethodsFile = value;}
		}

		public FileInfo ClassInitializationFile {
			get {return classInitializationFile;}
			set {classInitializationFile = value;}
		}

		public FileInfo MetaclassInitializationFile {
			get {return metaclassInitializationFile;}
			set {metaclassInitializationFile = value;}
		}


		protected override AccessPrivilegeLevel DefaultConstantAccessPrivilege {
			get {return AccessPrivilegeLevel.InHierarchy;}
		}

		protected override AccessPrivilegeLevel DefaultVariableAccessPrivilege {
			get {return AccessPrivilegeLevel.InHierarchy;}
		}

		public override bool declareNamespace() {
			if (!declareClassAndMetaclass()) return false;
			thisNamespace = ThisClass;
			return true;
		}

		public virtual bool declareClassAndMetaclass() {
			if (IsVerbose) Console.WriteLine("Declaring class: #" + Name);
			thisClass = null; 
			if (baseEnvironment == null) {
				thisClass = kernel.newClass(Name, ObjectStateArchitecture.Stateless);
			} else {
				thisClass = baseEnvironment.defineClass(Name, AccessPrivilegeLevel.Public, null);
			}
			thisMetaclass = (ESMetaclass)thisClass.Class;
			return true;
		}

		public virtual bool compileAll() {
			if (!compileMethodsFor(ThisMetaclass, ClassMethodsFile)) return false;
			if (!compileMethodsFor(ThisClass, InstanceMethodsFile)) return false;
			return true;
		}

		public override bool configureAll() {
			if (!base.configureAll()) return false;
			if (!configureMetaclass()) return false;
			return configureClass();
		}

		protected virtual bool configureClass() {
			if (ClassConfigurationFile == null) return true;
			if (IsVerbose) Console.WriteLine("Configuring class : " + ThisClass.PathnameString);
			return evaluateAsSelfExpression(ThisClass, ThisClass, ClassConfigurationFile);
		}

		protected virtual bool configureMetaclass() {
			if (MetaclassConfigurationFile == null) return true;
			if (IsVerbose) Console.WriteLine("Configuring metaclass: " + ThisMetaclass.PathnameString);
			return evaluateAsSelfExpression(ThisMetaclass, ThisMetaclass, MetaclassConfigurationFile);
		}

		protected virtual bool compileMethodsFor(ESBehavior methodClass, FileInfo methodDeclarationFile) {
			if (methodDeclarationFile == null) return true;
			if (IsVerbose) Console.WriteLine("Compiling methods for: " + methodClass.PathnameString);
			return evaluateAsSelfExpression(methodClass, methodClass, methodDeclarationFile);
		}

		public override bool initializeAll() {
			if (!base.initializeAll()) return false;
			if (!initializeMetaclass()) return false;
			return initializeClass();
		}

		protected virtual bool initializeClass() {
			ThisClass.validate();
			if (ClassInitializationFile == null) return true;
			if (IsVerbose) Console.WriteLine("Initializing class : " + ThisClass.PathnameString);
			return evaluateAsSelfExpression(ThisClass, ThisClass, ClassInitializationFile);
		}

		protected virtual bool initializeMetaclass() {
			ThisMetaclass.validate();
			if (MetaclassInitializationFile == null) return true;
			if (IsVerbose) Console.WriteLine("Initializing metaclass: " + ThisMetaclass.PathnameString);
			return evaluateAsSelfExpression(ThisMetaclass, ThisMetaclass, MetaclassInitializationFile);
		}


	}

}
