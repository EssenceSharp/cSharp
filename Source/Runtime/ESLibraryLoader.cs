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
using System.Text;
using Microsoft.Scripting;
using EssenceSharp.CompilationServices;
using EssenceSharp.UtilityServices;
#endregion

namespace EssenceSharp.Runtime {

	public class ESLibraryLoader {

		public static bool load(ESObjectSpace objectSpace, DirectoryInfo baseDirectory, bool beVerbose, out List<NamespaceObject> rootNamespaces) {
			var libraryLoader = new ESLibraryLoader(objectSpace, baseDirectory);
			libraryLoader.IsVerbose = beVerbose;
			return libraryLoader.load(out rootNamespaces);
		}

		public static bool load(ESObjectSpace objectSpace, DirectoryInfo baseDirectory, bool beVerbose, bool recurseIntoNestedNamespaces, out List<NamespaceObject> rootNamespaces) {
			var libraryLoader = new ESLibraryLoader(objectSpace, baseDirectory, recurseIntoNestedNamespaces);
			libraryLoader.IsVerbose = beVerbose;
			return libraryLoader.load(out rootNamespaces);
		}

		public static bool load(ESObjectSpace objectSpace, NamespaceObject baseEnvironment, DirectoryInfo baseDirectory, bool beVerbose, bool recurseIntoNestedNamespaces, out List<NamespaceObject> rootNamespaces) {
			var libraryLoader = new ESLibraryLoader(objectSpace, baseEnvironment, baseDirectory, recurseIntoNestedNamespaces);
			libraryLoader.IsVerbose = beVerbose;
			return libraryLoader.load(out rootNamespaces);
		}

		protected bool							isVerbose			= false;
		protected ESObjectSpace						objectSpace			= null;
		protected NamespaceObject					baseEnvironment			= null;
		protected DirectoryInfo						baseDirectory			= null;
		protected bool							recurseIntoNestedNamespaces	= false;
		protected List<NamespaceFactory>				namespaceFactories		= null; 
		protected List<TraitFactory>					traitFactories			= null; 
		protected List<ClassFactory>					classFactories			= null; 
		protected IDictionary<String, List<ESCompilationError>>		compilationErrors		= new Dictionary<String, List<ESCompilationError>>();


		public ESLibraryLoader(ESObjectSpace objectSpace) {
			this.objectSpace					= objectSpace;
			baseEnvironment						= objectSpace.RootNamespace;
		}

		public ESLibraryLoader(ESObjectSpace objectSpace, DirectoryInfo baseDirectory) : this(objectSpace) {
			this.baseDirectory					= baseDirectory;
		}

		public ESLibraryLoader(ESObjectSpace objectSpace, DirectoryInfo baseDirectory, bool recurseIntoNestedNamespaces) : this(objectSpace, baseDirectory) {
			this.baseDirectory					= baseDirectory;
			this.recurseIntoNestedNamespaces			= recurseIntoNestedNamespaces;
		}

		public ESLibraryLoader(ESObjectSpace objectSpace, NamespaceObject baseEnvironment, DirectoryInfo baseDirectory, bool recurseIntoNestedNamespaces) : this(objectSpace, baseDirectory, recurseIntoNestedNamespaces) {
			this.baseEnvironment					= baseEnvironment ?? objectSpace.RootNamespace;
		}

		#region Public protocol

		public bool IsVerbose {
			get { return isVerbose;}
			set { isVerbose = value;}
		}

		public ESObjectSpace ObjectSpace {
			get {return objectSpace;}
		}

		public NamespaceObject BaseEnvironment {
			get {return baseEnvironment;}
			set {baseEnvironment = value ?? objectSpace.RootNamespace;}
		}

		public DirectoryInfo BaseDirectory {
			get {return baseDirectory;}
			set {baseDirectory = value;}
		}

		public bool RecurseIntoNestedNamespaces {
			get {return recurseIntoNestedNamespaces;}
			set {recurseIntoNestedNamespaces = value;}
		}

		public virtual bool load(out List<NamespaceObject> rootNamespaces) {

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

			rootNamespaces = new List<NamespaceObject>();
			namespaceFactories = new List<NamespaceFactory>(); 
			traitFactories = new List<TraitFactory>(); 
			classFactories = new List<ClassFactory>(); 

			foreach (var rootNamespaceDirectory in rootNamespaceDirectories) {
				NamespaceObject rootNamespace;
				if (!loadNamespace(baseEnvironment, rootNamespaceDirectory, out rootNamespace)) return false;
				rootNamespaces.Add(rootNamespace);
			}

			foreach (var factory in namespaceFactories) 
				if (!factory.declareAll()) return false;
			foreach (var factory in traitFactories) 
				if (!factory.declareAll()) return false;
			foreach (var factory in classFactories) 
				if (!factory.declareAll()) return false;

			foreach (var factory in namespaceFactories) 
				if (!factory.configureAll()) return false;

			foreach (var factory in traitFactories) 
				if (!factory.configureAll()) return false;
			foreach (var factory in traitFactories) 
				if (!factory.compileAll()) return false;

			foreach (var factory in classFactories) 
				if (!factory.configureAll()) return false;

			foreach (var factory in classFactories) 
				if (!factory.compileAll()) return false;

			foreach (var factory in classFactories) {
				factory.ThisClass.activate();
				factory.ThisClass.Class.activate();
			}

			foreach (var factory in namespaceFactories) 
				if (!factory.initializeAll()) return false;

			foreach (var factory in traitFactories) 
				if (!factory.initializeAll()) return false;

			foreach (var factory in classFactories) 
				if (!factory.initializeAll()) return false;

			if (compilationErrors.Count > 0) {
				Console.WriteLine("Class library compilation error report:");
				Console.WriteLine("");
				foreach (var kvp in compilationErrors) {
					var pathname = kvp.Key;
					var errorList = kvp.Value;
					Console.Write("Location: ");
					Console.WriteLine(pathname);
					foreach (var error in errorList) {
						Console.Write("\t");
						Console.WriteLine(error);
					}
				}
				Console.Write(compilationErrors.Count);
				Console.WriteLine(" compilation errors.");
				Console.WriteLine("");
			}

			foreach (var factory in traitFactories) 
				factory.reportUndeclaredVariables();
			foreach (var factory in classFactories) 
				factory.reportUndeclaredVariables();

			Console.WriteLine("");

			return compilationErrors.Count < 1;

		}

		#endregion

		#region Internal operations

		protected enum DeclarationType {
			Namespace,
			TraitOrClass,
			Trait,
			Class
		}

		protected virtual bool loadNamespace(NamespaceObject baseEnvironment, DirectoryInfo baseDirectory, out NamespaceObject loadedNamespace) {
			loadedNamespace = null;
			if (baseDirectory == null || !baseDirectory.Exists) {
				Console.WriteLine("Namespace directory '" + baseDirectory.FullName + "' is not accessible.");
				return false;
			}
			var name = baseDirectory.Name;
			String namePrefix;
			int genericArity;
			if (!TypeName.parseUnqualifiedName(name, out namePrefix, out genericArity, (prefix, errorDescription) => {})) return true;
			if (namePrefix.Length != name.Length) {
				var extension = baseDirectory.Extension;
				if (extension.Length > 0) return true;
			}
			if (genericArity > 0) {
				name = namePrefix + "`" + genericArity.ToString();
			}
			FileSystemInfo[] files = baseDirectory.GetFileSystemInfos();
			FileSystemInfo namespaceConfigurationFile	= null;
			FileSystemInfo classConfigurationFile		= null;
			FileSystemInfo metaclassConfigurationFile	= null;
			FileSystemInfo initializerFile			= null;
			FileSystemInfo instanceMethodsFile		= null;
			FileSystemInfo classMethodsFile			= null;
			var variables					= new List<FileSystemInfo>();
			var constants					= new List<FileSystemInfo>();
			var nestedNamespaces				= recurseIntoNestedNamespaces ? new List<DirectoryInfo>() : null;
			DeclarationType type				= DeclarationType.Namespace;
			NamespaceFactory namespaceFactory		= null;
			TraitFactory traitFactory;
			ClassFactory behaviorFactory;

			var renameCount = 0;
			foreach (var fileEntry in files) {
				var fileName = fileEntry.Name.ToLower();
				if ((fileEntry.Attributes & FileAttributes.Directory) == FileAttributes.Directory ) {
					continue;
				} else {
					var sourceFile = (FileInfo)fileEntry;
					String targetPath;
					FileInfo targetFile;
					switch (fileName) {

						case "namespace.configure":
						case "configure.namespace":
							targetPath = Path.Combine(sourceFile.Directory.FullName, "namespace.def");
							targetFile = new FileInfo(targetPath);
							if (!targetFile.Exists) {
								sourceFile.MoveTo(targetPath);
								renameCount++;
							}
							break;

						case "trait.configure":
						case "configure.trait":
							targetPath = Path.Combine(sourceFile.Directory.FullName, "trait.def");
							targetFile = new FileInfo(targetPath);
							if (targetFile.Exists) {
								Console.WriteLine("Deprecated/retired file type; not renamed due to name conflict: " + sourceFile.FullName);
							} else {
								sourceFile.MoveTo(targetPath);
								renameCount++;
							}
							break;

						case "classTrait.configure":
						case "configure.classTrait":
							targetPath = Path.Combine(sourceFile.Directory.FullName, "classTrait.def");
							targetFile = new FileInfo(targetPath);
							if (targetFile.Exists) {
								Console.WriteLine("Deprecated/retired file type; not renamed due to name conflict: " + sourceFile.FullName);
							} else {
								sourceFile.MoveTo(targetPath);
								renameCount++;
							}
							break;

						case "class.configure":
						case "configure.class":
							targetPath = Path.Combine(sourceFile.Directory.FullName, "class.def");
							targetFile = new FileInfo(targetPath);
							if (targetFile.Exists) {
								Console.WriteLine("Deprecated/retired file type; not renamed due to name conflict: " + sourceFile.FullName);
							} else {
								sourceFile.MoveTo(targetPath);
								renameCount++;
							}
							break;

						case "metaclass.configure":
						case "configure.metaclass":
							targetPath = Path.Combine(sourceFile.Directory.FullName, "metaclass.def");
							targetFile = new FileInfo(targetPath);
							if (targetFile.Exists) {
								Console.WriteLine("Deprecated/retired file type; not renamed due to name conflict: " + sourceFile.FullName);
							} else {
								sourceFile.MoveTo(targetPath);
								renameCount++;
							}
							break;

						case "instance.methods":
							targetPath = Path.Combine(sourceFile.Directory.FullName, "methods.instance");
							targetFile = new FileInfo(targetPath);
							if (targetFile.Exists) {
								Console.WriteLine("Deprecated/retired file type; not renamed due to name conflict: " + sourceFile.FullName);
							} else {
								sourceFile.MoveTo(targetPath);
								renameCount++;
							}
							break;

						case "class.methods":
							targetPath = Path.Combine(sourceFile.Directory.FullName, "methods.class");
							targetFile = new FileInfo(targetPath);
							if (targetFile.Exists) {
								Console.WriteLine("Deprecated/retired file type; not renamed due to name conflict: " + sourceFile.FullName);
							} else {
								sourceFile.MoveTo(targetPath);
								renameCount++;
							}
							break;

						default:
							break;
					}
				}
			}

			if (renameCount > 0) files = baseDirectory.GetFileSystemInfos();

			foreach (var file in files) {
				var fileName = file.Name.ToLower();
				if ((file.Attributes & FileAttributes.Directory) == FileAttributes.Directory ) {
					if (recurseIntoNestedNamespaces) nestedNamespaces.Add((DirectoryInfo)file);
				} else {
					switch (fileName) {

						case "namespace.def":
							namespaceConfigurationFile = file;
							break;

						case "trait.def":
							if (type == DeclarationType.Class) { 
								Console.WriteLine(baseDirectory.FullName + " cannot be declared to be both a class and a trait.");
								return false;
							}
							type = DeclarationType.Trait;
							classConfigurationFile = file;
							break;

						case "classTrait.def":
							if (type == DeclarationType.Class) { 
								Console.WriteLine(baseDirectory.FullName + " cannot be declared to be both a class and a trait.");
								return false;
							}
							type = DeclarationType.Trait;
							metaclassConfigurationFile = file;
							break;

						case "class.def":
							if (type == DeclarationType.Trait) { 
								Console.WriteLine(baseDirectory.FullName + " cannot be declared to be both a class and a trait.");
								return false;
							}
							type = DeclarationType.Class;
							classConfigurationFile = file;
							break;

						case "metaclass.def":
							if (type == DeclarationType.Trait) { 
								Console.WriteLine(baseDirectory.FullName + " cannot be declared to be both a class and a trait.");
								return false;
							}
							type = DeclarationType.Class;
							metaclassConfigurationFile = file;
							break;

						case "initializer":
							initializerFile = file;
							break;

						case "methods.instance":
							if (type == DeclarationType.Namespace) { 
								type = DeclarationType.TraitOrClass;
							}
							instanceMethodsFile = file;
							break;

						case "methods.class":
							if (type == DeclarationType.Namespace) { 
								type = DeclarationType.TraitOrClass;
							}
							classMethodsFile = file;
							break;

						case "class.initialize":
						case "initialize.class":
						case "metaclass.initialize":
						case "initialize.metaclass":
							Console.WriteLine("Deprecated/retired file type; not processed: " + file.FullName);
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

			if (type == DeclarationType.TraitOrClass) type = DeclarationType.Class;

			bool success = true;
			var nameSymbol = objectSpace.SymbolRegistry.symbolFor(name);
			switch (type) {
				case DeclarationType.Trait:
					namespaceFactory = traitFactory = new TraitFactory(objectSpace, baseEnvironment, nameSymbol, compilationErrors);
					traitFactories.Add(traitFactory);
					traitFactory.ClassConfigurationFile = (FileInfo)classConfigurationFile;
					traitFactory.MetaclassConfigurationFile = (FileInfo)metaclassConfigurationFile;
					traitFactory.InstanceMethodsFile = (FileInfo)instanceMethodsFile;
					traitFactory.ClassMethodsFile = (FileInfo)classMethodsFile;
					break;
				case DeclarationType.Class:
					namespaceFactory = behaviorFactory = new ClassFactory(objectSpace, baseEnvironment, nameSymbol, compilationErrors);
					classFactories.Add(behaviorFactory);
					behaviorFactory.ClassConfigurationFile = (FileInfo)classConfigurationFile;
					behaviorFactory.MetaclassConfigurationFile = (FileInfo)metaclassConfigurationFile;
					behaviorFactory.InstanceMethodsFile = (FileInfo)instanceMethodsFile;
					behaviorFactory.ClassMethodsFile = (FileInfo)classMethodsFile;
					break;
				default:
				case DeclarationType.Namespace:
					namespaceFactory = new NamespaceFactory(objectSpace, baseEnvironment, nameSymbol, compilationErrors);
					namespaceFactories.Add(namespaceFactory);
					break;

			}
			namespaceFactory.InitializerFile = (FileInfo)initializerFile;
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
				NamespaceObject childNamespace;
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
		protected ESObjectSpace						objectSpace			= null;
		protected NamespaceObject					baseEnvironment;
		protected ESSymbol						name;
		protected FileInfo						namespaceConfigurationFile;
		protected FileInfo						initializerFile;
		protected ESNamespace						thisNamespace;
		protected List<Association<ESBindingReference, FileInfo>>	constants			= new List<Association<ESBindingReference, FileInfo>>();
		protected List<Association<ESBindingReference, FileInfo>>	variables			= new List<Association<ESBindingReference, FileInfo>>();
		protected IDictionary<String, List<ESCompilationError>>	compilationErrors;


		public NamespaceFactory(ESObjectSpace objectSpace, NamespaceObject baseEnvironment, ESSymbol name, IDictionary<String, List<ESCompilationError>> compilationErrors) {
			this.objectSpace	= objectSpace;
			this.baseEnvironment	= baseEnvironment;
			this.name		= name;
			this.compilationErrors	= compilationErrors;
		}

		public bool IsVerbose {
			get { return isVerbose;}
			set { isVerbose = value;}
		}

		public ESObjectSpace ObjectSpace {
			get {return objectSpace;}
		}

		public NamespaceObject BaseEnvironment {
			get {return baseEnvironment;}
		}

		public ESSymbol Name {
			get {return name;}
		}

		public NamespaceObject ThisNamespace {
			get {return thisNamespace;}
		}

		public FileInfo NamespaceConfigurationFile {
			get {return namespaceConfigurationFile;}
			set {namespaceConfigurationFile = value;}
		}

		public FileInfo InitializerFile {
			get {return initializerFile;}
			set {initializerFile = value;}
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
				thisNamespace = objectSpace.newNamespace();
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
			return initializeThis();
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
				Console.WriteLine("Name binding collision: " + name + " in: " + ThisNamespace.QualifiedName);
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
				Console.WriteLine("Name binding collision: " + name + " in: " + ThisNamespace.QualifiedName);
				return false;
			}
			association.Key = binding;
			return true;
		}

		protected virtual bool configureNamespace() {
			if (NamespaceConfigurationFile == null) return true;
			if (IsVerbose) Console.WriteLine("Configuring namespace: " + ThisNamespace.QualifiedName);
			Object value;
			return evaluateAsSelfExpression(ThisNamespace, ThisNamespace, NamespaceConfigurationFile, out value);
		}

		protected virtual bool initializeThis() {
			if (initializerFile == null) return true;
			if (IsVerbose) Console.WriteLine("Initializing: " + ThisNamespace.QualifiedName);
			Object value;
			return evaluate(ThisNamespace, ThisNamespace, InitializerFile, out value);
		}

		protected virtual bool initializeConstant(Association<ESBindingReference, FileInfo> association) {
			var binding = association.Key;
			var file = association.Value;
			if (IsVerbose) Console.WriteLine("Initializing constant: " + binding.Key + " in: " + ThisNamespace.QualifiedName);
			Object value;
			var success = evaluate(ThisNamespace, binding, file, out value);
			if (success) {
				binding.setValue(value);
				binding.beImmutable();
			}
			return success;
		}

		protected virtual bool initializeVariable(Association<ESBindingReference, FileInfo> association) {
			var binding = association.Key;
			var file = association.Value;
			if (IsVerbose) Console.WriteLine("Initializing variable: " + binding.Key + " in: " + ThisNamespace.QualifiedName);
			Object value;
			var success = evaluate(ThisNamespace, binding, file, out value);
			if (success) {
				binding.setValue(value);
			}
			return success;
		}

		protected virtual bool evaluate(NamespaceObject environment, Object selfValue, FileInfo file, out Object value) {
			objectSpace.evaluate(
					file, 
					environment, 
					selfValue, 
					(errorDescription, span, code, severity) => {
						var error = new ESCompilationError(file, environment, errorDescription, span, code, severity);
						error.addTo(compilationErrors);
					},
					out value);
			return true;
		}

		protected virtual bool evaluateAsSelfExpression(NamespaceObject environment, Object selfValue, FileInfo file, out Object value) {
			objectSpace.evaluateAsSelfExpression(
					file, 
					environment, 
					selfValue, 
					(errorDescription, span, code, severity) => {
						var error = new ESCompilationError(file, environment, errorDescription, span, code, severity);
						error.addTo(compilationErrors);
					},
					out value);
			return true;
		}

	}

	public abstract class BehavioralObjectFactory : NamespaceFactory {

		protected FileInfo classConfigurationFile;
		protected FileInfo metaclassConfigurationFile;
		protected FileInfo instanceMethodsFile;
		protected FileInfo classMethodsFile;

		protected BehavioralObjectFactory(ESObjectSpace objectSpace, NamespaceObject baseEnvironment, ESSymbol name, IDictionary<String, List<ESCompilationError>> compilationErrors) : base(objectSpace, baseEnvironment, name, compilationErrors) {
		}

		public abstract BehavioralObject ClassObject {get;}
		public abstract BehavioralObject MetaclassObject {get;}

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

		protected override AccessPrivilegeLevel DefaultConstantAccessPrivilege {
			get {return AccessPrivilegeLevel.InHierarchy;}
		}

		protected override AccessPrivilegeLevel DefaultVariableAccessPrivilege {
			get {return AccessPrivilegeLevel.InHierarchy;}
		}

		public abstract bool declareClassAndMetaclass();

		public override bool configureAll() {
			if (!base.configureAll()) return false;
			if (!configureMetaclass()) return false;
			return configureClass();
		}

		protected abstract bool configureClass();
		protected abstract bool configureMetaclass();

		public virtual bool compileAll() {
			if (!compileClassMethods()) return false;
			if (!compileInstanceMethods()) return false;
			return true;
		}

		public abstract bool compileInstanceMethods();
		public abstract bool compileClassMethods();

		protected virtual bool compileMethodsFor(BehavioralObject methodClass, FileInfo methodDeclarationFile) {
			if (methodDeclarationFile == null) return true;
			if (IsVerbose) Console.WriteLine("Compiling methods for: " + methodClass.QualifiedName);
			Object value;
			return evaluateAsSelfExpression(methodClass, methodClass, methodDeclarationFile, out value);
		}

		public void reportUndeclaredVariables() {
			ClassObject.withUndeclaredVariablesDo((selector, undeclared) => {
				Console.Write("Undeclared: ");
				Console.WriteLine(undeclared.PrimitiveValue);
			});
			MetaclassObject.withUndeclaredVariablesDo((selector, undeclared) => {
				Console.Write("Undeclared: ");
				Console.WriteLine(undeclared.PrimitiveValue);
			});
		}

	}

	public class ClassFactory : BehavioralObjectFactory {
		protected ESClass thisClass;
		protected ESMetaclass thisMetaclass;

		public ClassFactory(ESObjectSpace objectSpace, NamespaceObject baseEnvironment, ESSymbol name, IDictionary<String, List<ESCompilationError>> compilationErrors) : base(objectSpace, baseEnvironment, name, compilationErrors) {
		}

		public override BehavioralObject ClassObject {
			get {return ThisClass;}
		}

		public override BehavioralObject MetaclassObject {
			get {return ThisMetaclass;}
		}

		public ESClass ThisClass {
			get {return thisClass;}
		}

		public ESMetaclass ThisMetaclass {
			get {return thisMetaclass;}
		}

		public override bool declareNamespace() {
			if (!declareClassAndMetaclass()) return false;
			thisNamespace = ThisClass;
			return true;
		}

		public override bool declareClassAndMetaclass() {
			if (IsVerbose) Console.WriteLine("Declaring class: #" + Name);
			thisClass = null; 
			if (baseEnvironment == null) {
				thisClass = objectSpace.newClass(Name, ObjectStateArchitecture.Stateless);
			} else {
				thisClass = baseEnvironment.defineClass(Name, AccessPrivilegeLevel.Public, null);
			}
			thisMetaclass = (ESMetaclass)thisClass.Class;
			return true;
		}

		protected override bool configureClass() {
			if (ClassConfigurationFile == null) return true;
			if (IsVerbose) Console.WriteLine("Configuring class: " + ThisClass.QualifiedName);
			Object value;
			return evaluateAsSelfExpression(ThisClass, ThisClass, ClassConfigurationFile, out value);
		}

		protected override bool configureMetaclass() {
			if (MetaclassConfigurationFile == null) return true;
			if (IsVerbose) Console.WriteLine("Configuring metaclass: " + ThisMetaclass.QualifiedName);
			Object value;
			return evaluateAsSelfExpression(ThisMetaclass, ThisMetaclass, MetaclassConfigurationFile, out value);
		}

		public override bool compileInstanceMethods() {
			return compileMethodsFor(ThisClass, InstanceMethodsFile);
		}

		public override bool compileClassMethods() {
			return compileMethodsFor(ThisMetaclass, ClassMethodsFile);
		}

	}

	public class TraitFactory : BehavioralObjectFactory {
		protected ESInstanceTrait thisClass;
		protected ESClassTrait thisMetaclass;

		public TraitFactory(ESObjectSpace objectSpace, NamespaceObject baseEnvironment, ESSymbol name, IDictionary<String, List<ESCompilationError>> compilationErrors) : base(objectSpace, baseEnvironment, name, compilationErrors) {
		}

		public override BehavioralObject ClassObject {
			get {return ThisClass;}
		}

		public override BehavioralObject MetaclassObject {
			get {return ThisMetaclass;}
		}

		public ESInstanceTrait ThisClass {
			get {return thisClass;}
		}

		public ESClassTrait ThisMetaclass {
			get {return thisMetaclass;}
		}

		public override bool declareNamespace() {
			if (!declareClassAndMetaclass()) return false;
			thisNamespace = ThisClass;
			return true;
		}

		public override bool declareClassAndMetaclass() {
			if (IsVerbose) Console.WriteLine("Declaring trait: #" + Name);
			thisClass = null; 
			if (baseEnvironment == null) {
				thisClass = objectSpace.newTrait(Name);
			} else {
				thisClass = baseEnvironment.defineTrait(Name, AccessPrivilegeLevel.Public, null);
			}
			thisMetaclass = thisClass.ClassTrait;
			return true;
		}

		protected override bool configureClass() {
			if (ClassConfigurationFile == null) return true;
			if (IsVerbose) Console.WriteLine("Configuring instance trait: " + ThisClass.QualifiedName);
			Object value;
			return evaluateAsSelfExpression(ThisClass, ThisClass, ClassConfigurationFile, out value);
		}

		protected override bool configureMetaclass() {
			if (MetaclassConfigurationFile == null) return true;
			if (IsVerbose) Console.WriteLine("Configuring class trait: " + ThisMetaclass.QualifiedName);
			Object value;
			return evaluateAsSelfExpression(ThisMetaclass, ThisMetaclass, MetaclassConfigurationFile, out value);
		}

		public override bool compileInstanceMethods() {
			return compileMethodsFor(ThisClass, InstanceMethodsFile);
		}

		public override bool compileClassMethods() {
			return compileMethodsFor(ThisMetaclass, ClassMethodsFile);
		}

	}

}
