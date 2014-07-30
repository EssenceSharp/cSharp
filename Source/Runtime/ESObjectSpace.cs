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
using System.Reflection;
using System.Diagnostics;
#if CLR2
using FuncNs = Microsoft.Scripting.Utils;
#else
using FuncNs = System;
#endif
using Microsoft.Scripting;
using EssenceSharp.UtilityServices;
using EssenceSharp.ParsingServices;
using EssenceSharp.CompilationServices;
using EssenceSharp.Exceptions;
using EssenceSharp.Exceptions.System;
using EssenceSharp.Exceptions.System.PrimitiveFailures;
using EssenceSharp.Runtime.Binding;
#endregion

namespace EssenceSharp.Runtime {

	public class ESObjectSpace {
		
		#region Instance variables

		protected ObjectIdentityComparator	objectIdentityComparator	= new ObjectIdentityComparator();
		protected ObjectEqualityComparator	defaultObjectEqualityComparator	= null;
		protected bool				beVerbose			= false;

		#region Canonical Classes

		#region Architecturally-required classes
			  
		protected ESClass			canonicalObjectClass			= new ESClass(ObjectStateArchitecture.Stateless);
		protected ESClass			canonicalNamespaceClass			= new ESClass(ObjectStateArchitecture.Namespace);
		protected ESClass			canonicalBehaviorClass			= new ESClass(ObjectStateArchitecture.Behavior);
		protected ESClass			canonicalClassClass			= new ESClass(ObjectStateArchitecture.Class);
		protected ESClass			canonicalMetaclassClass			= new ESClass(ObjectStateArchitecture.Metaclass);
		protected ESClass			canonicalTraitBehaviorClass		= new ESClass(ObjectStateArchitecture.BehavioralTrait);
		protected ESClass			canonicalTraitClass			= new ESClass(ObjectStateArchitecture.InstanceTrait);
		protected ESClass			canonicalClassTraitClass		= new ESClass(ObjectStateArchitecture.ClassTrait);
		protected ESClass			canonicalTraitTransformationClass	= new ESClass(ObjectStateArchitecture.TraitTransformation);
		protected ESClass			canonicalTraitCompositionClass		= new ESClass(ObjectStateArchitecture.TraitComposition);
		protected ESClass			canonicalCompiledCodeClass		= new ESClass(ObjectStateArchitecture.Abstract);
		protected ESClass			canonicalBlockClass			= new ESClass(ObjectStateArchitecture.Block);
		protected ESClass			canonicalMethodClass			= new ESClass(ObjectStateArchitecture.Method);
		protected ESClass			canonicalAssociationClass		= new ESClass(ObjectStateArchitecture.Association);
		protected ESClass			canonicalBindingReferenceClass		= new ESClass(ObjectStateArchitecture.BindingReference);
		protected ESClass			canonicalMessageClass			= new ESClass(ObjectStateArchitecture.Message);
		protected ESClass			canonicalMessageSendClass		= new ESClass(ObjectStateArchitecture.MessageSend);
		protected ESClass			canonicalMagnitudeClass			= new ESClass(ObjectStateArchitecture.Abstract);

		#endregion

		#region Architecturally-required collection classes

		protected ESClass			canonicalCollectionClass		= new ESClass(ObjectStateArchitecture.Abstract);
		protected ESClass			canonicalKeyedCollectionClass		= new ESClass(ObjectStateArchitecture.Abstract);
		protected ESClass			canonicalIdentityDictionaryClass	= new ESClass(ObjectStateArchitecture.IdentityDictionary);
		protected ESClass			canonicalDictionaryClass		= new ESClass(ObjectStateArchitecture.Dictionary);
		protected ESClass			canonicalSequenceableCollectionClass	= new ESClass(ObjectStateArchitecture.Abstract);
		protected ESClass			canonicalArrayedCollectionClass		= new ESClass(ObjectStateArchitecture.Abstract);
		protected ESClass			canonicalArrayClass			= new ESClass(ObjectStateArchitecture.IndexedObjectSlots);
		protected ESClass			canonicalPrimitiveValueArrayClass	= new ESClass(ObjectStateArchitecture.Abstract);
		protected ESClass			canonicalByteArrayClass			= new ESClass(ObjectStateArchitecture.IndexedByteSlots);
		protected ESClass			canonicalStringClass			= new ESClass(ObjectStateArchitecture.IndexedCharSlots);
		protected ESClass			canonicalSymbolClass			= new ESClass(ObjectStateArchitecture.Symbol);
		protected ESClass			canonicalHalfWordArrayClass		= new ESClass(ObjectStateArchitecture.IndexedHalfWordSlots);
		protected ESClass			canonicalWordArrayClass			= new ESClass(ObjectStateArchitecture.IndexedWordSlots);
		protected ESClass			canonicalLongWordArrayClass		= new ESClass(ObjectStateArchitecture.IndexedLongWordSlots);
		protected ESClass			canonicalFloatArrayClass		= new ESClass(ObjectStateArchitecture.IndexedSinglePrecisionSlots);
		protected ESClass			canonicalDoubleArrayClass		= new ESClass(ObjectStateArchitecture.IndexedDoublePrecisionSlots);
		protected ESClass			canonicalQuadArrayClass			= new ESClass(ObjectStateArchitecture.IndexedQuadPrecisionSlots);
		protected ESClass			canonicalPathnameClass			= new ESClass(ObjectStateArchitecture.Pathname);

		#endregion

		#region Architecturally-required classes "adopted" or "captured" from the CLR using the DLR's meta-object protocol (or required superclasses thereof)

		protected ESClass			canonicalPrimitiveValueClass		= new ESClass(ObjectStateArchitecture.Abstract);
		protected ESClass			canonicalUndefinedObjectClass		= new ESClass(ObjectStateArchitecture.Nil);
		protected ESClass			canonicalBooleanClass			= new ESClass(ObjectStateArchitecture.Abstract);
		protected ESClass			canonicalFalseClass			= new ESClass(ObjectStateArchitecture.False);
		protected ESClass			canonicalTrueClass			= new ESClass(ObjectStateArchitecture.True);
		protected ESClass			canonicalCharacterClass			= new ESClass(ObjectStateArchitecture.Char);
		protected ESClass			canonicalArithmeticValueClass		= new ESClass(ObjectStateArchitecture.Abstract);
		protected ESClass			canonicalNumberClass			= new ESClass(ObjectStateArchitecture.Abstract);
		protected ESClass			canonicalIntegerClass			= new ESClass(ObjectStateArchitecture.Abstract);
		protected ESClass			canonicalSmallIntegerClass		= new ESClass(ObjectStateArchitecture.SmallInteger);
		protected ESClass			canonicalRationalClass			= new ESClass(ObjectStateArchitecture.Abstract);
		protected ESClass			canonicalInvariantPrecisionRealClass	= new ESClass(ObjectStateArchitecture.Abstract);
		protected ESClass			canonicalFloatClass			= new ESClass(ObjectStateArchitecture.SinglePrecision);
		protected ESClass			canonicalDoubleClass			= new ESClass(ObjectStateArchitecture.DoublePrecision);
		protected ESClass			canonicalQuadClass			= new ESClass(ObjectStateArchitecture.QuadPrecision);

		#endregion

		#endregion

		#region Canonical Namespaces

		protected ESNamespace			rootNamespace 				= null;
		protected ESNamespace			smalltalkNamespace 			= null;
		protected ESNamespace			universalNamespace 			= null;
		protected ESNamespace			undeclaredNamespace 			= null;
		protected ESNamespace			clrNamespace	 			= null;

		#endregion

		private readonly SymbolRegistry							symbolRegistry			= null;
		private readonly Dictionary<PrimitiveDomainType, PrimitiveDomain>		primitiveDomainRegistry		= new Dictionary<PrimitiveDomainType, PrimitiveDomain>();  
		protected readonly Dictionary<Type, ESClass>					typeToClassMap			= new Dictionary<Type, ESClass>();
		protected readonly Dictionary<String, AssemblyName>				assemblyNameBindings		= new Dictionary<String, AssemblyName>();
		protected readonly Dictionary<AssemblyName, FileInfo>				assemblyPathnameBindings	= new Dictionary<AssemblyName, FileInfo>();

		protected DynamicBindingGuru							dynamicBindingGuru		= null;
		protected MessageSendBinder.Registry						messageSendBinderRegistry	= null;
		protected GetVariableValueBinder.Registry					getVariableValueBinderRegistry	= null;
		protected SetVariableValueBinder.Registry					setVariableValueBinderRegistry	= null;


		protected DirectoryInfo								essenceSharpPath		= ESFileUtility.defaultEssenceSharpPath();
		protected DirectoryInfo								sharedSourcePath		= null;
		protected DirectoryInfo								sharedScriptsPath		= null;
		protected DirectoryInfo								sharedLibrariesPath		= null;
		protected DirectoryInfo								standardLibraryPath		= null;

		protected ESPathnameBinder							libraryPathBinder		= null;
		protected ESPathnameBinder							sciptPathBinder			= null;

		public static readonly String							standardLibraryName		= "Standard";
		protected bool									isStandardLibraryLoaded		= false;
		protected HashSet<String>							loadedLibraries			= new HashSet<String>();

		#endregion

		public ESObjectSpace() {
			symbolRegistry								= new SymbolRegistry(canonicalSymbolClass);
			intialize();
		}

		#region Properties

		#region Architecturally-required classes

		public ESClass ObjectClass {
			get {return canonicalObjectClass;}
		}

		public ESClass NamespaceClass {
			get {return canonicalNamespaceClass;}
		}

		public ESClass BehaviorClass {
			get {return canonicalBehaviorClass;}
		}

		public ESClass ClassClass {
			get {return canonicalClassClass;}
		}

		public ESClass MetaclassClass {
			get {return canonicalMetaclassClass;}
		}

		public ESClass TraitBehaviorClass {
			get {return canonicalTraitBehaviorClass;}
		}

		public ESClass TraitClass {
			get {return canonicalTraitClass;}
		}

		public ESClass ClassTraitClass {
			get {return canonicalClassTraitClass;}
		}

		public ESClass TraitTransformationClass {
			get {return canonicalTraitTransformationClass;}
		}

		public ESClass TraitCompositionClass {
			get {return canonicalTraitCompositionClass;}
		}

		public ESClass CompiledCodeClass {
			get {return canonicalCompiledCodeClass;}
		}

		public ESClass BlockClass {
			get {return canonicalBlockClass;}
		}

		public ESClass MethodClass {
			get {return canonicalMethodClass;}
		}

		public ESClass AssociationClass {
			get {return canonicalAssociationClass;}
		}

		public ESClass BindingReferenceClass {
			get {return canonicalBindingReferenceClass;}
		}

		public ESClass MessageClass {
			get {return canonicalMessageClass;}
		}

		public ESClass MessageSendClass {
			get {return canonicalMessageSendClass;}
		}

		public ESClass MagnitudeClass {
			get {return canonicalMagnitudeClass;}
		}

		#endregion

		#region Architecturally-required collection classes

		public ESClass CollectionClass {
			get {return canonicalCollectionClass;}
		}

		public ESClass KeyedCollectionClass {
			get {return canonicalKeyedCollectionClass;}
		}

		public ESClass IdentityDictionaryClass {
			get {return canonicalIdentityDictionaryClass;}
		}

		public ESClass DictionaryClass {
			get {return canonicalDictionaryClass;}
		}

		public ESClass SequenceableCollectionClass {
			get {return canonicalSequenceableCollectionClass;}
		}

		public ESClass ArrayedCollectionClass {
			get {return canonicalArrayedCollectionClass;}
		}

		public ESClass ArrayClass {
			get {return canonicalArrayClass;}
		}

		public ESClass PrimitiveValueArrayClass {
			get {return canonicalPrimitiveValueArrayClass;}
		}

		public ESClass ByteArrayClass {
			get {return canonicalByteArrayClass;}
		}

		public ESClass StringClass {
			get {return canonicalStringClass;}
		}

		public ESClass SymbolClass {
			get {return canonicalSymbolClass;}
		}

		public ESClass HalfWordArrayClass {
			get {return canonicalHalfWordArrayClass;}
		}

		public ESClass WordArrayClass {
			get {return canonicalWordArrayClass;}
		}

		public ESClass LongWordArrayClass {
			get {return canonicalLongWordArrayClass;}
		}

		public ESClass FloatArrayClass {
			get {return canonicalFloatArrayClass;}
		}

		public ESClass DoubleArrayClass {
			get {return canonicalDoubleArrayClass;}
		}

		public ESClass QuadArrayClass {
			get {return canonicalQuadArrayClass;}
		}

		public ESClass PathnameClass {
			get {return canonicalPathnameClass;}
		}

		#endregion

		#region Architecturally-required classes "adopted" or "captured" from the CLR using the DLR's meta-object protocol (or required superclasses thereof)

		public ESClass PrimitiveValueClass {
			get {return canonicalPrimitiveValueClass;}
		}

		public ESClass UndefinedObjectClass {
			get {return canonicalUndefinedObjectClass;}
		}

		public ESClass BooleanClass {
			get {return canonicalBooleanClass;}
		}

		public ESClass FalseClass {
			get {return canonicalFalseClass;}
		}

		public ESClass TrueClass {
			get {return canonicalTrueClass;}
		}

		public ESClass CharacterClass {
			get {return canonicalCharacterClass;}
		}

		public ESClass ArithmeticValueClass {
			get {return canonicalArithmeticValueClass;}
		}

		public ESClass NumberClass {
			get {return canonicalNumberClass;}
		}

		public ESClass IntegerClass {
			get {return canonicalIntegerClass;}
		}

		public ESClass SmallIntegerClass {
			get {return canonicalSmallIntegerClass;}
		}

		public ESClass RationalClass {
			get {return canonicalRationalClass;}
		}

		public ESClass InvariantPrecisionRealClass {
			get {return canonicalInvariantPrecisionRealClass;}
		}

		public ESClass FloatClass {
			get {return canonicalFloatClass;}
		}

		public ESClass DoubleClass {
			get {return canonicalDoubleClass;}
		}

		public ESClass QuadClass {
			get {return canonicalQuadClass;}
		}

		#endregion

		#region Canonical Namespaces

		public ESNamespace RootNamespace {
			get {return rootNamespace;}
		}

		public ESNamespace SmalltalkNamespace {
			get {return smalltalkNamespace;}
		}

		public ESNamespace UniversalNamespace {
			get {return universalNamespace;}
		}

		public ESNamespace UndeclaredNamespace {
			get {return undeclaredNamespace;}
		}

		public ESNamespace ClrNamespace {
			get {return clrNamespace;}
		}

		#endregion

		public ObjectIdentityComparator ObjectIdentityComparator {
			get {return objectIdentityComparator;}
		}

		public ObjectEqualityComparator DefaultObjectEqualityComparator {
			get {
				if (defaultObjectEqualityComparator == null) defaultObjectEqualityComparator = newObjectEqualityComparator();
				return defaultObjectEqualityComparator;
			}
		}

		public ESPathnameBinder LibraryPathBinder {
			get { return libraryPathBinder;}
		}

		public ESPathnameBinder ScriptPathBinder {
			get { return sciptPathBinder;}
		}

		public DynamicBindingGuru DynamicBindingGuru {
			get {return dynamicBindingGuru;} 
		}

		#region Registries

		public SymbolRegistry SymbolRegistry {
			get {return symbolRegistry;}
		}

		public MessageSendBinder.Registry MessageSendBinderRegistry {
			get {return messageSendBinderRegistry;}
		}

		public GetVariableValueBinder.Registry GetVariableValueBinderRegistry {
			get {return getVariableValueBinderRegistry;}
		}

		public SetVariableValueBinder.Registry SetVariableValueBinderRegistry {
			get {return setVariableValueBinderRegistry;}
		}

		#endregion

		#endregion

		#region Instance creation methods (factory methods)

		public ESObject newObject() {
			return new ESObject(canonicalObjectClass);
		}

		public ObjectEqualityComparator newObjectEqualityComparator() {
			return new ObjectEqualityComparator(this);
		}

		#region Namespaces

		public ESNamespace newNamespace() {
			return new ESNamespace(canonicalNamespaceClass);
		}

		public ESNamespace newNamespace(bool isBoundToHostNamespace) {
			return new ESNamespace(canonicalNamespaceClass, isBoundToHostNamespace);
		}

		public ESNamespace newNamespace(long capacity) {
			return new ESNamespace(canonicalNamespaceClass, capacity);
		}

		public ESNamespace newNamespace(ESNamespace environment, ESSymbol name) {
			return new ESNamespace(canonicalNamespaceClass, environment, name);
		}

		public ESNamespace newNamespace(ESNamespace environment, ESSymbol name, bool isBoundToHostNamespace) {
			return new ESNamespace(canonicalNamespaceClass, environment, name, isBoundToHostNamespace);
		}

		public ESNamespace newNamespace(long capacity, ESNamespace environment, ESSymbol name) {
			return new ESNamespace(canonicalNamespaceClass, capacity, environment, name);
		}

		#endregion

		#region Behaviors, Classes, Metaclasses

		public ESBehavior newBehavior() {
			return new ESBehavior(canonicalBehaviorClass, this);
		}

		public ESBehavior newBehavior(ObjectStateArchitecture instanceArchitecture, ESBehavior superclass) {
			return new ESBehavior(canonicalBehaviorClass, this, instanceArchitecture, superclass);
		}

		public ESClass newClass() {
			return new ESClass(newMetaclass());
		}

		public ESClass newClass(Type hostSystemType) {
			return new ESClass(newMetaclass(), hostSystemType);
		}

		public ESClass newClass(ESSymbol name, ObjectStateArchitecture instanceArchitecture) {
			return new ESClass(newMetaclass(), name, instanceArchitecture);
		}

		public ESClass newClass(ESSymbol name, ObjectStateArchitecture instanceArchitecture, ESBehavior superclass) {
			return new ESClass(newMetaclass(), name, instanceArchitecture, superclass);
		}

		public ESClass newClass(ESSymbol name, ObjectStateArchitecture instanceArchitecture, ESSymbol[] instanceVarnames, ESBehavior superclass) {
			return new ESClass(newMetaclass(), name, instanceArchitecture, instanceVarnames, superclass);
		}

		public ESMetaclass newMetaclass() {
			return new ESMetaclass(canonicalMetaclassClass, this, canonicalClassClass);
		}

		public ESMetaclass newMetaclass(ESBehavior superclass) {
			return new ESMetaclass(canonicalMetaclassClass, this, superclass);
		}

		public ESMetaclass newMetaclass(ESSymbol[] instanceVarnames, ESBehavior superclass) {
			return new ESMetaclass(canonicalMetaclassClass, this, instanceVarnames, superclass);
		}

		#endregion

		#region Traits

		public ESBehavioralTrait newTraitBehavior() {
			return new ESBehavioralTrait(canonicalTraitBehaviorClass);
		}

		public ESInstanceTrait newTrait() {
			return new ESInstanceTrait(canonicalTraitClass);
		}

		public ESInstanceTrait newTrait(ESSymbol name) {
			return new ESInstanceTrait(canonicalTraitClass, name);
		}

		public ESClassTrait newClassTrait() {
			return new ESClassTrait(canonicalClassTraitClass);
		}

		public ESTraitTransformation newTraitTransformation(Trait subject) {
			return new ESTraitTransformation(canonicalTraitTransformationClass, subject);
		}

		public ESTraitComposition newTraitComposition() {
			return new ESTraitComposition(canonicalTraitCompositionClass);
		}

		#endregion

		#region Blocks

		public ESBlock newBlock() {
			return new ESBlock(canonicalBlockClass);
		}

		public ESBlock newBlock(Delegate function, long numArgs) {
			return new ESBlock(canonicalBlockClass, function, numArgs);
		}

		public ESBlock newBlockToSend(ESSymbol selector) {
			var arity = selector.NumArgs;
			var blockDeclarationBuilder = new StringBuilder();
			blockDeclarationBuilder.Append(blockDeclarationHeaderWithNumArgs(selector.NumArgs + 1)); // " + 1" because the receiver will be the first argument
			blockDeclarationBuilder.AppendLine("a1 ");
			switch (selector.Type) {
				case SymbolType.Identifier:
					blockDeclarationBuilder.Append(selector.PrimitiveValue);
					break;
				case SymbolType.BinaryMessageSelector:
					blockDeclarationBuilder.Append(selector.PrimitiveValue);
					blockDeclarationBuilder.Append(" a2");
					break;
				case SymbolType.Keyword:
					var argIndex = 2;
					selector.keywordsDo(keyword => {
						blockDeclarationBuilder.Append(keyword); 
						blockDeclarationBuilder.Append(": a"); 
						blockDeclarationBuilder.Append(argIndex++); 
						blockDeclarationBuilder.Append(" ");});
					break;
				default:
					throw new InvalidArgumentException("The symbol #'" + selector.PrimitiveValue + "' is not a valid method name.");
			}
			ESBlock block;
			if (!compile(new StringReader(blockDeclarationBuilder.ToString()), null, null, null, out block)) {
				throw new InternalSystemError("Unexpected compilation error in ESObjectSpace.newBlockToSend() -- probably not user or programmer error");
			}
			return block;
		}

		#endregion

		#region Methods

		public ESMethod newMethod() {
			return new ESMethod(canonicalMethodClass);
		}

		public ESMethod newMethod(ESSymbol selector, Delegate function) {
			return new ESMethod(canonicalMethodClass, selector, function);
		}

		public ESMethod newMethod(ESSymbol selector, InlineOperation inlineOperation, Delegate function) {
			return new ESMethod(canonicalMethodClass, selector, inlineOperation, function);
		}

		public ESMethod newMethod(ESSymbol selector, Delegate function, NamespaceObject environment, BehavioralObject homeClass) {
			return new ESMethod(canonicalMethodClass, selector, function, environment, homeClass);
		}

		public ESMethod newMethod(ESSymbol selector, InlineOperation inlineOperation, Delegate function, NamespaceObject environment, BehavioralObject homeClass) {
			return new ESMethod(canonicalMethodClass, selector, inlineOperation, function, environment, homeClass);
		}

		public ESMethod newMethod(ESSymbol selector, Delegate function, ESSymbol protocol) {
			return new ESMethod(canonicalMethodClass, selector, function, protocol);
		}

		public ESMethod newMethod(ESSymbol selector, InlineOperation inlineOperation, Delegate function, ESSymbol protocol) {
			return new ESMethod(canonicalMethodClass, selector, inlineOperation, function, protocol);
		}

		public ESMethod newMethod(ESSymbol selector, Delegate function, NamespaceObject environment, BehavioralObject homeClass, ESSymbol protocol) {
			return new ESMethod(canonicalMethodClass, selector, function, environment, homeClass, protocol);
		}

		public ESMethod newMethod(ESSymbol selector, InlineOperation inlineOperation, Delegate function, NamespaceObject environment, BehavioralObject homeClass, ESSymbol protocol) {
			return new ESMethod(canonicalMethodClass, selector, inlineOperation, function, environment, homeClass, protocol);
		}

		public ESMethod newMethod(NamespaceObject environment, BehavioralObject homeClass, MethodDeclarationNode methodDeclarationNode) {
			return new ESMethod(canonicalMethodClass, environment, homeClass, methodDeclarationNode);
		}

		public ESMethod newMethod(NamespaceObject environment, BehavioralObject homeClass, MethodDeclarationNode methodDeclarationNode, ESSymbol protocol) {
			return new ESMethod(canonicalMethodClass, environment, homeClass, methodDeclarationNode, protocol);
		}

		public ESMethod newMethodToSendDoesNotUnderstand(BehavioralObject homeClass, ESSymbol selector) {
			var arity = selector.NumArgs;
			var methodDeclarationBuilder = new StringBuilder();
			methodDeclarationBuilder.AppendLine(methodDeclaratorHeader(selector));
			methodDeclarationBuilder.AppendLine();
			methodDeclarationBuilder.Append("        ^self doesNotUnderstand: (Message selector: #");
			methodDeclarationBuilder.Append(selector.PrimitiveValue);
			methodDeclarationBuilder.Append(" arguments: {");
			for (var i = 0; i < arity; i++) {
				methodDeclarationBuilder.Append("a");
				methodDeclarationBuilder.Append(i + 1);
				if (arity - i > 1) methodDeclarationBuilder.Append(". ");
			}
			methodDeclarationBuilder.AppendLine("})");
			ESMethod method;
			if (!compileMethod(new StringReader(methodDeclarationBuilder.ToString()), homeClass, symbolFor("error handling"), null, out method)) {
				throw new InternalSystemError("Unexpected compilation error in ESObjectSpace.newMethodToSendDoesNotUnderstand() -- probably not user or programmer error");
			}
			return method;
		}

		#endregion

		#region Associations

		public ESAssociation newAssociation() {
			return new ESAssociation(canonicalAssociationClass);
		}

		public ESAssociation newAssociation(Object key, Object value) {
			return new ESAssociation(canonicalAssociationClass, key, value);
		}

		#endregion

		#region BindingReferences

		public ESBindingReference newBindingReference() {
			return new ESBindingReference(canonicalBindingReferenceClass);
		}

		public ESBindingReference newBindingReference(String key, BindingHandle value) {
			return new ESBindingReference(canonicalBindingReferenceClass, key, value);
		}

		public ESBindingReference newBindingReference(String key, Object value) {
			return new ESBindingReference(canonicalBindingReferenceClass, key, value);
		}

		public ESBindingReference newBindingReference(String key, BindingHandle value, AccessPrivilegeLevel accessPrivilegeLevel) {
			return new ESBindingReference(canonicalBindingReferenceClass, key, value, accessPrivilegeLevel);
		}

		public ESBindingReference newBindingReference(String key, Object value, AccessPrivilegeLevel accessPrivilegeLevel) {
			return new ESBindingReference(canonicalBindingReferenceClass, key, value, accessPrivilegeLevel);
		}

		#endregion

		#region Messages

		public ESMessage newMessage() {
			return new ESMessage(canonicalMessageClass);
		}

		public ESMessage newMessage(ESSymbol selector, Object[] arguments) {
			return new ESMessage(canonicalMessageClass, selector, arguments);
		}

		#endregion

		#region MessageSends

		public ESMessageSend newMessageSend() {
			return new ESMessageSend(canonicalMessageSendClass);
		}

		public ESMessageSend newMessageSend(ESMethod method) {
			return new ESMessageSend(canonicalMessageSendClass, method);
		}

		public ESMessageSend newMessageSend(ESMethod method, Object[] arguments) {
			return new ESMessageSend(canonicalMessageSendClass, method, arguments);
		}

		public ESMessageSend newMessageSend(Object receiver, ESMethod method) {
			return new ESMessageSend(canonicalMessageSendClass, receiver, method);
		}

		public ESMessageSend newMessageSend(Object receiver, ESMethod method, Object[] arguments) {
			return new ESMessageSend(canonicalMessageSendClass, receiver, method, arguments);
		}

		public ESMessageSend newMessageSend(ESSymbol selector) {
			return new ESMessageSend(canonicalMessageSendClass, selector);
		}

		public ESMessageSend newMessageSend(ESSymbol selector, Object[] arguments) {
			return new ESMessageSend(canonicalMessageSendClass, selector, arguments);
		}

		public ESMessageSend newMessageSend(Object receiver, ESSymbol selector) {
			return new ESMessageSend(canonicalMessageSendClass, receiver, selector);
		}

		public ESMessageSend newMessageSend(Object receiver, ESSymbol selector, Object[] arguments) {
			return new ESMessageSend(canonicalMessageSendClass, receiver, selector, arguments);
		}

		#endregion

		#region Dictionaries

		public ESIdentityDictionary newIdentityDictionary() {
			return new ESIdentityDictionary(canonicalIdentityDictionaryClass);
		}

		public ESIdentityDictionary newIdentityDictionary(long capacity) {
			return new ESIdentityDictionary(canonicalIdentityDictionaryClass, capacity);
		}

		public ESIdentityDictionary newIdentityDictionary(ESAssociation[] associations) {
			return new ESIdentityDictionary(canonicalIdentityDictionaryClass, associations);
		}

		public ESDictionary newDictionary() {
			return new ESDictionary(canonicalDictionaryClass);
		}

		public ESDictionary newDictionary(long capacity) {
			return new ESDictionary(canonicalDictionaryClass, capacity);
		}

		public ESDictionary newDictionary(ESAssociation[] associations) {
			return new ESDictionary(canonicalDictionaryClass, associations);
		}

		#endregion

		#region Object Arrays

		public ESArray newArray() {
			return new ESArray(canonicalArrayClass);
		}

		public ESArray newArray(long size) {
			return new ESArray(canonicalArrayClass, size);
		}

		public ESArray newArray(Object[] slots) {
			return new ESArray(canonicalArrayClass, slots);
		}

		#endregion

		#region ByteArrays

		public ESByteArray newByteArray() {
			return new ESByteArray(canonicalByteArrayClass);
		}

		public ESByteArray newByteArray(long size) {
			return new ESByteArray(canonicalByteArrayClass, size);
		}

		public ESByteArray newByteArray(byte[] slots) {
			return new ESByteArray(canonicalByteArrayClass, slots);
		}

		#endregion

		#region Strings

		public ESString newString() {
			return new ESString(canonicalStringClass);
		}

		public ESString newString(long size) {
			return new ESString(canonicalStringClass, size);
		}

		public ESString newString(char[] slots) {
			return new ESString(canonicalStringClass, slots);
		}

		public ESString newString(String value) {
			return new ESString(canonicalStringClass, value);
		}

		#endregion

		#region Symbols

		public ESSymbol symbolFor(String value) {
			return symbolRegistry.symbolFor(value);
		}
		
		public ESSymbol symbolFor(char[] charArray) {
			return symbolRegistry.symbolFor(charArray);
		}
		
		public ESSymbol symbolForVariableOrParameterName(String value) {
			return symbolRegistry.symbolForVariableOrParameterName(value);
		}
		
		public ESSymbol symbolForFilenameEncodedString(String filename) {
			return symbolRegistry.symbolForFilenameEncodedString(filename);
		}
				
		public ESSymbol symbolFor(String value, char? qualifiedNameSeparatorChar) {
			return symbolRegistry.symbolFor(value, qualifiedNameSeparatorChar);
		}

		public ESSymbol selectorToEvaluatBlockWithNumArgs(long numArgs) {
			if (numArgs == 0) return symbolFor("value");
			var sb = new StringBuilder();
			for (var i = 0; i < numArgs; i++) {
				sb.Append("value:");
			}
			return symbolFor(sb.ToString());
		}

		public ESSymbol selectorToEvaluatMethodWithNumArgs(long numArgs) {
			if (numArgs == 0) return symbolFor("valueWithReciver:");
			var sb = new StringBuilder();
			sb.Append("valueWithReciver:");
			for (var i = 0; i < numArgs; i++) {
				sb.Append("with:");
			}
			return symbolFor(sb.ToString());
		}

		#endregion

		#region HalfWordArrays

		public ESHalfWordArray newHalfWordArray() {
			return new ESHalfWordArray(canonicalHalfWordArrayClass);
		}

		public ESHalfWordArray newHalfWordArray(long size) {
			return new ESHalfWordArray(canonicalHalfWordArrayClass, size);
		}

		public ESHalfWordArray newHalfWordArray(ushort[] slots) {
			return new ESHalfWordArray(canonicalHalfWordArrayClass, slots);
		}

		#endregion

		#region WordArrays

		public ESWordArray newWordArray() {
			return new ESWordArray(canonicalWordArrayClass);
		}

		public ESWordArray newWordArray(long size) {
			return new ESWordArray(canonicalWordArrayClass, size);
		}

		public ESWordArray newWordArray(uint[] slots) {
			return new ESWordArray(canonicalWordArrayClass, slots);
		}

		#endregion

		#region LongWordArrays

		public ESLongWordArray newLongWordArray() {
			return new ESLongWordArray(canonicalLongWordArrayClass);
		}

		public ESLongWordArray newLongWordArray(long size) {
			return new ESLongWordArray(canonicalLongWordArrayClass, size);
		}

		public ESLongWordArray newLongWordArray(ulong[] slots) {
			return new ESLongWordArray(canonicalLongWordArrayClass, slots);
		}

		#endregion

		#region FloatArrays

		public ESFloatArray newFloatArray() {
			return new ESFloatArray(canonicalFloatArrayClass);
		}

		public ESFloatArray newFloatArray(long size) {
			return new ESFloatArray(canonicalFloatArrayClass, size);
		}

		public ESFloatArray newFloatArray(float[] slots) {
			return new ESFloatArray(canonicalFloatArrayClass, slots);
		}

		#endregion

		#region DoubleArrays

		public ESDoubleArray newDoubleArray() {
			return new ESDoubleArray(canonicalDoubleArrayClass);
		}

		public ESDoubleArray newDoubleArray(long size) {
			return new ESDoubleArray(canonicalDoubleArrayClass, size);
		}

		public ESDoubleArray newDoubleArray(double[] slots) {
			return new ESDoubleArray(canonicalDoubleArrayClass, slots);
		}

		#endregion

		#region QuadArrays

		public ESQuadArray newQuadArray() {
			return new ESQuadArray(canonicalQuadArrayClass);
		}

		public ESQuadArray newQuadArray(long size) {
			return new ESQuadArray(canonicalQuadArrayClass, size);
		}

		public ESQuadArray newQuadArray(decimal[] slots) {
			return new ESQuadArray(canonicalQuadArrayClass, slots);
		}

		#endregion

		#region Pathnames

		public ESPathname newPathname() {
			return new ESPathname(canonicalPathnameClass);
		}

		public ESPathname newPathname(long size) {
			return new ESPathname(canonicalPathnameClass, size);
		}

		public ESPathname newPathname(String[] slots) {
			return new ESPathname(canonicalPathnameClass, slots);
		}

		public ESPathname pathnameFromString(String pathString) {
			return new ESPathname(canonicalPathnameClass, pathString.elementsFromString(ESPathname.defaultSeparator, null));
		}

		public ESPathname pathnameFromString(String pathString, char separatorChar, FuncNs.Func<Object, Object> transformer) {
			return new ESPathname(canonicalPathnameClass, pathString.elementsFromString(separatorChar, transformer));
		}

		public ESPathname pathnameFromStream(TextReader stream, char separatorChar, FuncNs.Func<Object, Object> transformer) {
			return new ESPathname(canonicalPathnameClass, stream.elementsFromStream(separatorChar, transformer));
		}

		public ESPathname pathnameFromString(String pathString, char[] separatorChars, FuncNs.Func<Object, Object> transformer) {
			return new ESPathname(canonicalPathnameClass, pathString.elementsFromString(separatorChars, transformer));
		}

		public ESPathname pathnameFromStream(TextReader stream, char[] separatorChars, FuncNs.Func<Object, Object> transformer) {
			return new ESPathname(canonicalPathnameClass, stream.elementsFromStream(separatorChars, transformer));
		}

		#endregion

		#endregion

		#region Conversions to Essence Sharp objects

		public ESByteArray asESByteArray(Object value) {
			var esValue = value as ESByteArray;
			return esValue ?? newByteArray((byte[])value);
		}

		public ESString esStringFromNonESObject(Object value) {
			var stringValue = value as String;
			if (stringValue != null) return newString(stringValue.ToCharArray());
			var charArrayValue = value as char[];
			if (charArrayValue != null) return newString(charArrayValue);
			return newString(new char[]{(char)value});
		}

		public ESString asESString(Object value) {
			var esValue = value as ESObject;
			if (esValue != null) return esValue.asESString();
			return esStringFromNonESObject(value);
		}

		public ESHalfWordArray asESHalfWordArray(Object value) {
			var esValue = value as ESHalfWordArray;
			return esValue ?? newHalfWordArray((ushort[])value);
		}

		public ESWordArray asESWordArray(Object value) {
			var esValue = value as ESWordArray;
			return esValue ?? newWordArray((uint[])value);
		}

		public ESLongWordArray asESLongWordArray(Object value) {
			var esValue = value as ESLongWordArray;
			return esValue ?? newLongWordArray((ulong[])value);
		}

		public ESFloatArray asESFloatArray(Object value) {
			var esValue = value as ESFloatArray;
			return esValue ?? newFloatArray((float[])value);
		}

		public ESDoubleArray asESDoubleArray(Object value) {
			var esValue = value as ESDoubleArray;
			return esValue ?? newDoubleArray((double[])value);
		}

		public ESQuadArray asESQuadArray(Object value) {
			var esValue = value as ESQuadArray;
			return esValue ?? newQuadArray((decimal[])value);
		}

		public ESArray asESArray(Object value) {
			var esValue = value as ESArray;
			return esValue ?? newArray((Object[])value);
		}

		public ESSymbol esSymbolFromNonESObject(Object value) {
			var stringValue = value as String;
			if (stringValue != null) return SymbolRegistry.symbolFor(stringValue);
			var charArrayValue = value as char[];
			if (charArrayValue != null) return SymbolRegistry.symbolFor(charArrayValue);
			return SymbolRegistry.symbolFor(new char[]{(char)value});
		}

		public ESSymbol asESSymbol(Object value) {
			var esValue = value as ESObject;
			if (esValue != null) return esValue.asESSymbol();
			return esSymbolFromNonESObject(value);
		}

		public ESPathname asESPathname(Object value) {
			var esValue = value as ESObject;
			return esValue == null ? newPathname((String[])value) : (ESPathname)esValue.asESPathname();
		}

		public ESNamespace asESNamespace(Object value) {
			var esValue = value as ESObject;
			if (esValue == null) throw new PrimInvalidOperandException("Must be a Namespace");
			return esValue.asESNamespace();
		}

		#region Blocks
		
 		public ESBlock asBlock0(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((FuncNs.Func<Object>)value, 0);
		}

		public ESBlock asBlock1(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((FuncNs.Func<Object, Object>)value, 1);
		}

		public ESBlock asBlock2(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((FuncNs.Func<Object, Object, Object>)value, 2);
		}

		public ESBlock asBlock3(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((FuncNs.Func<Object, Object, Object, Object>)value, 3);
		}

		public ESBlock asBlock4(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((FuncNs.Func<Object, Object, Object, Object, Object>)value, 4);
		}

		public ESBlock asBlock5(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((FuncNs.Func<Object, Object, Object, Object, Object, Object>)value, 5);
		}

		public ESBlock asBlock6(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((FuncNs.Func<Object, Object, Object, Object, Object, Object, Object>)value, 6);
		}

		public ESBlock asBlock7(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object>)value, 7);
		}

		public ESBlock asBlock8(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object>)value, 8);
		}

		public ESBlock asBlock9(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value, 9);
		}

		public ESBlock asBlock10(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value, 10);
		}

		public ESBlock asBlock11(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value, 11);
		}

		public ESBlock asBlock12(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value, 12);
		}

		public ESBlock asBlock13(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value, 13);
		}

		public ESBlock asBlock14(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value, 14);
		}

		public ESBlock asBlock15(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value, 15);
		}

		public ESBlock asBlock16(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value, 16);
		}

		public ESBlock asBlock17(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value, 17);
		}

		public ESBlock asBlock18(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value, 18);
		}

		public ESBlock asBlock19(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value, 19);
		}

		public ESBlock asBlock20(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value, 20);
		}

		public ESBlock asBlock21(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value, 21);
		}

		public ESBlock asBlock22(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value, 22);
		}

		public ESBlock asBlock23(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value, 23);
		}

		public ESBlock asBlock24(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value, 24);
		}

		public ESBlock asBlock25(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value, 25);
		}

		public ESBlock asBlock26(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value, 26);
		}

		public ESBlock asBlock27(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value, 27);
		}

		public ESBlock asBlock28(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value, 28);
		}

		public ESBlock asBlock29(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value, 29);
		}

		public ESBlock asBlock30(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value, 30);
		}

		public ESBlock asBlock31(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value, 31);
		}

		public ESBlock asBlock32(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue ?? newBlock((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)value, 32);
		}

		#endregion
		
		#region Instance creation from primitive values or foreign objects
		
		public ESObject instanceFrom(Object[] primitiveValue) {
			if (primitiveValue == null) return null;
			return newArray(primitiveValue); 
		}
		
		public ESObject instanceFrom(byte[] primitiveValue) {
			if (primitiveValue == null) return null;
			return newByteArray(primitiveValue); 
		}
		
		public ESObject instanceFrom(char[] primitiveValue) {
			if (primitiveValue == null) return null;
			return newString(primitiveValue); 
		}
		
		public ESObject instanceFrom(ushort[] primitiveValue) {
			if (primitiveValue == null) return null;
			return newHalfWordArray(primitiveValue); 
		}
		
		public ESObject instanceFrom(uint[] primitiveValue) {
			if (primitiveValue == null) return null;
			return newWordArray(primitiveValue); 
		}
		
		public ESObject instanceFrom(ulong[] primitiveValue) {
			if (primitiveValue == null) return null;
			return newLongWordArray(primitiveValue); 
		}
		
		public ESObject instanceFrom(float[] primitiveValue) {
			if (primitiveValue == null) return null;
			return newFloatArray(primitiveValue); 
		}
		
		public ESObject instanceFrom(double[] primitiveValue) {
			if (primitiveValue == null) return null;
			return newDoubleArray(primitiveValue); 
		}
		
		public ESObject instanceFrom(decimal[] primitiveValue) {
			if (primitiveValue == null) return null;
			return newQuadArray(primitiveValue); 
		}
		
		public ESObject instanceFrom(String primitiveValue) {
			if (primitiveValue == null) return null;
			return SymbolRegistry.symbolFor(primitiveValue);
		}
		
		#endregion

		#endregion

		#region Primitive Methods & Domains

		public void addPrimitiveDomain(PrimitiveDomain primitiveDomain) {
			primitiveDomain.ObjectSpace = this;
			primitiveDomainRegistry[primitiveDomain.Type] = primitiveDomain;
		}

		public bool getPrimitiveDomain(PrimitiveDomainType primitiveAffinity, out PrimitiveDomain primitiveDomain) {
			return primitiveDomainRegistry.TryGetValue(primitiveAffinity, out primitiveDomain);
		}

		public void primitiveDomainsDo(System.Action<PrimitiveDomain> enumerator1) {
			foreach (var kvp in primitiveDomainRegistry) enumerator1(kvp.Value);
		}

		public void publishedPrimitivesDo(System.Action<PrimitiveDomainType, String, Delegate> enumerator3) {
			foreach (var kvp in primitiveDomainRegistry) kvp.Value.publishedPrimitivesDo(enumerator3);
		}

		public bool getPrimitiveFunction(PrimitiveDomainType affinityDomain, String primitiveName, out Delegate function) {
			PrimitiveDomain primitiveDomain;
			function = null;
			if (!getPrimitiveDomain(affinityDomain, out primitiveDomain)) return false;
			return primitiveDomain.getPrimitiveFunction(primitiveName, out function);
		}

		public ESMethod getPrimitiveMethod(PrimitiveDomainType affinityDomain, String primitiveName, ESSymbol selector) {
			PrimitiveDomain primitiveDomain;
			if (!getPrimitiveDomain(affinityDomain, out primitiveDomain)) return null;
			return primitiveDomain.getPrimitiveMethod(primitiveName, selector);
		}

		public void publishCanonicalPrimitives() {
			primitiveDomainsDo(domain => domain.publishCanonicalPrimitives());
		}

		public virtual void installCanonicalPrimitivesInCanonicalClasses(ESSymbol protocol) {

			primitiveDomainsDo((PrimitiveDomain domain) => {
				switch (domain.Type) {

					default:
						domain.installPublishedPrimitivesInDomainClass(protocol);
						break;

					case PrimitiveDomainType.Nil:		 
					case PrimitiveDomainType.True:
					case PrimitiveDomainType.False:
					case PrimitiveDomainType.Char:
					case PrimitiveDomainType.SmallInteger:					
					case PrimitiveDomainType.LargeInteger:
					case PrimitiveDomainType.SinglePrecision:				
					case PrimitiveDomainType.DoublePrecision:				
					case PrimitiveDomainType.QuadPrecision:	
						break;

				}
			});

		}

		public virtual void generatePrimitiveMethodSource(DirectoryInfo basePath) {

			primitiveDomainsDo((PrimitiveDomain domain) => {
				domain.generatePrimitiveMethodSource(basePath);
			});
	
		}

		public virtual void generatePrimitiveMethodSource() {
			generatePrimitiveMethodSource(StandardLibraryPath);
		}

		#endregion

		#region Namespace & Class Binding

		public ESBehavior classOf(Object value) {
			var esValue = value as ESObject;
			if (esValue != null) return esValue.Class;
			return classOfHostSystemValue(value);
		}

		public ESBehavior classOfHostSystemValue(Object hostSystemValue) {
			if (hostSystemValue == null) return canonicalUndefinedObjectClass;
			var hostSystemType = hostSystemValue.GetType();
			switch (Type.GetTypeCode(hostSystemType)) {
				case TypeCode.Empty:
					return canonicalUndefinedObjectClass;
				case TypeCode.Boolean:
					return (bool)hostSystemValue ? canonicalTrueClass : canonicalFalseClass;
				case TypeCode.Char:
					return canonicalCharacterClass;
				case TypeCode.Byte:
				case TypeCode.SByte:
				case TypeCode.UInt16:
				case TypeCode.Int16:
				case TypeCode.UInt32:
				case TypeCode.Int32:
				case TypeCode.UInt64:
				case TypeCode.Int64:
					return canonicalSmallIntegerClass;
				case TypeCode.Single:
					return canonicalFloatClass;
				case TypeCode.Double:
					return canonicalDoubleClass;
				case TypeCode.Decimal:
					return canonicalQuadClass;
				case TypeCode.Object:
					break;
			}
			ESClass hostSystemClass;
			if (typeToClassMap.TryGetValue(hostSystemType, out hostSystemClass)) return hostSystemClass;
			return classForHostSystemType(new TypeName(hostSystemType));
		}

		public ESBehavior classForHostSystemType(Type hostSystemType) {
			ESClass hostSystemClass;
			if (typeToClassMap.TryGetValue(hostSystemType, out hostSystemClass)) return hostSystemClass;
			return classForHostSystemType(new TypeName(hostSystemType));
		}

		public ESBehavior classForHostSystemType(TypeName typeName) {

			ESBindingReference binding;
			var environment = ClrNamespace;
			var assembly = typeName.getAssembly(false);

			typeName.namespacePathElementsDo(nsName => {environment = environment.defineNamespace(nsName, AccessPrivilegeLevel.Public, null);});

			if (typeName.SpecifiesContaingType) {
				var nameBuilder = new StringBuilder();
				nameBuilder.Append(typeName.Namespace);
				typeName.containingTypeNamesDo(
					containingTypeName => {
						nameBuilder.Append(containingTypeName);
						var typeNameInPath = nameBuilder.ToString();
						nameBuilder.Append('+');
						environment = findOrCreateClassForHostSystemType(environment, new TypeName(typeNameInPath, assembly));
					});
			}

			return findOrCreateClassForHostSystemType(environment, typeName);

		}

		private ESClass findOrCreateClassForHostSystemType(ESNamespace environment, TypeName typeName) {
			var nameInEnvironment = typeName.NameWithModifyingSuffix;
			var qualifiedTypeName = typeName.FullName;
			var assembly = typeName.getAssembly(true);
			var hostSystemType = typeName.getType(true);
			ESClass hostSystemClass;
			var binding = environment.localBindingAt(nameInEnvironment, AccessPrivilegeLevel.Local);
			if (binding == null) {
				if (typeToClassMap.TryGetValue(hostSystemType, out hostSystemClass)) {
					environment[nameInEnvironment] = hostSystemClass.asBindingHandle();
				} else {
					hostSystemClass = environment.defineClass(nameInEnvironment, AccessPrivilegeLevel.Public, null);
					hostSystemClass.InstanceType = hostSystemType;
				}
			} else {
				var thisValue = binding.Value.Value;
				var hostSystemClassInEnv = thisValue as ESClass;

				/*
				if (hostSystemClassInEnv == null) {
					environment.removeKey(nameInEnvironment);
					if (typeToClassMap.TryGetValue(hostSystemType, out hostSystemClass)) {
						environment[nameInEnvironment] = hostSystemClass.asBindingHandle();
					} else {
						hostSystemClass = newClass(hostSystemType);
						hostSystemClass.setEnvironment(environment);
					}
				}
				*/

				if (typeToClassMap.TryGetValue(hostSystemType, out hostSystemClass)) {
					if (hostSystemClass != hostSystemClassInEnv) {
						environment[nameInEnvironment] = hostSystemClass.asBindingHandle();
					}
				} else {
					hostSystemClass = hostSystemClassInEnv;
					hostSystemClass.InstanceType = hostSystemType;
				}

			}
			return hostSystemClass;
		}

		internal void bindHostSystemTypeTo(Type hostSystemType, ESClass esClass) {
			typeToClassMap[hostSystemType] = esClass;
		}

		public NamespaceObject getNamespace(String qualifiedNamespaceName, AccessPrivilegeLevel requestorPrivilege) {
			var pathname = pathnameFromString(qualifiedNamespaceName);
			var value = pathname.valueInNamespaceIfAbsent(RootNamespace, requestorPrivilege, ImportTransitivity.Intransitive, null);
			return value as ESNamespace;
		}

		public NamespaceObject findOrCreateNamespace(String qualifiedNsName) {
			return findOrCreateNamespace(qualifiedNsName.elementsFromString('.', null));
		}

		public NamespaceObject findOrCreateNamespace(String[] qualifiedNsName) {

			var environment = RootNamespace;
			ESBindingReference binding;

			for (var i = 0; i < qualifiedNsName.Length; i++) { 
				var nsName = qualifiedNsName[i];
				ESNamespace childNs = null;
				binding = environment.localBindingAt(nsName, AccessPrivilegeLevel.Local);
				if (binding == null) {
					childNs = newNamespace(environment, symbolFor(nsName));
					childNs.setEnvironment(environment);
				} else {
					var thisValue = binding.Value.Value;
					childNs = thisValue as ESNamespace;
					if (childNs == null) {
						environment.removeKey(nsName);
						childNs = newNamespace(environment, symbolFor(nsName));
						childNs.setEnvironment(environment);
					}
				}
				environment = childNs;
			}

			return environment;

		}

		#endregion

		#region Binding Essence# Namespaces To CLR Namespaces / Assemblies

		public void bindNamespaceToAssemblyNamed(String qualifiedNamespaceName, AssemblyName assemblyName) {
			assemblyNameBindings[qualifiedNamespaceName] = assemblyName;
			if (beVerbose) Console.WriteLine("Binding " + qualifiedNamespaceName + " to assembly: " + assemblyName.FullName);
		}

		internal void bindNamespaceToAssemblyNamed(ESNamespace esNamespace, AssemblyName assemblyName) {
			bindNamespaceToAssemblyNamed(esNamespace.PathnameString, assemblyName);
		}
	
		public void bindNamespaceToAssemblyAt(String qualifiedNamespaceName, FileInfo assemblyPath) {
			if (beVerbose) Console.WriteLine("Binding " + qualifiedNamespaceName + " to assembly at: " + assemblyPath.FullName);
			var assemblyName = AssemblyName.GetAssemblyName(assemblyPath.FullName);
			assemblyPathnameBindings[assemblyName] = assemblyPath;
			bindNamespaceToAssemblyNamed(qualifiedNamespaceName, assemblyName);
		}
	
		public void bindNamespaceToAssemblyAt(ESNamespace esNamespace, FileInfo assemblyPath) {
			bindNamespaceToAssemblyAt(esNamespace.PathnameString, assemblyPath);
		}

		public String assemblyNameStringFor(String qualifiedNamespaceName) {
			var assemblyName = assemblyNameFor(qualifiedNamespaceName);
			return assemblyName == null ? null : assemblyName.ToString();
		}

		public AssemblyName assemblyNameFor(String qualifiedNamespaceName) {
			AssemblyName assemblyName;
			return assemblyNameBindings.TryGetValue(qualifiedNamespaceName, out assemblyName) ? assemblyName : null;
		}

		public FileInfo assemblyPathFor(ESNamespace esNamespace) {
			return assemblyPathFor(esNamespace.PathnameString);
		}

		public FileInfo assemblyPathFor(String qualifiedNamespaceName) {
			FileInfo assemblyPath;
			var assemblyName = assemblyNameFor(qualifiedNamespaceName);
			return assemblyPathnameBindings.TryGetValue(assemblyName, out assemblyPath) ? assemblyPath : null;
		}

		public Assembly assemblyNamed(String assemblyName, bool raiseExceptionOnError, out Exception caughtException) {
			return assemblyNamed(new AssemblyName(assemblyName), raiseExceptionOnError, out caughtException);
		}

		public Assembly assemblyNamed(AssemblyName assemblyName, bool raiseExceptionOnError, out Exception caughtException) {
			if (beVerbose) Console.WriteLine("Loading assembly: " + assemblyName.FullName);
			caughtException = null;
			try {
				return AppDomain.CurrentDomain.Load(assemblyName);
			} catch (Exception ex) {
				caughtException = ex;
				if (raiseExceptionOnError) throw new AssemblyBindingFailure("Unable to load assembly: " + assemblyName.FullName, ex);
				return null;
			}
		}

		public Assembly assemblyAt(FileInfo assemblyPath, bool raiseExceptionOnError) {
			if (beVerbose) Console.WriteLine("Loading assembly from: " + assemblyPath.FullName);
			try {
				return Assembly.LoadFrom(assemblyPath.FullName);
			} catch (Exception ex) {
				if (raiseExceptionOnError) throw new AssemblyBindingFailure("Unable to load assembly from: " + assemblyPath.FullName, ex);
				return null;
			}
		}

		public Assembly assemblyFor(ESNamespace esNamespace, bool raiseExceptionOnError) {
			return basicAssemblyFor(esNamespace.PathnameString, raiseExceptionOnError);
		}

		private Assembly basicAssemblyFor(String qualifiedNamespaceName, bool raiseExceptionOnError) {
			if (beVerbose) Console.WriteLine("Loading assembly for " + qualifiedNamespaceName);
			FileInfo assemblyPath;
			Assembly assembly;
			Exception caughtException;
			var assemblyName = assemblyNameFor(qualifiedNamespaceName);
			if (assemblyName == null) return null;
			assembly = assemblyNamed(assemblyName, false, out caughtException);
			if (assembly == null) {
				if (assemblyPathnameBindings.TryGetValue(assemblyName, out assemblyPath)) {
					assembly = assemblyAt(assemblyPath, raiseExceptionOnError);
				} else if (raiseExceptionOnError && caughtException != null) {
					throw new AssemblyBindingFailure("Unable to load assembly: " + assemblyName.FullName, caughtException);
				}
			}
			return assembly;
		}

		#endregion

		#region File paths

		protected virtual void setEssenceSharpPath(DirectoryInfo newDefaultEssenceSharpPath) {

			if (Equals(essenceSharpPath, newDefaultEssenceSharpPath)) return;
			essenceSharpPath = newDefaultEssenceSharpPath;

			sharedSourcePath = new DirectoryInfo(Path.Combine(essenceSharpPath.FullName,		"Source"));
			sharedScriptsPath = new DirectoryInfo(Path.Combine(sharedSourcePath.FullName,		"Scripts"));
			sharedLibrariesPath = new DirectoryInfo(Path.Combine(sharedSourcePath.FullName,		"Libraries"));

			libraryPathBinder = new ESPathnameBinder(SharedLibrariesPath, ".lib");
			sciptPathBinder = new ESPathnameBinder(SharedScriptsPath, ".es");

			if (!pathForSharedLibrary(standardLibraryName, out standardLibraryPath)) standardLibraryPath = new DirectoryInfo(Path.Combine(sharedLibrariesPath.FullName, standardLibraryName));

		}

		public DirectoryInfo EssenceSharpPath {
			get {return essenceSharpPath;}
			set {	var newDefaultEssenceSharpPath = value ?? ESFileUtility.defaultEssenceSharpPath();
				setEssenceSharpPath(newDefaultEssenceSharpPath);}
		}

		public DirectoryInfo SharedSourcePath {
			get {return sharedSourcePath;}
		}

		public DirectoryInfo SharedScriptsPath {
			get {return sharedScriptsPath;}
		}

		public DirectoryInfo SharedLibrariesPath {
			get {return sharedLibrariesPath;}
		}

		public DirectoryInfo StandardLibraryPath {
			get {return standardLibraryPath;}
		}

		public bool pathForSharedLibrary(String userLibraryName, out DirectoryInfo libraryPath) {
			var libraryName = new StringReader(userLibraryName).nextQualifiedIdentifier();
			return libraryPathBinder.pathFor(libraryName, out libraryPath);
		}
		
		public bool pathForScript(String scriptPathameSuffix, out FileInfo scriptPath) {
			return sciptPathBinder.pathFor(scriptPathameSuffix, out scriptPath);
		}

		#endregion

		#region Compilation/Evaluation Services

		public virtual ESCompiler newCompiler(TextReader sourceStream) {
			return new ESCompiler(this, sourceStream, SyntaxProfile.Essence);
		}

		public virtual ESCompiler newCompiler(TextReader sourceStream, SyntaxProfile syntaxProfile) {
			return new ESCompiler(this, sourceStream, syntaxProfile);
		}

		public virtual ESCompiler newCompiler(TextReader sourceStream, ParsingOptions parsingOptions) {
			return new ESCompiler(this, sourceStream, parsingOptions);
		}

		public String blockInvocationMessageFor(IList<String> argExpressions) {
			var numArgs = argExpressions == null ? 0 : argExpressions.Count;
			if (numArgs == 0) return "value";
			var blockParameterListBuilder = new StringBuilder();
			for (var index = 0; index < numArgs; index++) {
				blockParameterListBuilder.Append("value:"); 
				blockParameterListBuilder.Append(argExpressions[index]); 
				blockParameterListBuilder.Append(" ");
			}
			return blockParameterListBuilder.ToString();	
		}

		public String blockDeclarationHeaderWithNumArgs(long numArgs) {
			if (numArgs == 0) return "";
			var blockParameterListBuilder = new StringBuilder();
			for (var index = 1; index <= numArgs; index++) {
				blockParameterListBuilder.Append(":a"); 
				blockParameterListBuilder.Append(index); 
				blockParameterListBuilder.Append(" ");
			}
			blockParameterListBuilder.Append("| ");
			return blockParameterListBuilder.ToString();	
		}

		public String methodDeclaratorHeader(ESSymbol selector) {
			var methodHeaderBuilder = new StringBuilder();
			switch (selector.Type) {
				case SymbolType.Identifier:
					methodHeaderBuilder.Append(selector.PrimitiveValue);
					break;
				case SymbolType.BinaryMessageSelector:
					methodHeaderBuilder.Append(selector.PrimitiveValue);
					methodHeaderBuilder.Append(" a1");
					break;
				case SymbolType.Keyword:
					var argIndex = 1;
					selector.keywordsDo(keyword => {
						methodHeaderBuilder.Append(keyword); 
						methodHeaderBuilder.Append(": a"); 
						methodHeaderBuilder.Append(argIndex++); 
						methodHeaderBuilder.Append(" ");});
					break;
				default:
					throw new InvalidArgumentException("The symbol #'" + selector.PrimitiveValue + "' is not a valid method name.");
			}
			return methodHeaderBuilder.ToString();
		}

		#region Compiling

		#region Self Expressions

		public virtual bool compileSelfExpression(FileInfo file, NamespaceObject environment, Object selfValue, out ESBlock block) {
			using (var sourceStream = ESFileUtility.newReadStream(file)) {
				return compileSelfExpression(sourceStream, environment, selfValue, out block);
			}
		}

		public virtual bool compileSelfExpression(FileInfo file, ParsingOptions parsingOptions, NamespaceObject environment, Object selfValue, out ESBlock block) {
			using (var sourceStream = ESFileUtility.newReadStream(file)) {
				return compileSelfExpression(sourceStream, parsingOptions, environment, selfValue, out block);
			}
		}

		public virtual bool compileSelfExpression(SourceUnit sourceUnit, NamespaceObject environment, Object selfValue, ErrorSink errorSink, out ESBlock block) {
			using (var sourceStream = sourceUnit.GetReader()) {
				var compiler = newCompiler(sourceStream);
				if (errorSink != null) compiler.HandleError = (description, span, errorCode, severity) => errorSink.Add(sourceUnit, description, span, errorCode, severity);
				return compiler.compileSelfExpression(environment, selfValue, null, out block);
			}
		}

		public virtual bool compileSelfExpression(SourceUnit sourceUnit, ParsingOptions parsingOptions, NamespaceObject environment, Object selfValue, ErrorSink errorSink, out ESBlock block) {
			using (var sourceStream = sourceUnit.GetReader()) {
				var compiler = newCompiler(sourceStream, parsingOptions);
				if (errorSink != null) compiler.HandleError = (description, span, errorCode, severity) => errorSink.Add(sourceUnit, description, span, errorCode, severity);
				return compiler.compileSelfExpression(environment, selfValue, null, out block);
			}
		}

		public virtual bool compileSelfExpression(TextReader sourceStream, NamespaceObject environment, Object selfValue, out ESBlock block) {
			var compiler = newCompiler(sourceStream);
			return compiler.compileSelfExpression(environment, selfValue, null, out block);
		}

		public virtual bool compileSelfExpression(TextReader sourceStream, ParsingOptions parsingOptions, NamespaceObject environment, Object selfValue, out ESBlock block) {
			var compiler = newCompiler(sourceStream, parsingOptions);
			return compiler.compileSelfExpression(environment, selfValue, null, out block);
		}

		#endregion

		#region Block Declarations / Executable Code

		public virtual bool compile(FileInfo file, NamespaceObject environment, Object selfValue, System.Action<String, SourceSpan, int, Severity> handleError, out ESBlock block) {
			using (var sourceStream = ESFileUtility.newReadStream(file)) {
				return compile(sourceStream,  environment, selfValue, handleError, out block);
			}
		}

		public virtual bool compile(FileInfo file, ParsingOptions parsingOptions, NamespaceObject environment, Object selfValue, System.Action<String, SourceSpan, int, Severity> handleError, out ESBlock block) {
			using (var sourceStream = ESFileUtility.newReadStream(file)) {
				return compile(sourceStream, parsingOptions,  environment, selfValue, handleError, out block);
			}
		}

		public virtual bool compile(SourceUnit sourceUnit, NamespaceObject environment, Object selfValue, ErrorSink errorSink, out ESBlock block) {
			using (var sourceStream = sourceUnit.GetReader()) {
				var compiler = newCompiler(sourceStream);
				if (errorSink != null) compiler.HandleError = (description, span, errorCode, severity) => errorSink.Add(sourceUnit, description, span, errorCode, severity);
				return compiler.compile(environment, selfValue, out block);
			}
		}

		public virtual bool compile(SourceUnit sourceUnit, ParsingOptions parsingOptions, NamespaceObject environment, Object selfValue, ErrorSink errorSink, out ESBlock block) {
			using (var sourceStream = sourceUnit.GetReader()) {
				var compiler = newCompiler(sourceStream, parsingOptions);
				if (errorSink != null) compiler.HandleError = (description, span, errorCode, severity) => errorSink.Add(sourceUnit, description, span, errorCode, severity);
				return compiler.compile(environment, selfValue, out block);
			}
		}

		public virtual bool compile(TextReader sourceStream, NamespaceObject environment, Object selfValue, System.Action<String, SourceSpan, int, Severity> handleError, out ESBlock block) {
			var compiler = newCompiler(sourceStream);
			if (handleError != null) compiler.HandleError = handleError;
			return compiler.compile(environment, selfValue, out block);
		}


		public virtual bool compile(TextReader sourceStream, ParsingOptions parsingOptions, NamespaceObject environment, Object selfValue, System.Action<String, SourceSpan, int, Severity> handleError, out ESBlock block) {
			var compiler = newCompiler(sourceStream, parsingOptions);
			if (handleError != null) compiler.HandleError = handleError;
			return compiler.compile(environment, selfValue, out block);
		}

		#endregion

		#region Method Declarations

		public virtual bool compileMethod(FileInfo file, BehavioralObject methodClass, ESSymbol protocol, System.Action<String, SourceSpan, int, Severity> handleError, out ESMethod method) {
			using (var sourceStream = ESFileUtility.newReadStream(file)) {
				return compileMethod(sourceStream, methodClass, protocol, handleError, out method);
			}
		}

		public virtual bool compileMethod(FileInfo file, ParsingOptions parsingOptions, BehavioralObject methodClass, ESSymbol protocol, System.Action<String, SourceSpan, int, Severity> handleError, out ESMethod method) {
			using (var sourceStream = ESFileUtility.newReadStream(file)) {
				return compileMethod(sourceStream, parsingOptions, methodClass, protocol, handleError, out method);
			}
		}

		public virtual bool compileMethod(SourceUnit sourceUnit, BehavioralObject methodClass, ESSymbol protocol, ErrorSink errorSink, out ESMethod method) {
			using (var sourceStream = sourceUnit.GetReader()) {
				var compiler = newCompiler(sourceStream);
				if (errorSink != null) compiler.HandleError = (description, span, errorCode, severity) => errorSink.Add(sourceUnit, description, span, errorCode, severity);
				return compiler.compileMethod(methodClass, protocol, out method);
			}
		}

		public virtual bool compileMethod(SourceUnit sourceUnit, ParsingOptions parsingOptions, BehavioralObject methodClass, ESSymbol protocol, ErrorSink errorSink, out ESMethod method) {
			using (var sourceStream = sourceUnit.GetReader()) {
				var compiler = newCompiler(sourceStream, parsingOptions);
				if (errorSink != null) compiler.HandleError = (description, span, errorCode, severity) => errorSink.Add(sourceUnit, description, span, errorCode, severity);
				return compiler.compileMethod(methodClass, protocol, out method);
			}
		}
		
		public virtual bool compileMethod(TextReader sourceStream, BehavioralObject methodClass, ESSymbol protocol, System.Action<String, SourceSpan, int, Severity> handleError, out ESMethod method) {
			var compiler = newCompiler(sourceStream);
			if (handleError != null) compiler.HandleError = handleError;
			return compiler.compileMethod(methodClass, protocol, out method);
		}


		public virtual bool compileMethod(TextReader sourceStream, ParsingOptions parsingOptions, BehavioralObject methodClass, ESSymbol protocol, System.Action<String, SourceSpan, int, Severity> handleError, out ESMethod method) {
			var compiler = newCompiler(sourceStream, parsingOptions);
			if (handleError != null) compiler.HandleError = handleError;
			return compiler.compileMethod(methodClass, protocol, out method);
		}

		#endregion

		#endregion

		#region Evaluating

		#region Self Expressions

		public virtual bool evaluateAsSelfExpression(FileInfo file, NamespaceObject environment, Object selfValue, System.Action<String, SourceSpan, int, Severity> handleError, out Object value) {
			using (var sourceStream = ESFileUtility.newReadStream(file)) {
				return evaluateAsSelfExpression(sourceStream, environment, selfValue, handleError, out value);
			}
		}

		public virtual bool evaluateAsSelfExpression(FileInfo file, ParsingOptions parsingOptions, NamespaceObject environment, Object selfValue, System.Action<String, SourceSpan, int, Severity> handleError, out Object value) {
			using (var sourceStream = ESFileUtility.newReadStream(file)) {
				return evaluateAsSelfExpression(sourceStream, parsingOptions, environment, selfValue, handleError, out value);
			}
		}

		public virtual bool evaluateAsSelfExpression(SourceUnit sourceUnit, NamespaceObject environment, ErrorSink errorSink, Object selfValue, out Object value) {
			using (var sourceStream = sourceUnit.GetReader()) {
				var compiler = newCompiler(sourceStream);
				if (errorSink != null) compiler.HandleError = (description, span, errorCode, severity) => errorSink.Add(sourceUnit, description, span, errorCode, severity);
				return compiler.evaluateSelfExpression(environment, selfValue, null, out value);
			}
		}

		public virtual bool evaluateAsSelfExpression(SourceUnit sourceUnit, ParsingOptions parsingOptions, NamespaceObject environment, ErrorSink errorSink, Object selfValue, out Object value) {
			using (var sourceStream = sourceUnit.GetReader()) {
				var compiler = newCompiler(sourceStream, parsingOptions);
				if (errorSink != null) compiler.HandleError = (description, span, errorCode, severity) => errorSink.Add(sourceUnit, description, span, errorCode, severity);
				return compiler.evaluateSelfExpression(environment, selfValue, null, out value);
			}
		}

		public virtual bool evaluateAsSelfExpression(TextReader sourceStream, NamespaceObject environment, Object selfValue, System.Action<String, SourceSpan, int, Severity> handleError, out Object value) {
			var compiler = newCompiler(sourceStream);
			if (handleError != null) compiler.HandleError = handleError;
			return compiler.evaluateSelfExpression(environment, selfValue, null, out value);
		}

		public virtual bool evaluateAsSelfExpression(TextReader sourceStream, ParsingOptions parsingOptions, NamespaceObject environment, Object selfValue, System.Action<String, SourceSpan, int, Severity> handleError, out Object value) {
			var compiler = newCompiler(sourceStream, parsingOptions);
			if (handleError != null) compiler.HandleError = handleError;
			return compiler.evaluateSelfExpression(environment, selfValue, null, out value);
		}


		#endregion

		#region Executable Code ("Do its"; "initializers" in ANSI Smalltalk terminology)

		public virtual bool evaluate(FileInfo file, NamespaceObject environment, Object selfValue, System.Action<String, SourceSpan, int, Severity> handleError, out Object value) {
			using (var sourceStream = ESFileUtility.newReadStream(file)) {
				return evaluate(sourceStream, environment, selfValue, handleError, out value);
			}
		}

		public virtual bool evaluate(FileInfo file, ParsingOptions parsingOptions, NamespaceObject environment, Object selfValue, System.Action<String, SourceSpan, int, Severity> handleError, out Object value) {
			using (var sourceStream = ESFileUtility.newReadStream(file)) {
				return evaluate(sourceStream, parsingOptions, environment, selfValue, handleError, out value);
			}
		}

		public virtual bool evaluate(SourceUnit sourceUnit, NamespaceObject environment, ErrorSink errorSink, out Object value) {
			using (var sourceStream = sourceUnit.GetReader()) {
				var compiler = newCompiler(sourceStream);
				if (errorSink != null) compiler.HandleError = (description, span, errorCode, severity) => errorSink.Add(sourceUnit, description, span, errorCode, severity);
				return compiler.evaluate(environment, environment is ESBehavior, null, out value);
			}
		}

		public virtual bool evaluate(SourceUnit sourceUnit, ParsingOptions parsingOptions, NamespaceObject environment, ErrorSink errorSink, out Object value) {
			using (var sourceStream = sourceUnit.GetReader()) {
				var compiler = newCompiler(sourceStream, parsingOptions);
				if (errorSink != null) compiler.HandleError = (description, span, errorCode, severity) => errorSink.Add(sourceUnit, description, span, errorCode, severity);
				return compiler.evaluate(environment, environment is ESBehavior, null, out value);
			}
		}

		public virtual bool evaluate(TextReader sourceStream, NamespaceObject environment, Object selfValue, System.Action<String, SourceSpan, int, Severity> handleError, out Object value) {
			var compiler = newCompiler(sourceStream);
			if (handleError != null) compiler.HandleError = handleError;
			return compiler.evaluate(environment, selfValue, null, out value);
		}

		public virtual bool evaluate(TextReader sourceStream, ParsingOptions parsingOptions, NamespaceObject environment, Object selfValue, System.Action<String, SourceSpan, int, Severity> handleError, out Object value) {
			var compiler = newCompiler(sourceStream, parsingOptions);
			if (handleError != null) compiler.HandleError = handleError;
			return compiler.evaluate(environment, selfValue, null, out value);
		}

		#endregion

		#endregion

		#endregion

		#region Error handling and exceptions
		
		public static Object throwMessageNotUnderstood(Object receiver, ESMessage message) {
			throw new MessageNotUnderstood(receiver, message);
		}

		public Object performDoesNotUnderstand(Object receiver, ESBehavior esClass, ESMessage a1) {
			ESMethod method = esClass.compiledMethodAt(SymbolRegistry.DoesNotUnderstandSelector);
			if (method == null) return throwMessageNotUnderstood(esClass, newMessage(SymbolRegistry.DoesNotUnderstandSelector, new Object[]{a1}));
			return method.value1(receiver, a1);
		}

		public static void throwInvalidFunctionCallException(
					String messageText, 
					long expectedArgCount, 
					long actualArgCount, 
					Type expectedFunctionType, 
					Type actualFunctionType, 
					Exception specificException) {

			throw new InvalidFunctionCallException(
					messageText, 
					expectedArgCount, 
					actualArgCount, 
					expectedFunctionType, 
					actualFunctionType, 
					specificException);

		}

		public void throwInvalidArgumentException(BehavioralObject esClass, String opName, String parameterName, Object argValue) {
			var sb = new StringBuilder();
			sb.AppendLine("Invalid argument value: ");
			sb.Append("\tContext = ");
			sb.Append(esClass.PathnameString);
			sb.Append(">>");
			sb.AppendLine(opName);
			sb.Append("\tParameter name = ");
			sb.AppendLine(parameterName);
			sb.Append("\tArgument value = ");
			sb.Append(argValue == null ? "nil" : argValue.ToString());
			throw new PrimInvalidOperandException(sb.ToString());
		}
		
		public void throwInvalidInstanceVariableAccess(ESBehavior esClass, long slotIndex) {
			throwInvalidInstanceVariableAccess(esClass.Name, esClass.instVarNameAt(slotIndex), slotIndex);
		}

		public void throwInvalidInstanceVariableAccess(ESSymbol className, ESSymbol fieldName, long slotIndex) {
			throw new PrimInvalidOperandException((className == null ? SymbolRegistry.symbolFor("An anonymous class") : className) + "." + (fieldName == null ? slotIndex.ToString() : fieldName.PrimitiveValue));
		}
		
		public void throwIndexOutOfRangeException(long index, long minIndex, long maxIndex) {
			throw new PrimIndexBoundsExcessionException("Index = " + index.ToString() + " minValidIndex = " + minIndex + " maxValidIndex = " + maxIndex.ToString(), index, minIndex, maxIndex);
		}

		public void throwSelectorAliasingArityMismatchException(ESSymbol sourceSelector, ESSymbol selectorAlias) {
			var message = new StringBuilder();
			message.Append("Trait method selector aliasing arity mismatch: #");
			message.Append(sourceSelector.PrimitiveValue);
			message.Append(" cannot be renamed to #");
			message.Append(selectorAlias.PrimitiveValue);
			message.Append(" because the arity (number of message arguments required) is different.");
			throw new PrimInvalidOperationException(message.ToString());
		}

		public void throwSelectorAliasingCollisionException(ESSymbol sourceSelector, ESSymbol previousAlias, ESSymbol newAlias) {
			var message = new StringBuilder();
			message.Append("Trait method selector aliasing collision: #");
			message.Append(sourceSelector.PrimitiveValue);
			message.Append(" cannot be renamed to both #");
			message.Append(newAlias.PrimitiveValue);
			message.Append(" and #");
			message.Append(previousAlias.PrimitiveValue);
			throw new PrimInvalidOperationException(message.ToString());
		}
		
		#endregion

		#region System Initialization

		protected virtual void intialize() {
			createDynamicBinderRegistries();
			registerAdoptedHostSystemClasses();
			assignCanonicalNamesToCanonicalClasses();
			assignMetaclassToCanonicalClasses();
			establishCanonicalClassInheritanceStructure();
			createCanonicalNamespaces();
			establishCanonicalNamespaceStructure();
			assignCanonicalClassesToCanonicalNamspaces();
			addCanonicalPrimitiveDomains();
			publishCanonicalPrimitives();
			installCanonicalPrimitivesInCanonicalClasses(SymbolRegistry.symbolFor("system primitives"));
			bindToFileSystem();
		}

		protected void createDynamicBinderRegistries() {
			dynamicBindingGuru		= new DynamicBindingGuru(this);
			messageSendBinderRegistry	= new MessageSendBinder.Registry(dynamicBindingGuru);
			getVariableValueBinderRegistry	= new GetVariableValueBinder.Registry(dynamicBindingGuru);
			setVariableValueBinderRegistry	= new SetVariableValueBinder.Registry(dynamicBindingGuru);
		}

		protected virtual void assignMetaclassToCanonicalClasses() {

			canonicalObjectClass.setClass(newMetaclass());
			canonicalNamespaceClass.setClass(newMetaclass());
			canonicalBehaviorClass.setClass(newMetaclass());
			canonicalClassClass.setClass(newMetaclass());
			canonicalMetaclassClass.setClass(newMetaclass());

			canonicalTraitBehaviorClass.setClass(newMetaclass());
			canonicalTraitClass.setClass(newMetaclass());
			canonicalClassTraitClass.setClass(newMetaclass());
			canonicalTraitTransformationClass.setClass(newMetaclass());
			canonicalTraitCompositionClass.setClass(newMetaclass());

			canonicalCompiledCodeClass.setClass(newMetaclass());
			canonicalBlockClass.setClass(newMetaclass());
			canonicalMethodClass.setClass(newMetaclass());
			canonicalAssociationClass.setClass(newMetaclass());
			canonicalBindingReferenceClass.setClass(newMetaclass());
			canonicalMessageClass.setClass(newMetaclass());
			canonicalMessageSendClass.setClass(newMetaclass());
			canonicalMagnitudeClass.setClass(newMetaclass());

			canonicalCollectionClass.setClass(newMetaclass());
			canonicalKeyedCollectionClass.setClass(newMetaclass());
			canonicalIdentityDictionaryClass.setClass(newMetaclass());
			canonicalDictionaryClass.setClass(newMetaclass());
			canonicalSequenceableCollectionClass.setClass(newMetaclass());
			canonicalArrayedCollectionClass.setClass(newMetaclass());
			canonicalArrayClass.setClass(newMetaclass());
			canonicalPrimitiveValueArrayClass.setClass(newMetaclass());
			canonicalByteArrayClass.setClass(newMetaclass());
			canonicalStringClass.setClass(newMetaclass());
			canonicalSymbolClass.setClass(newMetaclass());
			canonicalHalfWordArrayClass.setClass(newMetaclass());
			canonicalWordArrayClass.setClass(newMetaclass());
			canonicalLongWordArrayClass.setClass(newMetaclass());
			canonicalFloatArrayClass.setClass(newMetaclass());
			canonicalDoubleArrayClass.setClass(newMetaclass());
			canonicalQuadArrayClass.setClass(newMetaclass());
			canonicalPathnameClass.setClass(newMetaclass());

			canonicalPrimitiveValueClass.setClass(newMetaclass());
			canonicalUndefinedObjectClass.setClass(newMetaclass());
			canonicalBooleanClass.setClass(newMetaclass());
			canonicalFalseClass.setClass(newMetaclass());
			canonicalTrueClass.setClass(newMetaclass());
			canonicalCharacterClass.setClass(newMetaclass());
			canonicalArithmeticValueClass.setClass(newMetaclass());
			canonicalNumberClass.setClass(newMetaclass());
			canonicalIntegerClass.setClass(newMetaclass());
			canonicalSmallIntegerClass.setClass(newMetaclass());
			canonicalRationalClass.setClass(newMetaclass());
			canonicalInvariantPrecisionRealClass.setClass(newMetaclass());
			canonicalFloatClass.setClass(newMetaclass());
			canonicalDoubleClass.setClass(newMetaclass());
			canonicalQuadClass.setClass(newMetaclass());

		}

		protected virtual void assignCanonicalNamesToCanonicalClasses() {

			canonicalObjectClass.setName(SymbolRegistry.symbolFor("Object"));
			canonicalNamespaceClass.setName(SymbolRegistry.symbolFor("Namespace"));
			canonicalBehaviorClass.setName(SymbolRegistry.symbolFor("Behavior"));
			canonicalClassClass.setName(SymbolRegistry.symbolFor("Class"));
			canonicalMetaclassClass.setName(SymbolRegistry.symbolFor("Metaclass"));

			canonicalTraitBehaviorClass.setName(SymbolRegistry.symbolFor("TraitBehavior"));
			canonicalTraitClass.setName(SymbolRegistry.symbolFor("Trait"));
			canonicalClassTraitClass.setName(SymbolRegistry.symbolFor("ClassTrait"));
			canonicalTraitTransformationClass.setName(SymbolRegistry.symbolFor("TraitTransformation"));
			canonicalTraitCompositionClass.setName(SymbolRegistry.symbolFor("TraitComposition"));

			canonicalCompiledCodeClass.setName(SymbolRegistry.symbolFor("CompiledCode"));
			canonicalBlockClass.setName(SymbolRegistry.symbolFor("Block"));
			canonicalMethodClass.setName(SymbolRegistry.symbolFor("Method"));
			canonicalAssociationClass.setName(SymbolRegistry.symbolFor("Association"));
			canonicalBindingReferenceClass.setName(SymbolRegistry.symbolFor("BindingReference"));
			canonicalMessageClass.setName(SymbolRegistry.symbolFor("Message"));
			canonicalMessageSendClass.setName(SymbolRegistry.symbolFor("MessageSend"));
			canonicalMagnitudeClass.setName(SymbolRegistry.symbolFor("Magnitude"));

			canonicalCollectionClass.setName(SymbolRegistry.symbolFor("Collection"));
			canonicalKeyedCollectionClass.setName(SymbolRegistry.symbolFor("KeyedCollection"));
			canonicalIdentityDictionaryClass.setName(SymbolRegistry.symbolFor("IdentityDictionary"));
			canonicalDictionaryClass.setName(SymbolRegistry.symbolFor("Dictionary"));
			canonicalSequenceableCollectionClass.setName(SymbolRegistry.symbolFor("SequenceableCollection"));
			canonicalArrayedCollectionClass.setName(SymbolRegistry.symbolFor("ArrayedCollection"));
			canonicalArrayClass.setName(SymbolRegistry.symbolFor("Array"));
			canonicalPrimitiveValueArrayClass.setName(SymbolRegistry.symbolFor("PrimitiveValueArray"));
			canonicalByteArrayClass.setName(SymbolRegistry.symbolFor("ByteArray"));
			canonicalStringClass.setName(SymbolRegistry.symbolFor("String"));
			canonicalSymbolClass.setName(SymbolRegistry.symbolFor("Symbol"));
			canonicalHalfWordArrayClass.setName(SymbolRegistry.symbolFor("HalfWordArray"));
			canonicalWordArrayClass.setName(SymbolRegistry.symbolFor("WordArray"));
			canonicalLongWordArrayClass.setName(SymbolRegistry.symbolFor("LongWordArray"));
			canonicalFloatArrayClass.setName(SymbolRegistry.symbolFor("FloatArray"));
			canonicalDoubleArrayClass.setName(SymbolRegistry.symbolFor("DoubleArray"));
			canonicalQuadArrayClass.setName(SymbolRegistry.symbolFor("QuadArray"));
			canonicalPathnameClass.setName(SymbolRegistry.symbolFor("Pathname"));

			canonicalPrimitiveValueClass.setName(SymbolRegistry.symbolFor("PrimitiveValue"));
			canonicalUndefinedObjectClass.setName(SymbolRegistry.symbolFor("UndefinedObject"));
			canonicalBooleanClass.setName(SymbolRegistry.symbolFor("Boolean"));
			canonicalFalseClass.setName(SymbolRegistry.symbolFor("False"));
			canonicalTrueClass.setName(SymbolRegistry.symbolFor("True"));
			canonicalCharacterClass.setName(SymbolRegistry.symbolFor("Character"));
			canonicalArithmeticValueClass.setName(SymbolRegistry.symbolFor("ArithmeticValue"));
			canonicalNumberClass.setName(SymbolRegistry.symbolFor("Number"));
			canonicalIntegerClass.setName(SymbolRegistry.symbolFor("Integer"));
			canonicalSmallIntegerClass.setName(SymbolRegistry.symbolFor("SmallInteger"));
			canonicalRationalClass.setName(SymbolRegistry.symbolFor("Rational"));
			canonicalInvariantPrecisionRealClass.setName(SymbolRegistry.symbolFor("InvariantPrecisionReal"));
			canonicalFloatClass.setName(SymbolRegistry.symbolFor("Float"));
			canonicalDoubleClass.setName(SymbolRegistry.symbolFor("Double"));
			canonicalQuadClass.setName(SymbolRegistry.symbolFor("Quad"));

		}

		protected virtual void establishCanonicalClassInheritanceStructure() {

			canonicalObjectClass.setSuperclass(null);
			canonicalNamespaceClass.setSuperclass(canonicalKeyedCollectionClass);
			canonicalBehaviorClass.setSuperclass(canonicalNamespaceClass);
			canonicalClassClass.setSuperclass(canonicalBehaviorClass);
			canonicalMetaclassClass.setSuperclass(canonicalBehaviorClass);

			canonicalTraitBehaviorClass.setSuperclass(canonicalNamespaceClass);
			canonicalTraitClass.setSuperclass(canonicalTraitBehaviorClass);
			canonicalClassTraitClass.setSuperclass(canonicalTraitBehaviorClass);
			canonicalTraitTransformationClass.setSuperclass(canonicalObjectClass);
			canonicalTraitCompositionClass.setSuperclass(canonicalObjectClass);

			canonicalCompiledCodeClass.setSuperclass(canonicalObjectClass);
			canonicalBlockClass.setSuperclass(canonicalCompiledCodeClass);
			canonicalMethodClass.setSuperclass(canonicalCompiledCodeClass);
			canonicalAssociationClass.setSuperclass(canonicalObjectClass);
			canonicalBindingReferenceClass.setSuperclass(canonicalObjectClass);
			canonicalMessageClass.setSuperclass(canonicalObjectClass);
			canonicalMessageSendClass.setSuperclass(canonicalMessageClass);
			canonicalMagnitudeClass.setSuperclass(canonicalObjectClass);

			canonicalCollectionClass.setSuperclass(canonicalObjectClass);
			canonicalKeyedCollectionClass.setSuperclass(canonicalCollectionClass);
			canonicalIdentityDictionaryClass.setSuperclass(canonicalKeyedCollectionClass);
			canonicalDictionaryClass.setSuperclass(canonicalIdentityDictionaryClass);
			canonicalSequenceableCollectionClass.setSuperclass(canonicalCollectionClass);
			canonicalArrayedCollectionClass.setSuperclass(canonicalSequenceableCollectionClass);
			canonicalArrayClass.setSuperclass(canonicalArrayedCollectionClass);
			canonicalPrimitiveValueArrayClass.setSuperclass(canonicalArrayedCollectionClass);
			canonicalByteArrayClass.setSuperclass(canonicalPrimitiveValueArrayClass);
			canonicalStringClass.setSuperclass(canonicalPrimitiveValueArrayClass);
			canonicalSymbolClass.setSuperclass(canonicalStringClass);
			canonicalHalfWordArrayClass.setSuperclass(canonicalPrimitiveValueArrayClass);
			canonicalWordArrayClass.setSuperclass(canonicalPrimitiveValueArrayClass);
			canonicalLongWordArrayClass.setSuperclass(canonicalPrimitiveValueArrayClass);
			canonicalFloatArrayClass.setSuperclass(canonicalPrimitiveValueArrayClass);
			canonicalDoubleArrayClass.setSuperclass(canonicalPrimitiveValueArrayClass);
			canonicalQuadArrayClass.setSuperclass(canonicalPrimitiveValueArrayClass);
			canonicalPathnameClass.setSuperclass(canonicalPrimitiveValueArrayClass);

			canonicalPrimitiveValueClass.setSuperclass(null);
			canonicalUndefinedObjectClass.setSuperclass(canonicalPrimitiveValueClass);
			canonicalBooleanClass.setSuperclass(canonicalPrimitiveValueClass);
			canonicalFalseClass.setSuperclass(canonicalBooleanClass);
			canonicalTrueClass.setSuperclass(canonicalBooleanClass);
			canonicalCharacterClass.setSuperclass(canonicalPrimitiveValueClass);
			canonicalArithmeticValueClass.setSuperclass(canonicalPrimitiveValueClass);
			canonicalNumberClass.setSuperclass(canonicalArithmeticValueClass);
			canonicalIntegerClass.setSuperclass(canonicalNumberClass);
			canonicalSmallIntegerClass.setSuperclass(canonicalIntegerClass);
			canonicalRationalClass.setSuperclass(canonicalNumberClass);
			canonicalInvariantPrecisionRealClass.setSuperclass(canonicalRationalClass);
			canonicalFloatClass.setSuperclass(canonicalInvariantPrecisionRealClass);
			canonicalDoubleClass.setSuperclass(canonicalInvariantPrecisionRealClass);
			canonicalQuadClass.setSuperclass(canonicalInvariantPrecisionRealClass);

		}

		protected virtual void createCanonicalNamespaces() {
			rootNamespace		= newNamespace(null, SymbolRegistry.symbolFor("Root"));
			smalltalkNamespace	= newNamespace(rootNamespace, SymbolRegistry.symbolFor("Smalltalk"));
			universalNamespace	= newNamespace(rootNamespace, SymbolRegistry.symbolFor("Universal"));
			undeclaredNamespace	= newNamespace(rootNamespace, SymbolRegistry.symbolFor("Undeclared"));
			clrNamespace		= newNamespace(rootNamespace, SymbolRegistry.symbolFor("CLR"), true);
		}

		public virtual void establishCanonicalNamespaceStructure() {
			rootNamespace.declareInSelf(true);
			rootNamespace.declareInSelfAs("EssenceSharp", true);
			clrNamespace.declareInSelfAs("HostSystem", true);
			rootNamespace.addImport(new ESImportSpec(smalltalkNamespace, AccessPrivilegeLevel.Public, ImportTransitivity.Intransitive));
			rootNamespace.addImport(new ESImportSpec(universalNamespace, AccessPrivilegeLevel.Public, ImportTransitivity.Transitive));
			rootNamespace.addImport(new ESImportSpec(undeclaredNamespace, AccessPrivilegeLevel.Public, ImportTransitivity.Intransitive));
			rootNamespace.addImport(new ESImportSpec(clrNamespace, AccessPrivilegeLevel.Public, ImportTransitivity.Intransitive));
		}

		protected virtual void assignCanonicalClassesToCanonicalNamspaces() {

			canonicalObjectClass.setEnvironment(SmalltalkNamespace);
			canonicalNamespaceClass.setEnvironment(SmalltalkNamespace);
			canonicalBehaviorClass.setEnvironment(SmalltalkNamespace);
			canonicalClassClass.setEnvironment(SmalltalkNamespace);
			canonicalMetaclassClass.setEnvironment(SmalltalkNamespace);

			canonicalTraitBehaviorClass.setEnvironment(SmalltalkNamespace);
			canonicalTraitClass.setEnvironment(SmalltalkNamespace);
			canonicalClassTraitClass.setEnvironment(SmalltalkNamespace);
			canonicalTraitTransformationClass.setEnvironment(SmalltalkNamespace);
			canonicalTraitCompositionClass.setEnvironment(SmalltalkNamespace);

			canonicalCompiledCodeClass.setEnvironment(SmalltalkNamespace);
			canonicalBlockClass.setEnvironment(SmalltalkNamespace);
			canonicalMethodClass.setEnvironment(SmalltalkNamespace);
			canonicalAssociationClass.setEnvironment(SmalltalkNamespace);
			canonicalBindingReferenceClass.setEnvironment(SmalltalkNamespace);
			canonicalMessageClass.setEnvironment(SmalltalkNamespace);
			canonicalMessageSendClass.setEnvironment(SmalltalkNamespace);
			canonicalMagnitudeClass.setEnvironment(SmalltalkNamespace);

			canonicalCollectionClass.setEnvironment(SmalltalkNamespace);
			canonicalKeyedCollectionClass.setEnvironment(SmalltalkNamespace);
			canonicalIdentityDictionaryClass.setEnvironment(SmalltalkNamespace);
			canonicalDictionaryClass.setEnvironment(SmalltalkNamespace);
			canonicalSequenceableCollectionClass.setEnvironment(SmalltalkNamespace);
			canonicalArrayedCollectionClass.setEnvironment(SmalltalkNamespace);
			canonicalArrayClass.setEnvironment(SmalltalkNamespace);
			canonicalPrimitiveValueArrayClass.setEnvironment(SmalltalkNamespace);
			canonicalByteArrayClass.setEnvironment(SmalltalkNamespace);
			canonicalStringClass.setEnvironment(SmalltalkNamespace);
			canonicalSymbolClass.setEnvironment(SmalltalkNamespace);
			canonicalHalfWordArrayClass.setEnvironment(SmalltalkNamespace);
			canonicalWordArrayClass.setEnvironment(SmalltalkNamespace);
			canonicalLongWordArrayClass.setEnvironment(SmalltalkNamespace);
			canonicalFloatArrayClass.setEnvironment(SmalltalkNamespace);
			canonicalDoubleArrayClass.setEnvironment(SmalltalkNamespace);
			canonicalQuadArrayClass.setEnvironment(SmalltalkNamespace);
			canonicalPathnameClass.setEnvironment(SmalltalkNamespace);

			canonicalPrimitiveValueClass.setEnvironment(SmalltalkNamespace);
			canonicalUndefinedObjectClass.setEnvironment(SmalltalkNamespace);
			canonicalBooleanClass.setEnvironment(SmalltalkNamespace);
			canonicalFalseClass.setEnvironment(SmalltalkNamespace);
			canonicalTrueClass.setEnvironment(SmalltalkNamespace);
			canonicalCharacterClass.setEnvironment(SmalltalkNamespace);
			canonicalArithmeticValueClass.setEnvironment(SmalltalkNamespace);
			canonicalNumberClass.setEnvironment(SmalltalkNamespace);
			canonicalIntegerClass.setEnvironment(SmalltalkNamespace);
			canonicalSmallIntegerClass.setEnvironment(SmalltalkNamespace);
			canonicalRationalClass.setEnvironment(SmalltalkNamespace);
			canonicalInvariantPrecisionRealClass.setEnvironment(SmalltalkNamespace);
			canonicalFloatClass.setEnvironment(SmalltalkNamespace);
			canonicalDoubleClass.setEnvironment(SmalltalkNamespace);
			canonicalQuadClass.setEnvironment(SmalltalkNamespace);

		}

		protected virtual void addCanonicalPrimitiveDomains() {

			addPrimitiveDomain(new ESObject.Primitives());
			addPrimitiveDomain(new ESNamespace.Primitives());
			addPrimitiveDomain(new ESBehavior.Primitives());
			addPrimitiveDomain(new ESClass.Primitives());
			addPrimitiveDomain(new ESMetaclass.Primitives());

			addPrimitiveDomain(new ESBehavioralTrait.Primitives());
			addPrimitiveDomain(new ESInstanceTrait.Primitives());
			addPrimitiveDomain(new ESClassTrait.Primitives());
			addPrimitiveDomain(new ESTraitTransformation.Primitives());
			addPrimitiveDomain(new ESTraitComposition.Primitives());

			addPrimitiveDomain(new ESCompiledCode.Primitives());
			addPrimitiveDomain(new ESBlock.Primitives());
			addPrimitiveDomain(new ESMethod.Primitives());
			addPrimitiveDomain(new ESAssociation.Primitives());
			addPrimitiveDomain(new ESBindingReference.Primitives());
			addPrimitiveDomain(new ESMessage.Primitives());
			addPrimitiveDomain(new ESMessageSend.Primitives());
			addPrimitiveDomain(new ESIdentityDictionary.Primitives());
			addPrimitiveDomain(new ESArray.Primitives());
			addPrimitiveDomain(new ESByteArray.Primitives());
			addPrimitiveDomain(new ESString.Primitives());
			addPrimitiveDomain(new ESSymbol.Primitives());
			addPrimitiveDomain(new ESHalfWordArray.Primitives());
			addPrimitiveDomain(new ESWordArray.Primitives());
			addPrimitiveDomain(new ESLongWordArray.Primitives());
			addPrimitiveDomain(new ESFloatArray.Primitives());
			addPrimitiveDomain(new ESDoubleArray.Primitives());
			addPrimitiveDomain(new ESQuadArray.Primitives());
			addPrimitiveDomain(new ESPathname.Primitives());

			addPrimitiveDomain(new UndefinedObjectPrimitives());
			addPrimitiveDomain(new FalsePrimitives());
			addPrimitiveDomain(new TruePrimitives());
			addPrimitiveDomain(new CharacterPrimitives());
			addPrimitiveDomain(new SmallIntegerPrimitives());
			addPrimitiveDomain(new SinglePrecisionPrimitives());
			addPrimitiveDomain(new DoublePrecisionPrimitives());
			addPrimitiveDomain(new QuadPrecisionPrimitives());

			addPrimitiveDomain(new CLR_System_Exception_Primitives());
			addPrimitiveDomain(new CLR_System_Collections_List_1_Object_Primitives());

		}

		public virtual void registerAdoptedHostSystemClasses() {

			typeToClassMap[TypeGuru.boolType] = BooleanClass;
			typeToClassMap[TypeGuru.charType] = CharacterClass;

			typeToClassMap[TypeGuru.longType] = SmallIntegerClass;
			typeToClassMap[TypeGuru.intType] = SmallIntegerClass;
			typeToClassMap[TypeGuru.uintType] = SmallIntegerClass;
			typeToClassMap[TypeGuru.shortType] = SmallIntegerClass;
			typeToClassMap[TypeGuru.ushortType] = SmallIntegerClass;
			typeToClassMap[TypeGuru.byteType] = SmallIntegerClass;
			typeToClassMap[TypeGuru.sbyteType] = SmallIntegerClass;

			typeToClassMap[TypeGuru.floatType] = FloatClass;
			typeToClassMap[TypeGuru.doubleType] = DoubleClass;
			typeToClassMap[TypeGuru.decimalType] = QuadClass;

		}

		protected virtual void bindToFileSystem() {
			EssenceSharpPath = ESFileUtility.defaultEssenceSharpPath();
		}

		#endregion

		#region Bootstrap Load

		public bool ensureStartUp() {
			return ensureStartUp(false, false);
		}

		public bool ensureStartUp(bool startupVerbosely, bool reportLibraryLoadTime) {
			return ensureStartUp(new List<String>(), startupVerbosely, reportLibraryLoadTime);
		}

		public bool ensureStartUp(List<String> libraryNames, bool startupVerbosely, bool reportLibraryLoadTime) {

			beVerbose = startupVerbosely;

			var stopwatch = new Stopwatch();
			stopwatch.Start();

			List<NamespaceObject> initialRootNamespaces;
			if (!isStandardLibraryLoaded) {
				if (beVerbose) Console.WriteLine("Loading standard library...");
				if (!ESLibraryLoader.load(this, rootNamespace, StandardLibraryPath, startupVerbosely, true, out initialRootNamespaces)) {
					Console.WriteLine("Bootstrap load of the Standard Library failed from " + StandardLibraryPath.FullName);
					return false;
				}
				isStandardLibraryLoaded = true;
				loadedLibraries.Add("Standard");
			}

			foreach (var name in libraryNames) {
				if (loadedLibraries.Contains(name)) continue;
				DirectoryInfo libraryPath;
				var libraryPathExists = pathForSharedLibrary(name, out libraryPath);
				if (libraryPathExists) { 
					if (beVerbose) Console.WriteLine("Loading library " + name + " from path " + libraryPath + " ...");
					if (!ESLibraryLoader.load(this, rootNamespace, libraryPath, startupVerbosely, true, out initialRootNamespaces)) {
						Console.WriteLine("Bootstrap load of library " + name + " failed from " + libraryPath.FullName);
						return false;
					}
					loadedLibraries.Add(name);
				} else {
					Console.WriteLine("Bootstrap load of library " + name + " failed: no library folder found with that name.");
				}
			}

			stopwatch.Stop();
			var loadTime = stopwatch.Elapsed;
			if (reportLibraryLoadTime) { 
				Console.WriteLine("");
				Console.WriteLine("Library load time = " + loadTime.ToString());
				Console.WriteLine("");
			}

			return true;

		}

		#endregion

	}

}
