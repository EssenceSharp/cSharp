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
using EssenceSharp.ClientServices;
using EssenceSharp.UtilityServices;
using EssenceSharp.ParsingServices;
using EssenceSharp.CompilationServices;
using EssenceSharp.Runtime.Binding;
#endregion

namespace EssenceSharp.Runtime {

	public class ESKernel {
		
		#region Instance variables

		protected bool									beVerbose			= false;

		#region Canonical Classes

		#region Architecturally-required classes
			  
		protected ESClass			canonicalObjectClass			= new ESClass(ObjectStateArchitecture.Stateless);
		protected ESClass			canonicalNamespaceClass			= new ESClass(ObjectStateArchitecture.Namespace);
		protected ESClass			canonicalBehaviorClass			= new ESClass(ObjectStateArchitecture.Behavior);
		protected ESClass			canonicalClassClass			= new ESClass(ObjectStateArchitecture.Class);
		protected ESClass			canonicalMetaclassClass			= new ESClass(ObjectStateArchitecture.Metaclass);
		protected ESClass			canonicalCompiledCodeClass		= new ESClass(ObjectStateArchitecture.Abstract);
		protected ESClass			canonicalBlockClass			= new ESClass(ObjectStateArchitecture.Block);
		protected ESClass			canonicalMethodClass			= new ESClass(ObjectStateArchitecture.Method);
		protected ESClass			canonicalAssociationClass		= new ESClass(ObjectStateArchitecture.Association);
		protected ESClass			canonicalBindingReferenceClass		= new ESClass(ObjectStateArchitecture.BindingReference);
		protected ESClass			canonicalMessageClass			= new ESClass(ObjectStateArchitecture.Message);
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
		protected ESNamespace			undeclaredNamespace 			= null;
		protected ESNamespace			clrNamespace	 			= null;

		#endregion

		private readonly SymbolRegistry							symbolRegistry			= null;
		private readonly Dictionary<PrimitiveDomainType, PrimitiveDomain>		primitiveDomainRegistry		= new Dictionary<PrimitiveDomainType, PrimitiveDomain>();  
		protected readonly Dictionary<Type, ESClass>					typeToClassMap			= new Dictionary<Type, ESClass>();
		protected readonly Dictionary<String, AssemblyName>				assemblyNameBindings	= new Dictionary<String, AssemblyName>();
		protected readonly Dictionary<AssemblyName, FileInfo>				assemblyPathnameBindings	= new Dictionary<AssemblyName, FileInfo>();

		protected DynamicBindingGuru							dynamicBindingGuru		= null;
		protected MessageSendBinder.Registry						messageSendBinderRegistry	= null;
		protected GetVariableValueBinder.Registry					getVariableValueBinderRegistry	= null;
		protected SetVariableValueBinder.Registry					setVariableValueBinderRegistry	= null;

		protected DirectoryInfo								essenceSharpPath		= ESFileUtility.defaultEssenceSharpPath();
		protected DirectoryInfo								sourcePath			= null;
		protected DirectoryInfo								scriptsPath			= null;
		protected DirectoryInfo								librariesPath			= null;
		protected DirectoryInfo								standardLibraryPath		= null;

		public static readonly String							standardLibraryName		= "Standard";
		protected bool									isStandardLibraryLoaded		= false;
		protected HashSet<String>							loadedLibraries			= new HashSet<String>();

		#endregion

		public ESKernel() {
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

		public ESNamespace UndeclaredNamespace {
			get {return undeclaredNamespace;}
		}

		public ESNamespace ClrNamespace {
			get {return clrNamespace;}
		}

		#endregion

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

		public ESBehavior newBehavior(ObjectStateArchitecture instanceArchitecture, ESSymbol[] instanceVarnames, ESBehavior superclass) {
			return new ESBehavior(canonicalBehaviorClass, this, instanceArchitecture, instanceVarnames, superclass);
		}

		public ESClass newClass() {
			return new ESClass(newMetaclass());
		}

		internal ESClass newClass(Type hostSystemType) {
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

		#region Compiled Code

		public ESBlock newBlock() {
			return new ESBlock(canonicalBlockClass);
		}

		public ESBlock newBlock(Delegate function, long numArgs) {
			return new ESBlock(canonicalBlockClass, function, numArgs);
		}

		public ESMethod newMethod() {
			return new ESMethod(canonicalMethodClass);
		}

		public ESMethod newMethod(ESSymbol selector, Delegate function) {
			return new ESMethod(canonicalMethodClass, selector, function);
		}

		public ESMethod newMethod(ESSymbol selector, InlineOperation inlineOperation, Delegate function) {
			return new ESMethod(canonicalMethodClass, selector, inlineOperation, function);
		}

		public ESMethod newMethod(ESSymbol selector, Delegate function, ESBehavior lexicalContext) {
			return new ESMethod(canonicalMethodClass, selector, function, lexicalContext);
		}

		public ESMethod newMethod(ESSymbol selector, InlineOperation inlineOperation, Delegate function, ESBehavior lexicalContext) {
			return new ESMethod(canonicalMethodClass, selector, inlineOperation, function, lexicalContext);
		}

		public ESMethod newMethod(ESSymbol selector, Delegate function, ESSymbol protocol) {
			return new ESMethod(canonicalMethodClass, selector, function, protocol);
		}

		public ESMethod newMethod(ESSymbol selector, InlineOperation inlineOperation, Delegate function, ESSymbol protocol) {
			return new ESMethod(canonicalMethodClass, selector, inlineOperation, function, protocol);
		}

		public ESMethod newMethod(ESSymbol selector, Delegate function, ESBehavior lexicalContext, ESSymbol protocol) {
			return new ESMethod(canonicalMethodClass, selector, function, lexicalContext, protocol);
		}

		public ESMethod newMethod(ESSymbol selector, InlineOperation inlineOperation, Delegate function, ESBehavior lexicalContext, ESSymbol protocol) {
			return new ESMethod(canonicalMethodClass, selector, inlineOperation, function, lexicalContext, protocol);
		}

		public ESMethod newMethod(MethodDeclarationNode methodDeclarationNode) {
			return new ESMethod(canonicalMethodClass, methodDeclarationNode);
		}

		public ESMethod newMethod(MethodDeclarationNode methodDeclarationNode, ESSymbol protocol) {
			return new ESMethod(canonicalMethodClass, methodDeclarationNode, protocol);
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
			return new ESPathname(canonicalPathnameClass, ESLexicalUtility.elementsFromString(pathString, ESPathname.defaultSeparator, null));
		}

		public ESPathname pathnameFromString(String pathString, char separatorChar, FuncNs.Func<Object, Object> transformer) {
			return new ESPathname(canonicalPathnameClass, ESLexicalUtility.elementsFromString(pathString, separatorChar, transformer));
		}

		public ESPathname pathnameFromStream(TextReader stream, char separatorChar, FuncNs.Func<Object, Object> transformer) {
			return new ESPathname(canonicalPathnameClass, ESLexicalUtility.elementsFromStream(stream, separatorChar, transformer));
		}

		public ESPathname pathnameFromString(String pathString, char[] separatorChars, FuncNs.Func<Object, Object> transformer) {
			return new ESPathname(canonicalPathnameClass, ESLexicalUtility.elementsFromString(pathString, separatorChars, transformer));
		}

		public ESPathname pathnameFromStream(TextReader stream, char[] separatorChars, FuncNs.Func<Object, Object> transformer) {
			return new ESPathname(canonicalPathnameClass, ESLexicalUtility.elementsFromStream(stream, separatorChars, transformer));
		}

		#endregion

		#endregion

		#region Primitive Methods & Domains

		public void addPrimitiveDomain(PrimitiveDomain primitiveDomain) {
			primitiveDomain.Kernel = this;
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
					case PrimitiveDomainType.Object:
					case PrimitiveDomainType.IndexedSlots:
					case PrimitiveDomainType.IndexedByteSlots:				
					case PrimitiveDomainType.IndexedCharSlots:				
					case PrimitiveDomainType.IndexedHalfWordSlots:				
					case PrimitiveDomainType.IndexedWordSlots:				
					case PrimitiveDomainType.IndexedLongWordSlots:			
					case PrimitiveDomainType.IndexedSinglePrecisionSlots:			
					case PrimitiveDomainType.IndexedDoublePrecisionSlots:			
					case PrimitiveDomainType.IndexedQuadPrecisionSlots:			
					case PrimitiveDomainType.IndexedObjectSlots:				
					case PrimitiveDomainType.NamedSlots:										
					case PrimitiveDomainType.Boolean:
					case PrimitiveDomainType.Magnitude:
					case PrimitiveDomainType.ArithmeticValue:
					case PrimitiveDomainType.Number:
					case PrimitiveDomainType.Integer:		
					case PrimitiveDomainType.ScaledDecimal:					
					case PrimitiveDomainType.InvariantPrecisionReal:
					case PrimitiveDomainType.Symbol:						
					case PrimitiveDomainType.Message:					
					case PrimitiveDomainType.MessageSend:					
					case PrimitiveDomainType.Association:					
					case PrimitiveDomainType.BindingReference:					
					case PrimitiveDomainType.Dictionary:					
					case PrimitiveDomainType.Namespace:					
					case PrimitiveDomainType.Pathname:
					case PrimitiveDomainType.CompiledCode:		
					case PrimitiveDomainType.Block:					
					case PrimitiveDomainType.Method:					
					case PrimitiveDomainType.Behavior:					
					case PrimitiveDomainType.Class:						
					case PrimitiveDomainType.Metaclass:					
					case PrimitiveDomainType.HostSystemObject:	
						domain.installPublishedPrimitivesInDomainClass(protocol);
						break;
					default:
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

		public virtual void generateDefaultPrimitiveMethodSource(DirectoryInfo basePath) {

			primitiveDomainsDo((PrimitiveDomain domain) => {
				domain.generateDefaultPrimitiveMethodSource(basePath);
			});
	
		}

		public virtual void generateDefaultPrimitiveMethodSource() {
			generateDefaultPrimitiveMethodSource(StandardLibraryPath);
		}

		#endregion

		#region File paths

		protected virtual void setEssenceSharpPath(DirectoryInfo newDefaultEssenceSharpPath) {
			if (Equals(essenceSharpPath, newDefaultEssenceSharpPath)) return;
			essenceSharpPath = newDefaultEssenceSharpPath;
			sourcePath = new DirectoryInfo(Path.Combine(essenceSharpPath.FullName,		"Source"));
			scriptsPath = new DirectoryInfo(Path.Combine(sourcePath.FullName,		"Scripts"));
			librariesPath = new DirectoryInfo(Path.Combine(sourcePath.FullName,		"Libraries"));
			standardLibraryPath = libraryPathFor(standardLibraryName);
		}

		public DirectoryInfo EssenceSharpPath {
			get {return essenceSharpPath;}
			set {var newDefaultEssenceSharpPath = value ?? ESFileUtility.defaultEssenceSharpPath();
				setEssenceSharpPath(newDefaultEssenceSharpPath);}
		}

		public DirectoryInfo SourcePath {
			get {return sourcePath;}
		}

		public DirectoryInfo ScriptsPath {
			get {return scriptsPath;}
		}

		public DirectoryInfo LibrariesPath {
			get {return librariesPath;}
		}

		public DirectoryInfo StandardLibraryPath {
			get {return standardLibraryPath;}
		}

		public DirectoryInfo libraryPathFor(String userLibraryName) {
			var libraryName = ESLexicalUtility.nextQualifiedIdentifierFrom(new StringReader(userLibraryName));
			return new DirectoryInfo(Path.Combine(librariesPath.FullName, libraryName));
		}

		public bool findFullScriptPathnameFor(String scriptPathameSuffix, out FileInfo scriptPath) {
			bool mustCheckForExtension = false;
			String suffixWithExtension;
			var extension = Path.GetExtension(scriptPathameSuffix);
			if (extension == ".es") {
				suffixWithExtension = scriptPathameSuffix;
			} else {
				mustCheckForExtension = true;
				suffixWithExtension = scriptPathameSuffix + ".es";
			}
			scriptPath = new FileInfo(scriptPathameSuffix);
			if (scriptPath.Exists) return true;
			if (mustCheckForExtension) {
				scriptPath = new FileInfo(suffixWithExtension);
				if (scriptPath.Exists) return true;
			}
			FileInfo innerScriptPath = null;
			EssenceLaunchPad.scriptSearchPathsDoUntil(
				pathnamePrefix => {
					innerScriptPath = new FileInfo(Path.Combine(pathnamePrefix, scriptPathameSuffix));
					if (innerScriptPath.Exists) return true;
					if (mustCheckForExtension) {
						innerScriptPath = new FileInfo(Path.Combine(pathnamePrefix, suffixWithExtension));
						if (innerScriptPath.Exists) return true;
					}
					innerScriptPath = null;
					return false;
				});
			if (innerScriptPath != null) {
				scriptPath = innerScriptPath;
				return true;
			}
			scriptPath = new FileInfo(Path.Combine(ScriptsPath.FullName, scriptPathameSuffix));
			if (scriptPath.Exists) return true;
			if (mustCheckForExtension) {
				scriptPath = new FileInfo(Path.Combine(ScriptsPath.FullName, suffixWithExtension));
				return scriptPath.Exists;
			}
			return false;
		}

		#endregion

		#region Conversions to Essence Sharp objects

		public ESByteArray asESByteArray(Object value) {
			ESByteArray esValue = value as ESByteArray;
			return esValue ?? newByteArray((byte[])value);
		}

		public ESString esStringFromNonESObject(Object value) {
			String stringValue = value as String;
			if (stringValue != null) return newString(stringValue.ToCharArray());
			return newString((char[])value);
		}

		public ESString asESString(Object value) {
			ESObject esValue = value as ESObject;
			if (esValue != null) return esValue.asESString();
			String stringValue = value as String;
			if (stringValue != null) return newString(stringValue.ToCharArray());
			return newString((char[])value);
		}

		public ESHalfWordArray asESHalfWordArray(Object value) {
			ESHalfWordArray esValue = value as ESHalfWordArray;
			return esValue ?? newHalfWordArray((ushort[])value);
		}

		public ESWordArray asESWordArray(Object value) {
			ESWordArray esValue = value as ESWordArray;
			return esValue ?? newWordArray((uint[])value);
		}

		public ESLongWordArray asESLongWordArray(Object value) {
			ESLongWordArray esValue = value as ESLongWordArray;
			return esValue ?? newLongWordArray((ulong[])value);
		}

		public ESFloatArray asESFloatArray(Object value) {
			ESFloatArray esValue = value as ESFloatArray;
			return esValue ?? newFloatArray((float[])value);
		}

		public ESDoubleArray asESDoubleArray(Object value) {
			ESDoubleArray esValue = value as ESDoubleArray;
			return esValue ?? newDoubleArray((double[])value);
		}

		public ESQuadArray asESQuadArray(Object value) {
			ESQuadArray esValue = value as ESQuadArray;
			return esValue ?? newQuadArray((decimal[])value);
		}

		public ESArray asESArray(Object value) {
			ESArray esValue = value as ESArray;
			return esValue ?? newArray((Object[])value);
		}

		public ESSymbol esSymbolFromNonESObject(Object value) {
			String stringValue = value as String;
			if (stringValue != null) return SymbolRegistry.symbolFor(stringValue);
			return SymbolRegistry.symbolFor((char[])value);
		}

		public ESSymbol asESSymbol(Object value) {
			ESObject esValue = value as ESObject;
			if (esValue != null) return esValue.asESSymbol();
			String stringValue = value as String;
			if (stringValue != null) return SymbolRegistry.symbolFor(stringValue);
			return SymbolRegistry.symbolFor((char[])value);
		}

		public ESPathname asESPathname(Object value) {
			ESObject esValue = value as ESObject;
			return esValue == null ? newPathname((String[])value) : (ESPathname)esValue.asESPathname();
		}

		public ESNamespace asESNamespace(Object value) {
			ESObject esValue = value as ESObject;
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

		#region Binding Essence# Namespace To CLR Namespace / Assembly

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

			var environment = ClrNamespace;
			ESBindingReference binding;
			var assembly = typeName.getAssembly(false);

			typeName.namespacePathElementsDo(
				nsName => {
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
				});

			if (typeName.SpecifiesInnerType) {
				var nameBuilder = new StringBuilder();
				nameBuilder.Append(typeName.Namespace);
				typeName.containingTypeNamesDo(
					containingTypeName => {
						nameBuilder.Append(containingTypeName);
						var typeNameInPath = nameBuilder.ToString();
						nameBuilder.Append('+');
						environment = findOrCreateClassForHostSystemType(environment, containingTypeName, assembly, typeNameInPath);
					});
			}

			return findOrCreateClassForHostSystemType(environment, typeName.NameWithGenericArguments, assembly, typeName.FullName);

		}

		private ESClass findOrCreateClassForHostSystemType(ESNamespace environment, String nameInEnvironment, Assembly assembly, String qualifiedTypeName) {
			ESClass hostSystemClass;
			Type hostSystemType = null;
			if (assembly != null) hostSystemType = assembly.GetType(qualifiedTypeName, false);
			if (hostSystemType == null) hostSystemType = Type.GetType(qualifiedTypeName, true);
			var binding = environment.localBindingAt(nameInEnvironment, AccessPrivilegeLevel.Local);
			if (binding == null) {
				if (typeToClassMap.TryGetValue(hostSystemType, out hostSystemClass)) {
					environment[nameInEnvironment] = hostSystemClass.asBindingHandle();
				} else {
					hostSystemClass = newClass(hostSystemType);
					hostSystemClass.setEnvironment(environment);
					hostSystemClass.bindToHostSystemSuperclasses();
				}
			} else {
				var thisValue = binding.Value.Value;
				hostSystemClass = thisValue as ESClass;
				if (hostSystemClass == null) {
					environment.removeKey(nameInEnvironment);
					if (typeToClassMap.TryGetValue(hostSystemType, out hostSystemClass)) {
						environment[nameInEnvironment] = hostSystemClass.asBindingHandle();
					} else {
						hostSystemClass = newClass(hostSystemType);
						hostSystemClass.setEnvironment(environment);
						hostSystemClass.bindToHostSystemSuperclasses();
					}
				}
			}
			return hostSystemClass;
		}

		internal void bindHostSystemTypeTo(Type hostSystemType, ESClass esClass) {
			typeToClassMap[hostSystemType] = esClass;
		}

		public ESNamespace getNamespace(String qualifiedNamespaceName, AccessPrivilegeLevel requestorPrivilege) {
			var pathname = pathnameFromString(qualifiedNamespaceName);
			var value = pathname.valueInNamespaceIfAbsent(RootNamespace, requestorPrivilege, ImportTransitivity.Intransitive, null);
			return value as ESNamespace;
		}

		public ESNamespace findOrCreateNamespace(String qualifiedNsName) {
			return findOrCreateNamespace(ESLexicalUtility.elementsFromString(qualifiedNsName, '.', null));
		}

		public ESNamespace findOrCreateNamespace(String[] qualifiedNsName) {

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

		#region Compilation/Evaluation Services

		public virtual ESCompiler newCompiler(TextReader sourceStream) {
			return new ESCompiler(this, sourceStream, SyntaxProfile.Universal);
		}

		public virtual ESCompiler newCompiler(TextReader sourceStream, SyntaxProfile syntaxProfile) {
			return new ESCompiler(this, sourceStream, syntaxProfile);
		}

		public virtual ESCompiler newCompiler(TextReader sourceStream, ParsingOptions parsingOptions) {
			return new ESCompiler(this, sourceStream, parsingOptions);
		}

		#region Compiling

		#region Self Expressions

		public virtual bool compileSelfExpression(FileInfo file, ESNamespace environment, Object selfValue, out ESBlock block) {
			using (var sourceStream = ESFileUtility.newReadStream(file)) {
				return compileSelfExpression(sourceStream, environment, selfValue, out block);
			}
		}

		public virtual bool compileSelfExpression(FileInfo file, ParsingOptions parsingOptions, ESNamespace environment, Object selfValue, out ESBlock block) {
			using (var sourceStream = ESFileUtility.newReadStream(file)) {
				return compileSelfExpression(sourceStream, parsingOptions, environment, selfValue, out block);
			}
		}

		public virtual bool compileSelfExpression(SourceUnit sourceUnit, ESNamespace environment, Object selfValue, ErrorSink errorSink, out ESBlock block) {
			using (var sourceStream = sourceUnit.GetReader()) {
				var compiler = newCompiler(sourceStream);
				if (errorSink != null) compiler.ReportError = (description, span, errorCode, severity) => errorSink.Add(sourceUnit, description, span, errorCode, severity);
				return compiler.compileSelfExpression(environment, environment is ESBehavior, selfValue, null, out block);
			}
		}

		public virtual bool compileSelfExpression(SourceUnit sourceUnit, ParsingOptions parsingOptions, ESNamespace environment, Object selfValue, ErrorSink errorSink, out ESBlock block) {
			using (var sourceStream = sourceUnit.GetReader()) {
				var compiler = newCompiler(sourceStream, parsingOptions);
				if (errorSink != null) compiler.ReportError = (description, span, errorCode, severity) => errorSink.Add(sourceUnit, description, span, errorCode, severity);
				return compiler.compileSelfExpression(environment, environment is ESBehavior, selfValue, null, out block);
			}
		}

		public virtual bool compileSelfExpression(TextReader sourceStream, ESNamespace environment, Object selfValue, out ESBlock block) {
			var compiler = newCompiler(sourceStream);
			return compiler.compileSelfExpression(environment, environment is ESBehavior, selfValue, null, out block);
		}

		public virtual bool compileSelfExpression(TextReader sourceStream, ParsingOptions parsingOptions, ESNamespace environment, Object selfValue, out ESBlock block) {
			var compiler = newCompiler(sourceStream, parsingOptions);
			return compiler.compileSelfExpression(environment, environment is ESBehavior, selfValue, null, out block);
		}

		#endregion

		#region Executable Code

		public virtual bool compile(FileInfo file, ESNamespace environment, Object selfValue, out ESBlock block) {
			using (var sourceStream = ESFileUtility.newReadStream(file)) {
				return compile(sourceStream,  environment, selfValue, out block);
			}
		}

		public virtual bool compile(FileInfo file, ParsingOptions parsingOptions, ESNamespace environment, Object selfValue, out ESBlock block) {
			using (var sourceStream = ESFileUtility.newReadStream(file)) {
				return compile(sourceStream, parsingOptions,  environment, selfValue, out block);
			}
		}

		public virtual bool compile(SourceUnit sourceUnit, ESNamespace environment, Object selfValue, ErrorSink errorSink, out ESBlock block) {
			using (var sourceStream = sourceUnit.GetReader()) {
				var compiler = newCompiler(sourceStream);
				if (errorSink != null) compiler.ReportError = (description, span, errorCode, severity) => errorSink.Add(sourceUnit, description, span, errorCode, severity);
				return compiler.compile(environment, environment is ESBehavior, selfValue, out block);
			}
		}

		public virtual bool compile(SourceUnit sourceUnit, ParsingOptions parsingOptions, ESNamespace environment, Object selfValue, ErrorSink errorSink, out ESBlock block) {
			using (var sourceStream = sourceUnit.GetReader()) {
				var compiler = newCompiler(sourceStream, parsingOptions);
				if (errorSink != null) compiler.ReportError = (description, span, errorCode, severity) => errorSink.Add(sourceUnit, description, span, errorCode, severity);
				return compiler.compile(environment, environment is ESBehavior, selfValue, out block);
			}
		}

		public virtual bool compile(TextReader sourceStream, ESNamespace environment, Object selfValue, out ESBlock block) {
			var compiler = newCompiler(sourceStream);
			return compiler.compile(environment, environment is ESBehavior, selfValue, out block);
		}


		public virtual bool compile(TextReader sourceStream, ParsingOptions parsingOptions, ESNamespace environment, Object selfValue, out ESBlock block) {
			var compiler = newCompiler(sourceStream, parsingOptions);
			return compiler.compile(environment, environment is ESBehavior, selfValue, out block);
		}

		#endregion

		#region Method Declarations

		public virtual bool compileMethod(FileInfo file, ESBehavior methodClass, ESSymbol protocol, out ESMethod method) {
			using (var sourceStream = ESFileUtility.newReadStream(file)) {
				return compileMethod(sourceStream, methodClass, protocol, out method);
			}
		}

		public virtual bool compileMethod(FileInfo file, ParsingOptions parsingOptions, ESBehavior methodClass, ESSymbol protocol, out ESMethod method) {
			using (var sourceStream = ESFileUtility.newReadStream(file)) {
				return compileMethod(sourceStream, parsingOptions, methodClass, protocol, out method);
			}
		}

		public virtual bool compileMethod(SourceUnit sourceUnit, ESBehavior methodClass, ESSymbol protocol, ErrorSink errorSink, out ESMethod method) {
			using (var sourceStream = sourceUnit.GetReader()) {
				var compiler = newCompiler(sourceStream);
				if (errorSink != null) compiler.ReportError = (description, span, errorCode, severity) => errorSink.Add(sourceUnit, description, span, errorCode, severity);
				return compiler.compileMethod(methodClass, protocol, out method);
			}
		}

		public virtual bool compileMethod(SourceUnit sourceUnit, ParsingOptions parsingOptions, ESBehavior methodClass, ESSymbol protocol, ErrorSink errorSink, out ESMethod method) {
			using (var sourceStream = sourceUnit.GetReader()) {
				var compiler = newCompiler(sourceStream, parsingOptions);
				if (errorSink != null) compiler.ReportError = (description, span, errorCode, severity) => errorSink.Add(sourceUnit, description, span, errorCode, severity);
				return compiler.compileMethod(methodClass, protocol, out method);
			}
		}
		
		public virtual bool compileMethod(TextReader sourceStream, ESBehavior methodClass, ESSymbol protocol, out ESMethod method) {
			var compiler = newCompiler(sourceStream);
			return compiler.compileMethod(methodClass, protocol, out method);
		}


		public virtual bool compileMethod(TextReader sourceStream, ParsingOptions parsingOptions, ESBehavior methodClass, ESSymbol protocol, out ESMethod method) {
			var compiler = newCompiler(sourceStream, parsingOptions);
			return compiler.compileMethod(methodClass, protocol, out method);
		}

		#endregion

		#endregion

		#region Evaluating

		#region Self Expressions

		public virtual bool evaluateAsSelfExpression(FileInfo file, ESNamespace environment, Object selfValue, out Object value) {
			using (var sourceStream = ESFileUtility.newReadStream(file)) {
				return evaluateAsSelfExpression(sourceStream, environment, selfValue, out value);
			}
		}

		public virtual bool evaluateAsSelfExpression(FileInfo file, ParsingOptions parsingOptions, ESNamespace environment, Object selfValue, out Object value) {
			using (var sourceStream = ESFileUtility.newReadStream(file)) {
				return evaluateAsSelfExpression(sourceStream, parsingOptions, environment, selfValue, out value);
			}
		}

		public virtual bool evaluateAsSelfExpression(SourceUnit sourceUnit, ESNamespace environment, ErrorSink errorSink, Object selfValue, out Object value) {
			using (var sourceStream = sourceUnit.GetReader()) {
				var compiler = newCompiler(sourceStream);
				if (errorSink != null) compiler.ReportError = (description, span, errorCode, severity) => errorSink.Add(sourceUnit, description, span, errorCode, severity);
				return compiler.evaluateSelfExpression(environment, environment is ESBehavior, selfValue, null, out value);
			}
		}

		public virtual bool evaluateAsSelfExpression(SourceUnit sourceUnit, ParsingOptions parsingOptions, ESNamespace environment, ErrorSink errorSink, Object selfValue, out Object value) {
			using (var sourceStream = sourceUnit.GetReader()) {
				var compiler = newCompiler(sourceStream, parsingOptions);
				if (errorSink != null) compiler.ReportError = (description, span, errorCode, severity) => errorSink.Add(sourceUnit, description, span, errorCode, severity);
				return compiler.evaluateSelfExpression(environment, environment is ESBehavior, selfValue, null, out value);
			}
		}

		public virtual bool evaluateAsSelfExpression(TextReader sourceStream, ESNamespace environment, Object selfValue, out Object value) {
			var compiler = newCompiler(sourceStream);
			return compiler.evaluateSelfExpression(environment, environment is ESBehavior, selfValue, null, out value);
		}

		public virtual bool evaluateAsSelfExpression(TextReader sourceStream, ParsingOptions parsingOptions, ESNamespace environment, Object selfValue, out Object value) {
			var compiler = newCompiler(sourceStream, parsingOptions);
			return compiler.evaluateSelfExpression(environment, environment is ESBehavior, selfValue, null, out value);
		}


		#endregion

		#region Executable Code

		public virtual bool evaluate(FileInfo file, ESNamespace environment, out Object value) {
			using (var sourceStream = ESFileUtility.newReadStream(file)) {
				return evaluate(sourceStream, environment, out value);
			}
		}

		public virtual bool evaluate(FileInfo file, ParsingOptions parsingOptions, ESNamespace environment, out Object value) {
			using (var sourceStream = ESFileUtility.newReadStream(file)) {
				return evaluate(sourceStream, parsingOptions, environment, out value);
			}
		}

		public virtual bool evaluate(SourceUnit sourceUnit, ESNamespace environment, ErrorSink errorSink, out Object value) {
			using (var sourceStream = sourceUnit.GetReader()) {
				var compiler = newCompiler(sourceStream);
				if (errorSink != null) compiler.ReportError = (description, span, errorCode, severity) => errorSink.Add(sourceUnit, description, span, errorCode, severity);
				return compiler.evaluate(environment, environment is ESBehavior, null, out value);
			}
		}

		public virtual bool evaluate(SourceUnit sourceUnit, ParsingOptions parsingOptions, ESNamespace environment, ErrorSink errorSink, out Object value) {
			using (var sourceStream = sourceUnit.GetReader()) {
				var compiler = newCompiler(sourceStream, parsingOptions);
				if (errorSink != null) compiler.ReportError = (description, span, errorCode, severity) => errorSink.Add(sourceUnit, description, span, errorCode, severity);
				return compiler.evaluate(environment, environment is ESBehavior, null, out value);
			}
		}

		public virtual bool evaluate(TextReader sourceStream, ESNamespace environment, out Object value) {
			var compiler = newCompiler(sourceStream);
			return compiler.evaluate(environment, environment is ESBehavior, null, out value);
		}

		public virtual bool evaluate(TextReader sourceStream, ParsingOptions parsingOptions, ESNamespace environment, out Object value) {
			var compiler = newCompiler(sourceStream, parsingOptions);
			return compiler.evaluate(environment, environment is ESBehavior, null, out value);
		}

		#endregion

		#endregion

		#endregion

		#region Error handling and exceptions
		
		public static Object throwMessageNotUnderstood(ESBehavior esClass, ESMessage message) {
			throw new MissingMemberException("Message not understood: " + esClass.Name + " instances do not know how to respond to the message '" + message.Selector.PrimitiveValue + "'");
		}

		public Object performDoesNotUnderstand(Object receiver, ESBehavior esClass, ESMessage a1) {
			ESMethod method = esClass.compiledMethodAt(SymbolRegistry.DoesNotUnderstandSelector);
			if (method == null) return throwMessageNotUnderstood(esClass, newMessage(SymbolRegistry.DoesNotUnderstandSelector, new Object[]{a1}));
			return method.value1(receiver, a1);
		}
		
		public Object throwInvalidArgumentException(ESBehavior esClass, String opName, String parameterName, Object argValue) {
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
		
		public Object throwInvalidInstanceVariableAccess(ESBehavior esClass, long slotIndex) {
			return throwInvalidInstanceVariableAccess(esClass.Name, esClass.instVarNameAt(slotIndex), slotIndex);
		}

		public Object throwInvalidInstanceVariableAccess(ESSymbol className, ESSymbol fieldName, long slotIndex) {
			throw new PrimInvalidOperandException((className == null ? SymbolRegistry.symbolFor("An anonymous Smalltalk class") : className) + "." + (fieldName == null ? slotIndex.ToString() : fieldName.PrimitiveValue));
		}
		
		public Object throwIndexOutOfRangeException(long index, long minIndex, long maxIndex) {
			throw new PrimIndexBoundsExcessionException("Index = " + index.ToString() + " minValidIndex = " + minIndex + " maxValidIndex = " + maxIndex.ToString(), index, minIndex, maxIndex);
		}
		
		#endregion

		#region System Initialization

		protected virtual void intialize() {
			assignMetaclassToCanonicalClasses();
			createCanonicalNamespaces();
			establishCanonicalNamespaceStructure();
			assignCanonicalNamesToCanonicalClasses();
			establishCanonicalClassInheritanceStructure();
			assignCanonicalClassesToCanonicalNamspaces();
			addCanonicalPrimitiveDomains();
			publishCanonicalPrimitives();
			installCanonicalPrimitivesInCanonicalClasses(SymbolRegistry.symbolFor("system primitives"));
			registerAdoptedHostSystemClasses();
			createDynamicBinderRegistries();
			bindToFileSystem();
		}

		protected virtual void assignMetaclassToCanonicalClasses() {

			canonicalObjectClass.setClass(newMetaclass());
			canonicalNamespaceClass.setClass(newMetaclass());
			canonicalBehaviorClass.setClass(newMetaclass());
			canonicalClassClass.setClass(newMetaclass());
			canonicalMetaclassClass.setClass(newMetaclass());
			canonicalCompiledCodeClass.setClass(newMetaclass());
			canonicalBlockClass.setClass(newMetaclass());
			canonicalMethodClass.setClass(newMetaclass());
			canonicalAssociationClass.setClass(newMetaclass());
			canonicalBindingReferenceClass.setClass(newMetaclass());
			canonicalMessageClass.setClass(newMetaclass());
			canonicalMagnitudeClass.setClass(newMetaclass());

			canonicalCollectionClass.setClass(newMetaclass());
			canonicalKeyedCollectionClass.setClass(newMetaclass());
			canonicalIdentityDictionaryClass.setClass(newMetaclass());
			canonicalDictionaryClass.setClass(newMetaclass());
			canonicalSequenceableCollectionClass.setClass(newMetaclass());
			canonicalArrayedCollectionClass.setClass(newMetaclass());
			canonicalArrayClass.setClass(newMetaclass());
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

		protected virtual void createCanonicalNamespaces() {
			rootNamespace		= newNamespace(null, SymbolRegistry.symbolFor("Root"));
			smalltalkNamespace	= newNamespace(rootNamespace, SymbolRegistry.symbolFor("Smalltalk"));
			undeclaredNamespace	= newNamespace(rootNamespace, SymbolRegistry.symbolFor("Undeclared"));
			clrNamespace		= newNamespace(rootNamespace, SymbolRegistry.symbolFor("CLR"), true);
			clrNamespace.Assembly	= TypeGuru.objectType.Assembly;
		}

		public virtual void establishCanonicalNamespaceStructure() {
			rootNamespace.declareInSelf(true);
			rootNamespace.declareInSelfAs(SymbolRegistry.symbolFor("EssenceSharp"), true);
			clrNamespace.declareInSelfAs(SymbolRegistry.symbolFor("HostSystem"), true);
			rootNamespace.addImport(new ESImportSpec(smalltalkNamespace, AccessPrivilegeLevel.Public, ImportTransitivity.Intransitive));
			rootNamespace.addImport(new ESImportSpec(undeclaredNamespace, AccessPrivilegeLevel.Public, ImportTransitivity.Intransitive));
			rootNamespace.addImport(new ESImportSpec(clrNamespace, AccessPrivilegeLevel.Public, ImportTransitivity.Intransitive));
		}

		protected virtual void assignCanonicalNamesToCanonicalClasses() {

			canonicalObjectClass.setName(SymbolRegistry.symbolFor("Object"));
			canonicalNamespaceClass.setName(SymbolRegistry.symbolFor("Namespace"));
			canonicalBehaviorClass.setName(SymbolRegistry.symbolFor("Behavior"));
			canonicalClassClass.setName(SymbolRegistry.symbolFor("Class"));
			canonicalMetaclassClass.setName(SymbolRegistry.symbolFor("Metaclass"));
			canonicalCompiledCodeClass.setName(SymbolRegistry.symbolFor("CompiledCode"));
			canonicalBlockClass.setName(SymbolRegistry.symbolFor("Block"));
			canonicalMethodClass.setName(SymbolRegistry.symbolFor("Method"));
			canonicalAssociationClass.setName(SymbolRegistry.symbolFor("Association"));
			canonicalBindingReferenceClass.setName(SymbolRegistry.symbolFor("BindingReference"));
			canonicalMessageClass.setName(SymbolRegistry.symbolFor("Message"));
			canonicalMagnitudeClass.setName(SymbolRegistry.symbolFor("Magnitude"));

			canonicalCollectionClass.setName(SymbolRegistry.symbolFor("Collection"));
			canonicalKeyedCollectionClass.setName(SymbolRegistry.symbolFor("KeyedCollection"));
			canonicalIdentityDictionaryClass.setName(SymbolRegistry.symbolFor("IdentityDictionary"));
			canonicalDictionaryClass.setName(SymbolRegistry.symbolFor("Dictionary"));
			canonicalSequenceableCollectionClass.setName(SymbolRegistry.symbolFor("SequenceableCollection"));
			canonicalArrayedCollectionClass.setName(SymbolRegistry.symbolFor("ArrayedCollection"));
			canonicalArrayClass.setName(SymbolRegistry.symbolFor("Array"));
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

			canonicalObjectClass.Superclass = null;
			canonicalNamespaceClass.Superclass = canonicalKeyedCollectionClass;
			canonicalBehaviorClass.Superclass = canonicalNamespaceClass;
			canonicalClassClass.Superclass = canonicalBehaviorClass;
			canonicalMetaclassClass.Superclass = canonicalBehaviorClass;
			canonicalCompiledCodeClass.Superclass = canonicalObjectClass;
			canonicalBlockClass.Superclass = canonicalCompiledCodeClass;
			canonicalMethodClass.Superclass = canonicalCompiledCodeClass;
			canonicalAssociationClass.Superclass = canonicalObjectClass;
			canonicalBindingReferenceClass.Superclass = canonicalObjectClass;
			canonicalMessageClass.Superclass = canonicalObjectClass;
			canonicalMagnitudeClass.Superclass = canonicalObjectClass;

			canonicalCollectionClass.Superclass = canonicalObjectClass;
			canonicalKeyedCollectionClass.Superclass = canonicalCollectionClass;
			canonicalIdentityDictionaryClass.Superclass = canonicalKeyedCollectionClass;
			canonicalDictionaryClass.Superclass = canonicalIdentityDictionaryClass;
			canonicalSequenceableCollectionClass.Superclass = canonicalCollectionClass;
			canonicalArrayedCollectionClass.Superclass = canonicalSequenceableCollectionClass;
			canonicalArrayClass.Superclass = canonicalArrayedCollectionClass;
			canonicalByteArrayClass.Superclass = canonicalArrayedCollectionClass;
			canonicalStringClass.Superclass = canonicalArrayedCollectionClass;
			canonicalSymbolClass.Superclass = canonicalStringClass;
			canonicalHalfWordArrayClass.Superclass = canonicalArrayedCollectionClass;
			canonicalWordArrayClass.Superclass = canonicalArrayedCollectionClass;
			canonicalLongWordArrayClass.Superclass = canonicalArrayedCollectionClass;
			canonicalFloatArrayClass.Superclass = canonicalArrayedCollectionClass;
			canonicalDoubleArrayClass.Superclass = canonicalArrayedCollectionClass;
			canonicalQuadArrayClass.Superclass = canonicalArrayedCollectionClass;
			canonicalPathnameClass.Superclass = canonicalArrayedCollectionClass;

			canonicalPrimitiveValueClass.Superclass = null;
			canonicalUndefinedObjectClass.Superclass = canonicalPrimitiveValueClass;
			canonicalBooleanClass.Superclass = canonicalPrimitiveValueClass;
			canonicalFalseClass.Superclass = canonicalBooleanClass;
			canonicalTrueClass.Superclass = canonicalBooleanClass;
			canonicalCharacterClass.Superclass = canonicalPrimitiveValueClass;
			canonicalArithmeticValueClass.Superclass = canonicalPrimitiveValueClass;
			canonicalNumberClass.Superclass = canonicalArithmeticValueClass;
			canonicalIntegerClass.Superclass = canonicalNumberClass;
			canonicalSmallIntegerClass.Superclass = canonicalIntegerClass;
			canonicalRationalClass.Superclass = canonicalNumberClass;
			canonicalInvariantPrecisionRealClass.Superclass = canonicalRationalClass;
			canonicalFloatClass.Superclass = canonicalInvariantPrecisionRealClass;
			canonicalDoubleClass.Superclass = canonicalInvariantPrecisionRealClass;
			canonicalQuadClass.Superclass = canonicalInvariantPrecisionRealClass;

		}

		protected virtual void assignCanonicalClassesToCanonicalNamspaces() {

			canonicalObjectClass.setEnvironment(SmalltalkNamespace);
			canonicalNamespaceClass.setEnvironment(SmalltalkNamespace);
			canonicalBehaviorClass.setEnvironment(SmalltalkNamespace);
			canonicalClassClass.setEnvironment(SmalltalkNamespace);
			canonicalMetaclassClass.setEnvironment(SmalltalkNamespace);
			canonicalCompiledCodeClass.setEnvironment(SmalltalkNamespace);
			canonicalBlockClass.setEnvironment(SmalltalkNamespace);
			canonicalMethodClass.setEnvironment(SmalltalkNamespace);
			canonicalAssociationClass.setEnvironment(SmalltalkNamespace);
			canonicalBindingReferenceClass.setEnvironment(SmalltalkNamespace);
			canonicalMessageClass.setEnvironment(SmalltalkNamespace);
			canonicalMagnitudeClass.setEnvironment(SmalltalkNamespace);

			canonicalCollectionClass.setEnvironment(SmalltalkNamespace);
			canonicalKeyedCollectionClass.setEnvironment(SmalltalkNamespace);
			canonicalIdentityDictionaryClass.setEnvironment(SmalltalkNamespace);
			canonicalDictionaryClass.setEnvironment(SmalltalkNamespace);
			canonicalSequenceableCollectionClass.setEnvironment(SmalltalkNamespace);
			canonicalArrayedCollectionClass.setEnvironment(SmalltalkNamespace);
			canonicalArrayClass.setEnvironment(SmalltalkNamespace);
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
			addPrimitiveDomain(new ESCompiledCode.Primitives());
			addPrimitiveDomain(new ESBlock.Primitives());
			addPrimitiveDomain(new ESMethod.Primitives());
			addPrimitiveDomain(new ESAssociation.Primitives());
			addPrimitiveDomain(new ESBindingReference.Primitives());
			addPrimitiveDomain(new ESMessage.Primitives());
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
		}

		public void registerAdoptedHostSystemClasses() {

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

		protected void createDynamicBinderRegistries() {
			dynamicBindingGuru		= new DynamicBindingGuru(this);
			messageSendBinderRegistry	= new MessageSendBinder.Registry(dynamicBindingGuru);
			getVariableValueBinderRegistry	= new GetVariableValueBinder.Registry(dynamicBindingGuru);
			setVariableValueBinderRegistry	= new SetVariableValueBinder.Registry(dynamicBindingGuru);
		}

		protected virtual void bindToFileSystem() {
			EssenceSharpPath = ESFileUtility.defaultEssenceSharpPath();
			var userSearchPathsFile = new FileInfo(Path.Combine(ScriptsPath.FullName, "searchPaths"));
			if (!userSearchPathsFile.Exists) return;
			using (var stream = userSearchPathsFile.OpenText()) {
				String searchPath = "";
				do {
					EssenceLaunchPad.scriptSearchPathAddLastIfAbsent(searchPath);
					searchPath = stream.ReadLine();
				} while (searchPath != null);
			}
		}

		#endregion

		#region Bootstrap Load

		public bool ensureStartUp() {
			return ensureStartUp(false, false);
		}

		public bool ensureStartUp(bool startupVerbosely, bool reportLibraryLoadTime) {
			return ensureStartUp(new HashSet<String>(), startupVerbosely, reportLibraryLoadTime);
		}

		public bool ensureStartUp(HashSet<String> libraryNames, bool startupVerbosely, bool reportLibraryLoadTime) {

			beVerbose = startupVerbosely;

			var stopwatch = new Stopwatch();
			stopwatch.Start();

			List<ESNamespace> initialRootNamespaces;
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
				var libraryPath = libraryPathFor(name);
				if (beVerbose) Console.WriteLine("Loading library " + name + " from path " + libraryPath + " ...");
				if (!ESLibraryLoader.load(this, rootNamespace, libraryPath, startupVerbosely, true, out initialRootNamespaces)) {
					Console.WriteLine("Bootstrap load of library " + name + " failed from " + libraryPath.FullName);
					return false;
				}
				loadedLibraries.Add(name);
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

	#region Primitive Domains

	public abstract class PrimitiveDomain {
 
		public static bool asBoolean(Object value) {
			if (value is bool) return (bool)value;
			throw new MustBeBoolean();
		}

		public static FuncNs.Func<Object> asFunctor0(Object value) {
			ESBlock esValue = value as ESBlock;
			return esValue == null ? (FuncNs.Func<Object>)value : esValue.F0;
		}

		protected ESKernel				kernel			= null;
		protected SymbolRegistry			symbolRegistry		= null;
		protected ESBehavior				domainClass		= null;
		protected Dictionary<String, Delegate>		primitiveRegistry	= new Dictionary<String, Delegate>();

		public abstract PrimitiveDomainType Type {
			get;
		}

		protected virtual void bindToKernel() {
		}

		protected virtual void unbindFromKernel() {
		}

		public ESKernel Kernel {
			get {return kernel;}

			internal 
			set {	if (kernel == value) return;
				if (kernel !=null) {
					unbindFromKernel();
					symbolRegistry = null;
				}
				kernel = value;
				if (kernel != null) {
					symbolRegistry = kernel.SymbolRegistry;
					bindToKernel();
				}}

		}

		public SymbolRegistry SymbolRegistry {
			get {return symbolRegistry;}
		}

		public ESBehavior DomainClass {
			get {return domainClass;}
		}

		public ObjectStateArchitecture ClassArchitecture {
			get {return DomainClass.Architecture;}
		}

		public ObjectStateArchitecture InstanceArchitecture {
			get {return DomainClass.InstanceArchitecture;}
		}

		public ESSymbol ClassName {
			get {return DomainClass.Name;}
		}

		public ESNamespace Environment {
			get {return DomainClass.Environment;}
		}

		public void publishPrimitive(String primitiveName, Delegate function) {
			primitiveRegistry[primitiveName] = function;
		}

		public bool getPrimitiveFunction(String primitiveName, out Delegate function) {
			return primitiveRegistry.TryGetValue(primitiveName, out function);
		}

		public ESMethod getPrimitiveMethod(String primitiveName, ESSymbol selector) {
			Delegate function;
			if (!getPrimitiveFunction(primitiveName, out function)) {
				return null;
			}
			return kernel.newMethod(selector, function);
		}

		public virtual void publishCanonicalPrimitives() {
		}

		public void publishedPrimitivesDo(System.Action<PrimitiveDomainType, String, Delegate> enumerator3) {
			foreach(var selectorFunctionAssoc in primitiveRegistry) {
				var name = selectorFunctionAssoc.Key;
				var function = selectorFunctionAssoc.Value;
				enumerator3(Type, name, function);
			}
		}

		public void installPublishedPrimitivesInDomainClass(ESSymbol protocol) {
			installPublishedPrimitivesInClass(protocol, DomainClass);
		}

		public void installPublishedPrimitivesInClass(ESSymbol protocol, ESBehavior targetClass) {
			publishedPrimitivesDo((PrimitiveDomainType domain, String name, Delegate function) => {
				targetClass.addMethod(kernel.newMethod(SymbolRegistry.symbolFor(name), function));
			});
		}

		public void generateDefaultPrimitiveMethodSource(DirectoryInfo basePath) {

			var folderPath = basePath.FullName;
			DomainClass.pathname().elementsDo(name => folderPath = Path.Combine(folderPath, name));
			var filePath = Path.Combine(folderPath, "system.primitives");
			var methodsFile = new FileInfo(filePath);
				
			using (var writeStream = methodsFile.CreateText()) {
				writeStream.Write("\t\"Published system primitives for ");
				writeStream.Write(DomainClass.NameString);
				writeStream.WriteLine("\"");
				writeStream.WriteLine("\t\"**** Code generated in order to document built-in system primitives (which may or may not be used by any classes.) So, although the library loader ignores this file, DO NOT MODIFY IT! ****\"");
				publishedPrimitivesDo((PrimitiveDomainType domain, String name, Delegate function) => {
					writeStream.WriteLine("");
					writeStream.Write("\t");
					writeStream.WriteLine("protocol: #'system primitives' method:");
					writeStream.Write("\t[## ");
					var selector = symbolRegistry.symbolFor(name);
					MethodInfo method;
					ParameterInfo[] parameters;
					switch (selector.Type) {
						case SymbolType.Keyword:
							method = function.Method;
							parameters = method.GetParameters();
							var parameterIndex = 1;
							selector.keywordsDo(keyword => {
								writeStream.Write(keyword);
								writeStream.Write(": ");
								writeStream.Write(parameters[parameterIndex++].Name);
								writeStream.Write(" ");
							});
							break;
						case SymbolType.BinaryMessageSelector:
							method = function.Method;
							parameters = method.GetParameters();
							writeStream.Write(name);
							writeStream.Write(" ");
							writeStream.Write(parameters[1].Name);
							break;
						default:
							writeStream.Write(name);
							break;
					}
					writeStream.WriteLine("");
					writeStream.WriteLine("");
					writeStream.Write("\t\t<primitive: ");
					if (selector.Type != SymbolType.Identifier) writeStream.Write("#");
					writeStream.Write(name);
					writeStream.Write(" domain: ");
					writeStream.Write(Type.ToString());
					writeStream.WriteLine(">");
					writeStream.WriteLine("\t];");

				});
			}

		}

	}

	public class UndefinedObjectPrimitives : PrimitiveDomain {

		protected override void bindToKernel() {
			domainClass = kernel.UndefinedObjectClass;
		}

		public override PrimitiveDomainType Type {
			get {return PrimitiveDomainType.Nil;}
		}

		#region Primitive Definitions

		public Object _class_(Object receiver) {
			return domainClass;
		}
		
		public Object _isMemberOf_(Object receiver, Object aBehavior) {
			return ReferenceEquals(domainClass, aBehavior);
		}
		
		public Object _isKindOf_(Object receiver, Object aBehavior) {
			ESBehavior esClass;
			try {
				esClass = (ESBehavior)aBehavior;
			} catch (InvalidCastException) {
				throw new PrimInvalidOperandException();
			}
			return domainClass.includesBehavior(esClass);
		}
		
		public Object _hasSameValueAs_(Object receiver, Object comparand) {
			return ReferenceEquals(null, comparand);
		}
		
		public Object _hash_(Object receiver) {
			return -1;
		}

		public Object _ifNil_(Object receiver, Object nilAction) {
			return ESObject.asFunctor0(nilAction)();
		}

		public Object _ifNilIfNotNil_(Object receiver, Object nilAction, Object notNilAction) {
			return ESObject.asFunctor0(nilAction)();
		}

		public Object _ifNotNilIfNil_(Object receiver, Object notNilAction, Object nilAction) {
			return ESObject.asFunctor0(nilAction)();
		}

		#endregion

		public override void publishCanonicalPrimitives() {

			publishPrimitive("class",							new FuncNs.Func<Object, Object>(_class_));
			publishPrimitive("isMemberOf:",							new FuncNs.Func<Object, Object, Object>(_isMemberOf_));
			publishPrimitive("isKindOf:",							new FuncNs.Func<Object, Object, Object>(_isKindOf_));

			publishPrimitive("=",								new FuncNs.Func<Object, Object, Object>(_hasSameValueAs_));
			publishPrimitive("hash",							new FuncNs.Func<Object, Object>(_hash_));

			publishPrimitive("ifNil:",							new FuncNs.Func<Object, Object, Object>(_ifNil_));
			publishPrimitive("ifNil:ifNotNil:",						new FuncNs.Func<Object, Object, Object, Object>(_ifNilIfNotNil_));
			publishPrimitive("ifNotNil:ifNil:",						new FuncNs.Func<Object, Object, Object, Object>(_ifNotNilIfNil_));

		}

	}

	public class FalsePrimitives : PrimitiveDomain {

		protected override void bindToKernel() {
			domainClass = kernel.UndefinedObjectClass;
		}

		public override PrimitiveDomainType Type {
			get {return PrimitiveDomainType.False;}
		}
		
		#region Primitive Definitions

		public Object _class_(Object receiver) {
			return domainClass;
		}
		
		public Object _isMemberOf_(Object receiver, Object aBehavior) {
			return ReferenceEquals(domainClass, aBehavior);
		}

		public Object _isKindOf_(Object receiver, Object aBehavior) {
			ESBehavior esClass;
			try {
				esClass = (ESBehavior)aBehavior;
			} catch (InvalidCastException) {
				throw new PrimInvalidOperandException();
			}
			return domainClass.includesBehavior(esClass);
		}
		
		public Object _hasSameValueAs_(Object receiver, Object comparand) {
			return comparand is bool ? !(bool)comparand : false;
		}
		
		public Object _hash_(Object receiver) {
			return 0;
		}

		public Object _not_(Object receiver) {
			return true;
		}

		public Object _and_(Object receiver, Object operand) {
			return receiver;
		}

		public Object _andValueOf_(Object receiver, Object operand) {
			return receiver;
		}

		public Object _or_(Object receiver, Object operand) {
			return operand;
		}

		public Object _orValueOf_(Object receiver, Object operand) {
			return asFunctor0(operand)();
		}

		public Object _xor_(Object receiver, Object operand) {
			return operand;
		}

		public Object _ifFalse_(Object receiver, Object operand) {
			return asFunctor0(operand)();
		}

		public Object _ifTrue_(Object receiver, Object operand) {
			return null;
		}

		public Object _ifFalseIfTrue_(Object receiver, Object falseAction, Object trueAction) {
			return asFunctor0(falseAction)();
		}

		public Object _ifTrueIfFalse_(Object receiver, Object trueAction, Object falseAction) {
			return asFunctor0(falseAction)();
		}
		
		#endregion

		public override void publishCanonicalPrimitives() {

			publishPrimitive("class",							new FuncNs.Func<Object, Object>(_class_));
			publishPrimitive("isMemberOf:",							new FuncNs.Func<Object, Object, Object>(_isMemberOf_));
			publishPrimitive("isKindOf:",							new FuncNs.Func<Object, Object, Object>(_isKindOf_));

			publishPrimitive("=",								new FuncNs.Func<Object, Object, Object>(_hasSameValueAs_));
			publishPrimitive("hash",							new FuncNs.Func<Object, Object>(_hash_));

			publishPrimitive("not",								new FuncNs.Func<Object, Object>(_not_));
			publishPrimitive("&",								new FuncNs.Func<Object, Object, Object>(_and_));
			publishPrimitive("and:",							new FuncNs.Func<Object, Object, Object>(_andValueOf_));
			publishPrimitive("|",								new FuncNs.Func<Object, Object, Object>(_or_));
			publishPrimitive("or:",								new FuncNs.Func<Object, Object, Object>(_orValueOf_));
			publishPrimitive("xor:",							new FuncNs.Func<Object, Object, Object>(_xor_));
			publishPrimitive("ifFalse:",							new FuncNs.Func<Object, Object, Object>(_ifFalse_));
			publishPrimitive("ifTrue:",							new FuncNs.Func<Object, Object, Object>(_ifTrue_));
			publishPrimitive("ifFalse:ifTrue:",						new FuncNs.Func<Object, Object, Object, Object>(_ifFalseIfTrue_));
			publishPrimitive("ifTrue:ifFalse:",						new FuncNs.Func<Object, Object, Object, Object>(_ifTrueIfFalse_));

		}

	}

	public class TruePrimitives : PrimitiveDomain {

		protected override void bindToKernel() {
			domainClass = kernel.TrueClass;
		}

		public override PrimitiveDomainType Type {
			get {return PrimitiveDomainType.True;}
		}
		
		#region Primitive Definitions

		public Object _class_(Object receiver) {
			return domainClass;
		}
		
		public Object _isMemberOf_(Object receiver, Object aBehavior) {
			return ReferenceEquals(domainClass, aBehavior);
		}
		
		public Object _isKindOf_(Object receiver, Object aBehavior) {
			ESBehavior esClass;
			try {
				esClass = (ESBehavior)aBehavior;
			} catch (InvalidCastException) {
				throw new PrimInvalidOperandException();
			}
			return domainClass.includesBehavior(esClass);
		}
		
		public Object _hasSameValueAs_(Object receiver, Object comparand) {
			return comparand is bool ? (bool)comparand : false;
		}
		
		public Object _hash_(Object receiver) {
			return 1;
		}

		public Object _not_(Object receiver) {
			return false;
		}

		public Object _and_(Object receiver, Object operand) {
			return operand;
		}

		public Object _andValueOf_(Object receiver, Object operand) {
			return asFunctor0(operand)();
		}

		public Object _or_(Object receiver, Object operand) {
			return receiver;
		}

		public Object _orValueOf_(Object receiver, Object operand) {
			return receiver;
		}

		public Object _xor_(Object receiver, Object operand) {
			return asBoolean(operand) ? false : receiver;
		}

		public Object _ifFalse_(Object receiver, Object operand) {
			return null;
		}

		public Object _ifTrue_(Object receiver, Object operand) {
			return asFunctor0(operand)();
		}

		public Object _ifFalseIfTrue_(Object receiver, Object falseAction, Object trueAction) {
			return asFunctor0(trueAction)();
		}

		public Object _ifTrueIfFalse_(Object receiver, Object trueAction, Object falseAction) {
			return asFunctor0(trueAction)();
		}
		
		#endregion

		public override void publishCanonicalPrimitives() {

			publishPrimitive("class",							new FuncNs.Func<Object, Object>(_class_));
			publishPrimitive("isMemberOf:",							new FuncNs.Func<Object, Object, Object>(_isMemberOf_));
			publishPrimitive("isKindOf:",							new FuncNs.Func<Object, Object, Object>(_isKindOf_));

			publishPrimitive("=",								new FuncNs.Func<Object, Object, Object>(_hasSameValueAs_));
			publishPrimitive("hash",							new FuncNs.Func<Object, Object>(_hash_));

			publishPrimitive("not",								new FuncNs.Func<Object, Object>(_not_));
			publishPrimitive("&",								new FuncNs.Func<Object, Object, Object>(_and_));
			publishPrimitive("and:",							new FuncNs.Func<Object, Object, Object>(_andValueOf_));
			publishPrimitive("|",								new FuncNs.Func<Object, Object, Object>(_or_));
			publishPrimitive("or:",								new FuncNs.Func<Object, Object, Object>(_orValueOf_));
			publishPrimitive("xor:",							new FuncNs.Func<Object, Object, Object>(_xor_));
			publishPrimitive("ifFalse:",							new FuncNs.Func<Object, Object, Object>(_ifFalse_));
			publishPrimitive("ifTrue:",							new FuncNs.Func<Object, Object, Object>(_ifTrue_));
			publishPrimitive("ifFalse:ifTrue:",						new FuncNs.Func<Object, Object, Object, Object>(_ifFalseIfTrue_));
			publishPrimitive("ifTrue:ifFalse:",						new FuncNs.Func<Object, Object, Object, Object>(_ifTrueIfFalse_));

		}

	}

	public class CharacterPrimitives : PrimitiveDomain {

		protected override void bindToKernel() {
			domainClass = kernel.CharacterClass;
		}

		public override PrimitiveDomainType Type {
			get {return PrimitiveDomainType.Char;}
		}
		
		#region Primitive Definitions

		public Object _class_(Object receiver) {
			return domainClass;
		}
		
		public Object _isMemberOf_(Object receiver, Object aBehavior) {
			return ReferenceEquals(domainClass, aBehavior);
		}
		
		public Object _isKindOf_(Object receiver, Object aBehavior) {
			ESBehavior esClass;
			try {
				esClass = (ESBehavior)aBehavior;
			} catch (InvalidCastException) {
				throw new PrimInvalidOperandException();
			}
			return domainClass.includesBehavior(esClass);
		}
		
		public Object _hasSameValueAs_(Object receiver, Object comparand) {
			char chComparand;
			try {
				chComparand = (char)comparand;
			} catch (InvalidCastException) {
				throw new PrimInvalidOperandException();
			}
			return (char)receiver == chComparand;
		}
		
		public Object _hash_(Object receiver) {
			return (long)(char)receiver;
		}
		
		public Object _compareTo_(Object receiver, Object comparand) {
			try {
				return (long)Math.Sign((char)receiver - (char)comparand);
			} catch (Exception ex) {
				throw new PrimitiveFailException(ex);
			}
		}
		
		public Object _isLessThan_(Object receiver, Object comparand) {
			try {
				return (char)receiver < (char)comparand;
			} catch (Exception ex) {
				throw new PrimitiveFailException(ex);
			}
		}
		
		public Object _asInteger_(Object receiver) {
			return (long)(char)receiver;
		}

		#endregion

		public override void publishCanonicalPrimitives() {

			publishPrimitive("class",							new FuncNs.Func<Object, Object>(_class_));
			publishPrimitive("isMemberOf:",							new FuncNs.Func<Object, Object, Object>(_isMemberOf_));
			publishPrimitive("isKindOf:",							new FuncNs.Func<Object, Object, Object>(_isKindOf_));

			publishPrimitive("=",								new FuncNs.Func<Object, Object, Object>(_hasSameValueAs_));
			publishPrimitive("hash",							new FuncNs.Func<Object, Object>(_hash_));

			publishPrimitive("compareTo:",							new FuncNs.Func<Object, Object, Object>(_compareTo_));
			publishPrimitive("<",								new FuncNs.Func<Object, Object, Object>(_isLessThan_));

			publishPrimitive("asInteger",							new FuncNs.Func<Object, Object>(_asInteger_));

		}

	}

	public class SmallIntegerPrimitives : PrimitiveDomain {

		protected override void bindToKernel() {
			domainClass = kernel.SmallIntegerClass;
		}

		public override PrimitiveDomainType Type {
			get {return PrimitiveDomainType.SmallInteger;}
		}
		
		#region Primitive Definitions

		public Object _class_(Object receiver) {
			return domainClass;
		}
		
		public Object _isMemberOf_(Object receiver, Object aBehavior) {
			return ReferenceEquals(domainClass, aBehavior);
		}
		
		public Object _isKindOf_(Object receiver, Object aBehavior) {
			ESBehavior esClass;
			try {
				esClass = (ESBehavior)aBehavior;
			} catch {
				throw new PrimInvalidOperandException();
			}
			return domainClass.includesBehavior(esClass);
		}
		
		public Object _hasSameValueAs_(Object receiver, Object comparand) {
			long numComparand;
			try {
				numComparand = (long)comparand;
			} catch {
				throw new PrimInvalidOperandException();
			}
			return (long)receiver == numComparand;
		}
		
		public Object _hash_(Object receiver) {
			return (long)receiver;
		}
	
		public Object _compareTo_(Object receiver, Object comparand) {
			long numComparand;
			try {
				numComparand = (long)comparand;
			} catch {
				throw new PrimInvalidOperandException("Comparand must be a SmallInteger");
			}
			return (long)Math.Sign((long)receiver - numComparand);
		}

		public Object _lessThan_(Object receiver, Object comparand) {
			long numComparand;
			try {
				numComparand = (long)comparand;
			} catch {
				throw new PrimInvalidOperandException("Comparand must be a SmallInteger");
			}
			return ((long)receiver - numComparand) < 0;
		}

		public Object _sign_(Object receiver) {
			var value = (long)receiver;
			if (value > 0L) return 1L;
			if (value < 0L) return -1L;
			return 0L;
		}

		public Object _isZero_(Object receiver) {
			return (long)receiver == 0;
		}

		public Object _positive_(Object receiver) {
			return (long)receiver >= 0;
		}

		public Object _negative_(Object receiver) {
			return (long)receiver < 0;
		}

		public Object _abs_(Object receiver) {
			long value = (long)receiver;
			return value >= 0 ? receiver : -value;
		}

		public Object _negated_(Object receiver) {
			return -(long)receiver;
		}
		
		public Object _sumWith_(Object receiver, Object operand) {
			long numOperand;
			try {
				numOperand = (long)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			return ((long)receiver) + numOperand;
		}
		
		public Object _differenceFrom_(Object receiver, Object operand) {
			long numOperand;
			try {
				numOperand = (long)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			return ((long)receiver) - numOperand;
		}
		
		public Object _productWith_(Object receiver, Object operand) {
			long numOperand;
			try {
				numOperand = (long)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			return ((long)receiver) * numOperand;
		}
		
		public Object _dividedBy_(Object receiver, Object operand) {
			long numOperand;
			try {
				numOperand = (long)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			return ((long)receiver) / (double)numOperand; // Result SHOULD be a Fraction, but those aren't implemented yet....
		}
		
		public Object _reciprocal_(Object receiver) {
			return 1.0 / (double)(long)receiver; // Result SHOULD be a Fraction, but those aren't implemented yet....
		}
		
		public Object _asCharacter_(Object receiver) {
			return (char)(long)receiver;
		}

		public Object _asFloat_(Object receiver) {
			return (float)(long)receiver;
		}

		public Object _asDouble_(Object receiver) {
			return (double)(long)receiver;
		}

		public Object _asQuad_(Object receiver) {
			return (decimal)(long)receiver;
		}

		public Object _roundTo_(Object receiver, Object operand) {
			// Smalltalk: 53 roundTo: 5 => 55; 1263.48 roundTo: 0.25 => 1263.50.
			// C-Lang: 53.roundTo(5) => 55; 1263.48.roundTo(0.25) => 1263.50.
			long modulus;
			try {
				modulus = (long)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			double self = (double)(long)receiver;
			return (long)(Math.Round(self / modulus) * modulus);
		}
		
		public Object _truncateTo_(Object receiver, Object modulus) {
			// Smalltalk: 53 truncateTo: 5 => 50; 1263.48 truncateTo: 0.25 => 1263.25.
			// C-Lang: 53.truncateTo(5) => 50; 1263.48.truncateTo(0.25) => 1263.25.
			long intModulus;
			try {
				intModulus = (long)modulus;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			double self = (long)receiver;
			return (self / intModulus) * intModulus;
		}
		
		public Object _dividedToIntegerBy_(Object receiver, Object operand) {
			long numOperand;
			try {
				numOperand = (long)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			return ((long)receiver) / numOperand; 
		}
		
		public Object _dividedAndFlooredToIntegerBy_(Object receiver, Object operand) {
			long longOperand;
			double doubleOperand;
			try {
				longOperand = (long)operand;
				doubleOperand = (double)longOperand;
			} catch {
				try {
					doubleOperand = (double)operand;
				} catch {
					throw new PrimInvalidOperandException("Operand must be a SmallInteger");
				}
			}
			return (long)Math.Floor((long)receiver / doubleOperand); 
		}
		
		public Object _remainderFromModulus_(Object receiver, Object operand) {
			// Implements standard keyword message "rem: operand" 
			// Smalltalk: ^receiver - ((receiver quo: operand) * operand)
			// C-Lang: return receiver - ((long)(receiver / operand) * operand); -- Or just "receiver % operand" :-)
			long numOperand;
			try {
				numOperand = (long)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			return (long)receiver % numOperand;
		}
		
		public Object _flooredRemainderFromModulus_(Object receiver, Object operand) {
			// Implements standard binary message "\\ operand" 
			// Smalltalk: ^receiver - (receiver // operand * operand)
			// C-Lang: return receiver - ((long)Math.Floor(receiver / (double)operand) * operand);
			long numOperand;
			try {
				numOperand = (long)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			long self = (long)receiver;
			return self - ((long)Math.Floor(self / (double)numOperand) * numOperand);
		}
		
		public Object _raisedTo_(Object receiver, Object operand) {
			long numOperand;
			try {
				numOperand = (long)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			return (long)Math.Pow((long)receiver, numOperand);
		}
		
		public Object _log_(Object receiver, Object operand) {
			long numOperand;
			try {
				numOperand = (long)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			return Math.Log((long)receiver, numOperand);
		}
		
		public Object _ln_(Object receiver) {
			return Math.Log((long)receiver, Math.E);
		}

		public Object _timesRepeat_(Object receiver, Object action) {
			var limit = (long)receiver;
			var actionFunction = ESObject.asFunctor0(action);
			for (var count = 0; count < limit; count++) {
				actionFunction();
			}
			return receiver;
		}

		public Object _toDo_(Object receiver, Object end, Object enumerator) {
			long primEnd;
			try {
				primEnd = (long)end;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			var enumerationFunction = ESObject.asFunctor1(enumerator);
			var primStart = (long)receiver;
			for (var inductionValue = primStart; inductionValue <= primEnd; inductionValue++) {
				enumerationFunction(inductionValue);
			}
			return receiver;
		}

		public Object _toByDo_(Object receiver, Object end, Object step, Object enumerator) {
			long primEnd;
			try {
				primEnd = (long)end;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			long primStep;
			try {
				primStep = (long)step;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			var enumerationFunction = ESObject.asFunctor1(enumerator);
			var primStart = (long)receiver;
			if (primStep > 0) {
				for (var inductionValue = primStart; inductionValue <= primEnd; inductionValue += primStep) {
					enumerationFunction(inductionValue);
				}
			} else if (primStep < 0) {
				for (var inductionValue = primStart; inductionValue >= primEnd; inductionValue -= primStep) {
					enumerationFunction(inductionValue);
				}
			} else {
				throw new PrimInvalidOperandException("Step value must not be zero");
			}
			return receiver;
		}

		public Object _bitAnd_(Object receiver, Object operand) {
			long numOperand;
			try {
				numOperand = (long)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			return ((long)receiver) & numOperand;
		}
				
		public Object _bitOr_(Object receiver, Object operand) {
			long numOperand;
			try {
				numOperand = (long)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			return ((long)receiver) | numOperand;
		}
				
		public Object _bitXor_(Object receiver, Object operand) {
			long numOperand;
			try {
				numOperand = (long)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a SmallInteger");
			}
			return ((long)receiver) ^ numOperand;
		}
				
		public Object _bitShift_(Object receiver, Object operand) {
			long numOperand;
			int shiftExtent;
			try {
				numOperand = (long)operand;
				shiftExtent = (int)numOperand;
			} catch {
				try {
					shiftExtent = (int)operand;
				} catch {
					throw new PrimInvalidOperandException("Operand must be a SmallInteger");
				}
			}
			return shiftExtent >= 0 ? ((long)receiver) << shiftExtent : ((long)receiver) >> -shiftExtent;
		}
				
		public Object _bitShiftLeft_(Object receiver, Object operand) {
			long numOperand;
			int shiftExtent;
			try {
				numOperand = (long)operand;
				shiftExtent = (int)numOperand;
			} catch {
				try {
					shiftExtent = (int)operand;
				} catch {
					throw new PrimInvalidOperandException("Operand must be a SmallInteger");
				}
			}
			return ((long)receiver) << shiftExtent;
		}
				
		public Object _bitShiftRight_(Object receiver, Object operand) {
			long numOperand;
			int shiftExtent;
			try {
				numOperand = (long)operand;
				shiftExtent = (int)numOperand;
			} catch {
				try {
					shiftExtent = (int)operand;
				} catch {
					throw new PrimInvalidOperandException("Operand must be a SmallInteger");
				}
			}
			return ((long)receiver) >> shiftExtent;
		}

		#endregion

		public override void publishCanonicalPrimitives() {

			publishPrimitive("class",						new FuncNs.Func<Object, Object>(_class_));
			publishPrimitive("isMemberOf:",						new FuncNs.Func<Object, Object, Object>(_isMemberOf_));
			publishPrimitive("isKindOf:",						new FuncNs.Func<Object, Object, Object>(_isKindOf_));

			publishPrimitive("=",							new FuncNs.Func<Object, Object, Object>(_hasSameValueAs_));
			publishPrimitive("hash",						new FuncNs.Func<Object, Object>(_hash_));

			publishPrimitive("compareTo:",						new FuncNs.Func<Object, Object, Object>(_compareTo_));
			publishPrimitive("<",							new FuncNs.Func<Object, Object, Object>(_lessThan_));

			publishPrimitive("asCharacter",						new FuncNs.Func<Object, Object>(_asCharacter_));
			publishPrimitive("asFloat",						new FuncNs.Func<Object, Object>(_asFloat_));
			publishPrimitive("asDouble",						new FuncNs.Func<Object, Object>(_asDouble_));
			publishPrimitive("asQuad",						new FuncNs.Func<Object, Object>(_asQuad_));

			publishPrimitive("sign",						new FuncNs.Func<Object, Object>(_sign_));
			publishPrimitive("isZero",						new FuncNs.Func<Object, Object>(_isZero_));
			publishPrimitive("positive",						new FuncNs.Func<Object, Object>(_positive_));
			publishPrimitive("negative",						new FuncNs.Func<Object, Object>(_negative_));
			publishPrimitive("abs",							new FuncNs.Func<Object, Object>(_abs_));
			publishPrimitive("negated",						new FuncNs.Func<Object, Object>(_negated_));

			publishPrimitive("+",							new FuncNs.Func<Object, Object, Object>(_sumWith_));
			publishPrimitive("-",							new FuncNs.Func<Object, Object, Object>(_differenceFrom_));
			publishPrimitive("*",							new FuncNs.Func<Object, Object, Object>(_productWith_));
			publishPrimitive("/",							new FuncNs.Func<Object, Object, Object>(_dividedBy_));
			publishPrimitive("reciprocal",						new FuncNs.Func<Object, Object>(_reciprocal_));

			publishPrimitive("roundTo:",						new FuncNs.Func<Object, Object, Object>(_roundTo_));
			publishPrimitive("truncateTo:",						new FuncNs.Func<Object, Object, Object>(_truncateTo_));
			publishPrimitive("quo:",						new FuncNs.Func<Object, Object, Object>(_dividedToIntegerBy_));
			publishPrimitive("//",							new FuncNs.Func<Object, Object, Object>(_dividedAndFlooredToIntegerBy_));
			publishPrimitive("rem:",						new FuncNs.Func<Object, Object, Object>(_remainderFromModulus_));
			publishPrimitive(@"\\",							new FuncNs.Func<Object, Object, Object>(_flooredRemainderFromModulus_));
			publishPrimitive("**",							new FuncNs.Func<Object, Object, Object>(_raisedTo_));
			publishPrimitive("log:",						new FuncNs.Func<Object, Object, Object>(_log_));
			publishPrimitive("ln",							new FuncNs.Func<Object, Object>(_ln_));

			publishPrimitive("timesRepeat:",					new FuncNs.Func<Object, Object, Object>(_timesRepeat_));
			publishPrimitive("to:do:",						new FuncNs.Func<Object, Object, Object, Object>(_toDo_));
			publishPrimitive("to:by:do:",						new FuncNs.Func<Object, Object, Object, Object, Object>(_toByDo_));

			publishPrimitive("bitAnd:",						new FuncNs.Func<Object, Object, Object>(_bitAnd_));
			publishPrimitive("bitOr:",						new FuncNs.Func<Object, Object, Object>(_bitOr_));
			publishPrimitive("bitXor:",						new FuncNs.Func<Object, Object, Object>(_bitXor_));
			
			publishPrimitive("bitShift:",		/* bitShift: */			new FuncNs.Func<Object, Object, Object>(_bitShift_));
			publishPrimitive("<<",							new FuncNs.Func<Object, Object, Object>(_bitShiftLeft_));
			publishPrimitive(">>",							new FuncNs.Func<Object, Object, Object>(_bitShiftRight_));

		}

	}

	public class SinglePrecisionPrimitives : PrimitiveDomain {

		protected override void bindToKernel() {
			domainClass = kernel.FloatClass;
		}

		public override PrimitiveDomainType Type {
			get {return PrimitiveDomainType.SinglePrecision;}
		}

		#region Primitives

		public Object _class_(Object receiver) {
			return domainClass;
		}
		
		public Object _isMemberOf_(Object receiver, Object aBehavior) {
			return ReferenceEquals(domainClass, aBehavior);
		}
		
		public Object _isKindOf_(Object receiver, Object aBehavior) {
			ESBehavior esClass;
			try {
				esClass = (ESBehavior)aBehavior;
			} catch (InvalidCastException) {
				throw new PrimInvalidOperandException();
			}
			return domainClass.includesBehavior(esClass);
		}
		
		public Object _hasSameValueAs_(Object receiver, Object comparand) {
			float numComparand;
			try {
				numComparand = (float)comparand;
			} catch (InvalidCastException) {
				throw new PrimInvalidOperandException();
			}
			return (float)receiver == numComparand;
		}
		
		public Object _hash_(Object receiver) {
			return (long)(float)receiver;
		}
	
		public Object _compareTo_(Object receiver, Object comparand) {
			float numComparand;
			try {
				numComparand = (float)comparand;
			} catch {
				throw new PrimInvalidOperandException("Comparand must be a Float");
			}
			return (long)Math.Sign((float)receiver - numComparand);
		}

		public Object _lessThan_(Object receiver, Object comparand) {
			float numComparand;
			try {
				numComparand = (float)comparand;
			} catch {
				throw new PrimInvalidOperandException("Comparand must be a Float");
			}
			return ((float)receiver - numComparand) < 0;
		}

		public Object _sign_(Object receiver) {
			var value = (float)receiver;
			if (value > 0.0f) return 1L;
			if (value < 0.0f) return -1L;
			return 0L;
		}

		public Object _isZero_(Object receiver) {
			return (float)receiver == 0.0f;
		}

		public Object _positive_(Object receiver) {
			return (float)receiver >= 0.0f;
		}

		public Object _negative_(Object receiver) {
			return (float)receiver < 0.0f;
		}

		public Object _abs_(Object receiver) {
			float value = (float)receiver;
			return value >= 0.0f ? receiver : -value;
		}

		public Object _negated_(Object receiver) {
			return -(float)receiver;
		}
		
		public Object _sumWith_(Object receiver, Object operand) {
			float numOperand;
			try {
				numOperand = (float)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			return ((float)receiver) + numOperand;
		}
		
		public Object _differenceFrom_(Object receiver, Object operand) {
			float numOperand;
			try {
				numOperand = (float)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			return ((float)receiver) - numOperand;
		}
		
		public Object _productWith_(Object receiver, Object operand) {
			float numOperand;
			try {
				numOperand = (float)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			return ((float)receiver) * numOperand;
		}
		
		public Object _dividedBy_(Object receiver, Object operand) {
			float numOperand;
			try {
				numOperand = (float)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			return ((float)receiver) / numOperand; 
		}
		
		public Object _reciprocal_(Object receiver) {
			return 1.0f / (float)receiver; 
		}
		
		public Object _asCharacter_(Object receiver) {
			return (char)(uint)(float)receiver;
		}

		public Object _asInteger_(Object receiver) {
			return (long)(float)receiver;
		}

		public Object _asDouble_(Object receiver) {
			return (double)(float)receiver;
		}

		public Object _asQuad_(Object receiver) {
			return (decimal)(float)receiver;
		}

		public Object _ceiling_(Object receiver) {
			return (float)Math.Ceiling((float)receiver);
		}

		public Object _floor_(Object receiver) {
			return (float)Math.Floor((float)receiver);
		}

		public Object _rounded_(Object receiver) {
			return (float)Math.Round((float)receiver);
		}

		public Object _truncated_(Object receiver) {
			return (long)(float)receiver;
		}

		public Object _roundTo_(Object receiver, Object operand) {
			// Smalltalk: 53 roundTo: 5 => 55; 1263.48 roundTo: 0.25 => 1263.50.
			// C-Lang: 53.roundTo(5) => 55; 1263.48.roundTo(0.25) => 1263.50.
			float modulus;
			try {
				modulus = (float)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			float self = (float)receiver;
			return (float)(Math.Round(self / modulus) * modulus);
		}
		
		public Object _truncateTo_(Object receiver, Object modulus) {
			// Smalltalk: 53 truncateTo: 5 => 50; 1263.48 truncateTo: 0.25 => 1263.25.
			// C-Lang: 53.truncateTo(5) => 50; 1263.48.truncateTo(0.25) => 1263.25.
			float numModulus;
			try {
				numModulus = (float)modulus;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			double self = (float)receiver;
			return ((long)(self / numModulus)) * numModulus;
		}
		
		public Object _dividedToIntegerBy_(Object receiver, Object operand) {
			float numOperand;
			try {
				numOperand = (float)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			return (long)((float)receiver / numOperand); 
		}
		
		public Object _dividedAndFlooredToIntegerBy_(Object receiver, Object operand) {
			float numOperand;
			try {
				numOperand = (float)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			return (long)Math.Floor((float)receiver / numOperand); 
		}
		
		public Object _remainderFromModulus_(Object receiver, Object operand) {
			// Implements standard keyword message "rem: operand" 
			// Smalltalk: ^receiver - ((receiver quo: operand) * operand)
			// C-Lang: return receiver - ((long)(receiver / operand) * operand); -- Or just "receiver % operand" :-)
			float numOperand;
			try {
				numOperand = (float)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			return (float)receiver % numOperand;
		}
		
		public Object _flooredRemainderFromModulus_(Object receiver, Object operand) {
			// Implements standard binary message "\\ operand" 
			// Smalltalk: ^receiver - (receiver // operand * operand)
			// C-Lang: return receiver - ((long)Math.Floor(receiver / (double)operand) * operand);
			float numOperand;
			try {
				numOperand = (float)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			float self = (float)receiver;
			return self - ((long)Math.Floor(self / numOperand) * numOperand);
		}
		
		public Object _raisedTo_(Object receiver, Object operand) {
			float numOperand;
			try {
				numOperand = (float)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			return (float)Math.Pow((float)receiver, numOperand);
		}
		
		public Object _log_(Object receiver, Object operand) {
			float numOperand;
			try {
				numOperand = (float)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			return (float)Math.Log((float)receiver, numOperand);
		}
		
		public Object _ln_(Object receiver) {
			return (float)Math.Log((float)receiver, (float)Math.E);
		}

		public Object _toDo_(Object receiver, Object end, Object enumerator) {
			float primEnd;
			try {
				primEnd = (float)end;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			var enumerationFunction = ESObject.asFunctor1(enumerator);
			var primStart = (float)receiver;
			for (var inductionValue = primStart; inductionValue <= primEnd; inductionValue++) {
				enumerationFunction(inductionValue);
			}
			return receiver;
		}

		public Object _toByDo_(Object receiver, Object end, Object step, Object enumerator) {
			float primEnd;
			try {
				primEnd = (float)end;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			float primStep;
			try {
				primStep = (float)step;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Float");
			}
			var enumerationFunction = ESObject.asFunctor1(enumerator);
			var primStart = (float)receiver;
			if (primStep > 0) {
				for (var inductionValue = primStart; inductionValue <= primEnd; inductionValue += primStep) {
					enumerationFunction(inductionValue);
				}
			} else if (primStep < 0) {
				for (var inductionValue = primStart; inductionValue >= primEnd; inductionValue -= primStep) {
					enumerationFunction(inductionValue);
				}
			} else {
				throw new PrimInvalidOperandException("Step value must not be zero");
			}
			return receiver;
		}

		#endregion

		public override void publishCanonicalPrimitives() {

			publishPrimitive("class",					new FuncNs.Func<Object, Object>(_class_));
			publishPrimitive("isMemberOf:",					new FuncNs.Func<Object, Object, Object>(_isMemberOf_));
			publishPrimitive("isKindOf:",					new FuncNs.Func<Object, Object, Object>(_isKindOf_));

			publishPrimitive("=",						new FuncNs.Func<Object, Object, Object>(_hasSameValueAs_));
			publishPrimitive("hash",					new FuncNs.Func<Object, Object>(_hash_));

			publishPrimitive("compareTo:",					new FuncNs.Func<Object, Object, Object>(_compareTo_));
			publishPrimitive("<",						new FuncNs.Func<Object, Object, Object>(_lessThan_));

			publishPrimitive("asCharacter",					new FuncNs.Func<Object, Object>(_asCharacter_));
			publishPrimitive("asInteger",					new FuncNs.Func<Object, Object>(_asInteger_));
			publishPrimitive("asDouble",					new FuncNs.Func<Object, Object>(_asDouble_));
			publishPrimitive("asQuad",					new FuncNs.Func<Object, Object>(_asQuad_));

			publishPrimitive("sign",					new FuncNs.Func<Object, Object>(_sign_));
			publishPrimitive("isZero",					new FuncNs.Func<Object, Object>(_isZero_));
			publishPrimitive("positive",					new FuncNs.Func<Object, Object>(_positive_));
			publishPrimitive("negative",					new FuncNs.Func<Object, Object>(_negative_));
			publishPrimitive("abs",						new FuncNs.Func<Object, Object>(_abs_));
			publishPrimitive("negated",					new FuncNs.Func<Object, Object>(_negated_));

			publishPrimitive("+",						new FuncNs.Func<Object, Object, Object>(_sumWith_));
			publishPrimitive("-",						new FuncNs.Func<Object, Object, Object>(_differenceFrom_));
			publishPrimitive("*",						new FuncNs.Func<Object, Object, Object>(_productWith_));
			publishPrimitive("/",						new FuncNs.Func<Object, Object, Object>(_dividedBy_));
			publishPrimitive("reciprocal",					new FuncNs.Func<Object, Object>(_reciprocal_));

			publishPrimitive("roundTo:",					new FuncNs.Func<Object, Object, Object>(_roundTo_));
			publishPrimitive("truncateTo:",					new FuncNs.Func<Object, Object, Object>(_truncateTo_));
			publishPrimitive("quo:",					new FuncNs.Func<Object, Object, Object>(_dividedToIntegerBy_));
			publishPrimitive("//",						new FuncNs.Func<Object, Object, Object>(_dividedAndFlooredToIntegerBy_));
			publishPrimitive("rem:",					new FuncNs.Func<Object, Object, Object>(_remainderFromModulus_));
			publishPrimitive(@"\\",						new FuncNs.Func<Object, Object, Object>(_flooredRemainderFromModulus_));
			publishPrimitive("**",						new FuncNs.Func<Object, Object, Object>(_raisedTo_));
			publishPrimitive("log:",					new FuncNs.Func<Object, Object, Object>(_log_));
			publishPrimitive("ln",						new FuncNs.Func<Object, Object>(_ln_));
			publishPrimitive("to:do:",					new FuncNs.Func<Object, Object, Object, Object>(_toDo_));
			publishPrimitive("to:by:do:",					new FuncNs.Func<Object, Object, Object, Object, Object>(_toByDo_));
		}

	}

	public class DoublePrecisionPrimitives : PrimitiveDomain {

		protected override void bindToKernel() {
			domainClass = kernel.DoubleClass;
		}

		public override PrimitiveDomainType Type {
			get {return PrimitiveDomainType.DoublePrecision;}
		}

		#region Primitive Definitions

		public Object _class_(Object receiver) {
			return domainClass;
		}
		
		public Object _isMemberOf_(Object receiver, Object aBehavior) {
			return ReferenceEquals(domainClass, aBehavior);
		}
		
		public Object _isKindOf_(Object receiver, Object aBehavior) {
			ESBehavior esClass;
			try {
				esClass = (ESBehavior)aBehavior;
			} catch (InvalidCastException) {
				throw new PrimInvalidOperandException();
			}
			return domainClass.includesBehavior(esClass);
		}
		
		public Object _hasSameValueAs_(Object receiver, Object comparand) {
			double numComparand;
			try {
				numComparand = (double)comparand;
			} catch (InvalidCastException) {
				throw new PrimInvalidOperandException();
			}
			return (double)receiver == numComparand;
		}
		
		public Object _hash_(Object receiver) {
			return (long)(double)receiver;
		}
	
		public Object _compareTo_(Object receiver, Object comparand) {
			double numComparand;
			try {
				numComparand = (double)comparand;
			} catch {
				throw new PrimInvalidOperandException("Comparand must be a Double");
			}
			return (long)Math.Sign((double)receiver - numComparand);
		}

		public Object _lessThan_(Object receiver, Object comparand) {
			double numComparand;
			try {
				numComparand = (double)comparand;
			} catch {
				throw new PrimInvalidOperandException("Comparand must be a Double");
			}
			return ((double)receiver - numComparand) < 0;
		}

		public Object _sign_(Object receiver) {
			var value = (double)receiver;
			if (value > 0.0d) return 1L;
			if (value < 0.0d) return -1L;
			return 0L;
		}

		public Object _isZero_(Object receiver) {
			return (double)receiver == 0.0d;
		}

		public Object _positive_(Object receiver) {
			return (double)receiver >= 0.0d;
		}

		public Object _negative_(Object receiver) {
			return (double)receiver < 0.0d;
		}

		public Object _abs_(Object receiver) {
			double value = (double)receiver;
			return value >= 0.0d ? receiver : -value;
		}

		public Object _negated_(Object receiver) {
			return -(double)receiver;
		}
		
		public Object _sumWith_(Object receiver, Object operand) {
			double numOperand;
			try {
				numOperand = (double)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			return ((double)receiver) + numOperand;
		}
		
		public Object _differenceFrom_(Object receiver, Object operand) {
			double numOperand;
			try {
				numOperand = (double)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			return ((double)receiver) - numOperand;
		}
		
		public Object _productWith_(Object receiver, Object operand) {
			double numOperand;
			try {
				numOperand = (double)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			return ((double)receiver) * numOperand;
		}
		
		public Object _dividedBy_(Object receiver, Object operand) {
			double numOperand;
			try {
				numOperand = (double)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			return ((double)receiver) / numOperand; 
		}
		
		public Object _reciprocal_(Object receiver) {
			return 1.0d / (double)receiver; 
		}
		
		public Object _asCharacter_(Object receiver) {
			return (char)(uint)(double)receiver;
		}

		public Object _asInteger_(Object receiver) {
			return (long)(double)receiver;
		}

		public Object _asFloat_(Object receiver) {
			return (float)(double)receiver;
		}

		public Object _asQuad_(Object receiver) {
			return (decimal)(double)receiver;
		}

		public Object _ceiling_(Object receiver) {
			return Math.Ceiling((double)receiver);
		}

		public Object _floor_(Object receiver) {
			return Math.Floor((double)receiver);
		}

		public Object _rounded_(Object receiver) {
			return Math.Round((double)receiver);
		}

		public Object _truncated_(Object receiver) {
			return (long)(double)receiver;
		}

		public Object _roundTo_(Object receiver, Object operand) {
			// Smalltalk: 53 roundTo: 5 => 55; 1263.48 roundTo: 0.25 => 1263.50.
			// C-Lang: 53.roundTo(5) => 55; 1263.48.roundTo(0.25) => 1263.50.
			double modulus;
			try {
				modulus = (double)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			double self = (double)receiver;
			return (Math.Round(self / modulus) * modulus);
		}
		
		public Object _truncateTo_(Object receiver, Object modulus) {
			// Smalltalk: 53 truncateTo: 5 => 50; 1263.48 truncateTo: 0.25 => 1263.25.
			// C-Lang: 53.truncateTo(5) => 50; 1263.48.truncateTo(0.25) => 1263.25.
			double numModulus;
			try {
				numModulus = (double)modulus;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			double self = (double)receiver;
			return ((long)(self / numModulus)) * numModulus;
		}
		
		public Object _dividedToIntegerBy_(Object receiver, Object operand) {
			double numOperand;
			try {
				numOperand = (double)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			return (long)((double)receiver / numOperand); 
		}
		
		public Object _dividedAndFlooredToIntegerBy_(Object receiver, Object operand) {
			double numOperand;
			try {
				numOperand = (double)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			return (long)Math.Floor((double)receiver / numOperand); 
		}
		
		public Object _remainderFromModulus_(Object receiver, Object operand) {
			// Implements standard keyword message "rem: operand" 
			// Smalltalk: ^receiver - ((receiver quo: operand) * operand)
			// C-Lang: return receiver - ((long)(receiver / operand) * operand); -- Or just "receiver % operand" :-)
			double numOperand;
			try {
				numOperand = (double)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			return (double)receiver % numOperand;
		}
		
		public Object _flooredRemainderFromModulus_(Object receiver, Object operand) {
			// Implements standard binary message "\\ operand" 
			// Smalltalk: ^receiver - (receiver // operand * operand)
			// C-Lang: return receiver - ((long)Math.Floor(receiver / (double)operand) * operand);
			double numOperand;
			try {
				numOperand = (double)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			double self = (double)receiver;
			return self - ((long)Math.Floor(self / numOperand) * numOperand);
		}
		
		public Object _raisedTo_(Object receiver, Object operand) {
			double numOperand;
			try {
				numOperand = (double)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			return Math.Pow((double)receiver, numOperand);
		}
		
		public Object _log_(Object receiver, Object operand) {
			double numOperand;
			try {
				numOperand = (double)operand;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			return Math.Log((double)receiver, numOperand);
		}
		
		public Object _ln_(Object receiver) {
			return Math.Log((double)receiver, Math.E);
		}

		public Object _toDo_(Object receiver, Object end, Object enumerator) {
			double primEnd;
			try {
				primEnd = (double)end;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			var enumerationFunction = ESObject.asFunctor1(enumerator);
			var primStart = (double)receiver;
			for (var inductionValue = primStart; inductionValue <= primEnd; inductionValue++) {
				enumerationFunction(inductionValue);
			}
			return receiver;
		}

		public Object _toByDo_(Object receiver, Object end, Object step, Object enumerator) {
			double primEnd;
			try {
				primEnd = (double)end;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			double primStep;
			try {
				primStep = (double)step;
			} catch {
				throw new PrimInvalidOperandException("Operand must be a Double");
			}
			var enumerationFunction = ESObject.asFunctor1(enumerator);
			var primStart = (double)receiver;
			if (primStep > 0) {
				for (var inductionValue = primStart; inductionValue <= primEnd; inductionValue += primStep) {
					enumerationFunction(inductionValue);
				}
			} else if (primStep < 0) {
				for (var inductionValue = primStart; inductionValue >= primEnd; inductionValue -= primStep) {
					enumerationFunction(inductionValue);
				}
			} else {
				throw new PrimInvalidOperandException("Step value must not be zero");
			}
			return receiver;
		}

		#endregion

		public override void publishCanonicalPrimitives() {

			publishPrimitive("class",					new FuncNs.Func<Object, Object>(_class_));
			publishPrimitive("isMemberOf:",					new FuncNs.Func<Object, Object, Object>(_isMemberOf_));
			publishPrimitive("isKindOf:",					new FuncNs.Func<Object, Object, Object>(_isKindOf_));

			publishPrimitive("=",						new FuncNs.Func<Object, Object, Object>(_hasSameValueAs_));
			publishPrimitive("hash",					new FuncNs.Func<Object, Object>(_hash_));

			publishPrimitive("compareTo:",					new FuncNs.Func<Object, Object, Object>(_compareTo_));
			publishPrimitive("<",						new FuncNs.Func<Object, Object, Object>(_lessThan_));

			publishPrimitive("asCharacter",					new FuncNs.Func<Object, Object>(_asCharacter_));
			publishPrimitive("asInteger",					new FuncNs.Func<Object, Object>(_asInteger_));
			publishPrimitive("asFloat",					new FuncNs.Func<Object, Object>(_asFloat_));
			publishPrimitive("asQuad",					new FuncNs.Func<Object, Object>(_asQuad_));

			publishPrimitive("sign",					new FuncNs.Func<Object, Object>(_sign_));
			publishPrimitive("isZero",					new FuncNs.Func<Object, Object>(_isZero_));
			publishPrimitive("positive",					new FuncNs.Func<Object, Object>(_positive_));
			publishPrimitive("negative",					new FuncNs.Func<Object, Object>(_negative_));
			publishPrimitive("abs",						new FuncNs.Func<Object, Object>(_abs_));
			publishPrimitive("negated",					new FuncNs.Func<Object, Object>(_negated_));

			publishPrimitive("+",						new FuncNs.Func<Object, Object, Object>(_sumWith_));
			publishPrimitive("-",						new FuncNs.Func<Object, Object, Object>(_differenceFrom_));
			publishPrimitive("*",						new FuncNs.Func<Object, Object, Object>(_productWith_));
			publishPrimitive("/",						new FuncNs.Func<Object, Object, Object>(_dividedBy_));
			publishPrimitive("reciprocal",					new FuncNs.Func<Object, Object>(_reciprocal_));

			publishPrimitive("roundTo:",					new FuncNs.Func<Object, Object, Object>(_roundTo_));
			publishPrimitive("truncateTo:",					new FuncNs.Func<Object, Object, Object>(_truncateTo_));
			publishPrimitive("quo:",					new FuncNs.Func<Object, Object, Object>(_dividedToIntegerBy_));
			publishPrimitive("//",						new FuncNs.Func<Object, Object, Object>(_dividedAndFlooredToIntegerBy_));
			publishPrimitive("rem:",					new FuncNs.Func<Object, Object, Object>(_remainderFromModulus_));
			publishPrimitive(@"\\",						new FuncNs.Func<Object, Object, Object>(_flooredRemainderFromModulus_));
			publishPrimitive("**",						new FuncNs.Func<Object, Object, Object>(_raisedTo_));
			publishPrimitive("log:",					new FuncNs.Func<Object, Object, Object>(_log_));
			publishPrimitive("ln",						new FuncNs.Func<Object, Object>(_ln_));
			publishPrimitive("to:do:",					new FuncNs.Func<Object, Object, Object, Object>(_toDo_));
			publishPrimitive("to:by:do:",					new FuncNs.Func<Object, Object, Object, Object, Object>(_toByDo_));
		}

	}

	public class QuadPrecisionPrimitives : PrimitiveDomain {

		protected override void bindToKernel() {
			domainClass = kernel.QuadClass;
		}

		public override PrimitiveDomainType Type {
			get {return PrimitiveDomainType.QuadPrecision;}
		}

		public override void publishCanonicalPrimitives() {
		}

	}

	#endregion

}
