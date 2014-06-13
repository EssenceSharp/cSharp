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
using System.Dynamic;
using System.Reflection;
using System.Runtime.CompilerServices;
using Microsoft.Scripting.Generation;
#if CLR2
using Microsoft.Scripting.Ast;
#else
using System.Linq.Expressions;
using FuncNs = System;
#endif
using EssenceSharp.CompilationServices;
using EssenceSharp.Exceptions.System.PrimitiveFailures;
#endregion

namespace EssenceSharp.Runtime.Binding {
	
	public enum MessageReceiverKind {
		General,
		Self,
		Super,
		ThisContext
	}

	public abstract class BinderRegistry {

		protected DynamicBindingGuru			dynamicBindingGuru	= null;
		protected ESKernel				kernel			= null;

		protected BinderRegistry(DynamicBindingGuru dynamicBindingGuru) {
			this.dynamicBindingGuru = dynamicBindingGuru;
			kernel = dynamicBindingGuru.Kernel;
		}

		public DynamicBindingGuru DynamicBindingGuru {
			get {return dynamicBindingGuru;}
		}

		public ESKernel Kernel {
			get {return kernel;}
		}

	}

	public static class TypeGuru {

		public static readonly Type[]			emptyTypeArray		= new Type[0];

		#region Type Constants

		public static readonly Type			voidType		= typeof(void);
		public static readonly Type			objectType		= typeof(Object);
		public static readonly Type			typeType		= typeof(Type);
		public static readonly Type			nullableType		= typeof(Nullable<>);
		public static readonly Type			nullType		= typeof(void);
		public static readonly Type			enumType		= typeof(Enum);
		public static readonly Type			boolType		= typeof(bool);
		public static readonly Type			charType		= typeof(char);
		public static readonly Type			stringType		= typeof(String);
		public static readonly Type			convertType		= typeof(Convert);
		public static readonly Type			runtimeHelpersType	= typeof(RuntimeHelpers);
		public static readonly Type			mathType		= typeof(Math);

		public static readonly Type			byteType		= typeof(byte);
		public static readonly Type			sbyteType		= typeof(sbyte);
		public static readonly Type			ushortType		= typeof(ushort);
		public static readonly Type			shortType		= typeof(short);
		public static readonly Type			uintType		= typeof(uint);
		public static readonly Type			intType			= typeof(int);
		public static readonly Type			ulongType		= typeof(ulong);
		public static readonly Type			longType		= typeof(long);
		public static readonly Type			floatType		= typeof(float);
		public static readonly Type			doubleType		= typeof(double);
		public static readonly Type			decimalType		= typeof(decimal);

		public static readonly Type			objectArrayType		= typeof(Object[]);
		public static readonly Type			sbyteArrayType		= typeof(sbyte[]);
		public static readonly Type			byteArrayType		= typeof(byte[]);
		public static readonly Type			charArrayType		= typeof(char[]);
		public static readonly Type			ushortArrayType		= typeof(ushort[]);
		public static readonly Type			shortArrayType		= typeof(short[]);
		public static readonly Type			uintArrayType		= typeof(uint[]);
		public static readonly Type			intArrayType		= typeof(int[]);
		public static readonly Type			ulongArrayType		= typeof(ulong[]);
		public static readonly Type			longArrayType		= typeof(long[]);
		public static readonly Type			floatArrayType		= typeof(float[]);
		public static readonly Type			doubleArrayType		= typeof(double[]);
		public static readonly Type			decimalArrayType	= typeof(decimal[]);
		public static readonly Type			stringArrayType		= typeof(String[]);

		public static readonly Type			iListType					= typeof(IList<>);
		public static readonly Type			listType					= typeof(List<>);
		public static readonly Type			hashSetType					= typeof(HashSet<>);
		public static readonly Type			keyValuePairType				= typeof(KeyValuePair<,>);
		public static readonly Type			keyValuePairForObjectKeyObjectValueType		= typeof(KeyValuePair<Object, Object>);
		public static readonly Type			keyValuePairForStringKeyBindingHandleValueType	= typeof(KeyValuePair<String, BindingHandle>);
		public static readonly Type			iDictionaryType					= typeof(IDictionary<,>);
		public static readonly Type			iDictionaryObjectKeyObjectValueType		= typeof(IDictionary<Object,Object>);
		public static readonly Type			iDictionaryStringKeyBindingHandleValueType	= typeof(IDictionary<String,BindingHandle>);
		public static readonly Type			dictionaryType					= typeof(Dictionary<,>);
		public static readonly Type			dictionaryObjectKeyObjectValueType		= typeof(Dictionary<Object,Object>);
		public static readonly Type			dictionaryStringKeyBindingHandleValueType	= typeof(Dictionary<String,BindingHandle>);

		public static readonly Type			exceptionType			= typeof(Exception);
		public static readonly Type			invalidCastExceptionType	= typeof(InvalidCastException);

		public static readonly Type			delegateType			= typeof(Delegate);

		public static readonly Type			esKernelType			= typeof(ESKernel);
		public static readonly Type			esObjectType			= typeof(ESObject);
		public static readonly Type			esNamedSlotsObjectType		= typeof(ESNamedSlotsObject);
		public static readonly Type			esNamespaceType			= typeof(ESNamespace);
		public static readonly Type			esBehaviorType			= typeof(ESBehavior);
		public static readonly Type			esClassType			= typeof(ESClass);
		public static readonly Type			esMetaclassType			= typeof(ESMetaclass);
		public static readonly Type			esBlockType			= typeof(ESBlock);
		public static readonly Type			esMethodType			= typeof(ESMethod);
		public static readonly Type			esAssociationType		= typeof(ESAssociation);
		public static readonly Type			esBindingReferenceType		= typeof(ESBindingReference);
		public static readonly Type			esMessageType			= typeof(ESMessage);

		public static readonly Type			esIdentityDictionaryType	= typeof(ESIdentityDictionary);
		public static readonly Type			esDictionaryType		= typeof(ESDictionary);
		public static readonly Type			esObjectArrayType		= typeof(ESArray);
		public static readonly Type			esByteArrayType			= typeof(ESByteArray);
		public static readonly Type			esStringType			= typeof(ESString);
		public static readonly Type			esSymbolType			= typeof(ESSymbol);
		public static readonly Type			esHalfWordArrayType		= typeof(ESHalfWordArray);
		public static readonly Type			esWordArrayType			= typeof(ESWordArray);
		public static readonly Type			esLongWordArrayType		= typeof(ESLongWordArray);
		public static readonly Type			esFloatArrayType		= typeof(ESFloatArray);
		public static readonly Type			esDoubleArrayType		= typeof(ESDoubleArray);
		public static readonly Type			esQuadArrayType			= typeof(ESQuadArray);
		public static readonly Type			esPathnameType			= typeof(ESPathname);

		public static readonly Type			symbolRegistryType		= typeof(SymbolRegistry);
		public static readonly Type			esAssociationArrayType		= typeof(ESAssociation[]);
		public static readonly Type			bindingHandleType		= typeof(BindingHandle);
		public static readonly Type			directBindingHandleType		= typeof(DirectBindingHandle);
		public static readonly Type			inlineOperationType		= typeof(InlineOperation);
		public static readonly Type			mustBeBooleanExceptionType	= typeof(MustBeBoolean);
		public static readonly Type			nonLocalReturnExceptionType	= typeof(ESNonLocalReturn);
		public static readonly Type			primitiveFailExceptionType	= typeof(PrimitiveFailException);
		public static readonly Type			esMethodDeclarationNodeType	= typeof(MethodDeclarationNode);

		#endregion

		private static readonly HashSet<Type>		essenceSharpTypes	= new HashSet<Type>();
		private static readonly Dictionary<Type, int>	numericTypeGenerality	= new Dictionary<Type, int>();

		static TypeGuru() {
			essenceSharpTypes.Add(esObjectType);
			essenceSharpTypes.Add(esNamedSlotsObjectType);
			essenceSharpTypes.Add(esNamespaceType);
			essenceSharpTypes.Add(esBehaviorType);
			essenceSharpTypes.Add(esMetaclassType);
			essenceSharpTypes.Add(esAssociationType);
			essenceSharpTypes.Add(esBindingReferenceType);
			essenceSharpTypes.Add(esIdentityDictionaryType);
			essenceSharpTypes.Add(esDictionaryType);
			essenceSharpTypes.Add(esObjectArrayType);
			essenceSharpTypes.Add(esByteArrayType);
			essenceSharpTypes.Add(esStringType);
			essenceSharpTypes.Add(esSymbolType);
			essenceSharpTypes.Add(esHalfWordArrayType);
			essenceSharpTypes.Add(esWordArrayType);
			essenceSharpTypes.Add(esLongWordArrayType);
			essenceSharpTypes.Add(esFloatArrayType);
			essenceSharpTypes.Add(esDoubleArrayType);
			essenceSharpTypes.Add(esQuadArrayType);
			essenceSharpTypes.Add(esPathnameType);

			numericTypeGenerality[byteType] = 0;
			numericTypeGenerality[sbyteType] = 1;
			numericTypeGenerality[ushortType] = 2;
			numericTypeGenerality[shortType] = 3;
			numericTypeGenerality[uintType] = 4;
			numericTypeGenerality[intType] = 5;
			numericTypeGenerality[ulongType] = 6;
			numericTypeGenerality[longType] = 7;
			numericTypeGenerality[floatType] = 8;
			numericTypeGenerality[decimalType] = 9;
			numericTypeGenerality[doubleType] = 10;
		}

		public static Type[] newHomogenousTypeArray(Type type, long size) {
			var types = new Type[size];
			for (var i = 0; i < size; i++) types[i] = type;
			return types;
		}

		public static bool isEssenceSharpType(this Type aType) {
			return esObjectType.IsAssignableFrom(aType);
		}

		public static bool implementsInterface(this Type type, Type interfaceType) {
			Type[] interfaces = type.GetInterfaces();
			if (interfaceType.ContainsGenericParameters) {
				var genericDefinition = interfaceType.GetGenericTypeDefinition();
				var iGenericArguments = genericDefinition.GetGenericArguments();
				for (int i = 0; i < interfaces.Length; i++) {
					var t = interfaces[i];
					var tGenericArguments = t.GetGenericArguments();
					if (iGenericArguments.Length == tGenericArguments.Length) {
						var closedType = genericDefinition.MakeGenericType(tGenericArguments);
						if (closedType.IsAssignableFrom(t)) return true;
					}
				}
			} else {
				for (int i = 0; i < interfaces.Length; i++) {
					var t = interfaces[i];
					if (interfaceType.IsAssignableFrom(t)) return true;
				}
			}
			return false;
		}

		public static int numericGenerality(this Type aType) {
			if (aType.IsValueType) {
				int generality;
				if (numericTypeGenerality.TryGetValue(aType, out generality)) return generality;
			} else if (aType.IsGenericType && aType.GetGenericTypeDefinition() == nullableType) {
				return Nullable.GetUnderlyingType(aType).numericGenerality();
			}
			return -1;
		}

		public static Type typeWithHighestNumericGenerality(this Type leftType, Type rightType) {
			int leftGenerality;
			if (!leftType.IsValueType) {
				if (!leftType.IsGenericType) return null;
				if (!(leftType.GetGenericTypeDefinition() == nullableType)) return null;
				leftType = Nullable.GetUnderlyingType(leftType);
			}
			if (!numericTypeGenerality.TryGetValue(leftType, out leftGenerality)) return null;
			int rightGenerality;
			if (!rightType.IsValueType) {
				if (!rightType.IsGenericType) return null;
				if (!(rightType.GetGenericTypeDefinition() == nullableType)) return null;
				rightType = Nullable.GetUnderlyingType(rightType);
			}
			if (!numericTypeGenerality.TryGetValue(rightType, out rightGenerality)) return null;
			return leftGenerality >= rightGenerality ? leftType : rightType;
		}

		public static bool isNumeric(this Type aType) {
			if (aType == null) return false;
			if (aType.IsEnum) return false;
			switch (Type.GetTypeCode(aType)) {
				case TypeCode.Byte:
				case TypeCode.SByte:
				case TypeCode.UInt16:
				case TypeCode.Int16:
				case TypeCode.UInt32:
				case TypeCode.Int32:
				case TypeCode.UInt64:
				case TypeCode.Int64:
				case TypeCode.Single:
				case TypeCode.Double:
				case TypeCode.Decimal:
					return true;
				case TypeCode.Object:
					if (aType.IsGenericType && aType.GetGenericTypeDefinition() == nullableType) {
						return Nullable.GetUnderlyingType(aType).isNumeric();
					}
					return false;
			}
			return false;
		}

		public static bool isSmallInteger(this Type aType) {
			if (aType == null) return false;
			if (aType.IsEnum) return false;
			switch (Type.GetTypeCode(aType)) {
				case TypeCode.Byte:
				case TypeCode.SByte:
				case TypeCode.UInt16:
				case TypeCode.Int16:
				case TypeCode.UInt32:
				case TypeCode.Int32:
				case TypeCode.Int64:
					return true;
				case TypeCode.Object:
					if (aType.IsGenericType && aType.GetGenericTypeDefinition() == nullableType) {
						return Nullable.GetUnderlyingType(aType).isSmallInteger();
					}
					return false;
			}
			return false;
		}

		public static bool isInteger(this Type aType) {
			if (aType == null) return false;
			if (aType.IsEnum) return false;
			switch (Type.GetTypeCode(aType)) {
				case TypeCode.Byte:
				case TypeCode.SByte:
				case TypeCode.UInt16:
				case TypeCode.Int16:
				case TypeCode.UInt32:
				case TypeCode.Int32:
				case TypeCode.UInt64:
				case TypeCode.Int64:
					return true;
				case TypeCode.Object:
					if (aType.IsGenericType && aType.GetGenericTypeDefinition() == nullableType) {
						return Nullable.GetUnderlyingType(aType).isInteger();
					}
					return false;
			}
			return false;
		}
		 
		public static bool isRational(this Type aType) {
			if (aType == null) return false;
			if (aType.IsEnum) return false;
			switch (Type.GetTypeCode(aType)) {
				case TypeCode.Single:
				case TypeCode.Double:
				case TypeCode.Decimal:
					return true;
				case TypeCode.Object:
					if (aType.IsGenericType && aType.GetGenericTypeDefinition() == nullableType) {
						return Nullable.GetUnderlyingType(aType).isRational();
					}
					return false;
			}
			return false;
		}

		public static bool isDecimal(this Type aType) {
			if (aType == null) return false;
			if (aType == decimalType) return true;
			if (aType.IsEnum) return false;
			if (Type.GetTypeCode(aType) == TypeCode.Object) {
				if (aType.IsGenericType && aType.GetGenericTypeDefinition() == nullableType) {
					return Nullable.GetUnderlyingType(aType).isDecimal();
				}
			}
			return false;
		}

		public static bool isBoolean(this Type aType) {
			if (aType == null) return false;
			if (aType == boolType) return true;
			if (Type.GetTypeCode(aType) == TypeCode.Object) {
				if (aType.IsGenericType && aType.GetGenericTypeDefinition() == nullableType) {
					return Nullable.GetUnderlyingType(aType).isBoolean();
				}
			}
			return false;
		}

		public static bool isCharacter(this Type aType) {
			if (aType == null) return false;
			if (aType == charType) return true;
			if (aType.IsEnum) return false;
			if (Type.GetTypeCode(aType) == TypeCode.Object) {
				if (aType.IsGenericType && aType.GetGenericTypeDefinition() == nullableType) {
					return Nullable.GetUnderlyingType(aType).isCharacter();
				}
			}
			return false;
		}

		public static bool isNullable(this Type aType) {
			if (aType == null) return false;
			return (Type.GetTypeCode(aType) == TypeCode.Object && aType.IsGenericType && aType.GetGenericTypeDefinition() == nullableType);
		}

		public static bool nullIsAssignable(this Type aType) {
			if (aType == null) return false;
			return !aType.IsValueType;
		}

		public static Type visibleProxy(this Type originalType) {
			if (originalType == null) return null;
			var type = originalType;
			while (!type.IsVisible) {
				var super = type.BaseType;
				if (super == null) return originalType;
				type = super;
			}
			return type;
		}

		public static Type[] parameterTypesFromDelegateType(Type delegateType) {

			MethodInfo invoke = delegateType.GetMethod("Invoke");

			ParameterInfo[] parameters = invoke.GetParameters();
			Type[] typeParameters = new Type[parameters.Length];
			for (int i = 0; i < parameters.Length; i++) {
				typeParameters[i] = parameters[i].ParameterType;
			}
			return typeParameters;
		}

		public static List<MethodInfo> indexerMethodsOf(Type aType, BindingFlags bindingFlags) {
			var indexerProperties = aType.GetProperties(bindingFlags);
			var indexerMethods = new List<MethodInfo>();
			foreach (var p in indexerProperties) {
				var indexParameters = p.GetIndexParameters();
				if (indexParameters.Length < 1) continue;
				indexerMethods.Add(p.GetGetMethod());
			}
			return indexerMethods;
		}

	}

	public static class ExpressionTreeExtensionMethods {

		public static Expression asRationalNumberExpression(this Expression aNumber) {
			if (aNumber.Type.isRational()) {
				return aNumber;
			} else {
				return aNumber.withType(TypeGuru.doubleType);
			}
		}

		public static BindingRestrictions asBindingRestriction(this Expression receiver) {
			return BindingRestrictions.GetExpressionRestriction(receiver);
		}

		public static DynamicMetaObject asDynamicMetaObject(this Expression receiver) {
			return new DynamicMetaObject(receiver, BindingRestrictions.Empty);
		}

		public static DynamicMetaObject asDynamicMetaObject(this Expression receiver, BindingRestrictions restrictions) {
			return new DynamicMetaObject(receiver, restrictions);
		}

		public static DynamicMetaObject asDynamicMetaObject(this Expression receiver, BindingRestrictions restrictions, Object value) {
			return new DynamicMetaObject(receiver, restrictions, value);
		}

	}

	public static class ExpressionTreeGuru {

		#region Constants

		public static readonly ConstantExpression				nilConstant				= Expression.Constant(null);
		public static readonly ConstantExpression				falseConstant				= Expression.Constant(false);
		public static readonly ConstantExpression				trueConstant				= Expression.Constant(true);
		public static readonly ConstantExpression				zeroConstant				= Expression.Constant(0L);
		public static readonly ConstantExpression				int32ZeroConstant			= Expression.Constant(0);
		public static readonly ConstantExpression				oneConstant				= Expression.Constant(1L);
		public static readonly ConstantExpression				int32OneConstant			= Expression.Constant(1);
		public static readonly ConstantExpression				oneHalfConstant				= Expression.Constant(0.5);
		public static readonly ConstantExpression				eulersConstant				= Expression.Constant(Math.E);

		public static readonly Object[]						emptyObjectArray			= new Object[0];
		public static readonly Expression[]					emptyExpressionArray			= new Expression[0];
		public static readonly CallInfo						zeroArgCallInfo				= new CallInfo(0);
		public static readonly CallInfo						oneArgCallInfo				= new CallInfo(1);

		public static readonly Type[]						messageConstructorSignature		= {TypeGuru.esBehaviorType, TypeGuru.esSymbolType, TypeGuru.objectArrayType};
		public static readonly ConstructorInfo					messageConstructorInfo			= TypeGuru.esMessageType.GetConstructor(messageConstructorSignature);
		public static readonly Type[]						compiledBlockConstructorSignature	= {TypeGuru.esBehaviorType, TypeGuru.delegateType, TypeGuru.longType};
		public static readonly ConstructorInfo					compiledBlockConstructorInfo		= TypeGuru.esBlockType.GetConstructor(compiledBlockConstructorSignature);
		public static readonly Type[]						methodConstructorSignature		= {TypeGuru.esBehaviorType, TypeGuru.esSymbolType, TypeGuru.delegateType};
		public static readonly ConstructorInfo					methodConstructorInfo			= TypeGuru.esMethodType.GetConstructor(methodConstructorSignature);
		public static readonly Type[]						inlineMethodConstructorSignature	= {TypeGuru.esBehaviorType, TypeGuru.esSymbolType, TypeGuru.inlineOperationType, TypeGuru.delegateType};
		public static readonly ConstructorInfo					inlineMethodConstructorInfo		= TypeGuru.esMethodType.GetConstructor(inlineMethodConstructorSignature);
		public static readonly Type[]						compiledMethodConstructorSignature	= {TypeGuru.esBehaviorType, TypeGuru.esMethodDeclarationNodeType};
		public static readonly ConstructorInfo					compiledMethodConstructorInfo		= TypeGuru.esMethodType.GetConstructor(compiledMethodConstructorSignature);

		#endregion

		#region Utilities

		public static CallInfo callInfoForArgCount(long argCount) {
			switch (argCount) {
				case 0:
					return zeroArgCallInfo;
				case 1:
					return oneArgCallInfo;
				default:
					return new CallInfo((int)argCount);
			}
		}

		#endregion

		#region BindingRestriction expressions

		public static  Expression expressionToGetClassOfESObject(Expression self) {
			if (!TypeGuru.esObjectType.IsAssignableFrom(self.Type)) self = self.withType(TypeGuru.esObjectType);
			return Expression.Field(self, TypeGuru.esObjectType, "_class");
			// return Expression.Property(self, TypeGuru.esObjectType, "Class");
		}

		public static  Expression expressionToGetVersionIdOfESClass(Expression esClass) {
			return Expression.Field(esClass, TypeGuru.esBehaviorType, "versionId");
		}

		public static  Expression expressionToGetVersionIdOfESClass(ESBehavior esClass) {
			return expressionToGetVersionIdOfESClass(Expression.Constant(esClass));
		}

		public static  Expression expressionToTestThatESObjectHasSameClassVersion(Expression self, ConstantExpression expectedClassVersionId) {
			var getVersionIdOfESClass = expressionToGetVersionIdOfESClass(expressionToGetClassOfESObject(self));
			return Expression.Equal(getVersionIdOfESClass, expectedClassVersionId);
		}

		public static  Expression expressionToTestThatESObjectHasSameClassVersion(Expression self, ESBehavior esClass) {
			var getVersionIdOfESClass = expressionToGetVersionIdOfESClass(expressionToGetClassOfESObject(self));
			return Expression.Equal(getVersionIdOfESClass, Expression.Constant(esClass.VersionId));
		}

		public static  Expression expressionToTestThatClassHasSameClassVersion(ESBehavior esClass) {
			var getVersionIdOfESClass = expressionToGetVersionIdOfESClass(esClass);
			return Expression.Equal(getVersionIdOfESClass, Expression.Constant(esClass.VersionId));
		}

		public static  Expression expressionToTestThatNilHasSameClassVersion(Expression self, ESClass undefinedObjectClass) {
			var valueIsNull = Expression.ReferenceEqual(self, ExpressionTreeGuru.nilConstant);
			var classHasSameVersionId = expressionToTestThatClassHasSameClassVersion(undefinedObjectClass);
			return Expression.AndAlso(valueIsNull, classHasSameVersionId);
		}

		#endregion

		#region Type Conversion Expressions 

		public static Expression withType(this Expression expression, Type targetType) {
			var expressionType = expression.Type; 
			if (expressionType == targetType) return expression;
			return Expression.Convert(expression, targetType);
		}

		public static Expression withType(this Expression expression, Type targetType, MethodInfo conversionOperator) {
			var expressionType = expression.Type; 
			if (expressionType == targetType) return expression;
			return Expression.Convert(expression, targetType, conversionOperator);
		}

		public static Expression promotedToLong(this Expression expression) {
			var expressionType = expression.Type; 
			if (!expressionType.isSmallInteger()) return expression;
			return expression.withType(TypeGuru.longType);
		}

		public static Expression expressionToInvoke_ToString(Expression value) {
			return Expression.Call(
					value,
					TypeGuru.objectType.GetMethod("ToString", TypeGuru.emptyTypeArray));
		}

		#region ES Indexed Slot Objects to Host System Arrays 

		public static Expression expressionToCreateESStringFromString(ESKernel kernel, Expression stringExpression) {
			return Expression.Call(
					Expression.Constant(kernel),
					TypeGuru.esKernelType.GetMethod("newString", new Type[]{TypeGuru.stringType}),
					stringExpression);
		}
		
		public static Expression expressionToCreateESSymbolFromString(SymbolRegistry symbolRegistry, Expression stringExpression) {
			return Expression.Call(
					Expression.Constant(symbolRegistry),
					TypeGuru.symbolRegistryType.GetMethod("symbolFor", new Type[]{TypeGuru.stringType}),
					stringExpression);
		}
		
		public static Expression expressionToCreateESSymbolFromCharArray(SymbolRegistry symbolRegistry, Expression charArrayExpression) {
			return Expression.Call(
					Expression.Constant(symbolRegistry),
					TypeGuru.symbolRegistryType.GetMethod("symbolFor", new Type[]{TypeGuru.charArrayType}),
					charArrayExpression);
		}
		
		public static Expression expressionToCreateESSymbolFromESString(SymbolRegistry symbolRegistry, Expression esStringExpression) {
			return expressionToCreateESSymbolFromCharArray(symbolRegistry, expressionToConvertESStringToCharArray(esStringExpression));
		}
		
		public static Expression expressionToCreateESSymbolFromEnumerationContant(SymbolRegistry symbolRegistry, Expression enumerationConstantExpression) {
			return Expression.Call(
					Expression.Constant(symbolRegistry),
					TypeGuru.symbolRegistryType.GetMethod("symbolFor", new Type[]{TypeGuru.stringType}),
					expressionToInvoke_ToString(enumerationConstantExpression));
		}

		public static Expression expressionToCreateESStringFromNonESObject(ESKernel kernel, Expression self) {
			return Expression.Call(
					Expression.Constant(kernel),
					TypeGuru.esKernelType.GetMethod("esStringFromNonESObject", new Type[]{TypeGuru.objectType}),
					self);
		}

		public static Expression expressionToCreateESSymbolFromNonESObject(ESKernel kernel, Expression self) {
			return Expression.Call(
					Expression.Constant(kernel),
					TypeGuru.esKernelType.GetMethod("esSymbolFromNonESObject", new Type[]{TypeGuru.stringType}),
					self);
		}

		public static Expression expressionToConvertESArrayToObjectArray(Expression esIndexedSlotsObjectExpression) {
			return Expression.Field(esIndexedSlotsObjectExpression, TypeGuru.esObjectArrayType, "slots");
		}

		public static Expression expressionToConvertESByteArrayToByteArray(Expression esIndexedSlotsObjectExpression) {
			return Expression.Field(esIndexedSlotsObjectExpression, TypeGuru.esByteArrayType, "slots");
		}

		public static Expression expressionToConvertESStringToCharArray(Expression esIndexedSlotsObjectExpression) {
			return Expression.Field(esIndexedSlotsObjectExpression, TypeGuru.esStringType, "slots");
		}

		public static Expression expressionToConvertESStringToString(Expression esIndexedSlotsObjectExpression) {
			return Expression.New(
				TypeGuru.stringType.GetConstructor(new Type[]{TypeGuru.charArrayType}),
				Expression.Field(esIndexedSlotsObjectExpression, TypeGuru.esStringType, "slots"));
		}

		public static Expression expressionToConvertESSymbolToCharArray(Expression esIndexedSlotsObjectExpression) {
			return Expression.Field(esIndexedSlotsObjectExpression, TypeGuru.esSymbolType, "slots");
		}

		public static Expression expressionToConvertESSymbolToString(Expression esIndexedSlotsObjectExpression) {
			return Expression.Field(esIndexedSlotsObjectExpression, TypeGuru.esSymbolType, "stringValue");
		}

		public static Expression expressionToConvertSymbolToEnumerationConstant(Expression symbolExpression, Type enumerationType) {
			return Expression.Convert(
					Expression.Call(
						null,
						TypeGuru.enumType.GetMethod(
							"Parse", 
							ESBehavior.staticMethodInvokeBindingFlags,
							Type.DefaultBinder,
							new Type[]{TypeGuru.typeType, TypeGuru.stringType},
							null),
						Expression.Constant(enumerationType),
						expressionToConvertESSymbolToString(symbolExpression)),
					enumerationType);
		}

		public static Expression expressionToConvertESHalfWordArrayToHalfWordArray(Expression esIndexedSlotsObjectExpression) {
			return Expression.Field(esIndexedSlotsObjectExpression, TypeGuru.esHalfWordArrayType, "slots");
		}

		public static Expression expressionToConvertESWordArrayToWordArray(Expression esIndexedSlotsObjectExpression) {
			return Expression.Field(esIndexedSlotsObjectExpression, TypeGuru.esWordArrayType, "slots");
		}

		public static Expression expressionToConvertESLongWordArrayToLongWordArray(Expression esIndexedSlotsObjectExpression) {
			return Expression.Field(esIndexedSlotsObjectExpression, TypeGuru.esLongWordArrayType, "slots");
		}

		public static Expression expressionToConvertESFloatArrayToFloatArray(Expression esIndexedSlotsObjectExpression) {
			return Expression.Field(esIndexedSlotsObjectExpression, TypeGuru.esFloatArrayType, "slots");
		}

		public static Expression expressionToConvertESDoubleArrayToDoubleArray(Expression esIndexedSlotsObjectExpression) {
			return Expression.Field(esIndexedSlotsObjectExpression, TypeGuru.esDoubleArrayType, "slots");
		}

		public static Expression expressionToConvertESQuadArrayToDecimalArray(Expression esIndexedSlotsObjectExpression) {
			return Expression.Field(esIndexedSlotsObjectExpression, TypeGuru.esQuadArrayType, "slots");
		}

		public static Expression expressionToConvertESPathnameToStringArray(Expression esIndexedSlotsObjectExpression) {
			return Expression.Field(esIndexedSlotsObjectExpression, TypeGuru.esPathnameType, "slots");
		}

		#endregion

		#region ESCompiledCode to Functors

  		public static Expression expressionToConvertESBlockToFunctor(Expression sourceExpression, long functorArity) {
			var asBlock = Expression.TypeAs(sourceExpression, TypeGuru.esBlockType);
			return
				Expression.Condition(
					Expression.ReferenceEqual(asBlock, ExpressionTreeGuru.nilConstant),
						Expression.Block(Expression.Convert(sourceExpression, ESCompiledCode.blockFunctionTypeForNumArgs(functorArity))),
						Expression.Block(Expression.Convert(Expression.Field(asBlock, TypeGuru.esBlockType, "function"), ESCompiledCode.blockFunctionTypeForNumArgs(functorArity))));
		}

  		public static Expression expressionToConvertESMethodToFunctor(Expression sourceExpression, long functorArity) {
			var asMethod = Expression.TypeAs(sourceExpression, TypeGuru.esMethodType);
			return
				Expression.Condition(
					Expression.ReferenceEqual(asMethod, ExpressionTreeGuru.nilConstant),
						Expression.Block(Expression.Convert(sourceExpression, ESCompiledCode.methodFunctionTypeForNumArgs(functorArity))),
						Expression.Block(Expression.Convert(Expression.Field(asMethod, TypeGuru.esMethodType, "function"), ESCompiledCode.methodFunctionTypeForNumArgs(functorArity))));
		}

		#endregion

		public static Expression expressionToConvertAssociationToKeyValuePair(Expression associationExpression, Type keyType, Type valueType) {
			return Expression.Call(
					associationExpression,
					TypeGuru.esAssociationType.GetMethod("asKeyValuePair", TypeGuru.emptyTypeArray));
		}

		public static Expression expressionToConvertESClassToInstanceType(Expression esClassExpression) {
			return Expression.Property(esClassExpression, TypeGuru.esBehaviorType, "InstanceType");
		}

		#endregion

		#region Operational expressions

		public static Expression expressionToCreateMustBeBooleanException(String errorMessage) {
			return Expression.New(
					TypeGuru.mustBeBooleanExceptionType.GetConstructor(
						ESBehavior.instanceCreationBindingFlags, 
						Type.DefaultBinder, 
						new Type[]{TypeGuru.stringType},
						null),
					Expression.Constant(errorMessage));
		}

		public static Expression expressionThatMustBeBoolean(Expression expressionWhoseTypeMustBeBoolean, String errorMessage) {
			var catchBlock = Expression.Catch(
						TypeGuru.invalidCastExceptionType, 
						Expression.Block(
							TypeGuru.boolType, 
							Expression.Throw(expressionToCreateMustBeBooleanException(errorMessage)), 
							ExpressionTreeGuru.trueConstant));
			return Expression.TryCatch(expressionWhoseTypeMustBeBoolean.withType(TypeGuru.boolType), catchBlock);
		}

		public static Expression expressionToInvoke_Kernel_classOf(ESKernel kernel, Expression self) {
			return Expression.Call(
					Expression.Constant(kernel),
					TypeGuru.esKernelType.GetMethod("classOf", new Type[]{TypeGuru.objectType}),
					self);
		}

		public static Expression expressionToInvoke_RuntimeHelpers_GetHashCode(Expression self) {
			return Expression.Call(
					null,
					TypeGuru.runtimeHelpersType.GetMethod("GetHashCode", new Type[]{TypeGuru.objectType}),
					self);
		}

		public static Expression expressionToSendReferenceEquals(Expression self, Expression comparand) {
			return Expression.ReferenceEqual(self, comparand);
		}

		public static Expression expressionToSendReferenceNotEquals(Expression self, Expression comparand) {
			return Expression.ReferenceNotEqual(self, comparand);
		}
		
		public static Expression expressionToCreateArray(Type elementType, Expression size) {
			return Expression.NewArrayBounds(elementType, new Expression[]{size});
		}
		
		public static Expression expressionToCreateKeyValuePair(Expression keyExpression, Expression valueExpression) {
			var keyType = keyExpression.Type;
			var valueType = valueExpression.Type;
			var typeArray = new Type[]{keyExpression.Type, valueExpression.Type};
			var kvpType = TypeGuru.keyValuePairType.MakeGenericType(typeArray);
			return Expression.New(
				kvpType.GetConstructor(typeArray),
				keyExpression, 
				valueExpression);
		}
		
		public static Expression expressionToCreateESObjectArray(ESBehavior esClass, IEnumerable<Expression> elements) {
			return Expression.New(
				TypeGuru.esObjectArrayType.GetConstructor(
					ESBehavior.instanceCreationBindingFlags, 
					Type.DefaultBinder, 
					new Type[]{TypeGuru.esBehaviorType, TypeGuru.objectArrayType},
					null),
				Expression.Constant(esClass),
				Expression.NewArrayInit(TypeGuru.objectType, elements));
		}
		
		public static Expression expressionToCreateESByteArray(ESBehavior esClass, IEnumerable<Expression> elements) {
			return Expression.New(
				TypeGuru.esByteArrayType.GetConstructor(
					ESBehavior.instanceCreationBindingFlags, 
					Type.DefaultBinder, 
					new Type[]{TypeGuru.esBehaviorType, TypeGuru.byteArrayType},
					null),
				Expression.Constant(esClass),
				Expression.NewArrayInit(TypeGuru.byteType, elements));
		}
		
		public static Expression expressionToCreateESHalfWordArray(ESBehavior esClass, IEnumerable<Expression> elements) {
			return Expression.New(
				TypeGuru.esHalfWordArrayType.GetConstructor(
					ESBehavior.instanceCreationBindingFlags, 
					Type.DefaultBinder, 
					new Type[]{TypeGuru.esBehaviorType, TypeGuru.ushortArrayType},
					null),
				Expression.Constant(esClass),
				Expression.NewArrayInit(TypeGuru.ushortType, elements));
		}
		
		public static Expression expressionToCreateESWordArray(ESBehavior esClass, IEnumerable<Expression> elements) {
			return Expression.New(
				TypeGuru.esWordArrayType.GetConstructor(
					ESBehavior.instanceCreationBindingFlags, 
					Type.DefaultBinder, 
					new Type[]{TypeGuru.esBehaviorType, TypeGuru.uintArrayType},
					null),
				Expression.Constant(esClass),
				Expression.NewArrayInit(TypeGuru.uintType, elements));
		}
		
		public static Expression expressionToCreateESLongWordArray(ESBehavior esClass, IEnumerable<Expression> elements) {
			return Expression.New(
				TypeGuru.esLongWordArrayType.GetConstructor(
					ESBehavior.instanceCreationBindingFlags, 
					Type.DefaultBinder, 
					new Type[]{TypeGuru.esBehaviorType, TypeGuru.ulongArrayType},
					null),
				Expression.Constant(esClass),
				Expression.NewArrayInit(TypeGuru.ulongType, elements));
		}
		
		public static Expression expressionToCreateESFloatArray(ESBehavior esClass, IEnumerable<Expression> elements) {
			return Expression.New(
				TypeGuru.esFloatArrayType.GetConstructor(
					ESBehavior.instanceCreationBindingFlags, 
					Type.DefaultBinder, 
					new Type[]{TypeGuru.esBehaviorType, TypeGuru.floatArrayType},
					null),
				Expression.Constant(esClass),
				Expression.NewArrayInit(TypeGuru.floatType, elements));
		}
		
		public static Expression expressionToCreateESDoubleArray(ESBehavior esClass, IEnumerable<Expression> elements) {
			return Expression.New(
				TypeGuru.esDoubleArrayType.GetConstructor(
					ESBehavior.instanceCreationBindingFlags, 
					Type.DefaultBinder, 
					new Type[]{TypeGuru.esBehaviorType, TypeGuru.doubleArrayType},
					null),
				Expression.Constant(esClass),
				Expression.NewArrayInit(TypeGuru.doubleType, elements));
		}
		
		public static Expression expressionToCreateESQuadArray(ESBehavior esClass, IEnumerable<Expression> elements) {
			return Expression.New(
				TypeGuru.esQuadArrayType.GetConstructor(
					ESBehavior.instanceCreationBindingFlags, 
					Type.DefaultBinder, 
					new Type[]{TypeGuru.esBehaviorType, TypeGuru.decimalArrayType},
					null),
				Expression.NewArrayInit(TypeGuru.decimalType, elements));
		}
		
		public static Expression expressionToCreateESString(ESBehavior esClass, IEnumerable<Expression> elements) {
			return Expression.New(
				TypeGuru.esStringType.GetConstructor(
					ESBehavior.instanceCreationBindingFlags, 
					Type.DefaultBinder, 
					new Type[]{TypeGuru.esBehaviorType, TypeGuru.charArrayType},
					null),
				Expression.Constant(esClass),
				Expression.NewArrayInit(TypeGuru.charType, elements));
		}
		
		public static Expression expressionToCreateESPathname(ESBehavior esClass, IEnumerable<Expression> elements) {
			return Expression.New(
				TypeGuru.esPathnameType.GetConstructor(
					ESBehavior.instanceCreationBindingFlags, 
					Type.DefaultBinder, 
					new Type[]{TypeGuru.esBehaviorType, TypeGuru.stringArrayType},
					null),
				Expression.NewArrayInit(TypeGuru.stringType, elements));
		}
		
		public static Expression expressionToCreateESIdentityDictionary(ESBehavior esClass, IEnumerable<Expression> associationElements) {
			return Expression.New(
				TypeGuru.esIdentityDictionaryType.GetConstructor(
					ESBehavior.instanceCreationBindingFlags, 
					Type.DefaultBinder, 
					new Type[]{TypeGuru.esBehaviorType, TypeGuru.esAssociationArrayType},
					null),
				Expression.Constant(esClass),
				Expression.NewArrayInit(TypeGuru.esAssociationType, associationElements));
		}
		
		public static Expression expressionToCreateESDictionary(ESBehavior esClass, IEnumerable<Expression> associationElements) {
			return Expression.New(
				TypeGuru.esDictionaryType.GetConstructor(
					ESBehavior.instanceCreationBindingFlags, 
					Type.DefaultBinder, 
					new Type[]{TypeGuru.esBehaviorType, TypeGuru.esAssociationArrayType},
					null),
				Expression.Constant(esClass),
				Expression.NewArrayInit(TypeGuru.esAssociationType, associationElements));
		}
		
		public static Expression expressionToCreateESAssociation(ESBehavior esClass, Expression key, Expression value) {
			return Expression.New(
				TypeGuru.esAssociationType.GetConstructor(
					ESBehavior.instanceCreationBindingFlags, 
					Type.DefaultBinder, 
					new Type[]{TypeGuru.esBehaviorType, TypeGuru.objectType, TypeGuru.objectType},
					null),
				Expression.Constant(esClass),
				key,
				value);
		}
		
		public static Expression expressionToCreateESBlock(ESBehavior esClass, Expression function, long numArgs) {
			return Expression.New(
				compiledBlockConstructorInfo,
				Expression.Constant(esClass),
				function,
				Expression.Constant(numArgs));
		}
		
		public static Expression expressionToCreateESMethod(ESBehavior esClass, ESSymbol selector, Expression function) {
			return Expression.New(
					methodConstructorInfo,
					Expression.Constant(esClass),
					Expression.Constant(selector),
					function);

		}
		
		public static Expression expressionToCreateESMethod(ESBehavior esClass, ESSymbol selector, InlineOperation inlineOperation, Expression function) {
			return Expression.New(
					inlineMethodConstructorInfo,
					Expression.Constant(esClass),
					Expression.Constant(selector),
					Expression.Constant(inlineOperation),
					function);

		}
		
		public static Expression expressionToCreateESMethod(ESBehavior esClass, MethodDeclarationNode methodDeclarationNode) {
			return Expression.New(
					compiledMethodConstructorInfo,
					Expression.Constant(esClass),
					Expression.Constant(methodDeclarationNode));

		}

		public static Expression expressionToComputeSignOf(Expression self, Type numericType) {
			var zero = zeroConstant.withType(numericType);
			return Expression.Condition(
					Expression.GreaterThan(self, zero),
						Expression.Constant(1L),
						Expression.Condition(
							Expression.LessThan(self, zero), 
								Expression.Constant(-1L),
								zeroConstant));
		}

		public static Expression expressionToComputeAbsoluteValueOf(Expression self, Type numericType) {
			return Expression.Condition(
					Expression.LessThan(self, zeroConstant.withType(numericType)),
						Expression.Negate(self),
						self);
		}

		public static Expression expressionToInvoke_Math_Floor(Expression self, Type numericType) {
			var operationType = numericType.isDecimal() ? numericType : TypeGuru.doubleType;
			return Expression.Call(
					null,
					TypeGuru.mathType.GetMethod("Floor", new Type[]{operationType}),
					self.withType(operationType));
		}

		public static Expression expressionToInvoke_Math_Ceiling(Expression self, Type numericType) {
			var operationType = numericType.isDecimal() ? numericType : TypeGuru.doubleType;
			return Expression.Call(
					null,
					TypeGuru.mathType.GetMethod("Ceiling", new Type[]{operationType}),
					self.withType(operationType));
		}

		public static Expression expressionToInvoke_Math_Round(Expression self, Type numericType) {
			var operationType = numericType.isDecimal() ? numericType : TypeGuru.doubleType;
			return Expression.Call(
					null,
					TypeGuru.mathType.GetMethod("Round", new Type[]{operationType}),
					self.withType(operationType));
		}

		public static Expression expressionToTruncateNumber(Expression self, Type numericType) {
			return self.withType(numericType).withType(TypeGuru.longType);
		}

		public static Expression expressionToRoundANumberTo(Expression self, Expression modulus) {
			// Smalltalk: 53 roundTo: 5 => 55; 1263.48 roundTo: 0.25 => 1263.50.
			// C-Lang: 53.roundTo(5) => 55; 1263.48.roundTo(0.25) => 1263.50.
			self = self.withType(TypeGuru.doubleType);
			modulus = modulus.withType(TypeGuru.doubleType);
			return Expression.Multiply(
					Expression.Call(
						null,
						TypeGuru.mathType.GetMethod("Round", new Type[]{TypeGuru.doubleType}),
						Expression.Divide(self, modulus)), 
					modulus);
		}

		public static Expression expressionToTruncateANumberTo(Expression self, Expression modulus) {
			// Smalltalk: 53 truncateTo: 5 => 50; 1263.48 truncateTo: 0.25 => 1263.25.
			// C-Lang: 53.truncateTo(5) => 50; 1263.48.truncateTo(0.25) => 1263.25.
			self = self.withType(TypeGuru.doubleType);
			modulus = modulus.withType(TypeGuru.doubleType);
			return Expression.Multiply(
					Expression.Divide(self, modulus).withType(TypeGuru.longType).withType(TypeGuru.doubleType), 
					modulus);
		}

		public static Expression expressionToInvoke_Math_Pow(Expression self, Expression exponent) {
			return Expression.Call(
					null,
					TypeGuru.mathType.GetMethod("Pow", new Type[]{TypeGuru.doubleType, TypeGuru.doubleType}),
					self.withType(TypeGuru.doubleType),
					exponent.withType(TypeGuru.doubleType));
		}

		public static Expression expressionToInvoke_Math_Log(Expression self) {
			return Expression.Call(
					null,
					TypeGuru.mathType.GetMethod("Log", new Type[]{TypeGuru.doubleType}),
					self.withType(TypeGuru.doubleType));
		}

		public static Expression expressionToInvoke_Math_Log(Expression self, Expression logBase) {
			return Expression.Call(
					null,
					TypeGuru.mathType.GetMethod("Log", new Type[]{TypeGuru.doubleType, TypeGuru.doubleType}),
					self.withType(TypeGuru.doubleType),
					logBase.withType(TypeGuru.doubleType));
		}

		public static Expression expressionToCreateMessage(ESBehavior messageClass, ESSymbol selector, IEnumerable<Expression> arguments) {
			var pSelector = Expression.Constant(selector);
			var createArrayAction = Expression.NewArrayInit(TypeGuru.objectType, arguments);
			return 
				Expression.New(
					messageConstructorInfo,
					Expression.Constant(messageClass),
					pSelector,
					createArrayAction);
		}

		public static Expression expressionToCreateMessage(ESBehavior messageClass, ESSymbol selector, DynamicMetaObject[] args) {
			var arguments = new Expression[args.Length];
			for (var i = 0; i < args.Length; i++) arguments[i] = args[i].Expression;
			return expressionToCreateMessage(messageClass, selector, arguments);
		}

		public static Expression expressionToThrowInvalidFunctionCallException(
						Expression self, 
						ESBehavior esClass, 
						ESSymbol selector, 
						String messageText, 
						long actualArgCount,
						Type expectedFunctionType, 
						Type actualFunctionType) {

			return
				Expression.Call(
						null,
						TypeGuru.esKernelType.GetMethod(
								"throwInvalidFunctionCallException", 
								new Type[]{TypeGuru.stringType, TypeGuru.longType, TypeGuru.longType, TypeGuru.typeType, TypeGuru.typeType, TypeGuru.exceptionType}),
						Expression.Constant(messageText),
						Expression.Constant(selector.NumArgs),
						Expression.Constant(actualArgCount),
						Expression.Constant(expectedFunctionType),
						Expression.Constant(actualFunctionType),
						nilConstant);
		}

		public static Expression expressionToSendDoesNotUnderstand(Expression self, ESBehavior esClass, SymbolRegistry symbolRegistry, Expression createMessageAction) {

			var method = esClass.compiledMethodAt(symbolRegistry.DoesNotUnderstandSelector);
			if (method == null) {
				return
					Expression.Call(
							null,
							TypeGuru.esKernelType.GetMethod("throwMessageNotUnderstood", new Type[]{TypeGuru.objectType, TypeGuru.esMessageType}),
							self,
							createMessageAction);
			} else {
				return			
					Expression.Invoke(
						Expression.Constant(method.Function).withType(ESCompiledCode.methodFunctionTypeForNumArgs(method.NumArgs)), 
						self, 
						createMessageAction); 

			}
		
		}

		public static Expression expressionToSendDoesNotUnderstand(Expression self, ESBehavior esClass, ESSymbol selector, DynamicMetaObject[] args) {
			var kernel = esClass.Kernel;
			var message = expressionToCreateMessage(kernel.MessageClass, selector, args);
			return expressionToSendDoesNotUnderstand(self, esClass, kernel.SymbolRegistry, message);
		}

		public static Expression expressionToInvokeESMethod(ESMethod method, Expression[] argumentsWithReceiver) {
			return 
				Expression.Invoke(
					Expression.Constant(method.Function).withType(ESCompiledCode.methodFunctionTypeForNumArgs(method.NumArgs)), 
					argumentsWithReceiver);
		}

		#endregion

	}

	public static class BindingRestrictionsGuru {

		public static readonly BindingRestrictions							invariantRestriction		= ExpressionTreeGuru.trueConstant.asBindingRestriction();

		public static BindingRestrictions restrictionFor(Expression valueExpression) {
			return BindingRestrictions.GetTypeRestriction(valueExpression, valueExpression.Type);
		}

		public static BindingRestrictions restrictionFor(Expression valueExpression, Object value) {
			return BindingRestrictions.GetInstanceRestriction(valueExpression, value);
		}

		public static BindingRestrictions restrictionFor(Expression valueExpression, Type type) {
			return BindingRestrictions.GetTypeRestriction(valueExpression, type);
		}

		public static BindingRestrictions restrictionFor(Object value) {
			return restrictionFor(Expression.Constant(value), value);
		}

		public static BindingRestrictions instanceRestrictionFor(DynamicMetaObject dmo) {
			return dmo.Restrictions.Merge(restrictionFor(dmo.Expression, dmo.Value));
		}

		public static BindingRestrictions instanceRestrictionsFor(IEnumerable<DynamicMetaObject> dmoCollection) {
			var restrictions = BindingRestrictions.Empty;
			foreach (var dmo in dmoCollection) restrictions = restrictions.Merge(dmo.addingInstanceRestriction());
			return restrictions;
		}

		public static BindingRestrictions formalTypeRestrictionFor(DynamicMetaObject dmo) {
			return dmo.Restrictions.Merge(restrictionFor(dmo.Expression, dmo.LimitType));
		}

		public static BindingRestrictions formalTypeRestrictionsFor(IEnumerable<DynamicMetaObject> dmoCollection) {
			var restrictions = BindingRestrictions.Empty;
			foreach (var dmo in dmoCollection) restrictions = restrictions.Merge(dmo.addingFormalTypeRestriction());
			return restrictions;
		}

		public static BindingRestrictions addingRestrictionFor(this BindingRestrictions restrictions, Expression valueExpression) {
			return restrictions.Merge(restrictionFor(valueExpression));
		}

		public static BindingRestrictions addingRestrictionFor(this BindingRestrictions restrictions, Expression valueExpression, Object value) {
			return restrictions.Merge(restrictionFor(valueExpression, value));
		}

		public static BindingRestrictions addingRestrictionFor(this BindingRestrictions restrictions, Expression valueExpression, Type type) {
			return restrictions.Merge(restrictionFor(valueExpression, type));
		}

		public static BindingRestrictions addingRestrictionFor(this BindingRestrictions restrictions, Object value) {
			return restrictions.Merge(restrictionFor(value));
		}

		public static BindingRestrictions addingRestrictionFor(this BindingRestrictions restrictions, DynamicMetaObject dmo) {
			return restrictions.Merge(dmo.Restrictions);
		}

		public static BindingRestrictions addingInstanceRestrictionFor(this BindingRestrictions restrictions, DynamicMetaObject dmo) {
			return restrictions.Merge(dmo.addingInstanceRestriction());
		}

		public static BindingRestrictions addingFormalTypeRestrictionFor(this BindingRestrictions restrictions, DynamicMetaObject dmo) {
			return restrictions.Merge(dmo.addingFormalTypeRestriction());
		}

		public static BindingRestrictions restrictionThatESObjectHasSameClassVersion(Expression self, Type expectedType, ESBehavior esClass) {
			return restrictionFor(self, expectedType).Merge(ExpressionTreeGuru.expressionToTestThatESObjectHasSameClassVersion(self, esClass).asBindingRestriction());
		}

		public static BindingRestrictions restrictionThatForeignObjectHasSameClassVersion(Expression self, Type expectedType, ESBehavior esClass) {
			return restrictionFor(self, expectedType).Merge(ExpressionTreeGuru.expressionToTestThatClassHasSameClassVersion(esClass).asBindingRestriction());
		}

	}

	public static class DynamicMetaObjectExtensionMethods {

		#region Extension Methods

		public static Expression asExpressionWithType(this DynamicMetaObject metaObject, Type targetType) {
			var expression = metaObject.Expression;
			var expressionType = expression.Type; 
			if (expressionType == targetType) return expression;
			return metaObject.asExpressionWithFormalType().withType(targetType);
		}

		public static Expression asExpressionWithType(this DynamicMetaObject metaObject, Type targetType, MethodInfo conversionOperator) {
			var expression = metaObject.Expression;
			var expressionType = expression.Type; 
			if (expressionType == targetType) return expression;
			return metaObject.asExpressionWithFormalType().withType(targetType, conversionOperator);
		}

		public static Expression asExpressionWithFormalType(this DynamicMetaObject metaObject) {
			var expression = metaObject.Expression;
			var targetType = metaObject.LimitType;
			return expression.withType(targetType);
		}

		public static BindingRestrictions addingRestrictions(this DynamicMetaObject metaObject, BindingRestrictions restrictions) {
			return metaObject.Restrictions.Merge(restrictions);
		}

		public static BindingRestrictions addingRestrictions(this DynamicMetaObject metaObject, Expression restrictionExpression) {
			return metaObject.Restrictions.Merge(restrictionExpression.asBindingRestriction());
		}

		public static BindingRestrictions addingInstanceRestriction(this DynamicMetaObject metaObject) {
			return BindingRestrictionsGuru.instanceRestrictionFor(metaObject);
		}

		public static BindingRestrictions addingFormalTypeRestriction(this DynamicMetaObject metaObject) {
			return BindingRestrictionsGuru.formalTypeRestrictionFor(metaObject);
		}

		public static BindingRestrictions restrictionThatESObjectHasSameClassVersion(this DynamicMetaObject receiver, ESBehavior esClass) {
			return BindingRestrictionsGuru.restrictionThatESObjectHasSameClassVersion(receiver.Expression, receiver.LimitType, esClass);
		}

		public static BindingRestrictions restrictionThatForeignObjectHasSameClassVersion(this DynamicMetaObject receiver, ESBehavior esClass) {
			return BindingRestrictionsGuru.restrictionThatForeignObjectHasSameClassVersion(receiver.Expression, receiver.LimitType, esClass);
		}

		public static BindingRestrictions bindingRestrictionsForESObjectReceiver(this DynamicMetaObject receiver, ESBehavior esClass) {
			return receiver.Restrictions.Merge(receiver.restrictionThatESObjectHasSameClassVersion(esClass));
		}

		public static BindingRestrictions bindingRestrictionsForForeignObjectReceiver(this DynamicMetaObject receiver, ESBehavior esClass) {
			return receiver.Restrictions.Merge(receiver.restrictionThatForeignObjectHasSameClassVersion(esClass));
		}

		public static BindingRestrictions bindingRestrictionsForForeignObjectReceiver(this DynamicMetaObject receiver, ESBehavior esClass, DynamicMetaObject operand) {
			var restrictions = bindingRestrictionsForForeignObjectReceiver(receiver, esClass);
			return restrictions.Merge(operand.Restrictions);
		}

		public static BindingRestrictions bindingRestrictionsForForeignObjectReceiver(this DynamicMetaObject receiver, ESBehavior esClass, DynamicMetaObject arg1, DynamicMetaObject arg2) {
			var restrictions = bindingRestrictionsForForeignObjectReceiver(receiver, esClass, arg1);
			return restrictions.Merge(arg2.Restrictions);
		}

		public static BindingRestrictions bindingRestrictionsForForeignObjectReceiver(this DynamicMetaObject receiver, ESBehavior esClass, IEnumerable<DynamicMetaObject> arguments) {
			var restrictions = bindingRestrictionsForForeignObjectReceiver(receiver, esClass);
			foreach(var arg in arguments) restrictions = restrictions.Merge(arg.Restrictions);
			return restrictions;
		}

		public static BindingRestrictions bindingRestrictionsForForeignObjectReceiver(this DynamicMetaObject receiver, ESBehavior esClass, IEnumerable<DynamicMetaObject> arguments, DynamicMetaObject operand) {
			var restrictions = bindingRestrictionsForForeignObjectReceiver(receiver, esClass, arguments);
			return restrictions.Merge(operand.Restrictions);
		}

		public static DynamicMetaObject withInstanceRestriction(this DynamicMetaObject metaObject) {
			return new DynamicMetaObject(
					metaObject.asExpressionWithFormalType(), 
					metaObject.Restrictions.Merge(BindingRestrictions.GetInstanceRestriction(metaObject.Expression, metaObject.Value)), 
					metaObject.Value);
		}

		public static DynamicMetaObject withFormalTypeAndTypeRestriction(this DynamicMetaObject metaObject) {
			return new DynamicMetaObject(
					metaObject.asExpressionWithFormalType(), 
					metaObject.Restrictions.Merge(BindingRestrictions.GetTypeRestriction(metaObject.Expression, metaObject.LimitType)), 
					metaObject.Value);
		}

		public static DynamicMetaObject withInstanceRestrictionConvertingTo(this DynamicMetaObject metaObject, Type targetType) {
			return new DynamicMetaObject(
					metaObject.asExpressionWithType(targetType), 
					metaObject.Restrictions.Merge(BindingRestrictions.GetInstanceRestriction(metaObject.Expression, metaObject.Value)), 
					metaObject.Value);
		}

		public static DynamicMetaObject withTypeRestrictionConvertingTo(this DynamicMetaObject metaObject, Type targetType) {
			return new DynamicMetaObject(
					metaObject.asExpressionWithType(targetType), 
					metaObject.Restrictions.Merge(BindingRestrictions.GetTypeRestriction(metaObject.Expression, metaObject.LimitType)), 
					metaObject.Value);
		}

		public static DynamicMetaObject withTypeRestrictionConvertingTo(this DynamicMetaObject metaObject, Type targetType, MethodInfo conversionOperator) {
			return new DynamicMetaObject(
					metaObject.asExpressionWithType(targetType, conversionOperator), 
					metaObject.Restrictions.Merge(BindingRestrictions.GetTypeRestriction(metaObject.Expression, metaObject.LimitType)), 
					metaObject.Value);
		}

		public static DynamicMetaObject withExpressionAndTypeRestriction(this DynamicMetaObject metaObject, Expression expression) {
			return new DynamicMetaObject(
					expression, 
					metaObject.Restrictions.Merge(BindingRestrictions.GetTypeRestriction(metaObject.Expression, metaObject.LimitType)), 
					metaObject.Value);
		}

		public static DynamicMetaObject withExpressionAndTypeRestrictionConvertingTo(this DynamicMetaObject metaObject, Expression expression, Type targetType) {
			return new DynamicMetaObject(
					expression.withType(targetType), 
					metaObject.Restrictions.Merge(BindingRestrictions.GetTypeRestriction(metaObject.Expression, metaObject.LimitType)), 
					metaObject.Value);
		}

		public static DynamicMetaObject withExpressionAndTypeRestrictionConvertingTo(this DynamicMetaObject metaObject, Expression expression, Type targetType, MethodInfo conversionOperator) {
			return new DynamicMetaObject(
					expression.withType(targetType, conversionOperator), 
					metaObject.Restrictions.Merge(BindingRestrictions.GetTypeRestriction(metaObject.Expression, metaObject.LimitType)), 
					metaObject.Value);
		}

		public static TypeBindingGuru typeBindingGuru(this DynamicMetaObject metaObject, ESKernel kernel) {
			return new TypeBindingGuru(kernel, metaObject);
		}

		#endregion

	}

	public class TypeBindingGuru {

		protected ESKernel			kernel;
		protected DynamicMetaObject		argument;
		protected Object			model;
		protected ESObject			esModel;
		protected Type				modelType;
		protected TypeCode			modelTypeCode;
		protected ObjectStateArchitecture	modelArchitecture;
		protected HashSet<Type>			preferredTargetTypes;
		protected HashSet<Type>			disfavoredTargetTypes;

		public TypeBindingGuru(ESKernel kernel, DynamicMetaObject argument) {
			this.kernel	= kernel;
			this.argument	= argument;
			model		= argument.Value;
			modelType	= argument.LimitType;
			modelTypeCode	= Type.GetTypeCode(modelType);
			esModel = model as ESObject;
			switch (modelTypeCode) {
				case TypeCode.Empty:
				case TypeCode.DBNull:
					modelArchitecture = ObjectStateArchitecture.Nil;
					break;
				case TypeCode.Boolean:
					modelArchitecture =  (bool)model ? ObjectStateArchitecture.True : ObjectStateArchitecture.False;
					break;
				case TypeCode.Char:
					modelArchitecture =  ObjectStateArchitecture.Char;
					break;
				case TypeCode.Byte:
					modelArchitecture =  ObjectStateArchitecture.SmallInteger;
					break;
				case TypeCode.SByte:
					modelArchitecture =  ObjectStateArchitecture.SmallInteger;
					break;
				case TypeCode.UInt16:
					modelArchitecture =  ObjectStateArchitecture.SmallInteger;
					break;
				case TypeCode.Int16:
					modelArchitecture =  ObjectStateArchitecture.SmallInteger;
					break;
				case TypeCode.UInt32:
					modelArchitecture =  ObjectStateArchitecture.SmallInteger;
					break;
				case TypeCode.Int32:
					modelArchitecture =  ObjectStateArchitecture.SmallInteger;
					break;
				case TypeCode.UInt64:
					modelArchitecture =  ObjectStateArchitecture.SmallInteger;
					break;
				case TypeCode.Int64:
					modelArchitecture =  ObjectStateArchitecture.SmallInteger;
					break;
				case TypeCode.Single:
					modelArchitecture =  ObjectStateArchitecture.SinglePrecision;
					break;
				case TypeCode.Double:
					modelArchitecture =  ObjectStateArchitecture.DoublePrecision;
					break;
				case TypeCode.Decimal:
					modelArchitecture =  ObjectStateArchitecture.QuadPrecision;
					break;
				case TypeCode.String:
					modelArchitecture =  ObjectStateArchitecture.HostSystemObject;
					break;
				case TypeCode.DateTime:
					modelArchitecture =  ObjectStateArchitecture.HostSystemObject;
					break;
				default:
				case TypeCode.Object:
					if (argument.HasValue && model == null) {
						modelArchitecture = ObjectStateArchitecture.Nil;
						return;
					} else if (esModel == null) {
						modelArchitecture = ObjectStateArchitecture.HostSystemObject;
						return;
					}
					modelArchitecture = esModel.Architecture;
					break;
			}
		}

		public ESKernel Kernel {
			get {return kernel;}
		}

		public DynamicMetaObject Argument {
			get {return argument;}
		}

		public Object Model {
			get {return model;}
		}

		public ESObject ESModel {
			get {return esModel;}
		}

		public Type ModelType {
			get {return modelType;}
		}

		public TypeCode ModelTypeCode {
			get {return modelTypeCode;}
		}

		public ObjectStateArchitecture ModelArchitecture {
			get {return modelArchitecture;}
		}

		public int PreferredTargetTypesCount {
			get {return PreferredTargetTypes.Count;}
		}

		public int DisfavoredTargetTypesCount {
			get {return DisfavoredTargetTypes.Count;}
		}

		public HashSet<Type> PreferredTargetTypes {
			get {	if (preferredTargetTypes == null) computePotentialTargetTypes();
				return preferredTargetTypes;}
		}

		public HashSet<Type> DisfavoredTargetTypes {
			get {	if (disfavoredTargetTypes == null) computePotentialTargetTypes();
				return disfavoredTargetTypes;}
		}

		public void potentialTargetTypesDo(Action<Type> enumerator1) {
			enumerator1(ModelType);
			foreach (var type in PreferredTargetTypes) enumerator1(type);
			foreach (var type in DisfavoredTargetTypes) enumerator1(type);
		}

		protected void computePotentialTargetTypes() {

			preferredTargetTypes = new HashSet<Type>();
			disfavoredTargetTypes = new HashSet<Type>();
			preferredTargetTypes.Add(TypeGuru.esObjectType);			
			switch (ModelTypeCode) {
				case TypeCode.Byte:
					modelArchitecture =  ObjectStateArchitecture.SmallInteger;
					preferredTargetTypes.Add(TypeGuru.floatType);
					preferredTargetTypes.Add(TypeGuru.doubleType);
					preferredTargetTypes.Add(TypeGuru.decimalType);
					preferredTargetTypes.Add(TypeGuru.ushortType);
					preferredTargetTypes.Add(TypeGuru.shortType);
					preferredTargetTypes.Add(TypeGuru.uintType);
					preferredTargetTypes.Add(TypeGuru.intType);
					preferredTargetTypes.Add(TypeGuru.ulongType);
					preferredTargetTypes.Add(TypeGuru.longType);
					disfavoredTargetTypes.Add(TypeGuru.sbyteType);
					return;
				case TypeCode.SByte:
					modelArchitecture =  ObjectStateArchitecture.SmallInteger;
					preferredTargetTypes.Add(TypeGuru.floatType);
					preferredTargetTypes.Add(TypeGuru.doubleType);
					preferredTargetTypes.Add(TypeGuru.decimalType);
					preferredTargetTypes.Add(TypeGuru.shortType);
					preferredTargetTypes.Add(TypeGuru.intType);
					preferredTargetTypes.Add(TypeGuru.longType);
					disfavoredTargetTypes.Add(TypeGuru.byteType);
					return;
				case TypeCode.UInt16:
					modelArchitecture =  ObjectStateArchitecture.SmallInteger;
					preferredTargetTypes.Add(TypeGuru.floatType);
					preferredTargetTypes.Add(TypeGuru.doubleType);
					preferredTargetTypes.Add(TypeGuru.decimalType);
					preferredTargetTypes.Add(TypeGuru.uintType);
					preferredTargetTypes.Add(TypeGuru.intType);
					preferredTargetTypes.Add(TypeGuru.ulongType);
					preferredTargetTypes.Add(TypeGuru.longType);
					disfavoredTargetTypes.Add(TypeGuru.byteType);
					disfavoredTargetTypes.Add(TypeGuru.sbyteType);
					disfavoredTargetTypes.Add(TypeGuru.shortType);
					return;
				case TypeCode.Int16:
					modelArchitecture =  ObjectStateArchitecture.SmallInteger;
					preferredTargetTypes.Add(TypeGuru.floatType);
					preferredTargetTypes.Add(TypeGuru.doubleType);
					preferredTargetTypes.Add(TypeGuru.decimalType);
					preferredTargetTypes.Add(TypeGuru.intType);
					preferredTargetTypes.Add(TypeGuru.longType);
					disfavoredTargetTypes.Add(TypeGuru.byteType);
					disfavoredTargetTypes.Add(TypeGuru.sbyteType);
					disfavoredTargetTypes.Add(TypeGuru.ushortType);
					return;
				case TypeCode.UInt32:
					modelArchitecture =  ObjectStateArchitecture.SmallInteger;
					preferredTargetTypes.Add(TypeGuru.floatType);
					preferredTargetTypes.Add(TypeGuru.doubleType);
					preferredTargetTypes.Add(TypeGuru.decimalType);
					preferredTargetTypes.Add(TypeGuru.ulongType);
					preferredTargetTypes.Add(TypeGuru.longType);
					disfavoredTargetTypes.Add(TypeGuru.byteType);
					disfavoredTargetTypes.Add(TypeGuru.sbyteType);
					disfavoredTargetTypes.Add(TypeGuru.ushortType);
					disfavoredTargetTypes.Add(TypeGuru.shortType);
					disfavoredTargetTypes.Add(TypeGuru.intType);
					return;
				case TypeCode.Int32:
					modelArchitecture =  ObjectStateArchitecture.SmallInteger;
					preferredTargetTypes.Add(TypeGuru.floatType);
					preferredTargetTypes.Add(TypeGuru.doubleType);
					preferredTargetTypes.Add(TypeGuru.decimalType);
					preferredTargetTypes.Add(TypeGuru.longType);
					disfavoredTargetTypes.Add(TypeGuru.byteType);
					disfavoredTargetTypes.Add(TypeGuru.sbyteType);
					disfavoredTargetTypes.Add(TypeGuru.ushortType);
					disfavoredTargetTypes.Add(TypeGuru.shortType);
					disfavoredTargetTypes.Add(TypeGuru.uintType);
					return;
				case TypeCode.UInt64:
					modelArchitecture =  ObjectStateArchitecture.SmallInteger;
					preferredTargetTypes.Add(TypeGuru.floatType);
					preferredTargetTypes.Add(TypeGuru.doubleType);
					preferredTargetTypes.Add(TypeGuru.decimalType);
					disfavoredTargetTypes.Add(TypeGuru.byteType);
					disfavoredTargetTypes.Add(TypeGuru.sbyteType);
					disfavoredTargetTypes.Add(TypeGuru.ushortType);
					disfavoredTargetTypes.Add(TypeGuru.shortType);
					disfavoredTargetTypes.Add(TypeGuru.uintType);
					disfavoredTargetTypes.Add(TypeGuru.intType);
					disfavoredTargetTypes.Add(TypeGuru.longType);
					return;
				case TypeCode.Int64:
					modelArchitecture =  ObjectStateArchitecture.SmallInteger;
					preferredTargetTypes.Add(TypeGuru.floatType);
					preferredTargetTypes.Add(TypeGuru.doubleType);
					preferredTargetTypes.Add(TypeGuru.decimalType);
					disfavoredTargetTypes.Add(TypeGuru.byteType);
					disfavoredTargetTypes.Add(TypeGuru.sbyteType);
					disfavoredTargetTypes.Add(TypeGuru.ushortType);
					disfavoredTargetTypes.Add(TypeGuru.shortType);
					disfavoredTargetTypes.Add(TypeGuru.uintType);
					disfavoredTargetTypes.Add(TypeGuru.intType);
					disfavoredTargetTypes.Add(TypeGuru.ulongType);
					return;
				case TypeCode.Single:
					modelArchitecture =  ObjectStateArchitecture.SinglePrecision;
					preferredTargetTypes.Add(TypeGuru.doubleType);
					preferredTargetTypes.Add(TypeGuru.decimalType);
					disfavoredTargetTypes.Add(TypeGuru.byteType);
					disfavoredTargetTypes.Add(TypeGuru.sbyteType);
					disfavoredTargetTypes.Add(TypeGuru.ushortType);
					disfavoredTargetTypes.Add(TypeGuru.shortType);
					disfavoredTargetTypes.Add(TypeGuru.uintType);
					disfavoredTargetTypes.Add(TypeGuru.intType);
					disfavoredTargetTypes.Add(TypeGuru.ulongType);
					disfavoredTargetTypes.Add(TypeGuru.longType);
					return;
				case TypeCode.Double:
					modelArchitecture =  ObjectStateArchitecture.DoublePrecision;
					disfavoredTargetTypes.Add(TypeGuru.byteType);
					disfavoredTargetTypes.Add(TypeGuru.sbyteType);
					disfavoredTargetTypes.Add(TypeGuru.ushortType);
					disfavoredTargetTypes.Add(TypeGuru.shortType);
					disfavoredTargetTypes.Add(TypeGuru.uintType);
					disfavoredTargetTypes.Add(TypeGuru.intType);
					disfavoredTargetTypes.Add(TypeGuru.ulongType);
					disfavoredTargetTypes.Add(TypeGuru.longType);
					disfavoredTargetTypes.Add(TypeGuru.floatType);
					disfavoredTargetTypes.Add(TypeGuru.decimalType);
					return;
				case TypeCode.Decimal:
					modelArchitecture =  ObjectStateArchitecture.QuadPrecision;
					disfavoredTargetTypes.Add(TypeGuru.byteType);
					disfavoredTargetTypes.Add(TypeGuru.sbyteType);
					disfavoredTargetTypes.Add(TypeGuru.ushortType);
					disfavoredTargetTypes.Add(TypeGuru.shortType);
					disfavoredTargetTypes.Add(TypeGuru.uintType);
					disfavoredTargetTypes.Add(TypeGuru.intType);
					disfavoredTargetTypes.Add(TypeGuru.ulongType);
					disfavoredTargetTypes.Add(TypeGuru.longType);
					disfavoredTargetTypes.Add(TypeGuru.floatType);
					disfavoredTargetTypes.Add(TypeGuru.doubleType);
					return;
				case TypeCode.String:
					preferredTargetTypes.Add(TypeGuru.charArrayType);			
					return;
				case TypeCode.Object:
					if (argument.HasValue && model == null) {
						modelArchitecture = ObjectStateArchitecture.Nil;
						return;
					} else {
						if (esModel == null) {
							modelArchitecture = ObjectStateArchitecture.HostSystemObject;
							return;
						}
						modelArchitecture = esModel.Architecture;
					}
					break;
				default:
				case TypeCode.DateTime:
				case TypeCode.Empty:
				case TypeCode.DBNull:
				case TypeCode.Boolean:
				case TypeCode.Char:
					return;
			}

			switch (modelArchitecture) {
				case ObjectStateArchitecture.IndexedByteSlots:
					preferredTargetTypes.Add(TypeGuru.byteArrayType);			
					break;
				case ObjectStateArchitecture.IndexedCharSlots:
					preferredTargetTypes.Add(TypeGuru.charArrayType);			
					preferredTargetTypes.Add(TypeGuru.stringType);			
					break;
				case ObjectStateArchitecture.IndexedHalfWordSlots:
					preferredTargetTypes.Add(TypeGuru.ushortArrayType);			
					break;
				case ObjectStateArchitecture.IndexedWordSlots:
					preferredTargetTypes.Add(TypeGuru.uintArrayType);			
					break;
				case ObjectStateArchitecture.IndexedLongWordSlots:
					preferredTargetTypes.Add(TypeGuru.ulongArrayType);			
					break;
				case ObjectStateArchitecture.IndexedSinglePrecisionSlots:
					preferredTargetTypes.Add(TypeGuru.floatArrayType);			
					break;
				case ObjectStateArchitecture.IndexedDoublePrecisionSlots:
					preferredTargetTypes.Add(TypeGuru.doubleArrayType);			
					break;
				case ObjectStateArchitecture.IndexedQuadPrecisionSlots:
					preferredTargetTypes.Add(TypeGuru.decimalArrayType);			
					break;
				case ObjectStateArchitecture.Pathname:
					preferredTargetTypes.Add(TypeGuru.stringArrayType);			
					break;
				case ObjectStateArchitecture.IndexedObjectSlots:
					preferredTargetTypes.Add(TypeGuru.objectArrayType);			
					break;
				case ObjectStateArchitecture.Symbol:
					preferredTargetTypes.Add(TypeGuru.charArrayType);			
					preferredTargetTypes.Add(TypeGuru.stringType);			
					break;
				case ObjectStateArchitecture.Message:
					break;
				case ObjectStateArchitecture.MessageSend:
					break;
				case ObjectStateArchitecture.Association:
					preferredTargetTypes.Add(TypeGuru.keyValuePairForObjectKeyObjectValueType);			
					break;
				case ObjectStateArchitecture.BindingReference:
					preferredTargetTypes.Add(TypeGuru.keyValuePairForStringKeyBindingHandleValueType);			
					break;
				case ObjectStateArchitecture.IdentityDictionary:
				case ObjectStateArchitecture.Dictionary:
					preferredTargetTypes.Add(TypeGuru.iDictionaryObjectKeyObjectValueType);			
					preferredTargetTypes.Add(TypeGuru.dictionaryObjectKeyObjectValueType);			
					break;
				case ObjectStateArchitecture.Namespace:
					preferredTargetTypes.Add(TypeGuru.iDictionaryStringKeyBindingHandleValueType);			
					preferredTargetTypes.Add(TypeGuru.dictionaryStringKeyBindingHandleValueType);			
					break;
				case ObjectStateArchitecture.Block:
					var block = model as ESBlock;
					preferredTargetTypes.Add(ESCompiledCode.blockFunctionTypeForNumArgs(block.NumArgs));			
					break;
				case ObjectStateArchitecture.Method:
					var method = model as ESMethod;
					preferredTargetTypes.Add(ESCompiledCode.methodFunctionTypeForNumArgs(method.NumArgs));			
					break;
				case ObjectStateArchitecture.Behavior:
				case ObjectStateArchitecture.Class:
				case ObjectStateArchitecture.Metaclass:
					preferredTargetTypes.Add(TypeGuru.typeType);			
					break;
				case ObjectStateArchitecture.LargeInteger:
					break;
				case ObjectStateArchitecture.ScaledDecimal:
					break;
				default:
					break;
			}

		}

		public long compatibilityIndexForTargetType(Type targetType) {
			if (targetType == ModelType) return 0;
			if (ModelArchitecture == ObjectStateArchitecture.Nil) return targetType.nullIsAssignable() ? 0 : -1;
			var methodInfo = CompilerHelpers.GetImplicitConverter(ModelType, targetType);
			if (methodInfo != null) return 0;
			if (targetType.IsAssignableFrom(ModelType)) return 1;
			if (ModelArchitecture == ObjectStateArchitecture.Symbol && targetType.IsEnum) return 1;
			bool isAssignable = false;
			foreach (var type in PreferredTargetTypes) {
				if (targetType == type) return 2;
				methodInfo = CompilerHelpers.GetImplicitConverter(type, targetType);
				if (methodInfo != null) return 2;
				if (targetType.IsAssignableFrom(type)) isAssignable = true;
			}
			if (isAssignable) return 4;
			methodInfo = CompilerHelpers.GetExplicitConverter(ModelType, targetType);
			if (methodInfo != null) return 16;
			foreach (var type in PreferredTargetTypes) {
				methodInfo = CompilerHelpers.GetExplicitConverter(type, targetType);
				if (methodInfo != null) return 32;
			}
			isAssignable = false;
			var hasExplicitConverter = false;
			foreach (var type in DisfavoredTargetTypes) {
				if (targetType == type) return 64;
				methodInfo = CompilerHelpers.GetImplicitConverter(type, targetType);
				if (methodInfo != null) return 64;
				if (targetType.IsAssignableFrom(type)) isAssignable = true;
				methodInfo = CompilerHelpers.GetExplicitConverter(type, targetType);
				if (methodInfo != null) hasExplicitConverter = true;
			}
			return isAssignable ? 128 : hasExplicitConverter ? 256 : -1;
		}

		public DynamicMetaObject metaObjectToConvertTo(Type targetType) {
			if (targetType == ModelType) return argument.withFormalTypeAndTypeRestriction();
			var methodInfo = CompilerHelpers.GetImplicitConverter(ModelType, targetType);
			if (methodInfo != null) return argument.withTypeRestrictionConvertingTo(targetType, methodInfo);
			if (targetType.IsAssignableFrom(ModelType)) return argument.withTypeRestrictionConvertingTo(targetType);

			Expression expression, asCharArrayExpression, asStringExpression;
			Type functorType;
			long arity;
			switch (ModelArchitecture) {
				case ObjectStateArchitecture.Nil:
					return argument.withInstanceRestrictionConvertingTo(targetType);
				case ObjectStateArchitecture.IndexedByteSlots:
					expression = ExpressionTreeGuru.expressionToConvertESByteArrayToByteArray(argument.asExpressionWithFormalType());
					if (targetType == TypeGuru.byteArrayType) return argument.withExpressionAndTypeRestriction(expression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.byteArrayType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.byteArrayType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					expression = argument.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.IndexedCharSlots:
					var asSymbolExpression = ExpressionTreeGuru.expressionToCreateESSymbolFromESString(kernel.SymbolRegistry, argument.asExpressionWithFormalType());
					if (targetType == TypeGuru.esSymbolType) return argument.withExpressionAndTypeRestriction(asSymbolExpression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.esSymbolType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(asSymbolExpression, targetType, methodInfo);

					asCharArrayExpression = ExpressionTreeGuru.expressionToConvertESStringToCharArray(argument.asExpressionWithFormalType());
					if (targetType == TypeGuru.charArrayType) return argument.withExpressionAndTypeRestriction(asCharArrayExpression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.charArrayType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(asCharArrayExpression, targetType, methodInfo);

					asStringExpression = ExpressionTreeGuru.expressionToConvertESStringToString(argument.asExpressionWithFormalType());
					if (targetType == TypeGuru.stringType) return argument.withExpressionAndTypeRestriction(asStringExpression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.stringType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(asStringExpression, targetType, methodInfo);

					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.esSymbolType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(asSymbolExpression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.charArrayType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(asCharArrayExpression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.stringType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(asStringExpression, targetType, methodInfo);

					expression = argument.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.Symbol:

					if (targetType.IsEnum) return argument.withExpressionAndTypeRestriction(ExpressionTreeGuru.expressionToConvertSymbolToEnumerationConstant(argument.asExpressionWithFormalType(), targetType));

					asStringExpression = ExpressionTreeGuru.expressionToConvertESSymbolToString(argument.asExpressionWithFormalType());
					if (targetType == TypeGuru.stringType) return argument.withExpressionAndTypeRestriction(asStringExpression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.stringType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(asStringExpression, targetType, methodInfo);

					asCharArrayExpression = ExpressionTreeGuru.expressionToConvertESSymbolToCharArray(argument.asExpressionWithFormalType());
					if (targetType == TypeGuru.charArrayType) return argument.withExpressionAndTypeRestriction(asCharArrayExpression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.charArrayType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(asCharArrayExpression, targetType, methodInfo);

					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.stringType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(asStringExpression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.charArrayType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(asCharArrayExpression, targetType, methodInfo);

					expression = argument.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.IndexedHalfWordSlots:
					expression = ExpressionTreeGuru.expressionToConvertESHalfWordArrayToHalfWordArray(argument.asExpressionWithFormalType());
					if (targetType == TypeGuru.ushortArrayType) return argument.withExpressionAndTypeRestriction(expression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.ushortArrayType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.ushortArrayType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					expression = argument.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.IndexedWordSlots:
					expression = ExpressionTreeGuru.expressionToConvertESWordArrayToWordArray(argument.asExpressionWithFormalType());
					if (targetType == TypeGuru.uintArrayType) return argument.withExpressionAndTypeRestriction(expression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.uintArrayType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.uintArrayType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					expression = argument.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.IndexedLongWordSlots:
					expression = ExpressionTreeGuru.expressionToConvertESLongWordArrayToLongWordArray(argument.asExpressionWithFormalType());
					if (targetType == TypeGuru.ulongArrayType) return argument.withExpressionAndTypeRestriction(expression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.ulongArrayType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.ulongArrayType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					expression = argument.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.IndexedSinglePrecisionSlots:
					expression = ExpressionTreeGuru.expressionToConvertESFloatArrayToFloatArray(argument.asExpressionWithFormalType());
					if (targetType == TypeGuru.floatArrayType) return argument.withExpressionAndTypeRestriction(expression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.floatArrayType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.floatArrayType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					expression = argument.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.IndexedDoublePrecisionSlots:
					expression = ExpressionTreeGuru.expressionToConvertESDoubleArrayToDoubleArray(argument.asExpressionWithFormalType());
					if (targetType == TypeGuru.doubleArrayType) return argument.withExpressionAndTypeRestriction(expression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.doubleArrayType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.doubleArrayType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					expression = argument.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.IndexedQuadPrecisionSlots:
					expression = ExpressionTreeGuru.expressionToConvertESQuadArrayToDecimalArray(argument.asExpressionWithFormalType());
					if (targetType == TypeGuru.decimalArrayType) return argument.withExpressionAndTypeRestriction(expression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.decimalArrayType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.decimalArrayType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					expression = argument.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.Pathname:
					expression = ExpressionTreeGuru.expressionToConvertESPathnameToStringArray(argument.asExpressionWithFormalType());
					if (targetType == TypeGuru.stringArrayType) argument.withExpressionAndTypeRestriction(expression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.stringArrayType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.stringArrayType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					expression = argument.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.IndexedObjectSlots:
					expression = ExpressionTreeGuru.expressionToConvertESArrayToObjectArray(argument.asExpressionWithFormalType());
					if (targetType == TypeGuru.objectArrayType) return argument.withExpressionAndTypeRestriction(expression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.objectArrayType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.objectArrayType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					expression = argument.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.Association:
					expression = ExpressionTreeGuru.expressionToConvertAssociationToKeyValuePair(argument.asExpressionWithFormalType(), TypeGuru.objectType, TypeGuru.objectType);
					if (targetType == TypeGuru.keyValuePairForObjectKeyObjectValueType) return argument.withExpressionAndTypeRestriction(expression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.keyValuePairForObjectKeyObjectValueType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.keyValuePairForObjectKeyObjectValueType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					expression = argument.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.BindingReference:
					expression = ExpressionTreeGuru.expressionToConvertAssociationToKeyValuePair(argument.asExpressionWithFormalType(), TypeGuru.stringType, TypeGuru.esBindingReferenceType);
					if (targetType == TypeGuru.keyValuePairForStringKeyBindingHandleValueType) return argument.withExpressionAndTypeRestriction(expression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.keyValuePairForStringKeyBindingHandleValueType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.keyValuePairForStringKeyBindingHandleValueType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					expression = argument.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.IdentityDictionary:
				case ObjectStateArchitecture.Dictionary:
					expression = argument.asExpressionWithFormalType();
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.iDictionaryObjectKeyObjectValueType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.dictionaryObjectKeyObjectValueType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.iDictionaryObjectKeyObjectValueType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.dictionaryObjectKeyObjectValueType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					expression = argument.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.Namespace:
					expression = argument.asExpressionWithFormalType();
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.iDictionaryStringKeyBindingHandleValueType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.dictionaryStringKeyBindingHandleValueType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.iDictionaryStringKeyBindingHandleValueType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.dictionaryStringKeyBindingHandleValueType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					expression = argument.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.Block:
					var block = Model as ESBlock;
					arity = block.NumArgs;
					functorType = ESCompiledCode.blockFunctionTypeForNumArgs(arity);
					expression = ExpressionTreeGuru.expressionToConvertESBlockToFunctor(argument.asExpressionWithFormalType(), arity);
					if (TypeGuru.delegateType.IsAssignableFrom(targetType)) return argument.withExpressionAndTypeRestriction(expression);
					methodInfo = CompilerHelpers.GetImplicitConverter(functorType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(functorType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					expression = argument.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.Method:
					var method = Model as ESMethod;
					arity = method.NumArgs;
					functorType = ESCompiledCode.methodFunctionTypeForNumArgs(arity);
					expression = ExpressionTreeGuru.expressionToConvertESMethodToFunctor(argument.asExpressionWithFormalType(), arity);
					if (TypeGuru.delegateType.IsAssignableFrom(targetType)) return argument.withExpressionAndTypeRestriction(expression);
					methodInfo = CompilerHelpers.GetImplicitConverter(functorType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(functorType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					expression = argument.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.Behavior:
				case ObjectStateArchitecture.Class:
				case ObjectStateArchitecture.Metaclass:
					expression = ExpressionTreeGuru.expressionToConvertESClassToInstanceType(argument.asExpressionWithFormalType());
					if (TypeGuru.typeType.IsAssignableFrom(targetType)) return argument.withExpressionAndTypeRestriction(expression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.typeType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.typeType, targetType);
					if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					break;
				case ObjectStateArchitecture.SmallInteger:
				case ObjectStateArchitecture.SinglePrecision:
				case ObjectStateArchitecture.DoublePrecision:
				case ObjectStateArchitecture.QuadPrecision:
					if (targetType.isNumeric()) return argument.withTypeRestrictionConvertingTo(targetType);
					expression = argument.asExpressionWithFormalType();
					break;
				default:
				case ObjectStateArchitecture.False:
				case ObjectStateArchitecture.True:
				case ObjectStateArchitecture.Char:
				case ObjectStateArchitecture.Stateless:
				case ObjectStateArchitecture.NamedSlots:
				case ObjectStateArchitecture.Message:
				case ObjectStateArchitecture.MessageSend:
				case ObjectStateArchitecture.LargeInteger:
				case ObjectStateArchitecture.ScaledDecimal:
				case ObjectStateArchitecture.HostSystemObject:
					expression = argument.asExpressionWithFormalType();
					break;
			}

			methodInfo = CompilerHelpers.GetExplicitConverter(ModelType, targetType);
			if (methodInfo != null) return argument.withExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
			return argument.withTypeRestrictionConvertingTo(targetType);

		}

	}

}
