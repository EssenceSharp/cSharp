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
using Microsoft.Scripting.Generation;
#if CLR2
using Microsoft.Scripting.Ast;
#else
using System.Linq.Expressions;
using FuncNs = System;
#endif
#endregion

namespace EssenceSharp.Runtime.Binding {

	public class ArgumentBindingGuru {

		protected ESObjectSpace			objectSpace;
		protected DynamicMetaObject		metaObject;
		protected Object			model;
		protected ESObject			esModel;
		protected Type				modelType;
		protected TypeCode			modelTypeCode;
		protected ObjectStateArchitecture	modelArchitecture;
		protected HashSet<Type>			preferredTargetTypes;
		protected HashSet<Type>			disfavoredTargetTypes;

		public ArgumentBindingGuru(ESObjectSpace objectSpace, Expression expression) : this (objectSpace, expression.asDynamicMetaObject(BindingRestrictions.Empty)) {
		}

		public ArgumentBindingGuru(ESObjectSpace objectSpace, Expression expression, Object model) : this (objectSpace, expression.asDynamicMetaObject(BindingRestrictions.Empty, model)) {
		}

		public ArgumentBindingGuru(ESObjectSpace objectSpace, Expression expression, BindingRestrictions restrictions, Object model) : this (objectSpace, expression.asDynamicMetaObject(restrictions, model)) {
		}

		public ArgumentBindingGuru(ESObjectSpace objectSpace, DynamicMetaObject metaObject) {
			this.objectSpace	= objectSpace;
			this.metaObject		= metaObject;
			model			= metaObject.Value;
			modelType		= metaObject.LimitType;
			modelTypeCode		= Type.GetTypeCode(modelType);
			esModel			= model as ESObject;
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
					if (metaObject.HasValue && model == null) {
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

		public ESObjectSpace ObjectSpace {
			get {return objectSpace;}
		}

		public DynamicMetaObject MetaObject {
			get {return metaObject;}
		}

		public bool HasModel {
			get {return metaObject.HasValue;}
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
					if (metaObject.HasValue && model == null) {
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
			return isAssignable ? 128 : hasExplicitConverter ? 256 : 512;
		}

		public DynamicMetaObject metaObjectToConvertTo(Type targetType) {
			if (targetType == ModelType) return metaObject.argumentWithFormalTypeAndTypeRestriction();
			var methodInfo = CompilerHelpers.GetImplicitConverter(ModelType, targetType);
			if (methodInfo != null) return metaObject.argumentWithTypeRestrictionConvertingTo(targetType, methodInfo);
			if (targetType.IsAssignableFrom(ModelType)) return metaObject.argumentWithTypeRestrictionConvertingTo(targetType);

			Expression expression, asCharArrayExpression, asStringExpression;
			switch (ModelArchitecture) {
				case ObjectStateArchitecture.Nil:
					return metaObject.argumentWithInstanceRestrictionConvertingTo(targetType);
				case ObjectStateArchitecture.IndexedByteSlots:
					expression = ExpressionTreeGuru.expressionToConvertESByteArrayToByteArray(metaObject.asExpressionWithFormalType());
					if (targetType == TypeGuru.byteArrayType) return metaObject.argumentWithExpressionAndTypeRestriction(expression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.byteArrayType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.byteArrayType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					expression = metaObject.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.IndexedCharSlots:
					var asSymbolExpression = ExpressionTreeGuru.expressionToCreateESSymbolFromESString(objectSpace.SymbolRegistry, metaObject.asExpressionWithFormalType());
					if (targetType == TypeGuru.esSymbolType) return metaObject.argumentWithExpressionAndTypeRestriction(asSymbolExpression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.esSymbolType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(asSymbolExpression, targetType, methodInfo);

					asCharArrayExpression = ExpressionTreeGuru.expressionToConvertESStringToCharArray(metaObject.asExpressionWithFormalType());
					if (targetType == TypeGuru.charArrayType) return metaObject.argumentWithExpressionAndTypeRestriction(asCharArrayExpression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.charArrayType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(asCharArrayExpression, targetType, methodInfo);

					asStringExpression = ExpressionTreeGuru.expressionToConvertESStringToString(metaObject.asExpressionWithFormalType());
					if (targetType == TypeGuru.stringType) return metaObject.argumentWithExpressionAndTypeRestriction(asStringExpression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.stringType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(asStringExpression, targetType, methodInfo);

					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.esSymbolType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(asSymbolExpression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.charArrayType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(asCharArrayExpression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.stringType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(asStringExpression, targetType, methodInfo);

					expression = metaObject.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.Symbol:

					if (targetType.IsEnum) return metaObject.argumentWithExpressionAndTypeRestriction(ExpressionTreeGuru.expressionToConvertSymbolToEnumerationConstant(metaObject.asExpressionWithFormalType(), targetType));

					asStringExpression = ExpressionTreeGuru.expressionToConvertESSymbolToString(metaObject.asExpressionWithFormalType());
					if (targetType == TypeGuru.stringType) return metaObject.argumentWithExpressionAndTypeRestriction(asStringExpression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.stringType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(asStringExpression, targetType, methodInfo);

					asCharArrayExpression = ExpressionTreeGuru.expressionToConvertESSymbolToCharArray(metaObject.asExpressionWithFormalType());
					if (targetType == TypeGuru.charArrayType) return metaObject.argumentWithExpressionAndTypeRestriction(asCharArrayExpression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.charArrayType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(asCharArrayExpression, targetType, methodInfo);

					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.stringType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(asStringExpression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.charArrayType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(asCharArrayExpression, targetType, methodInfo);

					expression = metaObject.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.IndexedHalfWordSlots:
					expression = ExpressionTreeGuru.expressionToConvertESHalfWordArrayToHalfWordArray(metaObject.asExpressionWithFormalType());
					if (targetType == TypeGuru.ushortArrayType) return metaObject.argumentWithExpressionAndTypeRestriction(expression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.ushortArrayType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.ushortArrayType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					expression = metaObject.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.IndexedWordSlots:
					expression = ExpressionTreeGuru.expressionToConvertESWordArrayToWordArray(metaObject.asExpressionWithFormalType());
					if (targetType == TypeGuru.uintArrayType) return metaObject.argumentWithExpressionAndTypeRestriction(expression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.uintArrayType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.uintArrayType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					expression = metaObject.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.IndexedLongWordSlots:
					expression = ExpressionTreeGuru.expressionToConvertESLongWordArrayToLongWordArray(metaObject.asExpressionWithFormalType());
					if (targetType == TypeGuru.ulongArrayType) return metaObject.argumentWithExpressionAndTypeRestriction(expression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.ulongArrayType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.ulongArrayType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					expression = metaObject.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.IndexedSinglePrecisionSlots:
					expression = ExpressionTreeGuru.expressionToConvertESFloatArrayToFloatArray(metaObject.asExpressionWithFormalType());
					if (targetType == TypeGuru.floatArrayType) return metaObject.argumentWithExpressionAndTypeRestriction(expression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.floatArrayType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.floatArrayType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					expression = metaObject.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.IndexedDoublePrecisionSlots:
					expression = ExpressionTreeGuru.expressionToConvertESDoubleArrayToDoubleArray(metaObject.asExpressionWithFormalType());
					if (targetType == TypeGuru.doubleArrayType) return metaObject.argumentWithExpressionAndTypeRestriction(expression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.doubleArrayType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.doubleArrayType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					expression = metaObject.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.IndexedQuadPrecisionSlots:
					expression = ExpressionTreeGuru.expressionToConvertESQuadArrayToDecimalArray(metaObject.asExpressionWithFormalType());
					if (targetType == TypeGuru.decimalArrayType) return metaObject.argumentWithExpressionAndTypeRestriction(expression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.decimalArrayType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.decimalArrayType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					expression = metaObject.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.Pathname:
					expression = ExpressionTreeGuru.expressionToConvertESPathnameToStringArray(metaObject.asExpressionWithFormalType());
					if (targetType == TypeGuru.stringArrayType) metaObject.argumentWithExpressionAndTypeRestriction(expression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.stringArrayType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.stringArrayType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					expression = metaObject.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.IndexedObjectSlots:
					expression = ExpressionTreeGuru.expressionToConvertESArrayToObjectArray(metaObject.asExpressionWithFormalType());
					if (targetType == TypeGuru.objectArrayType) return metaObject.argumentWithExpressionAndTypeRestriction(expression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.objectArrayType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.objectArrayType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					expression = metaObject.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.Association:
					expression = ExpressionTreeGuru.expressionToConvertAssociationToKeyValuePair(metaObject.asExpressionWithFormalType(), TypeGuru.objectType, TypeGuru.objectType);
					if (targetType == TypeGuru.keyValuePairForObjectKeyObjectValueType) return metaObject.argumentWithExpressionAndTypeRestriction(expression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.keyValuePairForObjectKeyObjectValueType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.keyValuePairForObjectKeyObjectValueType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					expression = metaObject.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.BindingReference:
					expression = ExpressionTreeGuru.expressionToConvertAssociationToKeyValuePair(metaObject.asExpressionWithFormalType(), TypeGuru.stringType, TypeGuru.esBindingReferenceType);
					if (targetType == TypeGuru.keyValuePairForStringKeyBindingHandleValueType) return metaObject.argumentWithExpressionAndTypeRestriction(expression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.keyValuePairForStringKeyBindingHandleValueType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.keyValuePairForStringKeyBindingHandleValueType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					expression = metaObject.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.IdentityDictionary:
				case ObjectStateArchitecture.Dictionary:
					expression = metaObject.asExpressionWithFormalType();
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.iDictionaryObjectKeyObjectValueType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.dictionaryObjectKeyObjectValueType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.iDictionaryObjectKeyObjectValueType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.dictionaryObjectKeyObjectValueType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					expression = metaObject.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.Namespace:
					expression = metaObject.asExpressionWithFormalType();
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.iDictionaryStringKeyBindingHandleValueType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.dictionaryStringKeyBindingHandleValueType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.iDictionaryStringKeyBindingHandleValueType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.dictionaryStringKeyBindingHandleValueType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					expression = metaObject.asExpressionWithFormalType();
					break;
				case ObjectStateArchitecture.Block:
					var block = Model as ESBlock;
					return metaObject.argumentWithExpressionAndTypeRestriction(
						ExpressionTreeGuru.expressionToCreateBridgeFunctorForBlockOrDelegate(objectSpace, metaObject, block.NumArgs, targetType));
				case ObjectStateArchitecture.Method:
					var method = Model as ESMethod;
					return metaObject.argumentWithExpressionAndTypeRestriction(
						ExpressionTreeGuru.expressionToCreateBridgeFunctorForMethodOrDelegate(objectSpace, metaObject, method.NumArgs, targetType));
				case ObjectStateArchitecture.Behavior:
				case ObjectStateArchitecture.Class:
				case ObjectStateArchitecture.Metaclass:
					expression = ExpressionTreeGuru.expressionToConvertESClassToInstanceType(metaObject.asExpressionWithFormalType());
					if (TypeGuru.typeType.IsAssignableFrom(targetType)) return metaObject.argumentWithExpressionAndTypeRestriction(expression);
					methodInfo = CompilerHelpers.GetImplicitConverter(TypeGuru.typeType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					methodInfo = CompilerHelpers.GetExplicitConverter(TypeGuru.typeType, targetType);
					if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
					break;
				case ObjectStateArchitecture.SmallInteger:
				case ObjectStateArchitecture.SinglePrecision:
				case ObjectStateArchitecture.DoublePrecision:
				case ObjectStateArchitecture.QuadPrecision:
					if (targetType.isNumeric()) return metaObject.argumentWithTypeRestrictionConvertingTo(targetType);
					expression = metaObject.asExpressionWithFormalType();
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
					expression = metaObject.asExpressionWithFormalType();
					break;
			}

			methodInfo = CompilerHelpers.GetExplicitConverter(ModelType, targetType);
			if (methodInfo != null) return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType, methodInfo);
			return metaObject.argumentWithExpressionAndTypeRestrictionConvertingTo(expression, targetType);

		}

	}

}
