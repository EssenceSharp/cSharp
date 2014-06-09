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
using System.Reflection;
using System.Linq;
using System.Dynamic;
#if CLR2
using Microsoft.Scripting.Ast;
using Expression = Microsoft.Scripting.Ast.Expression;
using ExpressionType = Microsoft.Scripting.Ast.ExpressionType;
using ParameterExpression = Microsoft.Scripting.Ast.ParameterExpression;
using FuncNs = Microsoft.Scripting.Utils;
#else
using System.Linq.Expressions;
using Expression =System.Linq.Expressions.Expression;
using ExpressionType =System.Linq.Expressions.ExpressionType;
using ParameterExpression =System.Linq.Expressions.ParameterExpression;
using FuncNs = System;
#endif
using EssenceSharp.ClientServices;

#endregion

namespace EssenceSharp.Runtime.Binding { 

	public class DynamicBindingGuru {

		#region Static variables and methods

		#region Constants

		public static readonly DynamicMetaObject	zero				= new DynamicMetaObject(ExpressionTreeGuru.zeroConstant, BindingRestrictions.Empty, 0L);
		public static readonly DynamicMetaObject	zeroInt32			= new DynamicMetaObject(ExpressionTreeGuru.zeroConstant, BindingRestrictions.Empty, 0);
		public static readonly DynamicMetaObject	one				= new DynamicMetaObject(ExpressionTreeGuru.oneConstant, BindingRestrictions.Empty, 1L);
		public static readonly DynamicMetaObject	oneInt32			= new DynamicMetaObject(ExpressionTreeGuru.oneConstant, BindingRestrictions.Empty, 1);
		public static readonly DynamicMetaObject	oneHalf				= new DynamicMetaObject(ExpressionTreeGuru.oneHalfConstant, BindingRestrictions.Empty, 0.5);
		public static readonly DynamicMetaObject	eulersConstant			= new DynamicMetaObject(ExpressionTreeGuru.eulersConstant, BindingRestrictions.Empty, Math.E);
		
		public static readonly DynamicMetaObject	oneWithHighestGenerality	= new DynamicMetaObject(Expression.Constant(1.0), BindingRestrictions.Empty, 1.0);
		public static readonly DynamicMetaObject	doNothing			= new DynamicMetaObject(Expression.Empty(), BindingRestrictions.Empty, null);

		public static readonly DynamicMetaObject[]	emptyArgArray			= new DynamicMetaObject[0];
		public static readonly List<DynamicMetaObject>	emptyArgList			= new List<DynamicMetaObject>();

		#endregion

		#region Utilities

		public static bool convertNumericTypesToHighestGenerality(DynamicMetaObject leftInput, DynamicMetaObject rightInput, out Expression leftOutput, out Expression rightOutput) {
			leftOutput = leftInput.Expression;
			rightOutput = rightInput.Expression;
			var leftType = leftInput.LimitType;
			var rightType = rightInput.LimitType;
			var typeWithHighestGenerality = leftType.typeWithHighestNumericGenerality(rightType);
			if (typeWithHighestGenerality == null) return false;
			leftOutput = leftType == typeWithHighestGenerality ? Expression.Convert(leftInput.Expression, leftType) : Expression.Convert(Expression.Convert(leftInput.Expression, leftType), typeWithHighestGenerality);
			rightOutput = rightType == typeWithHighestGenerality ? Expression.Convert(rightInput.Expression, rightType) : Expression.Convert(Expression.Convert(rightInput.Expression, rightType), typeWithHighestGenerality);
			return true;
		}

		public static Object dictionaryAtIfAbsent <KeyType, ValueType> (IDictionary<KeyType, ValueType> dictionary, KeyType key, FuncNs.Func<Object> valueIfAbsent) {
			ValueType value;
			if (!dictionary.TryGetValue(key, out value)) {
				return valueIfAbsent == null ? null : valueIfAbsent();
			}
			return value;
		}

		public static Object dictionaryAtIfAbsentPut <KeyType, ValueType> (IDictionary<KeyType, ValueType> dictionary, KeyType key, FuncNs.Func<ValueType> valueIfAbsent) {
			ValueType value;
			if (!dictionary.TryGetValue(key, out value)) {
				value = valueIfAbsent();
				dictionary[key] = value;
			}
			return value;
		}

		#region DynamicMetaObject argument operations

		public static DynamicMetaObject[] argArrayFor(DynamicMetaObject arg) {
			return new DynamicMetaObject[]{arg};
		}

		public static DynamicMetaObject[] argArrayFor(DynamicMetaObject arg0, DynamicMetaObject arg1) {
			return new DynamicMetaObject[]{arg0, arg1};
		}

		public static DynamicMetaObject[] argArrayFor(DynamicMetaObject[] argsPrefix, DynamicMetaObject argSuffix) {
			var argList = new List<DynamicMetaObject>(argsPrefix.Length + 1);
			argList.AddRange(argsPrefix);
			argList.Add(argSuffix);
			return argList.ToArray();
		}

		public static DynamicMetaObject[] argArrayFor(DynamicMetaObject argPrefix, DynamicMetaObject[] argsSuffix) {
			var argList = new List<DynamicMetaObject>(argsSuffix.Length + 1);
			argList.Add(argPrefix);
			argList.AddRange(argsSuffix);
			return argList.ToArray();
		}

		public static DynamicMetaObject[] argArrayFor(DynamicMetaObject argPrefix, DynamicMetaObject[] argsInfix, DynamicMetaObject argSuffix) {
			var argList = new List<DynamicMetaObject>(argsInfix.Length + 2);
			argList.Add(argPrefix);
			argList.AddRange(argsInfix);
			argList.Add(argSuffix);
			return argList.ToArray();
		}

		public static Type[] argLimitTypeArrayFor(DynamicMetaObject[] args) {
			var typeArray = new Type[args.Length];
			for (var i = 0; i < typeArray.Length; i++) typeArray[i] = args[i].LimitType;
 			return typeArray;
		}

		public static Expression[] expressionArrayFor(DynamicMetaObject[] metaObjects) {
			var expressionArray = new Expression[metaObjects.Length];
			int i = 0;
			foreach (var metaObject in metaObjects) {
				expressionArray[i++] = metaObject.Expression;
			}
			return expressionArray;
		}

		public static Expression[] expressionArrayFor(DynamicMetaObject[] metaObjects, Type conversionType) {
			var expressionArray = new Expression[metaObjects.Length];
			int i = 0;
			foreach (var metaObject in metaObjects) {
				var formalType = metaObject.LimitType;
				if (formalType == conversionType) {
					expressionArray[i++] = metaObject.Expression;
				} else {
					expressionArray[i++] = Expression.Convert(metaObject.Expression, conversionType);
				}
			}
			return expressionArray;
		}

		public static List<DynamicMetaObject> typeCompatibleArgumentsFor(ParameterInfo[] parameters, List<TypeBindingGuru> argGurus) {
			var arguments = new List<DynamicMetaObject>();
			long arity = parameters.Length;
			if (argGurus.Count != arity) return null;
			for (var i = 0; i < arity; i++) {
				var p = parameters[i];
				var argGuru = argGurus[i];
				var argMo = argGuru.metaObjectToConvertTo(p.ParameterType);
				if (argMo == null) {
					arguments = null;
					break;
				}
				arguments.Add(argMo);
			}
			return arguments;
		}

		#region Static-typing-idiocy duplicate methods

		public static List<DynamicMetaObject> typeCompatibleArgumentsFor(MethodInfo methodInfo, List<TypeBindingGuru> argGurus) {
			return typeCompatibleArgumentsFor(methodInfo.GetParameters(), argGurus);
		}

		public static List<DynamicMetaObject> typeCompatibleArgumentsFor(ConstructorInfo constructorInfo, List<TypeBindingGuru> argGurus) {
			return typeCompatibleArgumentsFor(constructorInfo.GetParameters(), argGurus);
		}

		#endregion

		#endregion

		#endregion

		#region DynamicMetaObjects for general message sending

		public static DynamicMetaObject metaObjectToSendMessage(DynamicMetaObject receiver, ESKernel kernel, ESBehavior esClass, ESSymbol selector, ESMethod method, DynamicMetaObject[] argumentsWithoutReceiver, BindingRestrictions bindingRestrictions) {

			Expression expression;
			var self = receiver.Expression;
			if (method == null) {
				expression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(self, esClass, selector, argumentsWithoutReceiver);
				return new DynamicMetaObject(expression, bindingRestrictions, receiver.Value);
			}

			long arity = argumentsWithoutReceiver.Length;
			InlineOperation operation = method.InlineOperation;
			if (operation == null) {
				var argumentsWithReceiver = new Expression[arity + 1];
				argumentsWithReceiver[0] = self;
				for (int i = 1, j = 0; i <= arity; i++, j++) argumentsWithReceiver[i] = argumentsWithoutReceiver[j].Expression;
				expression = ExpressionTreeGuru.expressionToInvokeESMethod(method, argumentsWithReceiver);
			} else {
				FieldInfo field;
				PropertyInfo property;
				MethodInfo methodInfo;
				DynamicMetaObject arg;
				TypeBindingGuru argGuru;
				Expression fieldExpression;
				Expression propertyExpression;
				expression = null;
				DynamicBindingGuru dynamicBindingGuru;
				List<DynamicMetaObject> typedArguments;
				switch (operation.Type) {
					case MethodOperationType.Convert:
						Type targetType = ESBehavior.typeFromAssemblyQualifiedName(operation.Operand, true);
						expression = Expression.Convert(receiver.asExpressionWithFormalType(), targetType);
						break;
					case MethodOperationType.GetField:
						field = esClass.getField(operation.Operand);
						expression = Expression.Field(esClass.IsHostSystemMetaclass ? null : receiver.asExpressionWithFormalType(), field);
						break;
					case MethodOperationType.GetProperty:
						property = esClass.getReadableProperty(operation.Operand);
						expression = Expression.Property(esClass.IsHostSystemMetaclass ? null : receiver.asExpressionWithFormalType(), property);
						break;
					case MethodOperationType.SetField:
						field = esClass.getField(operation.Operand);
						arg = argumentsWithoutReceiver[0];
						argGuru = arg.typeBindingGuru(kernel);
						arg = argGuru.metaObjectToConvertTo(field.FieldType);
						fieldExpression = Expression.Field(esClass.IsHostSystemMetaclass ? null : receiver.asExpressionWithFormalType(), field);
						expression = Expression.Block(Expression.Assign(fieldExpression, arg.asExpressionWithFormalType()), self);
						bindingRestrictions = bindingRestrictions.Merge(arg.Restrictions);
						break;
					case MethodOperationType.SetProperty:
						property = esClass.getWritableProperty(operation.Operand);
						arg = argumentsWithoutReceiver[0];
						argGuru = arg.typeBindingGuru(kernel);
						arg = argGuru.metaObjectToConvertTo(property.PropertyType);
						propertyExpression = Expression.Property(esClass.IsHostSystemMetaclass ? null : receiver.asExpressionWithFormalType(), property);
						expression = Expression.Block(Expression.Assign(propertyExpression, arg.asExpressionWithFormalType()), self);
						bindingRestrictions = bindingRestrictions.Merge(arg.Restrictions);
						break;
					case MethodOperationType.InvokeField:
						field = esClass.getField(operation.Operand);
						if (TypeGuru.delegateType.IsAssignableFrom(field.FieldType)) {
							fieldExpression = Expression.Field(esClass.IsHostSystemMetaclass ? null : receiver.asExpressionWithFormalType(), field);
							methodInfo = field.FieldType.GetMethod("Invoke");
							dynamicBindingGuru = kernel.DynamicBindingGuru;
							var argGurus = dynamicBindingGuru.dynamicMetaObjectArgumentGurusFor(argumentsWithoutReceiver);
							typedArguments = typeCompatibleArgumentsFor(methodInfo, argGurus);
							expression = Expression.Invoke(fieldExpression, expressionArrayFor(typedArguments.ToArray()));
							foreach(var argMo in typedArguments) bindingRestrictions = bindingRestrictions.Merge(argMo.Restrictions);
							if (methodInfo.ReturnType == TypeGuru.voidType) expression = Expression.Block(TypeGuru.objectType, expression, receiver.Expression);
						}
						break;
					case MethodOperationType.InvokeProperty:
						property = esClass.getReadableProperty(operation.Operand);
						if (TypeGuru.delegateType.IsAssignableFrom(property.PropertyType)) {
							propertyExpression = Expression.Property(esClass.IsHostSystemMetaclass ? null : receiver.asExpressionWithFormalType(), property);
							methodInfo = property.PropertyType.GetMethod("Invoke");
							dynamicBindingGuru = kernel.DynamicBindingGuru;
							var argGurus = dynamicBindingGuru.dynamicMetaObjectArgumentGurusFor(argumentsWithoutReceiver);
							typedArguments = typeCompatibleArgumentsFor(methodInfo, argGurus);
							expression = Expression.Invoke(propertyExpression, expressionArrayFor(typedArguments.ToArray()));
							foreach(var argMo in typedArguments) bindingRestrictions = bindingRestrictions.Merge(argMo.Restrictions);
							if (methodInfo.ReturnType == TypeGuru.voidType) expression = Expression.Block(TypeGuru.objectType, expression, receiver.Expression);
						}
						break;
					case MethodOperationType.InvokeMethod:
						dynamicBindingGuru = kernel.DynamicBindingGuru;
						if (dynamicBindingGuru.getMethodAndTypeCompatibleArgumentsFor(operation.Operand, esClass, argumentsWithoutReceiver, out methodInfo, out typedArguments)) {
							if (methodInfo.IsStatic) {
								expression = Expression.Call(
										null, 
										methodInfo, 
										expressionArrayFor(typedArguments.ToArray()));
							} else { 
								self = receiver.asExpressionWithFormalType();
								if (!methodInfo.DeclaringType.IsAssignableFrom(self.Type)) self = Expression.Convert(self, methodInfo.DeclaringType);
								expression = Expression.Call(
										self, 
										methodInfo, 
										expressionArrayFor(typedArguments.ToArray()));
							}
							foreach(var argMo in typedArguments) bindingRestrictions = bindingRestrictions.Merge(argMo.Restrictions);
							if (methodInfo.ReturnType == TypeGuru.voidType) expression = Expression.Block(TypeGuru.objectType, expression, receiver.Expression);
						}
						break;
					case MethodOperationType.CreateInstance:
						ConstructorInfo constructorInfo;
						dynamicBindingGuru = kernel.DynamicBindingGuru;
						var instanceClass = esClass is ESMetaclass ? ((ESMetaclass)esClass).CanonicalInstance : esClass;
						if (dynamicBindingGuru.getConstructorAndTypeCompatibleArgumentsFor(instanceClass, argumentsWithoutReceiver, out constructorInfo, out typedArguments)) {
							expression = Expression.New(constructorInfo, expressionArrayFor(typedArguments.ToArray()));
							foreach(var argMo in typedArguments) bindingRestrictions = bindingRestrictions.Merge(argMo.Restrictions);
						}
						break;
					default:
						break;
				}
				if (expression == null) {
					expression = operation.OnFailExpression;
				} else if (Type.GetTypeCode(expression.Type) != TypeCode.Object) {
					expression = Expression.Convert(expression, TypeGuru.objectType);
				}
			}
			return new DynamicMetaObject(expression, bindingRestrictions, receiver.Value);

		}

		#endregion

		#endregion

		protected ESKernel							kernel							= null;
		protected SymbolRegistry						symbolRegistry						= null;
		protected ESSymbol							selectorValue0						= null;
		protected ESSymbol							selectorValue1						= null;
		protected GetPropertyOrFieldOfForeignObjectBinder.Registry		getPropertyOrFieldOfForeignObjectBinderRegistry		= null;
		protected SetPropertyOrFieldOfForeignObjectBinder.Registry		setPropertyOrFieldOfForeignObjectBinderRegistry		= null;
		protected GetValueAtIndexOrKeyInForeignObjectBinder.Registry		getValueAtIndexOrKeyInForeignObjectBinderRegistry	= null;
		protected SetValueAtIndexOrKeyInForeignObjectBinder.Registry		setValueAtIndexOrKeyInForeignObjectBinderRegistry	= null;
		protected RemoveAtIndexOrKeyFromForeignCollectionBinder.Registry	removeAtIndexOrKeyFromForeignCollectionBinderRegistry	= null;
		protected UnaryMessageSendToForeignObjectBinder.Registry		unaryMessageSendToForeignObjectBinderRegistry		= null;
		protected BinaryMessageSendToForeignObjectBinder.Registry		binaryMessageSendToForeignObjectBinderRegistry		= null;
		protected ConvertTypeOfForeignObjectBinder.Registry			convertTypeOfForeignObjectBinderRegistry		= null;
		protected InvokeForeignFunctionBinder.Registry				invokeForeignFunctionBinderRegistry			= null;
		protected MessageSendToForeignObjectBinder.Registry			messageSendToForeignObjectBinderRegistry		= null;
		protected CreateInstanceOfForeignObjectBinder.Registry			createInstanceOfForeignObjectBinderRegistry		= null;

		public DynamicBindingGuru(ESKernel kernel) {
			this.kernel	= kernel;
			initialize();
		}

		protected virtual void initialize() {
			symbolRegistry							= kernel.SymbolRegistry;
			selectorValue0							= symbolFor("value");
			selectorValue1							= symbolFor("value:");
			getPropertyOrFieldOfForeignObjectBinderRegistry			= new GetPropertyOrFieldOfForeignObjectBinder.Registry(this);
			setPropertyOrFieldOfForeignObjectBinderRegistry			= new SetPropertyOrFieldOfForeignObjectBinder.Registry(this);
			getValueAtIndexOrKeyInForeignObjectBinderRegistry		= new GetValueAtIndexOrKeyInForeignObjectBinder.Registry(this);
			setValueAtIndexOrKeyInForeignObjectBinderRegistry		= new SetValueAtIndexOrKeyInForeignObjectBinder.Registry(this);
			removeAtIndexOrKeyFromForeignCollectionBinderRegistry		= new RemoveAtIndexOrKeyFromForeignCollectionBinder.Registry(this);
			unaryMessageSendToForeignObjectBinderRegistry			= new UnaryMessageSendToForeignObjectBinder.Registry(this);
			binaryMessageSendToForeignObjectBinderRegistry			= new BinaryMessageSendToForeignObjectBinder.Registry(this);
			convertTypeOfForeignObjectBinderRegistry			= new ConvertTypeOfForeignObjectBinder.Registry(this);
			invokeForeignFunctionBinderRegistry				= new InvokeForeignFunctionBinder.Registry(this);
			messageSendToForeignObjectBinderRegistry			= new MessageSendToForeignObjectBinder.Registry(this);
			createInstanceOfForeignObjectBinderRegistry			= new CreateInstanceOfForeignObjectBinder.Registry(this);
		}

		public ESKernel Kernel {
			get {return kernel;}
		}

		public SymbolRegistry SymbolRegistry {
			get {return symbolRegistry;}
		}

		public List<TypeBindingGuru> dynamicMetaObjectArgumentGurusFor(IEnumerable<DynamicMetaObject> metaObjects) {
			var argGurus = new List<TypeBindingGuru>();
			foreach (var mo in metaObjects) argGurus.Add(mo.typeBindingGuru(Kernel));
			return argGurus;
		}

		public bool selectMethodAndTypeCompatibleArgumentsFor(MethodInfo[] candidateMethods, DynamicMetaObject[] args, out MethodInfo methodInfo, out List<DynamicMetaObject> typedArguments) {
			if (candidateMethods.Length > 0) {
				long arity = args.Length;
				ParameterInfo[] parameters;
				var argGurus = dynamicMetaObjectArgumentGurusFor(args);
				if (candidateMethods.Length < 2) {
					methodInfo = candidateMethods[0];
					typedArguments = typeCompatibleArgumentsFor(methodInfo, argGurus);
					if (typedArguments == null) {
						methodInfo = null;
						return false;
					}
					return true;
				} else {
					typedArguments = new List<DynamicMetaObject>();
					methodInfo = null;
					long minScore = long.MaxValue;
					foreach (var mi in candidateMethods) {
						parameters = mi.GetParameters();
						if (parameters.Length != arity) continue;
						var score = 0L;
						for (var i = 0; i < arity; i++) {
							var p = parameters[i];
							var argGuru = argGurus[i];
							var index = argGuru.compatibilityIndexForTargetType(p.ParameterType);
							if (index < 0) {
								score = -1;
								break;
							}
							score += index;
						}
						if (score >= 0) {
							if (score < minScore) {
								minScore = score;
								methodInfo = mi;
							}
						}
					}
					if (methodInfo == null) {
						return false;
					} else {
						typedArguments = typeCompatibleArgumentsFor(methodInfo, argGurus);
						if (typedArguments == null) {
							methodInfo = null;
							return false;
						}
						return true;
					}
				}
			} else {
				methodInfo = null;
				typedArguments = null;
				return false;
			}
		}

		#region Static-typing-idiocy duplicate methods

		public bool getMethodAndTypeCompatibleArgumentsFor(String methodName, ESBehavior esClass, DynamicMetaObject[] args, out MethodInfo methodInfo, out List<DynamicMetaObject> typedArguments) {
			long arity = args.Length;
			if (arity < 1) {
				methodInfo = esClass.getHostMethod(methodName, TypeGuru.emptyTypeArray);
				typedArguments = emptyArgList;
				return methodInfo != null;
			} else {
				return selectMethodAndTypeCompatibleArgumentsFor(esClass.getHostMethodsMatching(methodName, arity).ToArray(), args, out methodInfo, out typedArguments);
			}
		}

		public bool getConstructorAndTypeCompatibleArgumentsFor(ESBehavior esClass, DynamicMetaObject[] args, out ConstructorInfo constructorInfo, out List<DynamicMetaObject> typedArguments) {
			long arity = args.Length;
			if (arity < 1) {
				constructorInfo = esClass.getHostConstructor(TypeGuru.emptyTypeArray);
				typedArguments = emptyArgList;
				return constructorInfo != null;
			} else {
				ParameterInfo[] parameters;
				var argGurus = dynamicMetaObjectArgumentGurusFor(args);
				var constructors = esClass.getHostConstructors(arity);
				if (constructors.Count > 0) {
					if (constructors.Count < 2) {
						constructorInfo = constructors[0];
						typedArguments = typeCompatibleArgumentsFor(constructorInfo, argGurus);
						if (typedArguments == null) {
							constructorInfo = null;
							return false;
						}
						return true;
					} else {
						typedArguments = new List<DynamicMetaObject>();
						constructorInfo = null;
						long minScore = long.MaxValue;
						foreach (var mi in constructors) {
							parameters = mi.GetParameters();
							var score = 0L;
							for (var i = 0; i < arity; i++) {
								var p = parameters[i];
								var argGuru = argGurus[i];
								var index = argGuru.compatibilityIndexForTargetType(p.ParameterType);
								if (index < 0) {
									score = -1;
									break;
								}
								score += index;
							}
							if (score >= 0) {
								if (score < minScore) {
									minScore = score;
									constructorInfo = mi;
								}
							}
						}
						if (constructorInfo == null) {
							return false;
						} else {
							typedArguments = typeCompatibleArgumentsFor(constructorInfo, argGurus);
							if (typedArguments == null) {
								constructorInfo = null;
								return false;
							}
							return true;
						}
					}
				} else {
					constructorInfo = null;
					typedArguments = null;
					return false;
				}
			}
		}

		#endregion

		#region Canonical Binders

		internal GetPropertyOrFieldOfForeignObjectBinder canonicalGetMemberBinderFor(ESBehavior esClass, ESSymbol selector) {
			return getPropertyOrFieldOfForeignObjectBinderRegistry.canonicalBinderFor(esClass, selector);
		}

		internal SetPropertyOrFieldOfForeignObjectBinder canonicalSetMemberBinderFor(ESBehavior esClass, ESSymbol selector) {
			return setPropertyOrFieldOfForeignObjectBinderRegistry.canonicalBinderFor(esClass, selector);
		}

		internal GetValueAtIndexOrKeyInForeignObjectBinder canonicalGetIndexBinderFor(ESBehavior esClass, ESSymbol selector) {
			return getValueAtIndexOrKeyInForeignObjectBinderRegistry.canonicalBinderFor(esClass, selector);
		}

		internal SetValueAtIndexOrKeyInForeignObjectBinder canonicalSetIndexBinderFor(ESBehavior esClass, ESSymbol selector) {
			return setValueAtIndexOrKeyInForeignObjectBinderRegistry.canonicalBinderFor(esClass, selector);
		}

		internal RemoveAtIndexOrKeyFromForeignCollectionBinder canonicalDeleteIndexBinderFor(ESBehavior esClass, ESSymbol selector) {
			return removeAtIndexOrKeyFromForeignCollectionBinderRegistry.canonicalBinderFor(esClass, selector);
		}

		internal UnaryMessageSendToForeignObjectBinder canonicalUnaryOperationBinderFor(ESBehavior esClass, ESSymbol selector, ExpressionType operation) {
			return unaryMessageSendToForeignObjectBinderRegistry.canonicalBinderFor(esClass, selector, operation);
		}

		internal BinaryMessageSendToForeignObjectBinder canonicalBinaryOperationBinderFor(ESBehavior esClass, ESSymbol selector, ExpressionType operation) {
			return binaryMessageSendToForeignObjectBinderRegistry.canonicalBinderFor(esClass, selector, operation);
		}

		internal ConvertTypeOfForeignObjectBinder canonicalConvertBinderFor(ESBehavior esClass, Type type) {
			return convertTypeOfForeignObjectBinderRegistry.canonicalBinderFor(esClass, type);
		}

		internal InvokeForeignFunctionBinder canonicalInvokeBinderFor(ESBehavior esClass, ESSymbol selector) {
			return invokeForeignFunctionBinderRegistry.canonicalBinderFor(esClass, selector);
		}

		internal MessageSendToForeignObjectBinder canonicalInvokeMemberBinderFor(ESBehavior esClass, ESSymbol selector, String memberName) {
			return messageSendToForeignObjectBinderRegistry.canonicalBinderFor(esClass, selector, memberName);
		}

		internal CreateInstanceOfForeignObjectBinder canonicalCreateInstanceBinderFor(ESBehavior esClass, ESSymbol selector) {
			return createInstanceOfForeignObjectBinderRegistry.canonicalBinderFor(esClass, selector);
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

		#endregion

		#region DynamicMetaObject Construction

		public DynamicMetaObject metaObjectForInvariantOperation(Expression invariantRestrictionOperation, Object value) {
			return new DynamicMetaObject(invariantRestrictionOperation.withType(TypeGuru.objectType), BindingRestrictionsGuru.invariantRestriction, value);
		}

		public DynamicMetaObject metaObjectForInstanceRestrictedOperation(Expression instanceRestrictedOperation, Object value) {
			return new DynamicMetaObject(instanceRestrictedOperation.withType(TypeGuru.objectType), BindingRestrictionsGuru.restrictionFor(instanceRestrictedOperation, value), value);
		}

		public DynamicMetaObject metaObjectForTypeRestrictedOperation(Expression typeRestrictedOperation, Type restrictionType, Object value) {
			return new DynamicMetaObject(typeRestrictedOperation.withType(TypeGuru.objectType), BindingRestrictionsGuru.restrictionFor(typeRestrictedOperation, restrictionType), value);
		}

		public DynamicMetaObject metaObjectForForeignObjectOperation(DynamicMetaObject receiver, ESBehavior esClass, Expression invariantOperationExpression) {
			
			return new DynamicMetaObject(invariantOperationExpression.withType(TypeGuru.objectType), receiver.bindingRestrictionsForForeignObjectReceiver(esClass), receiver.Value);
		}

		public DynamicMetaObject metaObjectToCreateAssociation(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject valueMO) {
			Expression expression = ExpressionTreeGuru.expressionToCreateESAssociation(kernel.AssociationClass, receiver.Expression, valueMO.Expression);
			return new DynamicMetaObject(
				Expression.Convert(expression, TypeGuru.objectType), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass), 
				receiver.Value);
		}

		public DynamicMetaObject metaObjectForConditionalAnd(DynamicMetaObject receiver, ESBehavior esClass, DynamicMetaObject operandMO) {
			var model = receiver.Value;
			var self = Expression.Convert(receiver.Expression, TypeGuru.boolType);
			operandMO = operandMO.BindInvoke(canonicalInvokeBinderFor(kernel.classOf(operandMO.Value), selectorValue0), emptyArgArray);
			var operand = Expression.Convert(operandMO.Expression, TypeGuru.boolType);
			return new DynamicMetaObject(
				Expression.Convert(Expression.AndAlso(self, operand), TypeGuru.objectType), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass).Merge(operandMO.addingInstanceRestriction()), 
				model);
		}

		public DynamicMetaObject metaObjectForConditionalOr(DynamicMetaObject receiver, ESBehavior esClass, DynamicMetaObject operandMO) {
			var model = receiver.Value;
			var self = Expression.Convert(receiver.Expression, TypeGuru.boolType);
			operandMO = operandMO.BindInvoke(canonicalInvokeBinderFor(kernel.classOf(operandMO.Value), selectorValue0), emptyArgArray);
			var operand = Expression.Convert(operandMO.Expression, TypeGuru.boolType);
			return new DynamicMetaObject(
				Expression.Convert(Expression.OrElse(self, operand), TypeGuru.objectType), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass).Merge(operandMO.addingInstanceRestriction()), 
				model);
		}

		public DynamicMetaObject metaObjectToInvokeArgIfTrue(DynamicMetaObject receiver, ESBehavior esClass, Expression testExpression, DynamicMetaObject actionToInvoke, Expression testFailResult) {
			var model = receiver.Value;
			var self = receiver.Expression;
			var actionFunction = actionToInvoke.BindInvoke(canonicalInvokeBinderFor(kernel.classOf(actionToInvoke.Value), selectorValue0), emptyArgArray);
			return new DynamicMetaObject(
				Expression.Condition(
					Expression.Convert(testExpression, TypeGuru.boolType), 
					actionFunction.Expression, 
					testFailResult), 
				(model == null ?
					receiver.addingRestrictions(ExpressionTreeGuru.expressionToTestThatNilHasSameClassVersion(self, kernel.UndefinedObjectClass)).Merge(actionToInvoke.addingInstanceRestriction()) :
					receiver.bindingRestrictionsForForeignObjectReceiver(esClass)).Merge(actionToInvoke.addingInstanceRestriction()), 
				model);
		}

		public DynamicMetaObject metaObjectToInvokeArgIfFalse(DynamicMetaObject receiver, ESBehavior esClass, Expression testExpression, DynamicMetaObject actionToInvoke, Expression testFailResult) {
			var model = receiver.Value;
			var self = receiver.Expression;
			var actionFunction = actionToInvoke.BindInvoke(canonicalInvokeBinderFor(kernel.classOf(actionToInvoke.Value), selectorValue0), emptyArgArray);
			return new DynamicMetaObject(
				Expression.Condition(
					Expression.Convert(testExpression, TypeGuru.boolType), 
					testFailResult, 
					actionFunction.Expression), 
				(model == null ?
					receiver.addingRestrictions(ExpressionTreeGuru.expressionToTestThatNilHasSameClassVersion(self, kernel.UndefinedObjectClass)).Merge(actionToInvoke.addingInstanceRestriction()) :
					receiver.bindingRestrictionsForForeignObjectReceiver(esClass)).Merge(actionToInvoke.addingInstanceRestriction()), 
				model);
		}

		public DynamicMetaObject metaObjectToInvokeArgIfTrueAndArgIfFalse(DynamicMetaObject receiver, ESBehavior esClass, Expression testExpression, DynamicMetaObject actionToInvokeIfTrue, DynamicMetaObject actionToInvokeIfFalse) {
			var model = receiver.Value;
			var self = receiver.Expression;
			var ifTrueAction = actionToInvokeIfTrue.BindInvoke(canonicalInvokeBinderFor(kernel.classOf(actionToInvokeIfTrue.Value), selectorValue0), emptyArgArray);
			var ifFalseAction = actionToInvokeIfFalse.BindInvoke(canonicalInvokeBinderFor(kernel.classOf(actionToInvokeIfFalse.Value), selectorValue0), emptyArgArray);
			return new DynamicMetaObject(
				Expression.Condition(
					Expression.Convert(testExpression, TypeGuru.boolType), 
					ifTrueAction.Expression, 
					ifFalseAction.Expression), 
				(model == null ?
					receiver.addingRestrictions(ExpressionTreeGuru.expressionToTestThatNilHasSameClassVersion(self, kernel.UndefinedObjectClass)).Merge(actionToInvokeIfTrue.addingInstanceRestriction()).Merge(actionToInvokeIfFalse.addingInstanceRestriction()) :
					receiver.bindingRestrictionsForForeignObjectReceiver(esClass)).Merge(actionToInvokeIfTrue.addingInstanceRestriction()).Merge(actionToInvokeIfFalse.addingInstanceRestriction()), 
				model);
		}

		public DynamicMetaObject metaObjectToComputeSignOf(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector) {
			Expression expression;
			var receiverType = receiver.LimitType;
			if (receiverType.isNumeric()) {
				expression = ExpressionTreeGuru.expressionToComputeSignOf(receiver.asExpressionWithFormalType(), receiverType);
			} else {
				expression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(receiver.Expression, esClass, selector, emptyArgArray);
			}
			return new DynamicMetaObject(
				Expression.Convert(expression, TypeGuru.objectType), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass), 
				receiver.Value);
		}

		public DynamicMetaObject metaObjectToComputeAbsoluteValueOf(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector) {
			Expression expression;
			var receiverType = receiver.LimitType;
			if (receiverType.isNumeric()) {
				expression = ExpressionTreeGuru.expressionToComputeAbsoluteValueOf(receiver.asExpressionWithFormalType(), receiverType);
			} else {
				expression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(receiver.Expression, esClass, selector, emptyArgArray);
			}
			return new DynamicMetaObject(
				Expression.Convert(expression, TypeGuru.objectType), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass), 
				receiver.Value);
		}

		public DynamicMetaObject metaObjectToComputeCeilingOf(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector) {
			Expression expression;
			var receiverType = receiver.LimitType;
			if (receiverType.isNumeric()) {
				if (receiverType.isRational()) {
					expression = Expression.Convert(ExpressionTreeGuru.expressionToInvoke_Math_Ceiling(receiver.asExpressionWithFormalType(), receiverType), TypeGuru.longType);
				} else {
					expression = receiver.asExpressionWithFormalType();
				}
			} else {
				expression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(receiver.Expression, esClass, selector, emptyArgArray);
			}
			return new DynamicMetaObject(
				Expression.Convert(expression, TypeGuru.objectType), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass), 
				receiver.Value);
		}

		public DynamicMetaObject metaObjectToComputeFloorOf(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector) {
			Expression expression;
			var receiverType = receiver.LimitType;
			if (receiverType.isNumeric()) {
				if (receiverType.isRational()) {
					expression = Expression.Convert(ExpressionTreeGuru.expressionToInvoke_Math_Floor(receiver.asExpressionWithFormalType(), receiverType), TypeGuru.longType);
				} else {
					expression = receiver.asExpressionWithFormalType();
				}
			} else {
				expression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(receiver.Expression, esClass, selector, emptyArgArray);
			}
			return new DynamicMetaObject(
				Expression.Convert(expression, TypeGuru.objectType), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass), 
				receiver.Value);
		}

		public DynamicMetaObject metaObjectToRoundANumber(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector) {
			Expression expression;
			var receiverType = receiver.LimitType;
			if (receiverType.isNumeric()) {
				if (receiverType.isRational()) {
					expression = Expression.Convert(ExpressionTreeGuru.expressionToInvoke_Math_Round(receiver.asExpressionWithFormalType(), receiverType), TypeGuru.longType);
				} else {
					expression = receiver.asExpressionWithFormalType();
				}
			} else {
				expression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(receiver.Expression, esClass, selector, emptyArgArray);
			}
			return new DynamicMetaObject(
				Expression.Convert(expression, TypeGuru.objectType), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass), 
				receiver.Value);
		}

		public DynamicMetaObject metaObjectToTruncateANumber(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector) {
			Expression expression;
			var receiverType = receiver.LimitType;
			if (receiverType.isNumeric()) {
				if (receiverType.isRational()) {
					expression = ExpressionTreeGuru.expressionToTruncateNumber(receiver.asExpressionWithFormalType(), receiverType);
				} else {
					expression = receiver.Expression;
				}
			} else {
				expression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(receiver.Expression, esClass, selector, emptyArgArray);
			}
			return new DynamicMetaObject(
				Expression.Convert(expression, TypeGuru.objectType), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass), 
				receiver.Value);
		}

		public DynamicMetaObject metaObjectToRoundANumberTo(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject modulusMO) {
			Expression expression;
			var receiverType = receiver.LimitType;
			var modulusType = modulusMO.LimitType;
			var operationType = receiverType.typeWithHighestNumericGenerality(modulusType);	
			if (operationType != null) {
				expression = Expression.Convert(
						ExpressionTreeGuru.expressionToRoundANumberTo(
							receiver.asExpressionWithFormalType(), 
							modulusMO.asExpressionWithFormalType()),
						operationType);
			} else {
				expression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(receiver.Expression, esClass, selector, argArrayFor(modulusMO));
			}
			return new DynamicMetaObject(
				Expression.Convert(expression, TypeGuru.objectType), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass), 
				receiver.Value);
		}

		public DynamicMetaObject metaObjectToTruncateANumberTo(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject modulusMO) {
			Expression expression;
			var receiverType = receiver.LimitType;
			var modulusType = modulusMO.LimitType;
			var operationType = receiverType.typeWithHighestNumericGenerality(modulusType);	
			if (operationType != null) {
				expression = ExpressionTreeGuru.expressionToTruncateANumberTo(receiver.asExpressionWithFormalType(), modulusMO.asExpressionWithFormalType());
			} else {
				expression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(receiver.Expression, esClass, selector, argArrayFor(modulusMO));
			}
			return new DynamicMetaObject(
				Expression.Convert(expression, TypeGuru.objectType), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass), 
				receiver.Value);
		}

		public DynamicMetaObject metaObjectToRaiseANumberTo(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject exponent) {
			Expression expression;
			var receiverType = receiver.LimitType;
			var exponentType = exponent.LimitType;
			var operationType = receiverType.typeWithHighestNumericGenerality(exponentType);	
			if (operationType != null) {
				expression = ExpressionTreeGuru.expressionToInvoke_Math_Pow(
									receiver.asExpressionWithFormalType(), 
									exponent.asExpressionWithFormalType());
			} else {
				expression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(receiver.Expression, esClass, selector, argArrayFor(exponent));
			}
			return new DynamicMetaObject(
				Expression.Convert(expression, TypeGuru.objectType), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass), 
				receiver.Value);
		}

		public DynamicMetaObject metaObjectToComputeNaturalLogarithm(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector) {
			Expression expression;
			var receiverType = receiver.LimitType;
			if (receiverType.isNumeric()) {
				expression = ExpressionTreeGuru.expressionToInvoke_Math_Log(receiver.asExpressionWithFormalType());
			} else {
				expression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(receiver.Expression, esClass, selector, emptyArgArray);
			}
			return new DynamicMetaObject(
				Expression.Convert(expression, TypeGuru.objectType), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass), 
				receiver.Value);
		}

		public DynamicMetaObject metaObjectToComputeLogarithm(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject logBase) {
			Expression expression;
			var receiverType = receiver.LimitType;
			var logBaseType = logBase.LimitType;
			var operationType = receiverType.typeWithHighestNumericGenerality(logBaseType);	
			if (operationType != null) {
				expression = ExpressionTreeGuru.expressionToInvoke_Math_Log(
									receiver.asExpressionWithFormalType(), 
									logBase.asExpressionWithFormalType());
			} else {
				expression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(receiver.Expression, esClass, selector, argArrayFor(logBase));
			}
			return new DynamicMetaObject(
				Expression.Convert(expression, TypeGuru.objectType), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass), 
				receiver.Value);
		}

		public DynamicMetaObject metaObjectToDivideToInteger(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject divisorMO) {
			// Selector = #quo:
			Expression expression;
			var receiverType = receiver.LimitType;
			var divisorType = divisorMO.LimitType;
			var operationType = receiverType.typeWithHighestNumericGenerality(divisorType);	
			if (operationType != null) {
				var self = Expression.Convert(receiver.asExpressionWithFormalType(), operationType);
				var divisor = Expression.Convert(divisorMO.asExpressionWithFormalType(), operationType);
				expression = Expression.Divide(self, divisor);
				if (!operationType.isInteger()) {
					expression = Expression.Convert(expression, TypeGuru.longType);
				}
			} else {
				expression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(receiver.Expression, esClass, selector, argArrayFor(divisorMO));
			}
			return new DynamicMetaObject(
				Expression.Convert(expression, TypeGuru.objectType), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass, divisorMO), 
				receiver.Value);
		}

		public DynamicMetaObject metaObjectToDivideAndFloorToInteger(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject divisorMO) {
			// Selector = #//
			Expression expression;
			var receiverType = receiver.LimitType;
			var divisorType = divisorMO.LimitType;
			var operationType = receiverType.typeWithHighestNumericGenerality(divisorType);	
			if (operationType != null) {
				if (!operationType.isInteger()) operationType = TypeGuru.doubleType;
				var self = Expression.Convert(receiver.asExpressionWithFormalType(), operationType);
				var divisor = Expression.Convert(divisorMO.asExpressionWithFormalType(), operationType);
				expression = Expression.Divide(self, divisor);
				expression = ExpressionTreeGuru.expressionToInvoke_Math_Floor(expression, operationType);
				expression = Expression.Convert(expression, TypeGuru.longType);
			} else {
				expression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(receiver.Expression, esClass, selector, argArrayFor(divisorMO));
			}
			return new DynamicMetaObject(
				Expression.Convert(expression, TypeGuru.objectType), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass, divisorMO), 
				receiver.Value);
		}

		public DynamicMetaObject metaObjectToDivideToResidual(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject modulusMO) {
			// Implements standard binary message "\\ operand" 
			// Smalltalk: ^receiver - (receiver // operand * operand)
			// C-Lang: return receiver - ((long)Math.Floor(receiver / (double)operand) * operand);
			Expression expression;
			var receiverType = receiver.LimitType;
			var modulusType = modulusMO.LimitType;
			var nominalOperationType = receiverType.typeWithHighestNumericGenerality(modulusType);	
			if (nominalOperationType != null) {
				var operationType = nominalOperationType.isInteger() ? TypeGuru.doubleType : nominalOperationType;
				var self = Expression.Convert(receiver.asExpressionWithFormalType(), TypeGuru.doubleType);
				var modulus = Expression.Convert(modulusMO.asExpressionWithFormalType(), TypeGuru.doubleType);
				expression = Expression.Divide(self, modulus);
				expression = ExpressionTreeGuru.expressionToInvoke_Math_Floor(expression, TypeGuru.doubleType);
				expression = Expression.Convert(expression, TypeGuru.longType);
				expression = Expression.Multiply(Expression.Convert(expression, TypeGuru.doubleType), modulus);
				expression = Expression.Subtract(self, expression);
				expression = Expression.Convert(expression, nominalOperationType);
			} else {
				expression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(receiver.Expression, esClass, selector, argArrayFor(modulusMO));
			}
			return new DynamicMetaObject(
				Expression.Convert(expression, TypeGuru.objectType), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass, modulusMO), 
				receiver.Value);
		}

		public DynamicMetaObject metaObjectForTimesRepeat(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject actionToRepeat) {
			Expression expression;
			BindingRestrictions restrictions;
			var model = receiver.Value;
			var self = receiver.Expression;
			var receiverType = receiver.LimitType;

			if (receiverType.isInteger()) {
				var actionFunction = actionToRepeat.BindInvoke(canonicalInvokeBinderFor(kernel.classOf(actionToRepeat.Value), selectorValue0), emptyArgArray);
				var induction = Expression.Parameter(TypeGuru.longType, "$induction");
				var step = Expression.Constant(1L);
				var limit = Expression.Convert(self, TypeGuru.longType);
				var exit = Expression.Label(TypeGuru.objectType);
				expression = Expression.Block(
						TypeGuru.objectType,
						new[] {induction},
						Expression.Assign(induction, Expression.Constant(0L)),
						Expression.Loop(
							Expression.IfThenElse(
								Expression.LessThan(induction, limit),
									Expression.Block(Expression.AddAssign(induction, step), actionFunction.Expression),
									Expression.Break(exit, self)),
						       exit));
				restrictions = receiver.bindingRestrictionsForForeignObjectReceiver(esClass).Merge(actionToRepeat.addingInstanceRestriction());
			} else {
				expression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(self, esClass, selector, new DynamicMetaObject[]{actionToRepeat});
				restrictions = receiver.bindingRestrictionsForForeignObjectReceiver(esClass);
			}

			return new DynamicMetaObject(
				expression, 
				restrictions, 
				model);

		}

		public Expression expressionForToByDo(bool convertOneBasedIndicesToZeroBased, DynamicMetaObject startingValueMO, DynamicMetaObject endingValueMO, DynamicMetaObject stepValueMO, ParameterExpression inductionParameter, Expression enumeratorExpression) {
			Expression expression;
			var startingValue = startingValueMO.asExpressionWithFormalType();
			var startingValueType = startingValueMO.LimitType;
			var endingValue = endingValueMO.asExpressionWithFormalType();
			var endingValueType = endingValueMO.LimitType;
			var stepValue = stepValueMO.Expression;
			var stepValueType = stepValueMO.LimitType;
			var typeWithHighestGenerality = endingValueType.typeWithHighestNumericGenerality(stepValueType);
			if (typeWithHighestGenerality == null) {
				typeWithHighestGenerality = startingValueType.typeWithHighestNumericGenerality(stepValueType);
				if (typeWithHighestGenerality == null) {
					typeWithHighestGenerality = startingValueType.typeWithHighestNumericGenerality(endingValueType);
					if (typeWithHighestGenerality == null) typeWithHighestGenerality = startingValueType;
				}
			}
			var candidateType = typeWithHighestGenerality.typeWithHighestNumericGenerality(startingValueType);
	
			if (candidateType == null) return null;

			if (convertOneBasedIndicesToZeroBased) {
				startingValue = Expression.Subtract(startingValue, Expression.Convert(Expression.Constant(1), startingValueType));
				endingValue = Expression.Subtract(endingValue, Expression.Convert(Expression.Constant(1), endingValueType));
			}

			typeWithHighestGenerality = candidateType;
			var induction = Expression.Parameter(typeWithHighestGenerality, "$induction");
			Expression inductionVariable;
			if (inductionParameter.Type != typeWithHighestGenerality) {
				inductionVariable = Expression.Convert(induction, inductionParameter.Type);
			} else {
				inductionVariable = induction;
			}
			var generalStartingValue = Expression.Convert(startingValue, typeWithHighestGenerality);
			var generalEndingValue = Expression.Convert(endingValue, typeWithHighestGenerality);
			var generalStepValue = Expression.Convert(stepValue, typeWithHighestGenerality);
			var upwardLoopExit = Expression.Label(typeWithHighestGenerality);
			var downwardLoopExit = Expression.Label(typeWithHighestGenerality);
			expression = Expression.Block(
				TypeGuru.objectType,
				new[] {induction, inductionParameter},
				Expression.Assign(induction, generalStartingValue),
				Expression.IfThenElse(
					Expression.GreaterThanOrEqual(generalStepValue, Expression.Convert(ExpressionTreeGuru.zeroConstant, typeWithHighestGenerality)),
						Expression.Loop(
							Expression.Condition(
								Expression.LessThanOrEqual(induction, generalEndingValue),
									Expression.Block(
										typeWithHighestGenerality, 
										Expression.Assign(inductionParameter, inductionVariable), 
										enumeratorExpression, 
										Expression.AddAssign(induction, generalStepValue)),
									Expression.Break(upwardLoopExit, induction, typeWithHighestGenerality)),
							upwardLoopExit),
						Expression.Loop(
							Expression.Condition(
								Expression.GreaterThanOrEqual(induction, generalEndingValue),
									Expression.Block(
										typeWithHighestGenerality, 
										Expression.Assign(inductionParameter, inductionVariable), 
										enumeratorExpression, 
										Expression.AddAssign(induction, generalStepValue)),
									Expression.Break(downwardLoopExit, induction, typeWithHighestGenerality)),
							downwardLoopExit)),
					startingValueMO.Expression);
			return expression;
		}

		public DynamicMetaObject metaObjectForToByDo(DynamicMetaObject startingValueMO, ESBehavior esClass, ESSymbol selector, DynamicMetaObject endingValueMO, DynamicMetaObject stepValueMO, DynamicMetaObject enumeratorMO) {

			BindingRestrictions restrictions = null;
			var model = startingValueMO.Value;
			var startingValue = startingValueMO.Expression;
			var inductionVariable = Expression.Parameter(TypeGuru.objectType, "$inductionVariable");
			var inductionVariableMO = inductionVariable.asDynamicMetaObject();
			var enumerator = enumeratorMO.BindInvoke(canonicalInvokeBinderFor(kernel.classOf(enumeratorMO.Value), selectorValue1), argArrayFor(inductionVariableMO));
			var expression = 
				expressionForToByDo(
					false,
					startingValueMO, 
					endingValueMO, 
					stepValueMO, 
					inductionVariable,
					enumerator.Expression);

			if (expression == null) {
				expression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(startingValue, esClass, selector, new DynamicMetaObject[]{endingValueMO, stepValueMO, enumeratorMO});
				restrictions = 
					startingValueMO.bindingRestrictionsForForeignObjectReceiver(esClass)
						.Merge(endingValueMO.addingFormalTypeRestriction())
						.Merge(stepValueMO.addingFormalTypeRestriction());
			} else {
				restrictions = 
					startingValueMO.bindingRestrictionsForForeignObjectReceiver(esClass)
						.Merge(endingValueMO.addingFormalTypeRestriction())
						.Merge(stepValueMO.addingFormalTypeRestriction())
						.Merge(enumeratorMO.addingInstanceRestriction());
			}

			return new DynamicMetaObject(expression, restrictions, model);

		}

		public DynamicMetaObject metaObjectToInvokeWithEnsureBlock(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject ensureBlockMO) {
			var invokeProtectedBlockMO = receiver.BindInvoke(canonicalInvokeBinderFor(esClass, selectorValue0), emptyArgArray);
			var invokeEnsureBlockMO = ensureBlockMO.BindInvoke(canonicalInvokeBinderFor(kernel.classOf(ensureBlockMO.Value), selectorValue0), emptyArgArray);
			Expression expression = Expression.TryFinally(invokeProtectedBlockMO.Expression, invokeEnsureBlockMO.Expression);
			expression = Expression.Block(TypeGuru.objectType, Expression.Convert(expression, TypeGuru.objectType));
			return new DynamicMetaObject(
				expression, 
				(esClass.InstanceArchitecture == ObjectStateArchitecture.HostSystemObject ?
					receiver.bindingRestrictionsForForeignObjectReceiver(esClass) :
					receiver.bindingRestrictionsForESObjectReceiver(esClass)).Merge(ensureBlockMO.addingInstanceRestriction()), 
				receiver.Value);
		}

		public DynamicMetaObject metaObjectToInvokeWithIfCurtailedBlock(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject ifCurtailedBlockMO) {
			var invokeProtectedBlockMO = receiver.BindInvoke(canonicalInvokeBinderFor(esClass, selectorValue0), emptyArgArray);
			var invokeIfCurtailedBlockMO = ifCurtailedBlockMO.BindInvoke(canonicalInvokeBinderFor(kernel.classOf(ifCurtailedBlockMO.Value), selectorValue0), emptyArgArray);
			var catchBlock = Expression.Catch(TypeGuru.exceptionType, Expression.Block(invokeIfCurtailedBlockMO.Expression, Expression.Rethrow(), Expression.Constant(new Object())));
			Expression expression = Expression.TryCatch(invokeProtectedBlockMO.Expression, catchBlock);
			expression = Expression.Block(TypeGuru.objectType, Expression.Convert(expression, TypeGuru.objectType));
			return new DynamicMetaObject(
				expression, 
				(esClass.InstanceArchitecture == ObjectStateArchitecture.HostSystemObject ?
					receiver.bindingRestrictionsForForeignObjectReceiver(esClass) :
					receiver.bindingRestrictionsForESObjectReceiver(esClass)).Merge(ifCurtailedBlockMO.addingInstanceRestriction()), 
				receiver.Value);
		}

		public DynamicMetaObject metaObjectToInvokeWithExceptionHandler(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject exceptionSelectorMO, DynamicMetaObject handleExceptionBlockMO) {
			var invokeProtectedBlockMO = receiver.BindInvoke(canonicalInvokeBinderFor(esClass, selectorValue0), emptyArgArray);
			ParameterExpression exception;
			DynamicMetaObject exceptionMO;
			DynamicMetaObject [] exceptionArgArray;
			DynamicMetaObject invokeIfCurtailedBlockMO;
			CatchBlock catchBlock;
			var exceptionSelectorClasss = kernel.classOf(exceptionSelectorMO.Value);
			if (exceptionSelectorClasss.IsHostSystemMetaclass) {
				var exceptionClass = ((ESMetaclass)exceptionSelectorClasss).CanonicalInstance;
				var exceptionType = exceptionClass.InstanceType;
				exception = Expression.Parameter(exceptionType, "ex");
				exceptionMO = exception.asDynamicMetaObject();
				exceptionArgArray = argArrayFor(exceptionMO);
				invokeIfCurtailedBlockMO = handleExceptionBlockMO.BindInvoke(canonicalInvokeBinderFor(kernel.classOf(handleExceptionBlockMO.Value), selectorValue1), exceptionArgArray);
				catchBlock = Expression.Catch(exception, invokeIfCurtailedBlockMO.Expression);
			} else { 
				exception = Expression.Parameter(TypeGuru.exceptionType, "ex");
				exceptionMO = exception.asDynamicMetaObject();
				exceptionArgArray = argArrayFor(exceptionMO);
				invokeIfCurtailedBlockMO = handleExceptionBlockMO.BindInvoke(canonicalInvokeBinderFor(kernel.classOf(handleExceptionBlockMO.Value), selectorValue1), exceptionArgArray);
				var exceptionSelectionPredicateMO = exceptionSelectorMO.BindInvokeMember(
									canonicalInvokeMemberBinderFor(
										exceptionSelectorClasss, 
										kernel.symbolFor("handles:"), 
										"Handles"), 
									exceptionArgArray);
				var exceptionSelectionPredicate = Expression.Convert(Expression.Convert(exceptionSelectionPredicateMO.Expression, TypeGuru.objectType), TypeGuru.boolType);
				var conditionalExceptionHandler = Expression.Condition(
										exceptionSelectionPredicate,
											Expression.Convert(invokeIfCurtailedBlockMO.Expression, TypeGuru.objectType),
											Expression.Block(TypeGuru.objectType, Expression.Rethrow(), Expression.Constant(new Object())));
				catchBlock = Expression.Catch(exception, conditionalExceptionHandler);
			}
			Expression expression = Expression.TryCatch(invokeProtectedBlockMO.Expression, catchBlock);
			expression = Expression.Block(TypeGuru.objectType, Expression.Convert(expression, TypeGuru.objectType));
			return new DynamicMetaObject(
				expression, 
				(esClass.InstanceArchitecture == ObjectStateArchitecture.HostSystemObject ?
					receiver.bindingRestrictionsForForeignObjectReceiver(esClass) :
					receiver.bindingRestrictionsForESObjectReceiver(esClass))
							.Merge(handleExceptionBlockMO.addingInstanceRestriction())
							.Merge(exceptionSelectorMO.addingInstanceRestriction())
							.Merge(handleExceptionBlockMO.addingInstanceRestriction()), 
				receiver.Value);


		}

		public DynamicMetaObject metaObjectForWhileNil(DynamicMetaObject receiver, ESBehavior esClass, DynamicMetaObject actionToRepeat) {
			var model = receiver.Value;
			var self = receiver.Expression;

			var testFunction = receiver.BindInvoke(canonicalInvokeBinderFor(esClass, selectorValue0), emptyArgArray);
			var actionFunction = actionToRepeat == null ?
				doNothing :
				actionToRepeat.BindInvoke(canonicalInvokeBinderFor(kernel.classOf(actionToRepeat.Value), selectorValue0), emptyArgArray);
			var exit = Expression.Label(TypeGuru.objectType);
			var loop = Expression.Loop(
					Expression.IfThenElse(
						Expression.ReferenceEqual(testFunction.Expression, ExpressionTreeGuru.nilConstant),
							actionFunction.Expression,
							Expression.Break(exit, self)),
					exit);

			return new DynamicMetaObject(
				loop, 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass).Merge(actionToRepeat.addingInstanceRestriction()), 
				model);
		}

		public DynamicMetaObject metaObjectForWhileNotNil(DynamicMetaObject receiver, ESBehavior esClass, DynamicMetaObject actionToRepeat) {
			var model = receiver.Value;
			var self = receiver.Expression;

			var testFunction = receiver.BindInvoke(canonicalInvokeBinderFor(esClass, selectorValue0), emptyArgArray);
			var actionFunction = actionToRepeat == null ?
				doNothing :
				actionToRepeat.BindInvoke(canonicalInvokeBinderFor(kernel.classOf(actionToRepeat.Value), selectorValue0), emptyArgArray);
			var exit = Expression.Label(TypeGuru.objectType);
			var loop = Expression.Loop(
					Expression.IfThenElse(
						Expression.ReferenceEqual(testFunction.Expression, ExpressionTreeGuru.nilConstant),
							Expression.Break(exit, self),
							actionFunction.Expression),
					exit);

			return new DynamicMetaObject(
				loop, 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass).Merge(actionToRepeat.addingInstanceRestriction()), 
				model);
		}

		public DynamicMetaObject metaObjectForWhileTrue(DynamicMetaObject receiver, ESBehavior esClass, DynamicMetaObject actionToRepeat) {
			var model = receiver.Value;
			var self = receiver.Expression;

			var testFunction = receiver.BindInvoke(canonicalInvokeBinderFor(esClass, selectorValue0), emptyArgArray);
			var actionFunction = actionToRepeat == null ?
				doNothing :
				actionToRepeat.BindInvoke(canonicalInvokeBinderFor(kernel.classOf(actionToRepeat.Value), selectorValue0), emptyArgArray);
			var exit = Expression.Label(TypeGuru.objectType);
			var loop = Expression.Loop(
					Expression.IfThenElse(
						Expression.Convert(testFunction.Expression, TypeGuru.boolType),
							actionFunction.Expression,
							Expression.Break(exit, self)),
					exit);

			return new DynamicMetaObject(
				loop, 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass).Merge(actionToRepeat.addingInstanceRestriction()), 
				model);
		}

		public DynamicMetaObject metaObjectForWhileFalse(DynamicMetaObject receiver, ESBehavior esClass, DynamicMetaObject actionToRepeat) {
			var model = receiver.Value;
			var self = receiver.Expression;

			var testFunction = receiver.BindInvoke(canonicalInvokeBinderFor(esClass, selectorValue0), emptyArgArray);
			var actionFunction = actionToRepeat == null ?
				doNothing :
				actionToRepeat.BindInvoke(canonicalInvokeBinderFor(kernel.classOf(actionToRepeat.Value), selectorValue0), emptyArgArray);
			var exit = Expression.Label(TypeGuru.objectType);
			var loop = Expression.Loop(
					Expression.IfThenElse(
						Expression.Convert(testFunction.Expression, TypeGuru.boolType),
							Expression.Break(exit, self),
							actionFunction.Expression),
						exit);

			return new DynamicMetaObject(
				loop, 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass).Merge(actionToRepeat.addingInstanceRestriction()), 
				model);
		}

		public DynamicMetaObject metaObjectForDo(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject enumeratorBlockMO) {
			var model = receiver.Value;
			var self = receiver.asExpressionWithFormalType();
			var receiverType = receiver.LimitType;
			Type elementType;
			ParameterExpression element;
			DynamicMetaObject elementMO;
			Expression invokeEnumeratorBlock;
			var exit = Expression.Label(TypeGuru.objectType);
			BlockExpression block = null;

			if (receiverType.IsArray) {
				elementType = receiverType.GetElementType();
				element = Expression.Parameter(elementType, "$element");
				elementMO = element.asDynamicMetaObject();
				invokeEnumeratorBlock = enumeratorBlockMO.BindInvoke(canonicalInvokeBinderFor(kernel.classOf(enumeratorBlockMO.Value), selectorValue1), argArrayFor(elementMO)).Expression;
				var induction = Expression.Parameter(TypeGuru.intType, "$induction");
				var getElement = Expression.ArrayAccess(self, induction);
				var assignEelement = Expression.Assign(element, getElement);
				var endingValue = Expression.Parameter(TypeGuru.intType, "$endingValue");
				var stepValue = Expression.Constant(1);
				var getLength = Expression.ArrayLength(self);
				block = Expression.Block(
					TypeGuru.objectType,
					new[] {induction, endingValue, element},
					Expression.Assign(induction, Expression.Constant(0)),
					Expression.Assign(endingValue, getLength),
					Expression.Loop(
						Expression.IfThenElse(
							Expression.LessThan(induction, endingValue),
								Expression.Block(
									assignEelement,
									Expression.AddAssign(induction, stepValue), 
									invokeEnumeratorBlock),
								Expression.Break(exit, self)),
					       exit));
			} else if (receiverType.implementsInterface(typeof(IDictionary<,>)) || receiverType.implementsInterface(typeof(IDictionary))) {
				var getEnumeratorMethod = receiverType.GetMethod("GetEnumerator", BindingFlags.Instance | BindingFlags.InvokeMethod | BindingFlags.Public, Type.DefaultBinder, TypeGuru.emptyTypeArray, null);
				var enumeratorType = getEnumeratorMethod.ReturnType;
				var valueType = TypeGuru.objectType;
				var value = Expression.Parameter(valueType, "$value");
				var valueMO = value.asDynamicMetaObject();
				invokeEnumeratorBlock = enumeratorBlockMO.BindInvoke(canonicalInvokeBinderFor(kernel.classOf(enumeratorBlockMO.Value), selectorValue1), argArrayFor(valueMO)).Expression;

				var currentProperty = enumeratorType.GetProperty("Current", BindingFlags.Instance | BindingFlags.GetProperty | BindingFlags.Public);
				var keyValuePairType = currentProperty.PropertyType;
				// var keyValuePairArgs = keyValuePairType.GetGenericArguments();

				var enumerator = Expression.Parameter(enumeratorType, "$enumerator");
				var assignEnumerator = Expression.Assign(enumerator, Expression.Call(self, getEnumeratorMethod));
				Expression getKeyValuePair = Expression.Property(enumerator, currentProperty);
				var keyValuePair = Expression.Parameter(keyValuePairType, "$keyValuePair");
				var assignKeyValuePair = Expression.Assign(keyValuePair, getKeyValuePair);
				Expression getValue = Expression.Property(keyValuePair, "Value");
				if (getValue.Type != TypeGuru.objectType) getValue = Expression.Convert(getValue, TypeGuru.objectType);
				var assignValue = Expression.Assign(value, getValue);
				var moveNext = Expression.Call(enumerator, enumeratorType.GetMethod("MoveNext"));

				block = Expression.Block(
						TypeGuru.objectType,
						new ParameterExpression[] {enumerator, keyValuePair, value},
						assignEnumerator,
						Expression.Loop(
						    Expression.IfThenElse(
							Expression.NotEqual(moveNext, ExpressionTreeGuru.falseConstant),
								Expression.Block(
									assignKeyValuePair,
									assignValue,
									invokeEnumeratorBlock),
								Expression.Break(exit, self)),
							exit));
			} else if (receiverType.implementsInterface(typeof(IEnumerable<>)) || receiverType.implementsInterface(typeof(IEnumerable))) {
				var getEnumeratorMethod = receiverType.GetMethod("GetEnumerator", BindingFlags.Instance | BindingFlags.InvokeMethod | BindingFlags.Public, Type.DefaultBinder, TypeGuru.emptyTypeArray, null);
				var enumeratorType = getEnumeratorMethod.ReturnType;
				elementType = TypeGuru.objectType;
				element = Expression.Parameter(elementType, "$element");
				elementMO = element.asDynamicMetaObject();
				invokeEnumeratorBlock = enumeratorBlockMO.BindInvoke(canonicalInvokeBinderFor(kernel.classOf(enumeratorBlockMO.Value), selectorValue1), argArrayFor(elementMO)).Expression;

				var enumerator = Expression.Parameter(enumeratorType, "$enumerator");
				var assignEnumerator = Expression.Assign(enumerator, Expression.Call(self, getEnumeratorMethod));
				Expression getElement = Expression.Property(enumerator, "Current");
				if (getElement.Type != TypeGuru.objectType) getElement = Expression.Convert(getElement, TypeGuru.objectType);
				var assignElement = Expression.Assign(element, getElement);
				var moveNext = Expression.Call(enumerator, enumeratorType.GetMethod("MoveNext"));

				block = Expression.Block(
						TypeGuru.objectType,
						new ParameterExpression[] {enumerator, element},
						assignEnumerator,
						Expression.Loop(
						    Expression.IfThenElse(
							Expression.NotEqual(moveNext, ExpressionTreeGuru.falseConstant),
								Expression.Block(
									assignElement,
									invokeEnumeratorBlock),
								Expression.Break(exit, self)),
							exit));
			} else {
				return metaObjectToSendDoesNotUnderstandToForeignObject(receiver, esClass, selector, argArrayFor(enumeratorBlockMO));
			}

			return new DynamicMetaObject(
				block, 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass).Merge(enumeratorBlockMO.addingInstanceRestriction()), 
				model);

		}

		public DynamicMetaObject metaObjectForFromToByDo(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject startingValueMO, DynamicMetaObject endingValueMO, DynamicMetaObject stepValueMO, DynamicMetaObject enumeratorBlockMO) {
			var model = receiver.Value;
			var self = receiver.asExpressionWithFormalType();
			var receiverType = receiver.LimitType;
			Type inductionVarType;
			ParameterExpression inductionVariable;
			Type elementType;
			ParameterExpression element;
			DynamicMetaObject elementMO;
			Expression getElement;
			Expression expression;
			BindingRestrictions restrictions = null;

			var startingValue = startingValueMO.Expression;

			if (receiverType.IsArray) {

				inductionVariable = Expression.Parameter(TypeGuru.intType, "$inductionVariable");
				elementType = receiverType.GetElementType();
				getElement = Expression.ArrayAccess(self, inductionVariable);

			} else {

				var methodInfo = receiverType.GetMethod("get_Item", esClass.HostObjectMethodInvokeBindingFlags);
				if (methodInfo == null) {
					inductionVariable = Expression.Parameter(TypeGuru.intType, "$inductionVariable");
					var indexerMethods = TypeGuru.indexerMethodsOf(receiverType, esClass.HostObjectPropertyGetBindingFlags);
					List<DynamicMetaObject> typedArguments;
					if (!selectMethodAndTypeCompatibleArgumentsFor(indexerMethods.ToArray(), argArrayFor(inductionVariable.asDynamicMetaObject()), out methodInfo, out typedArguments)) {
						return metaObjectToSendDoesNotUnderstandToForeignObject(receiver, esClass, selector, new DynamicMetaObject[]{startingValueMO, endingValueMO, stepValueMO, enumeratorBlockMO});
					}
				}

				var parameters = methodInfo.GetParameters();
				inductionVarType = parameters[0].ParameterType;
				inductionVariable = Expression.Parameter(inductionVarType, "$inductionVariable");
				elementType = methodInfo.ReturnType;
				getElement = Expression.Call(receiver.asExpressionWithFormalType(), methodInfo, inductionVariable);
			
			}

			element = Expression.Parameter(elementType, "$element");
			elementMO = element.asDynamicMetaObject();

			var assignEelement = Expression.Assign(element, getElement);
			var enumerator = enumeratorBlockMO.BindInvoke(canonicalInvokeBinderFor(kernel.classOf(enumeratorBlockMO.Value), selectorValue1), argArrayFor(elementMO));
			expression = 
				expressionForToByDo(
					true,
					startingValueMO, 
					endingValueMO, 
					stepValueMO, 
					inductionVariable,
					Expression.Block(
						TypeGuru.objectType,
						new[] {element},
						assignEelement,
						enumerator.Expression));

			if (expression == null) {
				expression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(startingValue, esClass, selector, new DynamicMetaObject[]{startingValueMO, endingValueMO, stepValueMO, enumeratorBlockMO});
				restrictions = 
					startingValueMO.bindingRestrictionsForForeignObjectReceiver(esClass)
						.Merge(startingValueMO.addingFormalTypeRestriction())
						.Merge(endingValueMO.addingFormalTypeRestriction())
						.Merge(stepValueMO.addingFormalTypeRestriction());
			} else {
				restrictions = 
					startingValueMO.bindingRestrictionsForForeignObjectReceiver(esClass)
						.Merge(startingValueMO.addingFormalTypeRestriction())
						.Merge(endingValueMO.addingFormalTypeRestriction())
						.Merge(stepValueMO.addingFormalTypeRestriction())
						.Merge(enumeratorBlockMO.addingInstanceRestriction());
			}

			return new DynamicMetaObject(expression, restrictions, model);

		}

		public DynamicMetaObject metaObjectForAssociationsDo(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject enumeratorBlockMO) {
			var model = receiver.Value;
			var self = receiver.asExpressionWithFormalType();
			var receiverType = receiver.LimitType;
			Type elementType;
			ParameterExpression element;
			DynamicMetaObject elementMO;
			Expression invokeEnumeratorBlock;
			var exit = Expression.Label(TypeGuru.objectType);
			BlockExpression block = null;

			if (receiverType.implementsInterface(typeof(IDictionary<,>)) || receiverType.implementsInterface(typeof(IDictionary))) {
				var getEnumeratorMethod = receiverType.GetMethod("GetEnumerator", esClass.HostObjectMethodInvokeBindingFlags, Type.DefaultBinder, TypeGuru.emptyTypeArray, null);
				var enumeratorType = getEnumeratorMethod.ReturnType;
				elementType = TypeGuru.objectType;
				element = Expression.Parameter(elementType, "$element");
				elementMO = element.asDynamicMetaObject();
				invokeEnumeratorBlock = enumeratorBlockMO.BindInvoke(canonicalInvokeBinderFor(kernel.classOf(enumeratorBlockMO.Value), selectorValue1), argArrayFor(elementMO)).Expression;

				var enumerator = Expression.Parameter(enumeratorType, "$enumerator");
				var assignEnumerator = Expression.Assign(enumerator, Expression.Call(self, getEnumeratorMethod));
				Expression getElement = Expression.Property(enumerator, "Current");
				if (getElement.Type != TypeGuru.objectType) getElement = Expression.Convert(getElement, TypeGuru.objectType);
				var assignElement = Expression.Assign(element, getElement);
				var moveNext = Expression.Call(enumerator, enumeratorType.GetMethod("MoveNext"));

				block = Expression.Block(
						TypeGuru.objectType,
						new ParameterExpression[] {enumerator, element},
						assignEnumerator,
						Expression.Loop(
						    Expression.IfThenElse(
							Expression.NotEqual(moveNext, ExpressionTreeGuru.falseConstant),
								Expression.Block(
									assignElement,
									invokeEnumeratorBlock),
								Expression.Break(exit, self)),
							exit));
			} else {
				return metaObjectToSendDoesNotUnderstandToForeignObject(receiver, esClass, selector, argArrayFor(enumeratorBlockMO));
			}

			return new DynamicMetaObject(
				block, 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass).Merge(enumeratorBlockMO.addingInstanceRestriction()), 
				model);

		}

		public DynamicMetaObject metaObjectForKeysDo(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject enumeratorBlockMO) {
			var model = receiver.Value;
			var self = receiver.asExpressionWithFormalType();
			var receiverType = receiver.LimitType;
			Type keyType;
			ParameterExpression key;
			DynamicMetaObject keyMo;
			Expression invokeEnumeratorBlock;
			var exit = Expression.Label(TypeGuru.objectType);
			BlockExpression block = null;

			if (receiverType.implementsInterface(typeof(IDictionary<,>)) || receiverType.implementsInterface(typeof(IDictionary))) {
				var getEnumeratorMethod = receiverType.GetMethod("GetEnumerator", esClass.HostObjectMethodInvokeBindingFlags, Type.DefaultBinder, TypeGuru.emptyTypeArray, null);
				var enumeratorType = getEnumeratorMethod.ReturnType;
				keyType = TypeGuru.objectType;
				key = Expression.Parameter(keyType, "$key");
				keyMo = key.asDynamicMetaObject();
				invokeEnumeratorBlock = enumeratorBlockMO.BindInvoke(canonicalInvokeBinderFor(kernel.classOf(enumeratorBlockMO.Value), selectorValue1), argArrayFor(keyMo)).Expression;

				var currentProperty = enumeratorType.GetProperty("Current", BindingFlags.Instance | BindingFlags.GetProperty | BindingFlags.Public);
				var keyValuePairType = currentProperty.PropertyType;
				// var keyValuePairArgs = keyValuePairType.GetGenericArguments();

				var enumerator = Expression.Parameter(enumeratorType, "$enumerator");
				var assignEnumerator = Expression.Assign(enumerator, Expression.Call(self, getEnumeratorMethod));
				Expression getKeyValuePair = Expression.Property(enumerator, currentProperty);
				var keyValuePair = Expression.Parameter(keyValuePairType, "$keyValuePair");
				var assignKeyValuePair = Expression.Assign(keyValuePair, getKeyValuePair);
				Expression getKey = Expression.Property(keyValuePair, "Key");
				if (getKey.Type != TypeGuru.objectType) getKey = Expression.Convert(getKey, TypeGuru.objectType);
				var assignKey = Expression.Assign(key, getKey);
				var moveNext = Expression.Call(enumerator, enumeratorType.GetMethod("MoveNext"));

				block = Expression.Block(
						TypeGuru.objectType,
						new ParameterExpression[] {enumerator, keyValuePair, key},
						assignEnumerator,
						Expression.Loop(
						    Expression.IfThenElse(
							Expression.NotEqual(moveNext, ExpressionTreeGuru.falseConstant),
								Expression.Block(
									assignKeyValuePair,
									assignKey,
									invokeEnumeratorBlock),
								Expression.Break(exit, self)),
							exit));
			} else {
				return metaObjectToSendDoesNotUnderstandToForeignObject(receiver, esClass, selector, argArrayFor(enumeratorBlockMO));
			}

			return new DynamicMetaObject(
				block, 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass).Merge(enumeratorBlockMO.addingInstanceRestriction()), 
				model);

		}

		public DynamicMetaObject metaObjectForKeysAndValuesDo(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject enumeratorBlockMO) {
			var model = receiver.Value;
			var self = receiver.asExpressionWithFormalType();
			var receiverType = receiver.LimitType;
			Type keyType;
			Type valueType;
			ParameterExpression key;
			ParameterExpression value;
			DynamicMetaObject keyMo;
			DynamicMetaObject valueMo;
			Expression invokeEnumeratorBlock;
			var exit = Expression.Label(TypeGuru.objectType);
			BlockExpression block = null;

			if (receiverType.implementsInterface(typeof(IDictionary<,>)) || receiverType.implementsInterface(typeof(IDictionary))) {
				var getEnumeratorMethod = receiverType.GetMethod("GetEnumerator", esClass.HostObjectMethodInvokeBindingFlags, Type.DefaultBinder, TypeGuru.emptyTypeArray, null);
				var enumeratorType = getEnumeratorMethod.ReturnType;
				keyType = TypeGuru.objectType;
				valueType = TypeGuru.objectType;
				key = Expression.Parameter(keyType, "$key");
				value = Expression.Parameter(keyType, "$value");
				keyMo = key.asDynamicMetaObject();
				valueMo = value.asDynamicMetaObject();
				invokeEnumeratorBlock = enumeratorBlockMO.BindInvoke(canonicalInvokeBinderFor(kernel.classOf(enumeratorBlockMO.Value), symbolFor("value:value:")), argArrayFor(keyMo, valueMo)).Expression;

				var currentProperty = enumeratorType.GetProperty("Current", BindingFlags.Instance | BindingFlags.GetProperty | BindingFlags.Public);
				var keyValuePairType = currentProperty.PropertyType;
				// var keyValuePairArgs = keyValuePairType.GetGenericArguments();

				var enumerator = Expression.Parameter(enumeratorType, "$enumerator");
				var assignEnumerator = Expression.Assign(enumerator, Expression.Call(self, getEnumeratorMethod));
				Expression getKeyValuePair = Expression.Property(enumerator, currentProperty);
				var keyValuePair = Expression.Parameter(keyValuePairType, "$keyValuePair");
				var assignKeyValuePair = Expression.Assign(keyValuePair, getKeyValuePair);
				Expression getKey = Expression.Property(keyValuePair, "Key");
				if (getKey.Type != TypeGuru.objectType) getKey = Expression.Convert(getKey, TypeGuru.objectType);
				Expression getValue = Expression.Property(keyValuePair, "Value");
				if (getValue.Type != TypeGuru.objectType) getValue = Expression.Convert(getValue, TypeGuru.objectType);
				var assignKey = Expression.Assign(key, getKey);
				var assignValue = Expression.Assign(value, getValue);
				var moveNext = Expression.Call(enumerator, enumeratorType.GetMethod("MoveNext"));

				block = Expression.Block(
						TypeGuru.objectType,
						new ParameterExpression[] {enumerator, keyValuePair, key, value},
						assignEnumerator,
						Expression.Loop(
						    Expression.IfThenElse(
							Expression.NotEqual(moveNext, ExpressionTreeGuru.falseConstant),
								Expression.Block(
									assignKeyValuePair,
									assignKey,
									assignValue,
									invokeEnumeratorBlock),
								Expression.Break(exit, self)),
							exit));
			} else {
				return metaObjectToSendDoesNotUnderstandToForeignObject(receiver, esClass, selector, argArrayFor(enumeratorBlockMO));
			}

			return new DynamicMetaObject(
				block, 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass).Merge(enumeratorBlockMO.addingInstanceRestriction()), 
				model);

		}

		public DynamicMetaObject metaObjectToConcatenateArrays(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject suffixMO) {
			Expression expression = null;
			var model = receiver.Value;
			var self = receiver.asExpressionWithFormalType();
			var esPrefix = self;
			var receiverType = receiver.LimitType;
			var esSuffix = suffixMO.asExpressionWithFormalType();
			if (receiverType.IsArray) {
				var suffixClass = kernel.classOf(suffixMO.Value);
				var elementType = receiverType.GetElementType();
				var prefixSize = receiver.BindUnaryOperation(canonicalUnaryOperationBinderFor(esClass, symbolFor("size"), ExpressionType.ArrayLength));
				var suffixSize = suffixMO.BindUnaryOperation(canonicalUnaryOperationBinderFor(suffixClass, symbolFor("size"), ExpressionType.ArrayLength));
				var newSize = prefixSize.BindBinaryOperation(canonicalBinaryOperationBinderFor(esClass, symbolFor("+"), ExpressionType.Add), suffixSize);
				var newArrayExpression = ExpressionTreeGuru.expressionToCreateArray(elementType, newSize.Expression);
				var newArray = newArrayExpression.asDynamicMetaObject();
				expression = Expression.Block(
					receiver.BindInvokeMember(canonicalInvokeMemberBinderFor(esClass, symbolFor("copyTo"), "CopyTo"), argArrayFor(newArray, zero)).Expression,
					suffixMO.BindInvokeMember(canonicalInvokeMemberBinderFor(suffixClass, symbolFor("copyTo"), "CopyTo"), argArrayFor(newArray, prefixSize)).Expression,
					newArray.Expression);
			} else if (receiverType == TypeGuru.stringType) {
				esPrefix = ExpressionTreeGuru.expressionToCreateESStringFromString(kernel, Expression.Convert(self, TypeGuru.stringType));
				expression = ExpressionTreeGuru.expressionToInvokeESMethod(kernel.StringClass.compiledMethodAt(symbolFor(",")), new Expression[]{esPrefix, esSuffix});
			} else {
				expression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(self, esClass, selector, argArrayFor(suffixMO));
			}
			return new DynamicMetaObject(
				Expression.Convert(expression, TypeGuru.objectType), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass, suffixMO), 
				receiver.Value);
		}

		public DynamicMetaObject metaObjectForAtIfAbsent(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject keyMO, DynamicMetaObject ifAbsentBlockMO) {
			var model = receiver.Value;
			var self = receiver.asExpressionWithFormalType();
			var receiverType = receiver.LimitType;
			Expression key;
			DynamicMetaObject typedKeyMO = null;
			MethodInfo methodInfo;

			/* NOTE: The following implementation is the best that can be achieved in .Net 3.5. It is possible to do better using .Net 4.0 */

			if (!(receiverType.implementsInterface(typeof(IDictionary<,>)))) {
				return metaObjectToSendDoesNotUnderstandToForeignObject(receiver, esClass, selector, argArrayFor(keyMO, ifAbsentBlockMO));
			}


			methodInfo = receiverType.GetMethod("get_Item", esClass.HostObjectMethodInvokeBindingFlags);
			if (methodInfo == null) {
				var indexerMethods = TypeGuru.indexerMethodsOf(receiverType, esClass.HostObjectPropertyGetBindingFlags);
				key = Expression.Parameter(TypeGuru.objectType, "key");
				List<DynamicMetaObject> typedArguments;
				if (!selectMethodAndTypeCompatibleArgumentsFor(indexerMethods.ToArray(), argArrayFor(keyMO), out methodInfo, out typedArguments)) {
					return metaObjectToSendDoesNotUnderstandToForeignObject(receiver, esClass, selector, new DynamicMetaObject[]{keyMO, ifAbsentBlockMO});
				}
				typedKeyMO = typedArguments[0];
			}

			var parameters = methodInfo.GetParameters();
			var keyType = parameters[0].ParameterType;
			var valueType = methodInfo.ReturnType;

			methodInfo = typeof(DynamicBindingGuru).GetMethod("dictionaryAtIfAbsent", BindingFlags.Static | BindingFlags.Public);
			methodInfo = methodInfo.MakeGenericMethod(keyType, valueType);

			if (typedKeyMO == null) {
				var keyGuru = keyMO.typeBindingGuru(kernel);
				typedKeyMO = keyGuru.metaObjectToConvertTo(keyType);
			}

			key = typedKeyMO.Expression;

			var ifAbsentFunctor = ExpressionTreeGuru.expressionToConvertESBlockToFunctor(ifAbsentBlockMO.Expression, 0);

			return new DynamicMetaObject(
				Expression.Call(methodInfo, self, key, ifAbsentFunctor), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass, keyMO, ifAbsentBlockMO), 
				model);
			
		}

		public DynamicMetaObject metaObjectForAtIfAbsentPut(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject keyMO, DynamicMetaObject ifAbsentValueBlockMO) {
			var model = receiver.Value;
			var self = receiver.asExpressionWithFormalType();
			var receiverType = receiver.LimitType;
			Expression key= Expression.Parameter(TypeGuru.objectType, "key");
			ParameterExpression value = Expression.Parameter(TypeGuru.objectType, "value");
			DynamicMetaObject valueMO = value.asDynamicMetaObject();
			DynamicMetaObject typedKeyMO = null;
			DynamicMetaObject typedValueMO = null;
			MethodInfo methodInfo;

			/* NOTE: The following implementation is the best that can be achieved in .Net 3.5. It is possible to do better using .Net 4.0 */

			if (!(receiverType.implementsInterface(typeof(IDictionary<,>)))) {
				return metaObjectToSendDoesNotUnderstandToForeignObject(receiver, esClass, selector, argArrayFor(keyMO, ifAbsentValueBlockMO));
			}


			var setItemMethodInfo = receiverType.GetMethod("set_Item", esClass.HostObjectMethodInvokeBindingFlags);
			if (setItemMethodInfo == null) {
				var indexerMethods = TypeGuru.indexerMethodsOf(receiverType, esClass.HostObjectPropertyGetBindingFlags);
				key = Expression.Parameter(TypeGuru.objectType, "key");
				List<DynamicMetaObject> typedArguments;
				if (!selectMethodAndTypeCompatibleArgumentsFor(indexerMethods.ToArray(), argArrayFor(keyMO, valueMO), out setItemMethodInfo, out typedArguments)) {
					return metaObjectToSendDoesNotUnderstandToForeignObject(receiver, esClass, selector, new DynamicMetaObject[]{keyMO, ifAbsentValueBlockMO});
				}
				typedKeyMO = typedArguments[0];
				typedKeyMO = typedArguments[1];
			}

			var parameters = setItemMethodInfo.GetParameters();
			var keyType = parameters[0].ParameterType;
			var valueType = parameters[1].ParameterType;

			methodInfo = typeof(DynamicBindingGuru).GetMethod("dictionaryAtIfAbsent", BindingFlags.Static | BindingFlags.Public);
			methodInfo = methodInfo.MakeGenericMethod(keyType, valueType);

			if (typedKeyMO == null) {
				var keyGuru = keyMO.typeBindingGuru(kernel);
				typedKeyMO = keyGuru.metaObjectToConvertTo(keyType);
			}
			if (typedValueMO == null) {
				var valueGuru = valueMO.typeBindingGuru(kernel);
				typedValueMO = valueGuru.metaObjectToConvertTo(valueType);
			}

			key = typedKeyMO.Expression;
			var typedValue = typedValueMO.asExpressionWithFormalType();

			var ifAbsentFunctor = ExpressionTreeGuru.expressionToConvertESBlockToFunctor(ifAbsentValueBlockMO.Expression, 0);
			var addIfAbsentExpression = 
				Expression.Block(
					TypeGuru.objectType,
					new ParameterExpression[]{value},
					Expression.Assign(value, Expression.Invoke(ifAbsentFunctor, ExpressionTreeGuru.emptyExpressionArray)),
					Expression.Call(self, setItemMethodInfo, new Expression[]{key, typedValue}),
					value);
			var lambda = Expression.Lambda<FuncNs.Func<Object>>(addIfAbsentExpression, false, new ParameterExpression[0]);

			return new DynamicMetaObject(
				Expression.Call(methodInfo, self, key, lambda), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass, keyMO, ifAbsentValueBlockMO), 
				model);
			
		}

		#region Sending #doesNotUnderstand:

		public DynamicMetaObject metaObjectToSendDoesNotUnderstandToESObject(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject[] argumentsWithoutReceiver) {
			return metaObjectToSendDoesNotUnderstand(receiver, esClass, selector, argumentsWithoutReceiver, receiver.bindingRestrictionsForESObjectReceiver(esClass));
		}

		public DynamicMetaObject metaObjectToSendDoesNotUnderstandToForeignObject(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject[] argumentsWithoutReceiver) {
			return metaObjectToSendDoesNotUnderstand(receiver, esClass, selector, argumentsWithoutReceiver, receiver.bindingRestrictionsForForeignObjectReceiver(esClass));
		}

		public DynamicMetaObject metaObjectToSendDoesNotUnderstand(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject[] argumentsWithoutReceiver, BindingRestrictions bindingRestrictions) {
			return new DynamicMetaObject(ExpressionTreeGuru.expressionToSendDoesNotUnderstand(receiver.Expression, esClass, selector, argumentsWithoutReceiver), bindingRestrictions, receiver.Value);
		}

		#endregion

		#endregion

		#region DynamicMetaObjects for general message sending

		public DynamicMetaObject metaObjectToSendMessageToESObject(DynamicMetaObject receiver, ESKernel kernel, ESBehavior esClass, ESSymbol selector, DynamicMetaObject[] metaObjectArgs) {

			ESMethod method;
			if (esClass == null) {
				method = null;
			} else {
				method = esClass.compiledMethodAt(selector);
				if (method == null) { 
					if (esClass.IsHostSystemMetaclass) {
						var name = selector.asHostSystemMemberName(CapitalizationScheme.InitialCapital);
						return receiver.BindInvokeMember(canonicalInvokeMemberBinderFor(esClass, selector, name), metaObjectArgs);
					}
					switch (selector.CanonicalSemantics) {
						case CanonicalSelectorSemantics.Ensure:
							return metaObjectToInvokeWithEnsureBlock(receiver, esClass, selector, metaObjectArgs[0]);
						case CanonicalSelectorSemantics.IfCurtailed:
							return metaObjectToInvokeWithIfCurtailedBlock(receiver, esClass, selector, metaObjectArgs[0]);
						case CanonicalSelectorSemantics.OnDo:
							return metaObjectToInvokeWithExceptionHandler(receiver, esClass, selector, metaObjectArgs[0], metaObjectArgs[1]);
					}
				}
			}

			return metaObjectToSendMessage(
					receiver, 
					kernel,
					esClass, 
					selector, 
					method,
					metaObjectArgs, 
					receiver.bindingRestrictionsForESObjectReceiver(esClass));

		}

		public DynamicMetaObject metaObjectToSendMessageToObject(DynamicMetaObject receiver, ESSymbol selector, DynamicMetaObject[] metaObjectArgs) {
			var model = receiver.Value;
			if (model == null) return metaObjectToSendMessageToNil(receiver, selector, metaObjectArgs);
			var esObject = model as ESObject;
			if (esObject == null) {
				return metaObjectToSendMessageToForeignObject(receiver, model, selector, metaObjectArgs);
			} else {
				return metaObjectToSendMessageToESObject(receiver, kernel, esObject.Class, selector, metaObjectArgs);
			}
		}

		#region Sending messages to special receivers

		#region Sending messages to nil

		public DynamicMetaObject metaObjectToSendMessageToNil(DynamicMetaObject receiver, ESSymbol selector, DynamicMetaObject[] metaObjectArgs) {
			var method = kernel.UndefinedObjectClass.compiledMethodAt(selector);
			return metaObjectToSendMessage(
					receiver, 
					kernel,
					kernel.UndefinedObjectClass, 
					selector, 
					method,
					metaObjectArgs, 
					ExpressionTreeGuru.expressionToTestThatNilHasSameClassVersion(receiver.Expression, kernel.UndefinedObjectClass).asBindingRestriction());
		}

		public DynamicMetaObject metaObjectToSendMessageToNilSelf(DynamicMetaObject receiver, ESSymbol selector, DynamicMetaObject[] metaObjectArgs) {
			var method = kernel.UndefinedObjectClass.compiledMethodAt(selector);
			return metaObjectToSendMessage(
					receiver, 
					kernel,
					kernel.UndefinedObjectClass, 
					selector, 
					method,
					metaObjectArgs, 
					ExpressionTreeGuru.expressionToTestThatClassHasSameClassVersion(kernel.UndefinedObjectClass).asBindingRestriction());
		}

		public DynamicMetaObject metaObjectToSendMessageToNilSuper(DynamicMetaObject receiver, ESSymbol selector, DynamicMetaObject[] metaObjectArgs) {
			var superclass = kernel.UndefinedObjectClass.Superclass;
			var method = superclass == null ? null : superclass.compiledMethodAt(selector);
			return metaObjectToSendMessage(
					receiver, 
					kernel,
					kernel.UndefinedObjectClass, 
					selector, 
					method,
					metaObjectArgs, 
					ExpressionTreeGuru.expressionToTestThatClassHasSameClassVersion(kernel.UndefinedObjectClass).asBindingRestriction());
		}

		#endregion

		#region Sending messages to self

		public DynamicMetaObject metaObjectToSendMessageToSelf(DynamicMetaObject receiver, ESSymbol selector, DynamicMetaObject[] metaObjectArgs) {
			var model = receiver.Value;
			if (model == null) return metaObjectToSendMessageToNilSelf(receiver, selector, metaObjectArgs);
			var esObject = model as ESObject;
			if (esObject == null) {
				return metaObjectToSendMessageToForeignSelf(receiver, selector, metaObjectArgs);
			} else {
				return metaObjectToSendMessageToESSelf(receiver, esObject.Class, selector, metaObjectArgs);
			}
		}

		public DynamicMetaObject metaObjectToSendMessageToESSelf(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject[] metaObjectArgs) {
			var method = esClass == null ? null : esClass.compiledMethodAt(selector);
			return metaObjectToSendMessage(
					receiver,
 					kernel,
					esClass, 
					selector, 
					method,
					metaObjectArgs, 
					receiver.bindingRestrictionsForESObjectReceiver(esClass));
		}

		public DynamicMetaObject metaObjectToSendMessageToForeignSelf(DynamicMetaObject receiver, ESSymbol selector, DynamicMetaObject[] metaObjectArgs) {
			var esClass = kernel.classOfHostSystemValue(receiver.Value);
			var method = esClass.compiledMethodAt(selector);
			if (method == null) {
				return metaObjectToSendSyntheticMessageToForeignObject(receiver, esClass, selector, metaObjectArgs);
			} else {
				return metaObjectToSendMessage(
						receiver,
 						kernel,
						esClass, 
						selector,
 						method,
						metaObjectArgs, 
						receiver.bindingRestrictionsForForeignObjectReceiver(esClass));
			}
		}

		#endregion

		#region Sending messages to super

		public DynamicMetaObject metaObjectToSendMessageToSuper(DynamicMetaObject receiver, ESSymbol selector, DynamicMetaObject[] metaObjectArgs) {
			var model = receiver.Value;
			if (model == null) return metaObjectToSendMessageToNilSuper(receiver, selector, metaObjectArgs);
			var esObject = model as ESObject;
			if (esObject == null) {
				return metaObjectToSendMessageToForeignSuper(receiver, selector, metaObjectArgs);
			} else {
				return metaObjectToSendMessageToESSuper(receiver, esObject.Class, selector, metaObjectArgs);
			}
		}

		public DynamicMetaObject metaObjectToSendMessageToESSuper(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject[] metaObjectArgs) {
			var superclass = esClass.Superclass;
			var method = superclass == null ? null : superclass.compiledMethodAt(selector);
			return metaObjectToSendMessage(
					receiver, 
					kernel,
					superclass, 
					selector, 
					method,
					metaObjectArgs, 
					receiver.bindingRestrictionsForESObjectReceiver(esClass));
		}

		public DynamicMetaObject metaObjectToSendMessageToForeignSuper(DynamicMetaObject receiver, ESSymbol selector, DynamicMetaObject[] metaObjectArgs) {
			var esClass = kernel.classOfHostSystemValue(receiver.Value);
			var superclass = esClass.Superclass;
			var method = superclass == null ? null : superclass.compiledMethodAt(selector);
			if (method == null) {
				return metaObjectToSendSyntheticMessageToForeignObject(receiver, esClass, selector, metaObjectArgs);
			} else {
				return metaObjectToSendMessage(
						receiver, 
						kernel,
						esClass, 
						selector, 
						method,
						metaObjectArgs, 
						receiver.bindingRestrictionsForForeignObjectReceiver(esClass));
			}
		}

		#endregion

		public DynamicMetaObject metaObjectToSendMessageToThisContext(DynamicMetaObject receiver, ESSymbol selector, DynamicMetaObject[] metaObjectArgs) {
			// @@ Handle thisContext
			var model = receiver.Value as ESObject;
			var esClass = model.Class;
			return metaObjectToSendMessage(
					receiver, 
					kernel,
					null,	// class
					selector, 
					null, // method
					metaObjectArgs, 
					receiver.bindingRestrictionsForESObjectReceiver(esClass));
		}

		#endregion

		#region Sending messages to foreign objects

		public DynamicMetaObject metaObjectToSendMessageToForeignObject(DynamicMetaObject receiver, Object model, ESSymbol selector, DynamicMetaObject[] metaObjectArgs) {
			var receiverType = model.GetType();
			var esClass = kernel.classOfHostSystemValue(model);
			if (receiverType.IsEnum) {
				var symbolMO 
					= new DynamicMetaObject(
						ExpressionTreeGuru.expressionToCreateESSymbolFromEnumerationContant(symbolRegistry, receiver.asExpressionWithFormalType()),
						receiver.Restrictions,
						receiver.Value);
				return metaObjectForForeignObjectOperation(
						receiver,
						esClass, 
						metaObjectToSendMessageToESObject(symbolMO, kernel, kernel.SymbolClass, selector, metaObjectArgs).Expression);
			}
			var method = esClass.compiledMethodAt(selector);
			if (method == null) {
				return metaObjectToSendSyntheticMessageToForeignObject(receiver, esClass, selector, metaObjectArgs);
			} else {
				return metaObjectToSendMessage(
						receiver, 
						kernel,
						esClass, 
						selector, 
						method,
						metaObjectArgs, 
						receiver.bindingRestrictionsForForeignObjectReceiver(esClass));
			}
		}

		public DynamicMetaObject metaObjectToSendSyntheticMessageToForeignObject(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject[] args) {

			switch (selector.CanonicalSemantics) {

				// Invariant operations:
				case CanonicalSelectorSemantics.IsIdenticalTo:
					return metaObjectForInvariantOperation(ExpressionTreeGuru.expressionToSendReferenceEquals(receiver.Expression, args[0].Expression), receiver.Value);
				case CanonicalSelectorSemantics.IsNotIdenticalTo:
					return metaObjectForInvariantOperation(ExpressionTreeGuru.expressionToSendReferenceNotEquals(receiver.Expression, args[0].Expression), receiver.Value);
				case CanonicalSelectorSemantics.IdentityHash:		
					return metaObjectForInvariantOperation(ExpressionTreeGuru.expressionToInvoke_RuntimeHelpers_GetHashCode(receiver.Expression), receiver.Value);
				case CanonicalSelectorSemantics.IsNil:
					return metaObjectForInvariantOperation(ExpressionTreeGuru.expressionToSendReferenceEquals(receiver.Expression, ExpressionTreeGuru.nilConstant), receiver.Value);
				case CanonicalSelectorSemantics.IsNotNil:
					return metaObjectForInvariantOperation(ExpressionTreeGuru.expressionToSendReferenceNotEquals(receiver.Expression, ExpressionTreeGuru.nilConstant), receiver.Value);

				// Receiver-type-dependent operations:
				case CanonicalSelectorSemantics.Yourself:
					return metaObjectForForeignObjectOperation(receiver, esClass, receiver.Expression);
				case CanonicalSelectorSemantics.Class:
					return metaObjectForForeignObjectOperation(receiver, esClass, Expression.Constant(esClass));
				case CanonicalSelectorSemantics.IsMemberOf:
					return metaObjectForForeignObjectOperation(receiver, esClass, ExpressionTreeGuru.expressionToSendReferenceEquals(Expression.Constant(esClass), args[0].Expression));
				case CanonicalSelectorSemantics.IsKindOf:
					var classExpression = Expression.Constant(esClass);
					var messageSendMO = metaObjectToSendMessageToESObject(classExpression.asDynamicMetaObject(), kernel, esClass.Class, symbolFor("includesBehavior:"), args);
					return messageSendMO.Expression.asDynamicMetaObject(receiver.restrictionThatForeignObjectHasSameClassVersion(esClass), receiver.Value);

				case CanonicalSelectorSemantics.AsBehavior:
				case CanonicalSelectorSemantics.AsClass:
					if (TypeGuru.typeType.IsAssignableFrom(receiver.LimitType)) { 
						return Expression.Constant(kernel.classForHostSystemType((Type)receiver.Value)).asDynamicMetaObject(receiver.addingInstanceRestriction().Merge(receiver.restrictionThatForeignObjectHasSameClassVersion(esClass)), receiver.Value);
					}
					break;
				case CanonicalSelectorSemantics.AsMetaclass:
					if (TypeGuru.typeType.IsAssignableFrom(receiver.LimitType)) { 
						return Expression.Constant(kernel.classForHostSystemType((Type)receiver.Value).Class).asDynamicMetaObject(receiver.addingInstanceRestriction().Merge(receiver.restrictionThatForeignObjectHasSameClassVersion(esClass)), receiver.Value);
					}
					break;

				case CanonicalSelectorSemantics.Ensure:
					return metaObjectToInvokeWithEnsureBlock(receiver, esClass, selector, args[0]);
				case CanonicalSelectorSemantics.IfCurtailed:
					return metaObjectToInvokeWithIfCurtailedBlock(receiver, esClass, selector, args[0]);
				case CanonicalSelectorSemantics.OnDo:
					return metaObjectToInvokeWithExceptionHandler(receiver, esClass, selector, args[0], args[1]);

				// GetIndexBinder:
				case CanonicalSelectorSemantics.At:
					return receiver.BindGetIndex(canonicalGetIndexBinderFor(esClass, selector), args);

				// SetIndexBinder;
				case CanonicalSelectorSemantics.AtPut:
					var index = argArrayFor(args[0]);
					var value = args[1];
					return receiver.BindSetIndex(canonicalSetIndexBinderFor(esClass, selector), index, value);

				// DeleteIndexBinder:
				case CanonicalSelectorSemantics.RemoveAt:
					return receiver.BindDeleteIndex(canonicalDeleteIndexBinderFor(esClass, selector), args);

				// UnaryOperationBinder:
				case CanonicalSelectorSemantics.Size:	
					if (receiver.LimitType.IsArray) {
						return metaObjectForForeignObjectOperation(receiver, esClass, Expression.ArrayLength(receiver.asExpressionWithFormalType()));
					} else {
						return receiver.BindInvokeMember(canonicalInvokeMemberBinderFor(esClass, selector, "Size"), args);
					}
				case CanonicalSelectorSemantics.Negated:
					return receiver.BindUnaryOperation(canonicalUnaryOperationBinderFor(esClass, selector, ExpressionType.Negate));
				case CanonicalSelectorSemantics.LogicalNot:
					return receiver.BindUnaryOperation(canonicalUnaryOperationBinderFor(esClass, selector, ExpressionType.Not));
				case CanonicalSelectorSemantics.IsTrue:
					return receiver.BindUnaryOperation(canonicalUnaryOperationBinderFor(esClass, selector, ExpressionType.IsTrue));
				case CanonicalSelectorSemantics.IsFalse:
					return receiver.BindUnaryOperation(canonicalUnaryOperationBinderFor(esClass, selector, ExpressionType.IsFalse));

				// BinaryOperationBinder:
				case CanonicalSelectorSemantics.IsEqualTo:
					return receiver.BindBinaryOperation(canonicalBinaryOperationBinderFor(esClass, selector, ExpressionType.Equal), args[0]);
				case CanonicalSelectorSemantics.IsNotEqualTo:
					return receiver.BindBinaryOperation(canonicalBinaryOperationBinderFor(esClass, selector, ExpressionType.NotEqual), args[0]);
				case CanonicalSelectorSemantics.LogicalAnd:
					return receiver.BindBinaryOperation(canonicalBinaryOperationBinderFor(esClass, selector, ExpressionType.And), args[0]);
				case CanonicalSelectorSemantics.LogicalOr:
					return receiver.BindBinaryOperation(canonicalBinaryOperationBinderFor(esClass, selector, ExpressionType.Or), args[0]);
				case CanonicalSelectorSemantics.LogicalXor:
					return receiver.BindBinaryOperation(canonicalBinaryOperationBinderFor(esClass, selector, ExpressionType.ExclusiveOr), args[0]);
				case CanonicalSelectorSemantics.IsLessThan:
					return receiver.BindBinaryOperation(canonicalBinaryOperationBinderFor(esClass, selector, ExpressionType.LessThan), args[0]);
				case CanonicalSelectorSemantics.IsGreaterThan:
					return receiver.BindBinaryOperation(canonicalBinaryOperationBinderFor(esClass, selector, ExpressionType.GreaterThan), args[0]);
				case CanonicalSelectorSemantics.IsLessThanOrEqual:
					return receiver.BindBinaryOperation(canonicalBinaryOperationBinderFor(esClass, selector, ExpressionType.LessThanOrEqual), args[0]);
				case CanonicalSelectorSemantics.IsGreaterThanOrEqual:
					return receiver.BindBinaryOperation(canonicalBinaryOperationBinderFor(esClass, selector, ExpressionType.GreaterThanOrEqual), args[0]);
				case CanonicalSelectorSemantics.IsZero:
					return receiver.BindBinaryOperation(canonicalBinaryOperationBinderFor(esClass, selector, ExpressionType.Equal), zero);
				case CanonicalSelectorSemantics.Positive:
					return receiver.BindBinaryOperation(canonicalBinaryOperationBinderFor(esClass, selector, ExpressionType.GreaterThanOrEqual), zero);
				case CanonicalSelectorSemantics.StrictlyPositive:
					return receiver.BindBinaryOperation(canonicalBinaryOperationBinderFor(esClass, selector, ExpressionType.GreaterThan), zero);
				case CanonicalSelectorSemantics.Negative:
					return receiver.BindBinaryOperation(canonicalBinaryOperationBinderFor(esClass, selector, ExpressionType.LessThan), zero);
				case CanonicalSelectorSemantics.Reciprocal:
					return one.BindBinaryOperation(canonicalBinaryOperationBinderFor(esClass, selector, ExpressionType.Divide), receiver);
				case CanonicalSelectorSemantics.Plus:
					return receiver.BindBinaryOperation(canonicalBinaryOperationBinderFor(esClass, selector, ExpressionType.Add), args[0]);
				case CanonicalSelectorSemantics.Minus:
					return receiver.BindBinaryOperation(canonicalBinaryOperationBinderFor(esClass, selector, ExpressionType.Subtract), args[0]);
				case CanonicalSelectorSemantics.Times:
					return receiver.BindBinaryOperation(canonicalBinaryOperationBinderFor(esClass, selector, ExpressionType.Multiply), args[0]);
				case CanonicalSelectorSemantics.DivideToRational:
					return receiver.BindBinaryOperation(canonicalBinaryOperationBinderFor(esClass, selector, ExpressionType.Divide), args[0]);
				case CanonicalSelectorSemantics.DivisionRemainder:
					return receiver.BindBinaryOperation(canonicalBinaryOperationBinderFor(esClass, selector, ExpressionType.Modulo), args[0]);
				case CanonicalSelectorSemantics.BitAnd:
					return receiver.BindBinaryOperation(canonicalBinaryOperationBinderFor(esClass, selector, ExpressionType.And), args[0]);
				case CanonicalSelectorSemantics.BitOr:
					return receiver.BindBinaryOperation(canonicalBinaryOperationBinderFor(esClass, selector, ExpressionType.Or), args[0]);
				case CanonicalSelectorSemantics.BitXor:
					return receiver.BindBinaryOperation(canonicalBinaryOperationBinderFor(esClass, selector, ExpressionType.ExclusiveOr), args[0]);
				case CanonicalSelectorSemantics.ShiftLeft:
					return receiver.BindBinaryOperation(canonicalBinaryOperationBinderFor(esClass, selector, ExpressionType.LeftShift), args[0]);
				case CanonicalSelectorSemantics.ShiftRight:
					return receiver.BindBinaryOperation(canonicalBinaryOperationBinderFor(esClass, selector, ExpressionType.RightShift), args[0]);

				// ConvertBinder:
				case CanonicalSelectorSemantics.AsCharacter:
					return receiver.BindConvert(canonicalConvertBinderFor(esClass, TypeGuru.charType));
				case CanonicalSelectorSemantics.AsInteger:
					return receiver.BindConvert(canonicalConvertBinderFor(esClass, TypeGuru.longType));
				case CanonicalSelectorSemantics.AsFloat:
					return receiver.BindConvert(canonicalConvertBinderFor(esClass, TypeGuru.floatType));
				case CanonicalSelectorSemantics.AsDouble:
					return receiver.BindConvert(canonicalConvertBinderFor(esClass, TypeGuru.doubleType));
				case CanonicalSelectorSemantics.AsQuad:
					return receiver.BindConvert(canonicalConvertBinderFor(esClass, TypeGuru.decimalType));
				case CanonicalSelectorSemantics.AsString:
					return metaObjectForForeignObjectOperation(receiver, esClass, ExpressionTreeGuru.expressionToCreateESStringFromNonESObject(kernel, receiver.Expression));
				case CanonicalSelectorSemantics.AsSymbol:
					return metaObjectForForeignObjectOperation(receiver, esClass, ExpressionTreeGuru.expressionToCreateESSymbolFromNonESObject(kernel, receiver.Expression));

				// InvokeBinder:
				case CanonicalSelectorSemantics.InvokeBlock:
					return receiver.BindInvoke(canonicalInvokeBinderFor(esClass, selector), args);

				// InvokeMemberBinder:
				case CanonicalSelectorSemantics.Hash:
					return receiver.BindInvokeMember(canonicalInvokeMemberBinderFor(esClass, selector, "GetHashCode"), args);
				case CanonicalSelectorSemantics.ShallowCopy:
					return receiver.BindInvokeMember(canonicalInvokeMemberBinderFor(esClass, selector, "MemberwiseClone"), args);
				case CanonicalSelectorSemantics.Copy:
					return receiver.BindInvokeMember(canonicalInvokeMemberBinderFor(esClass, selector, "Clone"), args);
				case CanonicalSelectorSemantics.CompareTo:
					var mo = receiver.BindInvokeMember(canonicalInvokeMemberBinderFor(esClass, selector, "CompareTo"), args);
					return metaObjectForForeignObjectOperation(
							receiver, 
							esClass, 
							ExpressionTreeGuru.expressionToComputeSignOf(
										Expression.Convert(mo.Expression, TypeGuru.intType), 
										TypeGuru.intType));
				case CanonicalSelectorSemantics.IsImmutable:
					return receiver.BindInvokeMember(canonicalInvokeMemberBinderFor(esClass, selector, "IsReadOnly"), args);
				case CanonicalSelectorSemantics.AsImmutable:
					return receiver.BindInvokeMember(canonicalInvokeMemberBinderFor(esClass, selector, "AsReadOnly"), args);

				// CreateInstanceBinder:
				case CanonicalSelectorSemantics.New:
					return receiver.BindCreateInstance(canonicalCreateInstanceBinderFor(esClass, selector), args);
				case CanonicalSelectorSemantics.NewWithSize:
					return receiver.BindCreateInstance(canonicalCreateInstanceBinderFor(esClass, selector), args);

				// Complex/Composable:

				case CanonicalSelectorSemantics.AsAssociationTo:
					return metaObjectToCreateAssociation(receiver, esClass, selector, args[0]);

				case CanonicalSelectorSemantics.ConditionalAnd:
					return metaObjectForConditionalAnd(receiver, esClass, args[0]);
				case CanonicalSelectorSemantics.ConditionalOr:
					return metaObjectForConditionalOr(receiver, esClass, args[0]);

				case CanonicalSelectorSemantics.IsBoolean:
					return metaObjectForForeignObjectOperation(receiver, esClass, Expression.TypeIs(receiver.asExpressionWithFormalType(), TypeGuru.boolType));
				case CanonicalSelectorSemantics.IfTrue:
					if (receiver.Value is bool) {
						return metaObjectToInvokeArgIfTrue(
							receiver,
							esClass,
							receiver.Expression,
							args[0],
							ExpressionTreeGuru.nilConstant);
					} else {
						return metaObjectToSendDoesNotUnderstandToForeignObject(receiver, esClass, selector, args);
					}
				case CanonicalSelectorSemantics.IfFalse:
					if (receiver.Value is bool) {
						return metaObjectToInvokeArgIfFalse(
							receiver,
							esClass,
							receiver.Expression,
							args[0],
							ExpressionTreeGuru.nilConstant);
					} else {
						return metaObjectToSendDoesNotUnderstandToForeignObject(receiver, esClass, selector, args);
					}
				case CanonicalSelectorSemantics.IfTrueIfFalse:
					if (receiver.Value is bool) {
						return metaObjectToInvokeArgIfTrueAndArgIfFalse(
							receiver,
							esClass,
							receiver.Expression,
							args[0],
							args[1]);
					} else {
						return metaObjectToSendDoesNotUnderstandToForeignObject(receiver, esClass, selector, args);
					}
				case CanonicalSelectorSemantics.IfFalseIfTrue:
					if (receiver.Value is bool) {
						return metaObjectToInvokeArgIfTrueAndArgIfFalse(
							receiver,
							esClass,
							receiver.Expression,
							args[1],
							args[0]);				
					} else {
						return metaObjectToSendDoesNotUnderstandToForeignObject(receiver, esClass, selector, args);
					}
				case CanonicalSelectorSemantics.IfNil:
					return metaObjectToInvokeArgIfTrue(
						receiver,
						esClass,
						ExpressionTreeGuru.expressionToSendReferenceEquals(receiver.Expression, ExpressionTreeGuru.nilConstant),
						args[0],
						receiver.Expression);
				case CanonicalSelectorSemantics.IfNotNil:
					return metaObjectToInvokeArgIfFalse(
						receiver,
						esClass,
						ExpressionTreeGuru.expressionToSendReferenceEquals(receiver.Expression, ExpressionTreeGuru.nilConstant),
						args[0],
						receiver.Expression);
				case CanonicalSelectorSemantics.IfNilIfNotNil:
					return metaObjectToInvokeArgIfTrueAndArgIfFalse(
						receiver,
						esClass,
						ExpressionTreeGuru.expressionToSendReferenceEquals(receiver.Expression, ExpressionTreeGuru.nilConstant),
						args[0],
						args[1]);
				case CanonicalSelectorSemantics.IfNotNilIfNil:
					return metaObjectToInvokeArgIfTrueAndArgIfFalse(
						receiver,
						esClass,
						ExpressionTreeGuru.expressionToSendReferenceNotEquals(receiver.Expression, ExpressionTreeGuru.nilConstant),
						args[0],
						args[1]);

				case CanonicalSelectorSemantics.Sign:
					return metaObjectToComputeSignOf(receiver, esClass, selector);
				case CanonicalSelectorSemantics.Abs:
					return metaObjectToComputeAbsoluteValueOf(receiver, esClass, selector);
				case CanonicalSelectorSemantics.Ceiling:
					return metaObjectToComputeCeilingOf(receiver, esClass, selector);
				case CanonicalSelectorSemantics.Floor:
					return metaObjectToComputeFloorOf(receiver, esClass, selector);
				case CanonicalSelectorSemantics.Rounded:
					return metaObjectToRoundANumber(receiver, esClass, selector);
				case CanonicalSelectorSemantics.Truncated:
					return metaObjectToTruncateANumber(receiver, esClass, selector);
				case CanonicalSelectorSemantics.RoundTo:
					return metaObjectToRoundANumberTo(receiver, esClass, selector, args[0]);
				case CanonicalSelectorSemantics.TruncateTo:
					return metaObjectToTruncateANumberTo(receiver, esClass, selector, args[0]);
				case CanonicalSelectorSemantics.RaisedToInteger:
				case CanonicalSelectorSemantics.RaisedTo:
					return metaObjectToRaiseANumberTo(receiver, esClass, selector, args[0]);
				case CanonicalSelectorSemantics.SquareRoot:
					return metaObjectToRaiseANumberTo(receiver, esClass, selector, oneHalf);
				case CanonicalSelectorSemantics.Log:
					return metaObjectToComputeLogarithm(receiver, esClass, selector, args[0]);
				case CanonicalSelectorSemantics.NaturalLog:
					return metaObjectToComputeNaturalLogarithm(receiver, esClass, selector);
				case CanonicalSelectorSemantics.DivideToInteger:
					// Selector = #quo:
					return metaObjectToDivideToInteger(receiver, esClass, selector, args[0]);
				case CanonicalSelectorSemantics.DivideFlooredToInteger:
					// Selector = #//
					return metaObjectToDivideAndFloorToInteger(receiver, esClass, selector, args[0]);
				case CanonicalSelectorSemantics.DivisionResidual:
					// Selector = #\\
					return metaObjectToDivideToResidual(receiver, esClass, selector, args[0]);
				case CanonicalSelectorSemantics.BitShift:
					var shiftExtent = args[0];
					var shiftExtentIsPositive = shiftExtent.BindBinaryOperation(canonicalBinaryOperationBinderFor(esClass, symbolFor(">="), ExpressionType.GreaterThanOrEqual), zero);
					var shiftExtentNegated = shiftExtent.BindUnaryOperation(canonicalUnaryOperationBinderFor(esClass, symbolFor("negated"), ExpressionType.Negate));
					var leftShift = receiver.BindBinaryOperation(canonicalBinaryOperationBinderFor(esClass, symbolFor("<<"), ExpressionType.LeftShift), shiftExtent);
					var rightShift = receiver.BindBinaryOperation(canonicalBinaryOperationBinderFor(esClass, symbolFor(">>"), ExpressionType.RightShift), shiftExtentNegated);
					var shiftExpression = Expression.Condition(
								Expression.Convert(shiftExtentIsPositive.Expression, TypeGuru.boolType), 
									Expression.Convert(leftShift.Expression, TypeGuru.objectType), 
									Expression.Convert(rightShift.Expression, TypeGuru.objectType));
					return new DynamicMetaObject(
							Expression.Convert(shiftExpression, TypeGuru.objectType),
							receiver.bindingRestrictionsForForeignObjectReceiver(esClass, shiftExtent),
							receiver.Value);

				case CanonicalSelectorSemantics.TimesRepeat:
					return metaObjectForTimesRepeat(receiver, esClass, selector, args[0]);

				case CanonicalSelectorSemantics.ToDo:
					return metaObjectForToByDo(receiver, esClass, selector, args[0], oneInt32, args[1]);
				case CanonicalSelectorSemantics.ToByDo:
					return metaObjectForToByDo(receiver, esClass, selector, args[0], args[1], args[2]);

				case CanonicalSelectorSemantics.WhileNil:
					return metaObjectForWhileNil(receiver, esClass, null);
				case CanonicalSelectorSemantics.WhileNotNil:
					return metaObjectForWhileNotNil(receiver, esClass, null);
				case CanonicalSelectorSemantics.WhileNilDo:
					return metaObjectForWhileNil(receiver, esClass, args[0]);
				case CanonicalSelectorSemantics.WhileNotNilDo:
					return metaObjectForWhileNotNil(receiver, esClass, args[0]);

				case CanonicalSelectorSemantics.WhileTrue:
					return metaObjectForWhileTrue(receiver, esClass, null);
				case CanonicalSelectorSemantics.WhileFalse:
					return metaObjectForWhileFalse(receiver, esClass, null);
				case CanonicalSelectorSemantics.WhileTrueDo:
					return metaObjectForWhileTrue(receiver, esClass, args[0]);
				case CanonicalSelectorSemantics.WhileFalseDo:
					return metaObjectForWhileFalse(receiver, esClass, args[0]);

				case CanonicalSelectorSemantics.Do:
					return metaObjectForDo(receiver, esClass, selector, args[0]);
				case CanonicalSelectorSemantics.AssociationsDo:
					return metaObjectForAssociationsDo(receiver, esClass, selector, args[0]);

				case CanonicalSelectorSemantics.FromToDo:
					return metaObjectForFromToByDo(
						receiver, 
						esClass, 
						selector, 
						args[0], 
						args[1], 
						oneInt32, 
						args[2]);
				case CanonicalSelectorSemantics.FromToByDo:
					return metaObjectForFromToByDo(
						receiver, 
						esClass, 
						selector, 
						args[0], 
						args[1], 
						args[2], 
						args[3]);

				case CanonicalSelectorSemantics.KeysDo:
					return metaObjectForKeysDo(receiver, esClass, selector, args[0]);
				case CanonicalSelectorSemantics.KeysAndValuesDo:
					return metaObjectForKeysAndValuesDo(receiver, esClass, selector, args[0]);

				case CanonicalSelectorSemantics.Concat:
					return metaObjectToConcatenateArrays(receiver, esClass, selector, args[0]);

				case CanonicalSelectorSemantics.AtIfAbsent:
					return metaObjectForAtIfAbsent(receiver, esClass, selector, args[0], args[1]);
				case CanonicalSelectorSemantics.AtIfAbsentPut:
					return metaObjectForAtIfAbsentPut(receiver, esClass, selector, args[0], args[1]);

				// Problematic:
				case CanonicalSelectorSemantics.Perform:		// Emit expression to get ESClass and use it to dynamically send message to instance
				case CanonicalSelectorSemantics.PerformWith:		// Emit expression to get ESClass and use it to dynamically send message to instance
				case CanonicalSelectorSemantics.PerformWithArguments:	// Emit expression to get ESClass and use it to dynamically send message to instance
					break;

				case CanonicalSelectorSemantics.Coerce:			// (x, y) => Convert.ChangeType(y, x.GetType())
					// Use TypeBindingGuru?;
					break;

				case CanonicalSelectorSemantics.InstVarValueAtName:
					// Same as <getField: name>, except <name> is dynamic?
				case CanonicalSelectorSemantics.InstVarValueAtNamePut:
					// Same as <setField: name>, except <name> is dynamic?
					break;

				default:
				case CanonicalSelectorSemantics.None:
					String name;
					switch (selector.NumArgs) {
						case 0:
							name = selector.asHostSystemMemberName(CapitalizationScheme.InitialCapital);
							return receiver.BindInvokeMember(canonicalInvokeMemberBinderFor(esClass, selector, name), args);
						case 1:
							if (selector.Type == SymbolType.BinaryMessageSelector) break; // If not handled above, a binary message selector can't be a valid host operator or method name.
							name = selector.asHostSystemMemberName(CapitalizationScheme.InitialCapital);
							return receiver.BindInvokeMember(canonicalInvokeMemberBinderFor(esClass, selector, name), args);
						default:
							name = selector.asHostSystemMemberName(CapitalizationScheme.InitialCapital);
							return receiver.BindInvokeMember(canonicalInvokeMemberBinderFor(esClass, selector, name), args);
					}
					
					break; // Yes, the C# compiler really IS that dumb....

			}

			return metaObjectToSendDoesNotUnderstandToForeignObject(receiver, esClass, selector, args);
		}

		#endregion

		#endregion

		#region Essence Sharp message sends to foreign/host-system target objects

		public class GetPropertyOrFieldOfForeignObjectBinder : GetMemberBinder {

			protected DynamicBindingGuru	dynamicBindingGuru	= null;
			protected ESKernel		kernel			= null;
			protected ESBehavior		esClass			= null;
			protected ESSymbol		selector		= null;

			protected GetPropertyOrFieldOfForeignObjectBinder(DynamicBindingGuru dynamicBindingGuru, ESBehavior esClass, ESSymbol selector) : base(selector.PrimitiveValue, false) { 
				this.dynamicBindingGuru	= dynamicBindingGuru;
				kernel = dynamicBindingGuru.Kernel;
				this.esClass		= esClass;
				this.selector		= selector;
			}

			public ESSymbol	Selector {
				get {return selector;}
			}

			public override DynamicMetaObject FallbackGetMember(DynamicMetaObject target, DynamicMetaObject errorSuggestion) {
				if (!target.HasValue) return Defer(target);

				var self = target.asExpressionWithFormalType();
				Expression getPropertyOrFieldExpression = null;

				if (esClass.getReadablePropertyOrElseField(
							Name, 
							(property) => getPropertyOrFieldExpression = Expression.Property(esClass.IsHostSystemMetaclass ? null : self, property), 
							(field) => getPropertyOrFieldExpression = Expression.Field(esClass.IsHostSystemMetaclass ? null : self, field))) {

					if (getPropertyOrFieldExpression.Type != TypeGuru.objectType) getPropertyOrFieldExpression = Expression.Convert(getPropertyOrFieldExpression, TypeGuru.objectType);
					return new DynamicMetaObject(
						getPropertyOrFieldExpression, 
						target.bindingRestrictionsForForeignObjectReceiver(esClass),
						target.Value);	
		
				} else if (Selector.CanonicalSemantics == CanonicalSelectorSemantics.Size) {
					if (esClass.getReadablePropertyOrElseField(
								"Count", 
								(property) => getPropertyOrFieldExpression = Expression.Property(esClass.IsHostSystemMetaclass ? null : self, property), 
								(field) => getPropertyOrFieldExpression = Expression.Field(esClass.IsHostSystemMetaclass ? null : self, field))) {

						if (getPropertyOrFieldExpression.Type != TypeGuru.objectType) getPropertyOrFieldExpression = Expression.Convert(getPropertyOrFieldExpression, TypeGuru.objectType);
						return new DynamicMetaObject(
							getPropertyOrFieldExpression, 
							target.bindingRestrictionsForForeignObjectReceiver(esClass),
							target.Value);	
		
					} else if (esClass.getReadablePropertyOrElseField(
								"Length", 
								(property) => getPropertyOrFieldExpression = Expression.Property(esClass.IsHostSystemMetaclass ? null : self, property), 
								(field) => getPropertyOrFieldExpression = Expression.Field(esClass.IsHostSystemMetaclass ? null : self, field))) {

						if (getPropertyOrFieldExpression.Type != TypeGuru.objectType) getPropertyOrFieldExpression = Expression.Convert(getPropertyOrFieldExpression, TypeGuru.objectType);
						return new DynamicMetaObject(
							getPropertyOrFieldExpression, 
							target.bindingRestrictionsForForeignObjectReceiver(esClass),
							target.Value);	
		
					} else {
						return target.BindInvokeMember(
								dynamicBindingGuru.canonicalInvokeMemberBinderFor(
									esClass, 
									Selector, 
									Selector.asHostSystemMemberName(CapitalizationScheme.InitialCapital)), 
								emptyArgArray);
					}
				}

				return new DynamicMetaObject( 
					ExpressionTreeGuru.expressionToSendDoesNotUnderstand(target.Expression, esClass, selector, emptyArgArray),
					target.bindingRestrictionsForForeignObjectReceiver(esClass),
					target.Value);	

			}

			public class Registry : BinderRegistry {

				protected readonly Dictionary<long, Dictionary<ESSymbol, GetPropertyOrFieldOfForeignObjectBinder>> registry = new Dictionary<long, Dictionary<ESSymbol, GetPropertyOrFieldOfForeignObjectBinder>>();

				public Registry(DynamicBindingGuru dynamicBindingGuru) : base(dynamicBindingGuru) {
				}

				public GetPropertyOrFieldOfForeignObjectBinder canonicalBinderFor(ESBehavior esClass, ESSymbol selector) {
					Dictionary<ESSymbol, GetPropertyOrFieldOfForeignObjectBinder> binderRegistry;
					GetPropertyOrFieldOfForeignObjectBinder binder;
					long classId = esClass.Identity;
					if (!registry.TryGetValue(classId, out binderRegistry)) {
						binderRegistry = new Dictionary<ESSymbol, GetPropertyOrFieldOfForeignObjectBinder>();
						registry[classId] = binderRegistry;
						binder = new GetPropertyOrFieldOfForeignObjectBinder(DynamicBindingGuru, esClass, selector);
						binderRegistry[selector] = binder;
						return binder;
					}
					if (!binderRegistry.TryGetValue(selector, out binder)) {
						binder = new GetPropertyOrFieldOfForeignObjectBinder(DynamicBindingGuru, esClass, selector);
						binderRegistry[selector] = binder;
					}
					return binder;
				}


			}

		}

		public class SetPropertyOrFieldOfForeignObjectBinder : SetMemberBinder {

			protected DynamicBindingGuru	dynamicBindingGuru	= null;
			protected ESKernel		kernel			= null;
			protected ESBehavior		esClass			= null;
			protected ESSymbol		selector		= null;

			protected SetPropertyOrFieldOfForeignObjectBinder(DynamicBindingGuru dynamicBindingGuru, ESBehavior esClass, ESSymbol selector) : base(selector.asHostSystemMemberName(CapitalizationScheme.InitialCapital), false) { 
				this.dynamicBindingGuru	= dynamicBindingGuru;
				kernel			= dynamicBindingGuru.Kernel;
				this.esClass		= esClass;
				this.selector		= selector;
			}

			public override DynamicMetaObject FallbackSetMember(DynamicMetaObject target, DynamicMetaObject value, DynamicMetaObject errorSuggestion) {
				if (!target.HasValue) return Defer(target, value);

				var self = target.asExpressionWithFormalType();
				Expression getPropertyOrFieldExpression = null;
				Type parameterType = null;

				if (esClass.getWritablePropertyOrElseField(
							Name, 
							(PropertyInfo property) => {
								parameterType = property.PropertyType; 
								getPropertyOrFieldExpression = Expression.Property(esClass.IsHostSystemMetaclass ? null : self, property);}, 
							(FieldInfo field)	=> {
								parameterType = field.FieldType; 
								getPropertyOrFieldExpression = Expression.Field(esClass.IsHostSystemMetaclass ? null : self, field);})) {
					var argGuru = value.typeBindingGuru(kernel);
					value = argGuru.metaObjectToConvertTo(parameterType);
					Expression assignExpression = Expression.Assign(getPropertyOrFieldExpression, value.Expression);
					return new DynamicMetaObject(
						Expression.Block(assignExpression, self),
						target.bindingRestrictionsForForeignObjectReceiver(esClass, value),
						target.Value);	
		
				} else {
					return new DynamicMetaObject(
						ExpressionTreeGuru.expressionToSendDoesNotUnderstand(target.Expression, esClass, selector, argArrayFor(value)),
						target.bindingRestrictionsForForeignObjectReceiver(esClass),
						target.Value);	
				}

			}

			public class Registry : BinderRegistry {

				protected readonly Dictionary<long, Dictionary<ESSymbol, SetPropertyOrFieldOfForeignObjectBinder>> registry = new Dictionary<long, Dictionary<ESSymbol, SetPropertyOrFieldOfForeignObjectBinder>>();

				public Registry(DynamicBindingGuru dynamicBindingGuru) : base(dynamicBindingGuru) {
				}

				public SetPropertyOrFieldOfForeignObjectBinder canonicalBinderFor(ESBehavior esClass, ESSymbol selector) {
					Dictionary<ESSymbol, SetPropertyOrFieldOfForeignObjectBinder> binderRegistry;
					SetPropertyOrFieldOfForeignObjectBinder binder;
					long classId = esClass.Identity;
					if (!registry.TryGetValue(classId, out binderRegistry)) {
						binderRegistry = new Dictionary<ESSymbol, SetPropertyOrFieldOfForeignObjectBinder>();
						registry[classId] = binderRegistry;
						binder = new SetPropertyOrFieldOfForeignObjectBinder(DynamicBindingGuru, esClass, selector);
						binderRegistry[selector] = binder;
						return binder;
					}
					if (!binderRegistry.TryGetValue(selector, out binder)) {
						binder = new SetPropertyOrFieldOfForeignObjectBinder(DynamicBindingGuru, esClass, selector);
						binderRegistry[selector] = binder;
					}
					return binder;
				}

			}

		}

		public class GetValueAtIndexOrKeyInForeignObjectBinder : GetIndexBinder {

			protected DynamicBindingGuru	dynamicBindingGuru	= null;
			protected ESKernel		kernel			= null;
			protected ESBehavior		esClass			= null;
			protected ESSymbol		selector		= null;

			protected GetValueAtIndexOrKeyInForeignObjectBinder(DynamicBindingGuru dynamicBindingGuru, ESBehavior esClass, ESSymbol selector) : base(ExpressionTreeGuru.callInfoForArgCount(selector.NumArgs)) { 
				this.dynamicBindingGuru	= dynamicBindingGuru;
				kernel = dynamicBindingGuru.Kernel;
				this.esClass		= esClass;
				this.selector		= selector;
			}

			public ESSymbol	Selector {
				get {return selector;}
			}

			public override DynamicMetaObject FallbackGetIndex(DynamicMetaObject target, DynamicMetaObject[] indexesOrKeys, DynamicMetaObject errorSuggestion) {
				if (!target.HasValue || indexesOrKeys.Any((indexOrKey) => !indexOrKey.HasValue)) return Defer(argArrayFor(target, indexesOrKeys)); 

				Expression performOperationExpression = null;
				var receiverType = target.LimitType;
				var indexMo = indexesOrKeys[0];
				var indexGuru = indexMo.typeBindingGuru(kernel);
				var typedIndexMo = indexMo;

				if (receiverType.IsArray) {
					indexMo = indexGuru.metaObjectToConvertTo(TypeGuru.intType);
					var index = Expression.Subtract(indexMo.Expression, oneInt32.Expression);
					performOperationExpression = Expression.ArrayAccess(target.asExpressionWithFormalType(), index);
				} else {
					Type indexParameterType;
					var methodInfo = receiverType.GetMethod("get_Item", esClass.HostObjectMethodInvokeBindingFlags);
					if (methodInfo == null) {
						var indexerMethods = TypeGuru.indexerMethodsOf(receiverType, esClass.HostObjectPropertyGetBindingFlags);
						List<DynamicMetaObject> typedArguments;
						if (dynamicBindingGuru.selectMethodAndTypeCompatibleArgumentsFor(indexerMethods.ToArray(), argArrayFor(indexesOrKeys[0]), out methodInfo, out typedArguments)) {
							typedIndexMo = typedArguments[0];
							indexParameterType = typedIndexMo.LimitType;
						} else {
							return target.BindInvokeMember(dynamicBindingGuru.canonicalInvokeMemberBinderFor(esClass, Selector, "At"), indexesOrKeys);
						}

						/*
						var propertyInfo = esClass.getReadableProperty("Chars");
						if (propertyInfo == null) {
							return target.BindInvokeMember(dynamicBindingGuru.canonicalInvokeMemberBinderFor(esClass, Selector, "At"), indexesOrKeys);
						}
						methodInfo = propertyInfo.GetGetMethod();
						*/
					} else {
						var parameters = methodInfo.GetParameters();
						var indexParameter = parameters[0];
						indexParameterType = indexParameter.ParameterType;
						typedIndexMo = indexGuru.metaObjectToConvertTo(indexParameterType);
					}
					Expression index;
					if (indexParameterType.isNumeric()) {
						index = Expression.Subtract(typedIndexMo.Expression, oneInt32.Expression);
					} else {
						index = typedIndexMo.Expression;
					}
					performOperationExpression = Expression.Call(methodInfo.IsStatic ? null : target.asExpressionWithFormalType(), methodInfo, index);
				}

				if (performOperationExpression.Type != TypeGuru.objectType) performOperationExpression = Expression.Convert(performOperationExpression, TypeGuru.objectType);

				return new DynamicMetaObject(
						performOperationExpression, 
						target.bindingRestrictionsForForeignObjectReceiver(esClass, indexMo),
						target.Value);

			}

			public class Registry : BinderRegistry {

				protected readonly Dictionary<long, Dictionary<ESSymbol, GetValueAtIndexOrKeyInForeignObjectBinder>> registry = new Dictionary<long, Dictionary<ESSymbol, GetValueAtIndexOrKeyInForeignObjectBinder>>();

				public Registry(DynamicBindingGuru dynamicBindingGuru) : base(dynamicBindingGuru) {
				}

				public GetValueAtIndexOrKeyInForeignObjectBinder canonicalBinderFor(ESBehavior esClass, ESSymbol selector) {
					Dictionary<ESSymbol, GetValueAtIndexOrKeyInForeignObjectBinder> binderRegistry;
					GetValueAtIndexOrKeyInForeignObjectBinder binder;
					long classId = esClass.Identity;
					if (!registry.TryGetValue(classId, out binderRegistry)) {
						binderRegistry = new Dictionary<ESSymbol, GetValueAtIndexOrKeyInForeignObjectBinder>();
						registry[classId] = binderRegistry;
						binder = new GetValueAtIndexOrKeyInForeignObjectBinder(DynamicBindingGuru, esClass, selector);
						binderRegistry[selector] = binder;
						return binder;
					}
					if (!binderRegistry.TryGetValue(selector, out binder)) {
						binder = new GetValueAtIndexOrKeyInForeignObjectBinder(DynamicBindingGuru, esClass, selector);
						binderRegistry[selector] = binder;
					}
					return binder;
				}

			}

		}

		public class SetValueAtIndexOrKeyInForeignObjectBinder : SetIndexBinder {

			protected DynamicBindingGuru	dynamicBindingGuru	= null;
			protected ESKernel		kernel			= null;
			protected ESBehavior		esClass			= null;
			protected ESSymbol		selector		= null;

			protected SetValueAtIndexOrKeyInForeignObjectBinder(DynamicBindingGuru dynamicBindingGuru, ESBehavior esClass, ESSymbol selector) : base(ExpressionTreeGuru.callInfoForArgCount(selector.NumArgs)) { 
				this.dynamicBindingGuru	= dynamicBindingGuru;
				kernel = dynamicBindingGuru.Kernel;
				this.esClass		= esClass;
				this.selector		= selector;
			}

			public ESSymbol	Selector {
				get {return selector;}
			}

			public override DynamicMetaObject FallbackSetIndex(DynamicMetaObject target, DynamicMetaObject[] indexesOrKeys, DynamicMetaObject value, DynamicMetaObject errorSuggestion) {
				if (!target.HasValue || !value.HasValue || indexesOrKeys.Any((indexOrKey) => !indexOrKey.HasValue)) return Defer(argArrayFor(target, indexesOrKeys, value)); 

				Expression performOperationExpression = null;
				var receiverType = target.LimitType;
				var indexMo = indexesOrKeys[0];
				var indexGuru = indexMo.typeBindingGuru(kernel);
				var valueGuru = value.typeBindingGuru(kernel);
				var typedIndexMo = indexMo;
				var typedValue = value;

				if (receiverType.IsArray) {
					typedIndexMo = indexGuru.metaObjectToConvertTo(TypeGuru.intType);
					var index = Expression.Subtract(typedIndexMo.Expression, oneInt32.Expression);
					typedValue = valueGuru.metaObjectToConvertTo(receiverType.GetElementType());
					performOperationExpression = Expression.Assign(Expression.ArrayAccess(target.asExpressionWithFormalType(), index), typedValue.Expression);
				} else {
					Type indexParameterType;
					var methodInfo = receiverType.GetMethod("set_Item", esClass.HostObjectMethodInvokeBindingFlags);
					if (methodInfo == null) {
						var indexerMethods = TypeGuru.indexerMethodsOf(receiverType, esClass.HostObjectPropertyGetBindingFlags);
						List<DynamicMetaObject> typedArguments;
						if (dynamicBindingGuru.selectMethodAndTypeCompatibleArgumentsFor(indexerMethods.ToArray(), argArrayFor(indexesOrKeys[0], value), out methodInfo, out typedArguments)) {
							typedIndexMo = typedArguments[0];
							typedValue = typedArguments[1];
							indexParameterType = typedIndexMo.LimitType;
						} else {
							return target.BindInvokeMember(dynamicBindingGuru.canonicalInvokeMemberBinderFor(esClass, Selector, "AtPut"), indexesOrKeys);
						}

						/*
						var propertyInfo = esClass.getWritableProperty("Chars");
						if (propertyInfo == null) {
							return target.BindInvokeMember(dynamicBindingGuru.canonicalInvokeMemberBinderFor(esClass, Selector, "AtPut"), indexesOrKeys);
						}
						methodInfo = propertyInfo.GetSetMethod();
						*/
					} else {
						var parameters = methodInfo.GetParameters();
						var indexParameter = parameters[0];
						indexParameterType = indexParameter.ParameterType;
						typedIndexMo = indexGuru.metaObjectToConvertTo(indexParameterType);
					}
					Expression index;
					if (indexParameterType.isNumeric()) {
						index = Expression.Subtract(typedIndexMo.Expression, oneInt32.Expression);
					} else {
						index = typedIndexMo.Expression;
					}
					typedValue = valueGuru.metaObjectToConvertTo(methodInfo.ReturnType);
					performOperationExpression = Expression.Call(methodInfo.IsStatic ? null : target.asExpressionWithFormalType(), methodInfo, new Expression[]{index, typedValue.Expression});
				}

				if (performOperationExpression.Type != TypeGuru.objectType) performOperationExpression = Expression.Convert(performOperationExpression, TypeGuru.objectType);

				return new DynamicMetaObject(
						performOperationExpression, 
						target.bindingRestrictionsForForeignObjectReceiver(esClass, indexMo, value),
						target.Value);

			}

			public class Registry : BinderRegistry {

				protected readonly Dictionary<long, Dictionary<ESSymbol, SetValueAtIndexOrKeyInForeignObjectBinder>> registry = new Dictionary<long, Dictionary<ESSymbol, SetValueAtIndexOrKeyInForeignObjectBinder>>();

				public Registry(DynamicBindingGuru dynamicBindingGuru) : base(dynamicBindingGuru) {
				}

				public SetValueAtIndexOrKeyInForeignObjectBinder canonicalBinderFor(ESBehavior esClass, ESSymbol selector) {
					Dictionary<ESSymbol, SetValueAtIndexOrKeyInForeignObjectBinder> binderRegistry;
					SetValueAtIndexOrKeyInForeignObjectBinder binder;
					long classId = esClass.Identity;
					if (!registry.TryGetValue(classId, out binderRegistry)) {
						binderRegistry = new Dictionary<ESSymbol, SetValueAtIndexOrKeyInForeignObjectBinder>();
						registry[classId] = binderRegistry;
						binder = new SetValueAtIndexOrKeyInForeignObjectBinder(DynamicBindingGuru, esClass, selector);
						binderRegistry[selector] = binder;
						return binder;
					}
					if (!binderRegistry.TryGetValue(selector, out binder)) {
						binder = new SetValueAtIndexOrKeyInForeignObjectBinder(DynamicBindingGuru, esClass, selector);
						binderRegistry[selector] = binder;
					}
					return binder;
				}

			}

		}

		public class RemoveAtIndexOrKeyFromForeignCollectionBinder : DeleteIndexBinder {

			protected DynamicBindingGuru	dynamicBindingGuru	= null;
			protected ESKernel		kernel			= null;
			protected ESBehavior		esClass			= null;
			protected ESSymbol		selector		= null;

			protected RemoveAtIndexOrKeyFromForeignCollectionBinder(DynamicBindingGuru dynamicBindingGuru, ESBehavior esClass, ESSymbol selector) : base(ExpressionTreeGuru.callInfoForArgCount(selector.NumArgs)) { 
				this.dynamicBindingGuru	= dynamicBindingGuru;
				kernel = dynamicBindingGuru.Kernel;
				this.esClass		= esClass;
				this.selector		= selector;
			}

			public ESSymbol	Selector {
				get {return selector;}
			}

			public override DynamicMetaObject FallbackDeleteIndex(DynamicMetaObject target, DynamicMetaObject[] indexesOrKeys, DynamicMetaObject errorSuggestion) {

				if (!target.HasValue || indexesOrKeys.Any((indexOrKey) => !indexOrKey.HasValue)) return Defer(argArrayFor(target, indexesOrKeys)); 

				return target.BindInvokeMember(dynamicBindingGuru.canonicalInvokeMemberBinderFor(esClass, Selector, "RemoveAt"), indexesOrKeys);

			}

			public class Registry : BinderRegistry {

				protected readonly Dictionary<long, Dictionary<ESSymbol, RemoveAtIndexOrKeyFromForeignCollectionBinder>> registry = new Dictionary<long, Dictionary<ESSymbol, RemoveAtIndexOrKeyFromForeignCollectionBinder>>();

				public Registry(DynamicBindingGuru dynamicBindingGuru) : base(dynamicBindingGuru) {
				}

				public RemoveAtIndexOrKeyFromForeignCollectionBinder canonicalBinderFor(ESBehavior esClass, ESSymbol selector) {
					Dictionary<ESSymbol, RemoveAtIndexOrKeyFromForeignCollectionBinder> binderRegistry;
					RemoveAtIndexOrKeyFromForeignCollectionBinder binder;
					long classId = esClass.Identity;
					if (!registry.TryGetValue(classId, out binderRegistry)) {
						binderRegistry = new Dictionary<ESSymbol, RemoveAtIndexOrKeyFromForeignCollectionBinder>();
						registry[classId] = binderRegistry;
						binder = new RemoveAtIndexOrKeyFromForeignCollectionBinder(DynamicBindingGuru, esClass, selector);
						binderRegistry[selector] = binder;
						return binder;
					}
					if (!binderRegistry.TryGetValue(selector, out binder)) {
						binder = new RemoveAtIndexOrKeyFromForeignCollectionBinder(DynamicBindingGuru, esClass, selector);
						binderRegistry[selector] = binder;
					}
					return binder;
				}

			}

		}

		public class UnaryMessageSendToForeignObjectBinder : UnaryOperationBinder {
	
			protected DynamicBindingGuru	dynamicBindingGuru	= null;
			protected ESKernel		kernel			= null;
			protected ESBehavior		esClass			= null;
			protected ESSymbol		selector		= null;

			protected UnaryMessageSendToForeignObjectBinder(DynamicBindingGuru dynamicBindingGuru, ESBehavior esClass, ESSymbol selector, ExpressionType operation) : base(operation) {
				this.dynamicBindingGuru	= dynamicBindingGuru;
				kernel = dynamicBindingGuru.Kernel;
				this.esClass		= esClass;
				this.selector		= selector;
			}

			public ESSymbol	Selector {
				get {return selector;}
			}

			public override DynamicMetaObject FallbackUnaryOperation(DynamicMetaObject target, DynamicMetaObject errorSuggestion) {
				if (!target.HasValue) return Defer(target);

				Expression unaryOperatorExpression = null;
				Object model = target.Value;
				var self = target.asExpressionWithFormalType();
				BindingRestrictions bindingRestrictions = null;

				switch (Selector.CanonicalSemantics) {
					case CanonicalSelectorSemantics.Size:	
						var receiverType = target.LimitType;
						if (receiverType.IsArray) {
							unaryOperatorExpression = Expression.ArrayLength(self);
						} else {
							return target.BindGetMember(dynamicBindingGuru.canonicalGetMemberBinderFor(esClass, Selector));
						}
						break;
					case CanonicalSelectorSemantics.Negated:
						unaryOperatorExpression = Expression.Negate(self);
						break;
					case CanonicalSelectorSemantics.LogicalNot:
						unaryOperatorExpression = Expression.Not(self);
						break;
					case CanonicalSelectorSemantics.IsTrue:
						unaryOperatorExpression =  Expression.Block(model is bool ? (Expression)self : ExpressionTreeGuru.falseConstant);
						break; 
					case CanonicalSelectorSemantics.IsFalse:
						unaryOperatorExpression =  Expression.Block(model is bool ? (Expression)Expression.Not(self) : ExpressionTreeGuru.falseConstant);
						break;
					default:
						unaryOperatorExpression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(self, esClass, selector, emptyArgArray);
						bindingRestrictions = target.bindingRestrictionsForForeignObjectReceiver(esClass);
						break;
				}

				if (bindingRestrictions == null) {
					bindingRestrictions = target.addingFormalTypeRestriction();
				}

				return new DynamicMetaObject(
						Expression.Convert(unaryOperatorExpression, TypeGuru.objectType), 
						bindingRestrictions,
						target.Value);

			}

			public class Registry : BinderRegistry {

				protected readonly Dictionary<long, Dictionary<ESSymbol, UnaryMessageSendToForeignObjectBinder>> registry = new Dictionary<long, Dictionary<ESSymbol, UnaryMessageSendToForeignObjectBinder>>();

				public Registry(DynamicBindingGuru dynamicBindingGuru) : base(dynamicBindingGuru) {
				}

				public UnaryMessageSendToForeignObjectBinder canonicalBinderFor(ESBehavior esClass, ESSymbol selector, ExpressionType operation) {
					Dictionary<ESSymbol, UnaryMessageSendToForeignObjectBinder> binderRegistry;
					UnaryMessageSendToForeignObjectBinder binder;
					long classId = esClass.Identity;
					if (!registry.TryGetValue(classId, out binderRegistry)) {
						binderRegistry = new Dictionary<ESSymbol, UnaryMessageSendToForeignObjectBinder>();
						registry[classId] = binderRegistry;
						binder = new UnaryMessageSendToForeignObjectBinder(DynamicBindingGuru, esClass, selector, operation);
						binderRegistry[selector] = binder;
						return binder;
					}
					if (!binderRegistry.TryGetValue(selector, out binder)) {
						binder = new UnaryMessageSendToForeignObjectBinder(DynamicBindingGuru, esClass, selector, operation);
						binderRegistry[selector] = binder;
					}
					return binder;
				}

			}

		}

		public class BinaryMessageSendToForeignObjectBinder : BinaryOperationBinder {
	
			protected DynamicBindingGuru	dynamicBindingGuru	= null;
			protected ESKernel		kernel			= null;
			protected ESBehavior		esClass			= null;
			protected ESSymbol		selector		= null;

			protected BinaryMessageSendToForeignObjectBinder(DynamicBindingGuru dynamicBindingGuru, ESBehavior esClass, ESSymbol selector, ExpressionType operation) : base(operation) {
				this.dynamicBindingGuru	= dynamicBindingGuru;
				kernel = dynamicBindingGuru.Kernel;
				this.esClass		= esClass;
				this.selector		= selector;
			}

			public ESSymbol	Selector {
				get {return selector;}
			}

			public override DynamicMetaObject FallbackBinaryOperation(DynamicMetaObject leftOperandMO, DynamicMetaObject rightOperandMO, DynamicMetaObject errorSuggestion) {
				if (!leftOperandMO.HasValue || !rightOperandMO.HasValue) return Defer(leftOperandMO, rightOperandMO);

				Expression leftOperand;
				Expression rightOperand;
				ParameterExpression leftParameter = Expression.Parameter(TypeGuru.objectType, "$leftOperand");
				ParameterExpression rightParameter = Expression.Parameter(TypeGuru.objectType, "$rightOperand");
				Expression binaryOperatorExpression = null;
				BindingRestrictions bindingRestrictions = null;;

				switch (Selector.CanonicalSemantics) {
					case CanonicalSelectorSemantics.IsEqualTo:
					case CanonicalSelectorSemantics.IsZero:
						if (convertNumericTypesToHighestGenerality(leftOperandMO, rightOperandMO, out leftOperand, out rightOperand)) {
							binaryOperatorExpression = Expression.Equal(leftOperand, rightOperand);
						} else {
							binaryOperatorExpression = Expression.Equal(leftOperandMO.asExpressionWithFormalType(), rightOperandMO.asExpressionWithFormalType());
						}
						break;
					case CanonicalSelectorSemantics.IsNotEqualTo:
						if (convertNumericTypesToHighestGenerality(leftOperandMO, rightOperandMO, out leftOperand, out rightOperand)) {
							binaryOperatorExpression = Expression.NotEqual(leftOperand, rightOperand);
						} else {
							binaryOperatorExpression = Expression.NotEqual(leftOperandMO.asExpressionWithFormalType(), rightOperandMO.asExpressionWithFormalType());
						}
						break;
					case CanonicalSelectorSemantics.LogicalAnd:
						if (convertNumericTypesToHighestGenerality(leftOperandMO, rightOperandMO, out leftOperand, out rightOperand)) {
							binaryOperatorExpression = Expression.And(leftOperand, rightOperand);
						} else {
							binaryOperatorExpression = Expression.And(Expression.Convert(leftOperandMO.asExpressionWithFormalType(), TypeGuru.boolType), Expression.Convert(rightOperandMO.asExpressionWithFormalType(), TypeGuru.boolType));
						}
						break;
					case CanonicalSelectorSemantics.LogicalOr:
						if (convertNumericTypesToHighestGenerality(leftOperandMO, rightOperandMO, out leftOperand, out rightOperand)) {
							binaryOperatorExpression = Expression.Or(leftOperand, rightOperand);
						} else {
							binaryOperatorExpression = Expression.Or(Expression.Convert(leftOperandMO.asExpressionWithFormalType(), TypeGuru.boolType), Expression.Convert(rightOperandMO.asExpressionWithFormalType(), TypeGuru.boolType));
						}
						break;
					case CanonicalSelectorSemantics.LogicalXor:
						if (convertNumericTypesToHighestGenerality(leftOperandMO, rightOperandMO, out leftOperand, out rightOperand)) {
							binaryOperatorExpression = Expression.ExclusiveOr(leftOperand, rightOperand);
						} else {
							binaryOperatorExpression = Expression.ExclusiveOr(Expression.Convert(leftOperandMO.asExpressionWithFormalType(), TypeGuru.boolType), Expression.Convert(rightOperandMO.asExpressionWithFormalType(), TypeGuru.boolType));
						}
						break;
					case CanonicalSelectorSemantics.IsLessThan:
						if (convertNumericTypesToHighestGenerality(leftOperandMO, rightOperandMO, out leftOperand, out rightOperand)) {
							binaryOperatorExpression = Expression.LessThan(leftOperand, rightOperand);
						} else {
							binaryOperatorExpression = Expression.LessThan(leftOperandMO.asExpressionWithFormalType(), rightOperandMO.asExpressionWithFormalType());
						}
						break;
					case CanonicalSelectorSemantics.Negative:
						if (convertNumericTypesToHighestGenerality(leftOperandMO, rightOperandMO, out leftOperand, out rightOperand)) {
							binaryOperatorExpression = Expression.LessThan(leftOperand, rightOperand);
						}
						break;
					case CanonicalSelectorSemantics.IsGreaterThan:
						if (convertNumericTypesToHighestGenerality(leftOperandMO, rightOperandMO, out leftOperand, out rightOperand)) {
							binaryOperatorExpression = Expression.GreaterThan(leftOperand, rightOperand);
						} else {
							binaryOperatorExpression = Expression.GreaterThan(leftOperandMO.asExpressionWithFormalType(), rightOperandMO.asExpressionWithFormalType());
						}
						break;
					case CanonicalSelectorSemantics.StrictlyPositive:
						if (convertNumericTypesToHighestGenerality(leftOperandMO, rightOperandMO, out leftOperand, out rightOperand)) {
							binaryOperatorExpression = Expression.GreaterThan(leftOperand, rightOperand);
						}
						break;
					case CanonicalSelectorSemantics.IsLessThanOrEqual:
						if (convertNumericTypesToHighestGenerality(leftOperandMO, rightOperandMO, out leftOperand, out rightOperand)) {
							binaryOperatorExpression = Expression.LessThanOrEqual(leftOperand, rightOperand);
						} else {
							binaryOperatorExpression = Expression.LessThanOrEqual(leftOperandMO.asExpressionWithFormalType(), rightOperandMO.asExpressionWithFormalType());
						}
						break;
					case CanonicalSelectorSemantics.IsGreaterThanOrEqual:
						if (convertNumericTypesToHighestGenerality(leftOperandMO, rightOperandMO, out leftOperand, out rightOperand)) {
							binaryOperatorExpression = Expression.GreaterThanOrEqual(leftOperand, rightOperand);
						} else {
							binaryOperatorExpression = Expression.GreaterThanOrEqual(leftOperandMO.asExpressionWithFormalType(), rightOperandMO.asExpressionWithFormalType());
						}
						break;
					case CanonicalSelectorSemantics.Positive:
						if (convertNumericTypesToHighestGenerality(leftOperandMO, rightOperandMO, out leftOperand, out rightOperand)) {
							binaryOperatorExpression = Expression.GreaterThanOrEqual(leftOperand, rightOperand);
						}
						break;
					case CanonicalSelectorSemantics.Plus:
						if (convertNumericTypesToHighestGenerality(leftOperandMO, rightOperandMO, out leftOperand, out rightOperand)) {
							binaryOperatorExpression = Expression.Add(leftOperand, rightOperand);
						} else {
							binaryOperatorExpression = Expression.Add(leftOperandMO.asExpressionWithFormalType(), rightOperandMO.asExpressionWithFormalType());
						}
						break;
					case CanonicalSelectorSemantics.Minus:
						if (convertNumericTypesToHighestGenerality(leftOperandMO, rightOperandMO, out leftOperand, out rightOperand)) {
							binaryOperatorExpression = Expression.Subtract(leftOperand, rightOperand);
						} else {
							binaryOperatorExpression = Expression.Subtract(leftOperandMO.asExpressionWithFormalType(), rightOperandMO.asExpressionWithFormalType());
						}
						break;
					case CanonicalSelectorSemantics.Times:
						if (convertNumericTypesToHighestGenerality(leftOperandMO, rightOperandMO, out leftOperand, out rightOperand)) {
							binaryOperatorExpression = Expression.Multiply(leftOperand, rightOperand);
						} else {
							binaryOperatorExpression = Expression.Multiply(leftOperandMO.asExpressionWithFormalType(), rightOperandMO.asExpressionWithFormalType());
						}
						break;
					case CanonicalSelectorSemantics.DivideToRational:
						if (convertNumericTypesToHighestGenerality(leftOperandMO, rightOperandMO, out leftOperand, out rightOperand)) {
							binaryOperatorExpression = Expression.Divide(leftOperand.asRationalNumberExpression(), rightOperand.asRationalNumberExpression());
						} else {
							binaryOperatorExpression = Expression.Divide(leftOperandMO.asExpressionWithFormalType(), rightOperandMO.asExpressionWithFormalType());
						}
						break;
					case CanonicalSelectorSemantics.Reciprocal:
						if (convertNumericTypesToHighestGenerality(leftOperandMO, rightOperandMO, out leftOperand, out rightOperand)) {
							binaryOperatorExpression = Expression.Divide(oneWithHighestGenerality.Expression, rightOperand.asRationalNumberExpression());
						}
						break;
					case CanonicalSelectorSemantics.DivisionRemainder:
						if (convertNumericTypesToHighestGenerality(leftOperandMO, rightOperandMO, out leftOperand, out rightOperand)) {
							binaryOperatorExpression = Expression.Modulo(leftOperand, rightOperand);
						} else {
							binaryOperatorExpression = Expression.Modulo(leftOperandMO.asExpressionWithFormalType(), rightOperandMO.asExpressionWithFormalType());
						}
						break;
					case CanonicalSelectorSemantics.BitAnd:
						if (convertNumericTypesToHighestGenerality(leftOperandMO, rightOperandMO, out leftOperand, out rightOperand)) {
							binaryOperatorExpression = Expression.And(leftOperand, rightOperand);
						} else {
							binaryOperatorExpression = Expression.And(leftOperandMO.asExpressionWithFormalType(), rightOperandMO.asExpressionWithFormalType());
						}
						break;
					case CanonicalSelectorSemantics.BitOr:
						if (convertNumericTypesToHighestGenerality(leftOperandMO, rightOperandMO, out leftOperand, out rightOperand)) {
							binaryOperatorExpression = Expression.Or(leftOperand, rightOperand);
						} else {
							binaryOperatorExpression = Expression.Or(leftOperandMO.asExpressionWithFormalType(), rightOperandMO.asExpressionWithFormalType());
						}
						break;
					case CanonicalSelectorSemantics.BitXor:
						if (convertNumericTypesToHighestGenerality(leftOperandMO, rightOperandMO, out leftOperand, out rightOperand)) {
							binaryOperatorExpression = Expression.ExclusiveOr(leftOperand, rightOperand);
						} else {
							binaryOperatorExpression = Expression.ExclusiveOr(leftOperandMO.asExpressionWithFormalType(), rightOperandMO.asExpressionWithFormalType());
						}
						break;
					case CanonicalSelectorSemantics.ShiftLeft:
						if (convertNumericTypesToHighestGenerality(leftOperandMO, rightOperandMO, out leftOperand, out rightOperand)) {
							binaryOperatorExpression = Expression.LeftShift(leftOperand, Expression.Convert(rightOperand, TypeGuru.intType));
						} else {
							binaryOperatorExpression = Expression.LeftShift(leftOperandMO.asExpressionWithFormalType(), Expression.Convert(rightOperandMO.asExpressionWithFormalType(), TypeGuru.intType));
						}
						break;
					case CanonicalSelectorSemantics.ShiftRight:
						if (convertNumericTypesToHighestGenerality(leftOperandMO, rightOperandMO, out leftOperand, out rightOperand)) {
							binaryOperatorExpression = Expression.RightShift(leftOperand, Expression.Convert(rightOperand, TypeGuru.intType));
						} else {
							binaryOperatorExpression = Expression.RightShift(leftOperandMO.asExpressionWithFormalType(), Expression.Convert(rightOperandMO.asExpressionWithFormalType(), TypeGuru.intType));
						}
						break;

					default:
						binaryOperatorExpression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(leftOperandMO.Expression, esClass, selector, argArrayFor(rightOperandMO));
						bindingRestrictions = leftOperandMO.bindingRestrictionsForForeignObjectReceiver(esClass, rightOperandMO);
						break;

				}

				if (binaryOperatorExpression == null) {
					binaryOperatorExpression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(leftOperandMO.Expression, esClass, selector, argArrayFor(rightOperandMO));
					bindingRestrictions = leftOperandMO.bindingRestrictionsForForeignObjectReceiver(esClass, rightOperandMO);
				} else {
					binaryOperatorExpression = Expression.Convert(binaryOperatorExpression, TypeGuru.objectType);
					if (bindingRestrictions == null) {
						bindingRestrictions = leftOperandMO.addingFormalTypeRestriction().Merge(rightOperandMO.addingFormalTypeRestriction());
					}
				}

				return new DynamicMetaObject(
						binaryOperatorExpression, 
						bindingRestrictions,
						leftOperandMO.Value);
			}

			public class Registry : BinderRegistry {

				protected readonly Dictionary<long, Dictionary<ESSymbol, BinaryMessageSendToForeignObjectBinder>> registry = new Dictionary<long, Dictionary<ESSymbol, BinaryMessageSendToForeignObjectBinder>>();

				public Registry(DynamicBindingGuru dynamicBindingGuru) : base(dynamicBindingGuru) {
				}

				public BinaryMessageSendToForeignObjectBinder canonicalBinderFor(ESBehavior esClass, ESSymbol selector, ExpressionType operation) {
					Dictionary<ESSymbol, BinaryMessageSendToForeignObjectBinder> binderRegistry;
					BinaryMessageSendToForeignObjectBinder binder;
					long classId = esClass.Identity;
					if (!registry.TryGetValue(classId, out binderRegistry)) {
						binderRegistry = new Dictionary<ESSymbol, BinaryMessageSendToForeignObjectBinder>();
						registry[classId] = binderRegistry;
						binder = new BinaryMessageSendToForeignObjectBinder(DynamicBindingGuru, esClass, selector, operation);
						binderRegistry[selector] = binder;
						return binder;
					}
					if (!binderRegistry.TryGetValue(selector, out binder)) {
						binder = new BinaryMessageSendToForeignObjectBinder(DynamicBindingGuru, esClass, selector, operation);
						binderRegistry[selector] = binder;
					}
					return binder;
				}

			}

		}

		public class ConvertTypeOfForeignObjectBinder : ConvertBinder {
	
			protected DynamicBindingGuru	dynamicBindingGuru	= null;
			protected ESKernel		kernel			= null;
			protected ESBehavior		esClass			= null;

			protected ConvertTypeOfForeignObjectBinder(DynamicBindingGuru dynamicBindingGuru, ESBehavior esClass, Type type) : base(type, true) {
				this.dynamicBindingGuru	= dynamicBindingGuru;
				kernel = dynamicBindingGuru.Kernel;
				this.esClass		= esClass;
			}

			public override DynamicMetaObject FallbackConvert(DynamicMetaObject target, DynamicMetaObject errorSuggestion) {
				if (!target.HasValue) return Defer(target);

				var targetGuru = target.typeBindingGuru(kernel);
				var conversion = targetGuru.metaObjectToConvertTo(Type);

				return new DynamicMetaObject(
							Expression.Convert(conversion.Expression, TypeGuru.objectType), 
							target.bindingRestrictionsForForeignObjectReceiver(esClass),
							target.Value);
			}

			public class Registry : BinderRegistry {

				protected readonly Dictionary<long, Dictionary<Type, ConvertTypeOfForeignObjectBinder>> registry = new Dictionary<long, Dictionary<Type, ConvertTypeOfForeignObjectBinder>>();

				public Registry(DynamicBindingGuru dynamicBindingGuru) : base(dynamicBindingGuru) {
				}

				public ConvertTypeOfForeignObjectBinder canonicalBinderFor(ESBehavior esClass, Type type) {
					Dictionary<Type, ConvertTypeOfForeignObjectBinder> binderRegistry;
					ConvertTypeOfForeignObjectBinder binder;
					long classId = esClass.Identity;
					if (!registry.TryGetValue(classId, out binderRegistry)) {
						binderRegistry = new Dictionary<Type, ConvertTypeOfForeignObjectBinder>();
						registry[classId] = binderRegistry;
						binder = new ConvertTypeOfForeignObjectBinder(DynamicBindingGuru, esClass, type);
						binderRegistry[type] = binder;
						return binder;
					}
					if (!binderRegistry.TryGetValue(type, out binder)) {
						binder = new ConvertTypeOfForeignObjectBinder(DynamicBindingGuru, esClass, type);
						binderRegistry[type] = binder;
					}
					return binder;
				}

			}

		}

		public class InvokeForeignFunctionBinder : InvokeBinder {
	
			protected DynamicBindingGuru	dynamicBindingGuru	= null;
			protected ESKernel		kernel			= null;
			protected ESBehavior		esClass			= null;
			protected ESSymbol		selector		= null;

			protected InvokeForeignFunctionBinder(DynamicBindingGuru dynamicBindingGuru, ESBehavior esClass, ESSymbol selector) : base(ExpressionTreeGuru.callInfoForArgCount(selector.NumArgs)) { 
				this.dynamicBindingGuru	= dynamicBindingGuru;
				kernel = dynamicBindingGuru.Kernel;
				this.esClass		= esClass;
				this.selector		= selector;
			}

			public ESSymbol	Selector {
				get {return selector;}
			}

			public override DynamicMetaObject FallbackInvoke(DynamicMetaObject target, DynamicMetaObject[] args, DynamicMetaObject errorSuggestion) {

				if (!target.HasValue || args.Any((arg) => !arg.HasValue)) return Defer(argArrayFor(target, args)); 

				if (TypeGuru.delegateType.IsAssignableFrom(target.LimitType)) {
					var method = target.LimitType.GetMethod("Invoke");
					var argGurus = dynamicBindingGuru.dynamicMetaObjectArgumentGurusFor(args);
					var arguments = typeCompatibleArgumentsFor(method, argGurus);
					Expression invokeExpression = Expression.Invoke(target.asExpressionWithFormalType(), expressionArrayFor(arguments.ToArray()));
					if (method.ReturnType == TypeGuru.voidType) invokeExpression = Expression.Block(TypeGuru.objectType, invokeExpression, target.Expression);
					return new DynamicMetaObject(
							invokeExpression, 
							target.addingFormalTypeRestriction().Merge(BindingRestrictions.Combine(arguments)),
							target.Value);
				} else {
					return target.BindInvokeMember(dynamicBindingGuru.canonicalInvokeMemberBinderFor(esClass, Selector, "Value"), args);
				}

			}

			public class Registry : BinderRegistry {

				protected readonly Dictionary<long, Dictionary<ESSymbol, InvokeForeignFunctionBinder>> registry = new Dictionary<long, Dictionary<ESSymbol, InvokeForeignFunctionBinder>>();

				public Registry(DynamicBindingGuru dynamicBindingGuru) : base(dynamicBindingGuru) {
				}

				public InvokeForeignFunctionBinder canonicalBinderFor(ESBehavior esClass, ESSymbol selector) {
					Dictionary<ESSymbol, InvokeForeignFunctionBinder> binderRegistry;
					InvokeForeignFunctionBinder binder;
					long classId = esClass.Identity;
					if (!registry.TryGetValue(classId, out binderRegistry)) {
						binderRegistry = new Dictionary<ESSymbol, InvokeForeignFunctionBinder>();
						registry[classId] = binderRegistry;
						binder = new InvokeForeignFunctionBinder(DynamicBindingGuru, esClass, selector);
						binderRegistry[selector] = binder;
						return binder;
					}
					if (!binderRegistry.TryGetValue(selector, out binder)) {
						binder = new InvokeForeignFunctionBinder(DynamicBindingGuru, esClass, selector);
						binderRegistry[selector] = binder;
					}
					return binder;
				}

			}

		}

		public class MessageSendToForeignObjectBinder : InvokeMemberBinder {
	
			protected DynamicBindingGuru	dynamicBindingGuru	= null;
			protected ESKernel		kernel			= null;
			protected ESBehavior		esClass			= null;
			protected ESSymbol		selector		= null;
			protected String		alternateMethodName	= null;

			protected MessageSendToForeignObjectBinder(DynamicBindingGuru dynamicBindingGuru, ESBehavior esClass, ESSymbol selector, String memberName) : base(memberName, false, ExpressionTreeGuru.callInfoForArgCount(selector.NumArgs)) {
				this.dynamicBindingGuru	= dynamicBindingGuru;
				kernel = dynamicBindingGuru.Kernel;
				this.esClass		= esClass;
				this.selector		= selector;
				alternateMethodName	= memberName.usingCapitalizationScheme(CapitalizationScheme.InitialLowerCase);
			}

			public ESSymbol	Selector {
				get {return selector;}
			}

			public String AlternateMethodName {
				get {return alternateMethodName;}
			}

			public override DynamicMetaObject FallbackInvokeMember(DynamicMetaObject target, DynamicMetaObject[] args, DynamicMetaObject errorSuggestion) {

				if (!target.HasValue || args.Any((arg) => !arg.HasValue)) return Defer(argArrayFor(target, args)); 

				Expression invokeMemberExpression = null;
				var receiverType = target.LimitType;
				MethodInfo methodInfo;
				long arity = args.Length;
				List<DynamicMetaObject> typedArguments;
				// var typesArray = argLimitTypeArrayFor(args);

				if (arity < 1) {
					methodInfo = esClass.getHostMethod(Name, TypeGuru.emptyTypeArray);
					if (methodInfo == null) methodInfo = esClass.getHostMethod(AlternateMethodName, TypeGuru.emptyTypeArray);
					typedArguments = emptyArgList;
				} else if (!dynamicBindingGuru.getMethodAndTypeCompatibleArgumentsFor(Name, esClass, args, out methodInfo, out typedArguments)) {
					dynamicBindingGuru.getMethodAndTypeCompatibleArgumentsFor(AlternateMethodName, esClass, args, out methodInfo, out typedArguments);
				}

				if (methodInfo == null) {
					switch (Selector.NumArgs) {
						case 0:
							return target.BindGetMember(dynamicBindingGuru.canonicalGetMemberBinderFor(esClass, Selector));
						case 1:
							return target.BindSetMember(dynamicBindingGuru.canonicalSetMemberBinderFor(esClass, Selector), args[0]);
						default:
							invokeMemberExpression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(target.Expression, esClass, selector, args);
							break;
					}
				} else {
					if (methodInfo.IsStatic) {
						invokeMemberExpression = Expression.Call(null, methodInfo, expressionArrayFor(typedArguments.ToArray()));
					} else { 
						var self = target.asExpressionWithFormalType();
						if (!methodInfo.DeclaringType.IsAssignableFrom(self.Type)) self = Expression.Convert(self, methodInfo.DeclaringType);
						invokeMemberExpression = Expression.Call(self, methodInfo, expressionArrayFor(typedArguments.ToArray()));
					}
					if (methodInfo.ReturnType == TypeGuru.voidType) invokeMemberExpression = Expression.Block(TypeGuru.objectType, invokeMemberExpression, target.Expression);
				}

				return new DynamicMetaObject(
						Expression.Convert(invokeMemberExpression, TypeGuru.objectType), 
						target.bindingRestrictionsForForeignObjectReceiver(esClass, typedArguments),
						target.Value);

			}

			public override DynamicMetaObject FallbackInvoke(DynamicMetaObject target, DynamicMetaObject[] args, DynamicMetaObject errorSuggestion) {

				if (!target.HasValue || args.Any((arg) => !arg.HasValue)) return Defer(argArrayFor(target, args)); 

				if (TypeGuru.delegateType.IsAssignableFrom(target.LimitType)) {
					var method = target.LimitType.GetMethod("Invoke");
					var argGurus = dynamicBindingGuru.dynamicMetaObjectArgumentGurusFor(args);
					var arguments = typeCompatibleArgumentsFor(method, argGurus);
					Expression invokeExpression = Expression.Invoke(target.asExpressionWithFormalType(), expressionArrayFor(arguments.ToArray()));
					if (method.ReturnType == TypeGuru.voidType) invokeExpression = Expression.Block(TypeGuru.objectType, invokeExpression, target.Expression);
					return new DynamicMetaObject(
							invokeExpression, 
							target.addingFormalTypeRestriction().Merge(BindingRestrictions.Combine(arguments)),
							target.Value);
				} else {
					return new DynamicMetaObject(
							ExpressionTreeGuru.expressionToSendDoesNotUnderstand(target.Expression, esClass, selector, args), 
							target.bindingRestrictionsForForeignObjectReceiver(esClass),
							target.Value);
				}

			}

			public class Registry : BinderRegistry {

				protected readonly Dictionary<long, Dictionary<ESSymbol, MessageSendToForeignObjectBinder>> registry = new Dictionary<long, Dictionary<ESSymbol, MessageSendToForeignObjectBinder>>();

				public Registry(DynamicBindingGuru dynamicBindingGuru) : base(dynamicBindingGuru) {
				}

				public MessageSendToForeignObjectBinder canonicalBinderFor(ESBehavior esClass, ESSymbol selector, String memberName) {
					Dictionary<ESSymbol, MessageSendToForeignObjectBinder> binderRegistry;
					MessageSendToForeignObjectBinder binder;
					long classId = esClass.Identity;
					if (!registry.TryGetValue(classId, out binderRegistry)) {
						binderRegistry = new Dictionary<ESSymbol, MessageSendToForeignObjectBinder>();
						registry[classId] = binderRegistry;
						binder = new MessageSendToForeignObjectBinder(DynamicBindingGuru, esClass, selector, memberName);
						binderRegistry[selector] = binder;
						return binder;
					}
					if (!binderRegistry.TryGetValue(selector, out binder)) {
						binder = new MessageSendToForeignObjectBinder(DynamicBindingGuru, esClass, selector, memberName);
						binderRegistry[selector] = binder;
					}
					return binder;
				}
			}

		}

		public class CreateInstanceOfForeignObjectBinder : CreateInstanceBinder {
	
			protected DynamicBindingGuru	dynamicBindingGuru	= null;
			protected ESKernel		kernel			= null;
			protected ESBehavior		esClass			= null;
			protected ESSymbol		selector		= null;

			protected CreateInstanceOfForeignObjectBinder(DynamicBindingGuru dynamicBindingGuru, ESBehavior esClass, ESSymbol selector) : base(ExpressionTreeGuru.callInfoForArgCount(selector.NumArgs)) { 
				this.dynamicBindingGuru	= dynamicBindingGuru;
				kernel = dynamicBindingGuru.Kernel;
				this.esClass		= esClass;
				this.selector		= selector;
			}

			public ESSymbol	Selector {
				get {return selector;}
			}

			public override DynamicMetaObject FallbackCreateInstance(DynamicMetaObject target, DynamicMetaObject[] args, DynamicMetaObject errorSuggestion) {

				if (!target.HasValue || args.Any((arg) => !arg.HasValue)) return Defer(argArrayFor(target, args)); 

				if (target.Value is Type) {
					ESBehavior wrapperClass;
					ConstructorInfo constructorInfo;
					var hostSystemType = (Type)target.Value;
					switch (Selector.NumArgs) {
						case 0:
							constructorInfo = hostSystemType.GetConstructor(
										ESBehavior.instanceCreationBindingFlags, 
										Type.DefaultBinder, 
										TypeGuru.emptyTypeArray, 
										null);
							if (constructorInfo != null) {
								return new DynamicMetaObject(
									Expression.New(constructorInfo, ExpressionTreeGuru.emptyExpressionArray), 
									target.bindingRestrictionsForForeignObjectReceiver(esClass, args),
									target.Value);
							}
							wrapperClass = kernel.classForHostSystemType(hostSystemType);
							break;
						case 1:
							wrapperClass = kernel.classForHostSystemType(hostSystemType);
							List<DynamicMetaObject> typedArguments;
							if (dynamicBindingGuru.getConstructorAndTypeCompatibleArgumentsFor(wrapperClass, args, out constructorInfo, out typedArguments)) {
								return new DynamicMetaObject(
									Expression.New(constructorInfo, expressionArrayFor(typedArguments.ToArray())), 
									target.bindingRestrictionsForForeignObjectReceiver(esClass, typedArguments),
									target.Value);
							}
							break;
						default:
							wrapperClass = kernel.classForHostSystemType(hostSystemType);
							break;
					}
					var wrapperMetaObject = new DynamicMetaObject(Expression.Constant(wrapperClass), target.Restrictions, wrapperClass);
					return dynamicBindingGuru.metaObjectToSendMessageToESObject(wrapperMetaObject, kernel, wrapperClass.Class, Selector, args);
				} else {
					return target.BindInvokeMember(
							dynamicBindingGuru.canonicalInvokeMemberBinderFor(
								esClass, 
								Selector, 
								Selector.asHostSystemMemberName(CapitalizationScheme.InitialCapital)), 
							args);
				}

			}

			public class Registry : BinderRegistry {

				protected readonly Dictionary<long, Dictionary<ESSymbol, CreateInstanceOfForeignObjectBinder>> registry = new Dictionary<long, Dictionary<ESSymbol, CreateInstanceOfForeignObjectBinder>>();

				public Registry(DynamicBindingGuru dynamicBindingGuru) : base(dynamicBindingGuru) {
				}

				public CreateInstanceOfForeignObjectBinder canonicalBinderFor(ESBehavior esClass, ESSymbol selector) {
					Dictionary<ESSymbol, CreateInstanceOfForeignObjectBinder> binderRegistry;
					CreateInstanceOfForeignObjectBinder binder;
					long classId = esClass.Identity;
					if (!registry.TryGetValue(classId, out binderRegistry)) {
						binderRegistry = new Dictionary<ESSymbol, CreateInstanceOfForeignObjectBinder>();
						registry[classId] = binderRegistry;
						binder = new CreateInstanceOfForeignObjectBinder(DynamicBindingGuru, esClass, selector);
						binderRegistry[selector] = binder;
						return binder;
					}
					if (!binderRegistry.TryGetValue(selector, out binder)) {
						binder = new CreateInstanceOfForeignObjectBinder(DynamicBindingGuru, esClass, selector);
						binderRegistry[selector] = binder;
					}
					return binder;
				}

			}

		}

		#endregion

	}

}
