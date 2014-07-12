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
			leftOutput = leftType == typeWithHighestGenerality ? leftOutput.withType(leftType) : leftOutput.withType(typeWithHighestGenerality);
			rightOutput = rightType == typeWithHighestGenerality ? rightOutput.withType(rightType) : rightOutput.withType(typeWithHighestGenerality);
			return true;
		}

		public static Object dictionaryAtIfAbsent <KeyType, ValueType> (IDictionary<KeyType, ValueType> dictionary, KeyType key, FuncNs.Func<Object> valueIfAbsent) {
			ValueType value;
			if (!dictionary.TryGetValue(key, out value)) {
				if (valueIfAbsent == null) return null;
				return valueIfAbsent();
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
				expressionArray[i++] = metaObject.asExpressionWithType(conversionType);
			}
			return expressionArray;
		}

		public static List<DynamicMetaObject> typeCompatibleArgumentsFor(ParameterInfo[] parameters, List<ArgumentBindingGuru> argGurus) {
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

		public static List<DynamicMetaObject> typeCompatibleArgumentsFor(MethodInfo methodInfo, List<ArgumentBindingGuru> argGurus) {
			return typeCompatibleArgumentsFor(methodInfo.GetParameters(), argGurus);
		}

		public static List<DynamicMetaObject> typeCompatibleArgumentsFor(ConstructorInfo constructorInfo, List<ArgumentBindingGuru> argGurus) {
			return typeCompatibleArgumentsFor(constructorInfo.GetParameters(), argGurus);
		}

		#endregion

		#endregion

		#endregion

		#region DynamicMetaObjects for general message sending

		public static DynamicMetaObject metaObjectToSendMessage(DynamicMetaObject receiver, ESObjectSpace objectSpace, ESBehavior esClass, ESSymbol selector, ESMethod method, DynamicMetaObject[] argumentsWithoutReceiver, BindingRestrictions bindingRestrictions) {

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
				EventInfo @event;
				MethodInfo methodInfo;
				DynamicMetaObject arg;
				ArgumentBindingGuru argGuru;
				Expression fieldExpression;
				Expression propertyExpression;
				expression = null;
				DynamicBindingGuru dynamicBindingGuru;
				List<DynamicMetaObject> typedArguments;
				switch (operation.Type) {
					case MethodOperationType.Convert:
						Type targetType = ESBehavior.typeFromAssemblyQualifiedName(operation.Operand, true);
						if (targetType == null) break;
						expression = receiver.asExpressionWithFormalType().withType(targetType);
						break;
					case MethodOperationType.GetField:
						field = esClass.getField(operation.Operand);
						if (field == null) break;
						expression = Expression.Field(esClass.IsHostSystemMetaclass ? null : receiver.asExpressionWithFormalType(), field).withCanonicalReturnType();
						break;
					case MethodOperationType.GetProperty:
						property = esClass.getReadableProperty(operation.Operand);
						if (property == null) break;
						expression = Expression.Property(esClass.IsHostSystemMetaclass ? null : receiver.asExpressionWithFormalType(), property).withCanonicalReturnType();
						break;
					case MethodOperationType.SetField:
						field = esClass.getField(operation.Operand);
						if (field == null) break;
						arg = argumentsWithoutReceiver[0];
						argGuru = arg.typeBindingGuru(objectSpace);
						arg = argGuru.metaObjectToConvertTo(field.FieldType);
						fieldExpression = Expression.Field(esClass.IsHostSystemMetaclass ? null : receiver.asExpressionWithFormalType(), field);
						expression = Expression.Block(Expression.Assign(fieldExpression, arg.asExpressionWithFormalType()), self);
						bindingRestrictions = bindingRestrictions.Merge(arg.Restrictions);
						break;
					case MethodOperationType.SetProperty:
						property = esClass.getWritableProperty(operation.Operand);
						if (property == null) break;
						arg = argumentsWithoutReceiver[0];
						argGuru = arg.typeBindingGuru(objectSpace);
						arg = argGuru.metaObjectToConvertTo(property.PropertyType);
						propertyExpression = Expression.Property(esClass.IsHostSystemMetaclass ? null : receiver.asExpressionWithFormalType(), property);
						expression = Expression.Block(Expression.Assign(propertyExpression, arg.asExpressionWithFormalType()), self);
						bindingRestrictions = bindingRestrictions.Merge(arg.Restrictions);
						break;
					case MethodOperationType.InvokeField:
						field = esClass.getField(operation.Operand);
						if (field == null) break;
						if (TypeGuru.delegateType.IsAssignableFrom(field.FieldType)) {
							fieldExpression = Expression.Field(esClass.IsHostSystemMetaclass ? null : receiver.asExpressionWithFormalType(), field);
							methodInfo = field.FieldType.GetMethod("Invoke");
							dynamicBindingGuru = objectSpace.DynamicBindingGuru;
							var argGurus = dynamicBindingGuru.dynamicMetaObjectArgumentGurusFor(argumentsWithoutReceiver);
							typedArguments = typeCompatibleArgumentsFor(methodInfo, argGurus);
							expression = Expression.Invoke(fieldExpression, expressionArrayFor(typedArguments.ToArray()));
							foreach(var argMo in typedArguments) bindingRestrictions = bindingRestrictions.Merge(argMo.Restrictions);
							if (methodInfo.ReturnType == TypeGuru.voidType) {
								expression = Expression.Block(TypeGuru.objectType, expression, self);
							} else {
								expression = expression.withCanonicalReturnType();
							}
						}
						break;
					case MethodOperationType.InvokeProperty:
						property = esClass.getReadableProperty(operation.Operand);
						if (property == null) break;
						if (TypeGuru.delegateType.IsAssignableFrom(property.PropertyType)) {
							propertyExpression = Expression.Property(esClass.IsHostSystemMetaclass ? null : receiver.asExpressionWithFormalType(), property);
							methodInfo = property.PropertyType.GetMethod("Invoke");
							dynamicBindingGuru = objectSpace.DynamicBindingGuru;
							var argGurus = dynamicBindingGuru.dynamicMetaObjectArgumentGurusFor(argumentsWithoutReceiver);
							typedArguments = typeCompatibleArgumentsFor(methodInfo, argGurus);
							expression = Expression.Invoke(propertyExpression, expressionArrayFor(typedArguments.ToArray()));
							foreach(var argMo in typedArguments) bindingRestrictions = bindingRestrictions.Merge(argMo.Restrictions);
							if (methodInfo.ReturnType == TypeGuru.voidType) {
								expression = Expression.Block(TypeGuru.objectType, expression, self);
							} else {
								expression = expression.withCanonicalReturnType();
							}
						}
						break;
					case MethodOperationType.InvokeEvent:
						var eventName = operation.Operand; 
						@event = esClass.getEvent(eventName);
						if (@event == null) break;
						var eventHandlerType = @event.EventHandlerType;
						Expression eventExpression = null;
						methodInfo = null;
						esClass.getReadablePropertyOrElseField(
								eventName, 
								(p) => {
									methodInfo = p.PropertyType.GetMethod("Invoke");
									eventExpression = Expression.Property(esClass.IsHostSystemMetaclass ? null : receiver.asExpressionWithFormalType(), p);
								}, 
								(f) => {
									methodInfo = f.FieldType.GetMethod("Invoke");
									eventExpression = Expression.Field(esClass.IsHostSystemMetaclass ? null : receiver.asExpressionWithFormalType(), f);
								});
						dynamicBindingGuru = objectSpace.DynamicBindingGuru;
						var eventArgGurus = dynamicBindingGuru.dynamicMetaObjectArgumentGurusFor(argumentsWithoutReceiver);
						typedArguments = typeCompatibleArgumentsFor(methodInfo, eventArgGurus);
						expression = Expression.Condition(
								Expression.ReferenceNotEqual(eventExpression, ExpressionTreeGuru.nilConstant),
									Expression.Invoke(eventExpression, expressionArrayFor(typedArguments.ToArray())),
									Expression.Empty());
						foreach(var argMo in typedArguments) bindingRestrictions = bindingRestrictions.Merge(argMo.Restrictions);
						if (methodInfo.ReturnType == TypeGuru.voidType) {
							expression = Expression.Block(TypeGuru.objectType, expression, self);
						} else {
							expression = expression.withCanonicalReturnType();
						}
						break;
					case MethodOperationType.InvokeMethod:
						dynamicBindingGuru = objectSpace.DynamicBindingGuru;
						if (dynamicBindingGuru.getMethodAndTypeCompatibleArgumentsFor(operation.Operand, esClass, argumentsWithoutReceiver, out methodInfo, out typedArguments)) {
							if (methodInfo.IsStatic) {
								expression = Expression.Call(
										null, 
										methodInfo, 
										expressionArrayFor(typedArguments.ToArray()));
							} else { 
								self = receiver.asExpressionWithFormalType();
								if (!methodInfo.DeclaringType.IsAssignableFrom(self.Type)) self = self.withType(methodInfo.DeclaringType);
								expression = Expression.Call(
										self, 
										methodInfo, 
										expressionArrayFor(typedArguments.ToArray()));
							}
							foreach(var argMo in typedArguments) bindingRestrictions = bindingRestrictions.Merge(argMo.Restrictions);
							if (methodInfo.ReturnType == TypeGuru.voidType) {
								expression = Expression.Block(TypeGuru.objectType, expression, self);
							} else {
								expression = expression.withCanonicalReturnType();
							}
						}
						break;
					case MethodOperationType.CreateInstance:
						ConstructorInfo constructorInfo;
						dynamicBindingGuru = objectSpace.DynamicBindingGuru;
						var instanceClass = esClass is ESMetaclass ? ((ESMetaclass)esClass).CanonicalInstance : esClass;
						if (dynamicBindingGuru.getConstructorAndTypeCompatibleArgumentsFor(instanceClass, argumentsWithoutReceiver, out constructorInfo, out typedArguments)) {
							expression = Expression.New(constructorInfo, expressionArrayFor(typedArguments.ToArray())).withCanonicalReturnType();
							foreach(var argMo in typedArguments) bindingRestrictions = bindingRestrictions.Merge(argMo.Restrictions);
						}
						break;
					default:
						break;
				}
				if (expression == null) expression = operation.OnFailExpression;
			}
			return new DynamicMetaObject(expression, bindingRestrictions, receiver.Value);

		}

		#endregion

		#endregion

		protected ESObjectSpace							objectSpace							= null;
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

		public DynamicBindingGuru(ESObjectSpace objectSpace) {
			this.objectSpace	= objectSpace;
			initialize();
		}

		protected virtual void initialize() {
			symbolRegistry							= objectSpace.SymbolRegistry;
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

		public ESObjectSpace ObjectSpace {
			get {return objectSpace;}
		}

		#region Canonical Binders

		internal GetPropertyOrFieldOfForeignObjectBinder canonicalGetMemberBinderFor(ESBehavior esClass, ESSymbol selector) {
			return getPropertyOrFieldOfForeignObjectBinderRegistry.canonicalBinderFor(esClass, selector);
		}

		internal GetPropertyOrFieldOfForeignObjectBinder canonicalGetMemberBinderFor(ESBehavior esClass, ESSymbol selector, String name) {
			return getPropertyOrFieldOfForeignObjectBinderRegistry.canonicalBinderFor(esClass, selector, name);
		}

		internal SetPropertyOrFieldOfForeignObjectBinder canonicalSetMemberBinderFor(ESBehavior esClass, ESSymbol selector) {
			return setPropertyOrFieldOfForeignObjectBinderRegistry.canonicalBinderFor(esClass, selector);
		}

		internal SetPropertyOrFieldOfForeignObjectBinder canonicalSetMemberBinderFor(ESBehavior esClass, ESSymbol selector, String name) {
			return setPropertyOrFieldOfForeignObjectBinderRegistry.canonicalBinderFor(esClass, selector, name);
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

		public SymbolRegistry SymbolRegistry {
			get {return symbolRegistry;}
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

		#endregion

		#region Argument / Method Binding

		public List<ArgumentBindingGuru> dynamicMetaObjectArgumentGurusFor(IEnumerable<DynamicMetaObject> metaObjects) {
			var argGurus = new List<ArgumentBindingGuru>();
			foreach (var mo in metaObjects) argGurus.Add(mo.typeBindingGuru(objectSpace));
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
				typedArguments = emptyArgList;
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

		#endregion

		#region Constructing DynamicMetaObjects for specific functions

		public DynamicMetaObject metaObjectForInvariantOperation(Expression invariantRestrictionOperation, Object value) {
			return new DynamicMetaObject(invariantRestrictionOperation.withCanonicalReturnType(), BindingRestrictionsGuru.invariantRestriction, value);
		}

		public DynamicMetaObject metaObjectForInstanceRestrictedOperation(Expression self, Expression instanceRestrictedOperation, Object value) {
			return new DynamicMetaObject(instanceRestrictedOperation.withCanonicalReturnType(), BindingRestrictionsGuru.restrictionFor(self, value), value);
		}

		public DynamicMetaObject metaObjectForTypeRestrictedOperation(Expression typeRestrictedOperation, Type restrictionType, Object value) {
			return new DynamicMetaObject(typeRestrictedOperation.withCanonicalReturnType(), BindingRestrictionsGuru.restrictionFor(typeRestrictedOperation, restrictionType), value);
		}

		public DynamicMetaObject metaObjectForForeignObjectOperation(DynamicMetaObject receiver, ESBehavior esClass, Expression invariantOperationExpression) {
			return new DynamicMetaObject(invariantOperationExpression.withCanonicalReturnType(), receiver.bindingRestrictionsForForeignObjectReceiver(esClass), receiver.Value);
		}

		public DynamicMetaObject metaObjectToCreateAssociation(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject valueMO) {
			Expression expression = ExpressionTreeGuru.expressionToCreateESAssociation(objectSpace.AssociationClass, receiver.Expression, valueMO.Expression);
			return new DynamicMetaObject(
				expression.withCanonicalReturnType(), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass), 
				receiver.Value);
		}

		public DynamicMetaObject metaObjectForConditionalAnd(DynamicMetaObject receiver, ESBehavior esClass, DynamicMetaObject operandMO) {
			var model = receiver.Value;
			var self = receiver.asExpressionWithType(TypeGuru.boolType);
			operandMO = operandMO.BindInvoke(canonicalInvokeBinderFor(objectSpace.classOf(operandMO.Value), selectorValue0), emptyArgArray);
			var operand = operandMO.asExpressionWithType(TypeGuru.boolType);
			return new DynamicMetaObject(
				Expression.AndAlso(self, operand).withCanonicalReturnType(), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass).Merge(operandMO.asInstanceRestriction()), 
				model);
		}

		public DynamicMetaObject metaObjectForConditionalOr(DynamicMetaObject receiver, ESBehavior esClass, DynamicMetaObject operandMO) {
			var model = receiver.Value;
			var self = receiver.asExpressionWithType(TypeGuru.boolType);
			operandMO = operandMO.BindInvoke(canonicalInvokeBinderFor(objectSpace.classOf(operandMO.Value), selectorValue0), emptyArgArray);
			var operand = operandMO.asExpressionWithType(TypeGuru.boolType);
			return new DynamicMetaObject(
				Expression.OrElse(self, operand).withCanonicalReturnType(), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass).Merge(operandMO.asInstanceRestriction()), 
				model);
		}

		public DynamicMetaObject metaObjectToInvokeArgIfTrue(DynamicMetaObject receiver, ESBehavior esClass, Expression testExpression, DynamicMetaObject actionToInvoke, Expression testFailResult) {
			var model = receiver.Value;
			var self = receiver.Expression;
			var actionFunction = actionToInvoke.BindInvoke(canonicalInvokeBinderFor(objectSpace.classOf(actionToInvoke.Value), selectorValue0), emptyArgArray);
			return new DynamicMetaObject(
				Expression.Condition(
					testExpression.withType(TypeGuru.boolType), 
					actionFunction.Expression, 
					testFailResult).withCanonicalReturnType(), 
				(model == null ?
					receiver.addingRestrictions(ExpressionTreeGuru.expressionToTestThatNilHasSameClassVersion(self, objectSpace.UndefinedObjectClass)).Merge(actionToInvoke.asInstanceRestriction()) :
					receiver.bindingRestrictionsForForeignObjectReceiver(esClass)).Merge(actionToInvoke.asInstanceRestriction()), 
				model);
		}

		public DynamicMetaObject metaObjectToInvokeArgIfFalse(DynamicMetaObject receiver, ESBehavior esClass, Expression testExpression, DynamicMetaObject actionToInvoke, Expression testFailResult) {
			var model = receiver.Value;
			var self = receiver.Expression;
			var actionFunction = actionToInvoke.BindInvoke(canonicalInvokeBinderFor(objectSpace.classOf(actionToInvoke.Value), selectorValue0), emptyArgArray);
			return new DynamicMetaObject(
				Expression.Condition(
					testExpression.withType(TypeGuru.boolType), 
					testFailResult, 
					actionFunction.Expression).withCanonicalReturnType(), 
				(model == null ?
					receiver.addingRestrictions(ExpressionTreeGuru.expressionToTestThatNilHasSameClassVersion(self, objectSpace.UndefinedObjectClass)).Merge(actionToInvoke.asInstanceRestriction()) :
					receiver.bindingRestrictionsForForeignObjectReceiver(esClass)).Merge(actionToInvoke.asInstanceRestriction()), 
				model);
		}

		public DynamicMetaObject metaObjectToInvokeArgIfTrueAndArgIfFalse(DynamicMetaObject receiver, ESBehavior esClass, Expression testExpression, DynamicMetaObject actionToInvokeIfTrue, DynamicMetaObject actionToInvokeIfFalse) {
			var model = receiver.Value;
			var self = receiver.Expression;
			var ifTrueAction = actionToInvokeIfTrue.BindInvoke(canonicalInvokeBinderFor(objectSpace.classOf(actionToInvokeIfTrue.Value), selectorValue0), emptyArgArray);
			var ifFalseAction = actionToInvokeIfFalse.BindInvoke(canonicalInvokeBinderFor(objectSpace.classOf(actionToInvokeIfFalse.Value), selectorValue0), emptyArgArray);
			return new DynamicMetaObject(
				Expression.Condition(
					testExpression.withType(TypeGuru.boolType), 
					ifTrueAction.Expression, 
					ifFalseAction.Expression).withCanonicalReturnType(), 
				(model == null ?
					receiver.addingRestrictions(ExpressionTreeGuru.expressionToTestThatNilHasSameClassVersion(self, objectSpace.UndefinedObjectClass)).Merge(actionToInvokeIfTrue.asInstanceRestriction()).Merge(actionToInvokeIfFalse.asInstanceRestriction()) :
					receiver.bindingRestrictionsForForeignObjectReceiver(esClass)).Merge(actionToInvokeIfTrue.asInstanceRestriction()).Merge(actionToInvokeIfFalse.asInstanceRestriction()), 
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
				expression.withCanonicalReturnType(), 
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
				expression.withCanonicalReturnType(), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass), 
				receiver.Value);
		}

		public DynamicMetaObject metaObjectToComputeCeilingOf(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector) {
			Expression expression;
			var receiverType = receiver.LimitType;
			if (receiverType.isNumeric()) {
				if (receiverType.isRational()) {
					expression = ExpressionTreeGuru.expressionToInvoke_Math_Ceiling(receiver.asExpressionWithFormalType(), receiverType).withType(TypeGuru.longType);
				} else {
					expression = receiver.asExpressionWithFormalType();
				}
			} else {
				expression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(receiver.Expression, esClass, selector, emptyArgArray);
			}
			return new DynamicMetaObject(
				expression.withCanonicalReturnType(), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass), 
				receiver.Value);
		}

		public DynamicMetaObject metaObjectToComputeFloorOf(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector) {
			Expression expression;
			var receiverType = receiver.LimitType;
			if (receiverType.isNumeric()) {
				if (receiverType.isRational()) {
					expression = ExpressionTreeGuru.expressionToInvoke_Math_Floor(receiver.asExpressionWithFormalType(), receiverType).withType(TypeGuru.longType);
				} else {
					expression = receiver.asExpressionWithFormalType();
				}
			} else {
				expression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(receiver.Expression, esClass, selector, emptyArgArray);
			}
			return new DynamicMetaObject(
				expression.withCanonicalReturnType(), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass), 
				receiver.Value);
		}

		public DynamicMetaObject metaObjectToRoundANumber(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector) {
			Expression expression;
			var receiverType = receiver.LimitType;
			if (receiverType.isNumeric()) {
				if (receiverType.isRational()) {
					expression = ExpressionTreeGuru.expressionToInvoke_Math_Round(receiver.asExpressionWithFormalType(), receiverType).withType(TypeGuru.longType);
				} else {
					expression = receiver.asExpressionWithFormalType();
				}
			} else {
				expression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(receiver.Expression, esClass, selector, emptyArgArray);
			}
			return new DynamicMetaObject(
				expression.withCanonicalReturnType(), 
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
				expression.withCanonicalReturnType(), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass), 
				receiver.Value);
		}

		public DynamicMetaObject metaObjectToRoundANumberTo(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject modulusMO) {
			Expression expression;
			var receiverType = receiver.LimitType;
			var modulusType = modulusMO.LimitType;
			var operationType = receiverType.typeWithHighestNumericGenerality(modulusType);	
			if (operationType != null) {
				expression = ExpressionTreeGuru.expressionToRoundANumberTo(
						receiver.asExpressionWithFormalType(), 
						modulusMO.asExpressionWithFormalType()).withType(operationType);
			} else {
				expression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(receiver.Expression, esClass, selector, argArrayFor(modulusMO));
			}
			return new DynamicMetaObject(
				expression.withCanonicalReturnType(), 
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
				expression.withCanonicalReturnType(), 
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
				expression.withCanonicalReturnType(), 
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
				expression.withCanonicalReturnType(), 
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
				expression.withCanonicalReturnType(), 
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
				var self = receiver.asExpressionWithFormalType().withType(operationType);
				var divisor = divisorMO.asExpressionWithFormalType().withType(operationType);
				expression = Expression.Divide(self, divisor);
				if (!operationType.isInteger()) {
					expression = expression.withType(TypeGuru.longType);
				}
			} else {
				expression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(receiver.Expression, esClass, selector, argArrayFor(divisorMO));
			}
			return new DynamicMetaObject(
				expression.withCanonicalReturnType(), 
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
				var self = receiver.asExpressionWithFormalType().withType(operationType);
				var divisor = divisorMO.asExpressionWithFormalType().withType(operationType);
				expression = Expression.Divide(self, divisor);
				expression = ExpressionTreeGuru.expressionToInvoke_Math_Floor(expression, operationType);
				expression = expression.withType(TypeGuru.longType);
			} else {
				expression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(receiver.Expression, esClass, selector, argArrayFor(divisorMO));
			}
			return new DynamicMetaObject(
				expression.withCanonicalReturnType(), 
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
				var self = receiver.asExpressionWithFormalType().withType(TypeGuru.doubleType);
				var modulus = modulusMO.asExpressionWithFormalType().withType(TypeGuru.doubleType);
				expression = Expression.Divide(self, modulus);
				expression = ExpressionTreeGuru.expressionToInvoke_Math_Floor(expression, TypeGuru.doubleType);
				expression = expression.withType(TypeGuru.longType);
				expression = Expression.Multiply(expression.withType(TypeGuru.doubleType), modulus);
				expression = Expression.Subtract(self, expression);
				expression = expression.withType(nominalOperationType);
			} else {
				expression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(receiver.Expression, esClass, selector, argArrayFor(modulusMO));
			}
			return new DynamicMetaObject(
				expression.withCanonicalReturnType(), 
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
				var actionFunction = actionToRepeat.BindInvoke(canonicalInvokeBinderFor(objectSpace.classOf(actionToRepeat.Value), selectorValue0), emptyArgArray);
				var induction = Expression.Parameter(TypeGuru.longType, "$induction");
				var step = Expression.Constant(1L);
				var limit =self.withType(TypeGuru.longType);
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
						       exit)).withCanonicalReturnType();
				restrictions = receiver.bindingRestrictionsForForeignObjectReceiver(esClass).Merge(actionToRepeat.asInstanceRestriction());
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
				startingValue = Expression.Subtract(startingValue, Expression.Constant(1).withType(startingValueType));
				endingValue = Expression.Subtract(endingValue, Expression.Constant(1).withType(endingValueType));
			}

			typeWithHighestGenerality = candidateType;
			var induction = Expression.Parameter(typeWithHighestGenerality, "$induction");
			Expression inductionVariable;
			if (inductionParameter.Type != typeWithHighestGenerality) {
				inductionVariable = induction.withType(inductionParameter.Type);
			} else {
				inductionVariable = induction;
			}
			var generalStartingValue = startingValue.withType(typeWithHighestGenerality);
			var generalEndingValue = endingValue.withType(typeWithHighestGenerality);
			var generalStepValue = stepValue.withType(typeWithHighestGenerality);
			var upwardLoopExit = Expression.Label(typeWithHighestGenerality);
			var downwardLoopExit = Expression.Label(typeWithHighestGenerality);
			expression = Expression.Block(
				TypeGuru.objectType,
				new[] {induction, inductionParameter},
				Expression.Assign(induction, generalStartingValue),
				Expression.IfThenElse(
					Expression.GreaterThanOrEqual(generalStepValue, ExpressionTreeGuru.zeroConstant.withType(typeWithHighestGenerality)),
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
			return expression.withCanonicalReturnType();
		}

		public DynamicMetaObject metaObjectForToByDo(DynamicMetaObject startingValueMO, ESBehavior esClass, ESSymbol selector, DynamicMetaObject endingValueMO, DynamicMetaObject stepValueMO, DynamicMetaObject enumeratorMO) {

			BindingRestrictions restrictions = null;
			var model = startingValueMO.Value;
			var startingValue = startingValueMO.Expression;
			var inductionVariable = Expression.Parameter(TypeGuru.objectType, "$inductionVariable");
			var inductionVariableMO = inductionVariable.asDynamicMetaObject();
			var enumerator = enumeratorMO.BindInvoke(canonicalInvokeBinderFor(objectSpace.classOf(enumeratorMO.Value), selectorValue1), argArrayFor(inductionVariableMO));
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
						.Merge(enumeratorMO.asInstanceRestriction());
			}

			return new DynamicMetaObject(expression, restrictions, model);

		}

		public DynamicMetaObject metaObjectToInvoke_OnEventDo(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject eventNameMO, DynamicMetaObject actionMO) {
			var model = receiver.Value;
			var self = receiver.asExpressionWithFormalType();
			var receiverType = receiver.LimitType;
			var eventNameString = ESObject.asHostString(eventNameMO.Value);
			var @event = receiverType.GetEvent(eventNameString, esClass.HostObjectMethodInvokeBindingFlags);
			var handlerType = @event.EventHandlerType;
			var addMethod = @event.GetAddMethod(true);
			Expression expression;
			Expression action;
			var actionIsBlock = actionMO.isBlock();
			if (actionIsBlock) { 
				var block = (ESBlock)actionMO.Value;
				var avatar = block.avatarWithType(handlerType);
				if (avatar == null) { 
					var argGuru = actionMO.typeBindingGuru(objectSpace);
					var typedActionMO = argGuru.metaObjectToConvertTo(handlerType);
					action = typedActionMO.Expression;
					var actionVar = Expression.Parameter(handlerType, "$action");
					expression = Expression.Block(
							new ParameterExpression[]{actionVar},
							Expression.Assign(actionVar, action),
							Expression.Call(actionMO.asExpressionWithFormalType(), "addAvatar", null, actionVar),
							Expression.Call(self, addMethod, new Expression[]{actionVar}), 
							self.withCanonicalReturnType());
				} else {
					action = Expression.Constant(avatar);
					expression = Expression.Block(
							Expression.Call(self, addMethod, new Expression[]{action}), 
							self.withCanonicalReturnType());
				}
			} else {
				action = actionMO.asExpressionWithType(handlerType);
				expression = Expression.Block(
						Expression.Call(self, addMethod, action), 
						self.withCanonicalReturnType());
			}

			return expression.asDynamicMetaObject(
					receiver.restrictionThatForeignObjectHasSameClassVersion(esClass).Merge(eventNameMO.asInstanceRestriction()).Merge(actionMO.asInstanceRestriction()),
					model);
		}

		public DynamicMetaObject metaObjectToInvoke_OnEventDoNotDo(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject eventNameMO, DynamicMetaObject actionMO) {
			var model = receiver.Value;
			var self = receiver.asExpressionWithFormalType();
			var receiverType = receiver.LimitType;
			var eventNameString = ESObject.asHostString(eventNameMO.Value);
			var @event = receiverType.GetEvent(eventNameString, esClass.HostObjectMethodInvokeBindingFlags);
			var eventField = receiverType.GetField(eventNameString, BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance);
			var invocationArray = ((Delegate)eventField.GetValue(model)).GetInvocationList();
			var handlerType = @event.EventHandlerType;
			var removeMethod = @event.GetRemoveMethod(true);
			Expression expression;
			Expression action;
			var actionIsBlock = actionMO.isBlock();
			if (actionIsBlock) { 
				var block = (ESBlock)actionMO.Value;
				var avatar = block.avatarWithType(handlerType);
				if (avatar == null) {
					action = actionMO.Expression;
				} else { 
					action = Expression.Constant(avatar);
				}
			} else {
				action = actionMO.Expression;
			}

			expression = Expression.Block(
					Expression.Call(self, removeMethod, action), 
					self.withCanonicalReturnType());

			return expression.asDynamicMetaObject(
					receiver.restrictionThatForeignObjectHasSameClassVersion(esClass).Merge(eventNameMO.asInstanceRestriction()).Merge(actionMO.asInstanceRestriction()),
					model);
		}

		public DynamicMetaObject metaObjectToInvokeWithEnsureBlock(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject ensureBlockMO) {
			var invokeProtectedBlockMO = receiver.BindInvoke(canonicalInvokeBinderFor(esClass, selectorValue0), emptyArgArray);
			var invokeEnsureBlockMO = ensureBlockMO.BindInvoke(canonicalInvokeBinderFor(objectSpace.classOf(ensureBlockMO.Value), selectorValue0), emptyArgArray);
			Expression expression = Expression.TryFinally(invokeProtectedBlockMO.Expression, invokeEnsureBlockMO.Expression);
			expression = Expression.Block(TypeGuru.objectType, expression.withCanonicalReturnType());
			return new DynamicMetaObject(
				expression, 
				(esClass.InstanceArchitecture == ObjectStateArchitecture.HostSystemObject ?
					receiver.bindingRestrictionsForForeignObjectReceiver(esClass) :
					receiver.bindingRestrictionsForESObjectReceiver(esClass)).Merge(ensureBlockMO.asInstanceRestriction()), 
				receiver.Value);
		}

		public DynamicMetaObject metaObjectToInvokeWithIfCurtailedBlock(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject ifCurtailedBlockMO) {
			var invokeProtectedBlockMO = receiver.BindInvoke(canonicalInvokeBinderFor(esClass, selectorValue0), emptyArgArray);
			var invokeIfCurtailedBlockMO = ifCurtailedBlockMO.BindInvoke(canonicalInvokeBinderFor(objectSpace.classOf(ifCurtailedBlockMO.Value), selectorValue0), emptyArgArray);
			var catchBlock = Expression.Catch(TypeGuru.exceptionType, Expression.Block(invokeIfCurtailedBlockMO.Expression, Expression.Rethrow(), Expression.Constant(new Object())));
			Expression expression = Expression.TryCatch(invokeProtectedBlockMO.Expression, catchBlock);
			expression = Expression.Block(TypeGuru.objectType, expression.withCanonicalReturnType());
			return new DynamicMetaObject(
				expression, 
				(esClass.InstanceArchitecture == ObjectStateArchitecture.HostSystemObject ?
					receiver.bindingRestrictionsForForeignObjectReceiver(esClass) :
					receiver.bindingRestrictionsForESObjectReceiver(esClass)).Merge(ifCurtailedBlockMO.asInstanceRestriction()), 
				receiver.Value);
		}

		public DynamicMetaObject metaObjectToInvokeWithExceptionHandler(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject exceptionSelectorMO, DynamicMetaObject handleExceptionBlockMO) {
			var invokeProtectedBlockMO = receiver.BindInvoke(canonicalInvokeBinderFor(esClass, selectorValue0), emptyArgArray);
			ParameterExpression exception;
			DynamicMetaObject exceptionMO;
			DynamicMetaObject [] exceptionArgArray;
			DynamicMetaObject invokeIfCurtailedBlockMO;
			CatchBlock catchBlock;
			var exceptionSelectorClasss = objectSpace.classOf(exceptionSelectorMO.Value);
			if (exceptionSelectorClasss.IsHostSystemMetaclass) {
				var exceptionClass = ((ESMetaclass)exceptionSelectorClasss).CanonicalInstance;
				var exceptionType = exceptionClass.InstanceType;
				exception = Expression.Parameter(exceptionType, "ex");
				exceptionMO = exception.asDynamicMetaObject();
				exceptionArgArray = argArrayFor(exceptionMO);
				invokeIfCurtailedBlockMO = handleExceptionBlockMO.BindInvoke(canonicalInvokeBinderFor(objectSpace.classOf(handleExceptionBlockMO.Value), selectorValue1), exceptionArgArray);
				catchBlock = Expression.Catch(exception, invokeIfCurtailedBlockMO.Expression);
			} else { 
				exception = Expression.Parameter(TypeGuru.exceptionType, "ex");
				exceptionMO = exception.asDynamicMetaObject();
				exceptionArgArray = argArrayFor(exceptionMO);
				invokeIfCurtailedBlockMO = handleExceptionBlockMO.BindInvoke(canonicalInvokeBinderFor(objectSpace.classOf(handleExceptionBlockMO.Value), selectorValue1), exceptionArgArray);
				var exceptionSelectionPredicateMO = exceptionSelectorMO.BindInvokeMember(
									canonicalInvokeMemberBinderFor(
										exceptionSelectorClasss, 
										objectSpace.symbolFor("handles:"), 
										"Handles"), 
									exceptionArgArray);
				var exceptionSelectionPredicate = exceptionSelectionPredicateMO.asExpressionWithType(TypeGuru.objectType).withType(TypeGuru.boolType);
				var conditionalExceptionHandler = Expression.Condition(
										exceptionSelectionPredicate,
											invokeIfCurtailedBlockMO.asExpressionWithType(TypeGuru.objectType),
											Expression.Block(TypeGuru.objectType, Expression.Rethrow(), Expression.Constant(new Object())));
				catchBlock = Expression.Catch(exception, conditionalExceptionHandler);
			}
			Expression expression = Expression.TryCatch(invokeProtectedBlockMO.Expression, catchBlock);
			expression = Expression.Block(TypeGuru.objectType, expression.withCanonicalReturnType());
			return new DynamicMetaObject(
				expression, 
				(esClass.InstanceArchitecture == ObjectStateArchitecture.HostSystemObject ?
					receiver.bindingRestrictionsForForeignObjectReceiver(esClass) :
					receiver.bindingRestrictionsForESObjectReceiver(esClass))
							.Merge(handleExceptionBlockMO.asInstanceRestriction())
							.Merge(exceptionSelectorMO.asInstanceRestriction())
							.Merge(handleExceptionBlockMO.asInstanceRestriction()), 
				receiver.Value);


		}

		public DynamicMetaObject metaObjectForWhileNil(DynamicMetaObject receiver, ESBehavior esClass, DynamicMetaObject actionToRepeat) {
			var model = receiver.Value;
			var self = receiver.Expression;

			var testFunction = receiver.BindInvoke(canonicalInvokeBinderFor(esClass, selectorValue0), emptyArgArray);
			var actionFunction = actionToRepeat == null ?
				doNothing :
				actionToRepeat.BindInvoke(canonicalInvokeBinderFor(objectSpace.classOf(actionToRepeat.Value), selectorValue0), emptyArgArray);
			var exit = Expression.Label(TypeGuru.objectType);
			var loop = Expression.Loop(
					Expression.IfThenElse(
						Expression.ReferenceEqual(testFunction.Expression, ExpressionTreeGuru.nilConstant),
							actionFunction.Expression,
							Expression.Break(exit, self)),
					exit);

			return new DynamicMetaObject(
				loop.withCanonicalReturnType(), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass).Merge(actionToRepeat.asInstanceRestriction()), 
				model);
		}

		public DynamicMetaObject metaObjectForWhileNotNil(DynamicMetaObject receiver, ESBehavior esClass, DynamicMetaObject actionToRepeat) {
			var model = receiver.Value;
			var self = receiver.Expression;

			var testFunction = receiver.BindInvoke(canonicalInvokeBinderFor(esClass, selectorValue0), emptyArgArray);
			var actionFunction = actionToRepeat == null ?
				doNothing :
				actionToRepeat.BindInvoke(canonicalInvokeBinderFor(objectSpace.classOf(actionToRepeat.Value), selectorValue0), emptyArgArray);
			var exit = Expression.Label(TypeGuru.objectType);
			var loop = Expression.Loop(
					Expression.IfThenElse(
						Expression.ReferenceEqual(testFunction.Expression, ExpressionTreeGuru.nilConstant),
							Expression.Break(exit, self),
							actionFunction.Expression),
					exit);

			return new DynamicMetaObject(
				loop.withCanonicalReturnType(), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass).Merge(actionToRepeat.asInstanceRestriction()), 
				model);
		}

		public DynamicMetaObject metaObjectForWhileTrue(DynamicMetaObject receiver, ESBehavior esClass, DynamicMetaObject actionToRepeat) {
			var model = receiver.Value;
			var self = receiver.Expression;

			var testFunction = receiver.BindInvoke(canonicalInvokeBinderFor(esClass, selectorValue0), emptyArgArray);
			var actionFunction = actionToRepeat == null ?
				doNothing :
				actionToRepeat.BindInvoke(canonicalInvokeBinderFor(objectSpace.classOf(actionToRepeat.Value), selectorValue0), emptyArgArray);
			var exit = Expression.Label(TypeGuru.objectType);
			var loop = Expression.Loop(
					Expression.IfThenElse(
						testFunction.asExpressionWithType(TypeGuru.boolType),
							actionFunction.Expression,
							Expression.Break(exit, self)),
					exit);

			return new DynamicMetaObject(
				loop.withCanonicalReturnType(), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass).Merge(actionToRepeat.asInstanceRestriction()), 
				model);
		}

		public DynamicMetaObject metaObjectForWhileFalse(DynamicMetaObject receiver, ESBehavior esClass, DynamicMetaObject actionToRepeat) {
			var model = receiver.Value;
			var self = receiver.Expression;

			var testFunction = receiver.BindInvoke(canonicalInvokeBinderFor(esClass, selectorValue0), emptyArgArray);
			var actionFunction = actionToRepeat == null ?
				doNothing :
				actionToRepeat.BindInvoke(canonicalInvokeBinderFor(objectSpace.classOf(actionToRepeat.Value), selectorValue0), emptyArgArray);
			var exit = Expression.Label(TypeGuru.objectType);
			var loop = Expression.Loop(
					Expression.IfThenElse(
						testFunction.asExpressionWithType(TypeGuru.boolType),
							Expression.Break(exit, self),
							actionFunction.Expression),
						exit);

			return new DynamicMetaObject(
				loop.withCanonicalReturnType(), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass).Merge(actionToRepeat.asInstanceRestriction()), 
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
				invokeEnumeratorBlock = enumeratorBlockMO.BindInvoke(canonicalInvokeBinderFor(objectSpace.classOf(enumeratorBlockMO.Value), selectorValue1), argArrayFor(elementMO)).Expression;
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
				invokeEnumeratorBlock = enumeratorBlockMO.BindInvoke(canonicalInvokeBinderFor(objectSpace.classOf(enumeratorBlockMO.Value), selectorValue1), argArrayFor(valueMO)).Expression;

				var currentProperty = enumeratorType.GetProperty("Current", BindingFlags.Instance | BindingFlags.GetProperty | BindingFlags.Public);
				var keyValuePairType = currentProperty.PropertyType;
				// var keyValuePairArgs = keyValuePairType.GetGenericArguments();

				var enumerator = Expression.Parameter(enumeratorType, "$enumerator");
				var assignEnumerator = Expression.Assign(enumerator, Expression.Call(self, getEnumeratorMethod));
				Expression getKeyValuePair = Expression.Property(enumerator, currentProperty);
				var keyValuePair = Expression.Parameter(keyValuePairType, "$keyValuePair");
				var assignKeyValuePair = Expression.Assign(keyValuePair, getKeyValuePair);
				Expression getValue = Expression.Property(keyValuePair, "Value");
				if (getValue.Type != TypeGuru.objectType) getValue = getValue.withType(TypeGuru.objectType);
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
				invokeEnumeratorBlock = enumeratorBlockMO.BindInvoke(canonicalInvokeBinderFor(objectSpace.classOf(enumeratorBlockMO.Value), selectorValue1), argArrayFor(elementMO)).Expression;

				var enumerator = Expression.Parameter(enumeratorType, "$enumerator");
				var assignEnumerator = Expression.Assign(enumerator, Expression.Call(self, getEnumeratorMethod));
				Expression getElement = Expression.Property(enumerator, "Current");
				if (getElement.Type != TypeGuru.objectType) getElement = getElement.withType(TypeGuru.objectType);
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
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass).Merge(enumeratorBlockMO.asInstanceRestriction()), 
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
			var enumerator = enumeratorBlockMO.BindInvoke(canonicalInvokeBinderFor(objectSpace.classOf(enumeratorBlockMO.Value), selectorValue1), argArrayFor(elementMO));
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
						.Merge(enumeratorBlockMO.asInstanceRestriction());
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
				invokeEnumeratorBlock = enumeratorBlockMO.BindInvoke(canonicalInvokeBinderFor(objectSpace.classOf(enumeratorBlockMO.Value), selectorValue1), argArrayFor(elementMO)).Expression;

				var enumerator = Expression.Parameter(enumeratorType, "$enumerator");
				var assignEnumerator = Expression.Assign(enumerator, Expression.Call(self, getEnumeratorMethod));
				Expression getElement = Expression.Property(enumerator, "Current");
				if (getElement.Type != TypeGuru.objectType) getElement = getElement.withType(TypeGuru.objectType);
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
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass).Merge(enumeratorBlockMO.asInstanceRestriction()), 
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
				invokeEnumeratorBlock = enumeratorBlockMO.BindInvoke(canonicalInvokeBinderFor(objectSpace.classOf(enumeratorBlockMO.Value), selectorValue1), argArrayFor(keyMo)).Expression;

				var currentProperty = enumeratorType.GetProperty("Current", BindingFlags.Instance | BindingFlags.GetProperty | BindingFlags.Public);
				var keyValuePairType = currentProperty.PropertyType;
				// var keyValuePairArgs = keyValuePairType.GetGenericArguments();

				var enumerator = Expression.Parameter(enumeratorType, "$enumerator");
				var assignEnumerator = Expression.Assign(enumerator, Expression.Call(self, getEnumeratorMethod));
				Expression getKeyValuePair = Expression.Property(enumerator, currentProperty);
				var keyValuePair = Expression.Parameter(keyValuePairType, "$keyValuePair");
				var assignKeyValuePair = Expression.Assign(keyValuePair, getKeyValuePair);
				Expression getKey = Expression.Property(keyValuePair, "Key");
				if (getKey.Type != TypeGuru.objectType) getKey = getKey.withType(TypeGuru.objectType);
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
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass).Merge(enumeratorBlockMO.asInstanceRestriction()), 
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
				invokeEnumeratorBlock = enumeratorBlockMO.BindInvoke(canonicalInvokeBinderFor(objectSpace.classOf(enumeratorBlockMO.Value), symbolFor("value:value:")), argArrayFor(keyMo, valueMo)).Expression;

				var currentProperty = enumeratorType.GetProperty("Current", BindingFlags.Instance | BindingFlags.GetProperty | BindingFlags.Public);
				var keyValuePairType = currentProperty.PropertyType;
				// var keyValuePairArgs = keyValuePairType.GetGenericArguments();

				var enumerator = Expression.Parameter(enumeratorType, "$enumerator");
				var assignEnumerator = Expression.Assign(enumerator, Expression.Call(self, getEnumeratorMethod));
				Expression getKeyValuePair = Expression.Property(enumerator, currentProperty);
				var keyValuePair = Expression.Parameter(keyValuePairType, "$keyValuePair");
				var assignKeyValuePair = Expression.Assign(keyValuePair, getKeyValuePair);
				Expression getKey = Expression.Property(keyValuePair, "Key");
				if (getKey.Type != TypeGuru.objectType) getKey = getKey.withType(TypeGuru.objectType);
				Expression getValue = Expression.Property(keyValuePair, "Value");
				if (getValue.Type != TypeGuru.objectType) getValue = getValue.withType(TypeGuru.objectType);
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
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass).Merge(enumeratorBlockMO.asInstanceRestriction()), 
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
				var suffixClass = objectSpace.classOf(suffixMO.Value);
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
				esPrefix = ExpressionTreeGuru.expressionToCreateESStringFromString(objectSpace, self.withType(TypeGuru.stringType));
				expression = ExpressionTreeGuru.expressionToInvokeESMethod(objectSpace.StringClass.compiledMethodAt(symbolFor(",")), new Expression[]{esPrefix, esSuffix});
			} else {
				expression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(self, esClass, selector, argArrayFor(suffixMO));
			}
			return new DynamicMetaObject(
				expression.withCanonicalReturnType(), 
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
				var keyGuru = keyMO.typeBindingGuru(objectSpace);
				typedKeyMO = keyGuru.metaObjectToConvertTo(keyType);
			}

			key = typedKeyMO.Expression;

			var ifAbsentFunctor = ExpressionTreeGuru.expressionToConvertCompiledCodeOrDelegateToFunctor(ifAbsentBlockMO.asExpressionWithFormalType(), ESCompiledCode.blockFunctionTypeForNumArgs(0));

			return new DynamicMetaObject(
				Expression.Call(methodInfo, self, key, ifAbsentFunctor).withCanonicalReturnType(), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass, keyMO, ifAbsentBlockMO), 
				model);
			
		}

		public DynamicMetaObject metaObjectForAtIfAbsentPut(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject keyMO, DynamicMetaObject ifAbsentValueBlockMO) {
			var model = receiver.Value;
			var self = receiver.asExpressionWithFormalType();
			var receiverType = receiver.LimitType;
			Expression key = Expression.Parameter(TypeGuru.objectType, "key");
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
				typedValueMO = typedArguments[1];
			}

			var parameters = setItemMethodInfo.GetParameters();
			var keyType = parameters[0].ParameterType;
			var valueType = parameters[1].ParameterType;

			methodInfo = typeof(DynamicBindingGuru).GetMethod("dictionaryAtIfAbsent", BindingFlags.Static | BindingFlags.Public);
			methodInfo = methodInfo.MakeGenericMethod(keyType, valueType);

			if (typedKeyMO == null) {
				var keyGuru = keyMO.typeBindingGuru(objectSpace);
				typedKeyMO = keyGuru.metaObjectToConvertTo(keyType);
			}
			if (typedValueMO == null) {
				var valueGuru = valueMO.typeBindingGuru(objectSpace);
				typedValueMO = valueGuru.metaObjectToConvertTo(valueType);
			}

			key = typedKeyMO.Expression;
			var typedValue = typedValueMO.asExpressionWithFormalType();

			var ifAbsentFunctor = ExpressionTreeGuru.expressionToConvertCompiledCodeOrDelegateToFunctor(ifAbsentValueBlockMO.asExpressionWithFormalType(), ESCompiledCode.blockFunctionTypeForNumArgs(0));
			var invokeIfAbsentFunctor = Expression.Invoke(ifAbsentFunctor, ExpressionTreeGuru.emptyExpressionArray);

			var addIfAbsentExpression = 
				Expression.Block(
					TypeGuru.objectType,
					new ParameterExpression[]{value},
					Expression.Assign(value, invokeIfAbsentFunctor),
					Expression.Call(self, setItemMethodInfo, new Expression[]{key, typedValue}),
					value);
			var lambda = Expression.Lambda<FuncNs.Func<Object>>(addIfAbsentExpression, false, new ParameterExpression[0]);

			return new DynamicMetaObject(
				Expression.Call(methodInfo, self, key, lambda).withCanonicalReturnType(), 
				receiver.bindingRestrictionsForForeignObjectReceiver(esClass, keyMO, ifAbsentValueBlockMO), 
				model);
			
		}

		#endregion

		#region Sending #doesNotUnderstand:

		public DynamicMetaObject metaObjectToSendDoesNotUnderstandToESObject(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject[] argumentsWithoutReceiver) {
			return metaObjectToSendDoesNotUnderstand(receiver, esClass, selector, argumentsWithoutReceiver, receiver.bindingRestrictionsForESObjectReceiver(esClass));
		}

		public DynamicMetaObject metaObjectToSendDoesNotUnderstandToForeignObject(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject[] argumentsWithoutReceiver) {
			return metaObjectToSendDoesNotUnderstand(receiver, esClass, selector, argumentsWithoutReceiver, receiver.bindingRestrictionsForForeignObjectReceiver(esClass));
		}

		public DynamicMetaObject metaObjectToSendDoesNotUnderstandToNil(DynamicMetaObject receiver, ESSymbol selector, DynamicMetaObject[] argumentsWithoutReceiver) {
			return metaObjectToSendDoesNotUnderstand(receiver, objectSpace.UndefinedObjectClass, selector, argumentsWithoutReceiver, receiver.asInstanceRestriction());
		}

		public DynamicMetaObject metaObjectToSendDoesNotUnderstand(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject[] argumentsWithoutReceiver, BindingRestrictions bindingRestrictions) {
			return new DynamicMetaObject(ExpressionTreeGuru.expressionToSendDoesNotUnderstand(receiver.Expression, esClass, selector, argumentsWithoutReceiver), bindingRestrictions, receiver.Value);
		}

		#endregion

		#region Sending messages--general

		public DynamicMetaObject metaObjectToSendMessageToESObject(DynamicMetaObject receiver, ESObjectSpace objectSpace, ESBehavior esClass, ESSymbol selector, DynamicMetaObject[] metaObjectArgs) {

			ESMethod method;
			if (esClass == null) {
				method = null;
			} else {
				method = esClass.compiledMethodAt(selector);
				if (method == null) { 
					if (esClass.IsHostSystemMetaclass) {
						switch (selector.NumArgs) {
							case 0:
								return canonicalGetMemberBinderFor(esClass, selector).FallbackGetMember(receiver);
							case 1:
								return canonicalSetMemberBinderFor(esClass, selector).FallbackSetMember(receiver, metaObjectArgs[0]);
							default:
								var name = selector.asHostSystemMemberName(CapitalizationScheme.InitialCapital);
								return canonicalInvokeMemberBinderFor(esClass, selector, name).FallbackInvokeMember(receiver, metaObjectArgs);
						}
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
					objectSpace,
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
				return metaObjectToSendMessageToESObject(receiver, objectSpace, esObject.Class, selector, metaObjectArgs);
			}
		}

		#region Sending messages to special receivers

		#region Sending messages to nil

		public DynamicMetaObject metaObjectToSendMessageToNil(DynamicMetaObject receiver, ESSymbol selector, DynamicMetaObject[] metaObjectArgs) {
			var method = objectSpace.UndefinedObjectClass.compiledMethodAt(selector);
			if (method == null) {
				return metaObjectToSendSyntheticMessageToNil(receiver, selector, metaObjectArgs);
			}
			return metaObjectToSendMessage(
					receiver, 
					objectSpace,
					objectSpace.UndefinedObjectClass, 
					selector, 
					method,
					metaObjectArgs, 
					ExpressionTreeGuru.expressionToTestThatNilHasSameClassVersion(receiver.Expression, objectSpace.UndefinedObjectClass).asBindingRestriction());
		}

		public DynamicMetaObject metaObjectToSendMessageToNilSuper(DynamicMetaObject receiver, ESSymbol selector, DynamicMetaObject[] metaObjectArgs) {
			var superclass = objectSpace.UndefinedObjectClass.Superclass;
			ESMethod method;
			if (superclass == null) {
				method = null;
			} else { 
				method = superclass.compiledMethodAt(selector);
				var homeClass = method.HomeClass;
				if (homeClass != superclass) { 
					superclass = homeClass.Superclass;
					method = superclass == null ? null : superclass.compiledMethodAt(selector);
				}
			}
			if (method == null) {
				return metaObjectToSendSyntheticMessageToNil(receiver, selector, metaObjectArgs);
			}
			return metaObjectToSendMessage(
					receiver, 
					objectSpace,
					objectSpace.UndefinedObjectClass, 
					selector, 
					method,
					metaObjectArgs, 
					ExpressionTreeGuru.expressionToTestThatNilHasSameClassVersion(receiver.Expression, objectSpace.UndefinedObjectClass).asBindingRestriction());
		}

		public DynamicMetaObject metaObjectToSendSyntheticMessageToNil(DynamicMetaObject receiver, ESSymbol selector, DynamicMetaObject[] args) {

			var self = receiver.Expression;
			var esClass = objectSpace.UndefinedObjectClass;

			// ExpressionTreeGuru.expressionToTestThatNilHasSameClassVersion(receiver.Expression, objectSpace.UndefinedObjectClass).asBindingRestriction()

			switch (selector.CanonicalSemantics) {

				// Invariant operations:
				case CanonicalSelectorSemantics.IsIdenticalTo:
					return metaObjectForInvariantOperation(ExpressionTreeGuru.expressionToSendReferenceEquals(receiver.Expression, args[0].Expression), receiver.Value);
				case CanonicalSelectorSemantics.IsNotIdenticalTo:
					return metaObjectForInvariantOperation(ExpressionTreeGuru.expressionToSendReferenceNotEquals(receiver.Expression, args[0].Expression), receiver.Value);
				case CanonicalSelectorSemantics.IdentityHash:		
					return metaObjectForInstanceRestrictedOperation(self, Expression.Constant(0L), receiver.Value);
				case CanonicalSelectorSemantics.IsNil:
					return metaObjectForInvariantOperation(ExpressionTreeGuru.expressionToSendReferenceEquals(receiver.Expression, ExpressionTreeGuru.nilConstant), receiver.Value);
				case CanonicalSelectorSemantics.IsNotNil:
					return metaObjectForInvariantOperation(ExpressionTreeGuru.expressionToSendReferenceNotEquals(receiver.Expression, ExpressionTreeGuru.nilConstant), receiver.Value);

				// Receiver-type-dependent operations:
				case CanonicalSelectorSemantics.Yourself:
					return metaObjectForInstanceRestrictedOperation(self, self, receiver.Value);
				case CanonicalSelectorSemantics.Class:
					return metaObjectForInstanceRestrictedOperation(self, Expression.Constant(esClass), receiver.Value);
				case CanonicalSelectorSemantics.IsMemberOf:
					return metaObjectForInstanceRestrictedOperation(self, ExpressionTreeGuru.expressionToSendReferenceEquals(Expression.Constant(esClass), args[0].Expression), receiver.Value);
				case CanonicalSelectorSemantics.IsKindOf:
					var classExpression = Expression.Constant(esClass);
					var messageSendMO = metaObjectToSendMessageToESObject(classExpression.asDynamicMetaObject(), objectSpace, esClass.Class, symbolFor("includesBehavior:"), args);
					return messageSendMO.Expression.asDynamicMetaObject(self.expressionToTestThatNilHasSameClassVersion(esClass).asBindingRestriction(), receiver.Value);

				case CanonicalSelectorSemantics.Size:	
					return metaObjectForInstanceRestrictedOperation(self, Expression.Constant(0L), receiver.Value);

				case CanonicalSelectorSemantics.IsTrue:
				case CanonicalSelectorSemantics.IsFalse:
					return metaObjectForInstanceRestrictedOperation(self, Expression.Constant(false), receiver.Value);

				case CanonicalSelectorSemantics.IsEqualTo:
					return metaObjectForInstanceRestrictedOperation(self, ExpressionTreeGuru.expressionToSendReferenceEquals(receiver.Expression, args[0].Expression), receiver.Value);
				case CanonicalSelectorSemantics.IsNotEqualTo:
					return metaObjectForInstanceRestrictedOperation(self, ExpressionTreeGuru.expressionToSendReferenceNotEquals(receiver.Expression, args[0].Expression), receiver.Value);

				// GetMemberBinder:
				case CanonicalSelectorSemantics.IsImmutable:
					return metaObjectForInstanceRestrictedOperation(self, Expression.Constant(true), receiver.Value);
				case CanonicalSelectorSemantics.AsImmutable:
					return metaObjectForInstanceRestrictedOperation(self, self, receiver.Value);

				// InvokeMemberBinder:
				case CanonicalSelectorSemantics.Hash:
					return metaObjectForInstanceRestrictedOperation(self, Expression.Constant(0L), receiver.Value);
				case CanonicalSelectorSemantics.ShallowCopy:
					return metaObjectForInstanceRestrictedOperation(self, self, receiver.Value);
				case CanonicalSelectorSemantics.Copy:
					return metaObjectForInstanceRestrictedOperation(self, self, receiver.Value);

				case CanonicalSelectorSemantics.AsAssociationTo:
					return metaObjectToCreateAssociation(receiver, esClass, selector, args[0]);


				case CanonicalSelectorSemantics.IsBoolean:
					return metaObjectForInstanceRestrictedOperation(self, Expression.Constant(false), receiver.Value);

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

				default:
				case CanonicalSelectorSemantics.None:
					
					break; // Yes, the C# compiler really IS that dumb....

			}

			return metaObjectToSendDoesNotUnderstandToNil(receiver, selector, args);

		}

		#endregion

		#region Sending messages to self

		public DynamicMetaObject metaObjectToSendMessageToSelf(DynamicMetaObject receiver, ESSymbol selector, DynamicMetaObject[] metaObjectArgs) {
			var model = receiver.Value;
			if (model == null) return metaObjectToSendMessageToNil(receiver, selector, metaObjectArgs);
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
 					objectSpace,
					esClass, 
					selector, 
					method,
					metaObjectArgs, 
					receiver.bindingRestrictionsForESObjectReceiver(esClass));
		}

		public DynamicMetaObject metaObjectToSendMessageToForeignSelf(DynamicMetaObject receiver, ESSymbol selector, DynamicMetaObject[] metaObjectArgs) {
			var esClass = objectSpace.classOfHostSystemValue(receiver.Value);
			var method = esClass.compiledMethodAt(selector);
			if (method == null) {
				return metaObjectToSendVirtualMessageToForeignObject(receiver, esClass, selector, metaObjectArgs);
			} else {
				return metaObjectToSendMessage(
						receiver,
 						objectSpace,
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
			ESMethod method;
			if (superclass == null) {
				method = null;
			} else { 
				method = superclass.compiledMethodAt(selector);
				var homeClass = method.HomeClass;
				if (homeClass != superclass) { 
					superclass = homeClass.Superclass;
					method = superclass == null ? null : superclass.compiledMethodAt(selector);
				}
			}
			return metaObjectToSendMessage(
					receiver, 
					objectSpace,
					esClass, 
					selector, 
					method,
					metaObjectArgs, 
					receiver.bindingRestrictionsForESObjectReceiver(esClass));
		}

		public DynamicMetaObject metaObjectToSendMessageToForeignSuper(DynamicMetaObject receiver, ESSymbol selector, DynamicMetaObject[] metaObjectArgs) {
			var esClass = objectSpace.classOfHostSystemValue(receiver.Value);
			var superclass = esClass.Superclass;
			ESMethod method;
			if (superclass == null) {
				method = null;
			} else { 
				method = superclass.compiledMethodAt(selector);
				var homeClass = method.HomeClass;
				if (homeClass != superclass) { 
					superclass = homeClass.Superclass;
					method = superclass == null ? null : superclass.compiledMethodAt(selector);
				}
			}
			if (method == null) {
				return metaObjectToSendVirtualMessageToForeignObject(receiver, esClass, selector, metaObjectArgs);
			} else {
				return metaObjectToSendMessage(
						receiver, 
						objectSpace,
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
					objectSpace,
					null,	// class
					selector, 
					null, // method
					metaObjectArgs, 
					receiver.bindingRestrictionsForESObjectReceiver(esClass));
		}

		#endregion

		#endregion

		#region Sending messages--foreign object receivers

		public DynamicMetaObject metaObjectToSendMessageToForeignObject(DynamicMetaObject receiver, Object model, ESSymbol selector, DynamicMetaObject[] metaObjectArgs) {
			var receiverType = model.GetType();
			var esClass = objectSpace.classOfHostSystemValue(model);
			if (receiverType.IsEnum) {
				var symbolMO 
					= new DynamicMetaObject(
						ExpressionTreeGuru.expressionToCreateESSymbolFromEnumerationContant(symbolRegistry, receiver.asExpressionWithFormalType()),
						receiver.Restrictions,
						receiver.Value);
				return metaObjectForForeignObjectOperation(
						receiver,
						esClass, 
						metaObjectToSendMessageToESObject(symbolMO, objectSpace, objectSpace.SymbolClass, selector, metaObjectArgs).Expression);
			}
			var method = esClass.compiledMethodAt(selector);
			if (method == null) {
				return metaObjectToSendVirtualMessageToForeignObject(receiver, esClass, selector, metaObjectArgs);
			} else {
				return metaObjectToSendMessage(
						receiver, 
						objectSpace,
						esClass, 
						selector, 
						method,
						metaObjectArgs, 
						receiver.bindingRestrictionsForForeignObjectReceiver(esClass));
			}
		}

		public DynamicMetaObject metaObjectToSendVirtualMessageToForeignObject(DynamicMetaObject receiver, ESBehavior esClass, ESSymbol selector, DynamicMetaObject[] args) {

			switch (selector.CanonicalSemantics) {

				// Invariant operations:
				case CanonicalSelectorSemantics.IsIdenticalTo:
					return metaObjectForInvariantOperation(ExpressionTreeGuru.expressionToSendReferenceEquals(receiver.Expression, args[0].Expression), receiver.Value);
				case CanonicalSelectorSemantics.IsNotIdenticalTo:
					return metaObjectForInvariantOperation(ExpressionTreeGuru.expressionToSendReferenceNotEquals(receiver.Expression, args[0].Expression), receiver.Value);
				case CanonicalSelectorSemantics.IdentityHash:		
					return metaObjectForForeignObjectOperation(receiver, esClass, ExpressionTreeGuru.expressionToInvoke_RuntimeHelpers_GetHashCode(receiver.Expression));
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
					var messageSendMO = metaObjectToSendMessageToESObject(classExpression.asDynamicMetaObject(), objectSpace, esClass.Class, symbolFor("includesBehavior:"), args);
					return messageSendMO.Expression.asDynamicMetaObject(receiver.restrictionThatForeignObjectHasSameClassVersion(esClass), receiver.Value);

				case CanonicalSelectorSemantics.AsBehavior:
				case CanonicalSelectorSemantics.AsClass:
					if (TypeGuru.typeType.IsAssignableFrom(receiver.LimitType)) { 
						return Expression.Constant(objectSpace.classForHostSystemType((Type)receiver.Value)).asDynamicMetaObject(receiver.asInstanceRestriction().Merge(receiver.restrictionThatForeignObjectHasSameClassVersion(esClass)), receiver.Value);
					}
					break;
				case CanonicalSelectorSemantics.AsMetaclass:
					if (TypeGuru.typeType.IsAssignableFrom(receiver.LimitType)) { 
						return Expression.Constant(objectSpace.classForHostSystemType((Type)receiver.Value).Class).asDynamicMetaObject(receiver.asInstanceRestriction().Merge(receiver.restrictionThatForeignObjectHasSameClassVersion(esClass)), receiver.Value);
					}
					break;

				case CanonicalSelectorSemantics.OnEventDo:
					return metaObjectToInvoke_OnEventDo(receiver, esClass, selector, args[0], args[1]);
				case CanonicalSelectorSemantics.OnEventDoNotDo:
					return metaObjectToInvoke_OnEventDoNotDo(receiver, esClass, selector, args[0], args[1]);

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
						// Can't use: return receiver.BindUnaryOperation(canonicalUnaryOperationBinderFor(esClass, selector, ExpressionType.ArrayLength));
						return metaObjectForForeignObjectOperation(receiver, esClass, Expression.ArrayLength(receiver.asExpressionWithFormalType()).withCanonicalReturnType());
					} else {
						return receiver.BindGetMember(canonicalGetMemberBinderFor(esClass, selector));
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

				// Ad-hoc conversions:
				case CanonicalSelectorSemantics.AsString:
					return metaObjectForForeignObjectOperation(receiver, esClass, ExpressionTreeGuru.expressionToCreateESStringFromNonESObject(objectSpace, receiver.Expression));
				case CanonicalSelectorSemantics.AsSymbol:
					return metaObjectForForeignObjectOperation(receiver, esClass, ExpressionTreeGuru.expressionToCreateESSymbolFromNonESObject(objectSpace, receiver.Expression));

				// InvokeBinder:
				case CanonicalSelectorSemantics.Invoke:
					return receiver.BindInvoke(canonicalInvokeBinderFor(esClass, selector), args);

				case CanonicalSelectorSemantics.InvokeWithArguments:
					break;

				// GetMemberBinder:
				case CanonicalSelectorSemantics.IsImmutable:
					return receiver.BindGetMember(canonicalGetMemberBinderFor(esClass, selector, "IsReadOnly"));
				case CanonicalSelectorSemantics.AsImmutable:
					return receiver.BindGetMember(canonicalGetMemberBinderFor(esClass, selector, "AsReadOnly"));

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
										mo.asExpressionWithType(TypeGuru.intType), 
										TypeGuru.intType).withCanonicalReturnType());

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
								shiftExtentIsPositive.asExpressionWithType(TypeGuru.boolType), 
									leftShift.asExpressionWithType(TypeGuru.objectType), 
									rightShift.asExpressionWithType(TypeGuru.objectType));
					return new DynamicMetaObject(
							shiftExpression.withCanonicalReturnType(),
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
					String name = selector.asHostSystemMemberName(CapitalizationScheme.InitialCapital);
					switch (selector.NumArgs) {
						case 0:
							return receiver.BindGetMember(canonicalGetMemberBinderFor(esClass, selector, name));
						case 1:
							if (selector.Type == SymbolType.BinaryMessageSelector) break; // If not handled above, a binary message selector can't be a valid host operator or method name.
							return receiver.BindSetMember(canonicalSetMemberBinderFor(esClass, selector, name), args[0]);
						default:
							return receiver.BindInvokeMember(canonicalInvokeMemberBinderFor(esClass, selector, name), args);
					}
					
					break; // Yes, the C# compiler really IS that dumb....

			}

			return metaObjectToSendDoesNotUnderstandToForeignObject(receiver, esClass, selector, args);
		}

		#region Essence Sharp VIRTUAL message send binders

		public class GetPropertyOrFieldOfForeignObjectBinder : GetMemberBinder {

			protected DynamicBindingGuru	dynamicBindingGuru	= null;
			protected ESObjectSpace		objectSpace			= null;
			protected ESBehavior		esClass			= null;
			protected ESSymbol		selector		= null;

			protected GetPropertyOrFieldOfForeignObjectBinder(DynamicBindingGuru dynamicBindingGuru, ESBehavior esClass, ESSymbol selector, String name) : base(name, false) { 
				this.dynamicBindingGuru	= dynamicBindingGuru;
				objectSpace = dynamicBindingGuru.objectSpace;
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

					return new DynamicMetaObject(
							getPropertyOrFieldExpression.withCanonicalReturnType(), 
							target.bindingRestrictionsForForeignObjectReceiver(esClass),
							target.Value);	
		
				} else if (Selector.CanonicalSemantics == CanonicalSelectorSemantics.Size) {
					if (esClass.getReadablePropertyOrElseField(
								"Count", 
								(property) => getPropertyOrFieldExpression = Expression.Property(esClass.IsHostSystemMetaclass ? null : self, property), 
								(field) => getPropertyOrFieldExpression = Expression.Field(esClass.IsHostSystemMetaclass ? null : self, field))) {

						return new DynamicMetaObject(
								getPropertyOrFieldExpression.withCanonicalReturnType(), 
								target.bindingRestrictionsForForeignObjectReceiver(esClass),
								target.Value);	
		
					} else if (esClass.getReadablePropertyOrElseField(
								"Length", 
								(property) => getPropertyOrFieldExpression = Expression.Property(esClass.IsHostSystemMetaclass ? null : self, property), 
								(field) => getPropertyOrFieldExpression = Expression.Field(esClass.IsHostSystemMetaclass ? null : self, field))) {

						return new DynamicMetaObject(
								getPropertyOrFieldExpression.withCanonicalReturnType(), 
								target.bindingRestrictionsForForeignObjectReceiver(esClass),
								target.Value);	
		
					}
				}

				return esClass.IsHostSystemMetaclass ?
					dynamicBindingGuru.canonicalInvokeMemberBinderFor(esClass, selector, Name).FallbackInvokeMember(target, emptyArgArray) :
					target.BindInvokeMember(
						dynamicBindingGuru.canonicalInvokeMemberBinderFor(
							esClass, 
							Selector, 
							Name), 
						emptyArgArray);

			}

			public class Registry : BinderRegistry {

				protected readonly Dictionary<long, Dictionary<ESSymbol, GetPropertyOrFieldOfForeignObjectBinder>> registry = new Dictionary<long, Dictionary<ESSymbol, GetPropertyOrFieldOfForeignObjectBinder>>();

				public Registry(DynamicBindingGuru dynamicBindingGuru) : base(dynamicBindingGuru) {
				}

				public GetPropertyOrFieldOfForeignObjectBinder canonicalBinderFor(ESBehavior esClass, ESSymbol selector) {
					return canonicalBinderFor(esClass, selector, selector.asHostSystemMemberName(CapitalizationScheme.InitialCapital));
				}

				public GetPropertyOrFieldOfForeignObjectBinder canonicalBinderFor(ESBehavior esClass, ESSymbol selector, String memberName) {
					Dictionary<ESSymbol, GetPropertyOrFieldOfForeignObjectBinder> binderRegistry;
					GetPropertyOrFieldOfForeignObjectBinder binder;
					long classId = esClass.Identity;
					if (!registry.TryGetValue(classId, out binderRegistry)) {
						binderRegistry = new Dictionary<ESSymbol, GetPropertyOrFieldOfForeignObjectBinder>();
						registry[classId] = binderRegistry;
						binder = new GetPropertyOrFieldOfForeignObjectBinder(DynamicBindingGuru, esClass, selector, memberName);
						binderRegistry[selector] = binder;
						return binder;
					}
					if (!binderRegistry.TryGetValue(selector, out binder)) {
						binder = new GetPropertyOrFieldOfForeignObjectBinder(DynamicBindingGuru, esClass, selector, memberName);
						binderRegistry[selector] = binder;
					}
					return binder;
				}


			}

		}

		public class SetPropertyOrFieldOfForeignObjectBinder : SetMemberBinder {

			protected DynamicBindingGuru	dynamicBindingGuru	= null;
			protected ESObjectSpace		objectSpace			= null;
			protected ESBehavior		esClass			= null;
			protected ESSymbol		selector		= null;

			protected SetPropertyOrFieldOfForeignObjectBinder(DynamicBindingGuru dynamicBindingGuru, ESBehavior esClass, ESSymbol selector, String name) : base(name, false) { 
				this.dynamicBindingGuru	= dynamicBindingGuru;
				objectSpace			= dynamicBindingGuru.objectSpace;
				this.esClass		= esClass;
				this.selector		= selector;
			}

			public ESSymbol	Selector {
				get {return selector;}
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
					var argGuru = value.typeBindingGuru(objectSpace);
					value = argGuru.metaObjectToConvertTo(parameterType);
					Expression assignExpression = Expression.Assign(getPropertyOrFieldExpression, value.Expression);
					return new DynamicMetaObject(
						Expression.Block(assignExpression, self),
						target.bindingRestrictionsForForeignObjectReceiver(esClass, value),
						target.Value);	
				}

				return esClass.IsHostSystemMetaclass ?
					dynamicBindingGuru.canonicalInvokeMemberBinderFor(esClass, selector, Name).FallbackInvokeMember(target, argArrayFor(value)) :
					target.BindInvokeMember(
						dynamicBindingGuru.canonicalInvokeMemberBinderFor(
							esClass, 
							Selector, 
							Name), 
						argArrayFor(value));
				

			}

			public class Registry : BinderRegistry {

				protected readonly Dictionary<long, Dictionary<ESSymbol, SetPropertyOrFieldOfForeignObjectBinder>> registry = new Dictionary<long, Dictionary<ESSymbol, SetPropertyOrFieldOfForeignObjectBinder>>();

				public Registry(DynamicBindingGuru dynamicBindingGuru) : base(dynamicBindingGuru) {
				}

				public SetPropertyOrFieldOfForeignObjectBinder canonicalBinderFor(ESBehavior esClass, ESSymbol selector) {
					return canonicalBinderFor(esClass, selector, selector.asHostSystemMemberName(CapitalizationScheme.InitialCapital));
				}

				public SetPropertyOrFieldOfForeignObjectBinder canonicalBinderFor(ESBehavior esClass, ESSymbol selector, String memberName) {
					Dictionary<ESSymbol, SetPropertyOrFieldOfForeignObjectBinder> binderRegistry;
					SetPropertyOrFieldOfForeignObjectBinder binder;
					long classId = esClass.Identity;
					if (!registry.TryGetValue(classId, out binderRegistry)) {
						binderRegistry = new Dictionary<ESSymbol, SetPropertyOrFieldOfForeignObjectBinder>();
						registry[classId] = binderRegistry;
						binder = new SetPropertyOrFieldOfForeignObjectBinder(DynamicBindingGuru, esClass, selector, memberName);
						binderRegistry[selector] = binder;
						return binder;
					}
					if (!binderRegistry.TryGetValue(selector, out binder)) {
						binder = new SetPropertyOrFieldOfForeignObjectBinder(DynamicBindingGuru, esClass, selector, memberName);
						binderRegistry[selector] = binder;
					}
					return binder;
				}

			}

		}

		public class GetValueAtIndexOrKeyInForeignObjectBinder : GetIndexBinder {

			protected DynamicBindingGuru	dynamicBindingGuru	= null;
			protected ESObjectSpace		objectSpace			= null;
			protected ESBehavior		esClass			= null;
			protected ESSymbol		selector		= null;

			protected GetValueAtIndexOrKeyInForeignObjectBinder(DynamicBindingGuru dynamicBindingGuru, ESBehavior esClass, ESSymbol selector) : base(ExpressionTreeGuru.callInfoForArgCount(selector.NumArgs)) { 
				this.dynamicBindingGuru	= dynamicBindingGuru;
				objectSpace = dynamicBindingGuru.objectSpace;
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
				var indexGuru = indexMo.typeBindingGuru(objectSpace);
				var typedIndexMo = indexMo;

				if (receiverType.IsArray) {
					indexMo = indexGuru.metaObjectToConvertTo(TypeGuru.intType);
					var index = Expression.Subtract(indexMo.Expression, oneInt32.Expression.withType(indexMo.LimitType));
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
					} else {
						var parameters = methodInfo.GetParameters();
						var indexParameter = parameters[0];
						indexParameterType = indexParameter.ParameterType;
						typedIndexMo = indexGuru.metaObjectToConvertTo(indexParameterType);
					}
					Expression index;
					if (indexParameterType.isNumeric()) {
						index = Expression.Subtract(typedIndexMo.Expression, oneInt32.Expression.withType(typedIndexMo.LimitType));
					} else {
						index = typedIndexMo.Expression;
					}
					performOperationExpression = Expression.Call(methodInfo.IsStatic ? null : target.asExpressionWithFormalType(), methodInfo, index);
				}

				return new DynamicMetaObject(
						performOperationExpression.withCanonicalReturnType(), 
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
			protected ESObjectSpace		objectSpace			= null;
			protected ESBehavior		esClass			= null;
			protected ESSymbol		selector		= null;

			protected SetValueAtIndexOrKeyInForeignObjectBinder(DynamicBindingGuru dynamicBindingGuru, ESBehavior esClass, ESSymbol selector) : base(ExpressionTreeGuru.callInfoForArgCount(selector.NumArgs)) { 
				this.dynamicBindingGuru	= dynamicBindingGuru;
				objectSpace = dynamicBindingGuru.objectSpace;
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
				var indexGuru = indexMo.typeBindingGuru(objectSpace);
				var valueGuru = value.typeBindingGuru(objectSpace);
				var typedIndexMo = indexMo;
				var typedValue = value;

				if (receiverType.IsArray) {
					typedIndexMo = indexGuru.metaObjectToConvertTo(TypeGuru.intType);
					var index = Expression.Subtract(typedIndexMo.Expression, oneInt32.Expression.withType(typedIndexMo.LimitType));
					typedValue = valueGuru.metaObjectToConvertTo(receiverType.GetElementType());
					performOperationExpression = Expression.Assign(Expression.ArrayAccess(target.asExpressionWithFormalType(), index), typedValue.Expression).withCanonicalReturnType();
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
					} else {
						var parameters = methodInfo.GetParameters();
						var indexParameter = parameters[0];
						indexParameterType = indexParameter.ParameterType;
						typedIndexMo = indexGuru.metaObjectToConvertTo(indexParameterType);
					}
					Expression index;
					if (indexParameterType.isNumeric()) {
						index = Expression.Subtract(typedIndexMo.Expression, oneInt32.Expression.withType(typedIndexMo.LimitType));
					} else {
						index = typedIndexMo.Expression;
					}
					if (methodInfo.ReturnType.isVoidType()) {
						var parameters = methodInfo.GetParameters();
						var valueParameter = parameters[1];
						typedValue = valueGuru.metaObjectToConvertTo(valueParameter.ParameterType);
						performOperationExpression = Expression.Block(
							Expression.Call(methodInfo.IsStatic ? null : target.asExpressionWithFormalType(), methodInfo, new Expression[]{index, typedValue.Expression}),
							target.Expression);
					} else {
						typedValue = valueGuru.metaObjectToConvertTo(methodInfo.ReturnType);
						performOperationExpression = Expression.Call(methodInfo.IsStatic ? null : target.asExpressionWithFormalType(), methodInfo, new Expression[]{index, typedValue.Expression}).withCanonicalReturnType();
					}
				}

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
			protected ESObjectSpace		objectSpace			= null;
			protected ESBehavior		esClass			= null;
			protected ESSymbol		selector		= null;

			protected RemoveAtIndexOrKeyFromForeignCollectionBinder(DynamicBindingGuru dynamicBindingGuru, ESBehavior esClass, ESSymbol selector) : base(ExpressionTreeGuru.callInfoForArgCount(selector.NumArgs)) { 
				this.dynamicBindingGuru	= dynamicBindingGuru;
				objectSpace = dynamicBindingGuru.objectSpace;
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
			protected ESObjectSpace		objectSpace			= null;
			protected ESBehavior		esClass			= null;
			protected ESSymbol		selector		= null;

			protected UnaryMessageSendToForeignObjectBinder(DynamicBindingGuru dynamicBindingGuru, ESBehavior esClass, ESSymbol selector, ExpressionType operation) : base(operation) {
				this.dynamicBindingGuru	= dynamicBindingGuru;
				objectSpace = dynamicBindingGuru.objectSpace;
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
						unaryOperatorExpression.withCanonicalReturnType(), 
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
			protected ESObjectSpace		objectSpace			= null;
			protected ESBehavior		esClass			= null;
			protected ESSymbol		selector		= null;

			protected BinaryMessageSendToForeignObjectBinder(DynamicBindingGuru dynamicBindingGuru, ESBehavior esClass, ESSymbol selector, ExpressionType operation) : base(operation) {
				this.dynamicBindingGuru	= dynamicBindingGuru;
				objectSpace = dynamicBindingGuru.objectSpace;
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
							binaryOperatorExpression = Expression.And(leftOperandMO.asExpressionWithFormalType().withType(TypeGuru.boolType), rightOperandMO.asExpressionWithFormalType().withType(TypeGuru.boolType));
						}
						break;
					case CanonicalSelectorSemantics.LogicalOr:
						if (convertNumericTypesToHighestGenerality(leftOperandMO, rightOperandMO, out leftOperand, out rightOperand)) {
							binaryOperatorExpression = Expression.Or(leftOperand, rightOperand);
						} else {
							binaryOperatorExpression = Expression.Or(leftOperandMO.asExpressionWithFormalType().withType(TypeGuru.boolType), rightOperandMO.asExpressionWithFormalType().withType(TypeGuru.boolType));
						}
						break;
					case CanonicalSelectorSemantics.LogicalXor:
						if (convertNumericTypesToHighestGenerality(leftOperandMO, rightOperandMO, out leftOperand, out rightOperand)) {
							binaryOperatorExpression = Expression.ExclusiveOr(leftOperand, rightOperand);
						} else {
							binaryOperatorExpression = Expression.ExclusiveOr(leftOperandMO.asExpressionWithFormalType().withType(TypeGuru.boolType), rightOperandMO.asExpressionWithFormalType().withType(TypeGuru.boolType));
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
							binaryOperatorExpression = Expression.LeftShift(leftOperand, rightOperand.withType(TypeGuru.intType));
						} else {
							binaryOperatorExpression = Expression.LeftShift(leftOperandMO.asExpressionWithFormalType(), rightOperandMO.asExpressionWithFormalType().withType(TypeGuru.intType));
						}
						break;
					case CanonicalSelectorSemantics.ShiftRight:
						if (convertNumericTypesToHighestGenerality(leftOperandMO, rightOperandMO, out leftOperand, out rightOperand)) {
							binaryOperatorExpression = Expression.RightShift(leftOperand, rightOperand.withType(TypeGuru.intType));
						} else {
							binaryOperatorExpression = Expression.RightShift(leftOperandMO.asExpressionWithFormalType(), rightOperandMO.asExpressionWithFormalType().withType(TypeGuru.intType));
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
					binaryOperatorExpression = binaryOperatorExpression.withType(TypeGuru.objectType);
					if (bindingRestrictions == null) {
						bindingRestrictions = leftOperandMO.addingFormalTypeRestriction().Merge(rightOperandMO.addingFormalTypeRestriction());
					}
				}

				return new DynamicMetaObject(
						binaryOperatorExpression.withCanonicalReturnType(), 
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
			protected ESObjectSpace		objectSpace			= null;
			protected ESBehavior		esClass			= null;

			protected ConvertTypeOfForeignObjectBinder(DynamicBindingGuru dynamicBindingGuru, ESBehavior esClass, Type type) : base(type, true) {
				this.dynamicBindingGuru	= dynamicBindingGuru;
				objectSpace = dynamicBindingGuru.objectSpace;
				this.esClass		= esClass;
			}

			public override DynamicMetaObject FallbackConvert(DynamicMetaObject target, DynamicMetaObject errorSuggestion) {
				if (!target.HasValue) return Defer(target);

				var targetGuru = target.typeBindingGuru(objectSpace);
				var conversion = targetGuru.metaObjectToConvertTo(Type);

				return new DynamicMetaObject(
							conversion.asExpressionWithFormalType().withCanonicalReturnType(), 
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
			protected ESObjectSpace		objectSpace			= null;
			protected ESBehavior		esClass			= null;
			protected ESSymbol		selector		= null;
			protected long			numArgs			= 0;

			protected InvokeForeignFunctionBinder(DynamicBindingGuru dynamicBindingGuru, ESBehavior esClass, ESSymbol selector) : base(ExpressionTreeGuru.callInfoForArgCount(selector.NumArgs)) { 
				this.dynamicBindingGuru	= dynamicBindingGuru;
				objectSpace = dynamicBindingGuru.objectSpace;
				this.esClass		= esClass;
				this.selector		= selector;
				numArgs			= selector.NumArgs;
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
					if (method.ReturnType == TypeGuru.voidType) {
						invokeExpression = Expression.Block(TypeGuru.objectType, invokeExpression, target.Expression);
					} else {
						invokeExpression = invokeExpression.withCanonicalReturnType();
					}
					return new DynamicMetaObject(
							invokeExpression, 
							target.addingFormalTypeRestriction().Merge(BindingRestrictions.Combine(arguments)),
							target.Value);
				} else if (numArgs == 0) {
					return target.BindGetMember(dynamicBindingGuru.canonicalGetMemberBinderFor(esClass, Selector));
				} else if (numArgs == 1) {
					return target.BindSetMember(dynamicBindingGuru.canonicalSetMemberBinderFor(esClass, Selector), args[0]);
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
			protected ESObjectSpace		objectSpace			= null;
			protected ESBehavior		esClass			= null;
			protected ESSymbol		selector		= null;
			protected String		alternateMethodName	= null;

			protected MessageSendToForeignObjectBinder(DynamicBindingGuru dynamicBindingGuru, ESBehavior esClass, ESSymbol selector, String memberName) : base(memberName, false, ExpressionTreeGuru.callInfoForArgCount(selector.NumArgs)) {
				this.dynamicBindingGuru	= dynamicBindingGuru;
				objectSpace = dynamicBindingGuru.objectSpace;
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
					switch (Selector.CanonicalSemantics) {
						case CanonicalSelectorSemantics.Invoke:
							if (Selector.NumArgs == 0) return dynamicBindingGuru.metaObjectForForeignObjectOperation(target, esClass, target.Expression);
							break;
						case CanonicalSelectorSemantics.Size:
							return dynamicBindingGuru.metaObjectForForeignObjectOperation(target, esClass, Expression.Constant(0L));
						default:
							break;
					}
					invokeMemberExpression = ExpressionTreeGuru.expressionToSendDoesNotUnderstand(target.Expression, esClass, selector, args);
				} else {
					if (methodInfo.IsStatic) {
						invokeMemberExpression = Expression.Call(null, methodInfo, expressionArrayFor(typedArguments.ToArray()));
					} else { 
						var self = target.asExpressionWithFormalType();
						if (!methodInfo.DeclaringType.IsAssignableFrom(self.Type)) self = self.withType(methodInfo.DeclaringType);
						invokeMemberExpression = Expression.Call(self, methodInfo, expressionArrayFor(typedArguments.ToArray()));
					}
					if (methodInfo.ReturnType == TypeGuru.voidType) { 
						invokeMemberExpression = Expression.Block(TypeGuru.objectType, invokeMemberExpression, target.Expression);
					} else {
						invokeMemberExpression = invokeMemberExpression.withCanonicalReturnType();
					}
				}

				return new DynamicMetaObject(
						invokeMemberExpression, 
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
					if (method.ReturnType == TypeGuru.voidType) {
						invokeExpression = Expression.Block(TypeGuru.objectType, invokeExpression, target.Expression);
					} else {
						invokeExpression = invokeExpression.withCanonicalReturnType();
					}
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

				public MessageSendToForeignObjectBinder canonicalBinderFor(ESBehavior esClass, ESSymbol selector, String methodName) {
					Dictionary<ESSymbol, MessageSendToForeignObjectBinder> binderRegistry;
					MessageSendToForeignObjectBinder binder;
					long classId = esClass.Identity;
					if (!registry.TryGetValue(classId, out binderRegistry)) {
						binderRegistry = new Dictionary<ESSymbol, MessageSendToForeignObjectBinder>();
						registry[classId] = binderRegistry;
						binder = new MessageSendToForeignObjectBinder(DynamicBindingGuru, esClass, selector, methodName);
						binderRegistry[selector] = binder;
						return binder;
					}
					if (!binderRegistry.TryGetValue(selector, out binder)) {
						binder = new MessageSendToForeignObjectBinder(DynamicBindingGuru, esClass, selector, methodName);
						binderRegistry[selector] = binder;
					}
					return binder;
				}
			}

		}

		public class CreateInstanceOfForeignObjectBinder : CreateInstanceBinder {
	
			protected DynamicBindingGuru	dynamicBindingGuru	= null;
			protected ESObjectSpace		objectSpace			= null;
			protected ESBehavior		esClass			= null;
			protected ESSymbol		selector		= null;

			protected CreateInstanceOfForeignObjectBinder(DynamicBindingGuru dynamicBindingGuru, ESBehavior esClass, ESSymbol selector) : base(ExpressionTreeGuru.callInfoForArgCount(selector.NumArgs)) { 
				this.dynamicBindingGuru	= dynamicBindingGuru;
				objectSpace = dynamicBindingGuru.objectSpace;
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
							wrapperClass = objectSpace.classForHostSystemType(hostSystemType);
							break;
						case 1:
							wrapperClass = objectSpace.classForHostSystemType(hostSystemType);
							List<DynamicMetaObject> typedArguments;
							if (dynamicBindingGuru.getConstructorAndTypeCompatibleArgumentsFor(wrapperClass, args, out constructorInfo, out typedArguments)) {
								return new DynamicMetaObject(
									Expression.New(constructorInfo, expressionArrayFor(typedArguments.ToArray())), 
									target.bindingRestrictionsForForeignObjectReceiver(esClass, typedArguments),
									target.Value);
							}
							break;
						default:
							wrapperClass = objectSpace.classForHostSystemType(hostSystemType);
							break;
					}
					var wrapperMetaObject = new DynamicMetaObject(Expression.Constant(wrapperClass), target.Restrictions, wrapperClass);
					return dynamicBindingGuru.metaObjectToSendMessageToESObject(wrapperMetaObject, objectSpace, wrapperClass.Class, Selector, args);
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

		#endregion

	}

	public abstract class BinderRegistry {

		protected DynamicBindingGuru			dynamicBindingGuru	= null;
		protected ESObjectSpace				objectSpace		= null;

		protected BinderRegistry(DynamicBindingGuru dynamicBindingGuru) {
			this.dynamicBindingGuru = dynamicBindingGuru;
			objectSpace = dynamicBindingGuru.ObjectSpace;
		}

		public DynamicBindingGuru DynamicBindingGuru {
			get {return dynamicBindingGuru;}
		}

		public ESObjectSpace ObjectSpace {
			get {return objectSpace;}
		}

	}

}
