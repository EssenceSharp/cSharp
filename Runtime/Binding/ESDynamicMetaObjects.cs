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
#if CLR2
using Microsoft.Scripting.Ast;
using Expression = Microsoft.Scripting.Ast.Expression;
#else
using System.Linq.Expressions;
using Expression = System.Linq.Expressions.Expression;
#endif
using EssenceSharp.ClientServices;
using EssenceSharp.UtilityServices;
#endregion

namespace EssenceSharp.Runtime.Binding {

	// Foreign-Hosted Binding (for cases where an Essence Sharp object is responding to requests sent by non-Essence Sharp code)

	public class ESDynamicMetaObject : DynamicMetaObject {

		protected ESBehavior valueClass = null;

		public ESDynamicMetaObject(Expression expression, BindingRestrictions restrictions, Object value, ESBehavior valueClass) : base(expression, restrictions, value) {
			this.valueClass = valueClass;
		}

		public ESBehavior ValueClass {
			get {	if (valueClass != null) return valueClass;
				if (HasValue) {
					valueClass = ((ESObject)Value).Class;
				}
				return valueClass;}
		}

		public BindingRestrictions DefaultBindingRestrictions {
			get {return this.bindingRestrictionsForESObjectReceiver(ValueClass);}
		}

		public bool createMetaObjectToSendMessage(String messageName, DynamicMetaObject[] args, out DynamicMetaObject messageSendMO) {
			var esClass = ValueClass;
			var kernel = esClass.Kernel;
			long numArgs = args.Length;
			var method = esClass.compiledMethodAtSystemSelector(messageName, numArgs);
			if (method == null) {
				ESSymbol selector;
				SymbolType symbolType;
				long syntacicalNumArgs;
				long ignored;
				messageName = messageName.usingCapitalizationScheme(CapitalizationScheme.InitialLowerCase);
				ESLexicalUtility.classifySymbol(messageName, null, out symbolType, out syntacicalNumArgs, out ignored);
				switch (symbolType) {
					case SymbolType.Identifier:
						switch (numArgs) {
							case 0:
								selector = kernel.symbolFor(messageName);
								break;
							case 1:
								selector = kernel.symbolFor(messageName + ":");
								break;
							default:
								selector = null;
								break;
						}
						break;
					case SymbolType.BinaryMessageSelector:
						selector = numArgs == 1 ? kernel.symbolFor(messageName) : null;
						break;
					case SymbolType.Keyword:
						selector = syntacicalNumArgs == numArgs ? kernel.symbolFor(messageName) : null;
						break;
					default:
					case SymbolType.String:
						selector = null;
						break;
				}
				if (selector == null) {
					messageSendMO = metaObjectToSendDoesNotUnderstand(kernel.symbolFor(messageName), args);
					return false;
				} else {
					method = esClass.compiledMethodAt(selector);
					if (method != null) { 
						messageSendMO =  DynamicBindingGuru.metaObjectToSendMessage(this, kernel, esClass, selector, method, args, this.bindingRestrictionsForESObjectReceiver(esClass));
						return true;
					} else {
						messageSendMO =  null;
						return false;
					}
				}
			} else {
				messageSendMO = DynamicBindingGuru.metaObjectToSendMessage(this, kernel, esClass, method.Selector, method, args, DefaultBindingRestrictions);
				return true;
			}
		}

		public DynamicMetaObject metaObjectToSendDoesNotUnderstand(ESSymbol selector, DynamicMetaObject[] args) {
			return ExpressionTreeGuru.expressionToSendDoesNotUnderstand(Expression, ValueClass, selector, args).asDynamicMetaObject(DefaultBindingRestrictions, Value);
			return new DynamicMetaObject(
					ExpressionTreeGuru.expressionToSendDoesNotUnderstand(Expression, ValueClass, selector, args), 
					DefaultBindingRestrictions,
					Value);
		}

		public DynamicMetaObject metaObjectToThrowInvalidFunctionCallException(ESSymbol selector, DynamicMetaObject[] args, String messageText, Type expectedFunctionType, Type actualFunctionType) {
			return ExpressionTreeGuru.expressionToThrowInvalidFunctionCallException(Expression, ValueClass, selector, messageText, (long)args.Length, expectedFunctionType, actualFunctionType).asDynamicMetaObject(DefaultBindingRestrictions, Value);
		}

		public override DynamicMetaObject BindGetMember(GetMemberBinder binder) {
			DynamicMetaObject messageSendMO;
			if (createMetaObjectToSendMessage(binder.Name, DynamicBindingGuru.emptyArgArray, out messageSendMO)) return messageSendMO;
			return binder.FallbackGetMember(this, messageSendMO);
		}

		public override DynamicMetaObject BindSetMember(SetMemberBinder binder, DynamicMetaObject value) {
			DynamicMetaObject messageSendMO;
			if (createMetaObjectToSendMessage(binder.Name, DynamicBindingGuru.argArrayFor(value), out messageSendMO)) return messageSendMO;
			return binder.FallbackSetMember(this, messageSendMO, value);
		}

		public override DynamicMetaObject BindDeleteMember(DeleteMemberBinder binder) {
			DynamicMetaObject messageSendMO;
			var kernel = ValueClass.Kernel;
			var memberName = kernel.symbolFor(binder.Name);
			if (createMetaObjectToSendMessage("removeKey:", DynamicBindingGuru.argArrayFor(Expression.Constant(memberName).asDynamicMetaObject(BindingRestrictions.Empty, memberName)), out messageSendMO)) return messageSendMO;
			return binder.FallbackDeleteMember(this, messageSendMO);
		}

		public override DynamicMetaObject BindGetIndex(GetIndexBinder binder, DynamicMetaObject[] indexes) {
			DynamicMetaObject messageSendMO;
			if (createMetaObjectToSendMessage("at:", DynamicBindingGuru.argArrayFor(indexes[0]), out messageSendMO)) return messageSendMO;
			return binder.FallbackGetIndex(this, indexes, messageSendMO);
		}

		public override DynamicMetaObject BindSetIndex(SetIndexBinder binder, DynamicMetaObject[] indexes, DynamicMetaObject value) {
			DynamicMetaObject messageSendMO;
			if (createMetaObjectToSendMessage("at:put:", DynamicBindingGuru.argArrayFor(indexes[0], value), out messageSendMO)) return messageSendMO;
			return binder.FallbackSetIndex(this, indexes, value, messageSendMO);
		}

		public override DynamicMetaObject BindDeleteIndex(DeleteIndexBinder binder, DynamicMetaObject[] indexes) {
			DynamicMetaObject messageSendMO;
			if (createMetaObjectToSendMessage("removeAt:", DynamicBindingGuru.argArrayFor(indexes[0]), out messageSendMO)) return messageSendMO;
			return binder.FallbackDeleteIndex(this, indexes, messageSendMO);
		}

		public override DynamicMetaObject BindUnaryOperation(UnaryOperationBinder binder) {
			
			DynamicMetaObject messageSendMO;
			Expression value;
			DynamicMetaObject arg;

			switch (binder.Operation) {
				case ExpressionType.Negate:
					if (createMetaObjectToSendMessage("negated", DynamicBindingGuru.emptyArgArray, out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.UnaryPlus:
					if (createMetaObjectToSendMessage("abs", DynamicBindingGuru.emptyArgArray, out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.Not:
					if (createMetaObjectToSendMessage("not", DynamicBindingGuru.emptyArgArray, out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.Decrement:
					if (createMetaObjectToSendMessage("decrement", DynamicBindingGuru.emptyArgArray, out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.Increment:
					if (createMetaObjectToSendMessage("increment", DynamicBindingGuru.emptyArgArray, out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.OnesComplement:
					if (createMetaObjectToSendMessage("bitInvert", DynamicBindingGuru.emptyArgArray, out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.IsTrue:
					value = Expression.Constant(true);
					arg = value.asDynamicMetaObject(BindingRestrictions.Empty, true);
					if (createMetaObjectToSendMessage("=", DynamicBindingGuru.argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.IsFalse:
 					value = Expression.Constant(false);
					arg = value.asDynamicMetaObject(BindingRestrictions.Empty, false);
					if (createMetaObjectToSendMessage("=", DynamicBindingGuru.argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				default:
				case ExpressionType.Extension:
					messageSendMO = null;
					break;
			}

			if (messageSendMO == null) {
				var esClass = ValueClass;
				var kernel = esClass.Kernel;
				var selector = kernel.symbolFor("??");
				messageSendMO = metaObjectToSendDoesNotUnderstand(selector, DynamicBindingGuru.emptyArgArray);
			}
			return binder.FallbackUnaryOperation(this, messageSendMO);

		}

		public override DynamicMetaObject BindBinaryOperation(BinaryOperationBinder binder, DynamicMetaObject arg) {

			DynamicMetaObject messageSendMO		= null;
			ESBehavior esClass			= null;
			ESKernel kernel;
			ESSymbol selector			= null;

			switch (binder.Operation) {
				case ExpressionType.Add:
					if (createMetaObjectToSendMessage("+", DynamicBindingGuru.argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.And:
					if (createMetaObjectToSendMessage("&", DynamicBindingGuru.argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.Divide:
					if (createMetaObjectToSendMessage("/", DynamicBindingGuru.argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.Equal:
					if (createMetaObjectToSendMessage("=", DynamicBindingGuru.argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.ExclusiveOr:
					if (createMetaObjectToSendMessage("xor:", DynamicBindingGuru.argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.GreaterThan:
					if (createMetaObjectToSendMessage(">", DynamicBindingGuru.argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.GreaterThanOrEqual:
					if (createMetaObjectToSendMessage(">=", DynamicBindingGuru.argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.LeftShift:
					if (createMetaObjectToSendMessage("<<", DynamicBindingGuru.argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.LessThan:
					if (createMetaObjectToSendMessage("<", DynamicBindingGuru.argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.LessThanOrEqual:
					if (createMetaObjectToSendMessage("<=", DynamicBindingGuru.argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.Modulo:
					if (createMetaObjectToSendMessage("rem:", DynamicBindingGuru.argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.Multiply:
					if (createMetaObjectToSendMessage("*", DynamicBindingGuru.argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.NotEqual:
					if (createMetaObjectToSendMessage("~=", DynamicBindingGuru.argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.Or:
					if (createMetaObjectToSendMessage("|", DynamicBindingGuru.argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.Power:
					if (createMetaObjectToSendMessage("**", DynamicBindingGuru.argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.RightShift:
					if (createMetaObjectToSendMessage(">>", DynamicBindingGuru.argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.Subtract:
					if (createMetaObjectToSendMessage("-", DynamicBindingGuru.argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				default:
				case ExpressionType.AddAssign:
					esClass = ValueClass;
					kernel = esClass.Kernel;
					selector = kernel.symbolFor("+=");
					break;
				case ExpressionType.AndAssign:
					esClass = ValueClass;
					kernel = esClass.Kernel;
					selector = kernel.symbolFor("&=");
					break;
				case ExpressionType.DivideAssign:
					esClass = ValueClass;
					kernel = esClass.Kernel;
					selector = kernel.symbolFor("/=");
					break;
				case ExpressionType.ExclusiveOrAssign:
					esClass = ValueClass;
					kernel = esClass.Kernel;
					selector = kernel.symbolFor("^=");
					break;
				case ExpressionType.LeftShiftAssign:
					esClass = ValueClass;
					kernel = esClass.Kernel;
					selector = kernel.symbolFor("<<=");
					break;
				case ExpressionType.ModuloAssign:
					esClass = ValueClass;
					kernel = esClass.Kernel;
					selector = kernel.symbolFor("%=");
					break;
				case ExpressionType.MultiplyAssign:
					esClass = ValueClass;
					kernel = esClass.Kernel;
					selector = kernel.symbolFor("*=");
					break;
				case ExpressionType.OrAssign:
					esClass = ValueClass;
					kernel = esClass.Kernel;
					selector = kernel.symbolFor("|=");
					break;
				case ExpressionType.PowerAssign:
					esClass = ValueClass;
					kernel = esClass.Kernel;
					selector = kernel.symbolFor("**=");
					break;
				case ExpressionType.RightShiftAssign:
					esClass = ValueClass;
					kernel = esClass.Kernel;
					selector = kernel.symbolFor(">>=");
					break;
				case ExpressionType.SubtractAssign:
					esClass = ValueClass;
					kernel = esClass.Kernel;
					selector = kernel.symbolFor("-=");
					break;
				case ExpressionType.Extension:
					esClass = ValueClass;
					kernel = esClass.Kernel;
					selector = kernel.symbolFor("??");
					break;
			}

			if (messageSendMO == null) {
				messageSendMO = metaObjectToSendDoesNotUnderstand(selector, DynamicBindingGuru.argArrayFor(arg));
			}

			return binder.FallbackBinaryOperation(this, arg, messageSendMO);

		}

		public override DynamicMetaObject BindConvert(ConvertBinder binder) {
			var esClass = ValueClass;
			var kernel = esClass.Kernel;
			var bindingGuru = this.typeBindingGuru(kernel);
			return new DynamicMetaObject(bindingGuru.metaObjectToConvertTo(binder.Type).Expression, DefaultBindingRestrictions, Value);
		}

		public override DynamicMetaObject BindInvoke(InvokeBinder binder, DynamicMetaObject[] args) {
			var esClass = ValueClass;
			var kernel = esClass.Kernel;
			long numArgs = args.Length;
			return binder.FallbackInvoke(this, args, metaObjectToSendDoesNotUnderstand(kernel.selectorToEvaluatBlockWithNumArgs(numArgs), args));
		}

		public override DynamicMetaObject BindInvokeMember(InvokeMemberBinder binder, DynamicMetaObject[] args) {
			DynamicMetaObject messageSendMO;
			if (createMetaObjectToSendMessage(binder.Name, args, out messageSendMO)) return messageSendMO;
			return binder.FallbackInvokeMember(this, args, messageSendMO);
		}

		public override DynamicMetaObject BindCreateInstance(CreateInstanceBinder binder, DynamicMetaObject[] args) {
			var esClass = ValueClass;
			var kernel = esClass.Kernel;
			long numArgs = args.Length;
			ESSymbol selector;
			switch (numArgs) {
				case 0: 
					selector = kernel.symbolFor("new");
					break;
				case 1: 
					selector = kernel.symbolFor("new:");
					break;
				default:
					selector = kernel.selectorToEvaluatBlockWithNumArgs(numArgs);
					break;
			}
			return binder.FallbackCreateInstance(this, args, metaObjectToSendDoesNotUnderstand(selector, args));
		}

		public override IEnumerable<String> GetDynamicMemberNames() {
			var selectors = new System.Collections.Generic.HashSet<String>();
			var esClass = ValueClass;
			while (esClass != null) {
				esClass.systemSelectorsDo((Object selectorObject, Object argCountObject) => {selectors.Add((String)selectorObject); return null;});
				esClass.selectorsDo(
					(Object selectorObject) => {
						ESSymbol selector = (ESSymbol)selectorObject;
						selectors.Add(selector.asHostSystemMemberName(CapitalizationScheme.InitialCapital));
						return null;
					});
				esClass = esClass.Superclass;
			}
			return selectors;
		}

	}

	public class ESBlockDynamicMetaObject : ESDynamicMetaObject {

		public ESBlockDynamicMetaObject(Expression expression, BindingRestrictions restrictions, object value, ESBehavior valueClass) : base(expression, restrictions, value, valueClass) {}

		public override DynamicMetaObject BindInvoke(InvokeBinder binder, DynamicMetaObject[] args) {
			var block = (ESBlock)Value;
			Expression invokeExpression = null;
			var numArgs = args.Length;
			if (numArgs != block.NumArgs) {
				var esClass = ValueClass;
				var kernel = esClass.Kernel;
				return metaObjectToThrowInvalidFunctionCallException(
						kernel.selectorToEvaluatBlockWithNumArgs(numArgs), 
						args, 
						"Argument count mismatch", 
						ESCompiledCode.blockFunctionTypeForNumArgs(block.NumArgs), 
						ESCompiledCode.blockFunctionTypeForNumArgs(args.Length));
			}
			switch (block.NumArgs) {
				case 0:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F0));
					break;
				case 1:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F1), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 2:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F2), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 3:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F3), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 4:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F4), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 5:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F5), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 6:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F6), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 7:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F7), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 8:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F8), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 9:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F9), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 10:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F10), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 11:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F11), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 12:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F12), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 13:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F13), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 14:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F14), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 15:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F15), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 16:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F16), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 17:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F17), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 18:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F18), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 19:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F19), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 20:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F20), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 21:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F21), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 22:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F22), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 23:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F23), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 24:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F24), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 25:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F25), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 26:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F26), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 27:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F27), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 28:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F28), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 29:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F29), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 30:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F30), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 31:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F31), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 32:
					invokeExpression = Expression.Invoke(Expression.Constant(block.F32), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
			}

			return new DynamicMetaObject(invokeExpression, DefaultBindingRestrictions, block);
 
		}

	}

	public class ESMethodDynamicMetaObject : ESDynamicMetaObject {

		public ESMethodDynamicMetaObject(Expression expression, BindingRestrictions restrictions, object value, ESBehavior valueClass) : base(expression, restrictions, value, valueClass) {}

		public override DynamicMetaObject BindInvoke(InvokeBinder binder, DynamicMetaObject[] args) {
			var method = (ESMethod)Value;
			Expression invokeExpression = null;
			var numArgs = args.Length;
			if (numArgs - method.NumArgs != 1) {
				var esClass = ValueClass;
				var kernel = esClass.Kernel;
				return metaObjectToThrowInvalidFunctionCallException(
						kernel.selectorToEvaluatMethodWithNumArgs(Math.Max(0, numArgs - 1)), 
						args, 
						"Argument count mismatch", 
						ESCompiledCode.methodFunctionTypeForNumArgs(method.NumArgs), 
						ESCompiledCode.blockFunctionTypeForNumArgs(args.Length));
			}
			switch (method.NumArgs) {
				case 1:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F1), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 2:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F2), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 3:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F3), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 4:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F4), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 5:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F5), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 6:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F6), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 7:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F7), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 8:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F8), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 9:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F9), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 10:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F10), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 11:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F11), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 12:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F12), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 13:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F13), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 14:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F14), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 15:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F15), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 16:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F16), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 17:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F17), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 18:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F18), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 19:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F19), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 20:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F20), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 21:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F21), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 22:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F22), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 23:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F23), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 24:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F24), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 25:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F25), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 26:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F26), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 27:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F27), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 28:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F28), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 29:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F29), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 30:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F30), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 31:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F31), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 32:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F32), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
				case 33:
					invokeExpression = Expression.Invoke(Expression.Constant(method.F33), DynamicBindingGuru.expressionArrayFor(args, TypeGuru.objectType));
					break;
			}

			return new DynamicMetaObject(invokeExpression, DefaultBindingRestrictions, method);
 
		}
	}

	public class ESBehaviorDynamicMetaObject : ESDynamicMetaObject {

		public ESBehaviorDynamicMetaObject(Expression expression, BindingRestrictions restrictions, object value, ESBehavior valueClass) : base(expression, restrictions, value, valueClass) {}

		public override DynamicMetaObject BindCreateInstance(CreateInstanceBinder binder, DynamicMetaObject[] args) {
			DynamicMetaObject messageSendMO;
			var esClass = ValueClass;
			var kernel = esClass.Kernel;
			long numArgs = args.Length;
			String selector;
			switch (numArgs) {
				case 0: 
					selector = "new";
					break;
				case 1: 
					if (createMetaObjectToSendMessage("new:", args, out messageSendMO)) return messageSendMO;
					selector = "value:";
					break;
				default:
					selector = kernel.selectorToEvaluatBlockWithNumArgs(numArgs).PrimitiveValue;
					break;
			}
			if (createMetaObjectToSendMessage(selector, args, out messageSendMO)) return messageSendMO;
			return binder.FallbackCreateInstance(this, args, metaObjectToSendDoesNotUnderstand(kernel.symbolFor(selector), args));
		}

	}

}
