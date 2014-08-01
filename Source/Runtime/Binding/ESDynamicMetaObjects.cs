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

		#region Static variables and methods

		public static DynamicMetaObject[] argArrayFor(DynamicMetaObject arg) {
			return new DynamicMetaObject[]{arg.withCanonicalArgumentType()};
		}

		public static DynamicMetaObject[] argArrayFor(DynamicMetaObject arg0, DynamicMetaObject arg1) {
			return new DynamicMetaObject[]{arg0.withCanonicalArgumentType(), arg1.withCanonicalArgumentType()};
		}

		public static DynamicMetaObject[] argArrayFor(DynamicMetaObject[] argsPrefix, DynamicMetaObject argSuffix) {
			var argList = new List<DynamicMetaObject>(argsPrefix.Length + 1);
			foreach (var mo in argsPrefix) argList.Add(mo.withCanonicalArgumentType());
			argList.Add(argSuffix.withCanonicalArgumentType());
			return argList.ToArray();
		}

		public static DynamicMetaObject[] argArrayFor(DynamicMetaObject argPrefix, DynamicMetaObject[] argsSuffix) {
			var argList = new List<DynamicMetaObject>(argsSuffix.Length + 1);
			argList.Add(argPrefix.withCanonicalArgumentType());
			foreach (var mo in argsSuffix) argList.Add(mo.withCanonicalArgumentType());
			return argList.ToArray();
		}

		public static DynamicMetaObject[] argArrayFor(DynamicMetaObject argPrefix, DynamicMetaObject[] argsInfix, DynamicMetaObject argSuffix) {
			var argList = new List<DynamicMetaObject>(argsInfix.Length + 2);
			argList.Add(argPrefix.withCanonicalArgumentType());
			foreach (var mo in argsInfix) argList.Add(mo.withCanonicalArgumentType());
			argList.Add(argSuffix.withCanonicalArgumentType());
			return argList.ToArray();
		}

		public static DynamicMetaObject[] argArrayFor(DynamicMetaObject[] args) {
			var argList = new List<DynamicMetaObject>(args.Length + 2);
			foreach (var mo in args) argList.Add(mo.withCanonicalArgumentType());
			return argList.ToArray();
		}

		public static Expression[] expressionArrayFor(DynamicMetaObject[] metaObjects) {
			var expressionArray = new Expression[metaObjects.Length];
			int i = 0;
			foreach (var metaObject in metaObjects) {
				expressionArray[i++] = metaObject.Expression.withCanonicalArgumentType();
			}
			return expressionArray;
		}

		#endregion

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

		public virtual bool IsBlock {
			get {return false;}
		}

		public BindingRestrictions DefaultBindingRestrictions {
			get {return this.bindingRestrictionsForESObjectReceiver(ValueClass);}
		}

		public bool createMetaObjectToSendMessage(String messageName, DynamicMetaObject[] args, out DynamicMetaObject messageSendMO) {
			var esClass = ValueClass;
			var objectSpace = esClass.ObjectSpace;
			var dynamicBindingGuru = objectSpace.DynamicBindingGuru;
			long numArgs = args.Length;
			var method = esClass.compiledMethodAtSystemSelector(messageName, numArgs);
			if (method == null) {
				ESSymbol selector;
				SymbolType symbolType;
				long syntacicalNumArgs;
				long ignored;
				messageName = messageName.usingCapitalizationScheme(CapitalizationScheme.InitialLowerCase);
				messageName.classifySymbol(null, out symbolType, out syntacicalNumArgs, out ignored);
				switch (symbolType) {
					case SymbolType.Identifier:
						switch (numArgs) {
							case 0:
								selector = objectSpace.symbolFor(messageName);
								break;
							case 1:
								selector = objectSpace.symbolFor(messageName + ":");
								break;
							default:
								selector = null;
								break;
						}
						break;
					case SymbolType.BinaryMessageSelector:
						selector = numArgs == 1 ? objectSpace.symbolFor(messageName) : null;
						break;
					case SymbolType.Keyword:
						selector = syntacicalNumArgs == numArgs ? objectSpace.symbolFor(messageName) : null;
						break;
					default:
					case SymbolType.String:
						selector = null;
						break;
				}
				if (selector == null) {
					messageSendMO = metaObjectToSendDoesNotUnderstand(objectSpace.symbolFor(messageName), args);
					return false;
				} else {
					if (dynamicBindingGuru.getMethodOrElseTryGetMetaObjectToInvokeVirtualMethodOfESObject(this, esClass, selector, args, out method, out messageSendMO)) {
						return true;
					}
					if (method == null) { 
						messageSendMO =  null;
						return false;
					} else {
						messageSendMO =  DynamicBindingGuru.metaObjectToSendMessage(this, objectSpace, esClass, selector, method, args, this.bindingRestrictionsForESObjectReceiver(esClass));
						return true;
					}
				}
			} else {
				messageSendMO = DynamicBindingGuru.metaObjectToSendMessage(this, objectSpace, esClass, method.Selector, method, args, DefaultBindingRestrictions);
				return true;
			}
		}

		public DynamicMetaObject metaObjectToSendDoesNotUnderstand(ESSymbol selector, DynamicMetaObject[] args) {
			return ExpressionTreeGuru.expressionToSendDoesNotUnderstand(Expression, ValueClass, selector, args).asDynamicMetaObject(DefaultBindingRestrictions, Value);
		}

		public DynamicMetaObject metaObjectToThrowInvalidFunctionCallException(ESSymbol selector, DynamicMetaObject[] args, String messageText, Type expectedFunctionType, Type actualFunctionType) {
			return ExpressionTreeGuru.expressionToThrowInvalidFunctionCallException(Expression, ValueClass, selector, messageText, (long)args.Length, expectedFunctionType, actualFunctionType).asDynamicMetaObject(BindingRestrictions.GetInstanceRestriction(Expression, Value), Value);
		}

		public override DynamicMetaObject BindGetMember(GetMemberBinder binder) {
			DynamicMetaObject messageSendMO;
			if (createMetaObjectToSendMessage(binder.Name, DynamicBindingGuru.emptyArgArray, out messageSendMO)) return messageSendMO;
			return binder.FallbackGetMember(this, messageSendMO);
		}

		public override DynamicMetaObject BindSetMember(SetMemberBinder binder, DynamicMetaObject value) {
			DynamicMetaObject messageSendMO;
			if (createMetaObjectToSendMessage(binder.Name, argArrayFor(value), out messageSendMO)) return messageSendMO;
			return binder.FallbackSetMember(this, messageSendMO, value);
		}

		public override DynamicMetaObject BindDeleteMember(DeleteMemberBinder binder) {
			DynamicMetaObject messageSendMO;
			var kernel = ValueClass.ObjectSpace;
			var memberName = kernel.symbolFor(binder.Name);
			if (createMetaObjectToSendMessage("removeKey:", DynamicBindingGuru.argArrayFor(Expression.Constant(memberName).asDynamicMetaObject(BindingRestrictions.Empty, memberName)), out messageSendMO)) return messageSendMO;
			return binder.FallbackDeleteMember(this, messageSendMO);
		}

		public override DynamicMetaObject BindGetIndex(GetIndexBinder binder, DynamicMetaObject[] indexes) {
			DynamicMetaObject messageSendMO;
			if (createMetaObjectToSendMessage("at:", argArrayFor(indexes), out messageSendMO)) return messageSendMO;
			return binder.FallbackGetIndex(this, indexes, messageSendMO);
		}

		public override DynamicMetaObject BindSetIndex(SetIndexBinder binder, DynamicMetaObject[] indexes, DynamicMetaObject value) {
			DynamicMetaObject messageSendMO;
			if (createMetaObjectToSendMessage("at:put:", argArrayFor(indexes[0], value), out messageSendMO)) return messageSendMO;
			return binder.FallbackSetIndex(this, indexes, value, messageSendMO);
		}

		public override DynamicMetaObject BindDeleteIndex(DeleteIndexBinder binder, DynamicMetaObject[] indexes) {
			DynamicMetaObject messageSendMO;
			if (createMetaObjectToSendMessage("removeAt:", argArrayFor(indexes[0]), out messageSendMO)) return messageSendMO;
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
					if (createMetaObjectToSendMessage("=", argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.IsFalse:
 					value = Expression.Constant(false);
					arg = value.asDynamicMetaObject(BindingRestrictions.Empty, false);
					if (createMetaObjectToSendMessage("=", argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				default:
				case ExpressionType.Extension:
					messageSendMO = null;
					break;
			}

			if (messageSendMO == null) {
				var esClass = ValueClass;
				var kernel = esClass.ObjectSpace;
				var selector = kernel.symbolFor("??");
				messageSendMO = metaObjectToSendDoesNotUnderstand(selector, DynamicBindingGuru.emptyArgArray);
			}
			return binder.FallbackUnaryOperation(this, messageSendMO);

		}

		public override DynamicMetaObject BindBinaryOperation(BinaryOperationBinder binder, DynamicMetaObject arg) {

			DynamicMetaObject messageSendMO		= null;
			ESBehavior esClass			= null;
			ESObjectSpace kernel;
			ESSymbol selector			= null;

			switch (binder.Operation) {
				case ExpressionType.Add:
					if (createMetaObjectToSendMessage("+", argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.And:
					if (createMetaObjectToSendMessage("&", argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.Divide:
					if (createMetaObjectToSendMessage("/", argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.Equal:
					if (createMetaObjectToSendMessage("=", argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.ExclusiveOr:
					if (createMetaObjectToSendMessage("xor:", argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.GreaterThan:
					if (createMetaObjectToSendMessage(">", argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.GreaterThanOrEqual:
					if (createMetaObjectToSendMessage(">=", argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.LeftShift:
					if (createMetaObjectToSendMessage("<<", argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.LessThan:
					if (createMetaObjectToSendMessage("<", argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.LessThanOrEqual:
					if (createMetaObjectToSendMessage("<=", argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.Modulo:
					if (createMetaObjectToSendMessage("rem:", argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.Multiply:
					if (createMetaObjectToSendMessage("*", argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.NotEqual:
					if (createMetaObjectToSendMessage("~=", argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.Or:
					if (createMetaObjectToSendMessage("|", argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.Power:
					if (createMetaObjectToSendMessage("**", argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.RightShift:
					if (createMetaObjectToSendMessage(">>", argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				case ExpressionType.Subtract:
					if (createMetaObjectToSendMessage("-", argArrayFor(arg), out messageSendMO)) return messageSendMO;
					break;
				default:
				case ExpressionType.AddAssign:
					esClass = ValueClass;
					kernel = esClass.ObjectSpace;
					selector = kernel.symbolFor("+=");
					break;
				case ExpressionType.AndAssign:
					esClass = ValueClass;
					kernel = esClass.ObjectSpace;
					selector = kernel.symbolFor("&=");
					break;
				case ExpressionType.DivideAssign:
					esClass = ValueClass;
					kernel = esClass.ObjectSpace;
					selector = kernel.symbolFor("/=");
					break;
				case ExpressionType.ExclusiveOrAssign:
					esClass = ValueClass;
					kernel = esClass.ObjectSpace;
					selector = kernel.symbolFor("^=");
					break;
				case ExpressionType.LeftShiftAssign:
					esClass = ValueClass;
					kernel = esClass.ObjectSpace;
					selector = kernel.symbolFor("<<=");
					break;
				case ExpressionType.ModuloAssign:
					esClass = ValueClass;
					kernel = esClass.ObjectSpace;
					selector = kernel.symbolFor("%=");
					break;
				case ExpressionType.MultiplyAssign:
					esClass = ValueClass;
					kernel = esClass.ObjectSpace;
					selector = kernel.symbolFor("*=");
					break;
				case ExpressionType.OrAssign:
					esClass = ValueClass;
					kernel = esClass.ObjectSpace;
					selector = kernel.symbolFor("|=");
					break;
				case ExpressionType.PowerAssign:
					esClass = ValueClass;
					kernel = esClass.ObjectSpace;
					selector = kernel.symbolFor("**=");
					break;
				case ExpressionType.RightShiftAssign:
					esClass = ValueClass;
					kernel = esClass.ObjectSpace;
					selector = kernel.symbolFor(">>=");
					break;
				case ExpressionType.SubtractAssign:
					esClass = ValueClass;
					kernel = esClass.ObjectSpace;
					selector = kernel.symbolFor("-=");
					break;
				case ExpressionType.Extension:
					esClass = ValueClass;
					kernel = esClass.ObjectSpace;
					selector = kernel.symbolFor("??");
					break;
			}

			if (messageSendMO == null) {
				messageSendMO = metaObjectToSendDoesNotUnderstand(selector, argArrayFor(arg));
			}

			return binder.FallbackBinaryOperation(this, arg, messageSendMO);

		}

		public override DynamicMetaObject BindConvert(ConvertBinder binder) {
			var esClass = ValueClass;
			var kernel = esClass.ObjectSpace;
			var bindingGuru = this.typeBindingGuru(kernel);
			return new DynamicMetaObject(bindingGuru.metaObjectToConvertTo(binder.Type).Expression, DefaultBindingRestrictions, Value);
		}

		public override DynamicMetaObject BindInvoke(InvokeBinder binder, DynamicMetaObject[] args) {
			var esClass = ValueClass;
			var kernel = esClass.ObjectSpace;
			long numArgs = args.Length;
			return binder.FallbackInvoke(this, args, metaObjectToSendDoesNotUnderstand(kernel.selectorToEvaluatBlockWithNumArgs(numArgs), args));
		}

		public override DynamicMetaObject BindInvokeMember(InvokeMemberBinder binder, DynamicMetaObject[] args) {
			DynamicMetaObject messageSendMO;
			if (createMetaObjectToSendMessage(binder.Name, argArrayFor(args), out messageSendMO)) return messageSendMO;
			return binder.FallbackInvokeMember(this, args, messageSendMO);
		}

		public override DynamicMetaObject BindCreateInstance(CreateInstanceBinder binder, DynamicMetaObject[] args) {
			var esClass = ValueClass;
			var kernel = esClass.ObjectSpace;
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

		public override bool IsBlock {
			get {return true;}
		}

		public override DynamicMetaObject BindInvoke(InvokeBinder binder, DynamicMetaObject[] args) {
			var block = (ESBlock)Value;
			Expression invokeExpression = null;
			var numArgs = args.Length;
			if (numArgs != block.NumArgs) {
				var esClass = ValueClass;
				var objectSpace = esClass.ObjectSpace;
				return metaObjectToThrowInvalidFunctionCallException(
						objectSpace.selectorToEvaluatBlockWithNumArgs(numArgs), 
						args, 
						"Argument count mismatch", 
						ESCompiledCode.blockFunctionTypeForNumArgs(block.NumArgs), 
						ESCompiledCode.blockFunctionTypeForNumArgs(args.Length));
			}

			invokeExpression = ExpressionTreeGuru.expressionToInvokeESBlock(this.asExpressionWithFormalType(), expressionArrayFor(args));

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
				var kernel = esClass.ObjectSpace;
				return metaObjectToThrowInvalidFunctionCallException(
						kernel.selectorToEvaluatMethodWithNumArgs(Math.Max(0, numArgs - 1)), 
						args, 
						"Argument count mismatch", 
						ESCompiledCode.methodFunctionTypeForNumArgs(method.NumArgs), 
						ESCompiledCode.blockFunctionTypeForNumArgs(args.Length));
			}

			invokeExpression = ExpressionTreeGuru.expressionToInvokeESMethod(this.asExpressionWithFormalType(), expressionArrayFor(args));

			return new DynamicMetaObject(invokeExpression, DefaultBindingRestrictions, method);
 
		}
	}

	public class ESBehaviorDynamicMetaObject : ESDynamicMetaObject {

		public ESBehaviorDynamicMetaObject(Expression expression, BindingRestrictions restrictions, object value, ESBehavior valueClass) : base(expression, restrictions, value, valueClass) {}

		public override DynamicMetaObject BindCreateInstance(CreateInstanceBinder binder, DynamicMetaObject[] args) {
			DynamicMetaObject messageSendMO;
			var esClass = ValueClass;
			var kernel = esClass.ObjectSpace;
			long numArgs = args.Length;
			String selector;
			switch (numArgs) {
				case 0: 
					selector = "new";
					break;
				case 1: 
					if (createMetaObjectToSendMessage("new:", argArrayFor(args), out messageSendMO)) return messageSendMO;
					selector = "value:";
					break;
				default:
					selector = kernel.selectorToEvaluatBlockWithNumArgs(numArgs).PrimitiveValue;
					break;
			}
			if (createMetaObjectToSendMessage(selector,  argArrayFor(args), out messageSendMO)) return messageSendMO;
			return binder.FallbackCreateInstance(this, args, metaObjectToSendDoesNotUnderstand(kernel.symbolFor(selector), args));
		}

	}

}
