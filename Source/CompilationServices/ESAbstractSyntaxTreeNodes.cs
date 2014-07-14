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
using System.Linq;
#if CLR2
using Microsoft.Scripting.Ast;
using Expression = Microsoft.Scripting.Ast.Expression;
#else
using System.Linq.Expressions;
using Expression = System.Linq.Expressions.Expression;
#endif
using EssenceSharp.ParsingServices;
using EssenceSharp.Runtime;
using EssenceSharp.Runtime.Binding;
using EssenceSharp.Exceptions;
using InvalidOperationException = EssenceSharp.Exceptions.InvalidOperationException;
#endregion

namespace EssenceSharp.CompilationServices { 

	// Abstract Syntax Tree Nodes are responsible for emitting CLR/DLR Expression Tree nodes and CallSites.

	public abstract class AbstractSyntaxTreeNode {

		#region Static variables and methods

		public static readonly bool useTailCallOptimization = false;

		public static ExpressionElementType[] newCLRExpressionArray<SourceElementType, ExpressionElementType>(NamespaceObject environment, BehavioralObject behavior, SourceElementType[] elements) 
			where SourceElementType : OperandNode 
			where ExpressionElementType : Expression {
			return Array.ConvertAll<SourceElementType, ExpressionElementType>(elements, element => (ExpressionElementType)element.asCLRExpression(environment, behavior));
		}

		public static ExpressionElementType[] newCLRExpressionArray<SourceElementType, ExpressionElementType>(NamespaceObject environment, BehavioralObject behavior, List<SourceElementType> elements) 
			where SourceElementType : OperandNode 
			where ExpressionElementType : Expression {
			var expArray = new ExpressionElementType[elements.Count];
			for (var i = 0; i < expArray.Length; i++) expArray[i] = (ExpressionElementType)elements[i].asCLRExpression(environment, behavior);
			return expArray;
		}

		#endregion

		protected readonly CodeGenerationContext		context;
		protected readonly NameBindingScope			scope;

		protected AbstractSyntaxTreeNode(CodeGenerationContext context) {
			this.context = context;
			scope = context.Scope;
		}
		
		public CodeGenerationContext Context {
			get {return context;}
		}

		public virtual NameBindingScope Scope {
			get {return scope;}
		}

		public virtual bool IsOperand {
			get {return false;}
		}

		public virtual bool IsAssignable {
			get {return false;}
		}

		public virtual bool IsPseudovariable {
			get {return false;}
		}

		public virtual bool IsSelf {
			get {return false;}
		}

		public virtual bool IsSuper {
			get {return false;}
		}

		public virtual bool IsThisContext {
			get {return false;}
		}

		public virtual MessageReceiverKind ReceiverKind {
			get {return MessageReceiverKind.General;}
		}

		public virtual bool IsMessage {
			get {return false;}
		}

		public virtual bool IsCodeDeclaration {
			get {return false;}
		}

		public virtual bool IsCodeLiteral {
			get {return false;}
		}

		public virtual bool IsMethodLiteral {
			get {return false;}
		}

		public virtual bool IsBlockLiteral {
			get {return false;}
		}

		public virtual bool IsZeroArgBlockLiteral {
			get {return false;}
		}

		public virtual bool IsOneArgBlockLiteral {
			get {return false;}
		}

		public virtual bool CompilesToExpression {
			// Can it be compiled to an Essence# expression?
			get {return false;}
		}

		public virtual bool CompilesToExecutableCode {
			// It's a "doIt"
			get {return false;}
		}

		public virtual bool CompilesToBlockOrMethod {
			// It's a method declaration, a block declaration, a block literal or a method literal.
			get {return false;}
		}

		public virtual bool CompilesToCLRExpressionTree {
			// Can it be compiled to a CLR Expression Tree node? For example, messages without a receiver cannot.
			get {return false;}
		}

		public bool IsCompilable {
			get {return Context.IsCompilable && CompilesToBlockOrMethod || CompilesToExecutableCode || CompilesToExpression;}
		}

		public virtual HashSet<ESSymbol> bindNonLocalVariablesToEnvironment(NamespaceObject environment, BehavioralObject behavior) {
			// By default, do nothing
			return null;
		}

		public virtual void invalidateCachedExpressions() {
			// By default, do nothing
		}

		public virtual Expression asInlinedCLRExpression(NamespaceObject environment, BehavioralObject behavior) {
			return asCLRExpression(environment, behavior);
		}

		public virtual Expression asCLRExpression(NamespaceObject environment, BehavioralObject behavior) {
			return null;
		}

	}

	public abstract class OperandNode : AbstractSyntaxTreeNode {

		protected Expression cachedExpression;

		public OperandNode(CodeGenerationContext context) : base(context) {
		}

		public override bool IsOperand {
			get {return true;}
		}

		public override bool CompilesToCLRExpressionTree {
			get {return true;}
		}

		public override void invalidateCachedExpressions() {
			cachedExpression = null;
		}

	}

	public class ReducedNode : OperandNode {

		protected Expression constantExpression = null;

		public ReducedNode(CodeGenerationContext context) : base(context) {
		}

		public ReducedNode(CodeGenerationContext context, Object constantValue) : base(context) {
			constantExpression = constantValue == null ? ExpressionTreeGuru.nilConstant : Expression.Constant(constantValue);
		}

		public ReducedNode(CodeGenerationContext context, Expression constantValue) : base(context) {
			constantExpression = constantValue == null ? ExpressionTreeGuru.nilConstant : constantValue;
		}

		public override Expression asCLRExpression(NamespaceObject environment, BehavioralObject behavior) {
			return Expression.Convert(constantExpression, TypeGuru.objectType);
		}

		public override string ToString() {
			if (constantExpression != null) {
				return constantExpression.ToString();
			} else {
				return base.ToString();
			}
		}

	}

	public class DynamicArrayLiteralNode : OperandNode {

		protected ExpressionNode[] elements;

		public DynamicArrayLiteralNode(CodeGenerationContext context, ExpressionNode[] elements) : base(context) {
			this.elements = elements;
		}

		public override void invalidateCachedExpressions() {
			base.invalidateCachedExpressions();
			foreach (var element in elements) element.invalidateCachedExpressions();
		}

		public override Expression asCLRExpression(NamespaceObject environment, BehavioralObject behavior) {
			if (cachedExpression != null) return cachedExpression;
			return cachedExpression = ExpressionTreeGuru.expressionToCreateESObjectArray(Context.ObjectSpace.ArrayClass, newCLRExpressionArray<ExpressionNode, Expression>(environment, behavior, elements));
		}

		public override HashSet<ESSymbol> bindNonLocalVariablesToEnvironment(NamespaceObject environment, BehavioralObject behavior) {
			HashSet<ESSymbol> undeclaredVariables = null;
			foreach (var element in elements) {
				var undeclaredVarSet = element.bindNonLocalVariablesToEnvironment(environment, behavior);
				if (undeclaredVarSet != null) {
					if (undeclaredVariables == null) {
						undeclaredVariables = undeclaredVarSet;
					} else {
						foreach (var undeclaredVarName in undeclaredVarSet) undeclaredVariables.Add(undeclaredVarName);
					}
				}
			}
			return undeclaredVariables;
		}

	}

	public class DictionaryLiteralNode : DynamicArrayLiteralNode {

		public DictionaryLiteralNode(CodeGenerationContext context, ExpressionNode[] elements) : base(context, elements) {
		}

		public override Expression asCLRExpression(NamespaceObject environment, BehavioralObject behavior) {
			if (cachedExpression != null) return cachedExpression;
			var associationElements = new List<Expression>();
			foreach (var element in elements) associationElements.Add(Expression.Convert(element.asCLRExpression(environment, behavior), typeof(ESAssociation)));
			return cachedExpression = ExpressionTreeGuru.expressionToCreateESDictionary(Context.ObjectSpace.DictionaryClass, associationElements);		}

	}

	public class VariableReferenceNode : OperandNode {

		protected NamedValueOccurrence variableReference;

		public VariableReferenceNode(CodeGenerationContext context, NamedValueOccurrence variableReference) : base(context) {
			this.variableReference = variableReference;
		}

		public NamedValueOccurrence VariableReference {
			get {return variableReference;}
		}

		public override bool IsAssignable {
			get {return variableReference.IsAssignable;}
		}

		public override Expression asCLRExpression(NamespaceObject environment, BehavioralObject behavior) {
			if (cachedExpression != null) return cachedExpression;
			return cachedExpression = variableReference.asCLRGetValueExpression();
		}

	}

	public abstract class PseudovariableNode : VariableReferenceNode {

		public PseudovariableNode(CodeGenerationContext context, NamedValueOccurrence variableReference) : base(context, variableReference) {
		}

		public override bool IsPseudovariable {
			get {return true;}
		}

	}

	public class SelfNode : PseudovariableNode {

		public SelfNode(CodeGenerationContext context, NamedValueOccurrence variableReference) : base(context, variableReference) {
		}

		public override bool IsSelf {
			get {return true;}
		}

		public override MessageReceiverKind ReceiverKind {
			get {return MessageReceiverKind.Self;}
		}

	}

	public class SuperNode : PseudovariableNode {

		public SuperNode(CodeGenerationContext context, NamedValueOccurrence variableReference) : base(context, variableReference) {
		}

		public override bool IsSuper {
			get {return true;}
		}

		public override MessageReceiverKind ReceiverKind {
			get {return MessageReceiverKind.Super;}
		}

	}

	public class ThisContextNode : PseudovariableNode {

		public ThisContextNode(CodeGenerationContext context, NamedValueOccurrence variableReference) : base(context, variableReference) {
		}

		public override bool IsThisContext {
			get {return true;}
		}

		public override MessageReceiverKind ReceiverKind {
			get {return MessageReceiverKind.ThisContext;}
		}

	}

	public abstract class MessageNode : AbstractSyntaxTreeNode {

		protected ESSymbol selector;

		protected MessageNode(CodeGenerationContext context, ESSymbol selector) : base(context) {
			this.selector = selector;
		}

		public override bool IsMessage {
			get {return true;}
		}

		public virtual bool FirstArgIsBlockLiteral {
			get {return false;}
		}

		public virtual bool FirstArgIsZeroArgBlockLiteral {
			get {return false;}
		}

		public virtual bool FirstArgIsOneArgBlockLiteral {
			get {return false;}
		}

		public virtual bool FirstTwoArgsAreBlockLiterals {
			get {return false;}
		}

		public virtual bool FirstTwoArgsAreZeroArgBlockLiterals {
			get {return false;}
		}

		public virtual bool FirstTwoArgsAreOneArgBlockLiterals {
			get {return false;}
		}

		public ESSymbol Selector {
			get {return selector;}
		}

		public long NumArgs {
			get {return selector.NumArgs;}
		}

		public abstract OperandNode argumentAt(int index);

		public abstract void argumentsDo(Action<OperandNode> enumerator1);
		
		public override HashSet<ESSymbol> bindNonLocalVariablesToEnvironment(NamespaceObject environment, BehavioralObject behavior) {
			HashSet<ESSymbol> undeclaredVariables = null;
			argumentsDo(argument => {
				var undeclaredVarSet = argument.bindNonLocalVariablesToEnvironment(environment, behavior);
				if (undeclaredVarSet != null) {
					if (undeclaredVariables == null) {
						undeclaredVariables = undeclaredVarSet;
					} else {
						foreach (var undeclaredVarName in undeclaredVarSet) undeclaredVariables.Add(undeclaredVarName);
					}
				}
			});
			return undeclaredVariables;
		}

		public void appendArgumentsAsCLRExpressionsTo(NamespaceObject environment, BehavioralObject behavior, List<Expression> arguments) {
			argumentsDo(argument => arguments.Add(argument.asCLRExpression(environment, behavior)));
		}

		public List<Expression> argumentsAsCLRExpressions(NamespaceObject environment, BehavioralObject behavior) {
			var arguments = new List<Expression>();
			appendArgumentsAsCLRExpressionsTo(environment, behavior, arguments);
			return arguments;
		}

	}

	public class UnaryMessageNode : MessageNode {

		public UnaryMessageNode(CodeGenerationContext context, ESSymbol selector) : base(context, selector) {
		}

		public override OperandNode argumentAt(int index) {
			throw new InvalidOperationException("Unary message nodes have no arguments.");
		}

		public override void argumentsDo(Action<OperandNode> enumerator1) {
			// We have none
		}

	}

	public class BinaryMessageNode : MessageNode {
		protected OperandNode operand;

		public BinaryMessageNode(CodeGenerationContext context, ESSymbol selector, OperandNode operand) : base(context, selector) {
			this.operand = operand;
		}

		public override bool FirstArgIsBlockLiteral {
			get {return operand.IsBlockLiteral;}
		}

		public override bool FirstArgIsZeroArgBlockLiteral {
			get {return operand.IsZeroArgBlockLiteral;}
		}

		public override bool FirstArgIsOneArgBlockLiteral {
			get {return operand.IsOneArgBlockLiteral;}
		}

		public OperandNode Operand {
			get {return operand;}
		}

		public override void invalidateCachedExpressions() {
			base.invalidateCachedExpressions();
			operand.invalidateCachedExpressions();
		}

		public override OperandNode argumentAt(int index) {
			if (index < 0) throw new InvalidArgumentException("The argument index must be greater than or equal to zero.");
			if (index > 0) throw new InvalidArgumentException("The argument index must be less than 1.");
			return operand;
		}

		public override void argumentsDo(Action<OperandNode> enumerator1) {
			// We have just one
			enumerator1(Operand);
		}

	}

	public class KeywordMessageNode : MessageNode {
		protected List<OperandNode> operands;

		public KeywordMessageNode(CodeGenerationContext context, ESSymbol selector, List<OperandNode> operands) : base(context, selector) {
			this.operands = operands;
		}

		public override bool FirstArgIsBlockLiteral {
			get {	if (operands.Count != 1) return false;
				return operands[0].IsBlockLiteral;}
		}

		public override bool FirstArgIsZeroArgBlockLiteral {
			get {	if (operands.Count != 1) return false;
				return operands[0].IsZeroArgBlockLiteral;}
		}

		public override bool FirstArgIsOneArgBlockLiteral {
			get {	if (operands.Count != 1) return false;
				return operands[0].IsOneArgBlockLiteral;}
		}

		public override bool FirstTwoArgsAreBlockLiterals {
			get {	if (operands.Count < 2) return false;
				return operands[0].IsBlockLiteral && operands[1].IsBlockLiteral;}
		}

		public override bool FirstTwoArgsAreZeroArgBlockLiterals {
			get {	if (operands.Count < 2) return false;
				return operands[0].IsZeroArgBlockLiteral && operands[1].IsZeroArgBlockLiteral;}
		}

		public override bool FirstTwoArgsAreOneArgBlockLiterals {
			get {	if (operands.Count < 2) return false;
				return operands[0].IsOneArgBlockLiteral && operands[1].IsOneArgBlockLiteral;}
		}

		public override void invalidateCachedExpressions() {
			base.invalidateCachedExpressions();
			foreach (var operand in operands) operand.invalidateCachedExpressions();
		}

		public override OperandNode argumentAt(int index) {
			if (index < 0) throw new InvalidArgumentException("The argument index must be greater than or equal to zero.");
			if (index >= operands.Count) throw new InvalidArgumentException("The argument index must be less than " + operands.Count + ".");
			return operands[index];
		}

		public override void argumentsDo(Action<OperandNode> enumerator1) {
			// We have at least one
			foreach (var operand in operands) enumerator1(operand);
		}

	}

	public class MessageSendNode : OperandNode {

		protected OperandNode receiver;
		protected MessageNode message;

		public MessageSendNode(CodeGenerationContext context, OperandNode receiver, MessageNode message) : base(context) {
			this.receiver = receiver;
			this.message = message;
		}

		public OperandNode Receiver {
			get {return receiver;}
		}

		public MessageNode Message {
			get {return message;}
		}

		public override void invalidateCachedExpressions() {
			base.invalidateCachedExpressions();
			receiver.invalidateCachedExpressions();
			message.invalidateCachedExpressions();
		}
		
		public override HashSet<ESSymbol> bindNonLocalVariablesToEnvironment(NamespaceObject environment, BehavioralObject behavior) {
			var undeclaredVariables = receiver.bindNonLocalVariablesToEnvironment(environment, behavior);
			var undeclaredVarSet = message.bindNonLocalVariablesToEnvironment(environment, behavior);
			if (undeclaredVarSet != null) {
				if (undeclaredVariables == null) {
					undeclaredVariables = undeclaredVarSet;
				} else {
					foreach (var undeclaredVarName in undeclaredVarSet) undeclaredVariables.Add(undeclaredVarName);
				}
			}
			return undeclaredVariables;
		}

		public override Expression asCLRExpression(NamespaceObject environment, BehavioralObject behavior) {
			if (cachedExpression != null) return cachedExpression;

			OperandNode arg1;
			OperandNode arg2;
			Expression receiverExpression = receiver.asCLRExpression(environment, behavior);
			Expression testExpression;
			Expression arg1Expression;
			Expression arg2Expression;
			LabelTarget exit;

			var selector = Message.Selector;

			switch(selector.CanonicalSemantics) {

				// Here is where message sends WILL be inlined:

				case CanonicalSelectorSemantics.IsNil:
					return cachedExpression = Expression.ReferenceEqual(receiverExpression, ExpressionTreeGuru.nilConstant).withCanonicalReturnType();

				case CanonicalSelectorSemantics.IsNotNil:
					return cachedExpression = Expression.ReferenceNotEqual(receiverExpression, ExpressionTreeGuru.nilConstant).withCanonicalReturnType();

				case CanonicalSelectorSemantics.IsIdenticalTo:
					arg1 = message.argumentAt(0);
					return cachedExpression = Expression.ReferenceEqual(receiverExpression, arg1.asCLRExpression(environment, behavior)).withCanonicalReturnType();

				case CanonicalSelectorSemantics.IsNotIdenticalTo:
					arg1 = message.argumentAt(0);
					return cachedExpression = Expression.ReferenceNotEqual(receiverExpression, arg1.asCLRExpression(environment, behavior)).withCanonicalReturnType();

				case CanonicalSelectorSemantics.LogicalNot:
					return cachedExpression = 
						Expression.Not(ExpressionTreeGuru.expressionThatMustBeBoolean(receiverExpression, "The receiver of #not must be a Boolean value")).withCanonicalReturnType();

				case CanonicalSelectorSemantics.ConditionalAnd:
					arg1 = message.argumentAt(0);
					arg1Expression = arg1.asInlinedCLRExpression(environment, behavior);
					return cachedExpression = 
						Expression.AndAlso(
								ExpressionTreeGuru.expressionThatMustBeBoolean(receiverExpression, "The receiver of #and: must be a Boolean value"),
								ExpressionTreeGuru.expressionThatMustBeBoolean(arg1Expression, "The argument of #and: must evaluate to a Boolean value")).withCanonicalReturnType();

				case CanonicalSelectorSemantics.ConditionalOr:
					arg1 = message.argumentAt(0);
					arg1Expression = arg1.asInlinedCLRExpression(environment, behavior);
					return cachedExpression = 
							Expression.OrElse(
								ExpressionTreeGuru.expressionThatMustBeBoolean(receiverExpression, "The receiver of #or: must be a Boolean value"),
								ExpressionTreeGuru.expressionThatMustBeBoolean(arg1Expression, "The argument of #or: must evaluate to a Boolean value")).withCanonicalReturnType();

				// Here is where message sends MIGHT be inlined:

				case CanonicalSelectorSemantics.IfNil:
					if (!message.FirstArgIsZeroArgBlockLiteral) break;
					testExpression = Expression.ReferenceEqual(receiverExpression, ExpressionTreeGuru.nilConstant);
					arg1 = message.argumentAt(0);
					arg1Expression = arg1.asInlinedCLRExpression(environment, behavior);
					return cachedExpression = Expression.Condition(
										testExpression,
											arg1Expression,
											receiverExpression).withCanonicalReturnType();

				case CanonicalSelectorSemantics.IfNotNil:
					if (!message.FirstArgIsZeroArgBlockLiteral) break;
					testExpression = Expression.ReferenceNotEqual(receiverExpression, ExpressionTreeGuru.nilConstant);
					arg1 = message.argumentAt(0);
					arg1Expression = arg1.asInlinedCLRExpression(environment, behavior);
					return cachedExpression = Expression.Condition(
										testExpression,
											arg1Expression,
											receiverExpression);

				case CanonicalSelectorSemantics.IfNilIfNotNil:
					if (!message.FirstTwoArgsAreZeroArgBlockLiterals) break;
					testExpression = Expression.ReferenceEqual(receiverExpression, ExpressionTreeGuru.nilConstant);
					arg1 = message.argumentAt(0);
					arg1Expression = arg1.asInlinedCLRExpression(environment, behavior);
					arg2 = message.argumentAt(1);
					arg2Expression = arg2.asInlinedCLRExpression(environment, behavior);
					return cachedExpression = Expression.Condition(
										testExpression,
											arg1Expression,
											arg2Expression).withCanonicalReturnType();

				case CanonicalSelectorSemantics.IfNotNilIfNil:
					if (!message.FirstTwoArgsAreZeroArgBlockLiterals) break;
					testExpression = Expression.ReferenceNotEqual(receiverExpression, ExpressionTreeGuru.nilConstant);
					arg1 = message.argumentAt(0);
					arg1Expression = arg1.asInlinedCLRExpression(environment, behavior);
					arg2 = message.argumentAt(1);
					arg2Expression = arg2.asInlinedCLRExpression(environment, behavior);
					return cachedExpression = Expression.Condition(
										testExpression,
											arg1Expression,
											arg2Expression).withCanonicalReturnType();

				case CanonicalSelectorSemantics.IfTrue:
					if (!message.FirstArgIsZeroArgBlockLiteral) break;
					arg1 = message.argumentAt(0);
					arg1Expression = arg1.asInlinedCLRExpression(environment, behavior);
					return cachedExpression = Expression.Condition(
										ExpressionTreeGuru.expressionThatMustBeBoolean(receiverExpression, "The receiver of #ifTrue: must be a Boolean value"),
											arg1Expression,
											ExpressionTreeGuru.nilConstant).withCanonicalReturnType();

				case CanonicalSelectorSemantics.IfFalse:
					if (!message.FirstArgIsZeroArgBlockLiteral) break;
					arg1 = message.argumentAt(0);
					arg1Expression = arg1.asInlinedCLRExpression(environment, behavior);
					return cachedExpression = Expression.Condition(
										ExpressionTreeGuru.expressionThatMustBeBoolean(receiverExpression, "The receiver of #ifFalse: must be a Boolean value"),
											ExpressionTreeGuru.nilConstant,
											arg1Expression).withCanonicalReturnType();

				case CanonicalSelectorSemantics.IfTrueIfFalse:
					if (!message.FirstTwoArgsAreZeroArgBlockLiterals) break;
					arg1 = message.argumentAt(0);
					arg1Expression = arg1.asInlinedCLRExpression(environment, behavior);
					arg2 = message.argumentAt(1);
					arg2Expression = arg2.asInlinedCLRExpression(environment, behavior);
					return cachedExpression = Expression.Condition(
										ExpressionTreeGuru.expressionThatMustBeBoolean(receiverExpression, "The receiver of #ifTrue:ifFalse: must be a Boolean value"),
											arg1Expression,
											arg2Expression).withCanonicalReturnType();

				case CanonicalSelectorSemantics.IfFalseIfTrue:
					if (!message.FirstTwoArgsAreZeroArgBlockLiterals) break;
					arg1 = message.argumentAt(0);
					arg1Expression = arg1.asInlinedCLRExpression(environment, behavior);
					arg2 = message.argumentAt(1);
					arg2Expression = arg2.asInlinedCLRExpression(environment, behavior);
					return cachedExpression = Expression.Condition(
										ExpressionTreeGuru.expressionThatMustBeBoolean(receiverExpression, "The receiver of #ifFalse:ifTrue: must be a Boolean value"),
											arg2Expression,
											arg1Expression).withCanonicalReturnType();

				case CanonicalSelectorSemantics.WhileNil:
					if (!receiver.IsBlockLiteral) break;
					exit = Expression.Label(TypeGuru.objectType);
					return cachedExpression = Expression.Loop(
									Expression.IfThenElse(
										Expression.ReferenceEqual(receiver.asInlinedCLRExpression(environment, behavior), ExpressionTreeGuru.nilConstant),
											Expression.Empty(),
											Expression.Break(exit, receiverExpression)),
									exit).withCanonicalReturnType();

				case CanonicalSelectorSemantics.WhileNotNil:
					if (!receiver.IsZeroArgBlockLiteral) break;
					exit = Expression.Label(TypeGuru.objectType);
					return cachedExpression = Expression.Loop(
									Expression.IfThenElse(
										Expression.ReferenceNotEqual(receiver.asInlinedCLRExpression(environment, behavior), ExpressionTreeGuru.nilConstant),
											Expression.Empty(),
											Expression.Break(exit, receiverExpression)),
									exit).withCanonicalReturnType();

				case CanonicalSelectorSemantics.WhileNilDo:
					if (!receiver.IsZeroArgBlockLiteral) break;
					if (!message.FirstArgIsZeroArgBlockLiteral) break;
					arg1 = message.argumentAt(0);
					arg1Expression = arg1.asInlinedCLRExpression(environment, behavior);
					exit = Expression.Label(TypeGuru.objectType);
					return cachedExpression = Expression.Loop(
									Expression.IfThenElse(
										Expression.ReferenceEqual(receiver.asInlinedCLRExpression(environment, behavior), ExpressionTreeGuru.nilConstant),
											arg1Expression,
											Expression.Break(exit, receiverExpression)),
									exit).withCanonicalReturnType();

				case CanonicalSelectorSemantics.WhileNotNilDo:
					if (!receiver.IsZeroArgBlockLiteral) break;
					if (!message.FirstArgIsZeroArgBlockLiteral) break;
					if (!message.FirstArgIsZeroArgBlockLiteral) break;
					arg1 = message.argumentAt(0);
					arg1Expression = arg1.asInlinedCLRExpression(environment, behavior);
					exit = Expression.Label(TypeGuru.objectType);
					return cachedExpression = Expression.Loop(
									Expression.IfThenElse(
										Expression.ReferenceNotEqual(receiver.asInlinedCLRExpression(environment, behavior), ExpressionTreeGuru.nilConstant),
											arg1Expression,
											Expression.Break(exit, receiverExpression)),
									exit).withCanonicalReturnType();

				case CanonicalSelectorSemantics.WhileTrue:
					if (!receiver.IsZeroArgBlockLiteral) break;
					exit = Expression.Label(TypeGuru.objectType);
					return cachedExpression = Expression.Loop(
									Expression.IfThenElse(
										ExpressionTreeGuru.expressionThatMustBeBoolean(receiver.asInlinedCLRExpression(environment, behavior), "The receiver of #whileTrue must evaluate to a Boolean value"),
											Expression.Empty(),
											Expression.Break(exit, receiverExpression)),
									exit).withCanonicalReturnType();

				case CanonicalSelectorSemantics.WhileFalse:
					if (!receiver.IsZeroArgBlockLiteral) break;
					exit = Expression.Label(TypeGuru.objectType);
					return cachedExpression = Expression.Loop(
									Expression.IfThenElse(
										ExpressionTreeGuru.expressionThatMustBeBoolean(receiver.asInlinedCLRExpression(environment, behavior), "The receiver of #whileFalse must evaluate to a Boolean value"),
											Expression.Break(exit, receiverExpression),
											Expression.Empty()),
									exit).withCanonicalReturnType();

				case CanonicalSelectorSemantics.WhileTrueDo:
					if (!receiver.IsZeroArgBlockLiteral) break;
					if (!message.FirstArgIsZeroArgBlockLiteral) break;
					arg1 = message.argumentAt(0);
					arg1Expression = arg1.asInlinedCLRExpression(environment, behavior);
					exit = Expression.Label(TypeGuru.objectType);
					return cachedExpression = Expression.Loop(
									Expression.IfThenElse(
										ExpressionTreeGuru.expressionThatMustBeBoolean(receiver.asInlinedCLRExpression(environment, behavior), "The receiver of #whileTrue: must evaluate to a Boolean value"),
											arg1Expression,
											Expression.Break(exit, receiverExpression)),
									exit).withCanonicalReturnType();

				case CanonicalSelectorSemantics.WhileFalseDo:
					if (!receiver.IsZeroArgBlockLiteral) break;
					if (!message.FirstArgIsZeroArgBlockLiteral) break;
					if (!message.FirstArgIsZeroArgBlockLiteral) break;
					arg1 = message.argumentAt(0);
					arg1Expression = arg1.asInlinedCLRExpression(environment, behavior);
					exit = Expression.Label(TypeGuru.objectType);
					return cachedExpression = Expression.Loop(
									Expression.IfThenElse(
										ExpressionTreeGuru.expressionThatMustBeBoolean(receiver.asInlinedCLRExpression(environment, behavior), "The receiver of #whileFalse: must evaluate to a Boolean value"),
											Expression.Break(exit, receiverExpression),
											arg1Expression),
									exit).withCanonicalReturnType();

				default:

					// Not an inlinable message

					break;

			}

			// Here is where messages will be sent for real:

			// var callSiteConstant = Context.messageSendCallSiteConstantFor(receiver.ReceiverKind, selector);
			var callSiteConstant = Context.messageSendCallSiteConstantFor(receiver.ReceiverKind, behavior, selector);
			if (callSiteConstant == null) Context.markAsUncompilable();

			List<Expression> argumentExpressions = new List<Expression>();
			argumentExpressions.Add(callSiteConstant);
			argumentExpressions.Add(receiverExpression);
			Message.appendArgumentsAsCLRExpressionsTo(environment, behavior, argumentExpressions);
			return cachedExpression = Expression.Invoke(Expression.Field(callSiteConstant, CodeGenerationContext.callSiteType[selector.NumArgs], "Target"), argumentExpressions);

		}

	}

	public abstract class StatementNode : OperandNode {

		public StatementNode(CodeGenerationContext context) : base(context) {
		}

	}

	public class ExpressionNode : StatementNode {
		protected OperandNode operand;

		public ExpressionNode(CodeGenerationContext context, OperandNode operand) : base(context) {
			this.operand = operand;
		}

		public override bool IsBlockLiteral {
			get {return operand.IsBlockLiteral;}
		}

		public override bool IsZeroArgBlockLiteral {
			get {return operand.IsZeroArgBlockLiteral;}
		}

		public override bool IsOneArgBlockLiteral {
			get {return operand.IsOneArgBlockLiteral;}
		}

		public override MessageReceiverKind ReceiverKind {
			get {return operand.ReceiverKind;}
		}

		public OperandNode Operand {
			get {return operand;}
		}

		public override bool CompilesToExpression {
			// It's a compilable expression
			get {return true;}
		}

		public override void invalidateCachedExpressions() {
			base.invalidateCachedExpressions();
			operand.invalidateCachedExpressions();
		}
		
		public override HashSet<ESSymbol> bindNonLocalVariablesToEnvironment(NamespaceObject environment, BehavioralObject behavior) {
			return operand.bindNonLocalVariablesToEnvironment(environment, behavior);
		}

		public override Expression asInlinedCLRExpression(NamespaceObject environment, BehavioralObject behavior) {
			return operand.asInlinedCLRExpression(environment, behavior);
		}

		public override Expression asCLRExpression(NamespaceObject environment, BehavioralObject behavior) {
			return operand.asCLRExpression(environment, behavior);
		}

	}

	public class CascadedMessageExpressionNode : ExpressionNode {
		protected MessageNode[] cascadedMessages;

		public CascadedMessageExpressionNode(CodeGenerationContext context, OperandNode operandExpression, MessageNode[] cascadedMessages) : base(context, operandExpression) {
			this.cascadedMessages = cascadedMessages;
		}

		public void cascadedMessagesDo(Action<MessageNode> enumerator1) {
			if (cascadedMessages == null) return;
			foreach (var message in cascadedMessages) enumerator1(message);
		}

		public override void invalidateCachedExpressions() {
			base.invalidateCachedExpressions();
			cascadedMessagesDo(message => message.invalidateCachedExpressions());
		}

		public override HashSet<ESSymbol> bindNonLocalVariablesToEnvironment(NamespaceObject environment, BehavioralObject behavior) {
			HashSet<ESSymbol> undeclaredVariables = base.bindNonLocalVariablesToEnvironment(environment, behavior);
			if (cascadedMessages != null) { 
				foreach (var messageNode in cascadedMessages) {
					var undeclaredVarSet = messageNode.bindNonLocalVariablesToEnvironment(environment, behavior);
					if (undeclaredVarSet != null) {
						if (undeclaredVariables == null) {
							undeclaredVariables = undeclaredVarSet;
						} else {
							foreach (var undeclaredVarName in undeclaredVarSet) undeclaredVariables.Add(undeclaredVarName);
						}
					}
				}
			}
			return undeclaredVariables;
		}

		public override Expression asCLRExpression(NamespaceObject environment, BehavioralObject behavior) {
			if (cachedExpression != null) return cachedExpression;
			var receiver = Expression.Parameter(TypeGuru.objectType, "$receiver");
			var receiverNode = Context.newReducedNode(receiver);
			var assignReceiver = Expression.Assign(receiver, operand.asCLRExpression(environment, behavior));
			var expressionSequence = new List<Expression>();
			expressionSequence.Add(assignReceiver);
			cascadedMessagesDo(messageNode => {
				var messageSend = Context.newMessageSendNode(receiverNode, messageNode);
				expressionSequence.Add(messageSend.asCLRExpression(environment, behavior));
			});
			return cachedExpression = Expression.Block(TypeGuru.objectType, new ParameterExpression[]{receiver}, expressionSequence);
		}

	}

	public class AssignmentStatementNode : StatementNode {

		protected NamedValueOccurrence[] variables;
		protected OperandNode operand;

		public AssignmentStatementNode(CodeGenerationContext context, NamedValueOccurrence[] variables, OperandNode operand) : base(context) {
			this.variables = variables;
			this.operand = operand;
			foreach (var variable in variables) variable.IsAssignmentTarget = true;
		}

		public OperandNode Operand {
			get {return operand;}
		}

		public void variablesDo(Action<NamedValueOccurrence> enumerator1) {
			if (variables == null) return;
			foreach (var variable in variables) enumerator1(variable);
		}

		public override void invalidateCachedExpressions() {
			base.invalidateCachedExpressions();
			operand.invalidateCachedExpressions();
		}

		public override HashSet<ESSymbol> bindNonLocalVariablesToEnvironment(NamespaceObject environment, BehavioralObject behavior) {
			return operand.bindNonLocalVariablesToEnvironment(environment, behavior);
		}

		public override Expression asCLRExpression(NamespaceObject environment, BehavioralObject behavior) {
			if (cachedExpression != null) return cachedExpression;
			cachedExpression = operand.asCLRExpression(environment, behavior);
			if (variables == null || variables.Length < 1) return cachedExpression; // This would be weird....
			foreach (var variable in variables) cachedExpression = variable.asCLRSetValueExpression(cachedExpression);
			return cachedExpression;
		}

	}

	public class ReturnStatmentNode : StatementNode {

		protected StatementNode statement;

		public ReturnStatmentNode(CodeGenerationContext context, StatementNode statement) : base(context) {
			this.statement = statement;
		}

		public StatementNode StatementNode {
			get {return statement;}
		}

		public override void invalidateCachedExpressions() {
			base.invalidateCachedExpressions();
			statement.invalidateCachedExpressions();
		}

		public override HashSet<ESSymbol> bindNonLocalVariablesToEnvironment(NamespaceObject environment, BehavioralObject behavior) {
			return statement.bindNonLocalVariablesToEnvironment(environment, behavior);
		}

		public override Expression asCLRExpression(NamespaceObject environment, BehavioralObject behavior) {
			if (cachedExpression != null) return cachedExpression;
			if (Scope.IsRoot) {
				return cachedExpression = Expression.Return(Context.ReturnTarget, StatementNode.asCLRExpression(environment, behavior), TypeGuru.objectType);
			}
			var createNonLocalReturnExpression =
				Expression.New(
					TypeGuru.nonLocalReturnExceptionType.GetConstructor(
						ESBehavior.instanceCreationBindingFlags, 
						Type.DefaultBinder, 
						new Type[]{TypeGuru.longType, TypeGuru.objectType},
						null),
					Context.IdentityExpression,
					StatementNode.asCLRExpression(environment, behavior));
			return cachedExpression = Expression.Block(Expression.Throw(createNonLocalReturnExpression), Context.SelfParameter);
		}

	}

	public class ExecutableCodeNode : AbstractSyntaxTreeNode {

		protected List<StatementNode> statements; 
		protected Expression cachedExpression;
 
		public ExecutableCodeNode(CodeGenerationContext context) : base(context) {
			statements = new List<StatementNode>();
		}

		public override bool CompilesToExecutableCode {
			get {return true;}
		}

		public override bool CompilesToCLRExpressionTree {
			get {return true;}
		}

		public int VariableDeclarationCount  {
			get {return Scope.LocalVariableDeclarationCount;}
		}

		public int StatementCount  {
			get {return statements == null ? 0 : statements.Count;}
		}

		public override void invalidateCachedExpressions() {
			cachedExpression = null;
			statementsDo(statement => statement.invalidateCachedExpressions());
		}

		public void variableDeclarationsDo(Action<StackResidentVariableDeclaration> enumerator1) {
			Scope.localVariablesDo(enumerator1);
		}

		public void statementsDo(Action<StatementNode> enumerator1) {
			if (statements == null) return;
			foreach (var statement in statements) enumerator1(statement);
		}

		public void add(StatementNode statement) {
			statements.Add(statement);
		}

		public override HashSet<ESSymbol> bindNonLocalVariablesToEnvironment(NamespaceObject environment, BehavioralObject behavior) {
			HashSet<ESSymbol> undeclaredVariables = null;
			foreach (var statement in statements) {
				var undeclaredVarSet = statement.bindNonLocalVariablesToEnvironment(environment, behavior);
				if (undeclaredVarSet != null) {
					if (undeclaredVariables == null) {
						undeclaredVariables = undeclaredVarSet;
					} else {
						foreach (var undeclaredVarName in undeclaredVarSet) undeclaredVariables.Add(undeclaredVarName);
					}
				}
			}
			return undeclaredVariables;
		}

		public override Expression asCLRExpression(NamespaceObject environment, BehavioralObject behavior) {
			if (cachedExpression != null) return cachedExpression;
			if (StatementCount < 1) return cachedExpression = Context.SelfParameter;
			if (VariableDeclarationCount < 1) {
				return cachedExpression = Expression.Block(
								TypeGuru.objectType,
								newCLRExpressionArray<StatementNode, Expression>(environment, behavior, statements));
			} else {
				var clrVarDeclarations = new List<ParameterExpression>();
				Scope.localVariablesDo(
					delegate (StackResidentVariableDeclaration declaration) {
						if (declaration.OccurrenceCount > 0) 
							clrVarDeclarations.Add(declaration.asCLRDeclarationExpression());
					});
				var statementExpressions = new List<Expression>();
				foreach (var statement in statements) statementExpressions.Add(statement.asCLRExpression(environment, behavior));
				cachedExpression = Expression.Block(TypeGuru.objectType, clrVarDeclarations, statementExpressions);
				return cachedExpression;

				/*
				var exception = Expression.Parameter(typeof(Exception), "ex");
				var reportExceptionExpression = 
					Expression.Block(
						TypeGuru.objectType,
							Expression.Call(
								null,
								typeof(Console).GetMethod("WriteLine", new Type[]{typeof(String)}),
								Expression.Call(exception, typeof(Exception).GetMethod("ToString", new Type[]{}), new Expression[0])),
							exception);
				var catchBlock = Expression.Catch(exception, reportExceptionExpression);
				return Expression.TryCatch(mainBlock, catchBlock);
				*/

			}
		}

	}

	public abstract class CodeDeclarationNode : AbstractSyntaxTreeNode {

		protected List<ParameterExpression>			parmeterExpressions;
		protected ExecutableCodeNode				body;
		protected HashSet<ESSymbol>				undeclaredVariables;

		protected CodeDeclarationNode(CodeGenerationContext context) : base(context) {
		}

		protected CodeDeclarationNode(CodeGenerationContext context, ExecutableCodeNode body) : this(context) {
			this.body = body;
		}

		public override bool IsCodeDeclaration {
			get {return true;}
		}

		public override bool CompilesToBlockOrMethod {
			get {return true;}
		}

		public virtual bool IsPrimitive {
			get {return false;}
		}

		public long Identity {
			get {return Context.Identity;}
		}

		public ExecutableCodeNode Body {
			get {return body;}
			set {body = value;}
		}

		public long NumArgs  {
			get {return Scope.ParameterDeclarationCount;}
		}

		public int VariableDeclarationCount  {
			get {return body == null ? 0 : body.VariableDeclarationCount;}
		}

		public int StatementCount  {
			get {return body == null ? 0 : body.StatementCount;}
		}

		public HashSet<ESSymbol> MessagesSent {
			get {return Context.MessagesSent;}
		}

		public HashSet<ESSymbol> MessagesSentToSelf {
			get {return Context.MessagesSentToSelf;}
		}

		public HashSet<ESSymbol> MessagesSentToSuper {
			get {return Context.MessagesSentToSuper;}
		}

		public HashSet<ESSymbol> MessagesSentToThisContext {
			get {return Context.MessagesSentToThisContext;}
		}

		public HashSet<ESSymbol> UndeclaredVariables {
			get {return undeclaredVariables;}
		}

		public virtual void parameterDeclarationsDo(Action<ParameterDeclaration> enumerator1) {
			Scope.localParametersDo(enumerator1);
		}

		public void variableDeclarationsDo(Action<StackResidentVariableDeclaration> enumerator1) {
			if (body == null) return;
			body.variableDeclarationsDo(enumerator1);
		}

		public void statementsDo(Action<StatementNode> enumerator1) {
			if (body == null) return;
			body.statementsDo(enumerator1);
		}

		protected List<ParameterExpression> newCLRParameterExpressions() {
			return new List<ParameterExpression>();
		}

		protected virtual List<ParameterExpression> computeCLRParameterExpressions() {
			var clrParameterDeclarations =  newCLRParameterExpressions();
			parameterDeclarationsDo(declaration => clrParameterDeclarations.Add(declaration.asCLRDeclarationExpression()));
			return clrParameterDeclarations;
		}

		public List<ParameterExpression> ParameterExpressions {
			get {	if (parmeterExpressions == null) parmeterExpressions = computeCLRParameterExpressions();
				return parmeterExpressions;}
		}

		public override void invalidateCachedExpressions() {
			body.invalidateCachedExpressions();
		}

		protected abstract Expression bodyAsCLRExpression(NamespaceObject environment, BehavioralObject behavior);

		public override HashSet<ESSymbol> bindNonLocalVariablesToEnvironment(NamespaceObject environment, BehavioralObject behavior) {
			undeclaredVariables = Scope.IsRoot ? Scope.bindNonLocalVariablesToEnvironment(environment, behavior) : null;
			var undeclaredVarSet = body.bindNonLocalVariablesToEnvironment(environment, behavior);
			if (undeclaredVarSet != null) {
				if (undeclaredVariables == null) {
					undeclaredVariables = undeclaredVarSet;
				} else {
					foreach (var undeclaredVarName in undeclaredVarSet) undeclaredVariables.Add(undeclaredVarName);				
				}
			}
			return undeclaredVariables;
		}

		public LambdaExpression lambdaFor(NamespaceObject environment, BehavioralObject behavior) {
			invalidateCachedExpressions();
			return (LambdaExpression)asCLRExpression(environment, behavior);
		}

		public Delegate functionFor(NamespaceObject environment, BehavioralObject behavior) {
			return lambdaFor(environment, behavior).Compile();
		}

	}

	public class BlockDeclarationNode : CodeDeclarationNode {

		public BlockDeclarationNode(CodeGenerationContext context) : base(context) {
		}

		public BlockDeclarationNode(CodeGenerationContext context, ExecutableCodeNode body) : base(context, body) {
		}

		protected override Expression bodyAsCLRExpression(NamespaceObject environment, BehavioralObject behavior) {
			var parameters = ParameterExpressions;
			if (StatementCount < 1) {
				if (parameters.Count > 0) {
					return parameters.Last();
				} else if (Scope.IsRoot) {
					return Expression.Block(new []{Context.SelfParameter}, Context.SelfParameter);
				} else {
					return ExpressionTreeGuru.nilConstant;
				}
			}
			var bodyExpression = body.asCLRExpression(environment, behavior);
			if (Scope.IsRoot) {
				var exception = Expression.Parameter(TypeGuru.nonLocalReturnExceptionType, "ex");
				var exceptionReturnValue = Expression.Field(exception, "returnValue");
				var handleNonLocalReturnExpression 
					= Expression.Block(
						Expression.Condition(
							Expression.Equal(Expression.Field(exception, "targetContextIdentity"), Context.IdentityExpression),
								Expression.Assign(Context.ReturnValueParameter, exceptionReturnValue),
								Expression.Block(TypeGuru.objectType, Expression.Rethrow(), exceptionReturnValue)));
				var catchBlock = Expression.Catch(exception, handleNonLocalReturnExpression);
				return Expression.Block(
					Context.ReturnValueParameter.Type,
					new []{Context.SelfParameter, Context.ReturnValueParameter},
					Expression.Assign(Context.SelfParameter, Expression.Constant(Context.SelfValue).withType(TypeGuru.objectType)),
					Expression.TryCatch(Expression.Assign(Context.ReturnValueParameter, bodyExpression.withType(Context.ReturnValueParameter.Type)), catchBlock),
					Expression.Label(Context.ReturnTarget, Context.ReturnValueParameter));
			}
			return bodyExpression;
		}

 		public override Expression asInlinedCLRExpression(NamespaceObject environment, BehavioralObject behavior) {
			bindNonLocalVariablesToEnvironment(environment, behavior);
			return bodyAsCLRExpression(environment, behavior);
		}

 		public override Expression asCLRExpression(NamespaceObject environment, BehavioralObject behavior) {
			bindNonLocalVariablesToEnvironment(environment, behavior);
			var parameters = ParameterExpressions;
			var body = bodyAsCLRExpression(environment, behavior);
			return Expression.Lambda(body, useTailCallOptimization, parameters);

			#region Legacy implementation

			/*
			switch (NumArgs) {
				case 0:
					return Expression.Lambda<Func<Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 1:
					return Expression.Lambda<Func<Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 2:
					return Expression.Lambda<Func<Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 3:
					return Expression.Lambda<Func<Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 4:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 5:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 6:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 7:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 8:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 9:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 10:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 11:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 12:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 13:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 14:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 15:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 16:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 17:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 18:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 19:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 20:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 21:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 22:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 23:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 24:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 25:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 26:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 27:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 28:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 29:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 30:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 31:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 32:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
			}
			*/

			#endregion

		}

	}

	public class MethodDeclarationNode : CodeDeclarationNode {

		protected ESSymbol className;
		protected ESSymbol selector;

		public MethodDeclarationNode(CodeGenerationContext context, ESSymbol className, ESSymbol selector) : base(context) {
			this.className = className;
			this.selector = selector;
		}

		public MethodDeclarationNode(CodeGenerationContext context, ESSymbol className, ESSymbol selector, ExecutableCodeNode body) : base(context, body) {
			this.className = className;
			this.selector = selector;
		}

		public bool SpecifiesClassName {
			get {return className != null;}
		}

		public ESSymbol ClassName {
			get {return className;}
		}

		public ESSymbol Selector {
			get {return selector;}
		}

		public virtual bool SpecifiesInlineOperation {
			get {return false;}
		}

		public virtual MethodOperationType OperationType {
			get {return MethodOperationType.Function;}
		}

		public virtual InlineOperation InlineOperation {
			get {return null;}
		}

		public BehavioralObject findSpecifiedClassIn(NamespaceObject environment) {
			if (SpecifiesClassName) {
				var value = ClassName.valueInNamespaceIfAbsent(environment ?? Context.ObjectSpace.SmalltalkNamespace, AccessPrivilegeLevel.Public, ImportTransitivity.Intransitive, null);
				return value as BehavioralObject;
			} else {
				return null;
			}
		}

		public override HashSet<ESSymbol> bindNonLocalVariablesToEnvironment(NamespaceObject environment, BehavioralObject behavior) {
			if (behavior == null) { 
				if (SpecifiesClassName) {
					behavior = findSpecifiedClassIn(environment);
					if (behavior == null) behavior = environment as BehavioralObject;
				} else {
					behavior = environment as BehavioralObject;
				}
			}
			return base.bindNonLocalVariablesToEnvironment(environment, behavior);;
		}

		public override void parameterDeclarationsDo(Action<ParameterDeclaration> enumerator1) {
			enumerator1(Context.Self);
			base.parameterDeclarationsDo(enumerator1);
		}

		protected override Expression bodyAsCLRExpression(NamespaceObject environment, BehavioralObject behavior) {
			if (StatementCount < 1) return Context.SelfParameter;
			var exception = Expression.Parameter(TypeGuru.nonLocalReturnExceptionType, "ex");
			var exceptionReturnValue = Expression.Field(exception, "returnValue");
			var returnValueExpression = Expression.Return(Context.ReturnTarget, exceptionReturnValue, TypeGuru.objectType);
			var handleNonLocalReturnExpression 
				= Expression.Block(
					Expression.Condition(
						Expression.Equal(Expression.Field(exception, "targetContextIdentity"), Context.IdentityExpression),
							returnValueExpression,
							Expression.Block(TypeGuru.objectType, Expression.Rethrow(), exceptionReturnValue)));
			var catchBlock = Expression.Catch(exception, handleNonLocalReturnExpression);
			return  Expression.Block(
					Expression.TryCatch(body.asCLRExpression(environment, behavior), catchBlock),
					Expression.Label(Context.ReturnTarget, Context.SelfParameter));
		}

 		public override Expression asCLRExpression(NamespaceObject environment, BehavioralObject behavior) {
			if (environment == null) environment = behavior;
			bindNonLocalVariablesToEnvironment(environment, behavior);
			var body = bodyAsCLRExpression(environment, behavior);
			return Expression.Lambda(body, useTailCallOptimization, ParameterExpressions);
			
			#region Legacy implementation

			/*
 			switch (NumArgs) {
				case 0:
					return Expression.Lambda<Func<Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 1:
					return Expression.Lambda<Func<Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 2:
					return Expression.Lambda<Func<Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 3:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 4:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 5:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 6:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 7:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 8:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 9:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 10:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 11:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 12:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 13:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 14:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 15:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 16:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 17:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 18:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 19:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 20:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 21:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 22:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 23:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 24:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 25:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 26:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 27:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 28:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 29:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 30:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 31:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
				case 32:
					return Expression.Lambda<Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>(bodyAsCLRExpression(), true, ParmeterExpressions);
			}
			*/

			#endregion

		}

		public void compileFor(ESMethod method) {
			method.Function = functionFor(method.Environment, method.HomeClass);
		}

	}

	public abstract class PrimitiveMethodDeclarationNode : MethodDeclarationNode {

		protected PrimitiveMethodDeclarationNode(CodeGenerationContext context, ESSymbol className, ESSymbol selector) : base(context, className, selector) {
		}

		protected PrimitiveMethodDeclarationNode(CodeGenerationContext context, ESSymbol className, ESSymbol selector, ExecutableCodeNode body) : base(context, className, selector, body) {
		}

		public override bool IsPrimitive {
			get {return true;}
		}

 		public virtual Expression onFailCodeAsCLRExpression(NamespaceObject environment, BehavioralObject behavior) {
			return base.bodyAsCLRExpression(environment, behavior);
		}

	}

	public class PrimitiveFunctionMethodDeclarationNode : PrimitiveMethodDeclarationNode {

		protected Delegate		primitiveFunction;

		public PrimitiveFunctionMethodDeclarationNode(CodeGenerationContext context, ESSymbol className, ESSymbol selector, Delegate primitiveFunction) : base(context, className, selector) {
			this.primitiveFunction = primitiveFunction;
		}

		public PrimitiveFunctionMethodDeclarationNode(CodeGenerationContext context, ESSymbol className, ESSymbol selector, Delegate primitiveFunction, ExecutableCodeNode body) : base(context, className, selector, body) {
			this.primitiveFunction = primitiveFunction;
		}

		public Object PrimitiveFunction {
			get {return primitiveFunction;}
		}

		protected override Expression bodyAsCLRExpression(NamespaceObject environment, BehavioralObject behavior) {
			var arguments = new List<Expression>();
			foreach (var p in ParameterExpressions) arguments.Add(p);
			var invokePrimExpression = Expression.Invoke(Expression.Convert(Expression.Constant(PrimitiveFunction), ESCompiledCode.methodFunctionTypeForNumArgs(NumArgs)), arguments); 
			var catchBlock = Expression.Catch(TypeGuru.primitiveFailExceptionType, onFailCodeAsCLRExpression(environment, behavior));
			return Expression.TryCatch(invokePrimExpression, catchBlock);
		}

	}

	public class InlineOperationMethodDeclarationNode : PrimitiveMethodDeclarationNode {

		protected InlineOperation	operation;

		public InlineOperationMethodDeclarationNode(CodeGenerationContext context, ESSymbol className, ESSymbol selector, InlineOperation operation) : base(context, className, selector) {
			this.operation = operation;
		}

		public override bool SpecifiesInlineOperation {
			get {return true;}
		}

		public override MethodOperationType OperationType {
			get {return InlineOperation.Type;}
		}

		public override InlineOperation InlineOperation {
			get {return operation;}
		}

 		public override Expression onFailCodeAsCLRExpression(NamespaceObject environment, BehavioralObject behavior) {
			if (StatementCount > 0) return base.bodyAsCLRExpression(environment, behavior);
			var parameters = ParameterExpressions;
			var parametersArray = new Expression[parameters.Count];
			for (var i = 0; i < parametersArray.Length; i++) parametersArray[i] = parameters[i];
			var messageClass = Context.ObjectSpace.MessageClass;
			var message = ExpressionTreeGuru.expressionToCreateMessage(messageClass, Selector, parametersArray);
			return Expression.Lambda(
				ExpressionTreeGuru.expressionToSendDoesNotUnderstand(Context.SelfParameter, behavior, Context.ObjectSpace.SymbolRegistry, message), 
				useTailCallOptimization, 
				ParameterExpressions);
		}

		protected override Expression bodyAsCLRExpression(NamespaceObject environment, BehavioralObject behavior) {
			// The implementation is simply to invoke the method by sending the message specified by its selector, 
			// because the dynamic binding logic won't actually invoke the method function, but will instead emit 
			// appropriate inline code as specified by the InlineOperation. And if the method's function ever is 
			// actually invoked, the dynamic binding logic will nevertheless just do whatever the method's 
			// InlineOperation specifies, without recursively invoking the method's function a second time.
			InlineOperation.OnFailExpression = onFailCodeAsCLRExpression(environment, behavior);
			MessageNode message = null;
			switch (Selector.Type) {
				case SymbolType.Identifier:
					message = Context.newUnaryMessageNode(Selector);
					break;
				case SymbolType.BinaryMessageSelector:
					VariableReferenceNode operand = null;
					Scope.localParametersDo(parameterDeclaration => operand = Context.newVariableReferenceNode(parameterDeclaration.newOccurrenceIn(Scope)));
					message = Context.newBinaryMessageNode(Selector, operand);
					break;
				case SymbolType.Keyword:
					var operands = new List<OperandNode>();
					Scope.localParametersDo(parameterDeclaration => operands.Add(Context.newVariableReferenceNode(parameterDeclaration.newOccurrenceIn(Scope))));
					message = Context.newKeywordMessageNode(Selector, operands);
					break;
			}
			var messageSend = Context.newMessageSendNode(Context.newSelfNode(), message);
			return Expression.Lambda(messageSend.asCLRExpression(environment, behavior), useTailCallOptimization, ParameterExpressions);
		}

	}

	public abstract class CodeLiteralNode : OperandNode {

		protected CodeLiteralNode(CodeGenerationContext context) : base(context) {
		}

		public abstract CodeDeclarationNode DeclarationNode {
			get;
		}

		public override bool IsCodeLiteral {
			get {return true;}
		}

		public override bool CompilesToBlockOrMethod {
			get {return true;}
		}

		public override NameBindingScope Scope {
			get {return DeclarationNode.Scope;}
		}

		public abstract HashSet<ESSymbol> UndeclaredVariables {
			get;
		}

		public long ParameterDeclarationCount  {
			get {return DeclarationNode.NumArgs;}
		}

		public int VariableDeclarationCount  {
			get {return DeclarationNode.VariableDeclarationCount;}
		}

		public int StatementCount  {
			get {return DeclarationNode.StatementCount;}
		}

		public void parameterDeclarationsDo(Action<ParameterDeclaration> enumerator1) {
			DeclarationNode.parameterDeclarationsDo(enumerator1);
		}

		public void variableDeclarationsDo(Action<StackResidentVariableDeclaration> enumerator1) {
			DeclarationNode.variableDeclarationsDo(enumerator1);
		}

		public void statementsDo(Action<StatementNode> enumerator1) {
			DeclarationNode.statementsDo(enumerator1);
		}

	}

	public class BlockLiteralNode : CodeLiteralNode {

		protected BlockDeclarationNode declarationNode;
		
		public BlockLiteralNode(CodeGenerationContext context, BlockDeclarationNode declarationNode) : base(context) {
			this.declarationNode = declarationNode;
		}

		public override bool IsBlockLiteral {
			get {return true;}
		}

		public override bool IsZeroArgBlockLiteral {
			get {return declarationNode.NumArgs == 0;}
		}

		public override bool IsOneArgBlockLiteral {
			get {return declarationNode.NumArgs == 1;}
		}

		public override CodeDeclarationNode DeclarationNode {
			get {return declarationNode;}
		}

		public override HashSet<ESSymbol> UndeclaredVariables {
			get {return declarationNode.UndeclaredVariables;}
		}

		public override void invalidateCachedExpressions() {
			base.invalidateCachedExpressions();
			declarationNode.invalidateCachedExpressions();
		}

		public override HashSet<ESSymbol> bindNonLocalVariablesToEnvironment(NamespaceObject environment, BehavioralObject behavior) {
			return declarationNode.bindNonLocalVariablesToEnvironment(environment, behavior);
		}

 		public override Expression asInlinedCLRExpression(NamespaceObject environment, BehavioralObject behavior) {
			return declarationNode.asInlinedCLRExpression(environment, behavior);
		}

 		public override Expression asCLRExpression(NamespaceObject environment, BehavioralObject behavior) {
			if (cachedExpression != null) return cachedExpression;
			return cachedExpression = ExpressionTreeGuru.expressionToCreateESBlock(
									Context.ObjectSpace.BlockClass, 
									declarationNode.asCLRExpression(environment, behavior),
									declarationNode.NumArgs);
		}

	}

	public class MethodLiteralNode : CodeLiteralNode {

		protected MethodDeclarationNode declarationNode;
	
		public MethodLiteralNode(CodeGenerationContext context, MethodDeclarationNode declarationNode) : base(context) {
			this.declarationNode = declarationNode;
		}

		public override bool IsMethodLiteral {
			get {return true;}
		}

		public override CodeDeclarationNode DeclarationNode {
			get {return declarationNode;}
		}

		public MethodDeclarationNode MethodDeclarationNode {
			get {return (MethodDeclarationNode)declarationNode;}
		}

		public ESSymbol Selector {
			get {return declarationNode.Selector;}
		}

		public override HashSet<ESSymbol> UndeclaredVariables {
			get {return declarationNode.UndeclaredVariables;}
		}

		public override void invalidateCachedExpressions() {
			base.invalidateCachedExpressions();
			declarationNode.invalidateCachedExpressions();
		}

		public override HashSet<ESSymbol> bindNonLocalVariablesToEnvironment(NamespaceObject environment, BehavioralObject behavior) {
			return MethodDeclarationNode.SpecifiesClassName ?
				declarationNode.bindNonLocalVariablesToEnvironment(environment, behavior) : 
				declarationNode.bindNonLocalVariablesToEnvironment(null, null);
		}

 		public override Expression asCLRExpression(NamespaceObject environment, BehavioralObject behavior) {
			if (cachedExpression != null) return cachedExpression;
			return cachedExpression = ExpressionTreeGuru.expressionToCreateESMethod(Context.ObjectSpace.MethodClass, environment, behavior, MethodDeclarationNode);
		}

	}
	
}
