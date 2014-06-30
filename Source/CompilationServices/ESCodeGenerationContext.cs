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
using System.Runtime.CompilerServices;
#if CLR2
using Microsoft.Scripting.Ast;
using Expression = Microsoft.Scripting.Ast.Expression;
#else
using System.Linq.Expressions;
using Expression = System.Linq.Expressions.Expression;
#endif
using EssenceSharp.Runtime;
using EssenceSharp.Runtime.Binding;
#endregion

namespace EssenceSharp.CompilationServices {

	public class CodeGenerationContext {

		#region Static variables and functions


		private static long										identityGenerator			= 0;
		public static String										selfName				= "self";
		public static String										superName				= "super";
		public static String										thisContextName				= "thisContext";

		public static readonly int targetMethodMaxArgs = ESCompiledCode.MaxArgs + 2;		// CallSite + Args + receiver
		public static readonly Type[] callSiteType = new Type[targetMethodMaxArgs];
		
		static CodeGenerationContext() {

			// Functor: <ReturnType> <CallSiteType> <ReceiverType> <Arg1Type> <Arg2Type> etc.
			callSiteType[0] = typeof(CallSite<Functor2<Object, CallSite, Object>>); 
			callSiteType[1] = typeof(CallSite<Functor3<Object, CallSite, Object, Object>>);
			callSiteType[2] = typeof(CallSite<Functor4<Object, CallSite, Object, Object, Object>>);
			callSiteType[3] = typeof(CallSite<Functor5<Object, CallSite, Object, Object, Object, Object>>);
			callSiteType[4] = typeof(CallSite<Functor6<Object, CallSite, Object, Object, Object, Object, Object>>);
			callSiteType[5] = typeof(CallSite<Functor7<Object, CallSite, Object, Object, Object, Object, Object, Object>>);
			callSiteType[6] = typeof(CallSite<Functor8<Object, CallSite, Object, Object, Object, Object, Object, Object, Object>>);
			callSiteType[7] = typeof(CallSite<Functor9<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object>>);
			callSiteType[8] = typeof(CallSite<Functor10<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object>>);
			callSiteType[9] = typeof(CallSite<Functor11<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>);
			callSiteType[10] = typeof(CallSite<Functor12<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>);
			callSiteType[11] = typeof(CallSite<Functor13<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>);
			callSiteType[12] = typeof(CallSite<Functor14<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>);
			callSiteType[13] = typeof(CallSite<Functor15<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>);
			callSiteType[14] = typeof(CallSite<Functor16<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>);
			callSiteType[15] = typeof(CallSite<Functor17<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>);
			callSiteType[16] = typeof(CallSite<Functor18<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>);
			callSiteType[17] = typeof(CallSite<Functor19<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>);
			callSiteType[18] = typeof(CallSite<Functor20<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>);
			callSiteType[19] = typeof(CallSite<Functor21<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>);
			callSiteType[20] = typeof(CallSite<Functor22<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>);
			callSiteType[21] = typeof(CallSite<Functor23<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>);
			callSiteType[22] = typeof(CallSite<Functor24<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>);
			callSiteType[23] = typeof(CallSite<Functor25<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>);
			callSiteType[24] = typeof(CallSite<Functor26<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>);
			callSiteType[25] = typeof(CallSite<Functor27<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>);
			callSiteType[26] = typeof(CallSite<Functor28<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>);
			callSiteType[27] = typeof(CallSite<Functor29<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>);
			callSiteType[28] = typeof(CallSite<Functor30<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>);
			callSiteType[29] = typeof(CallSite<Functor31<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>);
			callSiteType[30] = typeof(CallSite<Functor32<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>);
			callSiteType[31] = typeof(CallSite<Functor33<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>);
			callSiteType[32] = typeof(CallSite<Functor34<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>);

		}

		#endregion

		protected long				identity			= identityGenerator++;
		protected ConstantExpression		identityExpression;
		protected ESObjectSpace			objectSpace;
		protected SymbolRegistry		symbolRegistry;
		protected MessageSendBinder.Registry	messageSendBinderRegistry;
		protected ParameterExpression		selfParameter			= Expression.Parameter(TypeGuru.objectType, selfName);
		protected ParameterExpression		thisContextParameter		= Expression.Parameter(TypeGuru.objectType, thisContextName);
		protected ESSymbol			selfSymbol;
		protected ESSymbol			superSymbol;
		protected ESSymbol			thisContextSymbol;
		protected Object			selfValue;
		protected ParameterExpression[]		rootParameters;
		protected ESSymbol			methodSelector;
		protected HashSet<ESSymbol>		messagesSent;
		protected HashSet<ESSymbol>		messagesSentToSelf;
		protected HashSet<ESSymbol>		messagesSentToSuper;
		protected HashSet<ESSymbol>		messagesSentToThisContext;
		protected NameBindingScope		scope;
		protected PseudovariableSelf		self;
		protected PseudovariableSuper		super;
		protected PseudovariableThisContext	thisContext;
		protected LabelTarget			returnTarget			= Expression.Label(TypeGuru.objectType, "$methodReturnTarget");
		protected ParameterExpression		returnValueParameter		= Expression.Parameter(TypeGuru.objectType, "$returnValue");
		protected bool				isCompilable			= true;

		public CodeGenerationContext(ESCompiler compiler) {
			identityExpression		= Expression.Constant(identity);
			objectSpace				= compiler.ObjectSpace;
			bindToKernel();
		}

		public CodeGenerationContext(ESCompiler compiler, Object selfValue, ParameterExpression[] rootParameters) : this(compiler) {
			this.selfValue		= selfValue;
			this.rootParameters	= rootParameters;
		}

		protected void bindToKernel() {
			messageSendBinderRegistry	= objectSpace.MessageSendBinderRegistry;
			symbolRegistry			= objectSpace.SymbolRegistry;
			selfSymbol			= objectSpace.SymbolRegistry.symbolFor(selfName);
			superSymbol			= objectSpace.SymbolRegistry.symbolFor(superName);
			thisContextSymbol		= objectSpace.SymbolRegistry.symbolFor(thisContextName);
		}

		public long Identity {
			get {return identity;}
		}

		public Expression IdentityExpression {
			get {return identityExpression;}
		}

		public ESObjectSpace ObjectSpace {
			get {return objectSpace;}
		}

		public ESSymbol symbolFor(String value) {
			return symbolRegistry.symbolFor(value);
		}

		public ESSymbol SelfSymbol {
			get {return selfSymbol;}
		}

		public ESSymbol SuperSymbol {
			get {return superSymbol;}
		}

		public ESSymbol ThisContextSymbol {
			get {return thisContextSymbol;}
		}

		public ParameterExpression SelfParameter {
			get {return selfParameter;}
		}

		public ParameterExpression ThisContextParameter {
			get {return thisContextParameter;}
		}

		public Object SelfValue {
			get {return selfValue;}
		}

		public bool HasRootParameters {
			get {return rootParameters != null;}
		}

		public void rootParametersDo(Action<ParameterExpression> enumerator) {
			if (rootParameters == null) return;
			for (var i = 0; i < rootParameters.Length; i++) enumerator(rootParameters[i]);
		}

		public LabelTarget ReturnTarget {
			get {return returnTarget;}
		}

		public ParameterExpression ReturnValueParameter {
			get {return returnValueParameter;}
		}

		public bool IsCompilable {
			get {return isCompilable;}
		}

		public void markAsUncompilable() {
			isCompilable = false;
		}

		#region Scope Operations

		public ESSymbol MethodSelector {
			get {return methodSelector;}
			set {methodSelector = value;}
		}

		public PseudovariableSelf Self {
			get {return self;}
		}

		public PseudovariableSuper Super {
			get {return super;}
		}

		public PseudovariableThisContext ThisContext {
			get {return thisContext;}
		}

		public NameBindingScope Scope {
			get {return scope;}
		}

		public void setRootScope() {
			scope = new NameBindingScope(this);
			self = scope.declareSelf();
			super = scope.declareSuper();
			thisContext = scope.declareThisContext();
		}

		public void setRootScopeIfNone() {
			if (scope == null) setRootScope();
		}

		public NameBindingScope pushScope() {
			if (scope == null) {
				setRootScope();
				return scope;
			}
			scope = new NameBindingScope(this, scope);
			return scope;
		}

		public NameBindingScope popScope() {
			NameBindingScope poppedScope = scope;
			if (!scope.IsRoot) {
				scope = scope.OuterScope;
			}
			return poppedScope;
		}

		#endregion

		#region Messages Sent

		protected HashSet<ESSymbol> newMessagesSentSet() {
			return new HashSet<ESSymbol>(new SymbolIdentityComparator());
		}

		public HashSet<ESSymbol> MessagesSent {
			get {return messagesSent == null ? null : messagesSent;}
		}

		public HashSet<ESSymbol> MessagesSentToSelf {
			get {return messagesSentToSelf == null ? null : messagesSentToSelf;}
		}

		public HashSet<ESSymbol> MessagesSentToSuper {
			get {return messagesSentToSuper == null ? null : messagesSentToSuper;}
		}

		public HashSet<ESSymbol> MessagesSentToThisContext {
			get {return messagesSentToThisContext == null ? null : messagesSentToThisContext;}
		}

		protected void logMessageSent(ESSymbol messageSelector) {
			if (messagesSent == null) messagesSent = newMessagesSentSet();
			messagesSent.Add(messageSelector);
		}

		protected void logMessageSentToSelf(ESSymbol messageSelector) {
			logMessageSent(messageSelector);
			if (messagesSentToSelf == null) messagesSentToSelf = newMessagesSentSet();
			messagesSentToSelf.Add(messageSelector);
		}

		protected void logMessageSentToSuper(ESSymbol messageSelector) {
			logMessageSent(messageSelector);
			if (messagesSentToSuper == null) messagesSentToSuper = newMessagesSentSet();
			messagesSentToSuper.Add(messageSelector);
		}

		protected void logMessageSentToThisContext(ESSymbol messageSelector) {
			logMessageSent(messageSelector);
			if (messagesSentToThisContext == null) messagesSentToThisContext = newMessagesSentSet();
			messagesSentToThisContext.Add(messageSelector);
		}

		#endregion

		#region AST Node Instantiation

		public ReducedNode newReducedNode(Object reducedValue) {
			return new ReducedNode(this, reducedValue);
		}

		public ReducedNode newReducedNode(Expression reducedValue) {
			return new ReducedNode(this, reducedValue);
		}

		public DynamicArrayLiteralNode newDynamicArrayLiteralNode(List<ExpressionNode> elements) {
			return new DynamicArrayLiteralNode(this, elements.ToArray());
		}

		public DictionaryLiteralNode newDictionaryLiteralNode(List<ExpressionNode>  elements) {
			return new DictionaryLiteralNode(this, elements.ToArray());
		}

		public SelfNode newSelfNode() {
			return new SelfNode(this, Self.newOccurrenceIn(Scope));
		}

		public SuperNode newSuperNode() {
			return new SuperNode(this, Super.newOccurrenceIn(Scope));
		}

		public ThisContextNode newThisContextNode() {
			return new ThisContextNode(this, ThisContext.newOccurrenceIn(Scope));
		}

		public VariableReferenceNode newVariableReferenceNode(NamedValueOccurrence variableReference) {
			return new VariableReferenceNode(this, variableReference);
		}

		public UnaryMessageNode newUnaryMessageNode(ESSymbol selector) {
			return new UnaryMessageNode(this, selector);
		}

		public BinaryMessageNode newBinaryMessageNode(ESSymbol selector, OperandNode operand) {
			return new BinaryMessageNode(this, selector, operand);
		}

		public KeywordMessageNode newKeywordMessageNode(ESSymbol selector, List<OperandNode> operands) {
			return new KeywordMessageNode(this, selector, operands);
		}

		public MessageSendNode newMessageSendNode(OperandNode receiver, MessageNode message) {
			return new MessageSendNode(this, receiver, message);
		}

		public ExpressionNode newExpressionNode(OperandNode operand) {
			return new ExpressionNode(this, operand);
		}

		public ExpressionNode newCascadedMessageExpressionNode(OperandNode operandExpression, List<MessageNode> cascadedMessages) {
			return cascadedMessages == null || cascadedMessages.Count < 1 ?
				newExpressionNode(operandExpression) :
				new CascadedMessageExpressionNode(this, operandExpression, cascadedMessages.ToArray());
		}

		public AssignmentStatementNode newAssignmentStatementNode(NamedValueOccurrence[] variables, OperandNode operand) {
			return new AssignmentStatementNode(this, variables, operand);
		}

		public ReturnStatmentNode newReturnStatementNode(StatementNode statement) {
			return new ReturnStatmentNode(this, statement);
		}

		public ExecutableCodeNode newExecutableCodeNode() {
			return new ExecutableCodeNode(this);
		}

		public BlockDeclarationNode newBlockDeclarationNode() {
			return new BlockDeclarationNode(this);
		}

		public MethodDeclarationNode newMethodDeclarationNode(ESSymbol selector) {
			return new MethodDeclarationNode(this, selector);
		}

		public PrimitiveFunctionMethodDeclarationNode newPrimitiveFunctionMethodDeclarationNode(ESSymbol selector, Delegate primitiveFunction) {
			return new PrimitiveFunctionMethodDeclarationNode(this, selector, primitiveFunction);
		}

		public InlineOperationMethodDeclarationNode newInlineOperationMethodDeclarationNode(ESSymbol selector, InlineOperation operation) {
			return new InlineOperationMethodDeclarationNode(this, selector, operation);
		}

		public BlockLiteralNode newBlockLiteralNode(BlockDeclarationNode declarationNode) {
			return new BlockLiteralNode(this, declarationNode);
		}

		public MethodLiteralNode newMethodLiteralNode(MethodDeclarationNode declarationNode) {
			return new MethodLiteralNode(this, declarationNode);
		}

		#endregion

		#region Dynamic Binding

		public GetVariableValueBinder canonicalGetVariableBinderFor(NamespaceObject environment, ESSymbol name) {
			return objectSpace.GetVariableValueBinderRegistry.canonicalBinderFor(name, environment);
		}

		public SetVariableValueBinder canonicalSetVariableBinderFor(NamespaceObject environment, ESSymbol name) {
			return objectSpace.SetVariableValueBinderRegistry.canonicalBinderFor(name, environment);;
		}

		public ConstantExpression getVariableValueCallSiteConstantFor(NamespaceObject environment, ESSymbol variableName) {
			return Expression.Constant(CallSite<Functor2<Object, CallSite, Object>>.Create(canonicalGetVariableBinderFor(environment, variableName)));
		}

		public ConstantExpression setVariableValueCallSiteConstantFor(NamespaceObject environment, ESSymbol variableName) {
			return Expression.Constant(CallSite<Functor3<Object, CallSite, Object, Object>>.Create(canonicalSetVariableBinderFor(environment, variableName)));
		}

		public MessageSendBinder.Registry MessageSendBinderRegistry {
			get {return messageSendBinderRegistry;}
		}

		public ConstantExpression messageSendCallSiteConstantFor(MessageReceiverKind receiverKind, BehavioralObject selfReceiverClass, ESSymbol messageSelector) {
			switch (receiverKind) {
				case MessageReceiverKind.General:
					logMessageSent(messageSelector);
					break;
				case MessageReceiverKind.Self:
					logMessageSentToSelf(messageSelector);
					break;
				case MessageReceiverKind.Super:
					logMessageSentToSuper(messageSelector);
					break;
				case MessageReceiverKind.ThisContext:
					logMessageSentToThisContext(messageSelector);
					break;
			}
			var binder = MessageSendBinderRegistry.canonicalBinderFor(receiverKind, selfReceiverClass, messageSelector);
			switch (messageSelector.NumArgs) {
  				case 0:
					return Expression.Constant(CallSite<Functor2<Object, CallSite, Object>>.Create(binder));
				case 1:
					return Expression.Constant(CallSite<Functor3<Object, CallSite, Object, Object>>.Create(binder));
				case 2:
					return Expression.Constant(CallSite<Functor4<Object, CallSite, Object, Object, Object>>.Create(binder));
				case 3:
					return Expression.Constant(CallSite<Functor5<Object, CallSite, Object, Object, Object, Object>>.Create(binder));
				case 4:
					return Expression.Constant(CallSite<Functor6<Object, CallSite, Object, Object, Object, Object, Object>>.Create(binder));
				case 5:
					return Expression.Constant(CallSite<Functor7<Object, CallSite, Object, Object, Object, Object, Object, Object>>.Create(binder));
				case 6:
					return Expression.Constant(CallSite<Functor8<Object, CallSite, Object, Object, Object, Object, Object, Object, Object>>.Create(binder));
				case 7:
					return Expression.Constant(CallSite<Functor9<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object>>.Create(binder));
				case 8:
					return Expression.Constant(CallSite<Functor10<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object>>.Create(binder));
				case 9:
					return Expression.Constant(CallSite<Functor11<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>.Create(binder));
				case 10:
					return Expression.Constant(CallSite<Functor12<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>.Create(binder));
				case 11:
					return Expression.Constant(CallSite<Functor13<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>.Create(binder));
				case 12:
					return Expression.Constant(CallSite<Functor14<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>.Create(binder));
				case 13:
					return Expression.Constant(CallSite<Functor15<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>.Create(binder));
				case 14:
					return Expression.Constant(CallSite<Functor16<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>.Create(binder));
				case 15:
					return Expression.Constant(CallSite<Functor17<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>.Create(binder));
				case 16:
					return Expression.Constant(CallSite<Functor18<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>.Create(binder));
				case 17:
					return Expression.Constant(CallSite<Functor19<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>.Create(binder));
				case 18:
					return Expression.Constant(CallSite<Functor20<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>.Create(binder));
				case 19:
					return Expression.Constant(CallSite<Functor21<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>.Create(binder));
				case 20:
					return Expression.Constant(CallSite<Functor22<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>.Create(binder));
				case 21:
					return Expression.Constant(CallSite<Functor23<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>.Create(binder));
				case 22:
					return Expression.Constant(CallSite<Functor24<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>.Create(binder));
				case 23:
					return Expression.Constant(CallSite<Functor25<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>.Create(binder));
				case 24:
					return Expression.Constant(CallSite<Functor26<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>.Create(binder));
				case 25:
					return Expression.Constant(CallSite<Functor27<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>.Create(binder));
				case 26:
					return Expression.Constant(CallSite<Functor28<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>.Create(binder));
				case 27:
					return Expression.Constant(CallSite<Functor29<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>.Create(binder));
				case 28:
					return Expression.Constant(CallSite<Functor30<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>.Create(binder));
				case 29:
					return Expression.Constant(CallSite<Functor31<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>.Create(binder));
				case 30:
					return Expression.Constant(CallSite<Functor32<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>.Create(binder));
				case 31:
					return Expression.Constant(CallSite<Functor33<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>.Create(binder));
				case 32:
					return Expression.Constant(CallSite<Functor34<Object, CallSite, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>>.Create(binder));
				default:
					return null;
			}

		}

		#endregion

	}

	
}
