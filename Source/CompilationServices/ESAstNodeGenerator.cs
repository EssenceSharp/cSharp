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
using Microsoft.Scripting;
#if CLR2
using Expression = Microsoft.Scripting.Ast.Expression;
#else
using Expression = System.Linq.Expressions.Expression;
#endif
using EssenceSharp.ParsingServices;
using EssenceSharp.Runtime;
using EssenceSharp.Runtime.Binding;
using EssenceSharp.Exceptions.System.PrimitiveFailures;
#endregion

namespace EssenceSharp.CompilationServices {

	public class ASTGenerator : BaseParseTreeNodeOperation<AbstractSyntaxTreeNode> {

		#region Static variables and methods

		protected static readonly Expression					falseConstant			= ExpressionTreeGuru.falseConstant;
		protected static readonly Expression					trueConstant			= ExpressionTreeGuru.trueConstant;
		protected static readonly String					functionPrimitiveName		= "primitive:domain:";
		protected static readonly String					convertToTypePrimitiveName	= "convertTo:";
		protected static readonly String					getFieldPrimitiveName		= "getField:";
		protected static readonly String					setFieldPrimitiveName		= "setField:";
		protected static readonly String					invokeFieldPrimitiveName	= "invokeField:";
		protected static readonly String					getPropertyPrimitiveName	= "getProperty:";
		protected static readonly String					setPropertyPrimitiveName	= "setProperty:";
		protected static readonly String					invokePropertyPrimitiveName	= "invokeProperty:";
		protected static readonly String					invokeMethodPrimitiveName	= "invokeMethod:";

		protected static readonly IDictionary<String, MethodOperationType>	primitiveType			= new Dictionary<String, MethodOperationType>();

		static ASTGenerator() {
			primitiveType[functionPrimitiveName]				= MethodOperationType.Function;
			primitiveType[convertToTypePrimitiveName]			= MethodOperationType.Convert;
			primitiveType[getFieldPrimitiveName]				= MethodOperationType.GetField;
			primitiveType[setFieldPrimitiveName]				= MethodOperationType.SetField;
			primitiveType[invokeFieldPrimitiveName]				= MethodOperationType.InvokeField;
			primitiveType[getPropertyPrimitiveName]				= MethodOperationType.GetProperty;
			primitiveType[setPropertyPrimitiveName]				= MethodOperationType.SetProperty;
			primitiveType[invokePropertyPrimitiveName]			= MethodOperationType.InvokeProperty;
			primitiveType[invokeMethodPrimitiveName]			= MethodOperationType.InvokeMethod;
		}

		public static MethodOperationType? primitiveTypeFor(String primitiveSelectorName) {
			MethodOperationType type; 
			if (!primitiveType.TryGetValue(primitiveSelectorName, out type)) return null;
			return type;
		}

		#endregion

		protected ESCompiler							compiler;
		protected CodeGenerationContext						context;
		protected ESKernel							kernel;
		protected SymbolRegistry						symbolRegistry;
		protected List<CodeGenerationContext>					contextStack			= new  List<CodeGenerationContext>();

		public ASTGenerator(ESCompiler esCompiler, CodeGenerationContext context) {
			this.compiler = esCompiler;
			this.context = context;
			bindToCompiler();
		}

		protected void bindToCompiler() {
			kernel = compiler.Kernel;
			symbolRegistry = kernel.SymbolRegistry;
		}

		public ESCompiler Compiler {
			get {return compiler;}
		}

		public ESKernel Kernel {
			get {return kernel;}
		}

		public CodeGenerationContext Context {
			get {return context;}
		}

		protected void pushContext() {
			contextStack.Add(context);
			context = compiler.newCodeGenerationContext();
		}

		protected void popContext() {
			context = contextStack[0];
			contextStack.RemoveAt(0);
		}

		#region Utilities

		public ESSymbol identifierSymbolFrom(String identifierString) {
			// Unchecked!!! Be VERY SURE that the syntax is that of an identifier, and contains no path element separators.
			return symbolRegistry.symbolFor(identifierString, SymbolType.Identifier, 0, null, 1);
		}

		public ESSymbol binaryMessageSelectorSymbolFrom(String binarySelectorString) {
			// Unchecked!!! Be VERY SURE that the syntax is that of a binary message selector.
			return symbolRegistry.symbolFor(binarySelectorString, SymbolType.BinaryMessageSelector, 1, null, 1);
		}

		public ESSymbol symbolFrom(SymbolLiteralToken symbolToken) {
			return symbolRegistry.symbolFor(symbolToken.StringValue, symbolToken.SymbolType, symbolToken.NumArgs, symbolToken.QualifiedNameSeparatorChar, symbolToken.PathElementCount);
		}

		public Object valueFromLiteralToken(LexicalLiteralToken token) {
			switch (token.OperandNodeType) {
				case ParseNodeType.Nil:
					return null;
				case ParseNodeType.True:
					return trueConstant;
				case ParseNodeType.False:
					return falseConstant;
				case ParseNodeType.Integer:
					return ((IntegerLiteralToken)token).SmallIntegerValue;
				case ParseNodeType.ScaledDecimal:
					throw new UnimplementedPrimitiveException(); 
				case ParseNodeType.SinglePrecision:
					return ((SinglePrecisionLiteralToken)token).FloatValue;
				case ParseNodeType.DoublePrecision:
					return ((DoublePrecisionLiteralToken)token).DoubleValue;
				case ParseNodeType.QuadPrecision:
					return ((QuadPrecisionLiteralToken)token).DecimalValue;
				case ParseNodeType.Char:
					return ((CharLiteralToken)token).CharValue;
				case ParseNodeType.String:
					var stString = kernel.newString(((StringLiteralToken)token).StringValue);
					stString.beImmutable();
					return stString;
				case ParseNodeType.Symbol:
					return symbolFrom((SymbolLiteralToken)token);
				case ParseNodeType.LiteralBindingReference:
					LiteralBindingReferenceToken lbrToken = (LiteralBindingReferenceToken)token;
					var pathName = kernel.pathnameFromString(lbrToken.StringValue);
					pathName.beImmutable();
					return pathName;
				case ParseNodeType.ByteArray:
					var stByteArray = kernel.newByteArray(((ByteArrayLiteralToken)token).ByteArray);
					stByteArray.beImmutable();
					return stByteArray;
				case ParseNodeType.Array:
					ArrayLiteralToken arrayToken = (ArrayLiteralToken)token;
					var elements = new List<Object>();
					arrayToken.elementsDo(element => elements.Add(valueFromLiteralToken(element)));
					var stArray = kernel.newArray(elements.ToArray());
					stArray.beImmutable();
					return stArray;
				default:
					return null;
			}
		}

		protected static List<String> argNamesFromPrimitiveSpecArgumentList(List<KeywordMessageArgument> arguments, System.Action<String, SourceSpan> error) {
 			var argNames = new List<String>();
			foreach (var kma in arguments) argNames.Add(kma.asPathString('.'));
			return argNames;
		}	
	
		#endregion

		#region Parse Tree Traversal And AST Node Generation

		#region Terminal and/or Literal Nodes

		public override AbstractSyntaxTreeNode applyToNilToken(NilToken operand) {
			return Context.newReducedNode(ExpressionTreeGuru.nilConstant);
		}

		public override AbstractSyntaxTreeNode applyToFalseToken(FalseToken operand) {
			return Context.newReducedNode(falseConstant);
		}

		public override AbstractSyntaxTreeNode applyToTrueToken(TrueToken operand) {
			return Context.newReducedNode(trueConstant);
		}

		public override AbstractSyntaxTreeNode applyToCharLiteralToken(CharLiteralToken operand) {
			return Context.newReducedNode((Object)operand.CharValue);
		}

		public override AbstractSyntaxTreeNode applyToIntegerLiteralToken(IntegerLiteralToken operand) {
			return Context.newReducedNode((Object)operand.SmallIntegerValue);
		}

		public override AbstractSyntaxTreeNode applyToSinglePrecisionLiteralToken(SinglePrecisionLiteralToken operand) {
			return Context.newReducedNode((Object)operand.FloatValue);
		}

		public override AbstractSyntaxTreeNode applyToDoublePrecisionLiteralToken(DoublePrecisionLiteralToken operand) {
			return Context.newReducedNode((Object)operand.DoubleValue);
		}

		public override AbstractSyntaxTreeNode applyToQuadPrecisionLiteralToken(QuadPrecisionLiteralToken operand) {
			return Context.newReducedNode((Object)operand.DecimalValue);
		}

		public override AbstractSyntaxTreeNode applyToScaledDecimalLiteralToken(ScaledDecimalLiteralToken operand) {
			throw new UnimplementedPrimitiveException(); 
		}

		public override AbstractSyntaxTreeNode applyToStringLiteralToken(StringLiteralToken operand) {
			var stString = kernel.newString(operand.StringValue);
			stString.beImmutable();
			return Context.newReducedNode(stString);
		}

		public override AbstractSyntaxTreeNode applyToSymbolLiteralToken(SymbolLiteralToken operand) {
			return Context.newReducedNode(symbolFrom(operand));
		}

		public override AbstractSyntaxTreeNode applyToLiteralBindingReferenceToken(LiteralBindingReferenceToken operand) {
			var pathName = kernel.pathnameFromString(operand.StringValue);
			pathName.beImmutable();
			return Context.newReducedNode(pathName);
		}

		public override AbstractSyntaxTreeNode applyToByteArrayLiteralToken(ByteArrayLiteralToken operand) {
			var stByteArray = kernel.newByteArray(operand.ByteArray);
			stByteArray.beImmutable();
			return Context.newReducedNode(stByteArray);
		}

		public override AbstractSyntaxTreeNode applyToArrayLiteralToken(ArrayLiteralToken operand) {
			return Context.newReducedNode(valueFromLiteralToken(operand));
		}

		public override AbstractSyntaxTreeNode applyToSelfToken(SelfToken operand) {
			return Context.newSelfNode();
		}

		public override AbstractSyntaxTreeNode applyToSuperToken(SuperToken operand) {
			return Context.newSuperNode();
		}

		public override AbstractSyntaxTreeNode applyToThisContextToken(ThisContextToken operand) {
			return Context.newThisContextNode();
		}

		#endregion

		#region Non-Terminal Nodes

		public override AbstractSyntaxTreeNode applyToConstantReference(ConstantReference operand) {
			return operand.Constant.valueBy(this);
		}

		public override AbstractSyntaxTreeNode applyToPseudoVariableReference(PseudoVariableReference operand) {
			return operand.PseudoVariable.valueBy(this);
		}

		public override AbstractSyntaxTreeNode applyToVariableReference(VariableReference operand) {
			return Context.newVariableReferenceNode(Context.Scope.newOccurrenceOf(symbolRegistry.symbolFor(operand.BindingName)));
		}

		public override AbstractSyntaxTreeNode applyToLexicalLiteralValue(LexicalLiteralValue operand) {
			return operand.LiteralToken.valueBy(this);
		}

		public override AbstractSyntaxTreeNode applyToDynamicArrayLiteral(DynamicArrayLiteral operand) {
			var elements = new List<ExpressionNode>();
			operand.elementsDo(element => elements.Add((ExpressionNode)element.valueBy(this)));
			return Context.newDynamicArrayLiteralNode(elements);
		}

		public override AbstractSyntaxTreeNode applyToDictionaryLiteral(DictionaryLiteral operand) {
			var elements = new List<ExpressionNode>();
			operand.elementsDo(element => elements.Add((ExpressionNode)element.valueBy(this)));
			return Context.newDictionaryLiteralNode(elements);
		}

		public override AbstractSyntaxTreeNode applyToBinaryMessageOperand(BinaryMessageOperand operand) {
			var value = (OperandNode)operand.Operand.valueBy(this);
			if (operand.HasMessageChain) {
				operand.MessageChain.messagesDo(message => value = Context.newMessageSendNode(value, (MessageNode)message.valueBy(this)));
			}
			return Context.newExpressionNode(value);
		}

		public override AbstractSyntaxTreeNode applyToUnaryMessage(UnaryMessage operand) {
			var selector = symbolFrom(operand.SelectorSymbol);
			return Context.newUnaryMessageNode(selector);
		}

		public override AbstractSyntaxTreeNode applyToBinaryMessage(BinaryMessage operand) {
			return Context.newBinaryMessageNode(symbolFrom(operand.SelectorSymbol), (OperandNode)operand.Operand.valueBy(this));
		}

		public override AbstractSyntaxTreeNode applyToKeywordMessage(KeywordMessage operand) {
			var arguments = new List<OperandNode>();
			operand.messageArgumentsDo(argument => arguments.Add((OperandNode)argument.valueBy(this)));
			return Context.newKeywordMessageNode(symbolFrom(operand.SelectorSymbol), arguments);
		}

		public override AbstractSyntaxTreeNode applyToKeywordMessageArgument(KeywordMessageArgument operand) {
			var value = (OperandNode)operand.Operand.valueBy(this);
			if (operand.HasMessageChain) {
				operand.MessageChain.messagesDo(message => value = Context.newMessageSendNode(value, (MessageNode)message.valueBy(this)));
			}
			return Context.newExpressionNode(value);
		}

		public override AbstractSyntaxTreeNode applyToExpressionInLiteral(ExpressionInLiteral operand) {
			return operand.Expression.valueBy(this);
		}

		public override AbstractSyntaxTreeNode applyToNestedExpression(NestedExpression operand) {
			return operand.Statement.valueBy(this);
		}

		public override AbstractSyntaxTreeNode applyToStatement(Statement operand) {
			if (operand.IsAssignmentStatement) {
				var assignedVariables = new List<NamedValueOccurrence>(); 
				operand.assignmentPrefixesDo(
					delegate (AssignmentPrefix assignmentPrefix) {
						var variableOccurrence = Context.Scope.newOccurrenceOf(identifierSymbolFrom(assignmentPrefix.VariableName));
						if (!variableOccurrence.IsAssignable) {
							Context.markAsUncompilable();
							compiler.handleIllegalAssignment(variableOccurrence, assignmentPrefix.Span);
						}
						assignedVariables.Add(variableOccurrence);
					});
				return Context.newAssignmentStatementNode(assignedVariables.ToArray(), (OperandNode)operand.Expression.valueBy(this));
			} else {
				return operand.Expression.valueBy(this);
			}
		} // 

		public override AbstractSyntaxTreeNode applyToFinalStatement(FinalStatement operand) {
			StatementNode statementNode = (StatementNode)applyToStatement(operand);
			return operand.IsReturnStatement ? (AbstractSyntaxTreeNode)Context.newReturnStatementNode(statementNode) : (AbstractSyntaxTreeNode)statementNode;
		}

		// Supported Non-Terminal Node Entry Points:
		//
		// Note: Any literal token that represents a constant value (object) at runtime AND which is created as an STObject during compilation 
		// is also a valid entry point. That excludes a) anything which is purely syntactical, b) anything which would become generated CLR code, 
		// c) a dynamic array literal, d) a dictionary literal, d) a block literal and e) a method literal.

		public override AbstractSyntaxTreeNode applyToExpression(ParsingServices.Expression operand) {
			Context.setRootScopeIfNone();
			var value = (OperandNode)operand.Operand.valueBy(this);
			var prevReceiver = value;
			MessageNode finalMessage = null;
			operand.messagesDo(
				(Message message) => {
					prevReceiver = value; // The receiver of a cascaded message must be the value resulting from sending the penultimate (non-cascaded) message.
					finalMessage = (MessageNode)message.valueBy(this);
					value = Context.newMessageSendNode(value, finalMessage);
				});
			if (operand.CascadedMessageCount < 1) return Context.newExpressionNode(value);
			var cascadedMessages = new List<MessageNode>();
			cascadedMessages.Add(finalMessage);
			operand.cascadedMessageChainsDo(messageChain => messageChain.messagesDo(messageParseNode => cascadedMessages.Add((MessageNode)messageParseNode.valueBy(this))));
			return Context.newCascadedMessageExpressionNode(prevReceiver, cascadedMessages);
		}

		public override AbstractSyntaxTreeNode applyToExecutableCode(ExecutableCode operand) {
			Context.setRootScopeIfNone();
			var executableCode = Context.newExecutableCodeNode();
			operand.variablesDo(
				variable => Context.Scope.declareLocalVariable(variable.Name, 
					(existingDeclaration) => {
						Context.markAsUncompilable();
						compiler.handleVariableNameCollision(existingDeclaration.NameString, variable.Span);
						return existingDeclaration.IsVariable ? (StackResidentVariableDeclaration)existingDeclaration : Context.Scope.declareLocalVariable("variable$" + variable.Name, null);}));
			operand.statementsDo(statement => executableCode.add((StatementNode)statement.valueBy(this)));
			return executableCode;
		}

		public override AbstractSyntaxTreeNode applyToBlockDeclaration(BlockDeclaration operand) {
			Context.pushScope();
			var blockDeclaration = Context.newBlockDeclarationNode();
			operand.parametersDo(
				parameter => Context.Scope.declareParameter(parameter.Name, 
					(existingDeclaration) => {
						Context.markAsUncompilable();
						compiler.handleParameterNameCollision(existingDeclaration.NameString, parameter.Span);
						return existingDeclaration.IsParameter ? (ParameterDeclaration)existingDeclaration : Context.Scope.declareParameter("parameter$" + parameter.Name, null);}));
			if (Context.Scope.IsRoot) {
				Context.rootParametersDo(
					parameter => Context.Scope.declareParameter(parameter, 
						(NamedValueDeclaration existingDeclaration) => {
							Context.markAsUncompilable();
							compiler.handleParameterNameCollision(existingDeclaration.NameString, SourceSpan.None);
							return existingDeclaration.IsParameter ? (ParameterDeclaration)existingDeclaration : Context.Scope.declareParameter("rootParameter$" + parameter.Name, null);}));
			}
			if (operand.HasBody) {
				blockDeclaration.Body = (ExecutableCodeNode)operand.Body.valueBy(this);
			}
			Context.popScope();
			return blockDeclaration;
		}

		public override AbstractSyntaxTreeNode applyToBlockLiteral(BlockLiteral operand) {
			return Context.newBlockLiteralNode((BlockDeclarationNode)operand.Declaration.valueBy(this));
		}

		public override AbstractSyntaxTreeNode applyToMethodLiteral(MethodLiteral operand) {
			pushContext();
			var methodNodeDeclaration = (MethodDeclarationNode)operand.MethodDeclaration.valueBy(this);
			popContext();
			return Context.newMethodLiteralNode(methodNodeDeclaration);
		}

		public override AbstractSyntaxTreeNode applyToMethodDeclaration(MethodDeclaration operand) {
			Context.setRootScope();
			Context.MethodSelector = symbolFrom(operand.SelectorSymbol);
			var method = Context.newMethodDeclarationNode(Context.MethodSelector);
			method.Body = generateExecutableCodeNodeForMethodDeclaration(operand);
			return method;
		}

		protected ExecutableCodeNode generateExecutableCodeNodeForMethodDeclaration(MethodDeclaration operand) {
			operand.parametersDo(
				parameter => Context.Scope.declareParameter(parameter.Name, 
						(existingDeclaration) => {
							Context.markAsUncompilable();
							compiler.handleParameterNameCollision(existingDeclaration.NameString, parameter.Span);
							return existingDeclaration.IsParameter ? (ParameterDeclaration)existingDeclaration : Context.Scope.declareParameter("parameter$" + parameter.Name, null);}));
			return (ExecutableCodeNode)operand.Body.valueBy(this);
		}

		public override AbstractSyntaxTreeNode applyToPrimitiveMethodDeclaration(PrimitiveMethodDeclaration operand) {
			var methodSelector = symbolFrom(operand.SelectorSymbol);
			Context.setRootScope();
			Context.MethodSelector = methodSelector;
			PrimitiveMethodDeclarationNode method = null;
			Delegate primitiveFunction = null;
			var primitiveSpec = operand.PrimitiveSpecification;
			var primitiveSpecSelector = symbolFrom(primitiveSpec.SelectorSymbol);
			var primType = primitiveTypeFor(primitiveSpecSelector.PrimitiveValue);
			if (primType == null) {
				Context.markAsUncompilable();
				compiler.handlePrimitiveSpeficationError(
					"Unknown/unsupported primitive specification format: " + primitiveSpecSelector.ToString(), 
					primitiveSpec.Span);
				method = Context.newPrimitiveFunctionMethodDeclarationNode(Context.MethodSelector, primitiveFunction);
			} else {
				MethodOperationType opType = (MethodOperationType)primType;
				long requiredArity = -1;
				switch (opType) {
					case MethodOperationType.Function:
						primitiveFunction = primitiveFunctionFor(primitiveSpec.Arguments);
						method = Context.newPrimitiveFunctionMethodDeclarationNode(methodSelector, primitiveFunction);
						if (primitiveFunction == null) {
							requiredArity = methodSelector.NumArgs;
						} else {
							var methodInfo = primitiveFunction.Method;		// Primitive functions can't be private or otherwise inaccessible.
							requiredArity = methodInfo.GetParameters().Length - 1;	// The initial parameter of a primitive function is always the receiver of the message.
						}
						break;
					case MethodOperationType.Convert:
					case MethodOperationType.GetField:
					case MethodOperationType.GetProperty:
						requiredArity = 0;
						break;
					case MethodOperationType.SetField:
					case MethodOperationType.SetProperty:
						requiredArity = 1;
						break;
					case MethodOperationType.InvokeField:
					case MethodOperationType.InvokeProperty:
					case MethodOperationType.InvokeMethod:
						requiredArity = methodSelector.NumArgs; // We can't know the actual required arity until run time.
						break;
					default:
						Context.markAsUncompilable();
						compiler.handlePrimitiveSpeficationError(
							"Unknown/unsupported primitive specification format: " + primitiveSpecSelector.ToString(), 
							primitiveSpec.Span);
						method = Context.newPrimitiveFunctionMethodDeclarationNode(methodSelector, primitiveFunction);
						break;
				}
				if (methodSelector.NumArgs != requiredArity) {
					Context.markAsUncompilable();
					compiler.handlePrimitiveSpeficationError(
						"Method parameter arity mismatch: Method selector has " + methodSelector.NumArgs + " arguments, but the specified primitive function/operation requires " + requiredArity + " arguments.", 
						primitiveSpec.Span);
				}
				if (method == null) {
					String operandName = null;
					primitiveSpec.messageArgumentsDo(argument => operandName = argument.asPathString('.'));
					if (opType == MethodOperationType.InvokeMethod && operandName == "new") opType = MethodOperationType.CreateInstance;
					var operation = new InlineOperation(opType, operandName);
					method = Context.newInlineOperationMethodDeclarationNode(methodSelector, operation);
				}
			}
			method.Body = generateExecutableCodeNodeForMethodDeclaration(operand);
			return method;
		}

		protected Delegate primitiveFunctionFor(List<KeywordMessageArgument> arguments) {
			ParseTreeNode node;
			var argNames = argNamesFromPrimitiveSpecArgumentList(
						arguments, 
						(errorMessage, span) 
							=> compiler.handlePrimitiveSpeficationError(errorMessage, span));
			ESSymbol primName = symbolRegistry.symbolFor(argNames[0]);
			String domainName = argNames[1];
			PrimitiveDomainType domain = (PrimitiveDomainType)ushort.MaxValue; // On account of typical statically-typed compiler stupidity.
			try {
				domain = (PrimitiveDomainType)Enum.Parse(typeof(PrimitiveDomainType), domainName);
			} catch {
				Context.markAsUncompilable();
				node = arguments[1];
				compiler.handlePrimitiveSpeficationError(
					"Unknown primitive domain <" + domainName + ">.", 
					node.Span);
			}
			Delegate function;
    			if (!kernel.getPrimitiveFunction(domain, primName, out function)) {
				Context.markAsUncompilable();
				node = arguments[0];
				compiler.handlePrimitiveSpeficationError(
					"No primitive named <" + primName.PrimitiveValue + "> is defined in the primitive domain <" + domainName + ">.", 
					node.Span);
			}
			return function;
		}

		#endregion

		#endregion

	}

}
