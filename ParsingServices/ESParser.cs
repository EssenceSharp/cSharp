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
using Microsoft.Scripting;
#endregion

namespace EssenceSharp.ParsingServices {

	public class ESParser {

		#region Static variables and functions

		public static readonly SelfToken						selfToken				= new SelfToken(0, 0, 0, 0, 0);

		public static void reportError(String description, SourceSpan span, int errorCode, Severity severity) {
			Console.WriteLine("");
			switch (severity) {
				case Severity.Ignore:
					Console.Write("Note: [");
					break;
				case Severity.Warning:
					Console.Write("Warning: [");
					break;
				case Severity.Error:
					Console.Write("Error: [");
					break;
				case Severity.FatalError:
					Console.Write("Fatal error: [");
					break;
			}
			Console.Write(span.ToString());
			Console.Write("] ");
			Console.Write(description);
			if (errorCode > 0) {
				Console.Write(" error code = " + errorCode);
			}
			Console.WriteLine();
		}

		#endregion

		#region Instance Variables

		private int									errorCount				= 0;
		private System.Action<String, SourceSpan, int, Severity> 			reportErrorFunctor			= new System.Action<String, SourceSpan, int, Severity>(reportError);
		private Functor3<ParseTreeNode, ParseNodeType, ParseNodeType[], ParseTreeNode> 	handleUnexpectedTokenFunctor 		= null;
		
		private ESLexicalAnalyzer 							scanner 				= null;
		private LexicalToken 								nextToken 				= null;
		private bool 									hasNextToken 				= false;
		
		private ParsingContext 								context 				= ParsingContext.General;
		private List<ParsingContext>							contextStack				= null; 
		
		private bool 									supportsMethodLiteralSyntax 		= true;
		
		#endregion
		
		#region Constructors & Initialization

		public ESParser(TextReader sourceStream, SyntaxProfile syntaxProfile) 
			: this(new ESLexicalAnalyzer(sourceStream, syntaxProfile)) {
		}

		public ESParser (TextReader sourceStream, ParsingOptions parsingOptions)
			: this (new ESLexicalAnalyzer(sourceStream, parsingOptions)) {
		}
		
		public ESParser(ESLexicalAnalyzer scanner) {
			Scanner = scanner;
		}
	
		protected virtual void bindToScanner() {
			if (scanner == null) return;
			supportsMethodLiteralSyntax = scanner.SupportsMethodHeaderBeginToken;
		}
		
		#endregion
		
		#region Token IO 
		
		protected bool advanceToken() {
			nextToken = scanner.nextToken(Context);
			hasNextToken = nextToken != null;
			return hasNextToken;
		}
		
		protected bool peek(out LexicalToken token) {
			if (hasNextToken || advanceToken()) {
				token = nextToken;
				return true;
			}
				
			token = null;
			return false;
		}
		
		protected ParseNodeType peekLexicalType() {
			if (hasNextToken || advanceToken()) {
				return nextToken.ParseNodeType;
			} else {
				return ParseNodeType.EndOfSource;
			}
		}
		
		protected bool nextMatches(ParseNodeType tokenType, out LexicalToken token) {
			if (hasNextToken || advanceToken()) {
				token = nextToken;
				if (nextToken.ParseNodeType == tokenType) {
					// advanceToken();
					hasNextToken = false;
					return true;
				} else {
					return false;
				}
			}
			
			token = null;
			return false;
		}
		
		protected bool satisfies(Predicate<ParseNodeType> tokenTypeTest, out LexicalToken token) {
			if (hasNextToken || advanceToken()) {
				if (tokenTypeTest(nextToken.ParseNodeType)) {
					token = nextToken;
					// advanceToken();
					hasNextToken = false;
					return true;
				}
			}
			
			token = null;
			return false;
		}
		
		protected bool next(out LexicalToken token) {
			if (hasNextToken || advanceToken()) {
				token = nextToken;
				// advanceToken();
				hasNextToken = false;
				return true;
			}
			
			token = null;
			return false;
		}
		
		#endregion

		#region Error Handling 
		
		public virtual ParseTreeNode defaultHandleUnexpectedToken(ParseNodeType parentNodeType, ParseNodeType[] expectedNodeTypeSet, ParseTreeNode unexpectedNode) {
			StringBuilder sb = new StringBuilder();
			sb.Append("Syntax error in ");
			sb.Append(parentNodeType);
			sb.Append(": Expecting <");
			int count = 0;
			int lastIndex = expectedNodeTypeSet.Length - 1;
			foreach (ParseNodeType nodeType in expectedNodeTypeSet) {
				sb.Append(nodeType);
				count++;
				if (count < lastIndex) {
					sb.Append(", ");
				} else if (count == lastIndex) {
					sb.Append(" or ");
				}
			}
			sb.Append("> but encountered: ");
			sb.Append(unexpectedNode == null ? "<nothing>" : unexpectedNode.ToString());
			SourceSpan span;
			if (unexpectedNode == null) {
				span = new SourceSpan(
						new SourceLocation(scanner.OccurrenceIndex, (int)scanner.LineNumber, (int)scanner.ColumnNumber), 
						new SourceLocation(scanner.OccurrenceIndex, (int)scanner.LineNumber, (int)scanner.ColumnNumber)); 
			} else {
				span = unexpectedNode.Span;
			}
			ReportError(sb.ToString(), span, 0, Severity.Error); // String, SourceSpan, int errorCode, Severity
			return unexpectedNode == null ? null : (unexpectedNode.IsEndOfSource ? unexpectedNode : null);
		}

		protected ParseTreeNode handledUnexpectedToken(ParseNodeType parentNodeType, ParseNodeType[] expectedNodeTypeSet, ParseTreeNode unexpectedNode) {
			errorCount++;
			return handleUnexpectedTokenFunctor == null ?
				defaultHandleUnexpectedToken(parentNodeType, expectedNodeTypeSet, unexpectedNode) :
				handleUnexpectedTokenFunctor(parentNodeType, expectedNodeTypeSet, unexpectedNode);
		}

		#endregion
				
		#region Public Protocol
		
		#region Properties 
		
		public ESLexicalAnalyzer Scanner {
			get {return scanner;}
			set {	if (scanner == value) return;
				scanner = value;
				bindToScanner();}
		}
		
		public int ErrorCount {
			get {return errorCount;}
		}
		
		public System.Action<String, SourceSpan, int, Severity> ReportError {
			get {return reportErrorFunctor;}
			set {	reportErrorFunctor = value;
				bindToScanner();}
		}	

		public Functor3<ParseTreeNode, ParseNodeType, ParseNodeType[], ParseTreeNode> HandledUnexpectedToken {
			get {return handleUnexpectedTokenFunctor;}
			set {handleUnexpectedTokenFunctor = value == null ? defaultHandleUnexpectedToken : value;}
		}
		
		public bool SupportsMethodLiteralSyntax {
			get {return supportsMethodLiteralSyntax;}
		}
		
		protected ParsingContext Context {
			get {return context;}
			// set {context = value;}
		}
		
		#endregion

		public ParseTreeNode parseTree() {
			// BlockLiteral

			errorCount = 0;
			var block = parseBlockDeclaration();

			if (peekLexicalType() != ParseNodeType.EndOfSource) {
				return handledUnexpectedToken(
						ParseNodeType.BlockDeclaration, 
						new ParseNodeType[]{ParseNodeType.EndOfSource}, 
						nextToken);
			}

			return block.IsBlockDeclaration ?
				makeVirtualOuterBlockLiteral((BlockDeclaration)block) :
				block;

		}

		public ParseTreeNode selfExpressionParseTree(List<String> parameterNames) {
			// BlockLiteral, optionally with named parameters

			errorCount = 0;
			var expression = parseExpression(selfToken);

			switch (peekLexicalType()) {
				case ParseNodeType.EndOfSource:
					return makeVirtualOuterBlockLiteral(parameterNames, (Expression)expression);
				case ParseNodeType.StatementEnd:
					advanceToken();
					if (peekLexicalType() == ParseNodeType.EndOfSource) {
						return makeVirtualOuterBlockLiteral(parameterNames, (Expression)expression);
					} else {
						return handledUnexpectedToken(
								ParseNodeType.Expression, 
								new ParseNodeType[]{ParseNodeType.EndOfSource}, 
								nextToken);
					}
				default:
					return handledUnexpectedToken(
							ParseNodeType.Expression, 
							new ParseNodeType[]{ParseNodeType.StatementEnd, ParseNodeType.EndOfSource}, 
							nextToken);
			}

		}

		public ParseTreeNode methodParseTree() {
			// MethodDeclaration

			errorCount = 0;
			var node = parseMethodDeclaration();

			var nodeType = peekLexicalType();
			if (nodeType != ParseNodeType.EndOfSource || nodeType != ParseNodeType.MethodHeaderBegin) {
				return handledUnexpectedToken(
						ParseNodeType.MethodDeclaration, 
						new ParseNodeType[]{ParseNodeType.MethodHeaderBegin, ParseNodeType.EndOfSource}, 
						nextToken);
			}

			return node;

		}
	
		#endregion

		#region Internal Operations

		protected void pushContext(ParsingContext newContext) {
			if (contextStack == null) contextStack = new List<ParsingContext>();
			contextStack.Insert(0, context);
			context = newContext;
		}

		protected void popContext() {
			if (contextStack == null || contextStack.Count < 1) return;
			context = contextStack[0];
			contextStack.RemoveAt(0);
		}

		#region Parse Node Creation

		protected int nextOccurrenceIndex() {
			return scanner.nextOccurrenceIndex();
		}
		
		protected void resetOccurrenceIndex() {
			scanner.resetOccurrenceIndex();
		}

		protected virtual BlockLiteral makeVirtualOuterBlockLiteral(List<String> parameterNames, Expression expression) {
			scanner.resetStreamPosition();
			BlockParameterDeclarationList paramDeclarations = null;
			VerticalBarToken listEndToken = null;
			if (parameterNames != null) {
				var blockParameters = new List<BlockParameterToken>();
				foreach (var name in parameterNames) blockParameters.Add(scanner.newBlockParameterToken(name, 0, 0));
				listEndToken = scanner.newVerticalBarToken(0, 0);
				paramDeclarations = newBlockParameterDeclarationList(blockParameters, listEndToken);
			}
			var statements = new List<Statement>();
			statements.Add(newStatement(null, (Expression)expression, null));
			var body = newExecutableCode(null, statements, null);
			var blockDeclaration = newBlockDeclaration(paramDeclarations, body);
			return makeVirtualOuterBlockLiteral(blockDeclaration);
		}

		protected virtual BlockLiteral makeVirtualOuterBlockLiteral(BlockDeclaration blockDeclaration) {
			scanner.resetStreamPosition();
			var beginToken = scanner.newBlockBeginToken(0, 0);
			var endToken = scanner.newBlockEndToken(0, 0);
			return newBlockLiteral(beginToken, blockDeclaration, endToken);
		}

		protected virtual MethodDeclaration newMethodDeclaration(MethodHeader methodHeader, ExecutableCode executableCode) {
			return new MethodDeclaration(methodHeader, executableCode, nextOccurrenceIndex());
		}

		protected virtual PrimitiveMethodDeclaration newPrimitiveMethodDeclaration(
			MethodHeader methodHeader, 
			BinaryMessageSelectorToken leftEnclosingToken,
			KeywordMessage primitiveOrExternalCallSpec,
			BinaryMessageSelectorToken rightEnclosingToken,
			ExecutableCode executableCode) {

			return new PrimitiveMethodDeclaration(
							methodHeader, 
							leftEnclosingToken, 
							primitiveOrExternalCallSpec, 
							rightEnclosingToken, 
							executableCode,
							nextOccurrenceIndex());
		}

		protected virtual UnaryMethodHeader newUnaryMethodHeader(DeclarableIdentifierToken identifierToken) {
			return new UnaryMethodHeader(identifierToken, nextOccurrenceIndex());
		}

		protected virtual BinaryMethodHeader newBinaryMethodHeader(BinaryMessageSelectorToken selectorToken, DeclarableIdentifierToken parameterToken) {
			return new BinaryMethodHeader(selectorToken, parameterToken, nextOccurrenceIndex());
		}

		protected virtual KeywordMethodHeader newKeywordMethodHeader(List<KeywordMethodHeaderSegment> segments) {
			return new KeywordMethodHeader(segments, nextOccurrenceIndex());
		}

		protected virtual KeywordMethodHeaderSegment newKeywordMethodHeaderSegment(KeywordToken keywordToken, DeclarableIdentifierToken parameterToken) {
			return new KeywordMethodHeaderSegment(keywordToken, parameterToken, nextOccurrenceIndex());
		}

		protected virtual BlockDeclaration newBlockDeclaration(BlockParameterDeclarationList parameterList, ExecutableCode body) {
			return new BlockDeclaration(parameterList, body, nextOccurrenceIndex());
		}

		protected virtual ExecutableCode newExecutableCode(LocalVariableDeclarationList localVariables, List<Statement> statements, FinalStatement finalStatement) {
			return new ExecutableCode(localVariables, statements, finalStatement, nextOccurrenceIndex());
		}

		protected virtual LocalVariableDeclarationList newLocalVariableDeclarationList(VerticalBarToken listBegin, List<DeclarableIdentifierToken> variables, VerticalBarToken listEnd) {
			return new LocalVariableDeclarationList(listBegin, variables, listEnd, nextOccurrenceIndex());
		}

		protected virtual BlockParameterDeclarationList newBlockParameterDeclarationList(List<BlockParameterToken> parameters, VerticalBarToken listEnd) {
			return new BlockParameterDeclarationList(parameters, listEnd, nextOccurrenceIndex());
		}

		protected virtual Statement newStatement(List<AssignmentPrefix> assignmentPrefixes, Expression expression, StatementEndToken statementEndOp) {
			return new Statement(assignmentPrefixes, expression, statementEndOp, nextOccurrenceIndex());
		}

		protected virtual FinalStatement newFinalStatement(List<AssignmentPrefix> assignmentPrefixes, Expression expression, StatementEndToken statementEndOp) {
			return new FinalStatement(assignmentPrefixes, expression, statementEndOp, nextOccurrenceIndex());
		}

		protected virtual FinalStatement newFinalStatement(MethodReturnPrefix methodReturnPrefix, List<AssignmentPrefix> assignmentPrefixes, Expression expression, StatementEndToken statementEndOp) {
			return new FinalStatement(methodReturnPrefix, assignmentPrefixes, expression, statementEndOp, nextOccurrenceIndex());
		}

		protected virtual MethodReturnPrefix newMethodReturnPrefix(ReturnOpToken returnOp) {
			return new MethodReturnPrefix(returnOp, nextOccurrenceIndex());
		}

		protected virtual AssignmentPrefix newAssignmentPrefix(VariableAssignment assignmentTarget, AssignOpToken assignmentOp) {
			return new AssignmentPrefix(assignmentTarget, assignmentOp, nextOccurrenceIndex());
		}

		protected virtual Expression newExpression(Operand operand, MessageChain messageChain, List<CascadedMessageChain> cascadedMessages) {
			return new Expression(operand, messageChain, cascadedMessages, nextOccurrenceIndex());
		}

		protected virtual CascadedMessageChain newCascadedMessageChain(MessageCascadeOpToken messageCascadeOp, MessageChain messageChain) {
			return new CascadedMessageChain(messageCascadeOp, messageChain, nextOccurrenceIndex());
		}

		protected virtual InitiallyUnaryMessageChain newInitiallyUnaryMessageChain(UnaryOnlyMessageChain unaryMessageChain) {
			return new InitiallyUnaryMessageChain(unaryMessageChain, nextOccurrenceIndex());
		}

		protected virtual InitiallyUnaryMessageChain newInitiallyUnaryMessageChain(UnaryOnlyMessageChain unaryMessageChain, KeywordMessage keywordMessage) {
			return new InitiallyUnaryMessageChain(unaryMessageChain, keywordMessage, nextOccurrenceIndex());
		}

		protected virtual InitiallyUnaryMessageChain newInitiallyUnaryMessageChain(UnaryOnlyMessageChain unaryMessageChain, BinaryOnlyMessageChain binaryMessageChain) {
			return new InitiallyUnaryMessageChain(unaryMessageChain, binaryMessageChain, nextOccurrenceIndex());
		}

		protected virtual InitiallyUnaryMessageChain newInitiallyUnaryMessageChain(UnaryOnlyMessageChain unaryMessageChain, BinaryOnlyMessageChain binaryMessageChain, KeywordMessage keywordMessage) {
			return new InitiallyUnaryMessageChain(unaryMessageChain, binaryMessageChain, keywordMessage, nextOccurrenceIndex());
		}

		protected virtual InitiallyBinaryMessageChain newInitiallyBinaryMessageChain(BinaryOnlyMessageChain binaryMessageChain) {
			return new InitiallyBinaryMessageChain(binaryMessageChain, nextOccurrenceIndex());
		}

		protected virtual InitiallyBinaryMessageChain newInitiallyBinaryMessageChain(BinaryOnlyMessageChain binaryMessageChain, KeywordMessage keywordMessage) {
			return new InitiallyBinaryMessageChain(binaryMessageChain, keywordMessage, nextOccurrenceIndex());
		}

		protected virtual UnaryOnlyMessageChain newUnaryOnlyMessageChain(List<UnaryMessage> messages) {
			return new UnaryOnlyMessageChain(messages, nextOccurrenceIndex());
		}

		protected virtual BinaryOnlyMessageChain newBinaryOnlyMessageChain(List<BinaryMessage> messages) {
			return new BinaryOnlyMessageChain(messages, nextOccurrenceIndex());
		}

		protected virtual KeywordMessageChain newKeywordMessageChain(KeywordMessage keywordMessage) {
			return new KeywordMessageChain(keywordMessage, nextOccurrenceIndex());
		}

		protected virtual UnaryMessage newUnaryMessage(IdentifierToken selector) {
			return new UnaryMessage(selector, nextOccurrenceIndex());
		}

		protected virtual BinaryMessage newBinaryMessage(BinaryMessageSelectorToken selector, BinaryMessageOperand operand) {
			return new BinaryMessage(selector, operand, nextOccurrenceIndex());
		}

		protected virtual KeywordMessage newKeywordMessage(List<KeywordMessageSegment> segments) {
			return new KeywordMessage(segments, nextOccurrenceIndex());
		}

		protected virtual KeywordMessageSegment newKeywordMessageSegment(KeywordToken selector, KeywordMessageArgument argument) {
			return new KeywordMessageSegment(selector, argument, nextOccurrenceIndex());
		}

		protected virtual KeywordMessageArgument newKeywordMessageArgument(BinaryMessageOperand operand) {
			return new KeywordMessageArgument(operand, nextOccurrenceIndex());
		}

		protected virtual KeywordMessageArgument newKeywordMessageArgument(BinaryMessageOperand operand, BinaryOnlyMessageChain messageChain) {
			return new KeywordMessageArgument(operand, messageChain, nextOccurrenceIndex());
		}

		protected virtual BinaryMessageOperand newBinaryMessageOperand(Operand operand) {
			return new BinaryMessageOperand(operand, nextOccurrenceIndex());
		}

		protected virtual BinaryMessageOperand newBinaryMessageOperand(Operand operand, UnaryOnlyMessageChain messageChain) {
			return new BinaryMessageOperand(operand, messageChain, nextOccurrenceIndex());
		}

		protected virtual NestedExpression newNestedExpression(ExpressionBeginToken expressionBegin, Statement statement, ExpressionEndToken expressionEnd) {
			return new NestedExpression(expressionBegin, statement, expressionEnd, nextOccurrenceIndex());
		}

		protected virtual DynamicArrayLiteral newDynamicArrayLiteral(DynamicArrayBeginToken arrayBegin, List<ExpressionInLiteral> elements, CurlyBraceStructureEndToken arrayEnd) {
			return new DynamicArrayLiteral(arrayBegin, elements, arrayEnd, nextOccurrenceIndex());
		}

		protected virtual DictionaryLiteral newDictionaryLiteral(DictionaryLiteralBeginToken dictionaryBegin, List<ExpressionInLiteral> elements, CurlyBraceStructureEndToken dictionaryEnd) {
			return new DictionaryLiteral(dictionaryBegin, elements, dictionaryEnd, nextOccurrenceIndex());
		}

		protected virtual ExpressionInLiteral newExpressionInLiteral(Expression expression) {
			return new ExpressionInLiteral(expression, null, nextOccurrenceIndex());
		}

		protected virtual ExpressionInLiteral newExpressionInLiteral(Expression expression, StatementEndToken expressionSeparator) {
			return new ExpressionInLiteral(expression, expressionSeparator, nextOccurrenceIndex());
		}

		protected virtual VariableAssignment newVariableAssignment(DeclarableIdentifierToken bindableIdentifier) {
			return new VariableAssignment(bindableIdentifier, nextOccurrenceIndex());
		}

		protected virtual VariableReference newVariableReference(IdentifierToken operand) {
			return new VariableReference(operand, nextOccurrenceIndex());
		}

		protected virtual ConstantReference newConstantReference(ConstantReferenceToken operand) {
			return new ConstantReference(operand, nextOccurrenceIndex());
		}

		protected virtual PseudoVariableReference newPseudoVariableReference(PseudoVariableReferenceToken operand) {
			return new PseudoVariableReference(operand, nextOccurrenceIndex());
		}

		protected virtual LexicalLiteralValue newLexicalLiteralValue(LiteralValueToken token) {
			return new LexicalLiteralValue(token, nextOccurrenceIndex());
		}

		protected virtual BlockLiteral newBlockLiteral(BlockBeginToken blockBegin, BlockDeclaration blockDeclaration, BlockEndToken blockEnd) {
			return new BlockLiteral(blockBegin, blockDeclaration, blockEnd, nextOccurrenceIndex());
		}

		protected virtual MethodLiteral newMethodLiteral(BlockBeginToken blockBegin, MethodHeaderBeginToken methodHeaderToken, MethodDeclaration methodDeclaration, BlockEndToken blockEnd) {
			return new MethodLiteral(blockBegin, methodHeaderToken, methodDeclaration, blockEnd, nextOccurrenceIndex());
		}

		#endregion

		#region Syntax Tree Construction

		protected ParseTreeNode parseMethodDeclaration() {
			// MethodDeclaration
			
			ParseTreeNode methodHeader = parseMethodHeader();
			if (methodHeader == null || methodHeader.IsEndOfSource) {
				return handledUnexpectedToken(
						ParseNodeType.MethodDeclaration, 
						new ParseNodeType[]{ParseNodeType.MethodHeader}, 
						methodHeader);
			}
			LexicalToken leftEnclosingToken;
			ExecutableCode executableCode;
			if (nextMatches(ParseNodeType.BinaryMessageSelector, out leftEnclosingToken)) {
				ParseTreeNode primitiveOrExternalCallSpec = parseKeywordMessage(false);
				if (primitiveOrExternalCallSpec == null || primitiveOrExternalCallSpec.IsEndOfSource) {
					return handledUnexpectedToken(
							ParseNodeType.PrimitiveMethodDeclaration, 
							new ParseNodeType[]{ParseNodeType.KeywordMessage}, 
							primitiveOrExternalCallSpec);
				}
				LexicalToken rightEnclosingToken;
				if (nextMatches(ParseNodeType.BinaryMessageSelector, out rightEnclosingToken)) {
					executableCode = (ExecutableCode)parseExecutableCode();
					return newPrimitiveMethodDeclaration(
									(MethodHeader)methodHeader, 
									(BinaryMessageSelectorToken)leftEnclosingToken, 
									(KeywordMessage)primitiveOrExternalCallSpec, 
									(BinaryMessageSelectorToken)rightEnclosingToken, 
									executableCode);
				} else {
					return handledUnexpectedToken(
							ParseNodeType.PrimitiveMethodDeclaration, 
							new ParseNodeType[]{ParseNodeType.BinaryMessageSelector}, 
							rightEnclosingToken);
				}
			} else {
				executableCode = (ExecutableCode)parseExecutableCode();
				return newMethodDeclaration((MethodHeader)methodHeader, executableCode);
			}

		}
		
		protected ParseTreeNode parseMethodHeader() {
			// MethodHeader

			try {
				pushContext(ParsingContext.MethodHeader);
				LexicalToken selectorOrKeywordToken = null;
				if (nextMatches(ParseNodeType.MethodHeaderBegin, out selectorOrKeywordToken)) {
					if (!next(out selectorOrKeywordToken)) 
						return handledUnexpectedToken(
								ParseNodeType.MethodHeader, 
								new ParseNodeType[]{ParseNodeType.UnaryMethodHeader, ParseNodeType.BinaryMethodHeader, ParseNodeType.KeywordMethodHeader}, 
								nextToken);
				} else {
					advanceToken();
				}
				if (selectorOrKeywordToken == null || selectorOrKeywordToken.IsEndOfSource) return selectorOrKeywordToken;
				
				LexicalToken parameterToken = null;
				switch (selectorOrKeywordToken.ParseNodeType) {
					case ParseNodeType.Identifier:
						
						return newUnaryMethodHeader((DeclarableIdentifierToken)selectorOrKeywordToken);
						
					case ParseNodeType.BinaryMessageSelector:
					case ParseNodeType.VerticalBar:
						
						if (nextMatches(ParseNodeType.Identifier, out parameterToken)) {
							if (parameterToken.CanBeDeclaredAsVariableOrParameter) {
								if (selectorOrKeywordToken.ParseNodeType == ParseNodeType.VerticalBar) {
									selectorOrKeywordToken = ((VerticalBarToken)selectorOrKeywordToken).AsBinaryMessageSelectorToken;
								}
								return newBinaryMethodHeader((BinaryMessageSelectorToken)selectorOrKeywordToken, (DeclarableIdentifierToken)parameterToken);
							} else {
								return handledUnexpectedToken(
										ParseNodeType.BinaryMethodHeader, 
										new ParseNodeType[]{ParseNodeType.DeclarableIdentifier}, 
										parameterToken);
							}
						} else {
							return handledUnexpectedToken(
									ParseNodeType.BinaryMethodHeader, 
									new ParseNodeType[]{ParseNodeType.Identifier}, 
									parameterToken);
						}
						
					case ParseNodeType.Keyword:
						
						bool nextIsKeyword = true;
						List<KeywordMethodHeaderSegment> segments = new List<KeywordMethodHeaderSegment>();
						while (nextIsKeyword) {
							if (nextMatches(ParseNodeType.Identifier, out parameterToken)) {
								if (parameterToken.CanBeDeclaredAsVariableOrParameter) {
									segments.Add(newKeywordMethodHeaderSegment((KeywordToken)selectorOrKeywordToken, (DeclarableIdentifierToken)parameterToken));
								} else {
									return handledUnexpectedToken(
											ParseNodeType.KeywordMethodHeader, 
											new ParseNodeType[]{ParseNodeType.DeclarableIdentifier}, 
											parameterToken);
								}
							} else {
								return handledUnexpectedToken(
										ParseNodeType.KeywordMethodHeader, 
										new ParseNodeType[]{ParseNodeType.Identifier}, 
										parameterToken);
							}
							if (peek(out selectorOrKeywordToken) && selectorOrKeywordToken.ParseNodeType == ParseNodeType.Keyword) {
								advanceToken();
								nextIsKeyword = true;
							} else {
								nextIsKeyword = false;
							}
						}
						return newKeywordMethodHeader(segments);
						
					default:
						
						return handledUnexpectedToken(
								ParseNodeType.MethodHeader, 
								new ParseNodeType[]{ParseNodeType.Identifier, ParseNodeType.BinaryMessageSelector, ParseNodeType.Keyword}, 
								selectorOrKeywordToken);
						}
				
			} finally {
				popContext();
			}
		}
		
		protected ParseTreeNode parseBlockDeclaration() {
			// BlockDeclaration
			var parameters = parseBlockParameterDeclarationList();
			if (parameters != null) {
				if (parameters.IsEndOfSource) return newBlockDeclaration(null, null);
			}
			var body = parseExecutableCode();
			if (body != null) {
				if (body.IsEndOfSource) body = null; 
			}
			return newBlockDeclaration(
					(BlockParameterDeclarationList)parameters, 
					(ExecutableCode)body);
		}

		protected ParseTreeNode parseExecutableCode() {
			// ExecutableCode
			ParseTreeNode localVariables = parseLocalVariableDeclarationList();
			List<Statement> statements = new List<Statement>();

			ParseTreeNode statement = null;
			FinalStatement finalStatement = null;
			bool hasMoreStatements = false;
			do {
				statement = parseStatement();
				if (statement != null) {
					switch (statement.ParseNodeType) {
						case ParseNodeType.Statement:
							hasMoreStatements = true;
							statements.Add((Statement)statement);
							break;
						case ParseNodeType.FinalStatement:
							hasMoreStatements = false;
							finalStatement = (FinalStatement)statement;
							break;
						case ParseNodeType.EndOfSource:
							hasMoreStatements = false;
							break;
					}
				} else {
					hasMoreStatements = false;
				}
			} while (hasMoreStatements);
			return newExecutableCode((LocalVariableDeclarationList)localVariables, statements, finalStatement);
		}
		
		protected ParseTreeNode parseLocalVariableDeclarationList() {
			// LocalVariableDeclarationList
			LexicalToken listBegin;
			if  (nextMatches(ParseNodeType.VerticalBar, out listBegin)) {
				List<DeclarableIdentifierToken> variables = new List<DeclarableIdentifierToken>();
				LexicalToken variable;
				try {
					pushContext(ParsingContext.VariableDeclaration);
					while (nextMatches(ParseNodeType.Identifier, out variable)) {
						if (variable.CanBeDeclaredAsVariableOrParameter) {
							variables.Add((DeclarableIdentifierToken)variable);
						} else {
							return handledUnexpectedToken(ParseNodeType.LocalVariableDeclarationList, new ParseNodeType[]{ParseNodeType.DeclarableIdentifier}, variable);
						}
					}
				} finally {
					popContext();
				}
				LexicalToken listEnd;
				if  (nextMatches(ParseNodeType.VerticalBar, out listEnd)) {
					return newLocalVariableDeclarationList((VerticalBarToken)listBegin, variables, (VerticalBarToken)listEnd);
				} else {
					return handledUnexpectedToken(ParseNodeType.LocalVariableDeclarationList, new ParseNodeType[]{ParseNodeType.VerticalBar}, listEnd == null ? nextToken : listEnd);
				}
			}
			return null;
		}
		
		protected ParseTreeNode parseBlockParameterDeclarationList() {
			// BlockParameterDeclarationList
			List<BlockParameterToken> parameters = new List<BlockParameterToken>();
			LexicalToken blockParameter;
			pushContext(ParsingContext.VariableDeclaration);
			while (nextMatches(ParseNodeType.BlockParameter, out blockParameter)) {
				parameters.Add((BlockParameterToken)blockParameter);
			}
			popContext();
			if (parameters.Count < 1) return null;
			LexicalToken listEnd;
			if  (nextMatches(ParseNodeType.VerticalBar, out listEnd)) {
				return newBlockParameterDeclarationList(parameters, (VerticalBarToken)listEnd);
			}
			return null;
		}
		
		protected ParseTreeNode parseMethodReturnPrefix() {
			// MethodReturnPrefix
			LexicalToken returnOp;
			if (nextMatches(ParseNodeType.ReturnOp, out returnOp)) {
				return newMethodReturnPrefix((ReturnOpToken)returnOp);
			}
			return null;
		}
		
		protected ParseTreeNode parseAssignmentPrefix(DeclarableIdentifierToken identifier) {
			// AssignmentPrefix
			LexicalToken assignmentOp;
			if (nextMatches(ParseNodeType.AssignOp, out assignmentOp)) {
				return newAssignmentPrefix(newVariableAssignment(identifier), (AssignOpToken)assignmentOp);
			}
			return null;
		}
		
		protected ParseTreeNode parseStatement() {
			// Statement | FinalStatement
			ParseTreeNode methodReturnPrefix = parseMethodReturnPrefix();
			bool isStatementRequired = methodReturnPrefix != null;
			LexicalToken identifier, initialExpressionToken;
			ParseTreeNode assignmentPrefix = null;
			List<AssignmentPrefix> assignmentPrefixes = new List<AssignmentPrefix>();
			bool hasAssignmentPrefix = false;
	
			do {
				if (nextMatches(ParseNodeType.Identifier, out identifier)) {
					if (identifier.CanBeDeclaredAsVariableOrParameter) {
						assignmentPrefix = parseAssignmentPrefix((DeclarableIdentifierToken)identifier);
						if (assignmentPrefix == null) {
							initialExpressionToken = identifier;
							hasAssignmentPrefix = false;
						} else {
							assignmentPrefixes.Add((AssignmentPrefix)assignmentPrefix);
							initialExpressionToken = null;
							hasAssignmentPrefix = true;
						}
					} else {
						initialExpressionToken = identifier;
						hasAssignmentPrefix = false;
					}
				} else {
					initialExpressionToken = null;
					hasAssignmentPrefix = false;
				}
			} while (hasAssignmentPrefix);
		
			bool isExpressionRequired = isStatementRequired || assignmentPrefixes.Count > 0;
			ParseTreeNode expression = parseExpression(initialExpressionToken);
			bool isEndOfSource = false;
			bool hasNoExpression = expression == null || (isEndOfSource = expression.IsEndOfSource);
			if (hasNoExpression) {
				if (isExpressionRequired) 
					handledUnexpectedToken(ParseNodeType.Statement, new ParseNodeType[]{ParseNodeType.Expression}, initialExpressionToken == null ? nextToken : initialExpressionToken);
				return isEndOfSource ? expression : null;
			}
			LexicalToken endToken;
			StatementEndToken stmtEndToken = null;
			if (nextMatches(ParseNodeType.StatementEnd, out endToken) && methodReturnPrefix == null) {
				stmtEndToken = (StatementEndToken)endToken;
				return newStatement(assignmentPrefixes, (Expression)expression, stmtEndToken);
			} else {
				if (endToken.ParseNodeType == ParseNodeType.StatementEnd) stmtEndToken = (StatementEndToken)endToken;
				if (methodReturnPrefix == null) {
					return newFinalStatement(assignmentPrefixes, (Expression)expression, stmtEndToken);
				} else {
					return newFinalStatement((MethodReturnPrefix)methodReturnPrefix, assignmentPrefixes, (Expression)expression, stmtEndToken);
				}
			}
		}

		protected ParseTreeNode parseExpression(ParseTreeNode operand) {
			// Expression
			if (operand == null) {
				operand = parseOperand();
				if (operand == null || operand.IsEndOfSource) return operand;
			} else {
				switch (operand.OperandNodeType) {
					case ParseNodeType.Identifier:
						// VariableReference
						operand = newVariableReference((IdentifierToken)operand);
						break;
						
					case ParseNodeType.False:
					case ParseNodeType.Nil:
					case ParseNodeType.True:
						// ConstantReference
						operand = newConstantReference((ConstantReferenceToken)operand);
						break;
						
					case ParseNodeType.Self:
					case ParseNodeType.Super:
					case ParseNodeType.ThisContext:
						// PseudoVariableReference
						operand = newPseudoVariableReference((PseudoVariableReferenceToken)operand);
						break;
						
					case ParseNodeType.EndOfSource: 
						return operand;
						
					default:
						return handledUnexpectedToken(
								ParseNodeType.Expression, 
								new ParseNodeType[]{ParseNodeType.Identifier}, 
								operand);
				}
			}
			ParseTreeNode messageChain = parseMessageChain();
			bool hasMessageChain = messageChain != null && !messageChain.IsEndOfSource;
			List<CascadedMessageChain> cascadedMessages = null;
			if (hasMessageChain) {
				ParseTreeNode cascadedMessage = null;
				bool hasCascadedMessage = peekLexicalType() == ParseNodeType.MessageCascadeOp;
				if (hasCascadedMessage) cascadedMessages = new List<CascadedMessageChain>();
				while (hasCascadedMessage) {
					cascadedMessage = parseCascadedMessage();
					hasCascadedMessage = cascadedMessage != null && cascadedMessage.ParseNodeType != ParseNodeType.EndOfSource;
					if (hasCascadedMessage) {
						cascadedMessages.Add((CascadedMessageChain)cascadedMessage);
						hasCascadedMessage = peekLexicalType() == ParseNodeType.MessageCascadeOp;
					}
				}
			}
			var expression = newExpression((Operand)operand, (MessageChain)messageChain, hasMessageChain ? (cascadedMessages != null && cascadedMessages.Count > 0 ? cascadedMessages : null) : null);
			switch (peekLexicalType()) {
				case ParseNodeType.StatementEnd:
				case ParseNodeType.ExpressionEnd:
				case ParseNodeType.CurlyBraceStructureEnd:
				case ParseNodeType.BlockEnd:
				case ParseNodeType.EndOfSource:
					return expression;
				default:
					return handledUnexpectedToken(
							ParseNodeType.Expression, 
							new ParseNodeType[]{ParseNodeType.StatementEnd, ParseNodeType.ExpressionEnd, ParseNodeType.CurlyBraceStructureEnd, ParseNodeType.BlockEnd, ParseNodeType.EndOfSource}, 
							nextToken);

			}
		}
		
		protected ParseTreeNode parseCascadedMessage() {
			// CascadedMessage
			LexicalToken messageCascadeOp;
			if (nextMatches(ParseNodeType.MessageCascadeOp, out messageCascadeOp)) {
				ParseTreeNode messageChain = parseMessageChain();
				bool hasMessageChain = messageChain != null && messageChain.ParseNodeType != ParseNodeType.EndOfSource;
				if (hasMessageChain) {
					return newCascadedMessageChain((MessageCascadeOpToken)messageCascadeOp, (MessageChain)messageChain);
				} else {
					return handledUnexpectedToken(ParseNodeType.CascadedMessage, new ParseNodeType[]{ParseNodeType.MessageChain}, messageChain == null ? nextToken : messageChain);
				}
			} else {
				return null;
			}
		}
		
		protected ParseTreeNode parseMessageChain() {
			// MessageChain
			
			try {
				pushContext(ParsingContext.MessageSelector);
				switch (peekLexicalType()) {
					case ParseNodeType.Keyword:
						return parseKeywordMessageChain();
					case ParseNodeType.BinaryMessageSelector:
					case ParseNodeType.VerticalBar:
						return parseInitiallyBinaryMessageChain();
					case ParseNodeType.Identifier:
						return parseInitiallyUnaryMessageChain();
					case ParseNodeType.ExpressionEnd:
					case ParseNodeType.BlockEnd:
					case ParseNodeType.CurlyBraceStructureEnd:
					case ParseNodeType.StatementEnd:
					case ParseNodeType.EndOfSource:
						return null;
					default:
						return handledUnexpectedToken(
									ParseNodeType.MessageChain, 
									new ParseNodeType[]{ParseNodeType.ExpressionEnd, ParseNodeType.BlockEnd, ParseNodeType.CurlyBraceStructureEnd, ParseNodeType.StatementEnd, ParseNodeType.EndOfSource}, 
									nextToken);
				}
			} finally {
				popContext();
			}
			
		}
		
		protected ParseTreeNode parseKeywordMessageChain() {
			// KeywordMessageChain
			ParseTreeNode keywordMessage = parseKeywordMessage(true);
			if (keywordMessage != null && keywordMessage.ParseNodeType != ParseNodeType.EndOfSource) {
				return newKeywordMessageChain((KeywordMessage)keywordMessage);
			} else {
				return handledUnexpectedToken(ParseNodeType.KeywordMessageChain, new ParseNodeType[]{ParseNodeType.KeywordMessage}, keywordMessage == null ? nextToken : keywordMessage);
			}
		}
		
		protected ParseTreeNode parseInitiallyBinaryMessageChain() {
			// InitiallyBinaryMessageChain
			ParseTreeNode binaryMessageChain = parseBinaryOnlyMessageChain();
			if (binaryMessageChain.ParseNodeType != ParseNodeType.BinaryOnlyMessageChain) {
				return handledUnexpectedToken(ParseNodeType.InitiallyBinaryMessageChain, new ParseNodeType[]{ParseNodeType.BinaryOnlyMessageChain}, binaryMessageChain == null ? nextToken : binaryMessageChain);
			}
			switch (peekLexicalType()) {
				case ParseNodeType.Keyword:
					return newInitiallyBinaryMessageChain((BinaryOnlyMessageChain)binaryMessageChain, (KeywordMessage)parseKeywordMessage(true));
				default:
					return newInitiallyBinaryMessageChain((BinaryOnlyMessageChain)binaryMessageChain);
			}
		}
		
		protected ParseTreeNode parseInitiallyUnaryMessageChain() {
			// InitiallyUnaryMessageChain
			ParseTreeNode unaryMessageChain = parseUnaryOnlyMessageChain();
			switch (peekLexicalType()) {
				case ParseNodeType.Keyword:
					return newInitiallyUnaryMessageChain((UnaryOnlyMessageChain)unaryMessageChain, (KeywordMessage)parseKeywordMessage(true));
				case ParseNodeType.BinaryMessageSelector:
				case ParseNodeType.VerticalBar:
					ParseTreeNode binaryMessageChain = parseBinaryOnlyMessageChain();
					switch (peekLexicalType()) {
						case ParseNodeType.Keyword:
							return newInitiallyUnaryMessageChain((UnaryOnlyMessageChain)unaryMessageChain, (BinaryOnlyMessageChain)binaryMessageChain, (KeywordMessage)parseKeywordMessage(true));
						default:
							return newInitiallyUnaryMessageChain((UnaryOnlyMessageChain)unaryMessageChain, (BinaryOnlyMessageChain)binaryMessageChain);
					}
				default:
					return newInitiallyUnaryMessageChain((UnaryOnlyMessageChain)unaryMessageChain);
			}
		}
		
		protected ParseTreeNode parseBinaryOnlyMessageChain() {
			// BinaryOnlyMessageChain
			List<BinaryMessage> messages = new List<BinaryMessage>();
			ParseTreeNode binaryMessage = parseBinaryMessage();
			do {
				if (binaryMessage == null || binaryMessage.IsEndOfSource) 
					return handledUnexpectedToken(ParseNodeType.BinaryOnlyMessageChain, new ParseNodeType[]{ParseNodeType.BinaryMessage}, binaryMessage == null ? nextToken : binaryMessage);
				messages.Add((BinaryMessage)binaryMessage);
				switch (peekLexicalType()) {
					case ParseNodeType.BinaryMessageSelector:
					case ParseNodeType.VerticalBar:
						binaryMessage = parseBinaryMessage();
						break;
					default:
						binaryMessage = null;
						break;
				}
			} while(binaryMessage != null);
			return newBinaryOnlyMessageChain(messages);
		}
		
		protected ParseTreeNode parseUnaryOnlyMessageChain() {
			// UnaryOnlyMessageChain
			List<UnaryMessage> messages = new List<UnaryMessage>();
			ParseTreeNode unaryMessage = parseUnaryMessage();
			do {
				if (unaryMessage == null || unaryMessage.IsEndOfSource) 
					return handledUnexpectedToken(ParseNodeType.UnaryOnlyMessageChain, new ParseNodeType[]{ParseNodeType.UnaryMessage}, unaryMessage == null ? nextToken : unaryMessage);
				messages.Add((UnaryMessage)unaryMessage);
				unaryMessage = peekLexicalType() == ParseNodeType.Identifier ? parseUnaryMessage() : null;
			} while(unaryMessage != null);
			return newUnaryOnlyMessageChain(messages);
		}
		
		protected ParseTreeNode parseBinaryMessage() {
			// BinaryMessage
			LexicalToken selector;
			if (next(out selector)) {
				ParseTreeNode operand = parseBinaryMessageOperand(true);
				if (operand != null && operand.ParseNodeType != ParseNodeType.EndOfSource) {
					if (selector.ParseNodeType == ParseNodeType.VerticalBar) {
						selector = ((VerticalBarToken)selector).AsBinaryMessageSelectorToken;
					}
					return newBinaryMessage((BinaryMessageSelectorToken)selector, (BinaryMessageOperand)operand);
				} else {
					return handledUnexpectedToken(ParseNodeType.BinaryMessage, new ParseNodeType[]{ParseNodeType.BinaryMessageOperand}, operand == null ? nextToken : operand);
				}
			} else {
				return handledUnexpectedToken(ParseNodeType.BinaryMessage, new ParseNodeType[]{ParseNodeType.BinaryMessageSelector}, selector == null ? nextToken : selector);
			}
		}
		
		protected ParseTreeNode parseUnaryMessage() {
			// parseUnaryMessage
			LexicalToken selector;
			if (next(out selector)) {
				return newUnaryMessage((IdentifierToken)selector);
			} else {
				return handledUnexpectedToken(ParseNodeType.UnaryMessage, new ParseNodeType[]{ParseNodeType.Identifier}, selector == null ? nextToken : selector);
			}
		}

		protected ParseTreeNode parseKeywordMessage(bool acceptExpressionsAsParameters) {
			// KeywordMessage
			List<KeywordMessageSegment> segments = new List<KeywordMessageSegment>();
			ParseTreeNode keywordMessageSegment = parseKeywordMessageSegment(acceptExpressionsAsParameters);
			do {
				if (keywordMessageSegment == null || keywordMessageSegment.IsEndOfSource) 
					return handledUnexpectedToken(ParseNodeType.KeywordMessage, new ParseNodeType[]{ParseNodeType.KeywordMessageSegment}, keywordMessageSegment == null ? nextToken : keywordMessageSegment);
				segments.Add((KeywordMessageSegment)keywordMessageSegment);
				keywordMessageSegment = peekLexicalType() == ParseNodeType.Keyword ? parseKeywordMessageSegment(acceptExpressionsAsParameters) : null;
			} while(keywordMessageSegment != null);
			return newKeywordMessage(segments);
		}
		
		protected ParseTreeNode parseKeywordMessageSegment(bool acceptExpressionsAsParameters) {
			// KeywordMessageSegment
			LexicalToken selector;
			if (nextMatches(ParseNodeType.Keyword, out selector)) {
				ParseTreeNode argument = parseKeywordMessageArgument(acceptExpressionsAsParameters);
				if (argument != null && argument.ParseNodeType != ParseNodeType.EndOfSource) {
					return newKeywordMessageSegment((KeywordToken)selector, (KeywordMessageArgument)argument);
				} else {
					return handledUnexpectedToken(ParseNodeType.KeywordMessageSegment, new ParseNodeType[]{ParseNodeType.KeywordMessageArgument}, argument == null ? nextToken : argument);
				}
			} else {
				return handledUnexpectedToken(ParseNodeType.KeywordMessageSegment, new ParseNodeType[]{ParseNodeType.Keyword}, selector == null ? nextToken : selector);
			}
		}
		
		protected ParseTreeNode parseKeywordMessageArgument(bool acceptExpressionsAsParameters) {
			// KeywordMessageArgument
			ParseTreeNode operand = parseBinaryMessageOperand(acceptExpressionsAsParameters);
			if (operand == null || operand.IsEndOfSource) 
				return handledUnexpectedToken(ParseNodeType.KeywordMessageArgument, new ParseNodeType[]{ParseNodeType.BinaryMessageOperand}, operand == null ? nextToken : operand);
			bool expectBinaryMessageChain;
			switch (peekLexicalType()) {
				case ParseNodeType.BinaryMessageSelector:
				case ParseNodeType.VerticalBar:
					expectBinaryMessageChain = acceptExpressionsAsParameters;
					break;
				default:
					expectBinaryMessageChain = false;
					break;
			}
			if (expectBinaryMessageChain) {
				ParseTreeNode messageChain = parseBinaryOnlyMessageChain();
				if (messageChain == null || messageChain.IsEndOfSource) 
					return handledUnexpectedToken(ParseNodeType.KeywordMessageArgument, new ParseNodeType[]{ParseNodeType.BinaryOnlyMessageChain}, messageChain == null ? nextToken : messageChain);
				return newKeywordMessageArgument((BinaryMessageOperand)operand, (BinaryOnlyMessageChain)messageChain);
			} else {
				return newKeywordMessageArgument((BinaryMessageOperand)operand);
			}
		}
		
		protected ParseTreeNode parseBinaryMessageOperand(bool acceptExpressionsAsParameters) {
			// BinaryMessageOperand
			ParseTreeNode operand = parseOperand();
			if (operand == null || operand.IsEndOfSource) 
				return handledUnexpectedToken(ParseNodeType.BinaryMessageOperand, new ParseNodeType[]{ParseNodeType.Operand}, operand == null ? nextToken : operand);
			if (acceptExpressionsAsParameters && peekLexicalType() == ParseNodeType.Identifier) {
				ParseTreeNode messageChain = parseUnaryOnlyMessageChain();
				if (messageChain == null || messageChain.IsEndOfSource) 
					return handledUnexpectedToken(ParseNodeType.BinaryMessageOperand, new ParseNodeType[]{ParseNodeType.UnaryOnlyMessageChain}, messageChain == null ? nextToken : messageChain);
				return newBinaryMessageOperand((Operand)operand, (UnaryOnlyMessageChain)messageChain);
			} else {
				return newBinaryMessageOperand((Operand)operand);
			}
		}
		
		protected ParseTreeNode parseOperand() {
			// Operand
			try {
				pushContext(ParsingContext.General);
				LexicalToken token;
				if (!peek(out token)) return null;
				switch (token.OperandNodeType) {
					case ParseNodeType.Integer:
					case ParseNodeType.ScaledDecimal:
					case ParseNodeType.SinglePrecision:
					case ParseNodeType.DoublePrecision:
					case ParseNodeType.QuadPrecision:
					case ParseNodeType.Char:
					case ParseNodeType.String:
					case ParseNodeType.Symbol:
					case ParseNodeType.LiteralBindingReference:
					case ParseNodeType.ByteArray:
					case ParseNodeType.Array:
						// LexicalLiteralValue
						advanceToken();
						return newLexicalLiteralValue((LiteralValueToken)token);
						
					case ParseNodeType.Identifier:
						// VariableReference
						advanceToken();
						return newVariableReference((IdentifierToken)token);
						
					case ParseNodeType.False:
					case ParseNodeType.Nil:
					case ParseNodeType.True:
						// ConstantReference
						advanceToken();
						return newConstantReference((ConstantReferenceToken)token);

					case ParseNodeType.Self:
					case ParseNodeType.Super:
					case ParseNodeType.ThisContext:
						// PseudoVariableReference
						advanceToken();
						return newPseudoVariableReference((PseudoVariableReferenceToken)token);
						
					case ParseNodeType.ExpressionBegin:
						// NestedExpression 
						advanceToken();
						return parseNestedExpression((ExpressionBeginToken)token);
						
					case ParseNodeType.DynamicArrayBegin:
						// DynamicArrayLiteral
						advanceToken();
						return parseDynamicArrayLiteral((DynamicArrayBeginToken)token);
						
					case ParseNodeType.DictionaryLiteralBegin:
						// DictionaryLiteral
						advanceToken();
						return parseDictionaryLiteral((DictionaryLiteralBeginToken)token);
						
					case ParseNodeType.BlockBegin:
						// BlockLiteral (BlockBeginToken)
						advanceToken();
						return parseCodeLiteral((BlockBeginToken)token);
						
					case ParseNodeType.ExpressionEnd:
					case ParseNodeType.BlockEnd:
					case ParseNodeType.CurlyBraceStructureEnd:
					case ParseNodeType.StatementEnd:
					case ParseNodeType.EndOfSource: 
						return null;
						
					default:
						return handledUnexpectedToken(
								ParseNodeType.Operand, 
								new ParseNodeType[]{ParseNodeType.Identifier, ParseNodeType.Expression, ParseNodeType.Literal, ParseNodeType.BlockLiteral}, 
								token);
				}
			} finally {
				popContext();
			}
			
		}
		
		protected ParseTreeNode parseNestedExpression(ExpressionBeginToken beginToken) {
			ParseTreeNode statement = parseStatement();
			if (statement != null) {
				LexicalToken expressionEnd;
				if (nextMatches(ParseNodeType.ExpressionEnd, out expressionEnd)) {
					return newNestedExpression(beginToken, (Statement)statement, (ExpressionEndToken)expressionEnd);
				} else {
					return handledUnexpectedToken(ParseNodeType.NestedExpression, new ParseNodeType[]{ParseNodeType.ExpressionEnd}, nextToken);
				}
			} else {
				return handledUnexpectedToken(ParseNodeType.NestedExpression, new ParseNodeType[]{ParseNodeType.Expression}, nextToken);
			}
		}
		
		protected ParseTreeNode parseDynamicArrayLiteral(DynamicArrayBeginToken beginToken) {
			ParseTreeNode expression = parseExpression(null);
			LexicalToken endToken;
			bool hasExpression = expression != null && expression.ParseNodeType != ParseNodeType.EndOfSource;
			LexicalToken stmtSeparator = null;
			ExpressionInLiteral expressionLiteral = null;
			List<ExpressionInLiteral> elements = new List<ExpressionInLiteral>();
			while (hasExpression) {
				if (!nextMatches(ParseNodeType.StatementEnd, out stmtSeparator)) {
					expressionLiteral = newExpressionInLiteral((Expression)expression, null);
					elements.Add(expressionLiteral);
					if (nextMatches(ParseNodeType.CurlyBraceStructureEnd, out endToken)) {
						return newDynamicArrayLiteral(beginToken, elements, (CurlyBraceStructureEndToken)endToken);
					} else {
						return handledUnexpectedToken(ParseNodeType.DynamicArrayLiteral, new ParseNodeType[]{ParseNodeType.CurlyBraceStructureEnd}, expression == null ? nextToken : expression);
					}
				}
				expressionLiteral = newExpressionInLiteral((Expression)expression, (StatementEndToken)stmtSeparator);
				elements.Add(expressionLiteral);
				expression = parseExpression(null);
				hasExpression = expression != null && expression.ParseNodeType != ParseNodeType.EndOfSource;
			}
			return handledUnexpectedToken(ParseNodeType.DynamicArrayLiteral, new ParseNodeType[]{ParseNodeType.Expression}, expression == null ? nextToken : expression);
		}
		
		protected ParseTreeNode parseDictionaryLiteral(DictionaryLiteralBeginToken beginToken) {
			ParseTreeNode expression = parseExpression(null);
			LexicalToken endToken;
			bool hasExpression = expression != null && expression.ParseNodeType != ParseNodeType.EndOfSource;
			LexicalToken stmtSeparator = null;
			ExpressionInLiteral expressionLiteral = null;
			List<ExpressionInLiteral> elements = new List<ExpressionInLiteral>();
			while (hasExpression) {
				if (!nextMatches(ParseNodeType.StatementEnd, out stmtSeparator)) {
					expressionLiteral = newExpressionInLiteral((Expression)expression, null);
					elements.Add(expressionLiteral);
					if (nextMatches(ParseNodeType.CurlyBraceStructureEnd, out endToken)) {
						return newDictionaryLiteral(beginToken, elements, (CurlyBraceStructureEndToken)endToken);
					} else {
						return handledUnexpectedToken(ParseNodeType.DictionaryLiteral, new ParseNodeType[]{ParseNodeType.CurlyBraceStructureEnd}, expression == null ? nextToken : expression);
					}
				}
				expressionLiteral = newExpressionInLiteral((Expression)expression, (StatementEndToken)stmtSeparator);
				elements.Add(expressionLiteral);
				expression = parseExpression(null);
				hasExpression = expression != null && expression.ParseNodeType != ParseNodeType.EndOfSource;
			}
			return handledUnexpectedToken(ParseNodeType.DictionaryLiteral, new ParseNodeType[]{ParseNodeType.Expression}, expression == null ? nextToken : expression);
		}
		
		protected ParseTreeNode parseCodeLiteral(BlockBeginToken beginToken) {
			// CodeLiteral
			switch (peekLexicalType()) {
				case ParseNodeType.MethodHeaderBegin:
					return parseMethodLiteral(beginToken);
				case ParseNodeType.BinaryMessageSelector:
				case ParseNodeType.Keyword:
					return SupportsMethodLiteralSyntax ? parseMethodLiteral(beginToken) : parseBlockLiteral(beginToken);
				default:
					return parseBlockLiteral(beginToken);
			}
		}
		
		protected ParseTreeNode parseBlockLiteral(BlockBeginToken beginToken) {
			// BlockLiteral
			ParseTreeNode blockDeclaration = parseBlockDeclaration();
			if (blockDeclaration == null || blockDeclaration.IsEndOfSource) {
				return handledUnexpectedToken(ParseNodeType.BlockLiteral, new ParseNodeType[]{ParseNodeType.BlockDeclaration}, blockDeclaration == null ? nextToken : blockDeclaration);
			}
			LexicalToken endToken;
			if (nextMatches(ParseNodeType.BlockEnd, out endToken)) {
				return newBlockLiteral(beginToken, (BlockDeclaration)blockDeclaration, (BlockEndToken)endToken);
			} else {
				return handledUnexpectedToken(ParseNodeType.BlockLiteral, new ParseNodeType[]{ParseNodeType.BlockEnd}, endToken);
			}
		}
		
		protected ParseTreeNode parseMethodLiteral(BlockBeginToken beginToken) {
			// MethodLiteral
			LexicalToken methodHeaderBeginToken;
			if (!nextMatches(ParseNodeType.MethodHeaderBegin, out methodHeaderBeginToken)) methodHeaderBeginToken = null;
			ParseTreeNode  methodDeclaration = parseMethodDeclaration();
			if (methodDeclaration == null || methodDeclaration.IsEndOfSource) {
				return handledUnexpectedToken(ParseNodeType.MethodLiteral, new ParseNodeType[]{ParseNodeType.MethodDeclaration}, methodDeclaration == null ? nextToken : methodDeclaration);
			}
			LexicalToken endToken;
			if (nextMatches(ParseNodeType.BlockEnd, out endToken)) {
				return newMethodLiteral(beginToken, (MethodHeaderBeginToken)methodHeaderBeginToken, (MethodDeclaration)methodDeclaration, (BlockEndToken)endToken);
			} else {
				return handledUnexpectedToken(ParseNodeType.MethodLiteral, new ParseNodeType[]{ParseNodeType.BlockEnd}, endToken);
			}
		}
		
		#endregion

		#endregion

	}
	
}




