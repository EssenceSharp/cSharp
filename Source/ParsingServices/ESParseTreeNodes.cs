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
using System.Text;
using Microsoft.Scripting;
#endregion

namespace EssenceSharp.ParsingServices {
	
	public abstract class NonTerminalParseTreeNode : ParseTreeNode {

		protected NonTerminalParseTreeNode(int occurrenceIndex) : base(occurrenceIndex) {
		}

		public abstract LexicalToken InitialToken {
			get;
		}

		public abstract LexicalToken FinalToken {
			get;
		}
			
		public override uint LineNumberStart {
			get {	LexicalToken initialToken = InitialToken;
				return initialToken == null ? 0 : initialToken.LineNumberStart;}
		}
			
		public override uint ColumnNumberStart {
			get {	LexicalToken initialToken = InitialToken;
				return initialToken == null ? 0 : initialToken.ColumnNumberStart;}
		}
			
		public override uint LineNumberEnd {
			get {	LexicalToken finalToken = FinalToken;
				return finalToken == null ? 0 : finalToken.LineNumberEnd;}
		}
			
		public override uint ColumnNumberEnd {
			get {	LexicalToken finalToken = FinalToken;
				return finalToken == null ? 0 : finalToken.ColumnNumberEnd;}
		}

		public override SourceLocation Start {
			get {	LexicalToken initialToken = InitialToken;
				return new SourceLocation(
					OccurrenceIndex, 
					(int)(initialToken == null ? 0 : initialToken.LineNumberStart), 
					(int)(initialToken == null ? 0 : initialToken.ColumnNumberStart));}
		}

		public override SourceLocation End {
			get {	LexicalToken finalToken = FinalToken;
				return new SourceLocation(
					OccurrenceIndex, 
					(int)(finalToken == null ? 0 : finalToken.LineNumberEnd), 
					(int)(finalToken == null ? 0 : finalToken.ColumnNumberEnd));}
		}
		
		public override bool IsNonTerminalNode {
			get {return true;}
		}

		public virtual bool IsMethodHeader {
			get {return false;}
		}
		
		public virtual bool IsOperand {
			get {return false;}
		}
		
		public virtual bool IsNamedReference {
			get {return false;}
		}
		
		public override void printUsing(Action<String> append, Action<uint> newLine) {
			// Print out a technical description of the token and its meta-information
			
			printUsing(0, append, newLine);			
		}
		
		public override void printUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of the token and its meta-information
			
			append("[<");
			append(ParseNodeType.ToString());
			
			append("> Line=");
			append(LineNumberStart.ToString());
			append(" column=");
			append(ColumnNumberStart.ToString());
			append(": ");
		
			printElementsUsing(depth + 1, append, newLine);
			newLine(depth);
			append("]");
			
		}
		
	}
	
	public abstract class RootParseTreeNode : NonTerminalParseTreeNode {

		protected RootParseTreeNode(int occurrenceIndex) : base(occurrenceIndex) {
		}

	}
	
	public class MethodDeclaration : RootParseTreeNode {
		protected MethodHeader header = null;
		protected ExecutableCode body = null;
		
		public MethodDeclaration(MethodHeader header, ExecutableCode body, int occurrenceIndex) : base(occurrenceIndex) {
			this.header = header;
			this.body = body;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.MethodDeclaration;}
		}
	
		public override bool IsMethodDeclaration {
			get {return true;}
		}

		public override LexicalToken InitialToken {
			get {	if (header != null) {
					var token = header.InitialToken;
					if (token != null) return token;
				}
				if (body != null) {
					var token = body.InitialToken;
					if (token != null) return token;
				}
				return null;}
		}

		public override LexicalToken FinalToken {
			get {	if (body != null) {
					var token = body.FinalToken;
					if (token != null) return token;
				}
				if (header != null) {
					var token = header.FinalToken;
					if (token != null) return token;
				}
				return null;}
		}

		public MethodHeader Header {
			get {return header;}
		}

		public bool SpecifiesClassName {
			get {return Header.SpecifiesClassName;}
		}
		
		public SymbolLiteralToken ClassNameSymbol {
			get {return Header.ClassNameSymbol;}
		}
		
		public ExecutableCode Body {
			get {return body;}
		}

		public SymbolLiteralToken SelectorSymbol {
			get {return Header.SelectorSymbol;}
		}

		public String SelectorString {
			get {return Header.SelectorString;}
		}

		public uint NumArgs {
			get {return Header.NumArgs;}
		}

		public void parametersDo(Action<DeclarableIdentifierToken> enumerator1) {
			Header.parametersDo(enumerator1);
		}

		public void parameterNamesDo(Action<String> enumerator1) {
			Header.parameterNamesDo(enumerator1);
		}

		public void variablesDo(Action<DeclarableIdentifierToken> enumerator1) {
			if (body == null) return;
			body.variablesDo(enumerator1);
		}

		public void variableNamesDo(Action<String> enumerator1) {
			if (body == null) return;
			body.variableNamesDo(enumerator1);
		}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (header != null) {
				newLine(depth);
				header.printUsing(depth, append, newLine);
			}
			if (body != null) {
				newLine(depth);
				body.printUsing(depth, append, newLine);
			}
			
		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (header != null) {
				header.esPrintUsing(depth, append, newLine);
			}
			newLine(depth);
			if (body != null) {
				body.esPrintUsing(depth, append, newLine);
			}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToMethodDeclaration(this);
		}
		
	}
	
	public class PrimitiveMethodDeclaration : MethodDeclaration {
		protected BinaryMessageSelectorToken leftEnclosingToken = null;
		protected KeywordMessage primitiveSpecicationExpression = null;
		protected BinaryMessageSelectorToken rightEnclosingToken = null;
		
		public PrimitiveMethodDeclaration(MethodHeader header, BinaryMessageSelectorToken leftEnclosingToken, KeywordMessage primitiveSpecicationExpression, BinaryMessageSelectorToken rightEnclosingToken, ExecutableCode body, int occurrenceIndex) 
				: base(header, body, occurrenceIndex) {
			this.leftEnclosingToken = leftEnclosingToken;
			this.primitiveSpecicationExpression = primitiveSpecicationExpression;
			this.rightEnclosingToken = rightEnclosingToken;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.PrimitiveMethodDeclaration;}
		}

		public BinaryMessageSelectorToken LeftEnclosingToken {
			get {return leftEnclosingToken;}
		}

		public KeywordMessage PrimitiveSpecification {
			get {return primitiveSpecicationExpression;}
		}

		public BinaryMessageSelectorToken RightEnclosingToken {
			get {return rightEnclosingToken;}
		}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (header != null) {
				newLine(depth);
				header.printUsing(depth, append, newLine);
			}
			if (leftEnclosingToken != null) {
				newLine(depth);
				leftEnclosingToken.printUsing(depth, append, newLine);
			}
			if (primitiveSpecicationExpression != null) {
				newLine(depth);
				primitiveSpecicationExpression.printUsing(depth, append, newLine);
			}
			if (rightEnclosingToken != null) {
				newLine(depth);
				rightEnclosingToken.printUsing(depth, append, newLine);
			}
			if (body != null) {
				newLine(depth);
				body.printUsing(depth, append, newLine);
			}
			
		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (header != null) {
				header.esPrintUsing(depth, append, newLine);
			}
			newLine(depth);
			if (leftEnclosingToken != null) {
				leftEnclosingToken.esPrintUsing(depth, append, newLine);
			}
			if (primitiveSpecicationExpression != null) {
				primitiveSpecicationExpression.esPrintUsing(depth, append, newLine);
			}
			if (rightEnclosingToken != null) {
				rightEnclosingToken.esPrintUsing(depth, append, newLine);
			}
			newLine(depth);
			if (body != null) {
				newLine(depth);
				body.esPrintUsing(depth, append, newLine);
			}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToPrimitiveMethodDeclaration(this);

		}
		
	}
	
	public abstract class MethodHeader : NonTerminalParseTreeNode {
		
		protected DeclarableIdentifierToken	classNameToken;
		protected SymbolLiteralToken		classNameSymbol;
		protected SymbolLiteralToken		selectorSymbol;

		protected MethodHeader(DeclarableIdentifierToken classNameToken, int occurrenceIndex) : base(occurrenceIndex) {
			this.classNameToken = classNameToken;
			if (classNameToken != null) { 
				classNameSymbol = new SymbolLiteralToken(
							(char)0, 
							classNameToken.Name, 
							SymbolType.Identifier, 
							1, 
							null, 
							1, 
							classNameToken.OccurrenceIndex, 
							classNameToken.LineNumberStart, 
							classNameToken.ColumnNumberStart, 
							classNameToken.LineNumberEnd, 
							classNameToken.ColumnNumberEnd);
			}
		}

		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.MethodHeader;}
		}

		public override bool IsMethodHeader {
			get {return true;}
		}

		public bool SpecifiesClassName {
			get {return classNameToken != null;}
		}
		
		public DeclarableIdentifierToken ClassNameToken {
			get {return classNameToken;}
		}
		
		public SymbolLiteralToken ClassNameSymbol {
			get {return classNameSymbol;}
		}
		
		public SymbolLiteralToken SelectorSymbol {
			get {return selectorSymbol;}
		}
		
		public String SelectorString {
			get {return SelectorSymbol.StringValue;}
		}

		public abstract uint NumArgs {
			get;
		}

		public abstract void parametersDo(Action<DeclarableIdentifierToken> enumerator1);

		public abstract void parameterNamesDo(Action<String> enumerator1);

	}
	
	public class UnaryMethodHeader : MethodHeader {
		protected DeclarableIdentifierToken selectorToken = null;
		
		public UnaryMethodHeader(DeclarableIdentifierToken classNameToken, DeclarableIdentifierToken selectorToken, int occurrenceIndex) : base(classNameToken, occurrenceIndex) {
			this.selectorToken = selectorToken;
			selectorSymbol = new SymbolLiteralToken((char)0, selectorToken.Name, SymbolType.Identifier, 0, null, 1, selectorToken.OccurrenceIndex, selectorToken.LineNumberStart, selectorToken.ColumnNumberStart, selectorToken.LineNumberEnd, selectorToken.ColumnNumberEnd);
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.UnaryMethodHeader;}
		}

		public override LexicalToken InitialToken {
			get {return classNameToken ?? selectorToken;}
		}

		public override LexicalToken FinalToken {
			get {return selectorToken ?? classNameToken;}
		}

		public DeclarableIdentifierToken SelectorToken {
			get {return selectorToken;}
		}
		
		public override bool IsMethodHeader {
			get {return true;}
		}

		public override uint NumArgs {
			get {return 0;}
		}

		public override void parametersDo(Action<DeclarableIdentifierToken> enumerator1) {
		}

		public override void parameterNamesDo(Action<String> enumerator1) {
		}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (selectorToken != null) {
				newLine(depth);
				selectorToken.printUsing(depth, append, newLine);
			}
			
		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (selectorToken != null) {
				selectorToken.esPrintUsing(depth, append, newLine);
				newLine(depth);
			}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToUnaryMethodHeader(this);

		}
		
	}
	
	public class BinaryMethodHeader : MethodHeader {
		protected BinaryMessageSelectorToken selectorToken = null;
		protected DeclarableIdentifierToken parameter = null;
		
		public BinaryMethodHeader(DeclarableIdentifierToken classNameToken, BinaryMessageSelectorToken selectorToken, DeclarableIdentifierToken parameter, int occurrenceIndex) : base(classNameToken, occurrenceIndex) {
			this.selectorToken = selectorToken;
			this.parameter = parameter;
			selectorSymbol = new SymbolLiteralToken(
						(char)0, 
						selectorToken.Name, 
						SymbolType.BinaryMessageSelector, 
						1, 
						null, 
						1, 
						occurrenceIndex, 
						selectorToken.LineNumberStart, 
						selectorToken.ColumnNumberStart, 
						selectorToken.LineNumberEnd, 
						selectorToken.ColumnNumberEnd);
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.BinaryMethodHeader;}
		}
			
		public override LexicalToken InitialToken {
			get {return (LexicalToken)classNameToken ?? selectorToken;}
		}

		public override LexicalToken FinalToken {
			get {return Parameter;}
		}

		public BinaryMessageSelectorToken SelectorToken {
			get {return selectorToken;}
		}

		public DeclarableIdentifierToken Parameter {
			get {return parameter;}
		}

		public override uint NumArgs {
			get {return 1;}
		}

		public override void parametersDo(Action<DeclarableIdentifierToken> enumerator1) {
			enumerator1(Parameter);
		}

		public override void parameterNamesDo(Action<String> enumerator1) {
			enumerator1(Parameter.Name);
		}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (selectorToken != null) {
				newLine(depth);
				selectorToken.printUsing(depth, append, newLine);
			}
			if (parameter != null) {
				newLine(depth);
				parameter.printUsing(depth, append, newLine);
			}
			
		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (selectorToken != null) {
				selectorToken.esPrintUsing(depth, append, newLine);
			}
			if (parameter != null) {
				append(" ");
				parameter.esPrintUsing(depth, append, newLine);
			}
			newLine(depth);
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToBinaryMethodHeader(this);
		}
		
	}
	
	public class KeywordMethodHeader : MethodHeader {

		protected KeywordMethodHeaderSegment[] segments = null;
		
		public KeywordMethodHeader(DeclarableIdentifierToken classNameToken, KeywordMethodHeaderSegment[] segments, int occurrenceIndex) : base(classNameToken, occurrenceIndex) {
			this.segments = segments;
			selectorSymbol = computeSelectorSymbol();
		}
		
		public KeywordMethodHeader(DeclarableIdentifierToken classNameToken, List<KeywordMethodHeaderSegment> segments, int occurrenceIndex) : this(classNameToken, segments.ToArray(), occurrenceIndex) {
		}
			
		public override LexicalToken InitialToken {
			get {return classNameToken ?? segments[0].InitialToken;}
		}

		public override LexicalToken FinalToken {
			get {return segments[NumArgs - 1].FinalToken;}
		}

		protected SymbolLiteralToken computeSelectorSymbol() {
			uint selectorLineNumberStart = 0;
			uint selectorColumnNumberStart = 0;
			uint selectorLineNumberEnd = 0;
			uint selectorColumnNumberEnd = 0;
			var sb = new StringBuilder();
			for (var i = 0; i < segments.Length; i++) {
				var keyword = segments[i].Keyword;
				sb.Append(keyword.Name);
				if (i == 0) {
					selectorLineNumberStart = keyword.LineNumberStart;
					selectorColumnNumberStart = keyword.ColumnNumberStart;
				}
				selectorLineNumberEnd = keyword.LineNumberEnd;
				selectorColumnNumberEnd = keyword.ColumnNumberEnd;
			}
			return new SymbolLiteralToken(
					(char)0, 
					sb.ToString(), 
					SymbolType.Keyword, 
					NumArgs, 
					null, 
					1, 
					occurrenceIndex,
					selectorLineNumberStart, 
					selectorColumnNumberStart, 
					selectorLineNumberEnd, 
					selectorColumnNumberEnd);

		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.KeywordMethodHeader;}
		}

		public override uint NumArgs {
			get {return (uint)segments.Length;}
		}
		
		public void segmentsDo(Action<KeywordMethodHeaderSegment> enumerator1) {
			if (segments == null) return;
			foreach (var segment in segments) {
				enumerator1(segment);
			}
		}

		public override void parametersDo(Action<DeclarableIdentifierToken> enumerator1) {
			segmentsDo(segment => enumerator1(segment.Parameter));
		}

		public override void parameterNamesDo(Action<String> enumerator1) {
			parametersDo(parameter => enumerator1(parameter.Name));
		}

		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (segments != null) {
				printUsing(segments, depth, append, newLine);
			}
			
		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (segments != null) {
				esPrintUsing(segments, depth, append, newLine);
			}
			newLine(depth);
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToKeywordMethodHeader(this);
		}
		
	}
	
	public class KeywordMethodHeaderSegment : NonTerminalParseTreeNode {
		protected KeywordToken keyword = null;
		protected DeclarableIdentifierToken parameter = null;
		
		public KeywordMethodHeaderSegment(KeywordToken keyword, DeclarableIdentifierToken parameter, int occurrenceIndex) : base(occurrenceIndex) {
			this.keyword = keyword;
			this.parameter = parameter;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.KeywordMethodHeaderSegment;}
		}
			
		public override LexicalToken InitialToken {
			get {return Keyword;}
		}

		public override LexicalToken FinalToken {
			get {return Parameter;}
		}

		public KeywordToken Keyword {
			get {return keyword;}
		}

		public DeclarableIdentifierToken Parameter {
			get {return parameter;}
		}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (keyword != null) {
				newLine(depth);
				keyword.printUsing(depth, append, newLine);
			}
			if (parameter != null) {
				newLine(depth);
				parameter.printUsing(depth, append, newLine);
			}
			
		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (keyword != null) {
				keyword.esPrintUsing(depth, append, newLine);
			}
			if (parameter != null) {
				append(" ");
				parameter.esPrintUsing(depth, append, newLine);
			}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToKeywordMethodHeaderSegment(this);
		}

	}
	
	public class BlockDeclaration : RootParseTreeNode {
		protected BlockParameterDeclarationList parameterList = null;
		protected ExecutableCode body = null;
		
		public BlockDeclaration(BlockParameterDeclarationList parameterList, ExecutableCode body, int occurrenceIndex) : base(occurrenceIndex) {
			this.parameterList = parameterList;
			this.body = body;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.BlockDeclaration;}
		}
	
		public override bool IsBlockDeclaration {
			get {return true;}
		}

		public override LexicalToken InitialToken {
			get {	if (parameterList != null) {
					var token = parameterList.InitialToken;
					if (token != null) return token;
				}
				if (body != null) {
					var token = body.InitialToken;
					if (token != null) return token;
				}
				return null;}
		}

		public override LexicalToken FinalToken {
			get {	if (body != null) {
					var token = body.FinalToken;
					if (token != null) return token;
				}
				if (parameterList != null) {
					var token = parameterList.FinalToken;
					if (token != null) return token;
				}
				return null;}
		}
		
		public BlockParameterDeclarationList ParameterList {
			get {return parameterList;}
		}
		
		public bool HasBody {
			get {return body != null;}
		}

		public ExecutableCode Body {
			get {return body;}
		}

		public void parametersDo(Action<BlockParameterToken> enumerator1) {
			if (parameterList == null) return;
			parameterList.parametersDo(enumerator1);
		}

		public void parameterNamesDo(Action<String> enumerator1) {
			if (parameterList == null) return;
			parameterList.parameterNamesDo(enumerator1);
		}
		
		public void variablesDo(Action<DeclarableIdentifierToken> enumerator1) {
			if (body == null) return;
			body.variablesDo(enumerator1);
		}

		public void variableNamesDo(Action<String> enumerator1) {
			if (body == null) return;
			body.variableNamesDo(enumerator1);
		}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (parameterList != null) {
				newLine(depth);
				parameterList.printUsing(depth, append, newLine);
			}
			if (body != null) {
				newLine(depth);
				body.printUsing(depth, append, newLine);
			}
			
		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (parameterList != null) {
				parameterList.esPrintUsing(depth, append, newLine);
			}
			if (body != null) {
				body.esPrintUsing(depth, append, newLine);
			}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToBlockDeclaration(this);
		}
		
	}
	
	public class ExecutableCode : NonTerminalParseTreeNode {
		protected LocalVariableDeclarationList localVariables = null;
		protected Statement[] statements = null;
		protected FinalStatement finalStatement = null;
		
		public ExecutableCode(LocalVariableDeclarationList localVariables, Statement[] statements, FinalStatement finalStatement, int occurrenceIndex): base(occurrenceIndex) {
			this.localVariables = localVariables;
			this.statements = statements;
			this.finalStatement = finalStatement;
		}
		
		public ExecutableCode(LocalVariableDeclarationList localVariables, List<Statement> statements, FinalStatement finalStatement, int occurrenceIndex) 
			: this(localVariables, statements == null ? null : statements.ToArray(), finalStatement, occurrenceIndex) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.ExecutableCode;}
		}
	
		public override bool IsExecutableCode {
			get {return true;}
		}
	
		public override LexicalToken InitialToken {
			get {	if (localVariables != null) return localVariables.InitialToken;
				if (statements != null && statements.Length > 0) return statements[0].InitialToken;
				if (HasFinalStatement) return finalStatement.InitialToken;
				return null;}
		}

		public override LexicalToken FinalToken {
			get {	if (HasFinalStatement) return finalStatement.FinalToken;
				if (statements != null && statements.Length > 0) return statements[statements.Length - 1].FinalToken;
				if (localVariables != null) return localVariables.FinalToken;
				return null;}
		}

		public LocalVariableDeclarationList LocalVariables {
			get {return localVariables;}
		}

		public void variablesDo(Action<DeclarableIdentifierToken> enumerator1) {
			if (localVariables == null) return;
			localVariables.variablesDo(enumerator1);
		}

		public void variableNamesDo(Action<String> enumerator1) {
			if (localVariables == null) return;
			localVariables.variableNamesDo(enumerator1);
		}

		public Statement[] Statements {
			get {return statements;}
		}

		public FinalStatement FinalStatement {
			get {return finalStatement;}
		}
		
		public bool HasFinalStatement {
			get {return finalStatement != null;}
		}

		public void statementsDo(Action<Statement> enumerator1) {
			if (statements != null) {
				foreach (var statement in statements) enumerator1(statement);
			}
			if (finalStatement != null) enumerator1(finalStatement);
		}

		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (localVariables != null) {
				newLine(depth);
				localVariables.printUsing(depth, append, newLine);
			}
			if (statements != null) {
				printUsing(statements, depth, append, newLine);
			}
			if (finalStatement != null) {
				newLine(depth);
				finalStatement.printUsing(depth, append, newLine);
			}
			
		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (localVariables != null) {
				localVariables.esPrintUsing(depth, append, newLine);
			}
			if (statements != null) {
				esPrintUsing(statements, depth, append, newLine);
			}
			if (finalStatement != null) {
				finalStatement.esPrintUsing(depth, append, newLine);
			}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToExecutableCode(this);
		}

	}
	
	public abstract class NamedReferenceDeclarationSet : NonTerminalParseTreeNode {

		protected NamedReferenceDeclarationSet(int occurrenceIndex) : base(occurrenceIndex) {
		}

	}
	
	public class LocalVariableDeclarationList : NamedReferenceDeclarationSet {
		protected VerticalBarToken listBegin = null;
		protected DeclarableIdentifierToken[] variables = null;
		protected VerticalBarToken listEnd = null;
		
		public LocalVariableDeclarationList(VerticalBarToken listBegin, DeclarableIdentifierToken[] variables, VerticalBarToken listEnd, int occurrenceIndex) : base(occurrenceIndex) {
			this.listBegin = listBegin;
			this.variables = variables;
			this.listEnd = listEnd;
		}
		
		public LocalVariableDeclarationList(VerticalBarToken listBegin, List<DeclarableIdentifierToken> variables, VerticalBarToken listEnd, int occurrenceIndex) 
			: this(listBegin, variables == null ? null : variables.ToArray(), listEnd, occurrenceIndex) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.LocalVariableDeclarationList;}
		}

		public override LexicalToken FinalToken {
			get {	if (listEnd != null) return listEnd;
				if (variables != null && variables.Length > 0) return variables[variables.Length - 1];
				if (listBegin != null) return listBegin;
				return null;}
		}
	
		public override LexicalToken InitialToken {
			get {return ListBegin;}
		}

		public VerticalBarToken ListBegin {
			get {return listBegin;}
		}

		public DeclarableIdentifierToken[] Variables {
			get {return variables;}
		}

		public VerticalBarToken ListEnd {
			get {return listEnd;}
		}
		
		public void variablesDo(Action<DeclarableIdentifierToken> enumerator1) {
			if (variables == null) return;
			foreach (var v in variables) enumerator1(v);
		}

		public void variableNamesDo(Action<String> enumerator1) {
			variablesDo(variable => enumerator1(variable.Name));
		}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (listBegin != null) {
				newLine(depth);
				listBegin.printUsing(depth, append, newLine);
			}
			if (variables != null) {
				printUsing(variables, depth, append, newLine);
			}
			if (listEnd != null) {
				newLine(depth);
				listEnd.printUsing(depth, append, newLine);
			}
			
		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			newLine(depth);
			if (listBegin != null) {
				listBegin.esPrintUsing(depth, append, newLine);
				append(" ");
			}
			if (variables != null) {
				esPrintUsing(variables, depth, append, newLine);
			}
			if (listEnd != null) {
				listEnd.esPrintUsing(depth, append, newLine);
			}
			newLine(depth);
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToLocalVariableDeclarationList(this);
		}

	}
	
	public class BlockParameterDeclarationList : NamedReferenceDeclarationSet {
		protected BlockParameterToken[] parameters = null;
		protected VerticalBarToken listEnd = null;
		
		public BlockParameterDeclarationList(BlockParameterToken[] parameters, VerticalBarToken listEnd, int occurrenceIndex) : base(occurrenceIndex) {
			this.parameters = parameters;
			this.listEnd = listEnd;
		}
		
		public BlockParameterDeclarationList(List<BlockParameterToken> parameters, VerticalBarToken listEnd, int occurrenceIndex) 
			: this(parameters == null ? null : parameters.ToArray(), listEnd, occurrenceIndex) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.BlockParameterDeclarationList;}
		}
	
		public override LexicalToken InitialToken {
			get {	if (parameters != null && parameters.Length > 0) return parameters[0];
				return listEnd == null ? null : listEnd;}
		}
	
		public override LexicalToken FinalToken {
			get {	if (listEnd != null) return listEnd;
				if (parameters != null && parameters.Length > 0) return parameters[parameters.Length - 1];
				return null;}
		}

		public VerticalBarToken ListEnd {
			get {return listEnd;}
		}
		
		public void parametersDo(Action<BlockParameterToken> enumerator1) {
			if (parameters == null) return;
			foreach (var p in parameters) enumerator1(p);
		}

		public void parameterNamesDo(Action<String> enumerator1) {
			parametersDo(parameter => enumerator1(parameter.Name));
		}

		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (parameters != null) {
				printUsing(parameters, depth, append, newLine);
			}
			if (listEnd != null) {
				newLine(depth);
				listEnd.printUsing(depth, append, newLine);
			}
			
		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (parameters != null) {
				esPrintUsing(parameters, depth, append, newLine);
			}
			if (listEnd != null) {
				listEnd.esPrintUsing(depth, append, newLine);
				append(" ");
			}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToBlockParameterDeclarationList(this);
		}

	}
	
	public class Statement : NonTerminalParseTreeNode {
		protected AssignmentPrefix[] assignmentPrefixes = null;
		protected Expression expression = null;
		protected StatementEndToken statementEndOp = null;
		
		public Statement(AssignmentPrefix[] assignmentPrefixes, Expression expression, StatementEndToken statementEndOp, int occurrenceIndex) : base(occurrenceIndex) {
			this.assignmentPrefixes = assignmentPrefixes == null ? null : (assignmentPrefixes.Length > 0 ? assignmentPrefixes : null);
			this.expression = expression;
			this.statementEndOp = statementEndOp;
		}
		
		public Statement(List<AssignmentPrefix> assignmentPrefixes, Expression expression, StatementEndToken statementEndOp, int occurrenceIndex) 
			: this(assignmentPrefixes == null ? null : assignmentPrefixes.ToArray(), expression, statementEndOp, occurrenceIndex) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.Statement;}
		}
	
		public override bool IsStatement {
			get {return true;}
		}
	
		public override LexicalToken InitialToken {
			get {	if (assignmentPrefixes != null && assignmentPrefixes.Length > 0) return assignmentPrefixes[0].InitialToken;
				if (expression != null) return expression.InitialToken;
				return statementEndOp == null ? null : statementEndOp;}
		}
	
		public override LexicalToken FinalToken {
			get {	if (statementEndOp != null) return statementEndOp;
				if (expression != null) return expression.FinalToken;
				if (assignmentPrefixes != null && assignmentPrefixes.Length > 0) return assignmentPrefixes[assignmentPrefixes.Length - 1].InitialToken;
				return null;}
		}

		public AssignmentPrefix[] AssignmentPrefixes {
			get {return assignmentPrefixes;}
		}

		public Expression Expression {
			get {return expression;}
		}

		public StatementEndToken StatementEndOp {
			get {return statementEndOp;}
		}
		
		public bool IsAssignmentStatement {
			get {return assignmentPrefixes != null && assignmentPrefixes.Length > 0;}
		}
		
		public virtual bool IsReturnStatement {
			get {return false;}
		}
		
		public void assignmentPrefixesDo(Action<AssignmentPrefix> enumerator1) {
			if (assignmentPrefixes == null) return;
			foreach (var assignment in assignmentPrefixes) enumerator1(assignment);
		}

		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (assignmentPrefixes != null) {
				printUsing(assignmentPrefixes, depth, append, newLine);
			}
			if (expression != null) {
				newLine(depth);
				expression.printUsing(depth, append, newLine);
			}
			if (statementEndOp != null) {
				newLine(depth);
				statementEndOp.printUsing(depth, append, newLine);
			}
		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (assignmentPrefixes != null) {
				esPrintUsing(assignmentPrefixes, depth, append, newLine);
			}
			if (expression != null) {
				expression.esPrintUsing(depth, append, newLine);
			}
			if (statementEndOp != null) {
				statementEndOp.esPrintUsing(depth, append, newLine);
			}
			newLine(depth);
		}


		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToStatement(this);

		}

	}
	
	public class FinalStatement : Statement {
		protected MethodReturnPrefix methodReturnPrefix = null;
		
		public FinalStatement(AssignmentPrefix[] assignmentPrefixes, Expression expression, StatementEndToken statementEndOp, int occurrenceIndex) 
			: base(assignmentPrefixes, expression, statementEndOp, occurrenceIndex) {
		}
		
		public FinalStatement(MethodReturnPrefix methodReturnPrefix, AssignmentPrefix[] assignmentPrefixes, Expression expression, StatementEndToken statementEndOp, int occurrenceIndex) 
			: base(assignmentPrefixes, expression, statementEndOp, occurrenceIndex) {
			this.methodReturnPrefix = methodReturnPrefix;
		}
		
		public FinalStatement(List<AssignmentPrefix> assignmentPrefix, Expression expression, StatementEndToken statementEndOp, int occurrenceIndex) 
			: base(assignmentPrefix, expression, statementEndOp, occurrenceIndex) {
		}
		
		public FinalStatement(MethodReturnPrefix methodReturnPrefix, List<AssignmentPrefix> assignmentPrefixes, Expression expression, StatementEndToken statementEndOp, int occurrenceIndex) 
			: this(methodReturnPrefix, assignmentPrefixes.ToArray(), expression, statementEndOp, occurrenceIndex) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.FinalStatement;}
		}
	
		public override LexicalToken InitialToken {
			get {return methodReturnPrefix == null ? base.InitialToken : methodReturnPrefix.InitialToken;}
		}

		public MethodReturnPrefix MethodReturnPrefix {
			get {return methodReturnPrefix;}
		}
		
		public override bool IsReturnStatement {
			get {return methodReturnPrefix != null;}
		}

		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (methodReturnPrefix != null) {
				newLine(depth);
				methodReturnPrefix.printUsing(depth, append, newLine);
			}
			base. printElementsUsing(depth, append, newLine);
		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (methodReturnPrefix != null) {
				methodReturnPrefix.esPrintUsing(depth, append, newLine);
			}
			base. esPrintElementsUsing(depth, append, newLine);
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToFinalStatement(this);
		}
		
	}
	
	public abstract class StatementElement : NonTerminalParseTreeNode {

		protected StatementElement(int occurrenceIndex) : base(occurrenceIndex) {
		}

	}
	
	public class MethodReturnPrefix : StatementElement {
		protected ReturnOpToken returnOp = null;
		
		public MethodReturnPrefix(ReturnOpToken returnOp, int occurrenceIndex) : base(occurrenceIndex) {
			this.returnOp = returnOp;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.MethodReturnPrefix;}
		}
	
		public override LexicalToken InitialToken {
			get {return returnOp == null ? null : returnOp;}
		}
	
		public override LexicalToken FinalToken {
			get {return returnOp == null ? null : returnOp;}
		}

		public ReturnOpToken ReturnOp {
			get {return returnOp;}
		}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (returnOp != null) {
				newLine(depth);
				returnOp.printUsing(depth, append, newLine);
			}
		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (returnOp != null) {
				returnOp.esPrintUsing(depth, append, newLine);
			}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToMethodReturnPrefix(this);
		}
		
	}
	
	public class AssignmentPrefix : StatementElement {
		protected VariableAssignment variableAssignment = null;
		protected AssignOpToken assignOp = null;
		
		public AssignmentPrefix(VariableAssignment variableAssignment, AssignOpToken assignOp, int occurrenceIndex) : base(occurrenceIndex) {
			this.variableAssignment = variableAssignment;
			this.assignOp = assignOp;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.AssignmentPrefix;}
		}
	
		public override LexicalToken InitialToken {
			get {return variableAssignment == null ? assignOp : variableAssignment.InitialToken;}
		}
	
		public override LexicalToken FinalToken {
			get {return assignOp == null ? variableAssignment.FinalToken : assignOp;}
		}

		public VariableAssignment VariableAssignment {
			get {return variableAssignment;}
		}
		
		public String VariableName {
			get {return variableAssignment.BindingName;}
		}

		public AssignOpToken AssignOp {
			get {return assignOp;}
		}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (variableAssignment != null) {
				newLine(depth);
				variableAssignment.printUsing(depth, append, newLine);
			}
			if (assignOp != null) {
				newLine(depth);
				assignOp.printUsing(depth, append, newLine);
			}

		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (variableAssignment != null) {
				variableAssignment.esPrintUsing(depth, append, newLine);
			}
			if (assignOp != null) {
				append(" ");
				assignOp.esPrintUsing(depth, append, newLine);
				append(" ");
			}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToAssignmentPrefix(this);
		}
		
	}
	
	public class VariableAssignment : StatementElement {
		protected DeclarableIdentifierToken bindableIdentifier = null;
		
		public VariableAssignment(DeclarableIdentifierToken bindableIdentifier, int occurrenceIndex) : base(occurrenceIndex) {
			this.bindableIdentifier = bindableIdentifier;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.VariableAssignment;}
		}
	
		public override LexicalToken InitialToken {
			get {return bindableIdentifier;}
		}
	
		public override LexicalToken FinalToken {
			get {return bindableIdentifier;}
		}
		
		public DeclarableIdentifierToken BindableIdentifier {
			get {return bindableIdentifier;}
		}
		
		public String BindingName {
			get {return bindableIdentifier.Name;}
		}

		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (bindableIdentifier != null) {
				newLine(depth);
				bindableIdentifier.printUsing(depth, append, newLine);
			}

		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (bindableIdentifier != null) {
				bindableIdentifier.esPrintUsing(depth, append, newLine);
			}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToVariableAssignment(this);

		}
		
	}	
	
	public class Expression : StatementElement {
		protected Operand operand = null;
		protected MessageChain messageChain = null;
		protected CascadedMessageChain[] cascadedMessagesChain = null;
		
		public Expression(Operand operand, MessageChain messageChain, CascadedMessageChain[] cascadedMessagesChain, int occurrenceIndex) : base(occurrenceIndex) {
			this.operand = operand;
			this.messageChain = messageChain;
			this.cascadedMessagesChain = cascadedMessagesChain == null ? null : (cascadedMessagesChain.Length > 0 ? cascadedMessagesChain : null);
		}
		
		public Expression(Operand operand, MessageChain messageChain, List<CascadedMessageChain> cascadedMessages, int occurrenceIndex) 
			: this(operand, messageChain, cascadedMessages == null ? null : cascadedMessages.ToArray(), occurrenceIndex) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.Expression;}
		}
	
		public override bool IsExpression {
			get {return true;}
		}
	
		public override LexicalToken InitialToken {
			get {	if (operand != null) return operand.InitialToken;
				if (messageChain != null) return messageChain.InitialToken;
				if (cascadedMessagesChain != null && cascadedMessagesChain.Length > 0) return cascadedMessagesChain[0].InitialToken;
				return null;}
		}
	
		public override LexicalToken FinalToken {
			get {	if (cascadedMessagesChain != null && cascadedMessagesChain.Length > 0) return cascadedMessagesChain[cascadedMessagesChain.Length - 1].FinalToken;
				if (messageChain != null) return messageChain.FinalToken;
				if (operand != null) return operand.FinalToken;
				return null;}
		}

		public Operand Operand {
			get {return operand;}
		}

		public MessageChain MessageChain {
			get {return messageChain;}
		}

		public bool HasMessageChain {
			get {return messageChain != null;}
		}

		public void messagesDo(Action<Message> enumerator1) {
			if (HasMessageChain) MessageChain.messagesDo(enumerator1);
		}

		public int CascadedMessageCount {
			get {return cascadedMessagesChain == null ? 0 : cascadedMessagesChain.Length;}
		}
		
		public void cascadedMessageChainsDo(Action<CascadedMessageChain> enumerator1) {
			if (cascadedMessagesChain == null) return;
			foreach (var message in cascadedMessagesChain) enumerator1(message);
		}

		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (operand != null) {
				newLine(depth);
				operand.printUsing(depth, append, newLine);
			}
			if (messageChain != null) {
				newLine(depth);
				messageChain.printUsing(depth, append, newLine);
			}
			if (cascadedMessagesChain != null) {
				printUsing(cascadedMessagesChain, depth, append, newLine);
			}

		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (operand != null) {
				operand.esPrintUsing(depth, append, newLine);
			}
			if (messageChain != null) {
				messageChain.esPrintUsing(depth, append, newLine);
			}
			if (cascadedMessagesChain != null) {
				esPrintUsing(cascadedMessagesChain, depth, append, newLine);
			}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToExpression(this);
		}
		
	}
	
	public abstract class ExpressionElement : NonTerminalParseTreeNode {

		protected ExpressionElement(int occurrenceIndex) : base(occurrenceIndex) {
		}
		
	}
	
	public class CascadedMessageChain : ExpressionElement {
		protected MessageCascadeOpToken cascadeOpToken = null;
		protected MessageChain messageChain = null;
		
		public CascadedMessageChain(MessageCascadeOpToken cascadeOpToken, MessageChain messageChain, int occurrenceIndex) : base(occurrenceIndex) {
			this.cascadeOpToken = cascadeOpToken;
			this.messageChain = messageChain;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.CascadedMessage;}
		}
	
		public override LexicalToken InitialToken {
			get {	if (cascadeOpToken != null) return cascadeOpToken;
				if (messageChain != null) return messageChain.InitialToken;
				return null;}
		}
	
		public override LexicalToken FinalToken {
			get {	if (messageChain != null) return messageChain.FinalToken;
				if (cascadeOpToken != null) return cascadeOpToken;
				return null;}
		}

		public MessageCascadeOpToken CascadeOpToken {
			get {return cascadeOpToken;}
		}

		public MessageChain MessageChain {
			get {return messageChain;}
		}

		public bool HasMessageChain {
			get {return messageChain != null;}
		}

		public void messagesDo(Action<Message> enumerator1) {
			if (HasMessageChain) MessageChain.messagesDo(enumerator1);
		}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (cascadeOpToken != null) {
				newLine(depth);
				cascadeOpToken.printUsing(depth, append, newLine);
			}
			if (messageChain != null) {
				newLine(depth);
				messageChain.printUsing(depth, append, newLine);
			}

		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (cascadeOpToken != null) {
				cascadeOpToken.esPrintUsing(depth, append, newLine);
			}
			if (messageChain != null) {
				messageChain.esPrintUsing(depth, append, newLine);
			}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToCascadedMessage(this);
		}
		
	}
	
	public abstract class MessageChain : ExpressionElement {
		protected KeywordMessage keywordMessage = null;
		
		protected MessageChain(int occurrenceIndex) : base(occurrenceIndex) {
		}

		public MessageChain(KeywordMessage keywordMessage, int occurrenceIndex) : base(occurrenceIndex) {
			this.keywordMessage = keywordMessage;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.MessageChain;}
		}
	
		public override LexicalToken InitialToken {
			get {	if (keywordMessage != null) return keywordMessage.InitialToken;
				return null;}
		}
	
		public override LexicalToken FinalToken {
			get {	if (keywordMessage != null) return keywordMessage.FinalToken;
				return null;}
		}

		public bool HasKeywordMessage {
			get {return keywordMessage != null;}
		}

		public KeywordMessage KeywordMessage {
			get {return keywordMessage;}
		}
		
		public abstract uint MessageCount {
			get;
		}

		public abstract void messagesDo(Action<Message> enumerator1);

		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (keywordMessage != null) {
				newLine(depth);
				keywordMessage.printUsing(depth, append, newLine);
			}

		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (keywordMessage != null) {
				keywordMessage.esPrintUsing(depth, append, newLine);
			}
		}

	}
	
	public class KeywordMessageChain : MessageChain {

		public KeywordMessageChain(KeywordMessage keywordMessage, int occurrenceIndex) : base(keywordMessage, occurrenceIndex) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.KeywordMessageChain;}
		}
		
		public override uint MessageCount {
			get {return 1;}
		}
		
		public override void messagesDo(Action<Message> enumerator1) {
			if (HasKeywordMessage) enumerator1(KeywordMessage);
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToKeywordMessageChain(this);
		}

	}
		
	public class InitiallyBinaryMessageChain : MessageChain {

		protected BinaryOnlyMessageChain binaryMessageChain = null;
		
		protected InitiallyBinaryMessageChain(int occurrenceIndex) : base(occurrenceIndex) {
		}
		
		protected InitiallyBinaryMessageChain(KeywordMessage keywordMessage, int occurrenceIndex) : base(keywordMessage, occurrenceIndex) {
		}

		public InitiallyBinaryMessageChain(BinaryOnlyMessageChain binaryMessageChain, int occurrenceIndex) : base(occurrenceIndex) {
			this.binaryMessageChain = binaryMessageChain;
		}

		public InitiallyBinaryMessageChain(BinaryOnlyMessageChain binaryMessageChain, KeywordMessage keywordMessage, int occurrenceIndex) : base(keywordMessage, occurrenceIndex) {
			this.binaryMessageChain = binaryMessageChain;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.InitiallyBinaryMessageChain;}
		}
	
		public override LexicalToken InitialToken {
			get {	if (binaryMessageChain != null) return binaryMessageChain.InitialToken;
				return base.InitialToken;}
		}
	
		public override LexicalToken FinalToken {
			get {	LexicalToken finalToken = base.FinalToken;
				if (finalToken != null) return finalToken;
				if (binaryMessageChain != null) return binaryMessageChain.FinalToken;
				return null;}
		}

		public bool HasBinaryMessageChain {
			get {return binaryMessageChain != null;}
		}

		public virtual bool HasMessageChain {
			get {return binaryMessageChain != null;}
		}

		public BinaryOnlyMessageChain BinaryOnlyMessageChain {
			get {return binaryMessageChain;}
		}
		
		public override uint MessageCount {
			get {return HasMessageChain ? BinaryOnlyMessageChain.MessageCount + 1 : 0;}
		}
		
		public override void messagesDo(Action<Message> enumerator1) {
			if (HasBinaryMessageChain) BinaryOnlyMessageChain.messagesDo(enumerator1);
			if (HasKeywordMessage) enumerator1(KeywordMessage);
		}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (binaryMessageChain != null) {
				newLine(depth);
				binaryMessageChain.printUsing(depth, append, newLine);
			}
			base.printElementsUsing(depth, append, newLine);

		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (binaryMessageChain != null) {
				newLine(depth);
				binaryMessageChain.esPrintUsing(depth, append, newLine);
			}
			base.esPrintElementsUsing(depth, append, newLine);
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToInitiallyBinaryMessageChain(this);
		}
		
	}
	
	public class InitiallyUnaryMessageChain : InitiallyBinaryMessageChain {
		protected UnaryOnlyMessageChain unaryMessageChain = null;
		
		public InitiallyUnaryMessageChain(UnaryOnlyMessageChain unaryMessageChain, int occurrenceIndex) : base(occurrenceIndex) {
			this.unaryMessageChain = unaryMessageChain;
		}
		
		public InitiallyUnaryMessageChain(UnaryOnlyMessageChain unaryMessageChain, KeywordMessage keywordMessage, int occurrenceIndex) 
				: base(keywordMessage, occurrenceIndex) {
			this.unaryMessageChain = unaryMessageChain;
		}

		public InitiallyUnaryMessageChain(UnaryOnlyMessageChain unaryMessageChain, BinaryOnlyMessageChain binaryMessageChain, int occurrenceIndex) 
				: base(binaryMessageChain, occurrenceIndex) {
			this.unaryMessageChain = unaryMessageChain;
		}

		public InitiallyUnaryMessageChain(UnaryOnlyMessageChain unaryMessageChain, BinaryOnlyMessageChain binaryMessageChain, KeywordMessage keywordMessage, int occurrenceIndex) 
				: base(binaryMessageChain, keywordMessage, occurrenceIndex) {
			this.unaryMessageChain = unaryMessageChain;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.InitiallyUnaryMessageChain;}
		}
	
		public override LexicalToken InitialToken {
			get {	if (unaryMessageChain != null) return unaryMessageChain.InitialToken;
				return base.InitialToken;}
		}
	
		public override LexicalToken FinalToken {
			get {	LexicalToken finalToken = base.FinalToken;
				if (finalToken != null) return finalToken;
				if (unaryMessageChain != null) return unaryMessageChain.FinalToken;
				return null;}
		}

		public override bool HasMessageChain {
			get {return unaryMessageChain != null || base.HasMessageChain;}
		}

		public bool HasUnaryMessageChain {
			get {return unaryMessageChain != null;}
		}

		public UnaryOnlyMessageChain UnaryOnlyMessageChain {
			get {return unaryMessageChain;}
		}
		
		public override uint MessageCount {
			get {return UnaryOnlyMessageChain.MessageCount + base.MessageCount;}
		}
		
		public override void messagesDo(Action<Message> enumerator1) {
			if (HasMessageChain) UnaryOnlyMessageChain.messagesDo(enumerator1);
			base.messagesDo(enumerator1);
		}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (unaryMessageChain != null) {
				newLine(depth);
				unaryMessageChain.printUsing(depth, append, newLine);
			}
			base.printElementsUsing(depth, append, newLine);

		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (unaryMessageChain != null) {
				newLine(depth);
				unaryMessageChain.esPrintUsing(depth, append, newLine);
			}
			base.esPrintElementsUsing(depth, append, newLine);
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToInitiallyUnaryMessageChain(this);
		}
		
	}
	
	public abstract class MessageChainElement : ExpressionElement {

		protected MessageChainElement(int occurrenceIndex) : base(occurrenceIndex) {
		}
		
	}
	
	public class UnaryOnlyMessageChain : MessageChainElement {
		protected UnaryMessage[] messages = null;
		
		public UnaryOnlyMessageChain(UnaryMessage[] messages, int occurrenceIndex) : base(occurrenceIndex) {
			this.messages = messages == null ? null : (messages.Length > 0 ? messages : null);
		}
		
		public UnaryOnlyMessageChain(List<UnaryMessage> messages, int occurrenceIndex) 
			: this(messages == null ? null : messages == null ? null : messages.ToArray(), occurrenceIndex) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.UnaryOnlyMessageChain;}
		}
	
		public override LexicalToken InitialToken {
			get {	if (messages != null && messages.Length > 0) return messages[0].InitialToken;
				return null;}
		}
	
		public override LexicalToken FinalToken {
			get {	if (messages != null && messages.Length > 0) return messages[messages.Length - 1].FinalToken;
				return null;}
		}
		
		public uint MessageCount {
			get {return messages == null ? 0 : (uint)messages.Length;}
		}
		
		public void messagesDo(Action<Message> enumerator1) {
			if (messages == null) return;
			foreach (var message in messages) enumerator1(message);
		}

		public override bool TransformsToEpsilon {
			get {return MessageCount < 1;}
		}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (messages != null) {
				printUsing(messages, depth, append, newLine);
			}

		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (messages != null) {
				esPrintUsing(messages, depth, append, newLine);
			}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToUnaryOnlyMessageChain(this);
		}
		
	}
	
	public class BinaryOnlyMessageChain : MessageChainElement {
		protected BinaryMessage[] messages = null;
		
		public BinaryOnlyMessageChain(BinaryMessage[] messages, int occurrenceIndex) : base(occurrenceIndex) {
			this.messages = messages == null ? null : (messages.Length > 0 ? messages : null);
		}
		
		public BinaryOnlyMessageChain(List<BinaryMessage> messages, int occurrenceIndex) 
			: this(messages == null ? null : messages.ToArray(), occurrenceIndex) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.BinaryOnlyMessageChain;}
		}
	
		public override LexicalToken InitialToken {
			get {	if (messages != null && messages.Length > 0) return messages[0].InitialToken;
				return null;}
		}
	
		public override LexicalToken FinalToken {
			get {	if (messages != null && messages.Length > 0) return messages[messages.Length - 1].FinalToken;
				return null;}
		}
		
		public uint MessageCount {
			get {return messages == null ? 0 : (uint)messages.Length;}
		}
		
		public void messagesDo(Action<Message> enumerator1) {
			if (messages == null) return;
			foreach (var message in messages) enumerator1(message);
		}
		
		public override bool TransformsToEpsilon {
			get {return MessageCount < 1;}
		}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (messages != null) {
				printUsing(messages, depth, append, newLine);
			}

		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (messages != null) {
				esPrintUsing(messages, depth, append, newLine);
			}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToBinaryOnlyMessageChain(this);
		}
		
	}
	
	public abstract class Message : MessageChainElement {
		protected SymbolLiteralToken selectorSymbol = null;

		protected Message(int occurrenceIndex) : base(occurrenceIndex) {
		}
		
		public abstract uint NumArgs {
			get;
		}

		public SymbolLiteralToken SelectorSymbol {
			get {return selectorSymbol;}
		}

	}
	
	public class UnaryMessage : Message {
		protected IdentifierToken selectorToken = null;
		
		public UnaryMessage(IdentifierToken selectorToken, int occurrenceIndex) : base(occurrenceIndex) {
			this.selectorToken = selectorToken;
			selectorSymbol = new SymbolLiteralToken(
				(char)0, 
				selectorToken.Name, 
				SymbolType.Identifier, 
				0, 
				null, 
				1, 
				occurrenceIndex,
				selectorToken.LineNumberStart, 
				selectorToken.ColumnNumberStart,
				selectorToken.LineNumberEnd, 
				selectorToken.ColumnNumberEnd);
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.UnaryMessage;}
		}
	
		public override LexicalToken InitialToken {
			get {return selectorToken;}
		}
	
		public override LexicalToken FinalToken {
			get {return selectorToken;}
		}

		public IdentifierToken SelectorToken {
			get {return selectorToken;}
		}

		public override uint NumArgs {
			get {return 0;}
		}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (selectorToken != null) {
				newLine(depth);
				selectorToken.printUsing(depth, append, newLine);
			}

		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (selectorToken != null) {
				selectorToken.esPrintUsing(depth, append, newLine);
			}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToUnaryMessage(this);

		}

	}
	
	public class BinaryMessage : Message {
		protected BinaryMessageSelectorToken selectorToken = null;
		protected BinaryMessageOperand operand = null;
		
		public BinaryMessage(BinaryMessageSelectorToken selectorToken, BinaryMessageOperand operand, int occurrenceIndex) : base(occurrenceIndex) {
			this.selectorToken = selectorToken;
			this.operand = operand;
			selectorSymbol = new SymbolLiteralToken(
						(char)0, 
						selectorToken.Name, 
						SymbolType.BinaryMessageSelector, 
						1, 
						null, 
						1, 
						selectorToken.OccurrenceIndex,
						selectorToken.LineNumberStart, 
						selectorToken.ColumnNumberStart,
						selectorToken.LineNumberEnd, 
						selectorToken.ColumnNumberEnd);
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.BinaryMessage;}
		}
	
		public override LexicalToken InitialToken {
			get {return selectorToken;}
		}
	
		public override LexicalToken FinalToken {
			get {return selectorToken;}
		}

		public BinaryMessageSelectorToken SelectorToken {
			get {return selectorToken;}
		}

		public override uint NumArgs {
			get {return 1;}
		}
		
		public BinaryMessageOperand Operand {
			get {return operand;}
		}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (selectorToken != null) {
				newLine(depth);
				selectorToken.printUsing(depth, append, newLine);
			}
			if (operand != null) {
				newLine(depth);
				operand.printUsing(depth, append, newLine);
			}

		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (selectorToken != null) {
				selectorToken.esPrintUsing(depth, append, newLine);
			}
			if (operand != null) {
				operand.esPrintUsing(depth, append, newLine);
			}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToBinaryMessage(this);
		}
				
	}
	
	public class KeywordMessage : Message {
		protected KeywordMessageSegment[] segments = null;
		
		public KeywordMessage(KeywordMessageSegment[] segments, int occurrenceIndex) : base(occurrenceIndex) {
			this.segments = segments == null ? null : (segments.Length > 0 ? segments : null);
			selectorSymbol = computeSelectorSymbol();
		}
		
		public KeywordMessage(List<KeywordMessageSegment> segments, int occurrenceIndex) 
			: this(segments == null ? null : segments.ToArray(), occurrenceIndex) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.KeywordMessage;}
		}

		protected SymbolLiteralToken computeSelectorSymbol() {
			uint selectorLineNumberStart = 0;
			uint selectorColumnNumberStart = 0;
			uint selectorLineNumberEnd = 0;
			uint selectorColumnNumberEnd = 0;
			var sb = new StringBuilder();
			for (var i = 0; i < segments.Length; i++) {
				var keyword = segments[i].Keyword;
				sb.Append(keyword.Name);
				if (i == 0) {
					selectorLineNumberStart = keyword.LineNumberStart;
					selectorColumnNumberStart = keyword.ColumnNumberStart;
				}
				selectorLineNumberEnd = keyword.LineNumberEnd;
				selectorColumnNumberEnd = keyword.ColumnNumberEnd;
			}
			return new SymbolLiteralToken(
				(char)0, 
				sb.ToString(), 
				SymbolType.Keyword, 
				NumArgs, 
				null, 
				1, 
				occurrenceIndex,
				selectorLineNumberStart, 
				selectorColumnNumberStart,
				selectorLineNumberEnd, 
				selectorColumnNumberEnd);

		}
	
		public override LexicalToken InitialToken {
			get {	if (segments != null && segments.Length > 0) return segments[0].InitialToken;
				return null;}
		}
	
		public override LexicalToken FinalToken {
			get {	if (segments != null && segments.Length > 0) return segments[segments.Length - 1].FinalToken;
				return null;}
		}
				
		public uint SegmentCount {
			get {return segments == null ? 0 : (uint)segments.Length;}
		}

		public override uint NumArgs {
			get {return SegmentCount;}
		}
		
		public void messageSegmentsDo(Action<KeywordMessageSegment> enumerator1) {
			if (segments == null) return;
			foreach (var segment in segments) enumerator1(segment);
		}
		
		public void messageArgumentsDo(Action<KeywordMessageArgument> enumerator1) {
			messageSegmentsDo(segment => enumerator1(segment.Argument));
		}

		public override bool TransformsToEpsilon {
			get {return SegmentCount < 1;}
		}
		
		public List<KeywordMessageArgument> Arguments {
			get {	var arguments = new List<KeywordMessageArgument>();
				messageArgumentsDo(delegate (KeywordMessageArgument argument) {arguments.Add(argument);});
				return arguments;}
		}

		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (segments != null) {
				printUsing(segments, depth, append, newLine);
			}

		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (segments != null) {
				esPrintUsing(segments, depth, append, newLine);
			}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToKeywordMessage(this);
		}
		
	}
	
	public abstract class MessageElement : MessageChainElement {

		protected MessageElement(int occurrenceIndex) : base(occurrenceIndex) {
		}
		
	}
	
	public class BinaryMessageOperand : MessageElement {
		protected Operand operand = null;
		protected UnaryOnlyMessageChain messageChain = null;
		
		public BinaryMessageOperand(Operand operand, int occurrenceIndex) : base(occurrenceIndex) {
			this.operand = operand;
		}
		
		public BinaryMessageOperand(Operand operand, UnaryOnlyMessageChain messageChain, int occurrenceIndex) : base(occurrenceIndex) {
			this.operand = operand;
			this.messageChain = messageChain;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.BinaryMessageOperand;}
		}
	
		public override LexicalToken InitialToken {
			get {	if (operand != null) return operand.InitialToken;
				if (messageChain != null) return messageChain.InitialToken;
				return null;}
		}
	
		public override LexicalToken FinalToken {
			get {	if (messageChain != null) return messageChain.FinalToken;
				if (operand != null) return operand.FinalToken;
				return null;}
		}
		
		public Operand Operand {
			get {return operand;}
		}
		
		public UnaryOnlyMessageChain MessageChain {
			get {return messageChain;}
		}
		
		public bool HasMessageChain {
			get {return messageChain != null;}
		}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (operand != null) {
				newLine(depth);
				operand.printUsing(depth, append, newLine);
			}
			if (messageChain != null) {
				newLine(depth);
				messageChain.printUsing(depth, append, newLine);
			}

		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (operand != null) {
				operand.esPrintUsing(depth, append, newLine);
			}
			if (messageChain != null) {
				messageChain.esPrintUsing(depth, append, newLine);
			}
		}

		public override String asPathString(char separator) {
			StringBuilder sb = new StringBuilder();
			if (operand != null) {
				sb.Append(operand.asPathString(separator));
			}
			if (messageChain != null) {
				sb.Append(separator);
				messageChain.messagesDo(message => sb.Append(message.asPathString(separator)));
			}
			return sb.ToString();
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToBinaryMessageOperand(this);
		}
		
	}
	
	public class KeywordMessageSegment : MessageElement {
		protected KeywordToken keyword = null;
		protected KeywordMessageArgument argument = null;
		
		public KeywordMessageSegment(KeywordToken keyword, KeywordMessageArgument argument, int occurrenceIndex) : base(occurrenceIndex) {
			this.keyword = keyword;
			this.argument = argument;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.KeywordMessageSegment;}
		}
	
		public override LexicalToken InitialToken {
			get {	if (keyword != null) return keyword;
				if (argument != null) return argument.InitialToken;
				return null;}
		}
	
		public override LexicalToken FinalToken {
			get {	if (argument != null) return argument.FinalToken;
				if (keyword != null) return keyword;
				return null;}
		}
		
		public KeywordToken Keyword {
			get {return keyword;}
		}
		
		public KeywordMessageArgument Argument {
			get {return argument;}
		}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (keyword != null) {
				newLine(depth);
				keyword.printUsing(depth, append, newLine);
			}
			if (argument != null) {
				newLine(depth);
				argument.printUsing(depth, append, newLine);
			}

		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (keyword != null) {
				keyword.esPrintUsing(depth, append, newLine);
			}
			append(" ");
			if (argument != null) {
				argument.esPrintUsing(depth, append, newLine);
			}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToKeywordMessageSegment(this);
		}
		
	}
	
	public class KeywordMessageArgument : MessageElement {
		protected BinaryMessageOperand operand = null;
		protected BinaryOnlyMessageChain messageChain = null;
		
		public KeywordMessageArgument(BinaryMessageOperand operand, int occurrenceIndex) : base(occurrenceIndex) {
			this.operand = operand;
		}
		
		public KeywordMessageArgument(BinaryMessageOperand operand, BinaryOnlyMessageChain messageChain, int occurrenceIndex) : base(occurrenceIndex) {
			this.operand = operand;
			this.messageChain = messageChain;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.KeywordMessageArgument;}
		}
	
		public override LexicalToken InitialToken {
			get {return operand.InitialToken;}
		}
	
		public override LexicalToken FinalToken {
			get {return operand.FinalToken;}
		}
		
		public BinaryMessageOperand Operand {
			get {return operand;}
		}
		
		public BinaryOnlyMessageChain MessageChain {
			get {return messageChain;}
		}
		
		public bool HasMessageChain {
			get {return messageChain != null;}
		}

		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (operand != null) {
				newLine(depth);
				operand.printUsing(depth, append, newLine);
			}
			if (messageChain != null) {
				newLine(depth);
				messageChain.printUsing(depth, append, newLine);
			}

		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (operand != null) {
				operand.esPrintUsing(depth, append, newLine);
			}
			if (messageChain != null) {
				append(" ");
				messageChain.esPrintUsing(depth, append, newLine);
			}
		}

		public override String asPathString(char separator) {
			StringBuilder sb = new StringBuilder();
			if (operand != null) {
				sb.Append(operand.asPathString(separator));
			}
			if (messageChain != null) {
				sb.Append(separator);
				messageChain.messagesDo(message => sb.Append(message.asPathString(separator)));
			}
			return sb.ToString();
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToKeywordMessageArgument(this);
		}
		
	}
	
	public abstract class Operand : MessageElement {
		
		protected Operand(int occurrenceIndex) : base(occurrenceIndex) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.Operand;}
		}
		
		public override bool IsOperand {
			get {return true;}
		}

	}
	
	public abstract class LiteralValue : Operand {

		protected LiteralValue(int occurrenceIndex) : base(occurrenceIndex) {
		}
		
	}
	
	public abstract class NamedReference : Operand {

		protected NamedReference(int occurrenceIndex) : base(occurrenceIndex) {
		}
		
		public abstract String BindingName {
			get;
		}
		
		public override bool IsNamedReference {
			get {return true;}
		}

	}
	
	public class ConstantReference : NamedReference {
		protected ConstantReferenceToken constant = null;
		
		public ConstantReference(ConstantReferenceToken constant, int occurrenceIndex) : base(occurrenceIndex) {
			this.constant = constant;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.ConstantReference;}
		}
	
		public override LexicalToken InitialToken {
			get {return constant;}
		}
	
		public override LexicalToken FinalToken {
			get {return constant;}
		}
		
		public ConstantReferenceToken Constant {
			get {return constant;}
		}

		public override String BindingName {
			get {return Constant.Name;}
		}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (constant != null) {
				newLine(depth);
				constant.printUsing(depth, append, newLine);
			}

		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (constant != null) {
				constant.esPrintUsing(depth, append, newLine);
			}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToConstantReference(this);
		}
		
	}
	
	public class PseudoVariableReference : NamedReference {
		protected PseudoVariableReferenceToken pseudoVariable = null;
		
		public PseudoVariableReference(PseudoVariableReferenceToken pseudoVariable, int occurrenceIndex) : base(occurrenceIndex) {
			this.pseudoVariable = pseudoVariable;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.PseudoVariableReference;}
		}
	
		public override LexicalToken InitialToken {
			get {return pseudoVariable;}
		}
	
		public override LexicalToken FinalToken {
			get {return pseudoVariable;}
		}
		
		public PseudoVariableReferenceToken PseudoVariable {
			get {return pseudoVariable;}
		}

		public override String BindingName {
			get {return PseudoVariable.Name;}
		}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (pseudoVariable != null) {
				newLine(depth);
				pseudoVariable.printUsing(depth, append, newLine);
			}

		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (pseudoVariable != null) {
				pseudoVariable.esPrintUsing(depth, append, newLine);
			}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToPseudoVariableReference(this);
		}
		
	}
	
	public class VariableReference : NamedReference {
		protected IdentifierToken variable = null;
		
		public VariableReference(IdentifierToken variable, int occurrenceIndex) : base(occurrenceIndex) {
			this.variable = variable;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.VariableReference;}
		}
	
		public override LexicalToken InitialToken {
			get {return variable;}
		}
	
		public override LexicalToken FinalToken {
			get {return variable;}
		}
		
		public IdentifierToken Variable {
			get {return variable;}
		}

		public override String BindingName {
			get {return Variable.Name;}
		}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (variable != null) {
				newLine(depth);
				variable.printUsing(depth, append, newLine);
			}

		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (variable != null) {
				variable.esPrintUsing(depth, append, newLine);
			}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToVariableReference(this);

		}
		
	}
	
	public class NestedExpression : Operand {
		protected ExpressionBeginToken expressionBegin = null;
		protected Statement statement = null;
		protected ExpressionEndToken expressionEnd = null;
		
		public NestedExpression(ExpressionBeginToken expressionBegin, Statement statement, ExpressionEndToken expressionEnd, int occurrenceIndex) : base(occurrenceIndex) {
			this.expressionBegin = expressionBegin;
			this.statement = statement;
			this.expressionEnd = expressionEnd;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.NestedExpression;}
		}
	
		public override LexicalToken InitialToken {
			get {return expressionBegin;}
		}
	
		public override LexicalToken FinalToken {
			get {	if (expressionEnd != null) return expressionEnd;
				if (statement != null) return statement.FinalToken;
				return expressionBegin;}
		}
		
		public ExpressionBeginToken ExpressionBegin {
			get {return expressionBegin;}
		}
		
		public Statement Statement {
			get {return statement;}
		}
		
		public ExpressionEndToken ExpressionEnd {
			get {return expressionEnd;}
		}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (expressionBegin != null) {
				newLine(depth);
				expressionBegin.printUsing(depth, append, newLine);
			}
			if (statement != null) {
				newLine(depth);
				statement.printUsing(depth, append, newLine);
			}
			if (expressionEnd != null) {
				newLine(depth);
				expressionEnd.printUsing(depth, append, newLine);
			}

		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (expressionBegin != null) {
				expressionBegin.esPrintUsing(depth, append, newLine);
			}
			if (statement != null) {
				statement.esPrintUsing(depth, append, newLine);
			}
			if (expressionEnd != null) {
				expressionEnd.esPrintUsing(depth, append, newLine);
			}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToNestedExpression(this);
		}
		
	}
	
	public class LexicalLiteralValue : LiteralValue {
		protected LiteralValueToken literalToken = null;
		
		public LexicalLiteralValue(LiteralValueToken literalToken, int occurrenceIndex) : base(occurrenceIndex) {
			this.literalToken = literalToken;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.Literal;}
		}
	
		public override LexicalToken InitialToken {
			get {return literalToken;}
		}
	
		public override LexicalToken FinalToken {
			get {return literalToken;}
		}
		
		public LiteralValueToken LiteralToken {
			get {return literalToken;}
		}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (literalToken != null) {
				newLine(depth);
				literalToken.printUsing(depth, append, newLine);
			}
			
		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (literalToken != null) {
				literalToken.esPrintUsing(depth, append, newLine);
			}
		}

		public override String asPathString(char separator) {
			if (literalToken != null) {
				return literalToken.asPathString(separator);
			}
			return null;
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToLexicalLiteralValue(this);
		}
		
	}
	
	public class DynamicArrayLiteral : LiteralValue {
		protected DynamicArrayBeginToken arrayBegin = null;
		protected ExpressionInLiteral[] elements = null;
		protected CurlyBraceStructureEndToken arrayEnd = null; 
		
		public DynamicArrayLiteral(DynamicArrayBeginToken arrayBegin, ExpressionInLiteral[] elements, CurlyBraceStructureEndToken arrayEnd, int occurrenceIndex) 
				: base(occurrenceIndex) {
			this.arrayBegin = arrayBegin;
			this.elements = elements == null ? null : (elements.Length > 0 ? elements : null);
			this.arrayEnd = arrayEnd;
		}

		public DynamicArrayLiteral(DynamicArrayBeginToken arrayBegin, List<ExpressionInLiteral> elements, CurlyBraceStructureEndToken arrayEnd, int occurrenceIndex) : 
			this(arrayBegin, elements == null ? null : elements.ToArray(), arrayEnd, occurrenceIndex) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.DynamicArrayLiteral;}
		}
	
		public override LexicalToken InitialToken {
			get {return arrayBegin;}
		}
	
		public override LexicalToken FinalToken {
			get {	if (arrayEnd != null) return arrayEnd;
				if (elements != null && elements.Length > 0) return elements[elements.Length - 1].FinalToken;
				return arrayBegin;}
		}
		
		public DynamicArrayBeginToken ArrayBegin {
			get {return arrayBegin;}
		}

		public CurlyBraceStructureEndToken ArrayEnd {
			get {return arrayEnd;}
		}
		
		public void elementsDo(Action<ExpressionInLiteral> enumerator1) {
			if (elements == null) return;
			foreach (var element in elements) enumerator1(element);
		}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (arrayBegin != null) {
				newLine(depth);
				arrayBegin.printUsing(depth, append, newLine);
			}
			if (elements != null) {
				printUsing(elements, depth, append, newLine);
			}
			if (arrayEnd != null) {
				newLine(depth);
				arrayEnd.printUsing(depth, append, newLine);
			}
			
		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (arrayBegin != null) {
				arrayBegin.esPrintUsing(depth, append, newLine);
			}
			if (elements != null) {
				esPrintUsing(elements, depth, append, newLine);
			}
			if (arrayEnd != null) {
				arrayEnd.esPrintUsing(depth, append, newLine);
			}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToDynamicArrayLiteral(this);
		}
		
	}
	
	public class DictionaryLiteral : LiteralValue {
		protected DictionaryLiteralBeginToken dictionaryBegin = null;
		protected ExpressionInLiteral[] elements = null;
		protected CurlyBraceStructureEndToken dictionaryEnd = null;
		
		public DictionaryLiteral(DictionaryLiteralBeginToken dictionaryBegin, ExpressionInLiteral[] elements, CurlyBraceStructureEndToken dictionaryEnd, int occurrenceIndex) : base(occurrenceIndex) {
			this.dictionaryBegin = dictionaryBegin;
			this.elements = elements == null ? null : (elements.Length > 0 ? elements : null);
			this.dictionaryEnd = dictionaryEnd;
		}

		public DictionaryLiteral(DictionaryLiteralBeginToken dictionaryBegin, List<ExpressionInLiteral> elements, CurlyBraceStructureEndToken dictionaryEnd, int occurrenceIndex) 
			: this(dictionaryBegin, elements == null ? null : elements.ToArray(), dictionaryEnd, occurrenceIndex) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.DictionaryLiteral;}
		}
	
		public override LexicalToken InitialToken {
			get {return dictionaryBegin;}
		}
	
		public override LexicalToken FinalToken {
			get {	if (dictionaryEnd != null) return dictionaryEnd;
				if (elements != null && elements.Length > 0) return elements[elements.Length - 1].FinalToken;
				return dictionaryBegin;}
		}
		
		public DictionaryLiteralBeginToken DictionaryBegin {
			get {return dictionaryBegin;}
		}
		
		public CurlyBraceStructureEndToken DictionaryEnd {
			get {return dictionaryEnd;}
		}
		
		public void elementsDo(Action<ExpressionInLiteral> enumerator1) {
			if (elements == null) return;
			foreach (var element in elements) enumerator1(element);
		}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (dictionaryBegin != null) {
				newLine(depth);
				dictionaryBegin.printUsing(depth, append, newLine);
			}
			if (elements != null) {
				printUsing(elements, depth, append, newLine);
			}
			if (dictionaryEnd != null) {
				newLine(depth);
				dictionaryEnd.printUsing(depth, append, newLine);
			}
			
		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (dictionaryBegin != null) {
				dictionaryBegin.esPrintUsing(depth, append, newLine);
			}
			if (elements != null) {
				esPrintUsing(elements, depth, append, newLine);
			}
			if (dictionaryEnd != null) {
				dictionaryEnd.esPrintUsing(depth, append, newLine);
			}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToDictionaryLiteral(this);
		}
		
	}
	
	public class ExpressionInLiteral : NonTerminalParseTreeNode {
		// Elements of a DynamicArray or of a DictionaryLiteral
		protected Expression expression = null;
		protected StatementEndToken expressionSeparator = null;
		
		public ExpressionInLiteral(Expression expression, StatementEndToken expressionSeparator, int occurrenceIndex) : base(occurrenceIndex) {
			this.expression = expression;
			this.expressionSeparator = expressionSeparator;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.ExpressionInLiteral;}
		}
	
		public override LexicalToken InitialToken {
			get {return expression.InitialToken;}
		}
	
		public override LexicalToken FinalToken {
			get {	if (expressionSeparator != null) return expressionSeparator;
				if (expression != null) return expression.FinalToken;
				return null;}
		}

		public Expression Expression {
			get {return expression;}
		}

		public StatementEndToken ExpressionSeparator {
			get {return expressionSeparator;}
		}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (expression != null) {
				newLine(depth);
				expression.printUsing(depth, append, newLine);
			}
			if (expressionSeparator != null) {
				newLine(depth);
				expressionSeparator.printUsing(depth, append, newLine);
			}
			
		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (expression != null) {
				expression.esPrintUsing(depth, append, newLine);
			}
			if (expressionSeparator != null) {
				expressionSeparator.esPrintUsing(depth, append, newLine);
				append(" ");
			}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToExpressionInLiteral(this);
		}
		
	}
	
	public abstract class CodeLiteral : LiteralValue {
		protected BlockBeginToken blockBegin = null;
		protected BlockEndToken blockEnd = null;
		
		public CodeLiteral(BlockBeginToken blockBegin, BlockEndToken blockEnd, int occurrenceIndex) : base(occurrenceIndex) {
			this.blockBegin = blockBegin;
			this.blockEnd = blockEnd;
		}
	
		public override LexicalToken InitialToken {
			get {return blockBegin;}
		}
	
		public override LexicalToken FinalToken {
			get {	if (blockEnd != null) return blockEnd;
				return null;}
		}
		
		public BlockBeginToken BlockBegin {
			get {return blockBegin;}
		}
		
		public BlockEndToken BlockEnd {
			get {return blockEnd;}
		}

		public abstract void parameterNamesDo(Action<String> enumerator1);
				
		public abstract void variablesDo(Action<DeclarableIdentifierToken> enumerator1);

		public abstract void variableNamesDo(Action<String> enumerator1);

	}
	
	public class BlockLiteral : CodeLiteral {
		protected BlockDeclaration blockDeclaration = null;
		
		public BlockLiteral(BlockBeginToken blockBegin, BlockDeclaration blockDeclaration, BlockEndToken blockEnd, int occurrenceIndex) 
				: base(blockBegin, blockEnd, occurrenceIndex) {
			this.blockDeclaration = blockDeclaration;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.BlockLiteral;}
		}
	
		public override bool IsBlockLiteral {
			get {return true;}
		}
	
		public override LexicalToken InitialToken {
			get {	var initialToken = base.InitialToken;
				if (initialToken != null) return initialToken;
				if (blockDeclaration != null) {
					initialToken = blockDeclaration.InitialToken;
					if (initialToken != null) return initialToken;
				}
				return blockEnd;}
		}
	
		public override LexicalToken FinalToken {
			get {	var finalToken = base.FinalToken;
				if (finalToken != null) return finalToken;
				if (blockDeclaration != null) {
					finalToken = blockDeclaration.FinalToken;
					if (finalToken != null) return finalToken;
				}
				return blockBegin;}
		}
		
		public BlockDeclaration Declaration {
			get {return blockDeclaration;}
		}

		public BlockParameterDeclarationList ParameterList {
			get {return blockDeclaration == null ? null : blockDeclaration.ParameterList;}
		}
		
		public ExecutableCode Body {
			get {return blockDeclaration == null ? null : blockDeclaration.Body;}
		}

		public void parametersDo(Action<BlockParameterToken> enumerator1) {
			if (blockDeclaration == null) return;
			blockDeclaration.parametersDo(enumerator1);
		}

		public override void parameterNamesDo(Action<String> enumerator1) {
			if (blockDeclaration == null) return;
			blockDeclaration.parameterNamesDo(enumerator1);
		}
		
		public override void variablesDo(Action<DeclarableIdentifierToken> enumerator1) {
			if (blockDeclaration == null) return;
			blockDeclaration.variablesDo(enumerator1);
		}

		public override void variableNamesDo(Action<String> enumerator1) {
			if (blockDeclaration == null) return;
			blockDeclaration.variableNamesDo(enumerator1);
		}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (blockBegin != null) {
				newLine(depth);
				blockBegin.printUsing(depth, append, newLine);
			}
			if (blockDeclaration != null) {
				newLine(depth);
				blockDeclaration.printUsing(depth, append, newLine);
			}
			if (blockEnd != null) {
				newLine(depth);
				blockEnd.printUsing(depth, append, newLine);
			}
			
		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (blockBegin != null) {
				blockBegin.esPrintUsing(depth, append, newLine);
			}
			if (blockDeclaration != null) {
				blockDeclaration.esPrintUsing(depth, append, newLine);
			}
			if (blockEnd != null) {
				blockEnd.esPrintUsing(depth, append, newLine);
			}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToBlockLiteral(this);
		}
		
	}

	public class MethodLiteral : CodeLiteral {
		protected MethodHeaderBeginToken methodHeaderToken = null;
		protected MethodDeclaration methodDeclaration = null;
		
		public MethodLiteral(BlockBeginToken blockBegin, MethodHeaderBeginToken methodHeaderToken, MethodDeclaration methodDeclaration, BlockEndToken blockEnd, int occurrenceIndex) 
				: base(blockBegin, blockEnd, occurrenceIndex) {
			this.methodHeaderToken = methodHeaderToken;
			this.methodDeclaration = methodDeclaration;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.MethodLiteral;}
		}
	
		public override bool IsMethodLiteral {
			get {return true;}
		}
	
		public override LexicalToken FinalToken {
			get {	var finalToken = base.FinalToken;
				if (finalToken != null) return finalToken;
				if (methodDeclaration != null) return methodDeclaration.FinalToken;
				if (methodHeaderToken != null) return methodHeaderToken;
				return blockBegin;}
		}
		
		public MethodHeaderBeginToken MethodHeaderToken {
			get {return methodHeaderToken;}
		}
		
		public MethodDeclaration MethodDeclaration {
			get {return methodDeclaration;}
		}

		public void parametersDo(Action<DeclarableIdentifierToken> enumerator1) {
			if (methodDeclaration == null) return;
			methodDeclaration.parametersDo(enumerator1);
		}

		public override void parameterNamesDo(Action<String> enumerator1) {
			if (methodDeclaration == null) return;
			methodDeclaration.parameterNamesDo(enumerator1);
		}
		
		public override void variablesDo(Action<DeclarableIdentifierToken> enumerator1) {
			if (methodDeclaration == null) return;
			methodDeclaration.variablesDo(enumerator1);
		}

		public override void variableNamesDo(Action<String> enumerator1) {
			if (methodDeclaration == null) return;
			methodDeclaration.variableNamesDo(enumerator1);
		}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of each element of the receiver
			
			if (blockBegin != null) {
				newLine(depth);
				blockBegin.printUsing(depth, append, newLine);
			}
			if (methodHeaderToken != null) {
				newLine(depth);
				methodHeaderToken.printUsing(depth, append, newLine);
			}
			if (methodDeclaration != null) {
				newLine(depth);
				methodDeclaration.printUsing(depth, append, newLine);
			}
			if (blockEnd != null) {
				newLine(depth);
				blockEnd.printUsing(depth, append, newLine);
			}
			
		}
		
		public override void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (blockBegin != null) {
				blockBegin.esPrintUsing(depth, append, newLine);
			}
			if (methodHeaderToken != null) {
				methodHeaderToken.esPrintUsing(depth, append, newLine);
			}
			if (methodDeclaration != null) {
				methodDeclaration.esPrintUsing(depth, append, newLine);
			}
			if (blockEnd != null) {
				blockEnd.esPrintUsing(depth, append, newLine);
			}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToMethodLiteral(this);
		}

	}

}
