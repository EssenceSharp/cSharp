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
using System.IO;
using System.Text;
using Microsoft.Scripting;
using EssenceSharp.UtilityServices;
#endregion

namespace EssenceSharp.ParsingServices {
	
	// The Essence Sharp Smalltalk Parser parses ANSI-Standard Smalltalk, optionally recognizing one or more syntactical extensions, into a ParseNodeTree. 
	
	public enum SyntaxProfile {
		Amber,		// Squeak + DictionaryLiterals. Amber runs natively on the JavaScript virtual machine. (See notes #1 through #4 below.)
		ANSI,		// All widely-used implementations extend ANSI, and fully implement it syntactially (but not semantically; see notes #1 through #4 below.)
		SmalltalkX,	// VisualWorks + extensions, but with different qualified name syntax.  (See note #2 below.)
		Squeak,		// Most widely-used syntax profile. There are several implementations descended from Squeak, such as Pharo and Spoon. (See notes #1 through #4 below.)
		Universal,	// Default: Widest set of non-conflicting syntax elements, plus an extension for method literals (see note #5.)
		VisualWorks,	// Most widely-used syntax profile in commercial/proprietary use. (See note #2 below.)
	}	
	// Note 1: 	The constant names nil, true and false are always interpreted as their ANSI-mandated values when they occur in array literals, even though legacy (but not modern) versions of 
	//		Squeak and its descendents interpet them as Symbol literals in that case (Squeak used to conform to the original Smalltalk-80 semantics in this regard.) Note that it is 
	//		technically possible for a code generator to reinterpret them as Symbol literals, since the necessary context is available in the syntax tree.
	//
	// Note 2: 	The parser does not permit any of nil, true, false, self or super (and thisContext, if it's accepted as a pseudo-variable) to be declared as variable names, method parameter 
	//		names or block parameter names. That prohibition is required by the ANSI Smalltalk Standard (and is rather bad practice in any case.) The legacy compilers for most of the 
	//		widely-used Smalltalk implementations DO permit reserved identifiers to be declared as variables and/or parameters, but typically (interactively) request programmer permission 
	//		before proceeding with compilation.
	//
	// Note 3:	The parser does not permit assignment to either block parameters or method parameters. The ANSI Standard does not permit it, and it was not allowed in Smalltalk-80.
	//
	// Note 4:	The parser does not permit the usage of the underscore character as an assignment operator. ANSI requires that the underscore character be usable in identifiers, so that's what
	//		the parser implements.
	//
	// Note 5:	The parser (optionally) recognizes "##" (two immediately-adjacent hash characters) as a lexical token, and gives it the meaning "what follows is a method header." If and only
	// 		if the parser is configured to recognize this <MethodHeaderBegin> token, the token may optionally appear as the first token of any method declaration (but is not required.)
	//		Also (and this is the reason for/intended usage of the <MethodHeaderBegin> token,) it may appear as the first token following the <[> (BlockBegin) token. If it does, then the 
	//		source code enclosed within the <[> and <]> (BlockBegin and BlockEnd>) tokens will be parsed as a method declaration, and the entire construct (including the <[> and <]> tokens) 
	//		will be interpreted as a method literal (instead of as a block literal.) Additionally, if the initial token following a <[> (BlockBegin) token is either a binary message selector 
	//		or a keyword, and if the parser is also configured to recognize the <MethodHeaderBegin> token, then the souce code enclosed within the <[> and <]> tokens will be parsed as a 
	//		method declaration even though there is no leading <MethodHeaderBegin> token, and the entire construct (including the <[> and <]> tokens) will be interpreted as a method literal 
	//		(instead of as a block literal.) 
	//
	//		Whitespace is permitted but not required both preceding and following the <MethodHeaderBegin> token. 
	//
	//		Examples:
	//
	//			##at: index
	//				"Method declaration as it might appear in a code browser, with OPTIONAL leading <MethodHeaderBegin> token."
	//				^index > self size ifTrue: [IndexOutOfBoundsException raiseWith: index] ifFalse: [self elements at: index]
	//
	//			[## yourself ^self] "Method literal as it might appear in the body of a method or doIt, with leading (and required in this case) <MethodHeaderBegin> token."
	//
	//			[@ y ^Point x: self y: y] "Method literal as it might appear in the body of a method or doIt, without the (in this case) optional leading <MethodHeaderBegin> token."
	//
	//			[at: index ^self basicAt: index] "Method literal as it might appear in the body of a method or doIt, without the (in this case) optional leading <MethodHeaderBegin> token."
		
	public class ParsingOptions {

		protected const char		defaultQualifiedNameSeparator		= '.';

		protected bool			supportsMethodHeaderBeginToken		= true;
		protected bool			supportsThisContextPseudoVariable	= true;
		protected bool			supportsQualifiedSymbolSyntax		= true;
		protected char			qualifiedNameSeparatorChar		= defaultQualifiedNameSeparator;
		protected bool			supportsDynamicArrayLiterals		= true;
		protected bool			supportsDictionaryLiterals		= true;
		protected bool			supportsBindingReferenceLiterals	= false;
		protected bool			supportsCurlyBraceStructureSyntax	= true;
		
		public ParsingOptions() {}
		
		public ParsingOptions(SyntaxProfile syntaxProfile) {
			SyntaxProfile = syntaxProfile;
		}

		public ParsingOptions(bool supportQualifiedNameSyntax, 
					char qualifiedNameSegmentSeparatorChar,
					bool supportDynamicArrayLiterals,
					bool supportDictionaryLiterals,
					bool supportBindingReferenceLiterals,
					bool supportThisContextPseudoVariable) {
			SupportsQualifiedNameSyntax 		= supportQualifiedNameSyntax;
			QualifiedNameSeparatorChar 		= qualifiedNameSeparatorChar;
			SupportsDynamicArrayLiterals 		= supportDynamicArrayLiterals;
			SupportsDictionaryLiterals 		= supportDictionaryLiterals;
			SupportsBindingReferenceLiterals 	= supportBindingReferenceLiterals;
			SupportsThisContextPseudoVariable	= supportThisContextPseudoVariable;
		}

		public SyntaxProfile SyntaxProfile {
			set {switch (value) {
				case SyntaxProfile.Amber:
					SupportsMethodHeaderBeginToken = false;
					SupportsQualifiedNameSyntax = false;
					SupportsDynamicArrayLiterals = true;
					SupportsDictionaryLiterals = true;
					SupportsBindingReferenceLiterals = false;
					SupportsThisContextPseudoVariable = true;
					break;
				case SyntaxProfile.ANSI:
					SupportsMethodHeaderBeginToken = false;
					SupportsQualifiedNameSyntax = false;
					SupportsDynamicArrayLiterals = false;
					SupportsDictionaryLiterals = false;
					SupportsBindingReferenceLiterals = false;
					SupportsThisContextPseudoVariable = false;
					break;
				case SyntaxProfile.SmalltalkX:
					SupportsMethodHeaderBeginToken = false;
					SupportsQualifiedNameSyntax = true;
					QualifiedNameSeparatorChar = ':';
					SupportsDynamicArrayLiterals = false;
					SupportsDictionaryLiterals = false;
					SupportsBindingReferenceLiterals = true;
					SupportsThisContextPseudoVariable = true;
					break;
				case SyntaxProfile.Squeak:
					SupportsMethodHeaderBeginToken = false;
					SupportsQualifiedNameSyntax = false;
					SupportsDynamicArrayLiterals = true;
					SupportsDictionaryLiterals = false;
					SupportsBindingReferenceLiterals = false;
					SupportsThisContextPseudoVariable = true;
					break;
				case SyntaxProfile.Universal:
					SupportsMethodHeaderBeginToken = true;
					SupportsQualifiedNameSyntax = true;
					SupportsDynamicArrayLiterals = true;
					SupportsDictionaryLiterals = true;
					SupportsBindingReferenceLiterals = false;
					SupportsThisContextPseudoVariable = true;
					break;
				case SyntaxProfile.VisualWorks:
					SupportsMethodHeaderBeginToken = false;
					SupportsQualifiedNameSyntax = true;
					QualifiedNameSeparatorChar = '.';
					SupportsDynamicArrayLiterals = false;
					SupportsDictionaryLiterals = false;
					SupportsBindingReferenceLiterals = true;
					SupportsThisContextPseudoVariable = true;
					break;
			}}
		}
		
		public bool SupportsMethodHeaderBeginToken {
			get {return supportsMethodHeaderBeginToken;}
			set {supportsMethodHeaderBeginToken = value;}
		}
		
		public bool SupportsQualifiedNameSyntax {
			get {return supportsQualifiedSymbolSyntax;}
			set {supportsQualifiedSymbolSyntax = value;}
		}
		
		public char QualifiedNameSeparatorChar {
			// Cannot be an identifier char, a whitespace char or a control char
			get {return qualifiedNameSeparatorChar;}
			set {qualifiedNameSeparatorChar = value.isIdentifierChar() || Char.IsWhiteSpace(value) || Char.IsControl(value) ? defaultQualifiedNameSeparator : value;}
		}
		
		public bool SupportsDynamicArrayLiterals {
			get {return supportsDynamicArrayLiterals;}
			set {supportsDynamicArrayLiterals = value;
				supportsCurlyBraceStructureSyntax = SupportsDynamicArrayLiterals || SupportsDictionaryLiterals || SupportsBindingReferenceLiterals;}
		}
		
		public bool SupportsDictionaryLiterals {
			get {return supportsDictionaryLiterals;}
			set {supportsBindingReferenceLiterals = value ? false : supportsBindingReferenceLiterals;
				supportsDictionaryLiterals = value;
				supportsCurlyBraceStructureSyntax = SupportsDynamicArrayLiterals || SupportsDictionaryLiterals || SupportsBindingReferenceLiterals;}
		}
		
		public bool SupportsBindingReferenceLiterals {
			get {return supportsBindingReferenceLiterals;}
			set {supportsDictionaryLiterals = value ? false : supportsDictionaryLiterals;
				supportsBindingReferenceLiterals = value;
				supportsCurlyBraceStructureSyntax = SupportsDynamicArrayLiterals || SupportsDictionaryLiterals || SupportsBindingReferenceLiterals;}
		}
		
		public bool SupportsCurlyBraceStructureSyntax {
			get {return supportsCurlyBraceStructureSyntax;}
		}
		
		public bool SupportsThisContextPseudoVariable {
			get {return supportsThisContextPseudoVariable;}
			set {supportsThisContextPseudoVariable = value;}
		}	

	}

	public enum ParsingContext {// Used to modify the behavior of the LexicalAnalyizer
		General,
		MessageSelector,	// Expecting the name of a method or function
		VariableDeclaration,	// Expecting the name of a variable being declared
		MethodHeader		// Expecting the header of a method or function (should be the same as combining MessageSelector + VariableDeclaration)
	}
		
	// NOTE: These are _parse_ tree nodes, not _Abstract Syntax Tree_ nodes.  Parse tree nodes are responsible for faithfully representing the source code. 
	// They are completely independent of the target language, and can be used for code reformatting and other purposes besides compiling to machine code.
	// 
	// For the AST nodes, see the compiler.

	public enum ParseNodeType {
		MethodDeclaration,
		PrimitiveMethodDeclaration,
		MethodHeader,
		UnaryMethodHeader,
		BinaryMethodHeader,
		KeywordMethodHeader,
		KeywordMethodHeaderSegment,
		BlockDeclaration,
		ExecutableCode,
		LocalVariableDeclarationList,
		BlockParameterDeclarationList,
		Statement,
		FinalStatement,
		MethodReturnPrefix,
		AssignmentPrefix,
		VariableAssignment,
		Expression,
		MessageChain,
		CascadedMessage,
		InitiallyUnaryMessageChain,
		InitiallyBinaryMessageChain,
		KeywordMessageChain,
		UnaryOnlyMessageChain,
		BinaryOnlyMessageChain,
		UnaryMessage,
		BinaryMessage,
		KeywordMessage,
		KeywordMessageSegment,
		KeywordMessageArgument,
		BinaryMessageOperand,
		Operand,
		Literal,
		DynamicArrayLiteral,
		DictionaryLiteral,
		BlockLiteral,
		MethodLiteral,
		ConstantReference,
		PseudoVariableReference,
		VariableReference,
		NestedExpression,
		ExpressionInLiteral,
		
		// Lexical Token Types:
		Array,
		ByteArray,
		LiteralBindingReference, 	// VisualWorks (Not ANSI, and conflicts with literal dictionary syntax)
		Symbol,
		String,
		Char,
		SinglePrecision,
		DoublePrecision,
		QuadPrecision,
		ScaledDecimal,
		Integer,
		True,
		False,
		Nil,
		Keyword,			// Element of a keyword message selector
		BlockParameter,
		DeclarableIdentifier,
		Identifier,			// Variable name, parameter name or unary message selector
		BinaryMessageSelector,
		Self,
		Super,
		ThisContext,			// Not ANSI, but supported by most of the major dialects
		StatementEnd,
		MessageCascadeOp,
		MethodHeaderBegin,		// Universal Smalltalk syntax
		DynamicArrayBegin, 		// Squeak, Pharo, Amber, etc. (not ANSI, but widely adopted)
		DictionaryLiteralBegin, 	// Amber (not ANSI, and conflicts with LiteralBindingReference syntax)
		CurlyBraceStructureEnd,		// Dynamic array literal, dictionary literal, or BindingReference literal
		BlockBegin,
		BlockEnd,
		ExpressionBegin,
		ExpressionEnd,
		VerticalBar,			// Variable declaraction section delimiter OR binary message selector
		ReturnOp,
		AssignOp,
		Comment,
		InvalidToken,
		UnknownToken,
		IllegalChar,
		EndOfSource,
	}
	
	public abstract class ParseTreeNode {

		protected int	occurrenceIndex		= -1;

		protected ParseTreeNode(int occurrenceIndex) {
			this.occurrenceIndex = occurrenceIndex;
		}
			
		public int OccurrenceIndex {
			get {return occurrenceIndex;}
		}

		public abstract uint LineNumberStart {
			get;
		}
			
		public abstract uint ColumnNumberStart {
			get;
		}
		
		public abstract uint LineNumberEnd {
			get;
		}
			
		public abstract uint ColumnNumberEnd {
			get;
		}

		public virtual SourceLocation Start {
			get {return new SourceLocation(OccurrenceIndex, (int)LineNumberStart, (int)ColumnNumberStart);}
		}

		public virtual SourceLocation End {
			get {return new SourceLocation(OccurrenceIndex, (int)LineNumberEnd, (int)ColumnNumberEnd);}
		}

		public SourceSpan Span {
			get {return new SourceSpan(Start, End);}
		}

		public abstract ParseNodeType ParseNodeType {
			get;
		}

		public virtual ParseNodeType OperandNodeType {
			get {return ParseNodeType;}
		}
		
		public virtual bool TransformsToEpsilon {
			get {return false;}
		}
	
		public virtual bool IsLexical {
			get {return false;}
		}
		
		public virtual bool IsNonTerminalNode {
			get {return false;}
		}
		
		public virtual bool IsMeta {
			get {return false;}
		}
	
		public virtual bool IsEndOfSource {
			get {return false;}
		}
	
		public virtual bool IsExpression {
			get {return false;}
		}
	
		public virtual bool IsStatement {
			get {return false;}
		}
	
		public virtual bool IsExecutableCode {
			get {return false;}
		}
	
		public virtual bool IsMethodDeclaration {
			get {return false;}
		}
	
		public virtual bool IsBlockDeclaration {
			get {return false;}
		}
	
		public virtual bool IsBlockLiteral {
			get {return false;}
		}
	
		public virtual bool IsMethodLiteral {
			get {return false;}
		}

		public virtual bool RepresentsLexicalError {
			get {return false;}
		}
		
		public virtual bool RepresentsLiteralValue {
			get {return false;}
		}
		
		public virtual bool CanBeDeclaredAsVariable {
			get {return false;}
		}		
		
		public virtual bool CanBeDeclaredAsVariableOrParameter {
			get {return false;}
		}		
		
		public virtual void esPrintElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
		}
		
		protected virtual void esPrintUsing(ParseTreeNode[] elements, uint depth, Action<String> append, Action<uint> newLine) {
			if (elements != null) {
				int lastIndex = elements.Length - 1;
				for (int i = 0; i <= lastIndex; i++) {
					elements[i].esPrintUsing(depth, append, newLine);
					if (i < lastIndex) append(" ");
				}
			}
		}
		
		public virtual void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			esPrintElementsUsing(depth+1, append, newLine);
		}

		public void esPrintUsing(Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			esPrintUsing(0, append, newLine);
		}
		
		public void esPrintOn(StringBuilder sb) {
			// Print out canonical Smalltalk source code representation
			esPrintUsing(
				delegate (String text) {
					sb.Append(text);
				},
				delegate (uint depth) {
					sb.AppendLine("");
					if (depth > 0) {
						sb.Append(new String(' ', (int)depth * 8));
					}
				});
		}
		
		public void esPrintOn(TextWriter stream) {
			// Print out canonical Smalltalk source code representation
			esPrintUsing(
				delegate (String text) {
					stream.Write(text);
				},
				delegate (uint depth) {
					stream.WriteLine("");
					if (depth > 0) {
						stream.WriteLine(new String(' ', (int)depth * 8));
					}
				});
		}
		
		public virtual String asPathString(char separator) {
			return AsString;
		}
		
		public String AsString {
			// Answer the canonical Smalltalk source code representation
			get {StringBuilder sb = new StringBuilder();
				esPrintOn(sb);
				return sb.ToString();}
		}
		
		public virtual void printTypeUsing(Action<String> append) {
			append(GetType().Name);
		}
		
		public virtual void printUsing(uint depth, Action<String> append, Action<uint> newLine) {
			append("{<");
			printTypeUsing(append);
			append(">");
			printElementsUsing(depth+1, append, newLine);
			append("}");
		}
		
		public virtual void printUsing(Action<String> append, Action<uint> newLine) {
			printUsing(0, append, newLine);
		}
		
		public virtual void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
		}
		
		protected virtual void printUsing(ParseTreeNode[] elements, uint depth, Action<String> append, Action<uint> newLine) {
			if (elements != null) {
				for (uint i = 0; i < elements.Length; i++) {
					newLine(depth);
					elements[i].printUsing(depth, append, newLine);
				}
			}
		}

		public virtual void printOn(StringBuilder sb) {
			printUsing(
				text => sb.Append(text),
				delegate (uint depth) {
					sb.AppendLine("");
					if (depth > 0) {
						sb.Append(new String(' ', (int)depth * 8));
					}
				});
		}
		
		public virtual void printOn(TextWriter stream) {
			printUsing(
				stream.Write,
				delegate (uint depth) {
					stream.WriteLine("");
					if (depth > 0) {
						stream.WriteLine(new String(' ', (int)depth * 8));
					}
				});
		}
		
		public override String ToString() {
			var sb = new StringBuilder();
			printOn(sb);
			return sb.ToString();		
		
		}

		public virtual T valueBy<T>(ParseTreeNodeOperation<T> operation) {
			return operation.applyToParseTreeNode(this);
		}
		
	}

	public interface ParseTreeNodeOperation<T> {

		// Double dispatching root 
		T applyTo(ParseTreeNode operand);
        
		// General

		T applyToParseTreeNode(ParseTreeNode operand);

		// Lexical Tokens
		T applyToArrayLiteralToken(ArrayLiteralToken operand);
		T applyToAssignOpToken(AssignOpToken operand);
		T applyToBinaryMessageSelectorToken(BinaryMessageSelectorToken operand);
		T applyToBlockBeginToken(BlockBeginToken operand);
		T applyToBlockEndToken(BlockEndToken operand);
		T applyToBlockParameterToken(BlockParameterToken operand);
		T applyToByteArrayLiteralToken(ByteArrayLiteralToken operand);
		T applyToCharLiteralToken(CharLiteralToken operand);
		T applyToCommentToken(CommentToken operand);
		T applyToCurlyBraceStructureEndToken(CurlyBraceStructureEndToken operand);
		T applyToDeclarableIdentifierToken(DeclarableIdentifierToken operand);
		T applyToDictionaryLiteralBeginToken(DictionaryLiteralBeginToken operand);
		T applyToDoublePrecisionLiteralToken(DoublePrecisionLiteralToken operand);
		T applyToDynamicArrayBeginToken(DynamicArrayBeginToken operand);
		T applyToEndOfSourceToken(EndOfSourceToken operand);
		T applyToExpressionBeginToken(ExpressionBeginToken operand);
		T applyToExpressionEndToken(ExpressionEndToken operand);
		T applyToFalseToken(FalseToken operand);
		T applyToIllegalCharToken(IllegalCharToken operand);
		T applyToIntegerLiteralToken(IntegerLiteralToken operand);
		T applyToInvalidTokenToken(InvalidTokenToken operand);
		T applyToKeywordToken(KeywordToken operand);
		T applyToLiteralBindingReferenceToken(LiteralBindingReferenceToken operand);
		T applyToMessageCascadeOpToken(MessageCascadeOpToken operand);
		T applyToMethodHeaderBeginToken(MethodHeaderBeginToken operand);
		T applyToNilToken(NilToken operand);
		T applyToQuadPrecisionLiteralToken(QuadPrecisionLiteralToken operand);
		T applyToReturnOpToken(ReturnOpToken operand);
		T applyToScaledDecimalLiteralToken(ScaledDecimalLiteralToken operand);
		T applyToSelfToken(SelfToken operand);
		T applyToSinglePrecisionLiteralToken(SinglePrecisionLiteralToken operand);
		T applyToStatementEndToken(StatementEndToken operand);
		T applyToStringLiteralToken(StringLiteralToken operand);
		T applyToSuperToken(SuperToken operand);
		T applyToSymbolLiteralToken(SymbolLiteralToken operand);
		T applyToThisContextToken(ThisContextToken operand);
		T applyToTrueToken(TrueToken operand);
		T applyToUnknownTokenToken(UnknownTokenToken operand);
		T applyToVerticalBarToken(VerticalBarToken operand);


		// Non-terminal parse tree nodes
		T applyToAssignmentPrefix(AssignmentPrefix operand);
		T applyToBinaryMessage(BinaryMessage operand);
		T applyToBinaryMessageOperand(BinaryMessageOperand operand);
		T applyToBinaryMethodHeader(BinaryMethodHeader operand);
		T applyToBinaryOnlyMessageChain(BinaryOnlyMessageChain operand);
		T applyToBlockLiteral(BlockLiteral operand);
		T applyToBlockParameterDeclarationList(BlockParameterDeclarationList operand);
		T applyToCascadedMessage(CascadedMessageChain operand);
		T applyToConstantReference(ConstantReference operand);
		T applyToDictionaryLiteral(DictionaryLiteral operand);
		T applyToDynamicArrayLiteral(DynamicArrayLiteral operand);
		T applyToExecutableCode(ExecutableCode operand);
		T applyToExpression(Expression operand);
		T applyToExpressionInLiteral(ExpressionInLiteral operand);
		T applyToFinalStatement(FinalStatement operand);
		T applyToInitiallyBinaryMessageChain(InitiallyBinaryMessageChain operand);
		T applyToInitiallyUnaryMessageChain(InitiallyUnaryMessageChain operand);
		T applyToKeywordMessage(KeywordMessage operand);
		T applyToKeywordMessageArgument(KeywordMessageArgument operand);
		T applyToKeywordMessageChain(KeywordMessageChain operand);
		T applyToKeywordMessageSegment(KeywordMessageSegment operand);
		T applyToKeywordMethodHeader(KeywordMethodHeader operand);
		T applyToKeywordMethodHeaderSegment(KeywordMethodHeaderSegment operand);
		T applyToLexicalLiteralValue(LexicalLiteralValue operand);
		T applyToLocalVariableDeclarationList(LocalVariableDeclarationList operand);
		T applyToMethodDeclaration(MethodDeclaration operand);
		T applyToBlockDeclaration(BlockDeclaration operand);
		T applyToMethodLiteral(MethodLiteral operand);
		T applyToMethodReturnPrefix(MethodReturnPrefix operand);
		T applyToNestedExpression(NestedExpression operand);
		T applyToPrimitiveMethodDeclaration(PrimitiveMethodDeclaration operand);
		T applyToPseudoVariableReference(PseudoVariableReference operand);
		T applyToStatement(Statement operand);
		T applyToUnaryMessage(UnaryMessage operand);
		T applyToUnaryMethodHeader(UnaryMethodHeader operand);
		T applyToUnaryOnlyMessageChain(UnaryOnlyMessageChain operand);
		T applyToVariableAssignment(VariableAssignment operand);
		T applyToVariableReference(VariableReference operand);

	}

	public abstract class BaseParseTreeNodeOperation<T> : ParseTreeNodeOperation<T> {

		// Double dispatching root 

		public virtual T applyTo(ParseTreeNode operand) {
			return operand.valueBy(this);
		}

		public virtual T applyToParseTreeNode(ParseTreeNode operand) {
			// By default, do nothing
			return default(T);
		}

		// Lexical Tokens

		public virtual T applyToArrayLiteralToken(ArrayLiteralToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToAssignOpToken(AssignOpToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToBinaryMessageSelectorToken(BinaryMessageSelectorToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToBlockBeginToken(BlockBeginToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToBlockEndToken(BlockEndToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToBlockParameterToken(BlockParameterToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToByteArrayLiteralToken(ByteArrayLiteralToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToCharLiteralToken(CharLiteralToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToCommentToken(CommentToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToCurlyBraceStructureEndToken(CurlyBraceStructureEndToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToDeclarableIdentifierToken(DeclarableIdentifierToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToDictionaryLiteralBeginToken(DictionaryLiteralBeginToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToDoublePrecisionLiteralToken(DoublePrecisionLiteralToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToDynamicArrayBeginToken(DynamicArrayBeginToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToEndOfSourceToken(EndOfSourceToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToExpressionBeginToken(ExpressionBeginToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToExpressionEndToken(ExpressionEndToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToFalseToken(FalseToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToIllegalCharToken(IllegalCharToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToIntegerLiteralToken(IntegerLiteralToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToInvalidTokenToken(InvalidTokenToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToKeywordToken(KeywordToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToLiteralBindingReferenceToken(LiteralBindingReferenceToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToMessageCascadeOpToken(MessageCascadeOpToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToMethodHeaderBeginToken(MethodHeaderBeginToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToNilToken(NilToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToQuadPrecisionLiteralToken(QuadPrecisionLiteralToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToReturnOpToken(ReturnOpToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToScaledDecimalLiteralToken(ScaledDecimalLiteralToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToSelfToken(SelfToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToSinglePrecisionLiteralToken(SinglePrecisionLiteralToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToStatementEndToken(StatementEndToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToStringLiteralToken(StringLiteralToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToSuperToken(SuperToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToSymbolLiteralToken(SymbolLiteralToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToThisContextToken(ThisContextToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToTrueToken(TrueToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToUnknownTokenToken(UnknownTokenToken operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToVerticalBarToken(VerticalBarToken operand) {
			return applyToParseTreeNode(operand);
		}

		// Non-terminal parse tree nodes

		public virtual T applyToAssignmentPrefix(AssignmentPrefix operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToBinaryMessage(BinaryMessage operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToBinaryMessageOperand(BinaryMessageOperand operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToBinaryMethodHeader(BinaryMethodHeader operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToBinaryOnlyMessageChain(BinaryOnlyMessageChain operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToBlockLiteral(BlockLiteral operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToBlockParameterDeclarationList(BlockParameterDeclarationList operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToCascadedMessage(CascadedMessageChain operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToConstantReference(ConstantReference operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToDictionaryLiteral(DictionaryLiteral operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToDynamicArrayLiteral(DynamicArrayLiteral operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToExecutableCode(ExecutableCode operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToExpression(Expression operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToExpressionInLiteral(ExpressionInLiteral operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToFinalStatement(FinalStatement operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToInitiallyBinaryMessageChain(InitiallyBinaryMessageChain operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToInitiallyUnaryMessageChain(InitiallyUnaryMessageChain operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToKeywordMessage(KeywordMessage operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToKeywordMessageArgument(KeywordMessageArgument operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToKeywordMessageChain(KeywordMessageChain operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToKeywordMessageSegment(KeywordMessageSegment operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToKeywordMethodHeader(KeywordMethodHeader operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToKeywordMethodHeaderSegment(KeywordMethodHeaderSegment operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToLexicalLiteralValue(LexicalLiteralValue operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToLocalVariableDeclarationList(LocalVariableDeclarationList operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToBlockDeclaration(BlockDeclaration operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToMethodDeclaration(MethodDeclaration operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToMethodLiteral(MethodLiteral operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToMethodReturnPrefix(MethodReturnPrefix operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToNestedExpression(NestedExpression operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToPrimitiveMethodDeclaration(PrimitiveMethodDeclaration operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToPseudoVariableReference(PseudoVariableReference operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToStatement(Statement operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToUnaryMessage(UnaryMessage operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToUnaryMethodHeader(UnaryMethodHeader operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToUnaryOnlyMessageChain(UnaryOnlyMessageChain operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToVariableAssignment(VariableAssignment operand) {
			return applyToParseTreeNode(operand);
		}

		public virtual T applyToVariableReference(VariableReference operand) {
			return applyToParseTreeNode(operand);
		}

	}
	
}




