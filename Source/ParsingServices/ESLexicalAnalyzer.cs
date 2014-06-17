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
using EssenceSharp.UtilityServices;
#endregion

namespace EssenceSharp.ParsingServices {
	
	public enum NumberParsingResult {
		ValidNumber,
		Overflow,
		IntegerRadixNotFollowedByInteger,
		UnsupportedIntegerRadix,
		NotANumber
	}
	
	public abstract class LexicalAnalyzer<TokenType, ParsingContextType> {
		
		#region Instance Variables
			
		private bool			hasMoreData				= false;
		private TextReader		sourceStream				= null;
		private TextReader		lineStream				= null;
		private int			tokenOccurrenceIndex			= 0;
		private uint			lineNumber				= 0;
		private uint			columnNumber				= 0;
		private bool			hasNextChar				= false;
		private char			nextChar				= (char)0;
		
		protected List<TokenType>	lookAheadBuffer				= null;
			
		#endregion
		
		#region Constructors & Initialization
		
		protected void initialize() {
			hasMoreData = sourceStream != null && sourceStream.Peek() != -1;
			lineStream = null;
			lineNumber = 0;
			columnNumber = 0;
			hasNextChar = false;
			nextChar = (char)0;		
		}
		
		internal void resetStreamPosition() {
			lineNumber = 0;
			columnNumber = 0;
		}

		#endregion	
		
		#region Character IO
		
		protected bool advanceLine() {
			hasNextChar = false;
			hasMoreData = sourceStream != null && sourceStream.Peek() != -1;
			if (hasMoreData) {
				lineStream = new StringReader(sourceStream.ReadLine());
				lineNumber++;
				columnNumber = 0;
				return true;
			} else {
				lineStream = null;
				return false;
			}
		}
		
		protected bool advanceChar() {
			if (hasMoreData) {
				int next = lineStream.Read();
				if (next != -1) {
					columnNumber += (uint)(next == 9 ? 8 : 1);
					nextChar = (char)next;
				} else {
					if (!advanceLine()) return false;
					nextChar = (char)13;
				}
				hasNextChar = true;
				return true;
			} else {
				hasNextChar = false;
				return false;
			}
		}
		
		protected bool peek(ref char c) {
			if (hasMoreData && (hasNextChar || advanceChar())) {
				c = nextChar;
				return true;
			} else {
				return false;
			}
		}
		
		protected bool nextMatches(char c) {
			if (hasMoreData && (hasNextChar || advanceChar())) {
				if (nextChar == c) {
					advanceChar();
					return true;
				}
			}
			return false;
		}
		
		protected bool satisfies(Predicate<char> charTest) {
			if (hasMoreData && (hasNextChar || advanceChar())) {
				if (charTest(nextChar)) {
					advanceChar();
					return true;
				}
			}
			return false;
		}
		
		protected bool next(ref char c) {
			if (hasMoreData && (hasNextChar || advanceChar())) {
				c = nextChar;
				advanceChar();
				return true;
			} else {
				return false;
			}
		}
			
		protected int advanceUntil(Predicate<char> chTest) {
			int count = 0;
			char ch = (char)0;
			while (peek(ref ch)) {
				if (chTest(ch)) return count;
				advanceChar();
				count++;;
			}
			return count;
		}
			
		protected int advanceWhile(Predicate<char> chTest) {
			int count = 0;
			char ch = (char)0;
			while (peek(ref ch)) {
				if (!chTest(ch)) return count;
				advanceChar();
				count++;;
			}
			return count;
		}
		
		protected int appendOntoUntil(StringBuilder target, Predicate<char> chTest) {
			int count = 0;
			char ch = (char)0;
			while (peek(ref ch)) {
				if (chTest(ch)) return count;
				advanceChar();
				target.Append(ch);
				count++;;
			}
			return count;
		}
			
		protected int appendOntoWhile(StringBuilder target, Predicate<char> chTest) {
			int count = 0;
			char ch = (char)0;
			while (peek(ref ch)) {
				if (!chTest(ch)) return count;
				advanceChar();
				target.Append(ch);
				count++;;
			}
			return count;
		}
			
		protected  String nextUntil(Predicate<char> chTest) {
			StringBuilder target = new StringBuilder();
			if (appendOntoUntil(target, chTest) > 0) return target.ToString();
			return null;
		}
			
		protected  String nextWhile(Predicate<char> chTest) {
			StringBuilder target = new StringBuilder();
			if (appendOntoWhile(target, chTest) > 0) return target.ToString();
			return null;
		}
		
		#endregion
		
		#region Public Protocol
		
		#region Properties
		
		public int OccurrenceIndex {
			get {return tokenOccurrenceIndex + 1;}
		}

		public uint LineNumber {
			get {return lineNumber;}
		}
			
		public uint ColumnNumber {
			get {return columnNumber;}
		}
		
		public TextReader SourceStream {
			get {return sourceStream;}
			set {initialize();
				sourceStream = value;
				advanceLine();}
		}
		
		public bool HasMoreData {
			get {return hasMoreData;}
		}
		
		#endregion
		
		public bool skipOverWhitespace(ref char nextCh) {
			if (!peek(ref nextCh)) return false;
			while (Char.IsWhiteSpace(nextCh)) {
				advanceChar();
				if (!peek(ref nextCh)) return false;
			}
			return true;
		}
		
		public abstract TokenType nextToken(ParsingContextType context);	

		#endregion
				
		#region Tokenizing
		
		internal int nextOccurrenceIndex() {
			return tokenOccurrenceIndex++;
		}
		
		internal void resetOccurrenceIndex() {
			tokenOccurrenceIndex = 0;
		}
		
		protected void enqueueBufferedToken(TokenType bufferedToken) {
			if (lookAheadBuffer == null) {
				lookAheadBuffer = new List<TokenType>();
			}
			lookAheadBuffer.Add(bufferedToken);
		}
		
		protected TokenType dequeueBufferedToken() {
			if (lookAheadBuffer != null) {
				TokenType bufferedToken = lookAheadBuffer[0];
				lookAheadBuffer.RemoveAt(0);
				if (lookAheadBuffer.Count == 0) lookAheadBuffer = null;
				return bufferedToken;
			}
			return default(TokenType);
		}
		
		#endregion
		
	}
	
	public class ESLexicalAnalyzer : LexicalAnalyzer<LexicalToken, ParsingContext> {

		#region Instance Variables

		protected ParsingOptions	parsingOptions;
		
		protected List<CommentToken> commentBuffer = new List<CommentToken>();
			
		#endregion
		
		#region Constructors & Initialization
		
		public ESLexicalAnalyzer(TextReader sourceStream) {
			SourceStream = sourceStream;
			parsingOptions = new ParsingOptions();
			SyntaxProfile = ParsingServices.SyntaxProfile.ANSI;
		}
		
		public ESLexicalAnalyzer(TextReader sourceStream, SyntaxProfile syntaxProfile) {
			SourceStream = sourceStream;
			parsingOptions = new ParsingOptions();
			SyntaxProfile = syntaxProfile;
		}
		
		public ESLexicalAnalyzer(
				TextReader sourceStream, 
				ParsingOptions parsingOptions) {
			SourceStream = sourceStream;
			ParsingOptions = parsingOptions;
		}
		
		#endregion
		
		#region Public Protocol
		
		#region Properties

		public ParsingOptions ParsingOptions {
			get {return parsingOptions;}
			set {parsingOptions = value ?? new ParsingOptions();}
		}
		
		public SyntaxProfile SyntaxProfile {
			set {ParsingOptions.SyntaxProfile = value;}
		}
		
		public bool SupportsMethodHeaderBeginToken {
			get {return ParsingOptions.SupportsMethodHeaderBeginToken;}
			set {ParsingOptions.SupportsMethodHeaderBeginToken = value;}
		}
		
		public bool SupportsQualifiedNameSyntax {
			get {return ParsingOptions.SupportsQualifiedNameSyntax;}
			set {ParsingOptions.SupportsQualifiedNameSyntax = value;}
		}
		
		public char QualifiedNameSeparatorChar {
			// Cannot be an identifier char, a whitespace char or a control char
			get {return ParsingOptions.QualifiedNameSeparatorChar;}
			set {ParsingOptions.QualifiedNameSeparatorChar = value;}
		}
		
		public bool SupportsDynamicArrayLiterals {
			get {return ParsingOptions.SupportsDynamicArrayLiterals;}
			set {ParsingOptions.SupportsDynamicArrayLiterals = value;}
		}
		
		public bool SupportsDictionaryLiterals {
			get {return ParsingOptions.SupportsDictionaryLiterals;}
			set {ParsingOptions.SupportsDictionaryLiterals = value;}
		}
		
		public bool SupportsBindingReferenceLiterals {
			get {return ParsingOptions.SupportsBindingReferenceLiterals;}
			set {ParsingOptions.SupportsBindingReferenceLiterals = value;}
		}
		
		public bool SupportsCurlyBraceStructureSyntax {
			get {return ParsingOptions.SupportsCurlyBraceStructureSyntax;}
		}
		
		public bool SupportsThisContextPseudoVariable {
			get {return ParsingOptions.SupportsThisContextPseudoVariable;}
			set {ParsingOptions.SupportsThisContextPseudoVariable = value;}
		}	
		
		#endregion		
		
		public override LexicalToken nextToken(ParsingContext context) {
			
			LexicalToken token = basicNextToken(context);
			if (token == null) return null;
			if (token.IsComment) {
				commentBuffer.Add((CommentToken)token);
				return nextToken(context);
			} else if (commentBuffer.Count > 0) {
				CommentableToken commentableToken = (CommentableToken)token;
				commentableToken.PrecedingCommentsList = commentBuffer;
				commentBuffer.Clear();
				return commentableToken;
			} else {
				return token;
			}
			
		}
		
		#endregion		

		#region Lexical Token Creation

		#region Meta tokens

		#region Error tokens

		protected virtual IllegalCharToken newIllegalCharToken(char illegalChar, uint lineNumber, uint columnNumber) {
			return new IllegalCharToken(illegalChar, nextOccurrenceIndex(), lineNumber, columnNumber, lineNumber, columnNumber);
		}

		protected virtual InvalidTokenToken newInvalidTokenToken(LexicalToken invalidToken, String description, uint lineNumberStart, uint columnNumberStart) {
			return new InvalidTokenToken(
				invalidToken,
				description, 
				nextOccurrenceIndex(), 
				lineNumberStart, 
				columnNumberStart,
				LineNumber,
				ColumnNumber - 1);
		}

		protected virtual InvalidTokenToken newInvalidTokenToken(LexicalToken invalidToken, String description, IllegalCharToken illegalCharToken, uint lineNumberStart, uint columnNumberStart) {
			return new InvalidTokenToken(
				invalidToken,
				description, 
				illegalCharToken,
				nextOccurrenceIndex(), 
				lineNumberStart, 
				columnNumberStart,
				LineNumber,
				ColumnNumber - 1);
		}

		protected virtual UnknownTokenToken newUnknownTokenToken(String tokenPrefix, char invalidSuffix, uint lineNumberStart, uint columnNumberStart) {
			return newUnknownTokenToken(tokenPrefix, new String(invalidSuffix, 1), lineNumberStart, columnNumberStart);
		}

		protected virtual UnknownTokenToken newUnknownTokenToken(String tokenPrefix, String invalidSuffix, uint lineNumberStart, uint columnNumberStart) {
			return new UnknownTokenToken(
				tokenPrefix,
				invalidSuffix,
				nextOccurrenceIndex(), 
				lineNumberStart, 
				columnNumberStart,
				LineNumber,
				ColumnNumber - 1);
		}

		#endregion

		protected virtual CommentToken newCommentToken(String comment, uint lineNumberStart, uint columnNumberStart) {
			return new CommentToken(comment, nextOccurrenceIndex(), lineNumberStart, columnNumberStart, LineNumber, ColumnNumber - 1);
		}

		protected virtual EndOfSourceToken newEndOfSourceToken() {
			return new EndOfSourceToken(nextOccurrenceIndex(), LineNumber, ColumnNumber, LineNumber, ColumnNumber);
		}

		#endregion

		#region Single-character tokens

		internal virtual ReturnOpToken newReturnOpToken(uint lineNumber, uint columnNumber) {
			return new ReturnOpToken(nextOccurrenceIndex(), lineNumber, columnNumber, lineNumber, columnNumber);
		}

		internal virtual VerticalBarToken newVerticalBarToken(uint lineNumber, uint columnNumber) {
			return new VerticalBarToken(nextOccurrenceIndex(), lineNumber, columnNumber, lineNumber, columnNumber);
		}

		internal virtual ExpressionBeginToken newExpressionBeginToken(uint lineNumber, uint columnNumber) {
			return new ExpressionBeginToken(nextOccurrenceIndex(), lineNumber, columnNumber, lineNumber, columnNumber);
		}

		internal virtual ExpressionEndToken newExpressionEndToken(uint lineNumber, uint columnNumber) {
			return new ExpressionEndToken(nextOccurrenceIndex(), lineNumber, columnNumber, lineNumber, columnNumber);
		}

		internal virtual BlockBeginToken newBlockBeginToken(uint lineNumber, uint columnNumber) {
			return new BlockBeginToken(nextOccurrenceIndex(), lineNumber, columnNumber, lineNumber, columnNumber);
		}

		internal virtual BlockEndToken newBlockEndToken(uint lineNumber, uint columnNumber) {
			return new BlockEndToken(nextOccurrenceIndex(), lineNumber, columnNumber, lineNumber, columnNumber);
		}

		internal virtual DynamicArrayBeginToken newDynamicArrayBeginToken(uint lineNumber, uint columnNumber) {
			return new DynamicArrayBeginToken(nextOccurrenceIndex(), lineNumber, columnNumber, lineNumber, columnNumber);
		}

		internal virtual CurlyBraceStructureEndToken newCurlyBraceStructureEndToken(uint lineNumber, uint columnNumber) {
			return new CurlyBraceStructureEndToken(nextOccurrenceIndex(), lineNumber, columnNumber, lineNumber, columnNumber);
		}

		internal virtual MessageCascadeOpToken newMessageCascadeOpToken(uint lineNumber, uint columnNumber) {
			return new MessageCascadeOpToken(nextOccurrenceIndex(), lineNumber, columnNumber, lineNumber, columnNumber);
		}

		internal virtual StatementEndToken newStatementEndToken(uint lineNumber, uint columnNumber) {
			return new StatementEndToken(nextOccurrenceIndex(), lineNumber, columnNumber, lineNumber, columnNumber);
		}

		#endregion

		#region Multi-character tokens

		internal virtual MethodHeaderBeginToken newMethodHeaderBeginToken(uint lineNumber, uint columnNumber) {
			return new MethodHeaderBeginToken(nextOccurrenceIndex(), lineNumber, columnNumber, lineNumber, columnNumber + 1);
		}

		internal virtual AssignOpToken newAssignOpToken(uint lineNumber, uint columnNumber) {
			return new AssignOpToken(nextOccurrenceIndex(), lineNumber, columnNumber, lineNumber, columnNumber + 1);
		}

		internal virtual DictionaryLiteralBeginToken newDictionaryLiteralBeginToken(uint lineNumber, uint columnNumber) {
			return new DictionaryLiteralBeginToken(nextOccurrenceIndex(), lineNumber, columnNumber, lineNumber, columnNumber + 1);
		}
						
		internal virtual DeclarableIdentifierToken newDeclarableIdentifierToken(String name, uint pathElementCount, uint lineNumberStart, uint columnNumberStart) {
			return new DeclarableIdentifierToken(name, pathElementCount > 1 ? QualifiedNameSeparatorChar : (char?)null, nextOccurrenceIndex(), lineNumberStart, lineNumberStart, LineNumber, ColumnNumber - 1);
		}

		internal virtual BlockParameterToken newBlockParameterToken(String name, uint lineNumberStart, uint columnNumberStart) {
			return new BlockParameterToken(name, nextOccurrenceIndex(), lineNumberStart, lineNumberStart, LineNumber, ColumnNumber - 1);
		}

		internal virtual BinaryMessageSelectorToken newBinaryMessageSelectorToken(String name, uint lineNumberStart, uint columnNumberStart) {
			return new BinaryMessageSelectorToken(name, nextOccurrenceIndex(), lineNumberStart, lineNumberStart, LineNumber, ColumnNumber - 1);
		}

		internal virtual KeywordToken newKeywordToken(String value, uint lineNumberStart, uint columnNumberStart) {
			return new KeywordToken(value, nextOccurrenceIndex(), lineNumberStart, lineNumberStart, LineNumber, ColumnNumber - 1);
		}

		internal virtual NilToken newNilToken(uint lineNumberStart, uint columnNumberStart) {
			return new NilToken(nextOccurrenceIndex(), lineNumberStart, lineNumberStart, LineNumber, ColumnNumber - 1);
		}

		internal virtual FalseToken newFalseToken(uint lineNumberStart, uint columnNumberStart) {
			return new FalseToken(nextOccurrenceIndex(), lineNumberStart, lineNumberStart, LineNumber, ColumnNumber - 1);
		}

		internal virtual TrueToken newTrueToken(uint lineNumberStart, uint columnNumberStart) {
			return new TrueToken(nextOccurrenceIndex(), lineNumberStart, lineNumberStart, LineNumber, ColumnNumber - 1);
		}

		internal virtual SelfToken newSelfToken(uint lineNumberStart, uint columnNumberStart) {
			return new SelfToken(nextOccurrenceIndex(), lineNumberStart, lineNumberStart, LineNumber, ColumnNumber - 1);
		}

		internal virtual SuperToken newSuperToken(uint lineNumberStart, uint columnNumberStart) {
			return new SuperToken(nextOccurrenceIndex(), lineNumberStart, lineNumberStart, LineNumber, ColumnNumber - 1);
		}

		internal virtual ThisContextToken newThisContextToken(uint lineNumberStart, uint columnNumberStart) {
			return new ThisContextToken(nextOccurrenceIndex(), lineNumberStart, lineNumberStart, LineNumber, ColumnNumber - 1);
		}

		internal virtual CharLiteralToken newCharLiteralToken(char charLiteral, uint lineNumberStart, uint columnNumberStart) {
			return new CharLiteralToken(charLiteral, nextOccurrenceIndex(), lineNumberStart, columnNumberStart, LineNumber, ColumnNumber);
		}
		 
		internal virtual StringLiteralToken newStringLiteralToken(char enclosingChar, String value, uint lineNumberStart, uint columnNumberStart) {
			return new StringLiteralToken(enclosingChar, value, nextOccurrenceIndex(), lineNumberStart, lineNumberStart, LineNumber, ColumnNumber - 1);
		}

		internal virtual SymbolLiteralToken newSymbolLiteralToken(char enclosingChar, String value, SymbolType symbolType, uint numArgs, char? qualifiedNameSeparatorChar, uint pathElementCount, uint lineNumberStart, uint columnNumberStart) {
			return new SymbolLiteralToken(enclosingChar, value, symbolType, numArgs, qualifiedNameSeparatorChar, pathElementCount, nextOccurrenceIndex(), lineNumberStart, lineNumberStart, LineNumber, ColumnNumber - 1);
		}

		internal virtual LiteralBindingReferenceToken newLiteralBindingReferenceToken(String value, uint pathElementCount, uint lineNumberStart, uint columnNumberStart) {
			return new LiteralBindingReferenceToken(value, pathElementCount, nextOccurrenceIndex(), lineNumberStart, lineNumberStart, LineNumber, ColumnNumber - 1);
		}

		internal virtual ArrayLiteralToken newArrayLiteralToken(LexicalLiteralToken[] elements, uint lineNumberStart, uint columnNumberStart) {
			return new ArrayLiteralToken(elements, nextOccurrenceIndex(), lineNumberStart, lineNumberStart, LineNumber, ColumnNumber - 1);
		}

		internal virtual ByteArrayLiteralToken newByteArrayLiteralToken(byte[] elements, uint lineNumberStart, uint columnNumberStart) {
			return new ByteArrayLiteralToken(elements, nextOccurrenceIndex(), lineNumberStart, lineNumberStart, LineNumber, ColumnNumber - 1);
		}

		internal virtual ByteArrayLiteralToken newByteArrayLiteralToken(byte[] elements, List<CommentInNonObjectArray> internalComments, uint lineNumberStart, uint columnNumberStart) {
			return new ByteArrayLiteralToken(elements, internalComments, nextOccurrenceIndex(), lineNumberStart, lineNumberStart, LineNumber, ColumnNumber - 1);
		}

		internal virtual IntegerLiteralToken newIntegerLiteralToken(String decimalPrefix, int sign, uint lineNumberStart, uint columnNumberStart) {
			return new IntegerLiteralToken(decimalPrefix, sign, nextOccurrenceIndex(), lineNumberStart, columnNumberStart, LineNumber, ColumnNumber - 1);
		}
		 
		internal virtual IntegerLiteralToken newIntegerLiteralToken(String decimalPrefix, uint radix, int sign, uint lineNumberStart, uint columnNumberStart) {
			return new IntegerLiteralToken(decimalPrefix, radix, sign, nextOccurrenceIndex(), lineNumberStart, columnNumberStart, LineNumber, ColumnNumber - 1);
		}

		internal virtual ScaledDecimalLiteralToken newScaledDecimalLiteralToken(String decimalPrefix, String decimalSuffix, int sign, uint lineNumberStart, uint columnNumberStart) {
			return new ScaledDecimalLiteralToken(decimalPrefix, decimalSuffix, sign, nextOccurrenceIndex(), lineNumberStart, columnNumberStart, LineNumber, ColumnNumber - 1);
		}

		internal virtual ScaledDecimalLiteralToken newScaledDecimalLiteralToken(String decimalPrefix, String decimalSuffix, uint specifiedScale, int sign, uint lineNumberStart, uint columnNumberStart) {
			return new ScaledDecimalLiteralToken(decimalPrefix, decimalSuffix, specifiedScale, sign, nextOccurrenceIndex(), lineNumberStart, columnNumberStart, LineNumber, ColumnNumber - 1);
		}

		internal virtual SinglePrecisionLiteralToken newSinglePrecisionLiteralToken(float value, uint lineNumberStart, uint columnNumberStart) {
			return new SinglePrecisionLiteralToken(value, nextOccurrenceIndex(), lineNumberStart, lineNumberStart, LineNumber, ColumnNumber - 1);
		}

		internal virtual SinglePrecisionLiteralToken newSinglePrecisionLiteralToken(float value, long integerExponent, uint lineNumberStart, uint columnNumberStart) {
			return new SinglePrecisionLiteralToken(value, (int)integerExponent, nextOccurrenceIndex(), lineNumberStart, lineNumberStart, LineNumber, ColumnNumber - 1);
		}

		internal virtual DoublePrecisionLiteralToken newDoublePrecisionLiteralToken(double value, uint lineNumberStart, uint columnNumberStart) {
			return new DoublePrecisionLiteralToken(value, nextOccurrenceIndex(), lineNumberStart, lineNumberStart, LineNumber, ColumnNumber - 1);
		}

		internal virtual DoublePrecisionLiteralToken newDoublePrecisionLiteralToken(double value, long integerExponent, uint lineNumberStart, uint columnNumberStart) {
			return new DoublePrecisionLiteralToken(value, (int)integerExponent, nextOccurrenceIndex(), lineNumberStart, lineNumberStart, LineNumber, ColumnNumber - 1);
		}

		internal virtual QuadPrecisionLiteralToken newQuadPrecisionLiteralToken(decimal value, uint lineNumberStart, uint columnNumberStart) {
			return new QuadPrecisionLiteralToken(value, nextOccurrenceIndex(), lineNumberStart, lineNumberStart, LineNumber, ColumnNumber - 1);
		}

		internal virtual QuadPrecisionLiteralToken newQuadPrecisionLiteralToken(decimal value, long integerExponent, uint lineNumberStart, uint columnNumberStart) {
			return new QuadPrecisionLiteralToken(value, (int)integerExponent, nextOccurrenceIndex(), lineNumberStart, lineNumberStart, LineNumber, ColumnNumber - 1);
		}

		#endregion

		#endregion

		#region Tokenizing

		protected LexicalToken basicNextToken(ParsingContext context) {
			
			LexicalToken bufferedToken = dequeueBufferedToken();
			if (bufferedToken != null) return bufferedToken;
			
			char initialChar = (char)0;
			if (!skipOverWhitespace(ref initialChar)) return newEndOfSourceToken();
			
			uint initialLineNumber = LineNumber;
			uint initialColumnNumber = ColumnNumber;
			
			advanceChar();
			
			switch (initialChar) {
				// Comment
				case '"':
					return parseComment(initialLineNumber, initialColumnNumber);
					
				// ReturnOp:
				case '^':
					return newReturnOpToken(initialLineNumber, initialColumnNumber);
					
				// AssignmentOp or block arg
				case ':':
					return parseAssignmentOpOrBlockArg(initialLineNumber, initialColumnNumber);
					
				// Vertical Bar: Variable declaration section boundary or binary selector
				case '|':
					return newVerticalBarToken(initialLineNumber, initialColumnNumber);
					
				// Begin expression
				case '(':
					return newExpressionBeginToken(initialLineNumber, initialColumnNumber);
				
				// End expression
				case ')':
					return newExpressionEndToken(initialLineNumber, initialColumnNumber);
					
				// Begin block
				case '[':
					return newBlockBeginToken(initialLineNumber, initialColumnNumber);
				
				// End block
				case ']':
					return newBlockEndToken(initialLineNumber, initialColumnNumber);
					
				// Begin Dynamic Array
				case '{': 
					if (SupportsDynamicArrayLiterals) {
						return newDynamicArrayBeginToken(initialLineNumber, initialColumnNumber);
					} else {
						return newIllegalCharToken(initialChar, initialLineNumber, initialColumnNumber);
					}
					
				// End Curly-Brace Structure (e.g., a dynamic array or Dictionary literal)
				case '}': 
					if (SupportsCurlyBraceStructureSyntax) {
						return newCurlyBraceStructureEndToken(initialLineNumber, initialColumnNumber);
					} else {
						return newIllegalCharToken(initialChar, initialLineNumber, initialColumnNumber);
					}

				// MessageCascadeOp
				case ';':
					return newMessageCascadeOpToken(initialLineNumber, initialColumnNumber);
					
				// End statement
				case '.':
					return newStatementEndToken(initialLineNumber, initialColumnNumber);
					
				// Binary selectors
				case '~':
				case '!':
				case '@':
				case '%':
				case '&':
				case '*':
				case '=':
				case '\\':
				case '<':
				case '>':
				case ',':
				case '?':
				case '/':
				case '+':
					return parseBinaryMessageSelector(initialChar, initialLineNumber, initialColumnNumber);
					
				// Negative numbers or binary selectors
				case '-':
					return parseNegativeNumericLiteralOrBinaryMessageSelector(initialLineNumber, initialColumnNumber);
					
				// Numeric literals
				case '0':
				case '1':
				case '2':
				case '3':
				case '4':
				case '5':
				case '6':
				case '7':
				case '8':
				case '9':
					return parseNumericLiteral(initialChar, 1, initialLineNumber, initialColumnNumber);
					
				// Character literals
				case '$':
					return parseCharLiteral(initialLineNumber, initialColumnNumber);
					
				// String literals
				case '\'':
					return parseStringLiteral(initialChar, initialLineNumber, initialColumnNumber);
					
				// Symbol, method header begin, byteArray or array or curly-brace structure literals:
				case '#':
					return parseSymbolOrMethodHeaderBeginOrByteArrayOrArrayOrCurlyBraceStructure(false, initialLineNumber, initialColumnNumber);
				
				// False literal constant, identifer or keyword
				case 'f':
					return parseKeywordOrIdentifierOrFalseLiteralConstant(context, initialLineNumber, initialColumnNumber);
					
				// Nil literal constant, identifer or keyword
				case 'n':
					return parseKeywordOrIdentifierOrNilLiteralConstant(context, initialLineNumber, initialColumnNumber);
				
				// Self pseudo-variable, super pseudo-variable or identifier or keyword
				case 's':
					return parseKeywordOrIdentifierOrSelfOrSuperPseudoVariables(context, initialLineNumber, initialColumnNumber);
				
				// True literal constant, thisContext pseudo-variable or identifier or keyword
				case 't':
					return parseKeywordOrIdentifierOrTrueLiteralConstantOrThisContextPseudoVariable(context, initialLineNumber, initialColumnNumber);
					
				// Identifiers or keywords
				case '_':
				case 'A':
				case 'a':
				case 'B':
				case 'b':
				case 'C':
				case 'c':
				case 'D':
				case 'd':
				case 'E':
				case 'e':
				case 'F':
				case 'G':
				case 'g':
				case 'H':
				case 'h':
				case 'I':
				case 'i':
				case 'J':
				case 'j':
				case 'K':
				case 'k':
				case 'L':
				case 'l':
				case 'M':
				case 'm':
				case 'N':
				case 'O':
				case 'o':
				case 'P':
				case 'p':
				case 'Q':
				case 'q':
				case 'R':
				case 'r':
				case 'S':
				case 'T':
				case 'U':
				case 'u':
				case 'V':
				case 'v':
				case 'W':
				case 'w':
				case 'X':
				case 'x':
				case 'Y':
				case 'y':
				case 'Z':
				case 'z':
					return parseKeywordOrIdentifier(context, initialChar, initialLineNumber, initialColumnNumber);
					
				// Illegal/Reserved chars
				case '`': // Reserved
				default:
					return newIllegalCharToken(initialChar, initialLineNumber, initialColumnNumber);
			}
			
		}
		
		protected int scanDecimalInteger(char initialChar, bool ignoreLeadingZeros, StringBuilder digits) {
			bool initialCharIsZero = ignoreLeadingZeros && initialChar == '0';
			bool nonZeroDigitScanned = !initialCharIsZero && !Char.IsControl(initialChar);
			if (nonZeroDigitScanned) digits.Append(initialChar);
			int count = 0;
			advanceWhile((char ch) => {
							long digitVal = ch.decimalDigitValue();
							bool isDigit = digitVal >= 0; 
							if (isDigit) {
								if (ignoreLeadingZeros) {
									nonZeroDigitScanned = nonZeroDigitScanned || ch != '0';
									if (nonZeroDigitScanned) {
										digits.Append(ch); 
										count++;
									}
								} else {
									digits.Append(ch); 
									count++;
								}
							}
							return isDigit;});
			if (!nonZeroDigitScanned && initialCharIsZero) {
				digits.Append(initialChar);
			}
			return count;
		}

		protected int scanInteger(uint numberBase, char initialChar, bool ignoreLeadingZeros, StringBuilder digits) {
			bool initialCharIsZero = ignoreLeadingZeros && initialChar == '0';
			bool nonZeroDigitScanned = !initialCharIsZero && !Char.IsControl(initialChar);
			if (nonZeroDigitScanned) digits.Append(initialChar);
			int count = 0;
			advanceWhile((char ch) => {
							long digitVal = ch.digitValue();
							bool isDigit = digitVal >= 0 && digitVal < numberBase; 
							if (isDigit) {
								if (ignoreLeadingZeros) {
									nonZeroDigitScanned = nonZeroDigitScanned || ch != '0';
									if (nonZeroDigitScanned) {
										digits.Append(ch); 
										count++;
									}
								} else {
									digits.Append(ch); 
									count++;
								}
							}
							return isDigit;});
			if (!nonZeroDigitScanned && initialCharIsZero) {
				digits.Append(initialChar);
			}
			return count;
		}
		
		protected int parseDecimalInteger(int initialCount, ref long outerValue) {
			bool initialValueIsZero = initialCount > 0 ? outerValue == 0 : false;
			bool nonZeroDigitScanned = !initialValueIsZero;
			long value = outerValue;
			int count = initialCount;
			advanceWhile((char ch) => {
							long digitVal = ch.decimalDigitValue();
							bool isDigit = digitVal >= 0; 
							if (isDigit) {
								nonZeroDigitScanned = nonZeroDigitScanned || ch != '0';
								if (nonZeroDigitScanned) {
									value = value * 10 + digitVal; 
									count++;
								}
							}
							return isDigit;});
			outerValue = value;
			return count;
		}

		protected int parseInteger(int initialCount, uint numberBase, ref long outerValue) {
			bool initialValueIsZero = initialCount > 0 ? outerValue == 0 : false;
			bool nonZeroDigitScanned = !initialValueIsZero;
			long value = outerValue;
			int count = initialCount;
			advanceWhile((char ch) => {
							long digitVal = ch.digitValue();
							bool isDigit = digitVal >= 0 && digitVal < numberBase; 
							if (isDigit) {
								nonZeroDigitScanned = nonZeroDigitScanned || ch != '0';
								if (nonZeroDigitScanned) {
									value = value * numberBase + digitVal; 
									count++;
								}
							}
							return isDigit;});
			outerValue = value;
			return count;
		}
		
		protected LexicalToken parseNumericLiteral(char initialChar, int sign, uint initialLineNumber, uint initialColumnNumber) {
			
			StringBuilder digits = new StringBuilder();
			long prefixValue = 0;
			uint scale = 0;
			int scaleCount = 0;
			long integerExponent = 0;
			int exponentCount = 0;
			long suffixValue = 0;
			
			int prefixCount = scanDecimalInteger(initialChar, true, digits);
			String decimalPrefix = digits.ToString();
			
			char continuationChar = (char)0;
			if (peek(ref continuationChar)) {
				switch (continuationChar) {
						
					default:
						// Integers
						return newIntegerLiteralToken(decimalPrefix, sign, initialLineNumber, initialColumnNumber);
						
					case 'r':
						// Radix Integers
						uint radix = (uint)decimalPrefix.integerValueFromDecimalDigits();
						if (radix <= 36) {
							advanceChar();
							digits.Length = 0;
							if (scanInteger(radix, (char)0, true, digits) > 0) {
								return newIntegerLiteralToken(digits.ToString(), radix, sign, initialLineNumber, initialColumnNumber);
							} else {
								return newInvalidTokenToken(
									newIntegerLiteralToken("", radix, sign, initialLineNumber, initialColumnNumber),
									"IntegerRadixNotFollowedByInteger", 
									initialLineNumber, 
									initialColumnNumber);
							}
						} else {
							return newInvalidTokenToken(
								newIntegerLiteralToken("", radix, sign, initialLineNumber, initialColumnNumber),
								"UnsupportedIntegerRadix", 
								initialLineNumber, 
								initialColumnNumber);

						}
						
					case 's':
						// Scaled decimals
						
						advanceChar();
						digits.Length = 0;
						scaleCount = scanDecimalInteger((char)0, true, digits);
						if (scaleCount > 0) {
							scale = (uint)digits.ToString().integerValueFromDecimalDigits();
							return newScaledDecimalLiteralToken(decimalPrefix, null, scale, sign, initialLineNumber, initialColumnNumber);
						} else {
							return newScaledDecimalLiteralToken(decimalPrefix, null, sign, initialLineNumber, initialColumnNumber);
						}
						
					case '.':
						// Floating point numbers OR scaled decimals OR final integer in a statement
					
						advanceChar();
						digits.Length = 0;

						int suffixCount = scanDecimalInteger((char)0, false, digits);
						String decimalSuffix = digits.ToString();

						if (suffixCount > 0) {
							if (peek(ref continuationChar)) {
								switch (continuationChar) {
									case 's':
										advanceChar();
										digits.Length = 0;
										scaleCount = scanDecimalInteger((char)0, true, digits);
										if (scaleCount > 0) {
											scale = (uint)digits.ToString().integerValueFromDecimalDigits();
											return newScaledDecimalLiteralToken(decimalPrefix, decimalSuffix, scale, sign, initialLineNumber, initialColumnNumber);
										} else {
											return newScaledDecimalLiteralToken(decimalPrefix, decimalSuffix, sign, initialLineNumber, initialColumnNumber);
										}
										
									case 'E':
									case 'e':
										advanceChar();
										prefixValue = decimalPrefix.integerValueFromDecimalDigits();
										suffixValue = decimalSuffix.integerValueFromDecimalDigits();
										exponentCount = parseDecimalInteger(0, ref integerExponent);										
										if (exponentCount > 0) {
											return newSinglePrecisionLiteralToken(sign * ((float)prefixValue + ((float)suffixValue / (float)Math.Pow(10.0f, suffixCount))), integerExponent, initialLineNumber, initialColumnNumber);
										} else {
											return newSinglePrecisionLiteralToken(sign * ((float)prefixValue + ((float)suffixValue / (float)Math.Pow(10.0f, suffixCount))), initialLineNumber, initialColumnNumber);
										}
										
									case 'D':
									case 'd':
										advanceChar();
										prefixValue = decimalPrefix.integerValueFromDecimalDigits();
										suffixValue = decimalSuffix.integerValueFromDecimalDigits();
										exponentCount = parseDecimalInteger(0, ref integerExponent);
										if (exponentCount > 0) {
											return newDoublePrecisionLiteralToken(sign * ((double)prefixValue + ((double)suffixValue / Math.Pow(10.0d, suffixCount))), integerExponent, initialLineNumber, initialColumnNumber);
										} else {
											return newDoublePrecisionLiteralToken(sign * ((double)prefixValue + ((double)suffixValue / Math.Pow(10.0d, suffixCount))), initialLineNumber, initialColumnNumber);
										}
								
									case 'Q':
									case 'q':
										advanceChar();
										prefixValue = decimalPrefix.integerValueFromDecimalDigits();
										suffixValue = decimalSuffix.integerValueFromDecimalDigits();
										exponentCount = parseDecimalInteger(0, ref integerExponent);
										if (exponentCount > 0) {
											return newQuadPrecisionLiteralToken(sign * ((decimal)prefixValue + ((decimal)suffixValue / (decimal)Math.Pow(10.0d, suffixCount))), (int)integerExponent, initialLineNumber, initialColumnNumber);
										} else {
											return newQuadPrecisionLiteralToken(sign * ((decimal)prefixValue + ((decimal)suffixValue / (decimal)Math.Pow(10.0d, suffixCount))), initialLineNumber, initialColumnNumber);
										}
										
									default:
										prefixValue = decimalPrefix.integerValueFromDecimalDigits();
										suffixValue = decimalSuffix.integerValueFromDecimalDigits();
										return newSinglePrecisionLiteralToken(sign * ((float)prefixValue + ((float)suffixValue / (float)Math.Pow(10.0f, suffixCount))), initialLineNumber, initialColumnNumber);
								}
							} else {
								prefixValue = decimalPrefix.integerValueFromDecimalDigits();
								suffixValue = decimalSuffix.integerValueFromDecimalDigits();
								return newSinglePrecisionLiteralToken(sign * ((float)prefixValue + ((float)suffixValue / (float)Math.Pow(10.0f, suffixCount))), initialLineNumber, initialColumnNumber);
							}
						} else {
							enqueueBufferedToken(newStatementEndToken(initialLineNumber, initialColumnNumber));
							return newIntegerLiteralToken(decimalPrefix, sign, initialLineNumber, initialColumnNumber);
						}
						
					case 'e':
					case 'E':
						// Single-precision floating point numbers (no decimal)
						
						advanceChar();
						prefixValue = decimalPrefix.integerValueFromDecimalDigits();
						exponentCount = parseDecimalInteger(0, ref integerExponent);
						if (exponentCount > 0) {
							return newSinglePrecisionLiteralToken(sign * (float)prefixValue, (int)integerExponent, initialLineNumber, initialColumnNumber);
						} else {
							return newSinglePrecisionLiteralToken(sign * (float)prefixValue, initialLineNumber, initialColumnNumber);
						}
						
					case 'd':
					case 'D':
						// Double-precision floating point numbers (no decimal)
						
						advanceChar();
						prefixValue = decimalPrefix.integerValueFromDecimalDigits();
						exponentCount = parseDecimalInteger(0, ref integerExponent);
						if (exponentCount > 0) {
							return newDoublePrecisionLiteralToken(sign * (double)prefixValue, integerExponent, initialLineNumber, initialColumnNumber);
						} else {
							return newDoublePrecisionLiteralToken(sign * (double)prefixValue, initialLineNumber, initialColumnNumber);
						}
						
					case 'q':
					case 'Q':
						// Quad-precision floating point numbers (no decimal)
						
						advanceChar();
						prefixValue = decimalPrefix.integerValueFromDecimalDigits();
						suffixCount = parseDecimalInteger(0, ref integerExponent);
						if (suffixCount > 0) {
							return newQuadPrecisionLiteralToken(sign * (decimal)prefixValue, (int)integerExponent, initialLineNumber, initialColumnNumber);
						} else {
							return newQuadPrecisionLiteralToken(sign * (decimal)prefixValue, initialLineNumber, initialColumnNumber);
						}
				}
				
			} 
			
			return newIntegerLiteralToken(decimalPrefix, sign, initialLineNumber, initialColumnNumber);
			
		}
		
		protected LexicalToken parseComment(uint initialLineNumber, uint initialColumnNumber) {
			bool hasClosingChar = false;
			String comment = nextUntil(delegate (char c) {return hasClosingChar = c == '"';});
			CommentToken commentToken = newCommentToken(comment, initialLineNumber, initialColumnNumber);
			if (hasClosingChar) {
				advanceChar();
				return commentToken;
			} else {
				return newInvalidTokenToken(
					commentToken,
					"UnexpectedEndOfSourceStream", 
					initialLineNumber, 
					initialColumnNumber);
			}
		}
		
		protected LexicalToken parseAssignmentOpOrBlockArg(uint initialLineNumber, uint initialColumnNumber) {
			// @Issue: Disallow nil, true, false, self, super, thisContext
			StringBuilder prefix;
			char ch = (char)0;
			if (peek(ref ch)) {
				switch (ch) {
					case '=':
						advanceChar();
						return newAssignOpToken(initialLineNumber, initialColumnNumber);

				// Possibly the literal constant 'false'
				case 'f':
					advanceChar();
					FalseToken falseToken;
					if (parseFalseLiteralConstant(out falseToken, out prefix, initialLineNumber, initialColumnNumber)) {
						return newInvalidTokenToken(
							falseToken,
							"ReservedIdentifierCannotBeDeclaredAsVariableOrParameter", 
							initialLineNumber, 
							initialColumnNumber);
					} else {
						appendOntoWhile(prefix, ESLexicalUtility.isIdentifierChar);
						return newBlockParameterToken(prefix.ToString(), initialLineNumber, initialColumnNumber);
					}
					
				// Possibly the literal constant 'nil'
				case 'n':
					advanceChar();
					NilToken nilToken;
					if (parseNilLiteralConstant(out nilToken, out prefix, initialLineNumber, initialColumnNumber)) {
						return newInvalidTokenToken(
							nilToken,
							"ReservedIdentifierCannotBeDeclaredAsVariableOrParameter", 
							initialLineNumber, 
							initialColumnNumber);
					} else {
						appendOntoWhile(prefix, ESLexicalUtility.isIdentifierChar);
						return newBlockParameterToken(prefix.ToString(), initialLineNumber, initialColumnNumber);
					}
				
				// Possibly the pseudo-variable 'self' or 'super'
				case 's':
					advanceChar();
					PseudoVariableReferenceToken pseudoVariableReferenceToken;
					if (parseSelfOrSuperPseudoVariable(out pseudoVariableReferenceToken, out prefix, initialLineNumber, initialColumnNumber)) {
						return newInvalidTokenToken(
							pseudoVariableReferenceToken,
							"ReservedIdentifierCannotBeDeclaredAsVariableOrParameter", 
							initialLineNumber, 
							initialColumnNumber);
					} else {
						appendOntoWhile(prefix, ESLexicalUtility.isIdentifierChar);
						return newBlockParameterToken(prefix.ToString(), initialLineNumber, initialColumnNumber);
					}
				
				// Possibly the literal constant 'true' or the pseudo-variable 'thisContext'
				case 't':
					advanceChar();
					TrueToken trueToken;
					ThisContextToken thisContextToken;
					if (parseTrueLiteralConstant(out trueToken, !SupportsThisContextPseudoVariable, out prefix, initialLineNumber, initialColumnNumber)) {
						return newInvalidTokenToken(
							trueToken,
							"ReservedIdentifierCannotBeDeclaredAsVariableOrParameter", 
							initialLineNumber, 
							initialColumnNumber);
					} else {
						if (SupportsThisContextPseudoVariable && (prefix == null || prefix.Length == 1)) {
							if (prefix == null) prefix = new StringBuilder();
							if (parseThisContextPseudoVariable(out thisContextToken, prefix.Length < 1,  prefix, initialLineNumber, initialColumnNumber)) {
								return newInvalidTokenToken(
									thisContextToken,
									"ReservedIdentifierCannotBeDeclaredAsVariableOrParameter", 
									initialLineNumber, 
									initialColumnNumber);
							}
						}
						if (prefix == null) {
							prefix = new StringBuilder();
							prefix.Append("t");
						}
						appendOntoWhile(prefix, ESLexicalUtility.isIdentifierChar);
						return newBlockParameterToken(prefix.ToString(), initialLineNumber, initialColumnNumber);
					}
						
				case '_':
				case 'A':
				case 'a':
				case 'B':
				case 'b':
				case 'C':
				case 'c':
				case 'D':
				case 'd':
				case 'E':
				case 'e':
				case 'F':
				case 'G':
				case 'g':
				case 'H':
				case 'h':
				case 'I':
				case 'i':
				case 'J':
				case 'j':
				case 'K':
				case 'k':
				case 'L':
				case 'l':
				case 'M':
				case 'm':
				case 'N':
				case 'O':
				case 'o':
				case 'P':
				case 'p':
				case 'Q':
				case 'q':
				case 'R':
				case 'r':
				case 'S':
				case 'T':
				case 'U':
				case 'u':
				case 'V':
				case 'v':
				case 'W':
				case 'w':
				case 'X':
				case 'x':
				case 'Y':
				case 'y':
				case 'Z':
				case 'z':
					return newBlockParameterToken(nextWhile(ESLexicalUtility.isIdentifierChar), initialLineNumber, initialColumnNumber);
						
				default:
					return newUnknownTokenToken(
						":",
						ch,
						initialLineNumber, 
						initialColumnNumber);

				}
			} else {
				return newUnknownTokenToken(
					":",
					"",
					initialLineNumber, 
					initialColumnNumber);
			}
					
		}
		
		protected LexicalToken basicParseBinaryMessageSelector(char initialChar, Functor3<LexicalToken, String, uint, uint> createSTToken, uint initialLineNumber, uint initialColumnNumber) {
			char finalChar = (char)0;
			if (peek(ref finalChar)) {
				if (finalChar.isBinaryMessageSelectorChar()) {
					uint numberTokenLineNumber = LineNumber;
					uint numberTokenColumnNumber = ColumnNumber;
					advanceChar();
					char possibleDigitCh = (char)0;
					if (finalChar == '-' && peek(ref possibleDigitCh) && Char.IsDigit(possibleDigitCh)) {
						advanceChar();
						enqueueBufferedToken(parseNumericLiteral(possibleDigitCh, -1, numberTokenLineNumber, numberTokenColumnNumber));
						return createSTToken(new String(initialChar, 1), initialLineNumber, initialColumnNumber);
					}
					StringBuilder sb = new StringBuilder();
					sb.Append(initialChar);
					sb.Append(finalChar);
					return createSTToken(sb.ToString(), initialLineNumber, initialColumnNumber);
				}
			}
			return createSTToken(new String(initialChar, 1), initialLineNumber, initialColumnNumber);
		} 
		
		protected LexicalToken parseBinaryMessageSelector(char initialChar, uint initialLineNumber, uint initialColumnNumber) {
			return basicParseBinaryMessageSelector(
						initialChar, 
						delegate (String stringValue, uint iln, uint icn) {
							return newBinaryMessageSelectorToken(stringValue, iln, icn);
						}, 
						initialLineNumber, 
						initialColumnNumber);
		} 
		
		protected LexicalToken parseNegativeNumericLiteralOrBinaryMessageSelector(uint initialLineNumber, uint initialColumnNumber) {
			char possibleDigitCh = (char)0;
			if (peek(ref possibleDigitCh)) {
				if (Char.IsDigit(possibleDigitCh)) {
					advanceChar();
					return parseNumericLiteral(possibleDigitCh, -1, initialLineNumber, initialColumnNumber);
				}
			}
			return parseBinaryMessageSelector('-', initialLineNumber, initialColumnNumber);
		}
		
		protected LexicalToken parseCharLiteral(uint initialLineNumber, uint initialColumnNumber) {
			char ch = (char)0;
			if (peek(ref ch)) {
				advanceChar();
				return newCharLiteralToken(ch, initialLineNumber, initialColumnNumber);
			} else {
				return newInvalidTokenToken(
					newCharLiteralToken(ch, initialLineNumber, initialColumnNumber),
					"UnexpectedEndOfSourceStream", 
					initialLineNumber, 
					initialColumnNumber);
			}
		}
		
		protected LexicalToken basicParseStringLiteral(char enclosingChar, Functor4<LexicalToken, char, String, uint, uint> createToken, uint initialLineNumber, uint initialColumnNumber) {
			StringBuilder sb = new StringBuilder();
			bool hasClosingChar = false;
			String stringSegment = nextUntil(delegate (char c) {return hasClosingChar = c == enclosingChar;});
			advanceChar();
			sb.Append(stringSegment);
			while (nextMatches(enclosingChar)) {
				sb.Append(enclosingChar);
				stringSegment = nextUntil(delegate (char c) {return hasClosingChar = c == enclosingChar;});
				advanceChar();
				sb.Append(stringSegment);
			}
			return hasClosingChar ?
				createToken(enclosingChar, sb.ToString(), initialLineNumber, initialColumnNumber) :
				newInvalidTokenToken(
					createToken(enclosingChar, sb.ToString(), initialLineNumber, initialColumnNumber),
					"UnexpectedEndOfSourceStream", 
					initialLineNumber, 
					initialColumnNumber);
		}
		
		protected LexicalToken parseStringLiteral(char enclosingChar, uint initialLineNumber, uint initialColumnNumber) {
			return basicParseStringLiteral(
						enclosingChar, 
						delegate (char ec, String stringValue, uint iln, uint icn) {
							return newStringLiteralToken(ec, stringValue, iln, icn);
						}, 
						initialLineNumber, 
						initialColumnNumber);
		}
		
		protected void parseKeywordSequenceOrIdentifier(
					ParsingContext context, 
					StringBuilder prefix, 
					uint maxKeywords, 
					out uint keywordCount, 
					out uint pathElementCount, 
					Action<String> processMissingTerminalKeyword, 
					Action<String> processMissingTerminalPathElement) {
			/* Note: Neither "Blue Book" Smalltalk-80 nor ANSI Smalltalk define any syntax for qualified names. In spite of that, at least two significant
			 * Smalltalk dialects have done so: VisualWorks and Smalltalk-X (there may be others; I'm not an expert in every dialect.) Unfortunately,
			 * the syntax that each uses is different: VisualWorks uses industry-standared dotted pathnames (e.g., "Core.Collections.IdentityDictionary"),
			 * but Smalltalk-X uses '::' for the same purpose (and don't get the '::' confused with a semantically different operator token used in C++.)
			 * 
			 * The point is that the following code has to support both versions of the "qualified name path separator" syntax. And that's the reason for 
			 * the complexity of the code. Sorry about that. You can complain to unknownProgrammer@Cincom and/or Claus.Gittinger@exept.de, if you like.
			 */

			keywordCount = 0;
			char ch;
			int prevKeywordStartIndex = 0;
			uint qualifiedPathElementSeparatorCount = 0;
			bool prevWasKeyword = false;
			bool parseAnotherKeywordOrPathElement = false;
			bool acceptQualifiedNameSyntax = SupportsQualifiedNameSyntax;
			switch (context) {
				default:
				case ParsingContext.General:
					break;
				case ParsingContext.MessageSelector:
				case ParsingContext.VariableDeclaration:
				case ParsingContext.MethodHeader:
					acceptQualifiedNameSyntax = false;
					break;
			}

			bool isZeroCountAcceptable = prefix.Length > 0;

			do {
				int count = appendOntoWhile(prefix, ESLexicalUtility.isIdentifierChar);
				if (acceptQualifiedNameSyntax && nextMatches(QualifiedNameSeparatorChar)) {
					if (count == 0 && !isZeroCountAcceptable) {
						// Two separators in succession
						advanceChar();
						switch (QualifiedNameSeparatorChar) {
							case '.':
								enqueueBufferedToken(newStatementEndToken(LineNumber, ColumnNumber));
								break;
							default:
								ch = (char)0;
								next(ref ch); 
								enqueueBufferedToken(newIllegalCharToken(ch, LineNumber, ColumnNumber));
								parseAnotherKeywordOrPathElement = false;
								break;
						}
						pathElementCount = qualifiedPathElementSeparatorCount + 1;	
						if (processMissingTerminalPathElement != null) processMissingTerminalPathElement(prefix.ToString());
						return;
					} else {
						uint nextTokenLineNumber = LineNumber;
						uint nextTokenColumnNumber = ColumnNumber;
						ch = (char)0;
						switch (QualifiedNameSeparatorChar) {
							default:
								if (peek(ref ch) && ch.isIdentifierChar()) {
									prefix.Append(QualifiedNameSeparatorChar);
									qualifiedPathElementSeparatorCount++;
									parseAnotherKeywordOrPathElement = true;
								} else if (QualifiedNameSeparatorChar == '.') {
									advanceChar();
									enqueueBufferedToken(newStatementEndToken(nextTokenLineNumber, nextTokenColumnNumber));
								}
								break;
							case ':':
								if  (nextMatches(QualifiedNameSeparatorChar)) {
									prefix.Append(QualifiedNameSeparatorChar);
									prefix.Append(QualifiedNameSeparatorChar);
									parseAnotherKeywordOrPathElement = true;
								} else if (nextMatches('=')) {
									enqueueBufferedToken(newAssignOpToken(nextTokenLineNumber, nextTokenColumnNumber));
								}
								break;
						}

					}
				} else if (maxKeywords > 0 && qualifiedPathElementSeparatorCount < 1 && nextMatches(':')) {
					uint assignOpLineNumber = LineNumber;
					uint assignOpColumnNumber = ColumnNumber;
					if (nextMatches('=')) {
						enqueueBufferedToken(newAssignOpToken(assignOpLineNumber, assignOpColumnNumber));
						prevWasKeyword = false;
						parseAnotherKeywordOrPathElement = false;
					} else {
						prefix.Append(":");
						prevKeywordStartIndex = prefix.Length;
						prevWasKeyword = true;
						keywordCount++;
						acceptQualifiedNameSyntax = false;
						parseAnotherKeywordOrPathElement =  keywordCount < maxKeywords;
						if (parseAnotherKeywordOrPathElement) {
							ch = (char)0;
							if (peek(ref ch) && Char.IsLetter(ch)) {
								advanceChar();
								prefix.Append(ch);
							} else {
								parseAnotherKeywordOrPathElement = false;
							}
						}
					}
				} else {
					prevWasKeyword = false;
					parseAnotherKeywordOrPathElement = false;
				}
				isZeroCountAcceptable = false;
			} while (parseAnotherKeywordOrPathElement);

			if (!prevWasKeyword && keywordCount > 0) {
				if (processMissingTerminalKeyword != null) {
					int length = prefix.Length - prevKeywordStartIndex;
					char[] charArray = new char[length];
					prefix.CopyTo(prevKeywordStartIndex, charArray, 0, length);
					String identifier = new String(charArray);
					prefix.Length = prevKeywordStartIndex;
					processMissingTerminalKeyword(prefix.ToString());
				}
			}

			pathElementCount = qualifiedPathElementSeparatorCount + 1;	

		}
		
		protected LexicalToken parseKeywordOrIdentifier(ParsingContext context, StringBuilder prefix, uint initialLineNumber, uint initialColumnNumber) {			
			uint keywordCount;
			uint pathElementCount;
			bool isMissingFinalPathElement = false;
			parseKeywordSequenceOrIdentifier(
				context, 
				prefix, 
				1, 
				out keywordCount, 
				out pathElementCount, 
				null,
				identifierString => isMissingFinalPathElement = true);
			if (keywordCount > 0) {
				var kwt = newKeywordToken(prefix.ToString(), initialLineNumber, initialColumnNumber);
				if (isMissingFinalPathElement) {
					return newInvalidTokenToken(kwt, "The final qualified path element is missing.", kwt.LineNumberStart, kwt.ColumnNumberStart);
				} else {
					return kwt;
				}
			} else {
				var dit = newDeclarableIdentifierToken(prefix.ToString(), pathElementCount, initialLineNumber, initialColumnNumber);
				if (isMissingFinalPathElement) {
					return newInvalidTokenToken(dit, "The final qualified path element is missing.", dit.LineNumberStart, dit.ColumnNumberStart);
				} else {
					return dit;
				}
			}
		}
		
		protected LexicalToken parseKeywordOrIdentifier(ParsingContext context, char initialChar, uint initialLineNumber, uint initialColumnNumber) {
			StringBuilder identifier = new StringBuilder();
			identifier.Append(initialChar);
			return parseKeywordOrIdentifier(context, identifier, initialLineNumber, initialColumnNumber);
		}
		
		protected bool parseFalseLiteralConstant(out FalseToken falseToken, out StringBuilder prefix, uint initialLineNumber, uint initialColumnNumber) {
			if (nextMatches('a')) {
				if (nextMatches('l')) {
					if (nextMatches('s')) {
						if (nextMatches('e')) {
							char ch = (char)0;
							if (!peek(ref ch) || !ch.isIdentifierChar()) {
								prefix = null;
								falseToken = newFalseToken(initialLineNumber, initialColumnNumber);
								return true;
							} else {
								prefix = new StringBuilder();
								prefix.Append("false");
							}
						} else {
							prefix = new StringBuilder();
							prefix.Append("fals");
						}
					} else {
						prefix = new StringBuilder();
						prefix.Append("fal");
					}
				} else {
					prefix = new StringBuilder();
					prefix.Append("fa");
				}
			} else {
				prefix = new StringBuilder();
				prefix.Append('f');
			}
			falseToken = null;
			return false;
		}
		
		protected LexicalToken parseKeywordOrIdentifierOrFalseLiteralConstant(ParsingContext context, uint initialLineNumber, uint initialColumnNumber) {
			FalseToken falseToken = null;
			StringBuilder prefix = null;
			if (parseFalseLiteralConstant(out falseToken, out prefix, initialLineNumber, initialColumnNumber)) {
				return falseToken;
			} else {
				return parseKeywordOrIdentifier(context, prefix, initialLineNumber, initialColumnNumber);
			}
		}
		
		protected LexicalToken parseKeywordOrIdentifierSymbolOrFalseLiteralConstant(uint initialLineNumber, uint initialColumnNumber) {
			FalseToken falseToken = null;
			StringBuilder prefix = null;
			if (parseFalseLiteralConstant(out falseToken, out prefix, initialLineNumber, initialColumnNumber)) {
				return falseToken;
			} else {
				return parseKeywordOrIdentifierSymbol(prefix, true, initialLineNumber, initialColumnNumber);
			}
		}
		
		protected bool parseNilLiteralConstant(out NilToken nilToken, out StringBuilder prefix, uint initialLineNumber, uint initialColumnNumber) {
			if (nextMatches('i')) {
				if (nextMatches('l')) {
					char ch = (char)0;
					if (!peek(ref ch) || !ch.isIdentifierChar()) {
						prefix = null;
						nilToken = newNilToken(initialLineNumber, initialColumnNumber);
						return true;
					} else {
						prefix = new StringBuilder();
						prefix.Append("nil");
					}
				} else {
					prefix = new StringBuilder();
					prefix.Append("ni");
				}
			} else {
				prefix = new StringBuilder();
				prefix.Append('n');
			}
			nilToken = null;
			return false;
		}
		
		protected LexicalToken parseKeywordOrIdentifierOrNilLiteralConstant(ParsingContext context, uint initialLineNumber, uint initialColumnNumber) {
			NilToken nilToken = null;
			StringBuilder prefix = null;
			if (parseNilLiteralConstant(out nilToken, out prefix, initialLineNumber, initialColumnNumber)) {
				return nilToken;
			} else {
				return parseKeywordOrIdentifier(context, prefix, initialLineNumber, initialColumnNumber);
			}
		}
		
		protected LexicalToken parseKeywordOrIdentifierSymbolOrNilLiteralConstant(uint initialLineNumber, uint initialColumnNumber) {
			NilToken nilToken = null;
			StringBuilder prefix = null;
			if (parseNilLiteralConstant(out nilToken, out prefix, initialLineNumber, initialColumnNumber)) {
				return nilToken;
			} else {
				return parseKeywordOrIdentifierSymbol(prefix, true, initialLineNumber, initialColumnNumber);
			}
		}
		
		protected bool parseSelfOrSuperPseudoVariable(out PseudoVariableReferenceToken pseudoVariableToken, out StringBuilder prefix, uint initialLineNumber, uint initialColumnNumber) {
			char ch = (char)0;
			if (peek(ref ch)) {
				switch (ch) {
					case 'e':
						advanceChar();
						if (nextMatches('l')) {
							if (nextMatches('f')) {
								if (!peek(ref ch) || !ch.isIdentifierChar()) {
									prefix = null;
									pseudoVariableToken = newSelfToken(initialLineNumber, initialColumnNumber);
									return true;
								} else {
									prefix = new StringBuilder();
									prefix.Append("self");
								}
							} else {
								prefix = new StringBuilder();
								prefix.Append("sel");
							}
						} else {
							prefix = new StringBuilder();
							prefix.Append("se");
						}
						break;
					case 'u':
						advanceChar();
						if (nextMatches('p')) {
							if (nextMatches('e')) {
								if (nextMatches('r')) {
									if (!peek(ref ch) || !ch.isIdentifierChar()) {
										prefix = null;
										pseudoVariableToken = newSuperToken(initialLineNumber, initialColumnNumber);
										return true;
									} else {
										prefix = new StringBuilder();
										prefix.Append("super");
									}
								} else {
									prefix = new StringBuilder();
									prefix.Append("supe");
								}
							} else {
								prefix = new StringBuilder();
								prefix.Append("sup");
							}
						} else {
							prefix = new StringBuilder();
							prefix.Append("su");
						}
						break;
					default:
						prefix = new StringBuilder();
						prefix.Append('s');
						break;
				}
			} else {
				prefix = new StringBuilder();
				prefix.Append('s');
			}
			pseudoVariableToken = null;
			return false;
		}
		
		protected LexicalToken parseKeywordOrIdentifierOrSelfOrSuperPseudoVariables(ParsingContext context, uint initialLineNumber, uint initialColumnNumber) {
			StringBuilder prefix = null;
			PseudoVariableReferenceToken pseudoVariableToken;
			if (parseSelfOrSuperPseudoVariable(out pseudoVariableToken, out prefix, initialLineNumber, initialColumnNumber)) {
				return pseudoVariableToken;
			} else {
				return parseKeywordOrIdentifier(context, prefix, initialLineNumber, initialColumnNumber);
			}
		}
		
		protected bool parseTrueLiteralConstant(out TrueToken trueToken, bool addLeadingTToPrefix, out StringBuilder prefix, uint initialLineNumber, uint initialColumnNumber) {
			if (nextMatches('r')) {
				if (nextMatches('u')) {
					if (nextMatches('e')) {
						char ch = (char)0;
						if (!peek(ref ch) || !ch.isIdentifierChar()) {
							trueToken = newTrueToken(initialLineNumber, initialColumnNumber);
							prefix = null;
							return true;
						} else {
							prefix = new StringBuilder();
							prefix.Append("true");
						}
					} else {
						prefix = new StringBuilder();
						prefix.Append("tru");
					}
				} else {
					prefix = new StringBuilder();
					prefix.Append("tr");
				}
			} else if (addLeadingTToPrefix) {
				prefix = new StringBuilder();
				prefix.Append('t');
			} else {
				prefix = null;
			}
			trueToken = null;
			return false;
		}
		
		protected bool parseThisContextPseudoVariable(out ThisContextToken thisContextToken, bool addLeadingTToPrefix, StringBuilder prefix, uint initialLineNumber, uint initialColumnNumber) {
			if (SupportsThisContextPseudoVariable) {
				if (nextMatches('h')) {
					if (nextMatches('i')) {
						if (nextMatches('s')) {
							if (nextMatches('C')) {
								if (nextMatches('o')) {
									if (nextMatches('n')) {
										if (nextMatches('t')) {
											if (nextMatches('e')) {
												if (nextMatches('x')) {
													if (nextMatches('t')) {
														char ch = (char)0;
														if (!peek(ref ch) || !ch.isIdentifierChar()) {
															prefix = null;
															thisContextToken = newThisContextToken(initialLineNumber, initialColumnNumber);
															return true;
														} else {
															prefix.Append("thisContext");
														}
													} else {
														prefix.Append("thisContex");
													}
												} else {
													prefix.Append("thisConte");
												}
											} else {
												prefix.Append("thisCont");
											}
										} else {
											prefix.Append("thisCon");
										}
									} else {
										prefix.Append("thisCo");
									}
								} else {
									prefix.Append("thisC");
								}						
							} else {
								prefix.Append("this");
							}
						} else {
							prefix.Append("thi");
						}
					} else {
						prefix.Append("th");
					}				
				} else if (addLeadingTToPrefix) {
					prefix.Append('t');
				} else {
					prefix = null;
				}
			} else if (addLeadingTToPrefix) {
				prefix.Append('t');
			}
			thisContextToken = null;
			return false;
		}
		
		protected LexicalToken parseKeywordOrIdentifierOrTrueLiteralConstantOrThisContextPseudoVariable(ParsingContext context, uint initialLineNumber, uint initialColumnNumber) {
			TrueToken trueToken= null;
			StringBuilder prefix = null;
			if (parseTrueLiteralConstant(out trueToken, !SupportsThisContextPseudoVariable, out prefix, initialLineNumber, initialColumnNumber)) {
				return trueToken;
			}
			if (SupportsThisContextPseudoVariable && (prefix == null || prefix.Length == 1)) {
				ThisContextToken thisContextToken= null;
				if (prefix == null) prefix = new StringBuilder();
				if (parseThisContextPseudoVariable(out thisContextToken, prefix.Length < 1,  prefix, initialLineNumber, initialColumnNumber)) {
					return thisContextToken;				
				}
			}
			if (prefix == null) {
				prefix = new StringBuilder();
				prefix.Append('t');
			}
			return parseKeywordOrIdentifier(context, prefix, initialLineNumber, initialColumnNumber);	
		}
		
		protected LexicalToken parseKeywordOrIdentifierSymbolOrTrueLiteralConstant(uint initialLineNumber, uint initialColumnNumber) {
			TrueToken trueToken= null;
			StringBuilder prefix = null;
			if (parseTrueLiteralConstant(out trueToken, true, out prefix, initialLineNumber, initialColumnNumber)) {
				return trueToken;
			} else {
				return parseKeywordOrIdentifierSymbol(prefix, true, initialLineNumber, initialColumnNumber);
			}
		}
		
		protected LexicalToken parseStringLiteralSymbol(char enclosingChar, uint initialLineNumber, uint initialColumnNumber) {
			return basicParseStringLiteral(
						enclosingChar, 
						delegate (char ec, String stringValue, uint iln, uint icn) {
							return newSymbolLiteralToken(ec, stringValue, SymbolType.String, 0, null, 1, iln, icn);
						}, 
						initialLineNumber, 
						initialColumnNumber);
		}
		
		protected LexicalToken parseBinaryMessageSelectorSymbol(char initialChar, uint initialLineNumber, uint initialColumnNumber) {
			return basicParseBinaryMessageSelector(
						initialChar, 
						delegate (String stringValue, uint iln, uint icn) {
							return newSymbolLiteralToken((char)0, stringValue, SymbolType.BinaryMessageSelector, 1, null, 1, iln, icn);
						}, 
						initialLineNumber, 
						initialColumnNumber);
		}
		
		protected LexicalToken parseKeywordOrIdentifierSymbol(StringBuilder prefix, bool isEnclosedInArrayLiteral, uint initialLineNumber, uint initialColumnNumber) {
			
			uint keywordCount = 0;
			uint pathElementCount = 1;
			bool isMissingFinalPathElement = false;
			parseKeywordSequenceOrIdentifier(
							ParsingContext.General, 
							prefix, 
							uint.MaxValue, 
							out keywordCount,
							out pathElementCount,

							identifierString => 
								enqueueBufferedToken(
									isEnclosedInArrayLiteral ?
										(LexicalToken)
											(newSymbolLiteralToken(
												(char)0, 
												identifierString, 
												SymbolType.Identifier, 
												0, 
												null,
												1,
												initialLineNumber, 
												initialColumnNumber)) :
										(LexicalToken)(newDeclarableIdentifierToken(identifierString, pathElementCount, initialLineNumber, initialColumnNumber))),

							identifierString => isMissingFinalPathElement = true);
			
			var symbolToken = newSymbolLiteralToken(
							(char)0, 
							prefix.ToString(), 
							keywordCount > 0 ? 
								SymbolType.Keyword : 
								SymbolType.Identifier, 
							keywordCount, 
							pathElementCount > 1 ? QualifiedNameSeparatorChar : (char?)null,
							pathElementCount,
							initialLineNumber, 
							initialColumnNumber);

			if (isMissingFinalPathElement) {
				return newInvalidTokenToken(symbolToken, "The final qualified path element is missing.", symbolToken.LineNumberStart, symbolToken.ColumnNumberStart);
			} else {
				return symbolToken;
			}
			
		}
		
		protected LexicalToken parseKeywordOrIdentifierSymbol(char initialChar, bool isEnclosedInArrayLiteral, uint initialLineNumber, uint initialColumnNumber) {
			
			StringBuilder prefix = new StringBuilder();
			prefix.Append(initialChar);
			
			return parseKeywordOrIdentifierSymbol(prefix, isEnclosedInArrayLiteral, initialLineNumber, initialColumnNumber);
			
		}
		
		protected LexicalToken parseLiteralBindingReference(uint initialLineNumber, uint initialColumnNumber) {
			StringBuilder prefix = new StringBuilder();
			uint keywordCount;
			uint pathElementCount;
			bool isMissingFinalPathElement = true;
			parseKeywordSequenceOrIdentifier(
				ParsingContext.General, 
				prefix, 
				0, 
				out keywordCount, 
				out pathElementCount, 
				null,
				identifierString => isMissingFinalPathElement = true);
			LexicalToken token = newLiteralBindingReferenceToken(prefix.ToString(), pathElementCount, initialLineNumber, initialColumnNumber);
			if (isMissingFinalPathElement) {
				return newInvalidTokenToken(token, "The final qualified path element is missing.", token.LineNumberStart, token.ColumnNumberStart);
			}
			advanceWhile(Char.IsWhiteSpace);
			if (nextMatches('}')) {
				return token;
			} else if (HasMoreData) {
				uint icLineNumber = LineNumber;
				uint icColumnNumber = ColumnNumber;
				char illegalChar = (char)0;
				peek(ref illegalChar);
				return newInvalidTokenToken(
						token, 
						"LiteralBindingReferenceNotTerminatedByRightCurlyBrace", 
						newIllegalCharToken(illegalChar, icLineNumber, icColumnNumber),
						LineNumber, 
						ColumnNumber);
			} else {
				return newInvalidTokenToken(
						token, 
						"UnexpectedEndOfSourceStream", 
						LineNumber, 
						ColumnNumber);
			}
		}
		
		protected NumberParsingResult parseByte(out byte value) {
		
			long longValue = 0;
			
			int count = parseDecimalInteger(0, ref longValue);
			
			if (count > 0) {
				if (nextMatches('r')) {
					// Radix Integers
					uint radix = (uint)longValue;
					if (radix <= 36) {
						longValue = 0;
						if (parseInteger(0, radix, ref longValue) > 0) {
							if (longValue < 256) {
								value = (byte)longValue;
								return NumberParsingResult.ValidNumber;
							} else {
								value = 0;
								return NumberParsingResult.Overflow;
							}
						} else {
							value = 0;
							return NumberParsingResult.IntegerRadixNotFollowedByInteger;
						}
					} else {
						value = 0;
						return NumberParsingResult.UnsupportedIntegerRadix;
					}	
				} else {
					// Integers
					if (longValue < 256) {
						value = (byte)longValue;
						return NumberParsingResult.ValidNumber;
					} else {
						value = 0;
						return NumberParsingResult.Overflow;
					}
				}
			}
			
			value = 0;

			return NumberParsingResult.NotANumber;
			
		}
		
		protected LexicalToken parseByteArray(uint initialLineNumber, uint initialColumnNumber) {
			List<byte> arrayElements = new List<byte>();
			List<CommentInNonObjectArray> internalComments = new List<CommentInNonObjectArray>();
			uint beforeIndex = 0;
			
			while (true) {
				int count = advanceWhile(Char.IsWhiteSpace);
				if (!HasMoreData) {
					return newInvalidTokenToken(
							newByteArrayLiteralToken(arrayElements.ToArray(), initialLineNumber, initialColumnNumber), 
							"UnexpectedEndOfSourceStream", 
							initialLineNumber, 
							initialColumnNumber);
				}
				char ch = (char)0;
				if (peek(ref ch) && ch == '"') {
					LexicalToken comment = parseComment(initialLineNumber, initialColumnNumber);
					if (comment != null && comment.IsComment) {
						internalComments.Add(new CommentInNonObjectArray(beforeIndex, (CommentToken)comment));
					} else {
						return newInvalidTokenToken(
								newByteArrayLiteralToken(arrayElements.ToArray(), internalComments, initialLineNumber, initialColumnNumber), 
								"InvalidByteArrayElement", 
								initialLineNumber, 
								initialColumnNumber);
					}
				} else {
					byte byteValue;
					switch (parseByte(out byteValue)) {
						case NumberParsingResult.ValidNumber:
							arrayElements.Add(byteValue);
							beforeIndex++;
							break;
						case NumberParsingResult.Overflow:
							return newInvalidTokenToken(
									newByteArrayLiteralToken(arrayElements.ToArray(), internalComments, initialLineNumber, initialColumnNumber), 
									"ByteArrayElementValueOverflowsMaxByteValue", 
									initialLineNumber, 
									initialColumnNumber);

						case NumberParsingResult.IntegerRadixNotFollowedByInteger:
							return newInvalidTokenToken(
									newByteArrayLiteralToken(arrayElements.ToArray(), internalComments, initialLineNumber, initialColumnNumber), 
									"ByteArrayElementHasRadixNotFollowedByInteger", 
									initialLineNumber, 
									initialColumnNumber);
						case NumberParsingResult.UnsupportedIntegerRadix:
							return newInvalidTokenToken(
									newByteArrayLiteralToken(arrayElements.ToArray(), internalComments, initialLineNumber, initialColumnNumber), 
									"ByteArrayElementHasUnsupportedRadix", 
									initialLineNumber, 
									initialColumnNumber);
						case NumberParsingResult.NotANumber:
							if (nextMatches(']')) {
								return newByteArrayLiteralToken(arrayElements.ToArray(), internalComments, initialLineNumber, initialColumnNumber);
							} else {
								return newInvalidTokenToken(
										newByteArrayLiteralToken(arrayElements.ToArray(), internalComments, initialLineNumber, initialColumnNumber), 
										"InvalidByteArrayElement", 
										initialLineNumber, 
										initialColumnNumber);
							}
							
					}
				}
			}
			
		}
		
		protected LexicalToken nextArrayElementToken() {
			
			LexicalToken bufferedToken = dequeueBufferedToken();
			if (bufferedToken != null) return bufferedToken;
			
			char initialChar = (char)0;
			if (!next(ref initialChar)) return newEndOfSourceToken();
		
			uint initialLineNumber = LineNumber;
			uint initialColumnNumber = ColumnNumber;
						
			switch (initialChar) {
				// Comment
				case '"':
					return parseComment(initialLineNumber, initialColumnNumber);
					
				// Sub-Array
				case '(':
					return parseArray(initialLineNumber, initialColumnNumber);
					
				// Single-character Symbol literals
				case '[': 
				case ']':
				case '{': 
				case '}':
				case '.':
				case '^':
					return newSymbolLiteralToken('\'', new String(initialChar, 1), SymbolType.String, 0, null, 1, initialLineNumber, initialColumnNumber);

					// Binary selectors
				case '|':
				case '~':
				case '!':
				case '@':
				case '%':
				case '&':
				case '*':
				case '=':
				case '\\':
				case '<':
				case '>':
				case ',':
				case '?':
				case '/':
				case '+':
					return parseBinaryMessageSelectorSymbol(initialChar, initialLineNumber, initialColumnNumber);
					
				// Negative numbers or binary selectors
				case '-':
					return parseNegativeNumericLiteralOrBinaryMessageSelector(initialLineNumber, initialColumnNumber);
					
				// Numeric literals
				case '0':
				case '1':
				case '2':
				case '3':
				case '4':
				case '5':
				case '6':
				case '7':
				case '8':
				case '9':
					return parseNumericLiteral(initialChar, 1, initialLineNumber, initialColumnNumber);
					
				// Character literals
				case '$':
					return parseCharLiteral(initialLineNumber, initialColumnNumber);
					
				// String literals
				case '\'':
					return parseStringLiteral(initialChar, initialLineNumber, initialColumnNumber);
					
				// Symbol, byteArray or array or curly-brace structure literals:
				case '#':
					return parseSymbolOrMethodHeaderBeginOrByteArrayOrArrayOrCurlyBraceStructure(true, initialLineNumber, initialColumnNumber);
				
				// False literal constant, or identifier/keyword symbol
				case 'f':
					return parseKeywordOrIdentifierSymbolOrFalseLiteralConstant(initialLineNumber, initialColumnNumber);
					
				// Nil literal constant, or identifier/keyword symbol
				case 'n':
					return parseKeywordOrIdentifierSymbolOrNilLiteralConstant(initialLineNumber, initialColumnNumber);
				
				// True literal constant, or identifier/keyword symbol
				case 't':
					return parseKeywordOrIdentifierSymbolOrTrueLiteralConstant(initialLineNumber, initialColumnNumber);
					
				// Identifier or keyword symbols
				case '_':
				case 'A':
				case 'a':
				case 'B':
				case 'b':
				case 'C':
				case 'c':
				case 'D':
				case 'd':
				case 'E':
				case 'e':
				case 'F':
				case 'G':
				case 'g':
				case 'H':
				case 'h':
				case 'I':
				case 'i':
				case 'J':
				case 'j':
				case 'K':
				case 'k':
				case 'L':
				case 'l':
				case 'M':
				case 'm':
				case 'N':
				case 'O':
				case 'o':
				case 'P':
				case 'p':
				case 'Q':
				case 'q':
				case 'R':
				case 'r':
				case 'S':
				case 's':
				case 'T':
				case 'U':
				case 'u':
				case 'V':
				case 'v':
				case 'W':
				case 'w':
				case 'X':
				case 'x':
				case 'Y':
				case 'y':
				case 'Z':
				case 'z':
					return parseKeywordOrIdentifierSymbol(initialChar, true, initialLineNumber, initialColumnNumber);
					
				// Illegal/Reserved chars
				case '`':
				default:
					return newIllegalCharToken(initialChar, initialLineNumber, initialColumnNumber);
			}
					
		}
		
		protected LexicalToken parseArray(uint initialLineNumber, uint initialColumnNumber) {			
			
			var arrayElements = new List<LexicalLiteralToken>();
			var comments = new List<CommentToken>();
			
			while (true) {
				int count = advanceWhile(Char.IsWhiteSpace);
				if (nextMatches(')')) {
					foreach (CommentToken comment in comments) {
						commentBuffer.Add(comment);
					}
					return newArrayLiteralToken(arrayElements.ToArray(), initialLineNumber, initialColumnNumber);
				}
				
				LexicalToken element = nextArrayElementToken();
				if (element.IsEndOfSource) {
					return newInvalidTokenToken(
							newArrayLiteralToken(arrayElements.ToArray(), initialLineNumber, initialColumnNumber), 
							"UnexpectedEndOfSourceStream", 
							initialLineNumber, 
							initialColumnNumber);
				}			
				if (element.MayOccurAsElementOfArrayLiteral) {
					if (comments.Count > 0) {
						CommentableToken commentableToken = (CommentableToken)element;
						commentableToken.PrecedingCommentsList = comments;
						comments.Clear();
					}
					arrayElements.Add((LexicalLiteralToken)element);
					comments.Clear();
				} else if (element.IsComment) {
					comments.Add((CommentToken)element);
				} else {
					return newInvalidTokenToken(
							element, 
							"InvalidLiteralArrayElement", 
							LineNumber, 
							ColumnNumber);
				}
	
			}
			
		}
		
		protected LexicalToken parseSymbolOrMethodHeaderBeginOrByteArrayOrArrayOrCurlyBraceStructure(bool isEnclosedInArrayLiteral, uint initialLineNumber, uint initialColumnNumber) {

			char ch = (char)0;
			if (peek(ref ch)) {
				switch (ch) {
					
					// Method header begin:
					case '#':
						advanceChar();
						if (SupportsMethodHeaderBeginToken && !isEnclosedInArrayLiteral) {
							return newMethodHeaderBeginToken(initialLineNumber, initialColumnNumber);
						} else {
							return newUnknownTokenToken(
								"#",
								ch,
								initialLineNumber, 
								initialColumnNumber);
						}
						
					// ByteArrays:
					case '[':
						
						advanceChar();
						return parseByteArray(initialLineNumber, initialColumnNumber);
						
					// Arrays:
					case '(':
						
						advanceChar();
						return parseArray(initialLineNumber, initialColumnNumber);
						
					// Curly-brace structure (literal dictionary or LiteralBindingReference):
					case '{':
						
						advanceChar();
						if (SupportsDictionaryLiterals && !isEnclosedInArrayLiteral) {
							return newDictionaryLiteralBeginToken(initialLineNumber, initialColumnNumber);
						} else if (SupportsBindingReferenceLiterals) {
							return parseLiteralBindingReference(initialLineNumber, initialColumnNumber);
						} else {
							return newUnknownTokenToken(
								"#",
								ch,
								initialLineNumber, 
								initialColumnNumber);
						}
					
					// Symbols:
						
					// String literal format:
					case '\'':
						
						advanceChar();
						return parseStringLiteralSymbol(ch, initialLineNumber, initialColumnNumber);
						
					// Binary selector format:
					case '-':
					case '+':
					case '~':
					case '!':
					case '@':
					case '%':
					case '&':
					case '*':
					case '=':
					case '\\':
					case '<':
					case '>':
					case ',':
					case '?':
					case '/':
					case '|':
						
						advanceChar();
						return parseBinaryMessageSelectorSymbol(ch, initialLineNumber, initialColumnNumber);
						
					// Identifier or keyword format:
					case '_':
					case 'A':
					case 'a':
					case 'B':
					case 'b':
					case 'C':
					case 'c':
					case 'D':
					case 'd':
					case 'E':
					case 'e':
					case 'F':
					case 'f':
					case 'G':
					case 'g':
					case 'H':
					case 'h':
					case 'I':
					case 'i':
					case 'J':
					case 'j':
					case 'K':
					case 'k':
					case 'L':
					case 'l':
					case 'M':
					case 'm':
					case 'N':
					case 'n':
					case 'O':
					case 'o':
					case 'P':
					case 'p':
					case 'Q':
					case 'q':
					case 'R':
					case 'r':
					case 'S':
					case 's':
					case 'T':
					case 't':
					case 'U':
					case 'u':
					case 'V':
					case 'v':
					case 'W':
					case 'w':
					case 'X':
					case 'x':
					case 'Y':
					case 'y':
					case 'Z':
					case 'z':
						
						advanceChar();
						return parseKeywordOrIdentifierSymbol(ch, isEnclosedInArrayLiteral, initialLineNumber, initialColumnNumber);
					
					default:
						
						return newUnknownTokenToken(
							"#",
							new String(ch, 1),
							initialLineNumber, 
							initialColumnNumber);

				}	
				
			} 
			
			return newUnknownTokenToken(
				"#",
				"",
				initialLineNumber, 
				initialColumnNumber);


		}
		
		#endregion
		
	}
	
}






