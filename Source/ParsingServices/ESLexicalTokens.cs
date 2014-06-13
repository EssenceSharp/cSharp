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

	public interface LexicalLiteralToken {

		int OccurrenceIndex {
			get;
		}

		uint LineNumberStart {
			get;
		}
			
		uint ColumnNumberStart {
			get;
		}

		uint LineNumberEnd {
			get;
		}
			
		uint ColumnNumberEnd {
			get;
		}
		
		ParseNodeType ParseNodeType {
			get;
		}
		
		ParseNodeType OperandNodeType {
			get;
		}

		bool MayOccurAsElementOfArrayLiteral {
			get;
		}

		Object HostSystemValue {
			get;
		}

		T valueBy<T>(ParseTreeNodeOperation<T> operation);
		
		void esPrintOn(StringBuilder sb);
		
		void esPrintOn(TextWriter stream);
	
		void printTypeUsing(Action<String> append);
		
		void printUsing(Action<String> append, Action<uint> newLine);
		
		void printUsing(uint depth, Action<String> append, Action<uint> newLine);
		
		void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine);

		void printOn(StringBuilder sb);
		
		void printOn(TextWriter stream);

		String AsString {
			get;
		}

		String asPathString(char separator);
	
	}
			
	public abstract class LexicalToken : ParseTreeNode {

		protected uint	lineNumberStart		= 0;
		protected uint	columnNumberStart	= 0;
		protected uint	lineNumberEnd		= 0;
		protected uint	columnNumberEnd		= 0;
		
		/*
		public LexicalToken(uint lineNumber, uint columnNumber) {
			this.lineNumberStart = lineNumber;
			this.columnNumberStart = columnNumber;
		}
		*/

		public LexicalToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base(occurrenceIndex) {
			this.lineNumberStart	= lineNumberStart;
			this.columnNumberStart	= columnNumberStart;
			this.lineNumberEnd	= lineNumberEnd;
			this.columnNumberEnd	= columnNumberEnd;
		}
		
		public override uint LineNumberStart {
			get {return lineNumberStart;}
		}
			
		public override uint ColumnNumberStart {
			get {return columnNumberStart;}
		}
		
		public override uint LineNumberEnd {
			get {return lineNumberEnd;}
		}
			
		public override uint ColumnNumberEnd {
			get {return columnNumberEnd;}
		}
		
		public override bool IsLexical {
			get {return true;}
		}
		
		public virtual bool IsBinaryMessageSelectorChar {
			get {return false;}
		}
		
		public virtual bool MayOccurAsElementOfArrayLiteral {
			get {return false;}
		}
		
		public virtual bool IsComment {
			get {return false;}
		}
		
		public override void printUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out a technical description of the token and its meta-information
			
			append("{<");
			append(ParseNodeType.ToString());
			append("> Line=");
			append(LineNumberStart.ToString());
			append(" column=");
			append(ColumnNumberStart.ToString());
			append(": ");
			esPrintUsing(depth, append, newLine);
			append(" }");
			
		}

	}
	
	public class CommentToken : LexicalToken {
		protected String comment = "";
		
		public CommentToken(String comment, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
			this.comment = comment;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.Comment;}
		}
		
		public override bool IsComment {
			get {return true;}
		}
		
		public override bool TransformsToEpsilon {
			get {return false;} // Because of the case of a transformation back to Smalltalk source code
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			append("\"");
			append(comment);
			append("\"");

		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToCommentToken(this);
		}

	}
	
	public abstract class CommentableToken : LexicalToken {
		protected CommentToken[] precedingComments = null;
		
		public CommentableToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base(occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}

		public CommentableToken(CommentToken[] precedingComments, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base(occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
			this.precedingComments = precedingComments == null ? null : (precedingComments.Length > 0 ? precedingComments : null);
		}
		
		public CommentableToken(List<CommentToken> precedingComments, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : this(precedingComments == null ? null : precedingComments.ToArray(), occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public CommentToken[] PrecedingComments {
			get {return precedingComments;}
			set {precedingComments = value;}
		}
		
		public List<CommentToken> PrecedingCommentsList {
			set {precedingComments = value == null ? null : (value.Count > 0 ? value.ToArray() : null);}
		}

	}
	
	public abstract class MetaToken : CommentableToken {
		
		public MetaToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public override bool IsMeta {
			get {return true;}
		}
		
		public override bool TransformsToEpsilon {
			get {return true;}
		}

	}
	
	public abstract class LexicalErrorToken : MetaToken {
		
		public LexicalErrorToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public override bool RepresentsLexicalError {
			get {return true;}
		}
	
	}
	
	public class IllegalCharToken : LexicalErrorToken {
		protected char illegalChar = ' ';
		
		public IllegalCharToken(char illegalChar, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
			this.illegalChar = illegalChar;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.IllegalChar;}
		}
		
		public char IllegalChar {
			get {return illegalChar;}
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			append(ESLexicalUtility.charValueAsSmalltalk(IllegalChar));
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToIllegalCharToken(this);
		}

	}
	
	public class UnknownTokenToken : LexicalErrorToken {
		// For cases when the intended token is undefined/indeterminate
		protected String validPrefix = null;
		protected String invalidSuffix = null;
		
		public UnknownTokenToken(String validPrefix, String invalidSuffix, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
			this.validPrefix = validPrefix;
			this.invalidSuffix = invalidSuffix;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.UnknownToken;}
		}
		
		public String ValidPrefix {
			get {return validPrefix;}
		}
		
		public String InvalidSuffix {
			get {return invalidSuffix;}
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			append(ValidPrefix);
			append(" -> ");
			append(InvalidSuffix);
			append(" <- (invalid suffix)");
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToUnknownTokenToken(this);

		}

	}
	
	public class InvalidTokenToken : LexicalErrorToken {
		// For cases when the intended token is reasonably sure
		protected LexicalToken invalidToken = null;
		protected String reason = "";
		protected IllegalCharToken illegalCharToken = null;
		
		public InvalidTokenToken(LexicalToken invalidToken, String reason, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
			this.invalidToken = invalidToken;
			this.reason = reason;
		}
		
		public InvalidTokenToken(LexicalToken invalidToken, String reason, IllegalCharToken illegalCharToken, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : this (invalidToken, reason, occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
			this.illegalCharToken = illegalCharToken;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.InvalidToken;}
		}
		
		public LexicalToken InvalidToken {
			get {return invalidToken;}
		}
		
		public IllegalCharToken IllegalCharToken {
			get {return illegalCharToken;}
		}
		
		public bool HasIllegalChar {
			get {return illegalCharToken != null;}
		}
		
		public String Reason {
			get {return reason;}
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			append("Invalid Token[ ");
			append(Reason);
			append("]: ");
			invalidToken.esPrintUsing(append, newLine);
			if (HasIllegalChar) {
				append("(Illegal character: ");
				IllegalCharToken.esPrintUsing(append, newLine);
				append(")");
			}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToInvalidTokenToken(this);
		}
		
	}
	
	public abstract class SourceStructureToken : MetaToken {
		
		public SourceStructureToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
	}
	
	public class EndOfSourceToken : SourceStructureToken {
		
		public EndOfSourceToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.EndOfSource;}
		}
	
		public override bool IsEndOfSource {
			get {return true;}
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			append("<EndOfSource>");

		}

        public override T valueBy<T>(ParseTreeNodeOperation<T> operation)
        {
            return operation.applyToEndOfSourceToken(this);
        }

	}
	
	public abstract class GrammaticalToken : CommentableToken {
		
		public GrammaticalToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
	}
	
	public abstract class PrimitiveGrammaticalToken : GrammaticalToken {
		
		public PrimitiveGrammaticalToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
	}

	public class MethodHeaderBeginToken : PrimitiveGrammaticalToken {
		
		public MethodHeaderBeginToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.MethodHeaderBegin;}
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			append("##");

		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToMethodHeaderBeginToken(this);
		}
		
	}
	
	public class AssignOpToken : PrimitiveGrammaticalToken {
		
		public AssignOpToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.AssignOp;}
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			append(":=");

		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToAssignOpToken(this);
		}
		
	}

	public class ReturnOpToken : PrimitiveGrammaticalToken {
		
		public ReturnOpToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.ReturnOp;}
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			append("^");

		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToReturnOpToken(this);

		}

	}

	public class VerticalBarToken : PrimitiveGrammaticalToken {
		
		public VerticalBarToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.VerticalBar;}
		}
		
		public override bool IsBinaryMessageSelectorChar {
			get {return true;}
		}
		
		public BinaryMessageSelectorToken AsBinaryMessageSelectorToken {
			get {return new BinaryMessageSelectorToken("|", occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd);}
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			append("|");

		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToVerticalBarToken(this);

		}
		
	}
	
	public class ExpressionBeginToken : PrimitiveGrammaticalToken {
		
		public ExpressionBeginToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.ExpressionBegin;}
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			append("(");

		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToExpressionBeginToken(this);
		}
		
	}

	public class ExpressionEndToken : PrimitiveGrammaticalToken {
		
		public ExpressionEndToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.ExpressionEnd;}
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			append(")");

		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToExpressionEndToken(this);
		}
		
	}

	public class BlockBeginToken : PrimitiveGrammaticalToken {
		
		public BlockBeginToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.BlockBegin;}
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			append("[");

		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToBlockBeginToken(this);
		}
		
	}

	public class BlockEndToken : PrimitiveGrammaticalToken {
		
		public BlockEndToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.BlockEnd;}
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			append("]");

		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToBlockEndToken(this);
		}
		
	}

	public class DynamicArrayBeginToken : PrimitiveGrammaticalToken {
		
		public DynamicArrayBeginToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.DynamicArrayBegin;}
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			append("{");

		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToDynamicArrayBeginToken(this);
		}
		
	}
	
	public class DictionaryLiteralBeginToken : PrimitiveGrammaticalToken {
		
		public DictionaryLiteralBeginToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.DictionaryLiteralBegin;}
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			append("#{");

		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToDictionaryLiteralBeginToken(this);
		}
		
	}
	
	public class CurlyBraceStructureEndToken : PrimitiveGrammaticalToken {
		
		public CurlyBraceStructureEndToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.CurlyBraceStructureEnd;}
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			append("}");

		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToCurlyBraceStructureEndToken(this);
		}
		
	}

	public class MessageCascadeOpToken : PrimitiveGrammaticalToken {
		
		public MessageCascadeOpToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.MessageCascadeOp;}
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			append(";");

		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToMessageCascadeOpToken(this);
		}
		
	}
	
	public class StatementEndToken : PrimitiveGrammaticalToken {
		
		public StatementEndToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.StatementEnd;}
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			append(".");

		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToStatementEndToken(this);

		}
		
	}
	
	public abstract class AttributedGrammaticalToken : GrammaticalToken {
		
		public AttributedGrammaticalToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
	}

	public class BinaryMessageSelectorToken : AttributedGrammaticalToken {
		protected String name = null;
		
		public BinaryMessageSelectorToken(String name, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
			this.name = name;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.BinaryMessageSelector;}
		}
		
		public override bool IsBinaryMessageSelectorChar {
			get {return (name != null && name.Length == 1);}
		}

		public String Name {
			get {return name;}
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			append(Name);

		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToBinaryMessageSelectorToken(this);
		}
		
	}
	
	public abstract class IdentifierToken : AttributedGrammaticalToken {
		
		public IdentifierToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.Identifier;}
		}

		public abstract String Name {
			get;
		}

		public String UnarySelector {
			get {return Name;}
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			append(Name);

		}

	}
	
	public class DeclarableIdentifierToken : IdentifierToken {
		protected String	name				= null;
		protected char?		qualifiedNameSeparatorChar	= null;
		
		public DeclarableIdentifierToken(String name, char? qualifiedNameSeparatorChar, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
			this.name = name;
			this.qualifiedNameSeparatorChar = qualifiedNameSeparatorChar;
		}

		public override String Name {
			get {return name;}
		}
		
		public char? QualifiedNameSeparatorChar {
			get {return qualifiedNameSeparatorChar;}
		}

		public override bool CanBeDeclaredAsVariableOrParameter {
			get {return true;}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToDeclarableIdentifierToken(this);
		}	
	
	}
	
	public abstract class PseudoVariableReferenceToken : IdentifierToken {
		
		public PseudoVariableReferenceToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
	}
	
	public class SelfToken : PseudoVariableReferenceToken {
		
		public SelfToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public override ParseNodeType OperandNodeType {
			get {return ParseNodeType.Self;}
		}

		public override String Name {
			get {return "self";}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToSelfToken(this);

		}
		
	}
	
	public class SuperToken : PseudoVariableReferenceToken {
		
		public SuperToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public override ParseNodeType OperandNodeType {
			get {return ParseNodeType.Super;}
		}

		public override String Name {
			get {return "super";}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToSuperToken(this);

		}
		
	}
	
	public class ThisContextToken : PseudoVariableReferenceToken {
		
		public ThisContextToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public override ParseNodeType OperandNodeType {
			get {return ParseNodeType.ThisContext;}
		}

		public override String Name {
			get {return "thisContext";}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToThisContextToken(this);

		}
		
	}
	
	public abstract class ConstantReferenceToken : IdentifierToken, LexicalLiteralToken {
		
		public ConstantReferenceToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public abstract Object HostSystemValue {
			get;
		}
		
		public override bool MayOccurAsElementOfArrayLiteral {
			get {return true;}
		}
		
	}

	public class NilToken : ConstantReferenceToken {

		public NilToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base(occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {}

		public override ParseNodeType OperandNodeType {
			get { return ParseNodeType.Nil; }
		}

		public override String Name {
			get { return "nil"; }
		}
		
		public override Object HostSystemValue {
			get {return null;}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
			return operation.applyToNilToken(this);

		}

	}

	public abstract class BooleanLiteralToken : ConstantReferenceToken {
		
		public BooleanLiteralToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public abstract bool BooleanValue {
			get;
		}
		
	}
	
	public class TrueToken : BooleanLiteralToken {
		
		public TrueToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public override ParseNodeType OperandNodeType {
			get {return ParseNodeType.True;}
		}
			
		public override bool BooleanValue {
			get {return true;}
		}

		public override String Name {
			get {return "true";}
		}
		
		public override Object HostSystemValue {
			get {return true;}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToTrueToken(this);

		}
		
	}
	
	public class FalseToken : BooleanLiteralToken {
		
		public FalseToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public override ParseNodeType OperandNodeType {
			get {return ParseNodeType.False;}
		}
			
		public override bool BooleanValue {
			get {return false;}
		}

		public override String Name {
			get {return "false";}
		}
		
		public override Object HostSystemValue {
			get {return false;}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToFalseToken(this);
		}
		
	}
	
	public class BlockParameterToken : AttributedGrammaticalToken {
		protected String name = null;
		
		public BlockParameterToken(String name, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
			this.name = name;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.BlockParameter;}
		}

		public String Name {
			get {return name;}
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			append(":");
			append(Name);

		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToBlockParameterToken(this);
		}
				
	}
	
	public class KeywordToken : AttributedGrammaticalToken {
		protected String name = null;
		
		public KeywordToken(String name, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
			this.name = name;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.Keyword;}
		}

		public String Name {
			get {return name;}
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			append(Name);

		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToKeywordToken(this);
		}
		
	}

	public abstract class LiteralValueToken : CommentableToken, LexicalLiteralToken {
		
		public LiteralValueToken(int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public override bool MayOccurAsElementOfArrayLiteral {
			get {return true;}
		}
		
		public override bool RepresentsLiteralValue {
			get {return true;}
		}
		
		public abstract Object HostSystemValue {
			get;
		}

	}
	
	public abstract class NumericLiteralToken : LiteralValueToken {
		
		protected int sign = 0;
		
		public NumericLiteralToken(int sign, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
			this.sign = sign < 0 ? -1 : 1;
		}
		
		public int Sign {
			get {return sign;}
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			if (Sign < 0) append("-");
			
		}
		
	}
	
	public class IntegerLiteralToken : NumericLiteralToken {
		protected String digits = null;
		protected uint radix = 10;
		
		public IntegerLiteralToken(String digits, int sign, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (sign, occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
			this.digits = digits;
		}
		
		public IntegerLiteralToken(String digits, uint radix, int sign, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : this(digits, sign, occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
			this.radix = radix;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.Integer;}
		}
			
		public String Digits {
			get {return digits;}
		}
	
		public uint Radix {
			get {return radix;}
		}
		
		public long SmallIntegerValue {
			get {return ESLexicalUtility.integerValueFromDigits(Radix, Digits) * Sign;}
		}
		
		public override Object HostSystemValue {
			get {return ESLexicalUtility.integerValueFromDigits(Radix, Digits);}
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			base.esPrintUsing(depth, append, newLine);
			if (Radix != 10) {
				append(Radix.ToString());
				append("r");
			}
			append(Digits);
		
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToIntegerLiteralToken(this);
		}
				
	}
	
	public class ScaledDecimalLiteralToken : NumericLiteralToken {
		protected String decimalPrefix = null;
		protected String decimalSuffix = null;
		protected int specifiedScale = -1;
		
		protected ScaledDecimalLiteralToken(String decimalPrefix, String decimalSuffix, int specifiedScale, int sign, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (sign, occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
			this.decimalPrefix = decimalPrefix;
			this.decimalSuffix = decimalSuffix != null ? (decimalSuffix.Length > 0 ? decimalSuffix : null) : null;
			this.specifiedScale = specifiedScale;
		}
		
		public ScaledDecimalLiteralToken(String decimalPrefix, String decimalSuffix, uint specifiedScale, int sign, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) 
			: this (decimalPrefix, decimalSuffix, (int)specifiedScale, sign, occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public ScaledDecimalLiteralToken(String decimalPrefix, String decimalSuffix, int sign, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) 
			: this (decimalPrefix, decimalSuffix, -1, sign, occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.ScaledDecimal;}
		}
	
		public String DecimalPrefix {
			get {return decimalPrefix;}
		}
	
		public String DecimalSuffix {
			get {return decimalSuffix;}
		}
	
		public bool HasExplicitScaleSpecification {
			get {return specifiedScale >= 0;}
		}
		
		public uint SpecifiedScale {
			get {return specifiedScale >= 0 ? (uint)specifiedScale : 0;}
		}
		
		public override Object HostSystemValue {
			get {return null;}
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			base.esPrintUsing(append, newLine);
			append(DecimalPrefix);
			if (DecimalSuffix != null) {
				append(".");
				append(DecimalSuffix);
			}
			append("s");
			if (HasExplicitScaleSpecification) {
				append(SpecifiedScale.ToString());

			}
		
		}

        public override T valueBy<T>(ParseTreeNodeOperation<T> operation)
        {
            return operation.applyToScaledDecimalLiteralToken(this);

        }
						
	}
	
	public abstract class FloatingPointLiteralToken : NumericLiteralToken {
		protected int exponent = 1;
		protected bool usesExponentNotation = false;
		
		public FloatingPointLiteralToken(int sign, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (sign, occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public FloatingPointLiteralToken(int exponent, int sign, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (sign, occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
			this.exponent = exponent;
			usesExponentNotation = true;
		}
		
		public int Exponent {
			get {return exponent;}
		}
		
		public bool UsesExponentNotation {
			get {return usesExponentNotation;}
		}
			
		public virtual float FloatValue {
			get {return (float)DoubleValue;}
		}
			
		public virtual double DoubleValue {
			get {return (double)FloatValue;}
		}
			
		public virtual decimal DecimalValue {
			get {return (decimal)DoubleValue;}
		}
		
		public abstract void stPrintBaseValueUsing(Action<String> append);
		
		public virtual void stPrintExponentUsing(Action<String> append) {
			append(Exponent.ToString());
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			stPrintBaseValueUsing(append);
			if (UsesExponentNotation) {
				stPrintExponentUsing(append);
			}
		
		}
								
	}
	
	public class SinglePrecisionLiteralToken : FloatingPointLiteralToken {
		protected float floatValue = 0;
		
		public SinglePrecisionLiteralToken(float floatValue, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (Math.Sign(floatValue), occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
			this.floatValue = floatValue;
		}
		
		public SinglePrecisionLiteralToken(float floatValue, int exponent, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base(exponent, Math.Sign(floatValue), occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
			this.floatValue = floatValue;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.SinglePrecision;}
		}
			
		public override float FloatValue {
			get {return floatValue;}
		}
			
		public override decimal DecimalValue {
			get {return (decimal)FloatValue;}
		}

		public override Object HostSystemValue {
			get {return FloatValue;}
		}
		
		public override void stPrintBaseValueUsing(Action<String> append) {
			append(FloatValue.ToString());
		}
		
		public override void stPrintExponentUsing(Action<String> append) {
			append("e");
			base.stPrintExponentUsing(append);
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToSinglePrecisionLiteralToken(this);

		}
				
	}
	
	public class DoublePrecisionLiteralToken : FloatingPointLiteralToken {
		protected double doubleValue = 0;
		
		public DoublePrecisionLiteralToken(double doubleValue, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (Math.Sign(doubleValue), occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
			this.doubleValue = doubleValue;
		}
		
		public DoublePrecisionLiteralToken(double doubleValue, int exponent, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base(exponent, Math.Sign(doubleValue), occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
			this.doubleValue = doubleValue;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.DoublePrecision;}
		}
			
		public override double DoubleValue {
			get {return doubleValue;}
		}

		public override Object HostSystemValue {
			get {return DoubleValue;}
		}
		
		public override void stPrintBaseValueUsing(Action<String> append) {
			append(DoubleValue.ToString());
			append("d");
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToDoublePrecisionLiteralToken(this);
		}
		
	}
	
	public class QuadPrecisionLiteralToken : FloatingPointLiteralToken {
		protected decimal decimalValue = 0;
		
		public QuadPrecisionLiteralToken(decimal decimalValue, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (1, occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
			this.decimalValue = decimalValue;
		}
		
		public QuadPrecisionLiteralToken(decimal decimalValue, int exponent, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base(exponent, occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
			this.decimalValue = decimalValue;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.QuadPrecision;}
		}
		
		public override void stPrintBaseValueUsing(Action<String> append) {
			append(DoubleValue.ToString());
			append("q");
		}
			
		public override decimal DecimalValue {
			get {return decimalValue;}
		}

		public override Object HostSystemValue {
			get {return DecimalValue;}
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToQuadPrecisionLiteralToken(this);

		}
		
	}
	
	public class CharLiteralToken : LiteralValueToken {
		protected char charValue = ' ';

		public CharLiteralToken(char charValue, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
			this.charValue = charValue;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.Char;}
		}
			
		public char CharValue {
			get {return charValue;}
		}

		public override Object HostSystemValue {
			get {return CharValue;}
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			append(ESLexicalUtility.charValueAsSmalltalk(CharValue));
		
		}

		public override String asPathString(char separator) {
			return new string(new char[] {charValue});
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToCharLiteralToken(this);
		}
		
	}
	
	public abstract class CharArrayLiteralToken : LiteralValueToken {
		
		public String stLexicalStringFromString(String stringValue, char enclosingChar) {
			StringBuilder sb = new StringBuilder();
			foreach (char c in stringValue) {
				if (c == enclosingChar) sb.Append(c);
				sb.Append(c);
			}
			return sb.ToString();
		}

		protected String stringValue = "";
		
		public CharArrayLiteralToken(String stringValue, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
			this.stringValue = stringValue;
		}

		public String StringValue {
			get {return stringValue;}
		}

		public override String asPathString(char separator) {
			return StringValue;
		}

	}

	public class StringLiteralToken : CharArrayLiteralToken {
		protected char enclosingChar = '\'';
		protected String enclosingCharString = "'";
		
		public StringLiteralToken(char enclosingChar, String stringValue, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (stringValue, occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
			this.enclosingChar = enclosingChar;
			enclosingCharString = enclosingChar != '\'' ? new String(enclosingChar, 1) : "'";
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.String;}
		}
		
		public char EnclosingChar {
			get {return enclosingChar;}
		}
		
		public String EnclosingCharString {
			get {return enclosingCharString;}
		}

		public override Object HostSystemValue {
			get {return StringValue;}
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			append(EnclosingCharString);
			append(stLexicalStringFromString(StringValue, EnclosingChar));
			append(EnclosingCharString);
		
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToStringLiteralToken(this);

		}
					
	}
	
	public class SymbolLiteralToken : StringLiteralToken {
		protected SymbolType symbolType = SymbolType.Identifier;
		protected uint numArgs				= 0;
		protected char? qualifiedNameSeparatorChar	= null;
		protected uint pathElementCount			= 1;
		
		public SymbolLiteralToken(char enclosingChar, String stringValue, SymbolType symbolType, uint numArgs, char? qualifiedNameSeparatorChar, uint pathElementCount, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base(enclosingChar, stringValue, occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
			this.symbolType				= symbolType;
			this.numArgs				= numArgs;
			this.qualifiedNameSeparatorChar		= qualifiedNameSeparatorChar;
			this.pathElementCount			= pathElementCount;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.Symbol;}
		}

		public SymbolType SymbolType {
			get {return symbolType;}
		}
			
		public uint NumArgs {
			get {return numArgs;}
		}
		
		public char? QualifiedNameSeparatorChar {
			get {return qualifiedNameSeparatorChar;}
		}
			
		public uint PathElementCount {
			get {return pathElementCount;}
		}

		public override Object HostSystemValue {
			get {return StringValue;}
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			append("#");
			
			switch (SymbolType) {
				default:
				case SymbolType.BinaryMessageSelector:
				case SymbolType.Identifier:
				case SymbolType.Keyword:
					append(StringValue);
					break;
				case SymbolType.String:
					base.esPrintUsing(append, newLine);
					break;
			}
		
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToSymbolLiteralToken(this);

		}
							
	} 
	
	public class LiteralBindingReferenceToken : CharArrayLiteralToken {
		
		protected uint pathElementCount = 1;

		public LiteralBindingReferenceToken(String stringValue, uint pathElementCount, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base(stringValue, occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
			this.pathElementCount = pathElementCount;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.LiteralBindingReference;}
		}

		public override Object HostSystemValue {
			get {return StringValue;}
		}
		
		public uint PathElementCount {
			get {return pathElementCount;}
		}

		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			append("#{");
			append(StringValue);
			append("}");
		
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToLiteralBindingReferenceToken(this);
		}
		
	} 
	
	public class CommentInNonObjectArray {
		protected uint beforeIndex = 0;
		protected CommentToken commentToken = null;
		
		public CommentInNonObjectArray(uint beforeIndex, CommentToken commentToken) {
			this.beforeIndex = beforeIndex;
			this.commentToken = commentToken;
		}
		
		public uint BeforeIndex {
			get {return beforeIndex;}
		}
		
		public CommentToken CommentToken {
			get {return commentToken;}
		}
		
	}
	
	public class ByteArrayLiteralToken : LiteralValueToken {
		protected byte[] byteArray = null;
		protected CommentInNonObjectArray[] internalComments = null;
		
		public ByteArrayLiteralToken(byte[] byteArray, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
			this.byteArray = byteArray == null ? new byte[0] : byteArray;
		}
		
		public ByteArrayLiteralToken(byte[] byteArray, CommentInNonObjectArray[] internalComments, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : this (byteArray, occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
			this.internalComments = internalComments == null ? null : (internalComments.Length > 0 ? internalComments : null);
		}
		
		public ByteArrayLiteralToken(byte[] byteArray, List<CommentInNonObjectArray> internalComments, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) 
					: this (byteArray, internalComments == null ? null : internalComments.ToArray(), occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.ByteArray;}
		}
		
		public byte[] ByteArray {
			get {return byteArray;}
		}
		
		public CommentInNonObjectArray[] InternalComments {
			get {return internalComments;}
		}

		public override Object HostSystemValue {
			get {return ByteArray;}
		}
		
		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			append("#[");
			int finalIndex = ByteArray.Length - 1;
			for (int i = 0; i < finalIndex; i++) {
				byte value = ByteArray[i];
				append(value.ToString());
				append(" ");
			}
			if (finalIndex >= 0) {
				append(ByteArray[finalIndex].ToString());
			}
			append("]");
		
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToByteArrayLiteralToken(this);
		}
		
	}
	
	public class ArrayLiteralToken : LiteralValueToken {
		protected LexicalLiteralToken[] arrayElements = null;
			
		public ArrayLiteralToken(LexicalLiteralToken[] arrayElements, int occurrenceIndex, uint lineNumberStart, uint columnNumberStart, uint lineNumberEnd, uint columnNumberEnd) : base (occurrenceIndex, lineNumberStart, columnNumberStart, lineNumberEnd, columnNumberEnd) {
			this.arrayElements = arrayElements;
		}
		
		public override ParseNodeType ParseNodeType {
			get {return ParseNodeType.Array;}
		}
		
		public LexicalLiteralToken[] ArrayElements {
			get {return arrayElements;}
		}

		public long Size {
			get {return arrayElements == null ? 0 : arrayElements.Length;}
		}

		public Object[] ArrayValue {
			get {var array = new Object[Size];
			     if (array.Length == 0) return array;
			     for (var i = 0; i < array.Length; i++) array[i] = arrayElements[i].HostSystemValue;
			     return array;}
		}

		public override Object HostSystemValue {
			get {return ArrayValue;}
		}
		
		public void elementsDo(Action<LexicalLiteralToken> enumerator1) {
			if (arrayElements == null) return;
			foreach (var element in arrayElements) enumerator1(element);
		}

		public override void esPrintUsing(uint depth, Action<String> append, Action<uint> newLine) {
			// Print out canonical Smalltalk literal representation
			
			append("#(");
			int finalIndex = ArrayElements.Length - 1;
			for (int i = 0; i < finalIndex; i++) {
				LexicalToken value = (LexicalToken)ArrayElements[i];
				value.esPrintUsing(append, newLine);
				append(" ");
			}
			if (finalIndex >= 0) {
				((LexicalToken)ArrayElements[finalIndex]).esPrintUsing(append, newLine);
			}
			append(")");
		
		}

		public override T valueBy<T>(ParseTreeNodeOperation<T> operation) {
		    return operation.applyToArrayLiteralToken(this);
		}
		
	}

}
