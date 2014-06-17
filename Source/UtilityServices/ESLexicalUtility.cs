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
using System.IO;
using System.Globalization;
#if CLR2
using FuncNs = Microsoft.Scripting.Utils;
#else
using FuncNs = System;
#endif
using EssenceSharp.Exceptions;
#endregion

namespace EssenceSharp.UtilityServices {
		
	public static class ESLexicalUtility {

		public static String compose(this String[] path, String separatorString) {
			var sb = new StringBuilder();
			printOn(path, sb, separatorString);
			return sb.ToString();
		}

		public static void printOn(this String[] path, StringBuilder sb, String separatorString) {
			printOn(path, sb, separatorString, null);
		}

		public static void printOn(this String[] path, StringBuilder sb, String separatorString, Functor1<String> transformer) {
			if (path == null) return;
			long size = path.Length;
			if (size < 1) return;
			long lastIndex = size - 1;
			String element;
			if (transformer == null) {
				for (var i = 0; i < lastIndex; i++) {
					element = path[i];
					if (!String.IsNullOrEmpty(element)) {
						sb.Append(element);
						if (!String.IsNullOrEmpty(separatorString)) sb.Append(separatorString);
					}
				}
				element = path[lastIndex];
				if (!String.IsNullOrEmpty(element)) sb.Append(element);
			} else {
				for (var i = 0; i < lastIndex; i++) {
					element = path[i];
					if (!String.IsNullOrEmpty(element)) {
						sb.Append(transformer(element));
						if (!String.IsNullOrEmpty(separatorString)) sb.Append(separatorString);
					}
				}
				element = path[lastIndex];
				if (!String.IsNullOrEmpty(element)) sb.Append(transformer(element));
			}
		}

		private static readonly Dictionary<char, String> pseudonymForSymbolChar = new Dictionary<char, String>();
		private static readonly Dictionary<String, char> symbolCharForPseudonym = new Dictionary<String, char>();

		static ESLexicalUtility() {

			// For encoding symbol characters as filename characters.
			pseudonymForSymbolChar['"'] = "[dq]";
			pseudonymForSymbolChar['<'] = "[lt]";
			pseudonymForSymbolChar['>'] = "[gt]";
			pseudonymForSymbolChar['|'] = "[br]";
			pseudonymForSymbolChar[':'] = "[cn]";
			pseudonymForSymbolChar['*'] = "[st]";
			pseudonymForSymbolChar['?'] = "[qm]";
			pseudonymForSymbolChar['\\'] = "[bs]";
			pseudonymForSymbolChar['/'] = "[sl]";

			for (var ch = (char)0; ch < 32; ch++) {
				StringBuilder sb = new StringBuilder();
				sb.Append('[');
				pseudonymForSymbolChar[ch] = ((int)ch).ToString("D2");
				sb.Append(']');
			}

			// For decoding symbol char pseudonyms to their nonimal values from their encoded pseudonyms.
			symbolCharForPseudonym["dq"] = '"';
			symbolCharForPseudonym["lt"] = '<';
			symbolCharForPseudonym["gt"] = '>';
			symbolCharForPseudonym["br"] = '|';
			symbolCharForPseudonym["cn"] = ':';
			symbolCharForPseudonym["st"] = '*';
			symbolCharForPseudonym["qm"] = '?';
			symbolCharForPseudonym["bs"] = '\\';
			symbolCharForPseudonym["sl"] = '/';

			for (var ch = (char)0; ch < 32; ch++) {
				var pseudonym = ((int)ch).ToString("D2");
				symbolCharForPseudonym[pseudonym] = ch;
			}

		}

		public static String encodedAsFilename(this String symbolString) {
			StringBuilder sb = new StringBuilder();
			foreach (var ch in symbolString) {
				String pseudonym;
				if (pseudonymForSymbolChar.TryGetValue(ch, out pseudonym)) {
					sb.Append(pseudonym);
				} else {
					sb.Append(ch);
				}
			}
			return sb.ToString();
		}

		public static String decodedFromFilename(this String filename) {
			var stream = new StringReader(filename);
			StringBuilder decodedStringBuilder = new StringBuilder();
			StringBuilder pseudonymBuilder;
			int next;
			do {
				appendOntoUntil(stream, decodedStringBuilder, ch => ch == '[');
				next = stream.Read();
				if (next == -1) return decodedStringBuilder.ToString();
				pseudonymBuilder = new StringBuilder();
				appendOntoUntil(stream, pseudonymBuilder, ch => ch == ']');
				next = stream.Read();
				if (next == -1) {
					decodedStringBuilder.Append(pseudonymBuilder);
					return decodedStringBuilder.ToString();
				}
				var pseudonym = pseudonymBuilder.ToString();
				char originalChar;
				if (symbolCharForPseudonym.TryGetValue(pseudonym, out originalChar)) {
					decodedStringBuilder.Append(originalChar);
				} else {
					decodedStringBuilder.Append('[');
					decodedStringBuilder.Append(pseudonymBuilder);
					decodedStringBuilder.Append(']');
				}
			} while (stream.Peek() != -1);
			return decodedStringBuilder.ToString();
		}

		public static bool peekFor(this TextReader stream, char ch) {
			int c = stream.Peek();
			if ((char)c == ch) {
				stream.Read();
				return true;
			} else {
				return false;
			}
		}
			
		public static bool peekFor(this TextReader stream, Predicate<char> chTest) {
			int c = stream.Peek();
			if (chTest((char)c)) {
				stream.Read();
				return true;
			} else {
				return false;
			}
		}
		
		public static bool nextMatches(this TextReader stream, char ch) {
			int c = stream.Peek();
			if (c == ch) {
				stream.Read();
				return true;
			} else {
				return false;
			}
		}
		
		public static bool nextSatisfies(this TextReader stream, Predicate<char> charTest) {
			int c = stream.Peek();
			if (charTest((char)c)) {
				stream.Read();
				return true;
			} else {
				return false;
			}
		}
			
		public static int skipOverUntil(this TextReader stream, Predicate<char> chTest) {
			int count = 0;
			int c = stream.Peek();
			while (c != -1) {
				if (chTest((char)c)) return count;
				stream.Read();
				count++;
				c = stream.Peek();
			}
			return count;
		}
			
		public static int skipOverWhile(this TextReader stream, Predicate<char> chTest) {
			int count = 0;
			int c = stream.Peek();
			while (c != -1) {
				if (!chTest((char)c)) return count;
				stream.Read();
				count++;
				c = stream.Peek();
			}
			return count;
		}
			
		public static int skipWhiteSpace(this TextReader stream) {
			return skipOverWhile(stream, Char.IsWhiteSpace);
		}
			
		public static int skipSeparators(this TextReader stream) {
			return skipOverWhile(stream,  Char.IsSeparator);
		}
			
		public static bool skipToNextLineOf(this TextReader stream) {
			stream.ReadLine(); 
			return stream.Peek() != -1;
		}
			
		public static int appendOntoUntil(this TextReader stream, StringBuilder target, Predicate<char> chTest) {
			int count = 0;
			int c = stream.Peek();
			while (c != -1) {
				if (chTest((char)c)) return count;
				target.Append((char)stream.Read());
				count++;
				c = stream.Peek();
			}
			return count;
		}
			
		public static int appendOntoWhile(this TextReader stream, StringBuilder target, Predicate<char> chTest) {
			int count = 0;
			int c = stream.Peek();
			while (c != -1) {
				if (chTest((char)c)) target.Append((char)stream.Read());
				else return count;
				count++;
				c = stream.Peek();
			}
			return count;
		}
			
		public static String nextUntil(this TextReader stream, Predicate<char> chTest) {
			StringBuilder target = new StringBuilder();
			if (appendOntoUntil(stream, target, chTest) > 0) return target.ToString();
			return null;
		}
			
		public static String nextWhile(this TextReader stream, Predicate<char> chTest) {
			StringBuilder target = new StringBuilder();
			if (appendOntoWhile(stream, target, chTest) > 0) return target.ToString();
			return null;
		}
			
		public static String nextNonBlank(this TextReader stream) {
			int c;
			skipWhiteSpace(stream);
			c = stream.Peek();
			if (c == -1) return null;
			return nextUntil(stream, Char.IsWhiteSpace);
		}
			
		public static String nextToken(this TextReader stream, Predicate<char> isTokenPrefix, Predicate<char> isTokenInitialChar, Predicate<char> isTokenContinuationChar) {
			int c;
			StringBuilder target;
				
			skipOverWhile(stream, isTokenPrefix);
			c = stream.Peek();
			if (c == -1) return null;
			if (isTokenInitialChar == null) {
				return nextWhile(stream, isTokenContinuationChar);
			} else if (isTokenInitialChar((char)c)) {
				target = new StringBuilder();
				target.Append((char)stream.Read());
				appendOntoWhile(stream, target, isTokenContinuationChar);
				return target.ToString();
			} else return "";
		}
			
		public static String nextIdentifier(this TextReader stream) {
			return 
				nextToken(
					stream, 
					Char.IsWhiteSpace, 
					isInitialIdentifierChar, 
					isIdentifierChar);
		}
			
		public static String nextQualifiedIdentifier(this TextReader stream) {

			var element = nextIdentifier(stream);
			if (element == null) return element; 
			
			List<String>	qualifiedNameElements	= null;
			var		c			= stream.Peek();
			var		ch			= (char)c;

			if (ch == '.') {
				qualifiedNameElements = new List<String>();
				while (nextMatches(stream, '.')) {
					if (element.Length < 1) {
						var prefix = compose(qualifiedNameElements.ToArray(), ".");
						throw new InvalidArgumentException("Qualified identifier path element cannot have a length of zero. Check for unintentional duplication of the separator character ('.'). Prefix = " + prefix);
					}
					qualifiedNameElements.Add(element);
					element = stream.nextIdentifier();
					if (element == null) {
						var prefix = compose(qualifiedNameElements.ToArray(), ".");
						throw new InvalidArgumentException("Qualified identifier name cannot end with a period. Prefix = " + prefix);
					}
				}
				qualifiedNameElements.Add(element);
				return compose(qualifiedNameElements.ToArray(), ".");
			} else {
				return element;
			}
		}

		public static long? nextInteger(this TextReader stream) {
			String token =
				stream.nextToken(
					Char.IsWhiteSpace, 
					aChar => aChar == '-' || Char.IsDigit(aChar), 
					Char.IsDigit);
			return String.IsNullOrEmpty(token) ? null : (long?)long.Parse(token, NumberStyles.AllowLeadingSign);
		}

		public static ulong? nextUnsignedInteger(this TextReader stream) {
			String token =
				nextToken(
					stream, 
					Char.IsWhiteSpace, 
					Char.IsDigit, 
					Char.IsDigit);
			return String.IsNullOrEmpty(token) ? null : (ulong?)ulong.Parse(token, NumberStyles.AllowLeadingWhite);
		}
			
		public static double? nextRealNumber(this TextReader stream) {
			int c;
			char ch;
			StringBuilder token;
				
			skipOverWhile(stream, Char.IsWhiteSpace);
			c = stream.Peek();
			if (c == -1) return null;
			token = new StringBuilder();
			ch = (char)c;
			if (ch == '-' || ch == '+') token.Append((char)stream.Read());
			if (appendOntoWhile(stream, token, Char.IsDigit) > 0) {
				if (peekFor(stream, '.')) {
					token.Append('.');
					appendOntoWhile(stream, token, Char.IsDigit);
					if (peekFor(stream, 'E')) {
						token.Append('E');
						appendOntoWhile(stream, token, Char.IsDigit);
					}
				}
				return (double?)double.Parse(token.ToString(), NumberStyles.AllowLeadingSign | NumberStyles.AllowDecimalPoint | NumberStyles.AllowExponent);
			} return null;
		}

		public static bool anySatisfy(this String value, Predicate<char> chTest) {
			if (value == null) return false;
			foreach (var ch in value) {
				if (chTest(ch)) return true;
			}
			return false;
		}
		
		public static bool allSatisfy(this String value, Predicate<char> chTest) {
			if (value == null) return false;
			foreach (var ch in value) {
				if (!chTest(ch)) return false;
			}
			return true;
		}
		
		public static String charValueInSmalltalkFormat(this char value) {
			if (Char.IsLetterOrDigit(value) || Char.IsPunctuation(value)) {
				return "$" + new String(value, 1);
			} else {
				switch ((uint)value) {
					case 8:
						return "Character backspace";
					case 9:
						return "Character tab";
					case 10:
						return "Character lf";
					case 12:
						return "Character newPage";
					case 13:
						return "Character cr";
					case 27:
						return "Character esc";
					case 32:
						return "Character space";
					case 127:
						return "Character delete";
					default:
						return ((uint)value).ToString() + " asCharacter";
				}
			}
		}
		
		public static int decimalDigitValue(this char ch) {
			switch (ch) {
					
				case '0':
					return 0;
				case '1':
					return 1;
				case '2':
					return 2;
				case '3':
					return 3;
				case '4':
					return 4;
				case '5':
					return 5;
				case '6':
					return 6;
				case '7':
					return 7;
				case '8':
					return 8;
				case '9':
					return 9;
				default:
					return -1;
			}
		}
		
		public static bool isDigit(this char ch, int numberBase) {
			switch (ch) {
					
				case '0':
					return 0 < numberBase;
				case '1':
					return 1 < numberBase;
				case '2':
					return 2 < numberBase;
				case '3':
					return 3 < numberBase;
				case '4':
					return 4 < numberBase;
				case '5':
					return 5 < numberBase;
				case '6':
					return 6 < numberBase;
				case '7':
					return 7 < numberBase;
				case '8':
					return 8 < numberBase;
				case '9':
					return 9 < numberBase;
					
				case 'A':
				case 'a':
					return 10 < numberBase;
				case 'B':
				case 'b':
					return 11 < numberBase;
				case 'C':
				case 'c':
					return 12 < numberBase;
				case 'D':
				case 'd':
					return 13 < numberBase;
				case 'E':
				case 'e':
					return 14 < numberBase;
				case 'F':
				case 'f':
					return 15 < numberBase;
				case 'G':
				case 'g':
					return 16 < numberBase;
				case 'H':
				case 'h':
					return 17 < numberBase;
				case 'I':
				case 'i':
					return 18 < numberBase;
				case 'J':
				case 'j':
					return 19 < numberBase;
				case 'K':
				case 'k':
					return 20 < numberBase;
				case 'L':
				case 'l':
					return 21 < numberBase;
				case 'M':
				case 'm':
					return 22 < numberBase;
				case 'N':
				case 'n':
					return 23 < numberBase;
				case 'O':
				case 'o':
					return 24 < numberBase;
				case 'P':
				case 'p':
					return 25 < numberBase;
				case 'Q':
				case 'q':
					return 26 < numberBase;
				case 'R':
				case 'r':
					return 27 < numberBase;
				case 'S':
				case 's':
					return 28 < numberBase;
				case 'T':
				case 't':
					return 29 < numberBase;
				case 'U':
				case 'u':
					return 30 < numberBase;
				case 'V':
				case 'v':
					return 31 < numberBase;
				case 'W':
				case 'w':
					return 32 < numberBase;
				case 'X':
				case 'x':
					return 33 < numberBase;
				case 'Y':
				case 'y':
					return 34 < numberBase;
				case 'Z':
				case 'z':
					return 35 < numberBase;

				default:
					return false;
			}
		}

		public static bool isHexadecimalDigit(this char ch) {
			switch (ch) {
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
					return true;
				default:
					return false;
			}
		}
		
		public static int digitValue(this char ch) {
			switch (ch) {
					
				case '0':
					return 0;
				case '1':
					return 1;
				case '2':
					return 2;
				case '3':
					return 3;
				case '4':
					return 4;
				case '5':
					return 5;
				case '6':
					return 6;
				case '7':
					return 7;
				case '8':
					return 8;
				case '9':
					return 9;
					
				case 'A':
				case 'a':
					return 10;
				case 'B':
				case 'b':
					return 11;
				case 'C':
				case 'c':
					return 12;
				case 'D':
				case 'd':
					return 13;
				case 'E':
				case 'e':
					return 14;
				case 'F':
				case 'f':
					return 15;
				case 'G':
				case 'g':
					return 16;
				case 'H':
				case 'h':
					return 17;
				case 'I':
				case 'i':
					return 18;
				case 'J':
				case 'j':
					return 19;
				case 'K':
				case 'k':
					return 20;
				case 'L':
				case 'l':
					return 21;
				case 'M':
				case 'm':
					return 22;
				case 'N':
				case 'n':
					return 23;
				case 'O':
				case 'o':
					return 24;
				case 'P':
				case 'p':
					return 25;
				case 'Q':
				case 'q':
					return 26;
				case 'R':
				case 'r':
					return 27;
				case 'S':
				case 's':
					return 28;
				case 'T':
				case 't':
					return 29;
				case 'U':
				case 'u':
					return 30;
				case 'V':
				case 'v':
					return 31;
				case 'W':
				case 'w':
					return 32;
				case 'X':
				case 'x':
					return 33;
				case 'Y':
				case 'y':
					return 34;
				case 'Z':
				case 'z':
					return 35;

				default:
					return -1;
			}
		}
		
		public static long integerValueFromDecimalDigits(this String digits) {
			// UNCHECKED!!!  Assumes all digits are between '0' and '9' (inclusive)
			long value = 0;
			foreach (char ch in digits) {
				long digitVal = decimalDigitValue(ch);
				value = value * 10 + digitVal; 
			}
			return value;
		}
		
		public static long integerValueFromDigits(this String digits, uint numberBase) {
			// UNCHECKED!!!  Assumes all digits are between '0' and (char)(numberBase - 1) (inclusive), AND that numberBase > 1 and <= 36.
			long value = 0;
			foreach (char ch in digits) {
				long digitVal = digitValue(ch);
				if (digitVal < numberBase) value = value * numberBase + digitVal; 
			}
			return value;
		}
		
		public static bool isBinaryMessageSelectorChar(this char ch) {
			switch (ch) {
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
				case '-': // Ambiguous: could also be the first character of a negative number
				case '|': // Ambiguous: Could also be a variable declaration section enclosing character.
					return true;
				default:
					return false;
			}
		}
		
		public static bool isAmbiguousBinaryMessageSelectorChar(this char ch) {
			switch (ch) {
				case '-': // Ambiguous: could also be the first character of a negative number
				case '|': // Ambiguous: Could also be a variable declaration section enclosing character.
					return true;
				default:
					return false;
			}
		}
		
		public static bool isUnambiguousBinaryMessageSelectorChar(this char ch) {
			switch (ch) {
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
					return true;
				default:
					return false;
			}
		}
		
		public static bool isInitialIdentifierChar(this char ch) {
			switch (ch) {
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
					return true;
				default:
					return false;
			}
		}
		
		public static bool isIdentifierChar(this char ch) {
			switch (ch) {
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
					return true;
				default:
					return false;
			}
		}

		public static String[]  elementsFromString(this String pathString, char separatorChar, FuncNs.Func<Object, Object> transformer) {
			return elementsFromStream(new StringReader(pathString), separatorChar, transformer);
		}

		public static String[] elementsFromStream(this TextReader stream, char separatorChar, FuncNs.Func<Object, Object> transformer) {
			List<String> segments = new List<string>();
			StringBuilder segment = new StringBuilder();
			while(true) {
				int next = stream.Peek();
				while (next != -1 && next != separatorChar) {
					segment.Append((char)stream.Read());
					next = stream.Peek();
				}
				String element = transformer == null ? segment.ToString() : (String)transformer(segment.ToString());
				if (!String.IsNullOrEmpty(element)) segments.Add(element);
				if (next == -1) {
					return segments.ToArray();
				}
				segment = new StringBuilder(); // Should be segment.Clear(), but that's not supported in .Net 3.5
				stream.Read();
			}
		}

		public static String[]  elementsFromString(this String pathString, char[] separatorChars, FuncNs.Func<Object, Object> transformer) {
			return elementsFromStream(new StringReader(pathString), separatorChars, transformer);
		}

		public static String[] elementsFromStream(this TextReader stream, char[] separatorChars, FuncNs.Func<Object, Object> transformer) {
			if (separatorChars == null || separatorChars.Length < 1) {
				var value = stream.nextWhile(ch => true);
				return new String[]{transformer == null ? value : (String)transformer(value)};
			}
			if (separatorChars.Length < 2) return elementsFromStream(stream, separatorChars[0], transformer);
			List<String> segments = new List<string>();
			StringBuilder segment = new StringBuilder();
			while(true) {
				int next = stream.Peek();
				while (next != -1 && Array.FindIndex<char>(separatorChars, ch => ch == next) < 0) {
					segment.Append((char)stream.Read());
					next = stream.Peek();
				}
				String element = transformer == null ? segment.ToString() : (String)transformer(segment.ToString());
				if (!String.IsNullOrEmpty(element)) segments.Add(element);
				if (next == -1) {
					return segments.ToArray();
				}
				segment = new StringBuilder(); // Should be segment.Clear(), but that's not supported in .Net 3.5
				stream.Read();
			}
		}
		
		public static void classifySymbol(this String value, char? pathElementSeparatorChar, out SymbolType type, out long numArgs, out long pathElementCount) {
			/* Note: Neither "Blue Book" Smalltalk-80 nor ANSI Smalltalk define any syntax for qualified names. In spite of that, at least two significant
			 * Smalltalk dialects have done so: VisualWorks and Smalltalk-X (there may be others; I'm not an expert in every dialect.) Unfortunately,
			 * the syntax that each uses is different: VisualWorks uses industry-standared dotted pathnames (e.g., "Core.Collections.IdentityDictionary"),
			 * but Smalltalk-X uses '::' for the same purpose (and don't get the '::' confused with a semantically different operator token used in C++.)
			 * 
			 * The point is that the following code has to support both versions of the "qualified name path separator" syntax. And that's the reason for 
			 * the complexity of the code. Sorry about that. You can complain to unknownProgrammer@Cincom and/or Claus.Gittinger@exept.de, if you like.
			 */

			if (value == null || value.Length < 1) {
				numArgs = 0;
				pathElementCount = 1;
				type = SymbolType.String;
				return;
			} else if (value.Length < 3 && allSatisfy(value, isBinaryMessageSelectorChar)) {
				numArgs = 1;
				pathElementCount = 1;
				type = SymbolType.BinaryMessageSelector;
				return;
			}
			
			char pathElementSeparator = 
				pathElementSeparatorChar != null ? 
					(char)pathElementSeparatorChar :
					'_'; // The '_' can never be the actual path element separator char, because '_' has to be just another identifier char per ANSI.
			uint consecutivePathElementSeparatorCount = 0;
			bool nextMustBePathElementSeparator = false;
			uint keywordCount = 0;
			bool prevCharWasColon = false;
			long pathElementSeparatorCount = 0;
			type = SymbolType.Identifier;
			for (var index = 0; index < value.Length; index++) {
				char ch = value[index];
				switch (ch) {
					default:
						if (ch == pathElementSeparator) {
							if (consecutivePathElementSeparatorCount < 1 && keywordCount < 1) {
								consecutivePathElementSeparatorCount++;
								pathElementSeparatorCount++;
								nextMustBePathElementSeparator = false;
								continue;
							}
						}
						numArgs = 0;
						pathElementCount = 1;
						type = SymbolType.String;
						return;
						
					case ':': 
						bool isPathElementSeparator = ch == pathElementSeparator;
						if (pathElementSeparatorCount > 0) {
							if (isPathElementSeparator) {
								consecutivePathElementSeparatorCount++;
								if (consecutivePathElementSeparatorCount > 2) {
									numArgs = 0;
									pathElementCount = 1;
									type = SymbolType.String;
									return;
								} else if (consecutivePathElementSeparatorCount == 2) {
									nextMustBePathElementSeparator = false;
									pathElementSeparatorCount++;
								} else {
									nextMustBePathElementSeparator = true;
								}
							} else {
								nextMustBePathElementSeparator = false;
							}
						} else if (nextMustBePathElementSeparator) {
							nextMustBePathElementSeparator = false;
							pathElementSeparatorCount++;
						} else if (prevCharWasColon) {
							numArgs = 0;
							pathElementCount = 1;
							type = SymbolType.String;
							return;
						} else if (isPathElementSeparator) {
							if (keywordCount > 0) {
								nextMustBePathElementSeparator = false;
								consecutivePathElementSeparatorCount = 0;
								keywordCount++;
							} else {
								nextMustBePathElementSeparator = true;
								consecutivePathElementSeparatorCount++;
							}
						} else {
							nextMustBePathElementSeparator = false;
							consecutivePathElementSeparatorCount = 0;
							keywordCount++;
						}
						prevCharWasColon = true;
						break;
						
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
						if (nextMustBePathElementSeparator) {
							if (pathElementSeparatorCount > 0) {
								numArgs = 0;
								pathElementCount = 1;
								type = SymbolType.String;
								return;
							}
							nextMustBePathElementSeparator = false;
							keywordCount++;
						}
						consecutivePathElementSeparatorCount = 0;
						prevCharWasColon = false;
						break;	
						
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
						if (index < 1) {
							numArgs = 0;
							pathElementCount = 1;
							type = SymbolType.String;
							return;
						}
						if (nextMustBePathElementSeparator) {
							if (pathElementSeparatorCount > 0) {
								numArgs = 0;
								pathElementCount = 1;
								type = SymbolType.String;
								return;
							}
							nextMustBePathElementSeparator = false;
							keywordCount++;
						}
						consecutivePathElementSeparatorCount = 0;
						prevCharWasColon = false;
						break;						
				}
			}
			
			if (nextMustBePathElementSeparator && pathElementSeparatorCount < 1) keywordCount++;
			
			if (prevCharWasColon && keywordCount > 0) {
				numArgs = keywordCount;
				pathElementCount = 1;
				type = SymbolType.Keyword;
			} else if (keywordCount < 1 && !nextMustBePathElementSeparator) {
				numArgs = 0;
				pathElementCount = Math.Max(1, pathElementSeparatorCount + 1);
				type = SymbolType.Identifier;
			} else {
				numArgs = 0;
				pathElementCount = 1;
				type = SymbolType.String;
			}

		}
	}
	
}
