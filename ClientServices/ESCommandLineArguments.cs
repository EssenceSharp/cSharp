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
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using Microsoft.Scripting.Hosting;
using EssenceSharp.Properties;
using EssenceSharp.UtilityServices;
using EssenceSharp.Runtime;
#endregion

namespace EssenceSharp.ClientServices {

	public enum OperandAffinity {
		NoOperand,		// Either not a keyword (may be the argument to a keyword,) or it's a keyword that accepts no operand.
		AcceptsOperand,		// A keyword that optionally accepts one operand.
		RequiresOperand		// A keyword that requires one operand.
	}

	public enum OptionSemantics {
					// Keyword		OperandAffinity		Semantics
		Illegal,		// ??			NoOperand		Arg syntax error
		Unknown,		// ??			NoOperand		Unkown, unrecognized, undefined keyword
		FlagSet,		// -t -v -h		NoOperand		-t = timings will be reported; -v = verbose reporting on system bootstrap load; -h provide help
		DoIt,			// -d			RequiresOperand		operand = Executable Essence# code to be evaluated
		LiteralArgument,	// -a			RequiresOperand		operand = text of arg to script; will be evaluated by compiler before being passed to script
		PathnameArgument,	// -A			RequiresOperand		operand = pathname of script to be evaluated; the resulting value will be used as an argument to the base script
		Environment,		// -n			RequiresOperand		operand = compilation/evaluation namespace
		ImportList,		// -i			RequiresOperand		operand = comma-separated list of namespace names to be imported into the evaluation namespace
		LibraryList,		// -l			RequiresOperand		operand = comma-separated list of library names to be loaded into the execution context
		ScriptSearchPath,	// -s			RequiresOperand		operand = a search path where the system should check for scripts (affects only the current execution context)
		LibrarySearchPath,	// -L			RequiresOperand		operand = a search path where the system should check for class libraries (affects only the current execution context)
		EssenceSharpPath,	// -p			RequiresOperand		operand = pathname to use as the EssenceSharpPath for the current execution context (does not change the enviroment variable)
		Operand,		// Not a keyword	NoOperand		Either the name of the script file, or else one of the required operands
		QuotedOperand,		// Not a keyword	NoOperand		Either a scriptlet, or else one of the required operands
	}

	public abstract class Option {

		public abstract void applyTo(MultiScriptExecutive scriptExecutive);

	}

	public abstract class Scalar : Option {
		protected String value;

		public Scalar(String value) {
			this.value = value;
		}

		public virtual bool SpecifiesPathname { 
			get {return false;} 
		}

		public String Value {
			get {return value;}
		}

	}

	public abstract class Pathname : Scalar {

		public Pathname(String path) : base(path) {
		}

		public override bool SpecifiesPathname { 
			get {return true;} 
		}

	}

	public abstract class EvaluatableText : Scalar {

		public EvaluatableText(String value) : base(value) {
		}

	}

	public abstract class Identifier : Scalar {

		public Identifier(String value) : base(value) {
		}

	}

	public abstract class IdentifierList : Option {
		protected String[] names;

		public IdentifierList(String[] qualfiedIdentifiers) {
			this.names = qualfiedIdentifiers;
		}

		public String[] Names {
			get {return names;}
		}

	}

	public class ScriptPathnamePrefix : Pathname {

		public ScriptPathnamePrefix(String path) : base(path) {
		}

		public override void applyTo(MultiScriptExecutive scriptExecutive) {
			scriptExecutive.addScriptPathSuffix(Value);
		}

	}

	public class ScriptText : EvaluatableText {

		public ScriptText(String value) : base(value) {
		}

		public override void applyTo(MultiScriptExecutive scriptExecutive) {
			scriptExecutive.addScriptText(Value);
		}

	}

	public class ScriptLiteralArg : EvaluatableText {

		public ScriptLiteralArg(String value) : base(value) {
		}

		public override void applyTo(MultiScriptExecutive scriptExecutive) {
			scriptExecutive.addScriptArg(this);
		}

	}

	public class ScriptPathArg : Pathname {

		public ScriptPathArg(String path) : base(path) {
		}

		public override void applyTo(MultiScriptExecutive scriptExecutive) {
			scriptExecutive.addScriptArg(this);
		}

	}

	public class EssenceSharpPath : Pathname {

		public EssenceSharpPath(String path) : base(path) {
		}

		public override void applyTo(MultiScriptExecutive scriptExecutive) {
			scriptExecutive.EssenceSharpPath = Value;
		}

	}

	public class LibrarySearchPath : Pathname {

		public LibrarySearchPath(String path) : base(path) {
		}

		public override void applyTo(MultiScriptExecutive scriptExecutive) {
			scriptExecutive.addLibrarySearchPath(Value);
		}

	}

	public class ScriptSearchPath : Pathname {

		public ScriptSearchPath(String path) : base(path) {
		}

		public override void applyTo(MultiScriptExecutive scriptExecutive) {
			scriptExecutive.addScriptSearchPath(Value);
		}

	}

	public class LibraryList : IdentifierList {

		public LibraryList(String[] qualifiedIdentifiers) : base(qualifiedIdentifiers) {
		}

		public override void applyTo(MultiScriptExecutive scriptExecutive) {
			if (names == null) return;
			foreach (var qualifiedIdentifier in names) scriptExecutive.addLibrary(qualifiedIdentifier);
		}

	}

	public class EvaluationEnvironment : Identifier {

		public EvaluationEnvironment(String value) : base(value) {
		}

		public override void applyTo(MultiScriptExecutive scriptExecutive) {
			scriptExecutive.EnvironmentName = Value;
		}

	}
	
	public class ImportList : IdentifierList {

		public ImportList(String[] identifiers) : base(identifiers) {
		}

		public override void applyTo(MultiScriptExecutive scriptExecutive) {
			if (names == null) return;
			foreach (var name in names) scriptExecutive.addImport(name);
		}

	}

	public class FlagSet : Option {
		bool? reportTimings = false;
		bool? beVerbose = false;
		bool? provideHelp = false;

		public FlagSet(bool? reportTimings, bool? beVerbose, bool? provideHelp) {
			this.reportTimings = reportTimings;
			this.beVerbose = beVerbose;
			this.provideHelp = provideHelp;
		}

		public bool? ReportTimings {
			get {return reportTimings;}
		}

		public bool? BeVerbose {
			get {return beVerbose;}
		}

		public bool? ProvideHelp {
			get {return provideHelp;}
		}

		public override void applyTo(MultiScriptExecutive scriptExecutive) {
			if (provideHelp != null) scriptExecutive.HelpRequested = (bool)ProvideHelp;
			if (reportTimings != null) scriptExecutive.ReportTimings = (bool)ReportTimings;
			if (beVerbose != null) scriptExecutive.BeVerbose = (bool)BeVerbose;
		}

	}

	public enum ArgumentSyntax {
		None,
		Any,
		Pathname,
		ScriptText,
		Identifier,
		IdentifierList,
		QualifiedIdentier,
		QualifiedIdentierList,
	}

	public class Argument {

		public static Argument parse(String arg, ArgumentSyntax expectedSyntax) {
			HashSet<char> charSet;
			bool? reportTimings = null;
			bool? beVerbose = null;
			bool? provideHelp = null;
			var stream = new StringReader(arg);
			if (expectedSyntax == ArgumentSyntax.Any && ESLexicalUtility.nextSatisfies(stream, ch => ch == '-' || ch == '/')) {
				int c;
				char ch;
				c = stream.Read();
				if (c == -1) return new Argument(OptionSemantics.Illegal, arg);
				ch = (char)c;
				switch (ch) {
					case 'p':
						return new Argument(OptionSemantics.EssenceSharpPath);
					case 'L':
						return new Argument(OptionSemantics.LibrarySearchPath);
					case 's':
						return new Argument(OptionSemantics.ScriptSearchPath);
					case 'l':
						return new Argument(OptionSemantics.LibraryList);
					case 'n':
						return new Argument(OptionSemantics.Environment);
					case 'i':
						return new Argument(OptionSemantics.ImportList);
					case 'd':
						return new Argument(OptionSemantics.DoIt);
					case 'a':
						return new Argument(OptionSemantics.LiteralArgument);
					case 'A':
						return new Argument(OptionSemantics.PathnameArgument);
					case 't':
						charSet = new HashSet<char>();
						foreach (var chr in arg) charSet.Add(chr);
						if (charSet.Contains('v')) beVerbose = true;
						if (charSet.Contains('h')) provideHelp = true;
						return new Argument(true, beVerbose, provideHelp);
					case 'v':
						charSet = new HashSet<char>();
						foreach (var chr in arg) charSet.Add(chr);
						if (charSet.Contains('t')) reportTimings = true;
						if (charSet.Contains('h')) provideHelp = true;
						return new Argument(reportTimings, true, provideHelp);
					case 'h':
					case '?':
						charSet = new HashSet<char>();
						foreach (var chr in arg) charSet.Add(chr);
						if (charSet.Contains('t')) reportTimings = true;
						if (charSet.Contains('v')) beVerbose = true;
						return new Argument(reportTimings, beVerbose, true);
					default:
						return new Argument(OptionSemantics.Unknown, arg);
				}
			} else if (expectedSyntax == ArgumentSyntax.QualifiedIdentierList || expectedSyntax == ArgumentSyntax.IdentifierList) {
				var acceptQualifiedNameSyntax = expectedSyntax == ArgumentSyntax.QualifiedIdentierList;
				var values = new List<String>();
				var identifier = acceptQualifiedNameSyntax ? ESLexicalUtility.nextQualifiedIdentifierFrom(stream) : ESLexicalUtility.nextIdentifierFrom(stream);
				while (identifier != null && identifier.Length > 0) {
					values.Add(identifier);
					if (ESLexicalUtility.nextSatisfies(stream, opCh => opCh == ',' || opCh == ';')) {
						identifier = acceptQualifiedNameSyntax ? ESLexicalUtility.nextQualifiedIdentifierFrom(stream) : ESLexicalUtility.nextIdentifierFrom(stream);
					} else {
						int c;
						char ch;
						c = stream.Peek();
						if (c == -1) {
							break;
						} else { 
							return new Argument(OptionSemantics.Illegal, arg);
						}
					}
				}
				return values.Count > 0 ? new Argument(values.ToArray()) : new Argument(OptionSemantics.Illegal, arg);
			} else if (ESLexicalUtility.nextMatches(stream, '"')) {
				var value = ESLexicalUtility.nextFromUntil(stream, opCh => opCh == '"');
				return new Argument(OptionSemantics.QuotedOperand, value);
			} else { 
				return new Argument(arg);
			}

		}

		protected OperandAffinity	operandAffinity	= OperandAffinity.NoOperand;
		protected OptionSemantics	semantics	= OptionSemantics.Operand;
		protected String[]		operands;
		bool?				reportTimings	= false;
		bool?				beVerbose	= false;
		bool?				provideHelp	= false;

		protected Argument(OptionSemantics semantics, String value) : this(semantics, value == null ? null : new String[]{value}) {
		}

		protected Argument(OptionSemantics semantics, String[] values) {
			this.semantics = semantics;
			switch (semantics) {
				case OptionSemantics.Illegal:
				case OptionSemantics.Unknown:
				case OptionSemantics.FlagSet:
				case OptionSemantics.Operand:
				case OptionSemantics.QuotedOperand:
					operandAffinity = OperandAffinity.NoOperand;
					break;
				default:
					operandAffinity = OperandAffinity.RequiresOperand;
					break;
			}
			this.operands = values;
		}

		protected Argument(OptionSemantics semantics) {
			this.semantics = semantics;
			operandAffinity = OperandAffinity.RequiresOperand;
		}

		protected Argument(bool? reportTimings, bool? beVerbose, bool? provideHelp) {
			this.reportTimings = reportTimings;
			this.beVerbose = beVerbose;
			this.provideHelp = provideHelp;
			this.semantics = OptionSemantics.FlagSet;
			operandAffinity = OperandAffinity.NoOperand;
		}

		protected Argument(String value) {
			operands = new String[]{value};
		}

		protected Argument(String[] values) {
			this.operands = values;
		}

		public OperandAffinity OperandAffinity {
			get { return operandAffinity;}
		}

		public OptionSemantics Semantics {
			get { return semantics;}
		}

		public ArgumentSyntax OperandSyntax {
			get {
				switch (semantics) {
					case OptionSemantics.EssenceSharpPath:
  					case OptionSemantics.LibrarySearchPath:
  					case OptionSemantics.ScriptSearchPath:
    						return ArgumentSyntax.Pathname;
					case OptionSemantics.Environment:
						return ArgumentSyntax.QualifiedIdentier;
					case OptionSemantics.LibraryList:
					case OptionSemantics.ImportList:
						return ArgumentSyntax.QualifiedIdentierList;
					case OptionSemantics.DoIt:
 						return ArgumentSyntax.ScriptText;
					case OptionSemantics.PathnameArgument:
						return ArgumentSyntax.Pathname;
					case OptionSemantics.LiteralArgument:
 						return ArgumentSyntax.ScriptText;
					default:
						return ArgumentSyntax.None;
				}
			}
		}

		public bool ReportTimings {
			get { return reportTimings == null ? false : (bool)reportTimings;}
		}

		public bool BeVerbose {
			get { return beVerbose == null ? false : (bool)beVerbose;}
		}

		public bool ProvideHelp {
			get { return provideHelp == null ? false : (bool)provideHelp;}
		}

		public String OptionKeyword {
			get {	switch (Semantics) {
					case OptionSemantics.EssenceSharpPath:
						return "-p";
					case OptionSemantics.LibrarySearchPath:
						return "-L";
					case OptionSemantics.ScriptSearchPath:
						return "-s";
					case OptionSemantics.LibraryList:
						return "-l";
					case OptionSemantics.Environment:
						return "-n";
					case OptionSemantics.ImportList:
						return "-i";
					case OptionSemantics.DoIt:
						return "-d";
					case OptionSemantics.LiteralArgument:
						return "-a";
					case OptionSemantics.PathnameArgument:
						return "-A";
					case OptionSemantics.FlagSet:
						if (ProvideHelp) return "-h";
						if (ReportTimings) {
							return BeVerbose ? "-tv" : "-t";
						} else if (BeVerbose) {
							return "-v";
						} else {
							return "";
						}
					default:
						return "";
				}}
		}

		public int OperandCount {
			get { return operands == null ? 0 : operands.Length;}
		}

		public String[] Operands {
			get { return operands;}
		}

		public Option asOption() {
			switch (Semantics) {
				case OptionSemantics.Operand:
				case OptionSemantics.QuotedOperand:
					return new ScriptPathnamePrefix(operands[0]);
				case OptionSemantics.FlagSet:
					return new FlagSet(reportTimings, beVerbose, provideHelp);
				default:
					throw new InternalSystemException("Internal error in command line option processing");
			}
		}
 
		public Option asOptionWithOperand(Argument operand) {
			switch (Semantics) {
				case OptionSemantics.EssenceSharpPath:
					if (operand.Semantics != OptionSemantics.Operand)
						throw new InvalidArgumentException("The " + OptionKeyword + " option requires a syntactically-valid pathname name as its operand.");
					return new EssenceSharpPath(operand.Operands[0]);
				case OptionSemantics.LibrarySearchPath:
					if (operand.Semantics != OptionSemantics.Operand)
						throw new InvalidArgumentException("The " + OptionKeyword + " option requires a syntactically-valid pathname name as its operand.");
					return new LibrarySearchPath(operand.Operands[0]);
				case OptionSemantics.ScriptSearchPath:
					if (operand.Semantics != OptionSemantics.Operand)
						throw new InvalidArgumentException("The " + OptionKeyword + " option requires a syntactically-valid pathname name as its operand.");
					return new ScriptSearchPath(operand.Operands[0]);
				case OptionSemantics.LibraryList:
					if (operand.Semantics != OptionSemantics.Operand)
						throw new InvalidArgumentException("The " + OptionKeyword + " option requires a comma-separated list of syntactically-valid libray names as its operand.");
					return new LibraryList(operand.Operands);
				case OptionSemantics.Environment:
					if (operand.Semantics != OptionSemantics.Operand)
						throw new InvalidArgumentException("The " + OptionKeyword + " option requires a syntactically-valid namespace name as its operand.");
					return new EvaluationEnvironment(operand.Operands[0]);
				case OptionSemantics.ImportList:
					if (operand.Semantics != OptionSemantics.Operand)
						throw new InvalidArgumentException("The " + OptionKeyword + " option requires a comma-separated list of syntactically-valid namespace names as its operand.");
					return new ImportList(operand.Operands);
				case OptionSemantics.DoIt:
					if (operand.Semantics != OptionSemantics.Operand && operand.Semantics != OptionSemantics.QuotedOperand)
						throw new InvalidArgumentException("The " + OptionKeyword + " option requires a quoted operand.");
					return new ScriptText(operand.Operands[0]);
				case OptionSemantics.LiteralArgument:
					if (operand.Semantics != OptionSemantics.Operand && operand.Semantics != OptionSemantics.QuotedOperand)
						throw new InvalidArgumentException("The " + OptionKeyword + " option requires a syntactically-valid operand (it may need to be quoted).");
					return new ScriptLiteralArg(operand.Operands[0]);
				case OptionSemantics.PathnameArgument:
					if (operand.Semantics != OptionSemantics.Operand && operand.Semantics != OptionSemantics.QuotedOperand)
						throw new InvalidArgumentException("The " + OptionKeyword + " option requires a syntactically-valid pathname as its operand.");
					return new ScriptPathArg(operand.Operands[0]);
				default:
					throw new InternalSystemException("Internal error in command line option processing");
			}
		}
       
	}

}
