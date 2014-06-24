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
using Microsoft.Scripting;
using EssenceSharp.ParsingServices;
using EssenceSharp.Runtime;
#endregion

namespace EssenceSharp.ClientServices {

	public enum CompilationUnitKind {
		SelfExpression,			// A message sent to a virtual receiver which is not specified syntactically; the message may be cascaded (and typically would be.)
		BlockDeclaration,		// A block declaration (usually called a 'DoIt'); may optionally have block parameters, but must not be enclosed by square brackets.
		MethodDeclaration		// A method declaration (may optionally be preceded by the ## token, which marks the beginning of a method declaration, but the token is not necessary in this case.)
	}

	public class ESCompilerOptions: CompilerOptions {

		protected CompilationUnitKind	expectedSourceSyntax				= CompilationUnitKind.BlockDeclaration;
		protected String		environmentName;				// The name of the class or namespace which will be the context for name binding of the code to be compiled.
		protected String		methodProtocol;					// The protocol (category) of the method to be compiled (ignored if the compilation unit isn't a method.)
		protected Object		receiver;					// The receiver object (the value of 'self'); important for both self expressions and methods.
		protected ParsingOptions	parsingOptions;

		public ESCompilerOptions() : this(new ParsingOptions()) {
		}

		public ESCompilerOptions(CompilationUnitKind expectedSourceSyntax) : this(expectedSourceSyntax, new ParsingOptions()) {
		}

		public ESCompilerOptions(SyntaxProfile syntaxProfile) : this(new ParsingOptions(syntaxProfile)) {
		}

		public ESCompilerOptions(CompilationUnitKind expectedSourceSyntax, SyntaxProfile syntaxProfile) : this(new ParsingOptions(syntaxProfile)) {
			this.expectedSourceSyntax = expectedSourceSyntax;
		}

		public ESCompilerOptions(ParsingOptions parsingOptions) {
			ParsingOptions = parsingOptions;
		}

		public ESCompilerOptions(CompilationUnitKind sourceCodeKind, ParsingOptions parsingOptions) : this(parsingOptions) {
			this.expectedSourceSyntax = sourceCodeKind;
		}

		public CompilationUnitKind ExpectedSourceSyntax {
			get {return expectedSourceSyntax;}
			set {expectedSourceSyntax = value;}
		}

		public String EnvironmentName {
			get {return environmentName;}
			set {environmentName = value;}
		}

		public bool SpecifiesEnvironmentName {
			get {return environmentName != null;}
		}

		public NamespaceObject getEnvironment(ESKernel kernel) {
			return SpecifiesEnvironmentName ?
				kernel.findOrCreateNamespace(EnvironmentName) :
				kernel.SmalltalkNamespace;
		}

		public String MethodProtocol {
			get {return methodProtocol;}
			set {methodProtocol = value;}
		}

		public bool SpecifiesMethodProtocol {
			get {return methodProtocol != null;}
		}

		public ESSymbol getMethodProtocol(ESKernel kernel) {
			return SpecifiesMethodProtocol ?
				kernel.symbolFor(MethodProtocol) :
				kernel.symbolFor("unspecified");
		}

		public Object Receiver {
			get {return receiver;}
			set {receiver = value;}
		}

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

	}

}
