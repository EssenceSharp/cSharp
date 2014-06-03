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
using System.Reflection;
using Microsoft.Scripting;
using Microsoft.Scripting.Utils;
using Microsoft.Scripting.Runtime;
using EssenceSharp.UtilityServices;
using EssenceSharp.Runtime;
#endregion

namespace EssenceSharp.ClientServices {

	public class EssenceSharpContext : LanguageContext {

		public static String EssenceSharpDisplayName {
			get {return "Essence#";}
		}

		public static String EssenceSharpNames {
			get {return "Essence#;EssenceSharp;es";}
		}

		public static String EssenceSharpFileExtensions {
			get {return ".es";}
		}

		protected EssenceSharpOptions		options		= null;
		protected ESKernel			kernel		= null;
		protected bool				librariesLoaded	= false;

		public EssenceSharpContext(ScriptDomainManager manager, Dictionary<String,  Object> optionsDictionary) : base(manager) {
			kernel = new ESKernel();
			bindToLanguageOptions(optionsDictionary ?? new  Dictionary<String,  Object>());
			librariesLoaded = Kernel.ensureStartUp(options.LibraryNames, options.LoadLibrariesVerbosely, options.ReportLibraryLoadTime);
		}
 
		protected void bindToLanguageOptions(Dictionary<String,  Object> protoOptions) {
			var searchPathBuilder = new StringBuilder();
			var userPathnames = LanguageOptions.GetSearchPathsOption(protoOptions);
			if (userPathnames != null) {
				foreach (var pathname in userPathnames) {
					searchPathBuilder.Append(Environment.ExpandEnvironmentVariables(pathname));
					searchPathBuilder.Append(Path.PathSeparator);
				}
			}
			EssenceLaunchPad.scriptSearchPathsDo(
				pathname => {
					searchPathBuilder.Append(pathname);
					searchPathBuilder.Append(Path.PathSeparator);
				});
			Kernel.EssenceSharpPath = LanguageOptions.GetOption(protoOptions, EssenceSharpOptions.essenceSharpPathKey, ESFileUtility.defaultEssenceSharpPath());
			searchPathBuilder.Append(Kernel.ScriptsPath);
			protoOptions[EssenceSharpOptions.searchPathsKey]		= searchPathBuilder.ToString();

			options	= new EssenceSharpOptions(protoOptions);
			options.assemblyNameBindingsDo((qualifiedNsName, assemblyName) => Kernel.bindNamespaceToAssemblyNamed(qualifiedNsName, new AssemblyName(assemblyName)));
			options.assemblyPathBindingsDo((qualifiedNsName, assemblyPath) => Kernel.bindNamespaceToAssemblyAt(qualifiedNsName, new FileInfo(assemblyPath)));
		}

		public override void Shutdown() {
			base.Shutdown();
			kernel = null;
		}

		public ESKernel Kernel {
			get {	return kernel;}
		}

		public override Version LanguageVersion {
			get {return new AssemblyName(typeof(ESKernel).Assembly.FullName).Version;}
		}
        
		public override LanguageOptions Options {
			get {return options;}
		}

		public bool findFullScriptPathnameFor(String scriptPathameSuffix, out FileInfo scriptPath) {
			bool mustCheckForExtension = false;
			String suffixWithExtension;
			var extension = Path.GetExtension(scriptPathameSuffix);
			if (extension == ".es") {
				suffixWithExtension = scriptPathameSuffix;
			} else {
				mustCheckForExtension = true;
				suffixWithExtension = scriptPathameSuffix + ".es";
			}
			scriptPath = new FileInfo(scriptPathameSuffix);
			if (scriptPath.Exists) return true;
			if (mustCheckForExtension) {
				scriptPath = new FileInfo(suffixWithExtension);
				if (scriptPath.Exists) return true;
			}
			foreach (var pathnamePrefix in options.SearchPaths) {
				scriptPath = new FileInfo(Path.Combine(pathnamePrefix, scriptPathameSuffix));
				if (scriptPath.Exists) return true;
				if (mustCheckForExtension) {
					scriptPath = new FileInfo(Path.Combine(pathnamePrefix, suffixWithExtension));
					if (scriptPath.Exists) return true;
				}
			}
			return false;
		}

		public override CompilerOptions GetCompilerOptions() {
			return new ESCompilerOptions();
		}

		public override CompilerOptions GetCompilerOptions(Scope scope) {
			Assert.NotNull(scope);

			ESCompilerOptions options = (ESCompilerOptions)GetCompilerOptions();

			ScopeExtension scopeBoundParameters = scope.GetExtension(ContextId);
			if (scopeBoundParameters != null) {
			// Combine scope parameters with compiler options
			}

			return options;

		}
      
		[System.Diagnostics.CodeAnalysis.SuppressMessage("Microsoft.Design", "CA1024:UsePropertiesWhereAppropriate")]
		public override ErrorSink GetCompilerErrorSink() {
			return ErrorSink.Default;
		}

		public override ScriptCode CompileSourceCode(SourceUnit sourceUnit, CompilerOptions compilationOptions, ErrorSink errorSink) {

			var esCompilerOptions = (ESCompilerOptions)compilationOptions;
			var parsingOptions = esCompilerOptions.ParsingOptions;
			var environment = esCompilerOptions.getEnvironment(Kernel);	
			if (environment == null) environment = Kernel.SmalltalkNamespace;

			using (var sourceStream = sourceUnit.GetReader()) {
				switch (sourceUnit.Kind) {
					default: // None of these distinctions have any utility for Essence; the purpose of the switch statement is simply to document the standard (but useless) options provided by the DLR
					case Microsoft.Scripting.SourceCodeKind.Unspecified:
					case Microsoft.Scripting.SourceCodeKind.AutoDetect:
					case Microsoft.Scripting.SourceCodeKind.Expression:
					case Microsoft.Scripting.SourceCodeKind.SingleStatement:
					case Microsoft.Scripting.SourceCodeKind.Statements:
					case Microsoft.Scripting.SourceCodeKind.InteractiveCode:
					case Microsoft.Scripting.SourceCodeKind.File:
						ESBlock block = null;
						ESMethod method = null;
						switch (esCompilerOptions.ExpectedSourceSyntax) {
							case CompilationUnitKind.SelfExpression:
								return librariesLoaded && Kernel.compileSelfExpression(sourceUnit, parsingOptions, environment, esCompilerOptions.Receiver, errorSink, out block) ?
									new ESBlockScriptCode(sourceUnit, environment, block) :
									new ESBlockScriptCode(sourceUnit, environment, null);
							case CompilationUnitKind.BlockDeclaration:
								return librariesLoaded && Kernel.compile(sourceUnit, parsingOptions, environment, esCompilerOptions.Receiver, errorSink, out block) ?
									new ESBlockScriptCode(sourceUnit, environment, block) :
									new ESBlockScriptCode(sourceUnit, environment, null);
							case CompilationUnitKind.MethodDeclaration:
								var methodClass = (ESBehavior)environment;
								return librariesLoaded && Kernel.compileMethod(sourceUnit, parsingOptions, methodClass, esCompilerOptions.getMethodProtocol(Kernel), errorSink, out method) ?
									new ESMethodScriptCode(sourceUnit, methodClass, esCompilerOptions.Receiver, method) :
									new ESMethodScriptCode(sourceUnit, methodClass, esCompilerOptions.Receiver, null);
						}
						return null;
				}

			}

		}

	}

}
