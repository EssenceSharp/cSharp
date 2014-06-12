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
using System.Collections.Generic;
using System.Diagnostics;
using Microsoft.Scripting.Hosting;
using EssenceSharp.Properties;
using EssenceSharp.UtilityServices;
using EssenceSharp.Exceptions;
using EssenceSharp.Runtime;
#endregion

namespace EssenceSharp.ClientServices {

	public class MultiScriptExecutive {
		protected List<String>			errors				= new List<String>();
		protected EssenceSharpOptionsBuilder	optionsBuilder			= new EssenceSharpOptionsBuilder();
		protected String			environmentName			= "Smalltalk";
		protected List<String>			imports				= new List<String>();
		protected bool				reportTimings			= false;
		protected bool				beVerbose			= false;
		protected bool				helpRequested			= false;
		protected List<ScriptExecutive>		executives			= new List<ScriptExecutive>();
		protected ScriptExecutive		currentExecutive;

		public MultiScriptExecutive(String[] args) {
			if (args == null || args.Length < 1) return;
			var options = new List<Option>();
			Argument keyword;
			Argument operand;
			var index = 0;
			while (index < args.Length) { 
				var arg = args[index++];
				keyword = Argument.parse(arg, ArgumentSyntax.Any);
				if (keyword.OperandAffinity == OperandAffinity.RequiresOperand) {
					if (index < args.Length) {
						try {	
							arg = args[index++];
							operand = Argument.parse(arg, keyword.OperandSyntax);
							options.Add(keyword.asOptionWithOperand(operand));
						} catch (Exception ex) {
							errors.Add("Error in operand for " + keyword.OptionKeyword + " option: " + ex.Message);
						}
					} else {
						throw new InvalidArgumentException("The " + keyword.OptionKeyword + " option requires an operand.");
					}
				} else {
					try { 
						options.Add(keyword.asOption());
					} catch {
						errors.Add("Unknown/unrecognized argument: " + arg);
					}
				}
			}
			foreach (var option in options) option.applyTo(this);
		}

		public String EssenceSharpPath {
			set { optionsBuilder.EssenceSharpPath = new DirectoryInfo(value);}
		}

		public void addLibrarySearchPath(String userLibrarySearchPath) {
			optionsBuilder.addLibrarySearchPath(userLibrarySearchPath);
		}

		public void addScriptSearchPath(String userScriptSearchPath) {
			optionsBuilder.addScriptSearchPath(userScriptSearchPath);
		}

		public void addLibrary(String userLibraryName) {
			try { 
				var libraryName = ESLexicalUtility.nextQualifiedIdentifierFrom(new StringReader(userLibraryName));
				optionsBuilder.loadLibrary(libraryName);
			} catch {
				errors.Add("Library names must use qualified identifer syntax: " + userLibraryName);
			}
		}

		protected void setEnvironmentName(String envName) {
			if (currentExecutive == null) {
				try { 
					environmentName = ESLexicalUtility.nextQualifiedIdentifierFrom(new StringReader(envName));
				} catch {
					errors.Add("Invalid syntax for the name of the evaluation namespace.");
				}
			} else { 
				currentExecutive.EnvironmentName = envName;
			}
		}

		public String EnvironmentName {
			get { return environmentName;}
			set { setEnvironmentName(value);}
		}


		public void addImport(String namespaceName) {
			if (currentExecutive == null) {
				imports.Add(namespaceName);
			} else {
				currentExecutive.addImport(namespaceName);
			}
		}

		protected void setCurrentExecutive(ScriptExecutive executive) {
			currentExecutive = executive;
			if (currentExecutive != null) {
				currentExecutive.ReportTimings = ReportTimings;
				currentExecutive.BeVerbose = BeVerbose;
				executives.Add(currentExecutive);
			}
		}

		public void addScriptPathSuffix(String scriptPathSuffix) {
			setCurrentExecutive(new NamedScriptExecutive(scriptPathSuffix, environmentName, imports));
		}

		public void addScriptText(String scriptText) {
			setCurrentExecutive(new LiteralScriptExecutive(scriptText, environmentName, imports));
		}

		public void addScriptArg(Scalar scriptArg) {
			if (currentExecutive == null) {
				errors.Add("Script arguments may only be specified following the script to which they apply.");
			} else {
				currentExecutive.addScriptArg(scriptArg);
			}
		}

		public bool ReportTimings {
			get { return reportTimings;}
			set {	if (currentExecutive == null) {
					reportTimings = value;
					optionsBuilder.ReportTimings = value;
				} else {
					currentExecutive.ReportTimings = value;
				}}
		}

		public bool BeVerbose {
			get { return beVerbose;}
			set {	if (currentExecutive == null) {
					beVerbose = value;
				} else {
					currentExecutive.BeVerbose = value;
				}
				optionsBuilder.LoadLibrariesVerbosely = value;}
		}

		public bool HelpRequested {
			get { return helpRequested;}
			set { helpRequested = value;}
		}

		public void provideHelp() {
			Console.WriteLine("");
			var helpText = Resources.ES_Help;
			Console.WriteLine(helpText);
		}

		public void run() {
			if (HelpRequested || errors.Count > 0) {
				foreach (var error in errors) Console.WriteLine(error);
				provideHelp();
				return;
			}
			ScriptRuntime scriptRuntime = null;
			try {
				// ScriptRuntime scriptRuntime = ScriptRuntime.CreateFromConfiguration();  // This is the "standard" way to do it; but it's rather less flexible.
				scriptRuntime = EssenceLaunchPad.CreateRuntime(optionsBuilder);
				ScriptEngine engine = scriptRuntime.GetEngine("Essence#");
				foreach (var executive in executives) executive.run(engine);
			} finally {
				if (scriptRuntime != null) scriptRuntime.Shutdown();
			}
		}

	}

	public abstract class ScriptExecutive {
		protected List<String>			errors				= new List<String>();
		protected String			environmentName;
		protected List<String>			imports				= new List<String>();
		protected bool				reportTimings			= false;
		protected bool				beVerbose			= false;
		protected List<Scalar>			scriptArguments			= new List<Scalar>();
		protected Object[]			scriptArgs;
		protected TimeSpan			durationToRun			= TimeSpan.Zero;

		protected ScriptExecutive(String environmentName, List<String> imports) {
			this.environmentName	= environmentName ?? "Smalltalk";
			if (imports != null) {
				foreach (var nsName in imports) this.imports.Add(nsName);
			}
		}

		public abstract String Identity { get; }

		protected void setEnvironmentName(String envName) {
			try { 
				environmentName = ESLexicalUtility.nextQualifiedIdentifierFrom(new StringReader(envName));
			} catch {
				errors.Add("Invalid syntax for the name of the evaluation namespace.");
			}
		}

		public String EnvironmentName {
			get { return environmentName;}
			set { setEnvironmentName(value);}
		}

		public bool ReportTimings {
			get { return reportTimings;}
			set { reportTimings = value;}
		}

		public bool BeVerbose {
			get { return beVerbose;}
			set { beVerbose = value;}
		}

		public void addImport(String namespaceName) {
			imports.Add(namespaceName);
		}


		public void addScriptArg(Scalar scriptArg) {
			scriptArguments.Add(scriptArg);
		}

		protected void evaluatScriptArguments(ESKernel kernel, ESNamespace environment) {
 			var argList = new List<Object>();
			foreach (var argSpec in scriptArguments) {
				try { 
					Object value;
					FileInfo scriptPath;
					if (argSpec.SpecifiesPathname) {
						if (kernel.pathForScript(argSpec.Value, out scriptPath)) {
							if (kernel.evaluate(scriptPath, environment, out value)) {
								argList.Add(value);
							} else {
								errors.Add("Syntax error in script used as argument, pathname = " + argSpec.Value);
							}
						} else {
							errors.Add("Script path (used as argument to base script) not found: " + argSpec.Value);
						}
					} else if (kernel.evaluate(new StringReader(argSpec.Value), environment, out value)) {
						argList.Add(value);
					} else {
						errors.Add("Syntax error in argument: " + argSpec.Value);
					}
				} catch (Exception ex) {
					errors.Add("Error in argument evaluation: " + ex.ToString());
				}
			}
			scriptArgs = argList.ToArray();
		}

		protected abstract Object runScript(ScriptEngine engine, ESCompilerOptions compilationOptions, ESNamespace environment);

		public Object run(ScriptEngine engine) {
			if (errors.Count > 0) {
				foreach (var error in errors) Console.WriteLine(error);
				return null;
			}
			Object value = null;
			try {
				var kernel = engine.essenceSharpKernel();
				var environment = kernel.findOrCreateNamespace(EnvironmentName);
				foreach (var nsName in imports) {
					var ns = kernel.findOrCreateNamespace(nsName);
					environment.addImport(new ESImportSpec(ns, AccessPrivilegeLevel.InHierarchy, ImportTransitivity.Intransitive));
				}
				evaluatScriptArguments(kernel, environment);
				if (errors.Count > 0) {
					foreach (var error in errors) Console.WriteLine(error);
					return null;
				}
				var compilationOptions = (ESCompilerOptions)engine.GetCompilerOptions();
				compilationOptions.EnvironmentName = EnvironmentName;
 				value = runScript(engine, compilationOptions, environment);
				Console.WriteLine("");
				Console.WriteLine(Identity);
				Console.WriteLine(" => " + value);
			} finally {
				if (ReportTimings) { 
					Console.WriteLine("________________________________________");
					Console.WriteLine("Script run time = " + durationToRun.ToString());
					Console.WriteLine("");
				}
			}
			return value;
		}

	}

	public class LiteralScriptExecutive : ScriptExecutive {
		protected String text;

		public LiteralScriptExecutive(String text, String environmentName, List<String> imports) : base(environmentName, imports) {
			this.text = text;
		}

		public override String Identity {
			get { return text;}
		}
 
		protected override Object runScript(ScriptEngine engine, ESCompilerOptions compilationOptions, ESNamespace environment) {
 			var script = engine.CreateScriptSourceFromString(text);
			return script.Execute(compilationOptions, scriptArgs, out durationToRun);
		}	

	}

	public class NamedScriptExecutive : ScriptExecutive {
		protected String			pathnameSuffix;

		public NamedScriptExecutive(String pathnameSuffix, String environmentName, List<String> imports) : base(environmentName, imports) {
			this.pathnameSuffix = pathnameSuffix;

		}

		public override String Identity {
			get { return pathnameSuffix;}
		}

		protected override Object runScript(ScriptEngine engine, ESCompilerOptions compilationOptions, ESNamespace environment) {
			var script = engine.CreateScriptSourceFromPathSuffix(pathnameSuffix);
			return script.Execute(compilationOptions, scriptArgs, out durationToRun);
		}	

	}

}
