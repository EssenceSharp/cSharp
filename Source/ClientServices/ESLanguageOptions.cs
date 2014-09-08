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
using System.Collections.ObjectModel;
using System.Text;
using System.IO;
using Microsoft.Scripting;
using EssenceSharp.Properties;
using EssenceSharp.UtilityServices;
#endregion

namespace EssenceSharp.ClientServices {

	public class EssenceSharpOptionsBuilder {
		
		public static implicit operator Dictionary<String, Object>(EssenceSharpOptionsBuilder optionsBuilder) {
			return new Dictionary<String, Object>(optionsBuilder.Options);  
		}

		protected Dictionary<String, Object>	options				= new Dictionary<String, Object>();
		protected bool				provideExceptionDetail		= true;
		protected bool				showClrExceptions		= true;
		protected bool				providePerformanceStats		= false;
		protected bool				allowAdaptiveCompilation	= true;
		protected int				compilationThreshold		= 5;
		protected List<String>			scriptSearchPaths;
		protected DirectoryInfo			essenceSharpPath;
		protected List<String>			librarySearchPaths		= new List<String>();
		protected List<String>			libraryNames			= new List<String>();
		protected IDictionary<String, String>	assemblyNameBindings;
		protected IDictionary<String, String>	assemblyPathBindings;
		protected bool				loadLibrariesVerbosely		= false;
		protected bool				reportTimings			= false;

		protected virtual void transferValuesToOptionsDictionary() {
			options[EssenceSharpOptions.exceptionDetailKey]			= ProvideExceptionDetail;
			options[EssenceSharpOptions.showClrExceptionsKey]		= ShowClrExceptions;
			options[EssenceSharpOptions.perfStatsKey]			= ProvidePerformanceStats;
			options[EssenceSharpOptions.noAdaptiveCompilationKey]		= !AllowAdaptiveCompilation;
			options[EssenceSharpOptions.compilationThresholdKey]		= CompilationThreshold;
			options[EssenceSharpOptions.essenceSharpPathKey]		= EssenceSharpPath;
			options[EssenceSharpOptions.loadLibrariesVerboselyKey]		= loadLibrariesVerbosely;
			options[EssenceSharpOptions.reportTimingsKey]			= reportTimings;

			var pathNamesList = new List<String>();
			foreach (var path in librarySearchPaths)			pathNamesList.Add(path);
			options[EssenceSharpOptions.librarySearchPathsKey]		= new ReadOnlyCollection<String>(pathNamesList.ToArray());

			var libraryNamesList = new List<String>();
			foreach (var name in libraryNames)				libraryNamesList.Add(name);
			options[EssenceSharpOptions.libraryNamesKey]			= new ReadOnlyCollection<String>(libraryNamesList.ToArray());
			
			if (scriptSearchPaths != null) {
				var searchPathBuilder = new StringBuilder();
				var limit = scriptSearchPaths.Count - 1;
				for (var index = 0; index < limit; index++) {
					var name = scriptSearchPaths[index];
					searchPathBuilder.Append(name);
					searchPathBuilder.Append(Path.PathSeparator);
				}
				searchPathBuilder.Append(scriptSearchPaths[limit]);
				options[EssenceSharpOptions.scriptSearchPathsKey]		= searchPathBuilder.ToString();
			}
		}

		public IDictionary<String, Object> Options {
			get {	transferValuesToOptionsDictionary();
				return options;}
		}

		public bool ProvideExceptionDetail {
			get { return provideExceptionDetail; }
			set { provideExceptionDetail = value;}
		}

		public bool ShowClrExceptions {
			get { return showClrExceptions; }
			set { showClrExceptions = value;}
		}

		public bool ProvidePerformanceStats {
			get { return providePerformanceStats; }
			set { providePerformanceStats = value;}
		}

		public bool AllowAdaptiveCompilation {
			get { return allowAdaptiveCompilation; }
			set { allowAdaptiveCompilation = value;}
		} 
        
		public int CompilationThreshold {
			get { return compilationThreshold; }
			set { compilationThreshold = value;}
		}

		public String EssenceSharpPathString {
			get {return EssenceSharpPath.FullName;}
			set { EssenceSharpPath = new DirectoryInfo(String.IsNullOrEmpty(value) ? Environment.ExpandEnvironmentVariables(value) : Settings.Default.PathEnvironmentVarName);}
		}

		public DirectoryInfo EssenceSharpPath {
			get {return essenceSharpPath ?? ESFileUtility.defaultEssenceSharpPath();}
			set { essenceSharpPath = value;}
		}

		public List<String> LibrarySearchPaths {
			get {return librarySearchPaths;}
		}

		public void addLibrarySearchPath(String pathname) {
			if (String.IsNullOrEmpty(pathname)) return;
			if (librarySearchPaths == null) librarySearchPaths = new List<String>();
			librarySearchPaths.Add(pathname);
		}

		public void addLibrarySearchPaths(IEnumerable<String> pathnames) {
			foreach (var pathname in pathnames) addLibrarySearchPath(pathname);
		}

		public List<String> LibraryNames {
			get {return libraryNames;}
		}

		public void loadLibrary(String name) {
			if (String.IsNullOrEmpty(name)) return;
			libraryNames.Add(name);
		}

		public void loadLibraries(IEnumerable<String> names) {
			foreach (var name in names) loadLibrary(name);
		}

		public void addScriptSearchPath(String pathname) {
			if (String.IsNullOrEmpty(pathname)) return;
			if (scriptSearchPaths == null) scriptSearchPaths = new List<String>();
			scriptSearchPaths.Add(pathname);
		}

		public void addScriptSearchPaths(IEnumerable<String> pathnames) {
			foreach (var pathname in pathnames) addScriptSearchPath(pathname);
		}

		public void bindNamespaceToAssemblyNamed(String qualifiedNamespaceName, String assemblyName) {
			if (assemblyNameBindings == null) {
				assemblyNameBindings = new Dictionary<String, String>();
				options[EssenceSharpOptions.assemblyNameBindingsKey] = assemblyNameBindings;
			}
			assemblyNameBindings[qualifiedNamespaceName] = assemblyName;
		}

		public void bindNamespaceToAssemblyAt(String qualifiedNamespaceName, String assemblyPath) {
			if (assemblyPathBindings == null) {
				assemblyPathBindings = new Dictionary<String, String>();
				options[EssenceSharpOptions.assemblyPathBindingsKey] = assemblyPathBindings;
			}
			assemblyPathBindings[qualifiedNamespaceName] = assemblyPath;
		}

		public bool LoadLibrariesVerbosely {
			get {return loadLibrariesVerbosely;}
			set { loadLibrariesVerbosely = value;}
		}

		public bool ReportTimings {
			get {return reportTimings;}
			set { reportTimings = value;}
		}
		
	}
	
	public class EssenceSharpOptions : LanguageOptions {

		// Standard DLR Hosting Options:
		public static readonly String	exceptionDetailKey		= "ExceptionDetail";
		public static readonly String	showClrExceptionsKey		= "ShowClrExceptions";
		public static readonly String	perfStatsKey			= "PerfStats";
		public static readonly String	noAdaptiveCompilationKey	= "NoAdaptiveCompilation";
		public static readonly String	compilationThresholdKey		= "CompilationThreshold";
		public static readonly String	scriptSearchPathsKey		= "SearchPaths";

		// Essence Sharp Options:
		public static readonly String	essenceSharpPathKey		= "EssenceSharpPath";
		public static readonly String	librarySearchPathsKey		= "LibrarySearchPaths";
		public static readonly String	libraryNamesKey			= "LibraryNames";
		public static readonly String	assemblyNameBindingsKey		= "AssemblyNameBindings";
		public static readonly String	assemblyPathBindingsKey		= "AssemblyPathBindings";
		public static readonly String	loadLibrariesVerboselyKey	= "LoadLibrariesVerbosely";
		public static readonly String	reportTimingsKey		= "ReportTimings";

		public static IDictionary<String, String> getStringMapOption(IDictionary<String, Object> options, String key) {

			Object value;
			if (options == null || !options.TryGetValue(key, out value)) {
				return null;
			}

			var map = value as IDictionary<String, String>;
			if (map != null) return map;

			throw new ArgumentException(String.Format("Invalid value for option {0}", key));

		}

		protected DirectoryInfo			essenceSharpPath;
		protected List<String>			librarySearchPaths		= new List<String>();
		protected List<String>			libraryNames			= new List<String>();
		protected IDictionary<String, String>	assemblyNameBindings;
		protected IDictionary<String, String>	assemblyPathBindings;
		protected bool				loadLibrariesVerbosely		= false;
		protected bool				reportTimings		= false;

		public EssenceSharpOptions() : base(null) {
		}

		public EssenceSharpOptions(IDictionary<string, object> options) : base(options) {
			essenceSharpPath		= GetOption(options, essenceSharpPathKey, ESFileUtility.defaultEssenceSharpPath());
			assemblyNameBindings		= getStringMapOption(options, assemblyNameBindingsKey);
			assemblyPathBindings		= getStringMapOption(options, assemblyPathBindingsKey);
			loadLibrariesVerbosely		= GetOption(options, loadLibrariesVerboselyKey, false);
			reportTimings			= GetOption(options, reportTimingsKey, false);

			var pathNamesList		= GetOption(options, librarySearchPathsKey, new ReadOnlyCollection<String>(new String[0]));
			foreach (var name in pathNamesList) librarySearchPaths.Add(name);

			var libraryNamesList		= GetOption(options, libraryNamesKey, new ReadOnlyCollection<String>(new String[0]));
			foreach (var name in libraryNamesList) libraryNames.Add(name);
		}

		public EssenceSharpOptions(EssenceSharpOptionsBuilder optionsBuilder) : this(optionsBuilder.Options) {
		}

		public DirectoryInfo EssenceSharpPath {
			get {return essenceSharpPath;}
		}

		public List<String> LibrarySearchPaths {
			get {return librarySearchPaths;}
		}

		public List<String> LibraryNames {
			get {return libraryNames;}
		}

		public bool LoadLibrariesVerbosely {
			get {return loadLibrariesVerbosely;}
		}

		public bool ReportTimings {
			get {return reportTimings;}
		}

		public void assemblyNameBindingsDo(Action<String, String> enumerator2) {
			if (assemblyNameBindings == null) return;
			foreach (var kvp in assemblyNameBindings) {
				var qualifiedNamespaceName = kvp.Key;
				if (qualifiedNamespaceName == null) continue;
				var assemblyName = kvp.Value;
				if (assemblyName == null) continue;
				enumerator2(qualifiedNamespaceName, assemblyName);
			}
		}

		public void assemblyPathBindingsDo(Action<String, String> enumerator2) {
			if (assemblyPathBindings == null) return;
			foreach (var kvp in assemblyPathBindings) {
				var qualifiedNamespaceName = kvp.Key;
				if (qualifiedNamespaceName == null) continue;
				var assemblyPath = kvp.Value;
				if (assemblyPath == null) continue;
				enumerator2(qualifiedNamespaceName, assemblyPath);
			}
		}

	}

}
