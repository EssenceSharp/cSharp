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
using Microsoft.Scripting.Hosting;
using EssenceSharp.Runtime;
#endregion

namespace EssenceSharp.ClientServices { 

	public static class EssenceLaunchPad {

		private static readonly IList<String>	scriptSearchPaths	= new List<String>();
		private static ESKernel			defaultKernel		= new ESKernel();

		public static ESKernel DefaultKernel {
			get {return defaultKernel;}
		}

		#region Script Paths

		public static int indexOfSearchPath(String pathname) {
			return scriptSearchPaths.IndexOf(pathname);
		}

		public static void scriptSearchPathAddLast(String pathname) {
			if (String.IsNullOrEmpty(pathname)) return;
			pathname = Environment.ExpandEnvironmentVariables(pathname);
			var index = scriptSearchPaths.IndexOf(pathname);
			if (index >= 0) scriptSearchPaths.RemoveAt(index);
			scriptSearchPaths.Add(pathname);
		}

		public static void scriptSearchPathAddLastIfAbsent(String pathname) {
			if (String.IsNullOrEmpty(pathname)) return;
			pathname = Environment.ExpandEnvironmentVariables(pathname);
			var index = scriptSearchPaths.IndexOf(pathname);
			if (index >= 0) return;
			scriptSearchPaths.Add(pathname);
		}

		public static void scriptSearchPathAddFirst(String pathname) {
			scriptSearchPathInsert(0, pathname);
		}

		public static void scriptSearchPathInsert(int insertionIndex, String pathname) {
			if (String.IsNullOrEmpty(pathname)) return;
			pathname = Environment.ExpandEnvironmentVariables(pathname);
			var index = scriptSearchPaths.IndexOf(pathname);
			if (index >= 0) {
				scriptSearchPaths.RemoveAt(index);
				if (index < insertionIndex) insertionIndex--;
			}
			scriptSearchPaths.Insert(insertionIndex, pathname);
		}

		public static bool scriptSearchPathRemove(String pathname) {
			return scriptSearchPaths.Remove(pathname);
		}

		public static void scriptSearchPathsDo(Action<String> enumerator1) {
			foreach (var pathname in scriptSearchPaths) enumerator1(pathname);
		}

		public static void scriptSearchPathsDoUntil(Func<String , bool> enumerator1) {
			foreach (var pathname in scriptSearchPaths) if (enumerator1(pathname)) return;
		}

		#endregion

		#region DLR Hosting Configuration

		/// <summary>
		/// Creates a new ScriptRuntime with the Essence scipting engine pre-configured.
		/// </summary>
		/// <returns></returns>
		public static ScriptRuntime CreateRuntime() {
			return new ScriptRuntime(CreateRuntimeSetup(new EssenceSharpOptionsBuilder()));
		}

		/// <summary>
		/// Creates a new ScriptRuntime with the Essence scipting engine pre-configured with additional options.
		/// </summary>
		public static ScriptRuntime CreateRuntime(IDictionary<String, Object> options) {
			return new ScriptRuntime(CreateRuntimeSetup(options));
		}

		/// <summary>
		/// Creates a new ScriptRuntime with the Essence scipting engine pre-configured with additional options.
		/// </summary>
		public static ScriptRuntime CreateRuntime(EssenceSharpOptionsBuilder options) {
			return new ScriptRuntime(CreateRuntimeSetup(options));
		}

		/// <summary>
		/// Creates a new ScriptRuntime and returns the ScriptEngine for Essence. If the ScriptRuntime is required,
		/// it can be acquired from the Runtime property on the engine.
		/// </summary>
		public static ScriptEngine CreateEngine() {
			return GetEngine(CreateRuntime());
		}

		/// <summary>
		/// Creates a new ScriptRuntime with the specified options and returns the ScriptEngine for Essence. 
		/// If the ScriptRuntime is requierd it can be acquired from the Runtime property on the engine.
		/// </summary>
		public static ScriptEngine CreateEngine(IDictionary<String, Object> options) {
			return GetEngine(CreateRuntime(options));
		}

		/// <summary>
		/// Given a ScriptRuntime gets the ScriptEngine for Essence.
		/// </summary>
		public static ScriptEngine GetEngine(ScriptRuntime runtime) {
			return runtime.GetEngineByTypeName(typeof(EssenceSharpContext).AssemblyQualifiedName);
		}

		/// <summary>
		/// Creates a ScriptRuntimeSetup Object which includes the Essence script engine with the specified options.
		/// 
		/// The ScriptRuntimeSetup Object can then be additionally configured and used to create a ScriptRuntime.
		/// </summary>
		public static ScriptRuntimeSetup CreateRuntimeSetup(IDictionary<String, Object> options) {
			ScriptRuntimeSetup setup = new ScriptRuntimeSetup();
			setup.LanguageSetups.Add(CreateLanguageSetup(options));

			if (options != null) {
				Object value;
				if (options.TryGetValue("Debug", out value) && value is bool && (bool)value) {
					setup.DebugMode = true;
				}

				if (options.TryGetValue("PrivateBinding", out value) && value is bool && (bool)value) {
					setup.PrivateBinding = true;
				}
			}

			return setup;
		}

		/// <summary>
		/// Creates a ScriptRuntimeSetup Object which includes the Essence script engine with the specified options.
		/// 
		/// The ScriptRuntimeSetup Object can then be additionally configured and used to create a ScriptRuntime.
		/// </summary>
		public static ScriptRuntimeSetup CreateRuntimeSetup(EssenceSharpOptionsBuilder options) {
			return CreateRuntimeSetup(options.Options);
		}

		/// <summary>
		/// Creates a LanguageSetup Object which includes the Essence script engine with the specified options.
		/// 
		/// The LanguageSetup Object can be used with other LanguageSetup objects from other languages to
		/// configure a ScriptRuntimeSetup Object.
		/// </summary>
		public static LanguageSetup CreateLanguageSetup(IDictionary<String, Object> options) {

			var setup = new LanguageSetup(
					typeof(EssenceSharpContext).AssemblyQualifiedName,
					EssenceSharpContext.EssenceSharpDisplayName,
					EssenceSharpContext.EssenceSharpNames.Split(';'),
					EssenceSharpContext.EssenceSharpFileExtensions.Split(';')
			);

			if (options != null) {
				foreach (var entry in options) setup.Options.Add(entry.Key, entry.Value);
			}

			return setup;

		}

		#endregion

	}

}
