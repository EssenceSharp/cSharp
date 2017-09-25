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
using EssenceSharp.ClientServices;
#endregion

namespace EssenceSharp {

	class Program {
		static void Main(string[] args) {
			TimeSpan durationToRun = TimeSpan.Zero;
			ScriptRuntime scriptRuntime = null;
			try {
				// ScriptRuntime scriptRuntime = ScriptRuntime.CreateFromConfiguration();  // This is the "standard" to do it; but it's rather less flexible.
				var optionsBuilder = new EssenceSharpOptionsBuilder();
				optionsBuilder.addScriptSearchPath(".");
				String scriptFileName = "Default.es";
				if (args.Length > 0) {
					scriptFileName = args[0];
					var extension = Path.GetExtension(scriptFileName);
					if (extension != ".es") scriptFileName = scriptFileName + ".es";
					for (var i = 1; i < args.Length; i++) optionsBuilder.loadLibrary(args[i]);
				}
				scriptRuntime = EssenceLaunchPad.CreateRuntime(optionsBuilder);
				ScriptEngine engine = scriptRuntime.GetEngine("Essence#");
				var compilationOptions = (ESCompilerOptions)engine.GetCompilerOptions();
				compilationOptions.EnvironmentName = "Smalltalk"; // Should eventually be specifiable by command-line argument.
				ScriptSource script = engine.CreateScriptSourceFromPathSuffix(scriptFileName);
				var scriptArgs = new Object[]{}; // Should eventually be specifiable by command-line argument(s).
				var stopwatch = new Stopwatch();
				stopwatch.Start();
				var value = script.Execute(compilationOptions, scriptArgs);
				stopwatch.Stop();
				durationToRun = stopwatch.Elapsed;
				Console.WriteLine(" => " + value);
			} finally {
				if (scriptRuntime != null) scriptRuntime.Shutdown();
				Console.WriteLine("________________________________________");
				Console.WriteLine("Script run time = " + durationToRun.ToString() + " (includes setup and compilation time)");
			}
		}
	}

}
