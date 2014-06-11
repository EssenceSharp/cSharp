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
using System.Text;
using System.Runtime.CompilerServices;
using EssenceSharp.Runtime;
#endregion

namespace EssenceSharp.ClientServices {

	public static class ObjectExtensionMethods {

		public static long identityHash(this Object clrObject) {
			return RuntimeHelpers.GetHashCode(clrObject);
		}

	}

	public static class ByteExtensionMethods {

		public static long asInteger(this byte value) {
			return (long)value;
		}

		public static byte coerce(this byte receiver, Object operand) {
			return (byte)operand;
		}

		public static byte additiveIdentity(this byte value) {
			return (byte)0;
		}

		public static byte multiplicativeIdentity(this byte value) {
			return (byte)1;
		}

	}

	public static class SByteExtensionMethods {

		public static long asInteger(this sbyte value) {
			return (long)value;
		}

		public static sbyte coerce(this sbyte receiver, Object operand) {
			return (sbyte)operand;
		}

		public static sbyte additiveIdentity(this sbyte value) {
			return (sbyte)0;
		}

		public static sbyte multiplicativeIdentity(this sbyte value) {
			return (sbyte)1;
		}

	}

	public static class CharExtensionMethods {

		public static long asInteger(this char value) {
			return (long)value;
		}

		public static char coerce(this char receiver, Object operand) {
			return (char)operand;
		}

		public static char additiveIdentity(this char value) {
			return (char)0;
		}

		public static char multiplicativeIdentity(this char value) {
			return (char)1;
		}

	}

	public static class UShortExtensionMethods {

		public static long asInteger(this ushort value) {
			return (long)value;
		}

		public static ushort coerce(this ushort receiver, Object operand) {
			return (ushort)operand;
		}

		public static ushort additiveIdentity(this ushort value) {
			return (ushort)0;
		}

		public static ushort multiplicativeIdentity(this ushort value) {
			return (ushort)1;
		}

	}

	public static class ShortExtensionMethods {

		public static long asInteger(this short value) {
			return (long)value;
		}

		public static short coerce(this short receiver, Object operand) {
			return (short)operand;
		}

		public static short additiveIdentity(this short value) {
			return (short)0;
		}

		public static short multiplicativeIdentity(this short value) {
			return (short)1;
		}

	}

	public static class UIntExtensionMethods {

		public static long asInteger(this uint value) {
			return (long)value;
		}

		public static uint coerce(this uint receiver, Object operand) {
			return (uint)operand;
		}

		public static uint additiveIdentity(this uint value) {
			return (uint)0;
		}

		public static uint multiplicativeIdentity(this uint value) {
			return (uint)1;
		}

	}

	public static class IntExtensionMethods {

		public static long asInteger(this int value) {
			return (long)value;
		}

		public static int coerce(this int receiver, Object operand) {
			return (int)operand;
		}

		public static int additiveIdentity(this int value) {
			return (int)0;
		}

		public static int multiplicativeIdentity(this int value) {
			return (int)1;
		}

	}

	public static class ULongExtensionMethods {

		public static long asInteger(this ulong value) {
			return (long)value;
		}

		public static ulong coerce(this ulong receiver, Object operand) {
			return (ulong)operand;
		}

		public static ulong additiveIdentity(this ulong value) {
			return (ulong)0;
		}

		public static ulong multiplicativeIdentity(this ulong value) {
			return (ulong)1;
		}

	}

	public static class LongExtensionMethods {

		public static long asInteger(this long value) {
			return value;
		}

		public static long coerce(this long receiver, Object operand) {
			return (long)operand;
		}

		public static long additiveIdentity(this long value) {
			return (long)0;
		}

		public static long multiplicativeIdentity(this long value) {
			return (long)1;
		}

	}

	public static class FloatExtensionMethods {

		public static long asInteger(this float value) {
			return (long)value;
		}

		public static float coerce(this float receiver, Object operand) {
			return (float)operand;
		}

		public static float additiveIdentity(this float value) {
			return (float)0;
		}

		public static float multiplicativeIdentity(this float value) {
			return (float)1;
		}

	}

	public static class DoubleExtensionMethods {

		public static long asInteger(this double value) {
			return (long)value;
		}

		public static double coerce(this double receiver, Object operand) {
			return (double)operand;
		}

		public static double additiveIdentity(this double value) {
			return (double)0;
		}

		public static double multiplicativeIdentity(this double value) {
			return (double)1;
		}

	}

	public static class DecimalExtensionMethods {

		public static long asInteger(this decimal value) {
			return (long)value;
		}

		public static decimal coerce(this decimal receiver, Object operand) {
			return (decimal)operand;
		}

		public static decimal additiveIdentity(this decimal value) {
			return (decimal)0;
		}

		public static decimal multiplicativeIdentity(this decimal value) {
			return (decimal)1;
		}

	}

	public static class StringExtensionMethods {

		public static String usingCapitalizationScheme(this String hostString, CapitalizationScheme capScheme) {
			if (hostString == null) return "";
			if (hostString.Length < 1) return hostString;
			char ch;
			StringBuilder sb;
			switch (capScheme) {
				case CapitalizationScheme.AsIs:
					return hostString;
				case CapitalizationScheme.InitialCapital:
					ch = hostString[0];
					if (Char.IsUpper(ch)) return hostString;
					sb = new StringBuilder();
					sb.Append(Char.ToUpper(ch));
					for (var i = 1; i < hostString.Length; i++) sb.Append(hostString[i]);
					return sb.ToString();
				case CapitalizationScheme.InitialLowerCase:
					ch = hostString[0];
					if (Char.IsLower(ch)) return hostString;
					sb = new StringBuilder();
					sb.Append(Char.ToLower(ch));
					for (var i = 1; i < hostString.Length; i++) sb.Append(hostString[i]);
					return sb.ToString();
			}
			return hostString;
		}

	}

}

namespace Microsoft.Scripting.Hosting {

	using EssenceSharp.ClientServices;
	using Microsoft.Scripting.Hosting.Providers;
	using Microsoft.Scripting.Runtime;

	public static class ScriptEngineExtensionMethods {

		public static ScriptSource CreateScriptSourceFromPathSuffix(this ScriptEngine engine, String scriptPathameSuffix) {
			var context = engine.essenceSharpContext();
			FileInfo scriptPath;
			if (context.scriptPathnameFor(scriptPathameSuffix, out scriptPath)) {
				return engine.CreateScriptSourceFromFile(scriptPath.FullName);
			}
			return engine.CreateScriptSourceFromFile(scriptPathameSuffix);
		}

		public static EssenceSharpContext essenceSharpContext(this ScriptEngine engine) {
			return (EssenceSharpContext)HostingHelpers.GetLanguageContext(engine);
		}

		public static ESKernel essenceSharpKernel(this ScriptEngine engine) {
			return engine.essenceSharpContext().Kernel;
		}

	}

	public static class ScriptSourceExtensionMethods {

		public static Object Execute(this ScriptSource aScriptSource, Object[] arguments) {
			var compiledCode = aScriptSource.Compile();
			var essenceScriptCode = compiledCode.essenceScriptCode();
			if (essenceScriptCode == null) return null;
			return essenceScriptCode.Run(arguments);
		}

		public static Object Execute(this ScriptSource aScriptSource, ErrorListener errorListener, Object[] arguments) {
			var compiledCode = aScriptSource.Compile(errorListener);
			var essenceScriptCode = compiledCode.essenceScriptCode();
			if (essenceScriptCode == null) return null;
			return essenceScriptCode.Run(arguments);
		}

		public static Object Execute(this ScriptSource aScriptSource, ESCompilerOptions compilerOptions) {
			var compiledCode = aScriptSource.Compile(compilerOptions);
			var essenceScriptCode = compiledCode.essenceScriptCode();
			if (essenceScriptCode == null) return null;
			return essenceScriptCode.Run();
		}

		public static Object Execute(this ScriptSource aScriptSource, ESCompilerOptions compilerOptions, Object[] arguments, out TimeSpan durationToRun) {
			var compiledCode = aScriptSource.Compile(compilerOptions);
			var essenceScriptCode = compiledCode.essenceScriptCode();
			if (essenceScriptCode == null) {
				durationToRun = TimeSpan.Zero;
				return null;
			}
			var value = essenceScriptCode.Run(arguments);
			durationToRun = essenceScriptCode.DurationToRun;
			return value;
		}

		public static Object Execute(this ScriptSource aScriptSource, ESCompilerOptions compilerOptions, Object[] arguments) {
			var compiledCode = aScriptSource.Compile(compilerOptions);
			var essenceScriptCode = compiledCode.essenceScriptCode();
			if (essenceScriptCode == null) return null;
			return essenceScriptCode.Run(arguments);
		}

		public static Object Execute(this ScriptSource aScriptSource, ESCompilerOptions compilerOptions, ErrorListener errorListener) {
			var compiledCode = aScriptSource.Compile(compilerOptions, errorListener);
			var essenceScriptCode = compiledCode.essenceScriptCode();
			if (essenceScriptCode == null) return null;
			return essenceScriptCode.Run();
		}

		public static Object Execute(this ScriptSource aScriptSource, ESCompilerOptions compilerOptions, ErrorListener errorListener, Object[] arguments) {
			var compiledCode = aScriptSource.Compile(compilerOptions, errorListener);
			var essenceScriptCode = compiledCode.essenceScriptCode();
			if (essenceScriptCode == null) return null;
			return essenceScriptCode.Run(arguments);
		}

		public static Object Execute(this ScriptSource aScriptSource, ESCompilerOptions compilerOptions, ErrorListener errorListener, Object[] arguments, out TimeSpan durationToRun) {
			var compiledCode = aScriptSource.Compile(compilerOptions, errorListener);
			var essenceScriptCode = compiledCode.essenceScriptCode();
			if (essenceScriptCode == null) {
				durationToRun = TimeSpan.Zero;
				return null;
			}
			var value = essenceScriptCode.Run(null, arguments);
			durationToRun = essenceScriptCode.DurationToRun;
			return value;
		}

		public static Object Execute(this ScriptSource aScriptSource, ScriptScope scriptScope, Object[] arguments) {
			var compiledCode = aScriptSource.Compile();
			var essenceScriptCode = compiledCode.essenceScriptCode();
			if (essenceScriptCode == null) return null;
			return essenceScriptCode.Run(scriptScope.scope(), arguments);
		}

		public static Object Execute(this ScriptSource aScriptSource, ScriptScope scriptScope, Object[] arguments, out TimeSpan durationToRun) {
			var compiledCode = aScriptSource.Compile();
			var essenceScriptCode = compiledCode.essenceScriptCode();
			if (essenceScriptCode == null) {
				durationToRun = TimeSpan.Zero;
				return null;
			}
			var value = essenceScriptCode.Run(scriptScope.scope(), arguments);
			durationToRun = essenceScriptCode.DurationToRun;
			return value;
		}

		public static Object Execute(this ScriptSource aScriptSource, ErrorListener errorListener, ScriptScope scriptScope, Object[] arguments) {
			var compiledCode = aScriptSource.Compile(errorListener);
			var essenceScriptCode = compiledCode.essenceScriptCode();
			if (essenceScriptCode == null) return null;
			return essenceScriptCode.Run(scriptScope.scope(), arguments);
		}

		public static Object Execute(this ScriptSource aScriptSource, ErrorListener errorListener, ScriptScope scriptScope, Object[] arguments, out TimeSpan durationToRun) {
			var compiledCode = aScriptSource.Compile(errorListener);
			var essenceScriptCode = compiledCode.essenceScriptCode();
			if (essenceScriptCode == null) {
				durationToRun = TimeSpan.Zero;
				return null;
			}
			var value = essenceScriptCode.Run(scriptScope.scope(), arguments);
			durationToRun = essenceScriptCode.DurationToRun;
			return value;
		}

		public static Object Execute(this ScriptSource aScriptSource, ESCompilerOptions compilerOptions, ScriptScope scriptScope, Object[] arguments) {
			var compiledCode = aScriptSource.Compile(compilerOptions);
			var essenceScriptCode = compiledCode.essenceScriptCode();
			if (essenceScriptCode == null) return null;
			return essenceScriptCode.Run(scriptScope.scope(), arguments);
		}

		public static Object Execute(this ScriptSource aScriptSource, ESCompilerOptions compilerOptions, ScriptScope scriptScope, Object[] arguments, out TimeSpan durationToRun) {
			var compiledCode = aScriptSource.Compile(compilerOptions);
			var essenceScriptCode = compiledCode.essenceScriptCode();
			if (essenceScriptCode == null) {
				durationToRun = TimeSpan.Zero;
				return null;
			}
			var value = essenceScriptCode.Run(scriptScope.scope(), arguments);
			durationToRun = essenceScriptCode.DurationToRun;
			return value;
		}

		public static Object Execute(this ScriptSource aScriptSource, ESCompilerOptions compilerOptions, ErrorListener errorListener, ScriptScope scriptScope, Object[] arguments) {
			var compiledCode = aScriptSource.Compile(compilerOptions, errorListener);
			var essenceScriptCode = compiledCode.essenceScriptCode();
			if (essenceScriptCode == null) return null;
			return essenceScriptCode.Run(scriptScope.scope(), arguments);
		}

		public static Object Execute(this ScriptSource aScriptSource, ESCompilerOptions compilerOptions, ErrorListener errorListener, ScriptScope scriptScope, Object[] arguments, out TimeSpan durationToRun) {
			var compiledCode = aScriptSource.Compile(compilerOptions, errorListener);
			var essenceScriptCode = compiledCode.essenceScriptCode();
			if (essenceScriptCode == null) {
				durationToRun = TimeSpan.Zero;
				return null;
			}
			var value = essenceScriptCode.Run(scriptScope.scope(), arguments);
			durationToRun = essenceScriptCode.DurationToRun;
			return value;
		}

		private static ESScriptCode essenceScriptCode(this CompiledCode aCompiledCode) {
			if (aCompiledCode == null) return null;
			return (ESScriptCode)HostingHelpers.GetScriptCode(aCompiledCode);
		}

	}

	public static class ScriptScopeExtensionMethods {

		internal static Scope scope(this ScriptScope aScriptScope) {
			return HostingHelpers.GetScope(aScriptScope);
		}

	}

}
