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
using System.Diagnostics;
using Microsoft.Scripting;
using Microsoft.Scripting.Runtime;
using EssenceSharp.Runtime;
#endregion

namespace EssenceSharp.ClientServices {

	public enum CodeKind {
		Function,
		Method
	}

	public abstract class ESScriptCode : ScriptCode {

		protected SourceUnit		sourceUnit;
		protected ESNamespace		bindingNamespace;
		protected TimeSpan		durationToRun			= TimeSpan.Zero;

		public ESScriptCode(SourceUnit sourceUnit, ESNamespace bindingNamespace) : base(sourceUnit) {
			this.sourceUnit			= sourceUnit;
			this.bindingNamespace		= bindingNamespace;
		}

		public abstract CodeKind Kind {
			get;
		}

		public abstract ESCompiledCode Code {
			get;
		}

		public Delegate Function {
			get {return Code.Function;}
		}

		public ESNamespace BindingNamespace {
			get {return bindingNamespace;}
		}

		public TimeSpan DurationToRun {
			get {return durationToRun;}
		}

		protected void bindToScope(Scope scope) {
			if (bindingNamespace != null && scope != null) {
				var storage = scope.Storage as ScopeStorage;
				if (storage !=null) {
					var variableNames = storage.GetMemberNames();
					foreach (var name in variableNames) {
						bindingNamespace.importVariableFrom(scope, name, AccessPrivilegeLevel.Public, null);
					}
				}
			}
		}

		public override Object Run(Scope scope) {
			return Run(scope, null);
		}

		public virtual Object Run(Object[] arguments) {
			return Run(null, arguments);
		}

		public abstract Object Run(Scope scope, Object[] arguments);

	}

	public class ESBlockScriptCode : ESScriptCode {

		protected ESBlock		block;

		public ESBlockScriptCode(SourceUnit sourceUnit, ESNamespace bindingNamespace, ESBlock block) : base(sourceUnit, bindingNamespace) {
			this.block		= block;
		}

		public override CodeKind Kind {
			get {return CodeKind.Function;}
		}

		public override ESCompiledCode Code {
			get {return block;}
		}

		public ESBlock Block {
			get {return block;}
		}

		public override Object Run(Scope scope, Object[] arguments) {
			if (block == null) return null;
			bindToScope(scope);
			Object value;
			var stopwatch = new Stopwatch();
			stopwatch.Start();
			if (arguments == null || arguments.Length < 1) {
				value = block.value0();
			} else {
				value = block.valueWithArguments(arguments);
			}
			stopwatch.Stop();
			durationToRun = stopwatch.Elapsed;
			return value;
		}

	}

	public class ESMethodScriptCode : ESScriptCode {

		protected ESMethod		method;
		protected Object		defaultReceiver;

		public ESMethodScriptCode(SourceUnit sourceUnit, ESNamespace bindingNamespace, Object defaultReceiver, ESMethod method) : base(sourceUnit, bindingNamespace) {
			this.defaultReceiver	= defaultReceiver;
			this.method		= method;
		}

		public override CodeKind Kind {
			get {return CodeKind.Method;}
		}

		public override ESCompiledCode Code {
			get {return method;}
		}

		public ESMethod Method {
			get {return method;}
		}

		public Object DefaultReceiver {
			get {return defaultReceiver;}
		}

		public override Object Run(Scope scope) {
			return Run(scope, DefaultReceiver, null);
		}

		public override Object Run(Object[] arguments) {
			return Run(null, DefaultReceiver, arguments);
		}
		public override Object Run(Scope scope, Object[] arguments) {
			return Run(scope, DefaultReceiver, arguments);
		}

		public Object Run(Scope scope, Object receiver, Object[] arguments) {
			if (method == null) return null;
			bindToScope(scope);

			Object value;
			var stopwatch = new Stopwatch();
			stopwatch.Start();
			if (arguments == null || arguments.Length < 1) {
				value = method.value0(receiver);
			} else { 
				value = method.valueWithReceiverWithArguments(receiver, arguments);
			}
			stopwatch.Stop();
			durationToRun = stopwatch.Elapsed;
			return value;
		}

	}
}


