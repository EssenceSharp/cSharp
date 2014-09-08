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
		protected ESObjectSpace		objectSpace;
		protected ESCompilerOptions	esCompilerOptions;
		protected NamespaceObject	bindingNamespace;
		protected ErrorSink		errorSink;
		protected TimeSpan		durationToRun			= TimeSpan.Zero;

		public ESScriptCode(SourceUnit sourceUnit, ESObjectSpace objectSpace, ESCompilerOptions esCompilerOptions, ErrorSink errorSink) : base(sourceUnit) {
			this.sourceUnit					= sourceUnit;
			this.objectSpace				= objectSpace;
			this.esCompilerOptions				= esCompilerOptions;
			this.errorSink					= errorSink;
			bindingNamespace				= esCompilerOptions.getEnvironment(objectSpace);	
			if (bindingNamespace == null) bindingNamespace	= objectSpace.SmalltalkNamespace;
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

		public NamespaceObject BindingNamespace {
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
						bindingNamespace.importScopeVariableFrom(scope, name, AccessPrivilegeLevel.Public, null);
					}
				}
			}
		}

		protected abstract bool compile(Scope scope);

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

		public ESBlockScriptCode(SourceUnit sourceUnit, ESObjectSpace objectSpace, ESCompilerOptions esCompilerOptions, ErrorSink errorSink) : base(sourceUnit, objectSpace, esCompilerOptions, errorSink) {
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

		protected override bool compile(Scope scope) {
			bindToScope(scope);
			var parsingOptions	= esCompilerOptions.ParsingOptions;
			switch (esCompilerOptions.ExpectedSourceSyntax) {
				case CompilationUnitKind.SelfExpression:
					return objectSpace.compileSelfExpression(sourceUnit, parsingOptions, bindingNamespace, esCompilerOptions.Receiver, errorSink, out block);
				case CompilationUnitKind.BlockDeclaration:
					return objectSpace.compile(sourceUnit, parsingOptions, bindingNamespace, esCompilerOptions.Receiver, errorSink, out block);
				default:
				case CompilationUnitKind.MethodDeclaration:
					return false;
			}
		}

		public override Object Run(Scope scope, Object[] arguments) {
			compile(scope);
			if (block == null) return null;
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

		protected Object		messageReceiver;
		protected ESMethod		method;

		public ESMethodScriptCode(SourceUnit sourceUnit, ESObjectSpace objectSpace, ESCompilerOptions esCompilerOptions, ErrorSink errorSink) : base(sourceUnit, objectSpace, esCompilerOptions, errorSink) {
			messageReceiver		= esCompilerOptions.Receiver;
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

		public Object MessageReceiver {
			get {return messageReceiver;}
		}

		protected override bool compile(Scope scope) {
			bindToScope(scope);
			var parsingOptions	= esCompilerOptions.ParsingOptions;
			var methodClass		= (ESBehavioralObject)bindingNamespace;
			return objectSpace.compileMethod(sourceUnit, parsingOptions, methodClass, esCompilerOptions.getMethodProtocol(objectSpace), errorSink, out method);
		}

		public override Object Run(Scope scope) {
			return Run(scope, MessageReceiver, null);
		}

		public override Object Run(Object[] arguments) {
			return Run(null, MessageReceiver, arguments);
		}
		public override Object Run(Scope scope, Object[] arguments) {
			return Run(scope, MessageReceiver, arguments);
		}

		public Object Run(Scope scope, Object receiver, Object[] arguments) {
			compile(scope);
			if (method == null) return null;

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


