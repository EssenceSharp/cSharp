﻿/*
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
using System.Runtime.CompilerServices;
using System.Reflection;
using System.Dynamic;
#if CLR2
using Microsoft.Scripting.Ast;
using FuncNs = Microsoft.Scripting.Utils;
#else
using System.Linq.Expressions;
using FuncNs = System;
#endif
using EssenceSharp.CompilationServices;
using EssenceSharp.Exceptions.System;
using EssenceSharp.Exceptions.System.PrimitiveFailures;
using EssenceSharp.Runtime.Binding;
#endregion

namespace EssenceSharp.Runtime { 
	
	public abstract class ESCompiledCode : ESObject {

		#region Static variables and functions

		private static readonly int										maxArgs						= 32;
		private static readonly Type[]										blockFunctionType				= new Type[MaxArgs + 1];	
		private static readonly Type[]										methodFunctionType				= new Type[MaxArgs + 1];

		#region Host System Reflection

		public static int MaxArgs {
			get {return maxArgs;}
		}

		public static Type blockFunctionTypeForNumArgs(long numArgs) {
			return blockFunctionType[numArgs];
		}

		public static Type methodFunctionTypeForNumArgs(long numArgs) {
			return methodFunctionType[numArgs];
		}

		#endregion

		#region Static Initialization
		
		static ESCompiledCode() {

 			blockFunctionType[0] = typeof(FuncNs.Func<Object>);
			blockFunctionType[1] = typeof(FuncNs.Func<Object, Object>);
			blockFunctionType[2] = typeof(FuncNs.Func<Object, Object, Object>);
			blockFunctionType[3] = typeof(FuncNs.Func<Object, Object, Object, Object>);
			blockFunctionType[4] = typeof(FuncNs.Func<Object, Object, Object, Object, Object>);
			blockFunctionType[5] = typeof(FuncNs.Func<Object, Object, Object, Object, Object, Object>);
			blockFunctionType[6] = typeof(FuncNs.Func<Object, Object, Object, Object, Object, Object, Object>);
			blockFunctionType[7] = typeof(FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object>);
			blockFunctionType[8] = typeof(FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			blockFunctionType[9] = typeof(FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			blockFunctionType[10] = typeof(FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			blockFunctionType[11] = typeof(FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			blockFunctionType[12] = typeof(FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			blockFunctionType[13] = typeof(FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			blockFunctionType[14] = typeof(FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			blockFunctionType[15] = typeof(FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			blockFunctionType[16] = typeof(FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			blockFunctionType[17] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			blockFunctionType[18] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			blockFunctionType[19] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			blockFunctionType[20] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			blockFunctionType[21] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			blockFunctionType[22] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			blockFunctionType[23] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			blockFunctionType[24] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			blockFunctionType[25] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			blockFunctionType[26] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			blockFunctionType[27] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			blockFunctionType[28] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			blockFunctionType[29] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			blockFunctionType[30] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			blockFunctionType[31] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			blockFunctionType[32] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
	
			methodFunctionType[0] = typeof(FuncNs.Func<Object, Object>);
			methodFunctionType[1] = typeof(FuncNs.Func<Object, Object, Object>);
			methodFunctionType[2] = typeof(FuncNs.Func<Object, Object, Object, Object>);
			methodFunctionType[3] = typeof(FuncNs.Func<Object, Object, Object, Object, Object>);
			methodFunctionType[4] = typeof(FuncNs.Func<Object, Object, Object, Object, Object, Object>);
			methodFunctionType[5] = typeof(FuncNs.Func<Object, Object, Object, Object, Object, Object, Object>);
			methodFunctionType[6] = typeof(FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object>);
			methodFunctionType[7] = typeof(FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			methodFunctionType[8] = typeof(FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			methodFunctionType[9] = typeof(FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			methodFunctionType[10] = typeof(FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			methodFunctionType[11] = typeof(FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			methodFunctionType[12] = typeof(FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			methodFunctionType[13] = typeof(FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			methodFunctionType[14] = typeof(FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			methodFunctionType[15] = typeof(FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			methodFunctionType[16] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			methodFunctionType[17] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			methodFunctionType[18] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			methodFunctionType[19] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			methodFunctionType[20] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			methodFunctionType[21] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			methodFunctionType[22] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			methodFunctionType[23] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			methodFunctionType[24] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			methodFunctionType[25] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			methodFunctionType[26] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			methodFunctionType[27] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			methodFunctionType[28] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			methodFunctionType[29] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			methodFunctionType[30] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			methodFunctionType[31] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);
			methodFunctionType[32] = typeof(Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>);

		}

		#endregion

		#endregion

		protected Delegate											function;
		protected long 												numArgs;
		
		protected ESCompiledCode(ESBehavior esClass) : base(esClass) {
			setFunctionToDefault(out function);
		}
		
		protected ESCompiledCode(ESBehavior esClass, long numArgs) : base(esClass) {
			this.numArgs = numArgs;
		}
		
		protected ESCompiledCode(ESBehavior esClass, Delegate function, long numArgs) : base(esClass) {
			this.numArgs = numArgs;
			Function = function;
		}
		
		public abstract void setFunctionToDefault(out Delegate function);
		
		public virtual long NumArgs {
			get {return numArgs;}
			set {numArgs = value;}
		}

		public bool HasFunction {
			get {return function != null;}
		}

		public Delegate Function {
			get {return function;}
			set {setFunction(value);}
		}

		protected void setFunction(Delegate newFunction) {
			if (function == newFunction) return;
			function = newFunction;
			changedFunction();
		}

		protected virtual void changedFunction() {
			if (function == null) setFunctionToDefault(out function);
		}

		public abstract NamespaceObject Environment {
			get;
		}
		
		public abstract ESCompiledCode RootContext {
			get;
		}
		
		public abstract bool HasHomeClass {
			get;
		}

		public abstract BehavioralObject HomeClass {
			get;
		}
		
		protected ESObject throwTargetParameterCountException(long argCountAttempted) {
			throw new TargetParameterCountException();
		}

		#region Functor Delegates
				
		public FuncNs.Func<Object> F0 {
			get {return (FuncNs.Func<Object>)function;}
		}

		public FuncNs.Func<Object, Object> F1 {
			get {return (FuncNs.Func<Object, Object>)function;}
		}

		public FuncNs.Func<Object, Object, Object> F2 {
			get {return (FuncNs.Func<Object, Object, Object>)function;}
		}

		public FuncNs.Func<Object, Object, Object, Object> F3 {
			get {return (FuncNs.Func<Object, Object, Object, Object>)function;}
		}

		public FuncNs.Func<Object, Object, Object, Object, Object> F4 {
			get {return (FuncNs.Func<Object, Object, Object, Object, Object>)function;}
		}

		public FuncNs.Func<Object, Object, Object, Object, Object, Object> F5 {
			get {return (FuncNs.Func<Object, Object, Object, Object, Object, Object>)function;}
		}

		public FuncNs.Func<Object, Object, Object, Object, Object, Object, Object> F6 {
			get {return (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object>)function;}
		}

		public FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object> F7 {
			get {return (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object>)function;}
		}

		public FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object> F8 {
			get {return (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;}
		}

		public FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> F9 {
			get {return (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;}
		}

		public FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> F10 {
			get {return (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;}
		}

		public FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> F11 {
			get {return (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;}
		}

		public FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> F12 {
			get {return (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;}
		}

		public FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> F13 {
			get {return (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;}
		}

		public FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> F14 {
			get {return (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;}
		}

		public FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> F15 {
			get {return (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;}
		}

		public FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> F16 {
			get {return (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;}
		}

		public Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> F17 {
			get {return (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;}
		}

		public Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> F18 {
			get {return (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;}
		}

		public Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> F19 {
			get {return (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;}
		}

		public Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> F20 {
			get {return (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;}
		}

		public Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> F21 {
			get {return (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;}
		}

		public Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> F22 {
			get {return (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;}
		}

		public Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> F23 {
			get {return (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;}
		}

		public Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> F24 {
			get {return (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;}
		}

		public Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> F25 {
			get {return (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;}
		}

		public Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> F26 {
			get {return (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;}
		}

		public Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> F27 {
			get {return (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;}
		}

		public Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> F28 {
			get {return (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;}
		}

		public Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> F29 {
			get {return (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;}
		}

		public Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> F30 {
			get {return (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;}
		}

		public Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> F31 {
			get {return (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;}
		}

		public Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> F32 {
			get {return (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;}
		}

		public Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> F33 {
			get {return (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;}
		}

		#endregion

		public new class Primitives : PrimitiveDomain {

			protected override void bindToObjectSpace() {
				domainClass = objectSpace.CompiledCodeClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.CompiledCode;}
			}

			#region Primitive Definitions
		
			public static Object _function_(Object receiver) {
				return ((ESCompiledCode)receiver).Function;
			}
		
			public static Object _numArgs_(Object receiver) {
				return ((ESCompiledCode)receiver).NumArgs;
			}
		
			public static Object _rootContext_(Object receiver) {
				return ((ESCompiledCode)receiver).RootContext;
			}
		
			public static Object _homeClass_(Object receiver) {
				return ((ESCompiledCode)receiver).HomeClass;
			}

			#endregion

			public override void publishCanonicalPrimitives() {

				publishPrimitive("function",					new FuncNs.Func<Object, Object>(_function_));
				publishPrimitive("numArgs",					new FuncNs.Func<Object, Object>(_numArgs_));
				publishPrimitive("rootContext",					new FuncNs.Func<Object, Object>(_rootContext_));
				publishPrimitive("homeClass",					new FuncNs.Func<Object, Object>(_homeClass_));

			}

		}
		
	}

	public class BlockIdentityComparator : IdentityComparator<ESBlock> {}
	
	public class ESBlock : ESCompiledCode {

		protected ESCompiledCode 									enclosingContext;
		protected IDictionary<Type, Delegate>								avatars; 
		// An 'avatar' is a function with typed parameters that does nothing except to invoke this block's function.
		// Note that the degenerate case is where the block's function is its own avatar.
		// They key of the dictionary is the delegate type of the avatar (based on the parameter signature,) 
		// and the value is the delegate having that parameter signature.
		
		public ESBlock(ESBehavior esClass) : base(esClass) {
		}
		
		public ESBlock(ESBehavior esClass, Delegate function, long numArgs) : base(esClass, function, numArgs) {
		}

		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.Block;}
		}
		
		public override bool IsBlock {
			get {return true;}
		}
		
		public ESCompiledCode EnclosingContext {
			get {return enclosingContext;}
			set {enclosingContext = value;}
		}
		
		public override NamespaceObject Environment {
			get {return enclosingContext == null ? null : enclosingContext.Environment;}
		}
		
		public override ESCompiledCode RootContext {
			get {return enclosingContext == null ? null : enclosingContext.RootContext;}
		}
		
		public override bool HasHomeClass {
			get {return enclosingContext == null ? false : enclosingContext.HasHomeClass;}
		}
		
		public override BehavioralObject HomeClass {
			get {return enclosingContext == null ? null : enclosingContext.HomeClass;}
		}
		
		public override ESBlock asBlock() {
			return this;
		}
		
		public bool asBoolean() {
			return asBoolean(value0());
		}

		protected override void changedFunction() {
			base.changedFunction();
			avatars = null;
		}

		public void addAvatar(Delegate avatar) {
			var baseType = function.GetType();
			var avatarType = avatar.GetType();
			if (avatarType == baseType) return;
			if (avatars == null) avatars = new Dictionary<Type, Delegate>();
			avatars[avatarType] = avatar;
		}

		public void removeAvatar(Delegate avatar) {
			if (avatars == null) return;
			removeAvatar(avatar.GetType());
		}

		public void removeAvatar(Type avatarType) {
			if (avatars == null) return;
			var baseType = function.GetType();
			if (avatarType == baseType) return;
			if (avatars.Remove(avatarType)) {
				if (avatars.Count < 1) avatars = null;
			}
		}

		public Delegate avatarWithType(Type avatarType) {
			if (avatars != null) {
				Delegate avatar;
				if (avatars.TryGetValue(avatarType, out avatar)) return avatar;
			}
			var baseType = function.GetType();
			return avatarType == baseType ? function : null;
		}

		public void avatarsDo(Action<Delegate> enumerator1) {
			enumerator1(function);
			if (avatars == null) return;
			foreach (var kvp in avatars) enumerator1(kvp.Value);
		}

		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			append("numArgs: ");
			append(NumArgs.ToString());
		}

		public override DynamicMetaObject GetMetaObject(Expression parameter) {
			return new ESBlockDynamicMetaObject(parameter, BindingRestrictions.Empty, this, Class);
		}
		
		public override int GetHashCode() {
			return RuntimeHelpers.GetHashCode(this);
		}

		public override bool Equals(Object comparand) {
			return this == comparand;
		}      
		
		public override bool Equals(ESObject comparand) {
			return this == comparand;
		}    

		public override T valueBy<T>(Operation<T> operation) {
			return operation.applyToCompiledBlock(this);
		}

		#region Setting Default Function

		public override void setFunctionToDefault(out Delegate function) {
 
			switch (NumArgs) {
				default:
				case 0:
					function = (FuncNs.Func<Object>)(delegate () {return null;});
					break;
				case 1:
					function = (FuncNs.Func<Object, Object>)(delegate (Object a1) {return a1;});
					break;
				case 2:
					function = (FuncNs.Func<Object, Object, Object>)(delegate (Object a1, Object a2) {return a2;});
					break;
				case 3:
					function = (FuncNs.Func<Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3) {return a3;});
					break;
				case 4:
					function = (FuncNs.Func<Object, Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3, Object a4) {return a4;});
					break;
				case 5:
					function = (FuncNs.Func<Object, Object, Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3, Object a4, Object a5) {return a5;});
					break;
				case 6:
					function = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3, Object a4, Object a5, Object a6) {return a6;});
					break;
				case 7:
					function = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7) {return a7;});
					break;
				case 8:
					function = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8) {return a8;});
					break;
				case 9:
					function = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9) {return a9;});
					break;
				case 10:
					function = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10) {return a10;});
					break;
				case 11:
					function = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11) {return a11;});
					break;
				case 12:
					function = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12) {return a12;});
					break;
				case 13:
					function = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13) {return a13;});
					break;
				case 14:
					function = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14) {return a14;});
					break;
				case 15:
					function = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15) {return a15;});
					break;
				case 16:
					function = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16) {return a16;});
					break;
				case 17:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17) {return a17;});
					break;
				case 18:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18) {return a18;});
					break;
				case 19:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19) {return a19;});
					break;
				case 20:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20) {return a20;});
					break;
				case 21:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21) {return a21;});
					break;
				case 22:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22) {return a22;});
					break;
				case 23:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23) {return a23;});
					break;
				case 24:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24) {return a24;});
					break;
				case 25:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25) {return a25;});
					break;
				case 26:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26) {return a26;});
					break;
				case 27:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27) {return a27;});
					break;
				case 28:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28) {return a28;});
					break;
				case 29:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29) {return a29;});
					break;
				case 30:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30) {return a30;});
					break;
				case 31:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30, Object a31) {return a31;});
					break;
				case 32:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30, Object a31, Object a32) {return a32;});
					break;
			}

		}
		
		#endregion

		#region Invoking

		public Object valueWithArguments(Object[] a) {
			if (a.Length != numArgs) throw new PrimInvalidOperandException("Number of parameters specified does not match the expected number");
			switch (numArgs) {
				case 0:
					return value0();
				case 1:
					return value1(a[0]);
				case 2:
					return value2(a[0], a[1]);
				case 3:
					return value3(a[0], a[1], a[2]);
				case 4:
					return value4(a[0], a[1], a[2], a[3]);
				case 5:
					return value5(a[0], a[1], a[2], a[3], a[4]);
				case 6:
					return value6(a[0], a[1], a[2], a[3], a[4], a[5]);
				case 7:
					return value7(a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
				case 8:
					return value8(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
				case 9:
					return value9(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8]);
				case 10:
					return value10(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9]);
				case 11:
					return value11(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10]);
				case 12:
					return value12(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11]);
				case 13:
					return value13(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12]);
				case 14:
					return value14(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13]);
				case 15:
					return value15(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14]);
				case 16:
					return value16(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15]);
				case 17:
					return value17(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16]);
				case 18:
					return value18(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17]);
				case 19:
					return value19(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18]);
				case 20:
					return value20(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19]);
				case 21:
					return value21(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19], a[20]);
				case 22:
					return value22(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19], a[20], a[21]);
				case 23:
					return value23(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19], a[20], a[21], a[22]);
				case 24:
					return value24(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19], a[20], a[21], a[22], a[23]);
				case 25:
					return value25(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19], a[20], a[21], a[22], a[23], a[24]);
				case 26:
					return value26(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19], a[20], a[21], a[22], a[23], a[24], a[25]);
				case 27:
					return value27(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19], a[20], a[21], a[22], a[23], a[24], a[25], a[26]);
				case 28:
					return value28(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19], a[20], a[21], a[22], a[23], a[24], a[25], a[26], a[27]);
				case 29:
					return value29(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19], a[20], a[21], a[22], a[23], a[24], a[25], a[26], a[27], a[28]);
				case 30:
					return value30(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19], a[20], a[21], a[22], a[23], a[24], a[25], a[26], a[27], a[28], a[29]);
				case 31:
					return value31(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19], a[20], a[21], a[22], a[23], a[24], a[25], a[26], a[27], a[28], a[29], a[30]);
				case 32:
					return value32(a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19], a[20], a[21], a[22], a[23], a[24], a[25], a[26], a[27], a[28], a[29], a[30], a[31]);
				default:
					throw new PrimitiveFailException("Too many arguments");
				}
		}
		
		public Object value0() {
			FuncNs.Func<Object> functor;
			try {
				functor = (FuncNs.Func<Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value0()", NumArgs, 0, function.GetType(), blockFunctionTypeForNumArgs(0), invalidCastEx);
				functor = null;
			}
			return functor();
		}

		public Object value1(Object a1) {
			FuncNs.Func<Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value1()", NumArgs, 1, function.GetType(), blockFunctionTypeForNumArgs(1), invalidCastEx);
				functor = null;
			}
			return functor(a1);
		}

		public Object value2(Object a1, Object a2) {
			FuncNs.Func<Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value2()", NumArgs, 2, function.GetType(), blockFunctionTypeForNumArgs(2), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2);
		}

		public Object value3(Object a1, Object a2, Object a3) {
			FuncNs.Func<Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value3()", NumArgs, 3, function.GetType(), blockFunctionTypeForNumArgs(3), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3);
		}

		public Object value4(Object a1, Object a2, Object a3, Object a4) {
			FuncNs.Func<Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value4()", NumArgs, 4, function.GetType(), blockFunctionTypeForNumArgs(4), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4);
		}

		public Object value5(Object a1, Object a2, Object a3, Object a4, Object a5) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value5()", NumArgs, 5, function.GetType(), blockFunctionTypeForNumArgs(5), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5);
		}

		public Object value6(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value6()", NumArgs, 6, function.GetType(), blockFunctionTypeForNumArgs(6), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6);
		}

		public Object value7(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value7()", NumArgs, 7, function.GetType(), blockFunctionTypeForNumArgs(7), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7);
		}

		public Object value8(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value8()", NumArgs, 8, function.GetType(), blockFunctionTypeForNumArgs(8), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8);
		}

		public Object value9(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value9()", NumArgs, 9, function.GetType(), blockFunctionTypeForNumArgs(9), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9);
		}

		public Object value10(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value10()", NumArgs, 10, function.GetType(), blockFunctionTypeForNumArgs(10), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10);
		}

		public Object value11(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value11()", NumArgs, 11, function.GetType(), blockFunctionTypeForNumArgs(11), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11);
		}

		public Object value12(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value12()", NumArgs, 12, function.GetType(), blockFunctionTypeForNumArgs(12), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12);
		}

		public Object value13(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value13()", NumArgs, 13, function.GetType(), blockFunctionTypeForNumArgs(13), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13);
		}

		public Object value14(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value14()", NumArgs, 14, function.GetType(), blockFunctionTypeForNumArgs(14), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14);
		}

		public Object value15(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value15()", NumArgs, 15, function.GetType(), blockFunctionTypeForNumArgs(15), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15);
		}

		public Object value16(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value16()", NumArgs, 16, function.GetType(), blockFunctionTypeForNumArgs(16), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16);
		}

		public Object value17(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value17()", NumArgs, 17, function.GetType(), blockFunctionTypeForNumArgs(17), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17);
		}

		public Object value18(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value18()", NumArgs, 18, function.GetType(), blockFunctionTypeForNumArgs(18), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18);
		}

		public Object value19(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value19()", NumArgs, 19, function.GetType(), blockFunctionTypeForNumArgs(19), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19);
		}

		public Object value20(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value20()", NumArgs, 20, function.GetType(), blockFunctionTypeForNumArgs(20), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20);
		}

		public Object value21(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value21()", NumArgs, 21, function.GetType(), blockFunctionTypeForNumArgs(21), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21);
		}

		public Object value22(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value22()", NumArgs, 22, function.GetType(), blockFunctionTypeForNumArgs(22), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22);
		}

		public Object value23(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value23()", NumArgs, 23, function.GetType(), blockFunctionTypeForNumArgs(23), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23);
		}

		public Object value24(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value24()", NumArgs, 24, function.GetType(), blockFunctionTypeForNumArgs(24), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24);
		}

		public Object value25(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value25()", NumArgs, 25, function.GetType(), blockFunctionTypeForNumArgs(25), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25);
		}

		public Object value26(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value26()", NumArgs, 26, function.GetType(), blockFunctionTypeForNumArgs(26), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26);
		}

		public Object value27(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value27()", NumArgs, 27, function.GetType(), blockFunctionTypeForNumArgs(27), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27);
		}

		public Object value28(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value28()", NumArgs, 28, function.GetType(), blockFunctionTypeForNumArgs(28), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28);
		}

		public Object value29(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value29()", NumArgs, 29, function.GetType(), blockFunctionTypeForNumArgs(29), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29);
		}

		public Object value30(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value30()", NumArgs, 30, function.GetType(), blockFunctionTypeForNumArgs(30), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30);
		}

		public Object value31(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30, Object a31) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value31()", NumArgs, 31, function.GetType(), blockFunctionTypeForNumArgs(31), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31);
		}

		public Object value32(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30, Object a31, Object a32) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Block.value32()", NumArgs, 32, function.GetType(), blockFunctionTypeForNumArgs(32), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32);
		}
		
		#endregion

		public new class Primitives : PrimitiveDomain {

			protected override void bindToObjectSpace() {
				domainClass = objectSpace.BlockClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.Block;}
			}
		
			#region Primitive Definitions

			public static Object _setFunction_(Object receiver, Object functionObject) {
				((ESBlock)receiver).Function = (Delegate)functionObject;
				return receiver;
			}

			public static Object _numArgs_(Object receiver) {
				return ((ESBlock)receiver).NumArgs;
			}
		
			public static Object _environment_(Object receiver) {
				return ((ESBlock)receiver).Environment;
			}
		
			public static Object _enclosingContext_(Object receiver) {
				return ((ESBlock)receiver).EnclosingContext;
			}
		
			public static Object _rootContext_(Object receiver) {
				return ((ESBlock)receiver).RootContext;
			}
		
			public static Object _hasHomeClass_(Object receiver) {
				return ((ESBlock)receiver).HasHomeClass;
			}
		
			public static Object _homeClass_(Object receiver) {
				return ((ESBlock)receiver).HomeClass;
			}
		
			public static Object _whileNil_(Object receiver) {
				FuncNs.Func<Object> function = asFunctor0(receiver);
				while (function() == null);
				return receiver;
			}
 		
			public static Object _whileNotNil_(Object receiver) {
				FuncNs.Func<Object> function = asFunctor0(receiver);
				while (function() != null);
				return receiver;
			}
		
			public static Object _whileNilDo_(Object receiver, Object nilAction) {
				Object actionValue = receiver;
				FuncNs.Func<Object> function = asFunctor0(receiver);
				FuncNs.Func<Object> actionFunction = asFunctor0(nilAction);
				while (function() == null) actionValue = actionFunction();
				return actionValue;
			}
 		
			public static Object _whileNotNilDo_(Object receiver, Object notNilAction) {
				Object actionValue = receiver;
				FuncNs.Func<Object> function = asFunctor0(receiver);
				FuncNs.Func<Object> actionFunction = asFunctor0(notNilAction);
				while (function() != null) actionValue = actionFunction();
				return actionValue;
			}

			public static Object _whileTrue_(Object receiver) {
				FuncNs.Func<Object> boolFunction = asFunctor0(receiver);
				while ((bool)boolFunction());
				return receiver;
			}

			public static Object _whileFalse_(Object receiver) {
				FuncNs.Func<Object> boolFunction = asFunctor0(receiver);
				while (!(bool)boolFunction());
				return receiver;
			}

			public static Object _whileTrueDo_(Object receiver, Object actionBlockObject) {
				Object actionValue = receiver;
				FuncNs.Func<Object> boolFunction = asFunctor0(receiver);
				FuncNs.Func<Object> actionFunction = asFunctor0(actionBlockObject);
				while ((bool)boolFunction()) actionValue = actionFunction();
				return actionValue;
			}

			public static Object _whileFalseDo_(Object receiver, Object actionBlockObject) {
				Object actionValue = receiver;
				FuncNs.Func<Object> boolFunction = asFunctor0(receiver);
				FuncNs.Func<Object> actionFunction = asFunctor0(actionBlockObject);
				while (!(bool)boolFunction()) actionValue = actionFunction();
				return actionValue;
			}

			public static Object _valueWithArguments_(Object receiver, Object arguments) {
				ESObject esObject = arguments as ESObject;
				Object[] argArray = esObject == null ? (Object[])arguments : esObject.asHostArray<Object>();
				return ((ESBlock)receiver).valueWithArguments(argArray);
			}

			public static Object _value0_(Object receiver) {
				FuncNs.Func<Object> functor;
				try {
					functor = (FuncNs.Func<Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value0()", ((ESBlock)receiver).NumArgs, 0, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(0), invalidCastEx);
				}
				return functor();
			}

			public static Object _value1_(Object receiver, Object a1) {
				FuncNs.Func<Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value1()", ((ESBlock)receiver).NumArgs, 1, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(1), invalidCastEx);
				}
				return functor(a1);
			}

			public static Object _value2_(Object receiver, Object a1, Object a2) {
				FuncNs.Func<Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value2()", ((ESBlock)receiver).NumArgs, 2, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(2), invalidCastEx);
				}
				return functor(a1, a2);
			}

			public static Object _value3_(Object receiver, Object a1, Object a2, Object a3) {
				FuncNs.Func<Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value3()", ((ESBlock)receiver).NumArgs, 3, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(3), invalidCastEx);
				}
				return functor(a1, a2, a3);
			}

			public static Object _value4_(Object receiver, Object a1, Object a2, Object a3, Object a4) {
				FuncNs.Func<Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value4()", ((ESBlock)receiver).NumArgs, 4, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(4), invalidCastEx);
				}
				return functor(a1, a2, a3, a4);
			}

			public static Object _value5_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value5()", ((ESBlock)receiver).NumArgs, 5, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(5), invalidCastEx);
				}
				return functor(a1, a2, a3, a4, a5);
			}

			public static Object _value6_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value6()", ((ESBlock)receiver).NumArgs, 6, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(6), invalidCastEx);
				}
				return functor(a1, a2, a3, a4, a5, a6);
			}

			public static Object _value7_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value7()", ((ESBlock)receiver).NumArgs, 7, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(7), invalidCastEx);
				}
				return functor(a1, a2, a3, a4, a5, a6, a7);
			}

			public static Object _value8_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value8()", ((ESBlock)receiver).NumArgs, 8, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(8), invalidCastEx);
				}
				return functor(a1, a2, a3, a4, a5, a6, a7, a8);
			}

			public static Object _value9_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value9()", ((ESBlock)receiver).NumArgs, 9, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(9), invalidCastEx);
				}
				return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9);
			}

			public static Object _value10_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value10()", ((ESBlock)receiver).NumArgs, 10, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(10), invalidCastEx);
				}
				return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10);
			}

			public static Object _value11_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value11()", ((ESBlock)receiver).NumArgs, 11, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(11), invalidCastEx);
				}
				return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11);
			}

			public static Object _value12_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value12()", ((ESBlock)receiver).NumArgs, 12, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(12), invalidCastEx);
				}
				return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12);
			}

			public static Object _value13_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value13()", ((ESBlock)receiver).NumArgs, 13, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(13), invalidCastEx);
				}
				return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13);
			}

			public static Object _value14_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value14()", ((ESBlock)receiver).NumArgs, 14, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(14), invalidCastEx);
				}
				return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14);
			}

			public static Object _value15_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value15()", ((ESBlock)receiver).NumArgs, 15, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(15), invalidCastEx);
				}
				return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15);
			}

			public static Object _value16_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value16()", ((ESBlock)receiver).NumArgs, 16, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(16), invalidCastEx);
				}
				return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16);
			}

			public static Object _value17_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value17()", ((ESBlock)receiver).NumArgs, 17, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(17), invalidCastEx);
				}
				return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17);
			}

			public static Object _value18_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value18()", ((ESBlock)receiver).NumArgs, 18, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(18), invalidCastEx);
				}
				return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18);
			}

			public static Object _value19_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value19()", ((ESBlock)receiver).NumArgs, 19, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(19), invalidCastEx);
				}
				return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19);
			}

			public static Object _value20_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value20()", ((ESBlock)receiver).NumArgs, 20, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(20), invalidCastEx);
				}
				return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20);
			}

			public static Object _value21_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value21()", ((ESBlock)receiver).NumArgs, 21, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(21), invalidCastEx);
				}
				return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21);
			}

			public static Object _value22_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value22()", ((ESBlock)receiver).NumArgs, 22, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(22), invalidCastEx);
				}
				return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22);
			}

			public static Object _value23_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value23()", ((ESBlock)receiver).NumArgs, 23, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(23), invalidCastEx);
				}
				return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23);
			}

			public static Object _value24_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value24()", ((ESBlock)receiver).NumArgs, 24, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(24), invalidCastEx);
				}
				return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24);
			}

			public static Object _value25_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value25()", ((ESBlock)receiver).NumArgs, 25, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(25), invalidCastEx);
				}
				return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25);
			}

			public static Object _value26_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value26()", ((ESBlock)receiver).NumArgs, 26, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(26), invalidCastEx);
				}
				return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26);
			}

			public static Object _value27_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value27()", ((ESBlock)receiver).NumArgs, 27, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(27), invalidCastEx);
				}
				return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27);
			}

			public static Object _value28_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value28()", ((ESBlock)receiver).NumArgs, 28, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(28), invalidCastEx);
				}
				return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28);
			}

			public static Object _value29_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value29()", ((ESBlock)receiver).NumArgs, 29, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(29), invalidCastEx);
				}
				return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29);
			}

			public static Object _value30_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value30()", ((ESBlock)receiver).NumArgs, 30, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(30), invalidCastEx);
				}
				return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30);
			}

			public static Object _value31_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30, Object a31) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value31()", ((ESBlock)receiver).NumArgs, 31, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(31), invalidCastEx);
				}
				return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31);
			}

			public static Object _value32_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30, Object a31, Object a32) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESBlock)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Block.value32()", ((ESBlock)receiver).NumArgs, 32, ((ESBlock)receiver).function.GetType(), blockFunctionTypeForNumArgs(32), invalidCastEx);
				}
				return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32);
			}

			#endregion
		
			public override void publishCanonicalPrimitives() {

				publishPrimitive("function:",					new FuncNs.Func<Object, Object, Object>(_setFunction_));

				publishPrimitive("numArgs",					new FuncNs.Func<Object, Object>(_numArgs_));

				publishPrimitive("environment",					new FuncNs.Func<Object, Object>(_environment_));
				publishPrimitive("enclosingContext",				new FuncNs.Func<Object, Object>(_enclosingContext_));
				publishPrimitive("rootContext",					new FuncNs.Func<Object, Object>(_rootContext_));
				publishPrimitive("hasHomeClass",				new FuncNs.Func<Object, Object>(_hasHomeClass_));
				publishPrimitive("homeClass",					new FuncNs.Func<Object, Object>(_homeClass_));

				publishPrimitive("whileNil",					new FuncNs.Func<Object, Object>(_whileNil_));
				publishPrimitive("whileNotNil",					new FuncNs.Func<Object, Object>(_whileNotNil_));
				publishPrimitive("whileNil:",					new FuncNs.Func<Object, Object, Object>(_whileNilDo_));
				publishPrimitive("whileNotNil:",				new FuncNs.Func<Object, Object, Object>(_whileNotNilDo_));

				publishPrimitive("whileFalse",					new FuncNs.Func<Object, Object>(_whileFalse_));
				publishPrimitive("whileTrue",					new FuncNs.Func<Object, Object>(_whileTrue_));
				publishPrimitive("whileFalse:",					new FuncNs.Func<Object, Object, Object>(_whileFalseDo_));
				publishPrimitive("whileTrue:",					new FuncNs.Func<Object, Object, Object>(_whileTrueDo_));

				publishPrimitive("valueWithArguments:",				new FuncNs.Func<Object, Object, Object>(_valueWithArguments_));

				publishPrimitive("value",					new FuncNs.Func<Object, Object>(_value0_));
				publishPrimitive("value:",					new FuncNs.Func<Object, Object, Object>(_value1_));
				publishPrimitive("value:value:",					new FuncNs.Func<Object, Object, Object, Object>(_value2_));
				publishPrimitive("value:value:value:",					new FuncNs.Func<Object, Object, Object, Object, Object>(_value3_));
				publishPrimitive("value:value:value:value:",					new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_value4_));
				publishPrimitive("value:value:value:value:value:",					new FuncNs.Func<Object, Object, Object, Object, Object, Object, Object>(_value5_));
				publishPrimitive("value:value:value:value:value:value:",					new FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object>(_value6_));
				publishPrimitive("value:value:value:value:value:value:value:",					new FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object>(_value7_));
				publishPrimitive("value:value:value:value:value:value:value:value:",					new FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>(_value8_));
				publishPrimitive("value:value:value:value:value:value:value:value:value:",					new FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>(_value9_));
				publishPrimitive("value:value:value:value:value:value:value:value:value:value:",					new FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>(_value10_));
				publishPrimitive("value:value:value:value:value:value:value:value:value:value:value:",					new FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>(_value11_));
				publishPrimitive("value:value:value:value:value:value:value:value:value:value:value:value:",					new FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>(_value12_));
				publishPrimitive("value:value:value:value:value:value:value:value:value:value:value:value:value:",					new FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>(_value13_));
				publishPrimitive("value:value:value:value:value:value:value:value:value:value:value:value:value:value:",					new FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>(_value14_));
				publishPrimitive("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:",					new FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>(_value15_));
				publishPrimitive("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:",					new Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>(_value16_));
				publishPrimitive("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:",					new Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>(_value17_));
				publishPrimitive("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:",					new Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>(_value18_));
				publishPrimitive("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:",					new Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>(_value19_));
				publishPrimitive("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:",					new Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>(_value20_));
				publishPrimitive("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:",					new Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>(_value21_));
				publishPrimitive("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:",					new Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>(_value22_));
				publishPrimitive("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:",					new Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>(_value23_));
				publishPrimitive("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:",					new Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>(_value24_));
				publishPrimitive("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:",					new Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>(_value25_));
				publishPrimitive("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:",					new Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>(_value26_));
				publishPrimitive("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:",					new Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>(_value27_));
				publishPrimitive("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:",					new Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>(_value28_));
				publishPrimitive("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:",					new Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>(_value29_));
				publishPrimitive("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:",					new Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>(_value30_));
				publishPrimitive("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:",					new Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>(_value31_));
				publishPrimitive("value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:value:",					new Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>(_value32_));
			}

		}
		
	}

	public class InlineOperation {
		protected MethodOperationType	type			= MethodOperationType.Function;
		protected String		operand;
		protected Expression		onFailExpression	= Expression.Empty();

		public InlineOperation(MethodOperationType type, String operand) {
			this.type		= type;
			this.operand		= operand;
		}

		public MethodOperationType Type {
			get {return type;}
		}

		public String Operand {
			get {return operand;}
		}

		public Expression OnFailExpression {
			get {return onFailExpression;}
			set {onFailExpression = value ?? Expression.Empty();}
		}

	}

	public class MethodIdentityComparator : IdentityComparator<ESMethod> {}

	public class ESMethod : ESCompiledCode {

		#region Static variables and functions

		private static long										identityGenerator			= 0;

		#endregion

		protected long											identity;
		protected NamespaceObject									environment;
		protected BehavioralObject 									homeClass;
		protected ESSymbol										selector;
		protected MethodDeclarationNode									methodDeclarationNode;
		protected InlineOperation									inlineOperation;
		protected HashSet<ESSymbol>									protocols;

		#region Constructors

		internal ESMethod(ESBehavior esClass) : base(esClass) {
			identity			= identityGenerator++;
		}
		
		public ESMethod(ESBehavior esClass, ESSymbol selector, Delegate function) : this(esClass, selector, function, null, null) {
		}
		
		public ESMethod(ESBehavior esClass, ESSymbol selector, Delegate function, NamespaceObject environment, BehavioralObject homeClass) : this(esClass, selector, function, environment, homeClass, null) {
		}
		
		public ESMethod(ESBehavior esClass, ESSymbol selector, Delegate function, ESSymbol protocol) : this(esClass, selector, function, null, null, protocol) {
		}
		
		public ESMethod(ESBehavior esClass, ESSymbol selector, Delegate function, NamespaceObject environment, BehavioralObject homeClass, ESSymbol protocol) : this(esClass, selector, null, function, environment, homeClass, protocol) {
		}
		
		public ESMethod(ESBehavior esClass, ESSymbol selector, InlineOperation inlineOperation, Delegate function) : this(esClass, selector, inlineOperation, function, null, null) {
		}
		
		public ESMethod(ESBehavior esClass, ESSymbol selector, InlineOperation inlineOperation, Delegate function, NamespaceObject environment, BehavioralObject homeClass) : this(esClass, selector, inlineOperation, function, environment, homeClass, null) {
		}

		public ESMethod(ESBehavior esClass, ESSymbol selector, InlineOperation inlineOperation, Delegate function, ESSymbol protocol) : this(esClass, selector, inlineOperation, function, null, null, protocol) {
		}
		
		public ESMethod(ESBehavior esClass, ESSymbol selector, InlineOperation inlineOperation, Delegate function, NamespaceObject environment, BehavioralObject homeClass, ESSymbol protocol) : base(esClass) {
			identity			= identityGenerator++;
			this.environment		= environment ?? homeClass;
			this.homeClass			= homeClass ?? environment as BehavioralObject;
			Selector			= selector;
			this.inlineOperation		= inlineOperation;
			Function			= function;
			addToProtocol(protocol);
		}
		
		public ESMethod(ESBehavior esClass, NamespaceObject environment, BehavioralObject homeClass, MethodDeclarationNode methodDeclarationNode) : this(esClass, environment, homeClass, methodDeclarationNode, null) {
		}
		
		public ESMethod(ESBehavior esClass, NamespaceObject environment, BehavioralObject homeClass, MethodDeclarationNode methodDeclarationNode, ESSymbol protocol) : base(esClass) {
			identity			= identityGenerator++;
			this.environment		= environment ?? homeClass;
			this.homeClass			= homeClass ?? environment as BehavioralObject;
			this.methodDeclarationNode	= methodDeclarationNode;
			Selector			= methodDeclarationNode.Selector;
			this.inlineOperation		= methodDeclarationNode.InlineOperation;
			Function			= methodDeclarationNode.functionFor(this.environment, this.homeClass);
			addToProtocol(protocol);
		}

		#endregion

		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.Method;}
		}

		public long Identity {
			get {return identity;}
		}
		public override bool IsMethod {
			get {return true;}
		}
		
		public override ESMethod asESMethod() {
			return this;
		}
		
		public MethodDeclarationNode MethodDeclarationNode {
			get {return methodDeclarationNode;}
		}

		public MethodOperationType OperationType {
			get {return inlineOperation == null ? MethodOperationType.Function : inlineOperation.Type;}
		}

		public bool RepresentsInlineOperation {
			get {return inlineOperation != null;}
		}

		public InlineOperation InlineOperation {
			get {return inlineOperation;}
		}
		
		public override NamespaceObject Environment {
			get {return environment;}
		}

		public override ESCompiledCode RootContext {
			get {return this;}
		}
		
		public override bool HasHomeClass {
			get {return homeClass != null;}
		}

		public override BehavioralObject HomeClass {
			get {return homeClass;}
		}

		public bool IsClassMethod {
			get {
				return homeClass == null ? false : homeClass.IsMetaclass;
			}
		}
		public bool IsInstanceMethod {
			get {
				return !IsClassMethod;
			}
		}
		
		public ESSymbol Selector {
			get {return selector;}
			set {	selector = value;
				numArgs = selector == null ? 0 : selector.NumArgs;}
		}
		
		public override long NumArgs {
			get {return numArgs;}
			set {}
		}
		
		public long ProtocolCount {
			get {return protocols == null ? 0 : protocols.Count;}
		}

		public HashSet<ESSymbol> Protocols {
 			get {return protocols;}
		}

		internal void setEnvironmentAndHomeClass(NamespaceObject newEnvironment, BehavioralObject newHomeClass) {
			var isNewEnvironment = environment != newEnvironment;
			var isNewHomeClass = homeClass != newHomeClass;
			if (isNewEnvironment || isNewHomeClass) { 
				environment = newEnvironment;
				homeClass = newHomeClass;
				recompile();
			}
		}

		public void recompile() {
			if (homeClass == null || methodDeclarationNode == null) return;
			methodDeclarationNode.compileFor(this);
		}

		internal void become(ESMethod other) {
			identity = other.Identity;
			methodDeclarationNode = other.MethodDeclarationNode;
			inlineOperation = other.InlineOperation;
			var otherHomeClass = other.HomeClass;
			if (homeClass == otherHomeClass) { 
				var otherFunction = other.Function;
				if (otherFunction == null) {
					recompile();
				} else { 
					Function = otherFunction;
				}
			} else { 
				homeClass = otherHomeClass;
				recompile();
			}
			if (other.ProtocolCount > 0) foreach (var protocol in other.Protocols) addToProtocol(protocol);
		}
		
		public ESMethod newCopyIn(BehavioralObject newHomeClass) {
			var newCopy = (ESMethod)shallowCopy();
			newCopy.setEnvironmentAndHomeClass(Environment, newHomeClass);
			return newCopy;
		}

		protected HashSet<ESSymbol> newProtocolSet() {
			return new HashSet<ESSymbol>(new SymbolIdentityComparator());
		}

		public void protocolsDo(FuncNs.Func<Object, Object> enumerator1) {
			if (protocols == null) return;
			foreach (var protocol in protocols) enumerator1(protocol);
		}

		public void addToProtocol(ESSymbol protocol) {
			if (protocol == null) return;
			if (protocols == null) protocols = newProtocolSet();
			protocols.Add(protocol);
		}

		public void removeFromProtocol(ESSymbol protocol) {
			if (protocol == null) return;
			if (protocols == null) return;
			protocols.Remove(protocol);
			if (protocols.Count < 1) protocols = null;
		}

		public void removeFromAllProtocols() {
			protocols = null;
		}

		public HashSet<ESSymbol> MessagesSent {
			get {return methodDeclarationNode == null ? null : methodDeclarationNode.MessagesSent;}
		}

		public HashSet<ESSymbol> MessagesSentToSelf {
			get {return methodDeclarationNode == null ? null : methodDeclarationNode.MessagesSentToSelf;}
		}

		public HashSet<ESSymbol> MessagesSentToSuper {
			get {return methodDeclarationNode == null ? null : methodDeclarationNode.MessagesSentToSuper;}
		}

		public HashSet<ESSymbol> MessagesSentToThisContext {
			get {return methodDeclarationNode == null ? null : methodDeclarationNode.MessagesSentToThisContext;}
		}

		public void withUndeclaredVariablesDo(Action<ESSymbol> enumerator1) {
			if (methodDeclarationNode == null) return;
			var undeclaredVariables = methodDeclarationNode.UndeclaredVariables;
			if (undeclaredVariables == null) return;
			foreach (var varName in undeclaredVariables) enumerator1(varName);
		}

		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (homeClass == null) {
				append(" ??");
			} else {
				append(homeClass.Name.PrimitiveValue);
			}
			append(">>");
			if (selector == null) {
				append("??");
			} else { 
				append(selector.PrimitiveValue);
			}
			append(" ");
		}

		public override DynamicMetaObject GetMetaObject(Expression parameter) {
			return new ESMethodDynamicMetaObject(parameter, BindingRestrictions.Empty, this, Class);
		}

		public override T valueBy<T>(Operation<T> operation) {
			return operation.applyToCompiledMethod(this);
		}
		
		#region Setting Default Function
		
		public override void setFunctionToDefault(out Delegate function) {

			switch (NumArgs) {
				default:
				case 0:
					function = (FuncNs.Func<Object, Object>)(delegate (Object receiver) {return receiver;});
					break;
				case 1:
					function = (FuncNs.Func<Object, Object, Object>)(delegate (Object receiver, Object a1) {return a1;});
					break;
				case 2:
					function = (FuncNs.Func<Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2) {return a2;});
					break;
				case 3:
					function = (FuncNs.Func<Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3) {return a3;});
					break;
				case 4:
					function = (FuncNs.Func<Object, Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3, Object a4) {return a4;});
					break;
				case 5:
					function = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5) {return a5;});
					break;
				case 6:
					function = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6) {return a6;});
					break;
				case 7:
					function = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7) {return a7;});
					break;
				case 8:
					function = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8) {return a8;});
					break;
				case 9:
					function = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9) {return a9;});
					break;
				case 10:
					function = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10) {return a10;});
					break;
				case 11:
					function = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11) {return a11;});
					break;
				case 12:
					function = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12) {return a12;});
					break;
				case 13:
					function = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13) {return a13;});
					break;
				case 14:
					function = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14) {return a14;});
					break;
				case 15:
					function = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15) {return a15;});
					break;
				case 16:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16) {return a16;});
					break;
				case 17:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17) {return a17;});
					break;
				case 18:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18) {return a18;});
					break;
				case 19:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19) {return a19;});
					break;
				case 20:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20) {return a20;});
					break;
				case 21:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21) {return a21;});
					break;
				case 22:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22) {return a22;});
					break;
				case 23:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23) {return a23;});
					break;
				case 24:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24) {return a24;});
					break;
				case 25:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25) {return a25;});
					break;
				case 26:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26) {return a26;});
					break;
				case 27:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27) {return a27;});
					break;
				case 28:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28) {return a28;});
					break;
				case 29:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29) {return a29;});
					break;
				case 30:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30) {return a30;});
					break;
				case 31:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30, Object a31) {return a31;});
					break;
				case 32:
					function = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)(delegate (Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30, Object a31, Object a32) {return a32;});
					break;
			}

		}
		
		#endregion
		
		#region Invoking
		
		public Object valueWithReceiverWithArguments(Object self, Object[] a) {
			if (a.Length - numArgs != 0) throw new PrimitiveFailException("Number of parameters specified does not match the expected number");
			switch (numArgs) {
				case 0:
					return value0(self);
				case 1:
					return value1(self, a[0]);
				case 2:
					return value2(self, a[0], a[1]);
				case 3:
					return value3(self, a[0], a[1], a[2]);
				case 4:
					return value4(self, a[0], a[1], a[2], a[3]);
				case 5:
					return value5(self, a[0], a[1], a[2], a[3], a[4]);
				case 6:
					return value6(self, a[0], a[1], a[2], a[3], a[4], a[5]);
				case 7:
					return value7(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6]);
				case 8:
					return value8(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7]);
				case 9:
					return value9(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8]);
				case 10:
					return value10(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9]);
				case 11:
					return value11(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10]);
				case 12:
					return value12(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11]);
				case 13:
					return value13(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12]);
				case 14:
					return value14(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13]);
				case 15:
					return value15(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14]);
				case 16:
					return value16(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15]);
				case 17:
					return value17(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16]);
				case 18:
					return value18(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17]);
				case 19:
					return value19(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18]);
				case 20:
					return value20(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19]);
				case 21:
					return value21(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19], a[20]);
				case 22:
					return value22(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19], a[20], a[21]);
				case 23:
					return value23(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19], a[20], a[21], a[22]);
				case 24:
					return value24(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19], a[20], a[21], a[22], a[23]);
				case 25:
					return value25(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19], a[20], a[21], a[22], a[23], a[24]);
				case 26:
					return value26(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19], a[20], a[21], a[22], a[23], a[24], a[25]);
				case 27:
					return value27(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19], a[20], a[21], a[22], a[23], a[24], a[25], a[26]);
				case 28:
					return value28(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19], a[20], a[21], a[22], a[23], a[24], a[25], a[26], a[27]);
				case 29:
					return value29(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19], a[20], a[21], a[22], a[23], a[24], a[25], a[26], a[27], a[28]);
				case 30:
					return value30(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19], a[20], a[21], a[22], a[23], a[24], a[25], a[26], a[27], a[28], a[29]);
				case 31:
					return value31(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19], a[20], a[21], a[22], a[23], a[24], a[25], a[26], a[27], a[28], a[29], a[30]);
				case 32:
					return value32(self, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19], a[20], a[21], a[22], a[23], a[24], a[25], a[26], a[27], a[28], a[29], a[30], a[31]);
				default:
					throw new PrimitiveFailException("Too many arguments");
			}
		}
		
		public Object value0(Object self) {
			FuncNs.Func<Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver0()", NumArgs, 0, function.GetType(), methodFunctionTypeForNumArgs(0), invalidCastEx);
				functor = null;
			}
			return functor(self);
		}

		public Object value1(Object self, Object a1) {
			FuncNs.Func<Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver1()", NumArgs, 1, function.GetType(), methodFunctionTypeForNumArgs(1), invalidCastEx);
				functor = null;
			}
			return functor(self, a1);
		}

		public Object value2(Object self, Object a1, Object a2) {
			FuncNs.Func<Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver2()", NumArgs, 2, function.GetType(), methodFunctionTypeForNumArgs(2), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2);
		}

		public Object value3(Object self, Object a1, Object a2, Object a3) {
			FuncNs.Func<Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver3()", NumArgs, 3, function.GetType(), methodFunctionTypeForNumArgs(3), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3);
		}

		public Object value4(Object self, Object a1, Object a2, Object a3, Object a4) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver4()", NumArgs, 4, function.GetType(), methodFunctionTypeForNumArgs(4), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4);
		}

		public Object value5(Object self, Object a1, Object a2, Object a3, Object a4, Object a5) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver5()", NumArgs, 5, function.GetType(), methodFunctionTypeForNumArgs(5), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5);
		}

		public Object value6(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver6()", NumArgs, 6, function.GetType(), methodFunctionTypeForNumArgs(6), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6);
		}

		public Object value7(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver7()", NumArgs, 7, function.GetType(), methodFunctionTypeForNumArgs(7), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7);
		}

		public Object value8(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver8()", NumArgs, 8, function.GetType(), methodFunctionTypeForNumArgs(8), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8);
		}

		public Object value9(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver9()", NumArgs, 9, function.GetType(), methodFunctionTypeForNumArgs(9), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9);
		}

		public Object value10(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver10()", NumArgs, 10, function.GetType(), methodFunctionTypeForNumArgs(10), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10);
		}

		public Object value11(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver11()", NumArgs, 11, function.GetType(), methodFunctionTypeForNumArgs(11), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11);
		}

		public Object value12(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver12()", NumArgs, 12, function.GetType(), methodFunctionTypeForNumArgs(12), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12);
		}

		public Object value13(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver13()", NumArgs, 13, function.GetType(), methodFunctionTypeForNumArgs(13), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13);
		}

		public Object value14(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver14()", NumArgs, 14, function.GetType(), methodFunctionTypeForNumArgs(14), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14);
		}

		public Object value15(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver15()", NumArgs, 15, function.GetType(), methodFunctionTypeForNumArgs(15), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15);
		}

		public Object value16(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver16()", NumArgs, 16, function.GetType(), methodFunctionTypeForNumArgs(16), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16);
		}

		public Object value17(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver17()", NumArgs, 17, function.GetType(), methodFunctionTypeForNumArgs(17), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17);
		}

		public Object value18(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver18()", NumArgs, 18, function.GetType(), methodFunctionTypeForNumArgs(18), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18);
		}

		public Object value19(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver19()", NumArgs, 19, function.GetType(), methodFunctionTypeForNumArgs(19), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19);
		}

		public Object value20(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver20()", NumArgs, 20, function.GetType(), methodFunctionTypeForNumArgs(20), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20);
		}

		public Object value21(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver21()", NumArgs, 21, function.GetType(), methodFunctionTypeForNumArgs(21), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21);
		}

		public Object value22(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver22()", NumArgs, 22, function.GetType(), methodFunctionTypeForNumArgs(22), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22);
		}

		public Object value23(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver23()", NumArgs, 23, function.GetType(), methodFunctionTypeForNumArgs(23), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23);
		}

		public Object value24(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver24()", NumArgs, 24, function.GetType(), methodFunctionTypeForNumArgs(24), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24);
		}

		public Object value25(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver25()", NumArgs, 25, function.GetType(), methodFunctionTypeForNumArgs(25), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25);
		}

		public Object value26(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver26()", NumArgs, 26, function.GetType(), methodFunctionTypeForNumArgs(26), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26);
		}

		public Object value27(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver27()", NumArgs, 27, function.GetType(), methodFunctionTypeForNumArgs(27), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27);
		}

		public Object value28(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver28()", NumArgs, 28, function.GetType(), methodFunctionTypeForNumArgs(28), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28);
		}

		public Object value29(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver29()", NumArgs, 29, function.GetType(), methodFunctionTypeForNumArgs(29), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29);
		}

		public Object value30(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver30()", NumArgs, 30, function.GetType(), methodFunctionTypeForNumArgs(30), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30);
		}

		public Object value31(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30, Object a31) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver31()", NumArgs, 31, function.GetType(), methodFunctionTypeForNumArgs(31), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31);
		}

		public Object value32(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30, Object a31, Object a32) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("Method.valueWithReceiver32()", NumArgs, 32, function.GetType(), methodFunctionTypeForNumArgs(32), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32);
		}

		#endregion

		public new class Primitives : PrimitiveDomain {

			protected override void bindToObjectSpace() {
				domainClass = objectSpace.MethodClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.Method;}
			}
		
			#region Primitive Definitions
		
			public static Object _environment_(Object receiver) {
				return ((ESMethod)receiver).Environment;
			}
		
			public static Object _rootContext_(Object receiver) {
				return ((ESMethod)receiver).RootContext;
			}
		
			public static Object _hasHomeClass_(Object receiver) {
				return ((ESMethod)receiver).HasHomeClass;
			}
		
			public static Object _homeClass_(Object receiver) {
				return ((ESMethod)receiver).HomeClass;
			}
		
			public static Object _selector_(Object receiver) {
				return ((ESMethod)receiver).Selector;
			}
		
			public static Object _numArgs_(Object receiver) {
				return ((ESMethod)receiver).NumArgs;
			}
		
			public static Object _protocolCount_(Object receiver) {
				return ((ESMethod)receiver).ProtocolCount;
			}

			public static Object _protocolsDo_(Object receiver, Object enumerator1) {
				((ESMethod)receiver).protocolsDo(asFunctor1(enumerator1));
				return receiver;
			}

			public Object _addToProtocol_(Object receiver, Object protocol) {
				((ESMethod)receiver).addToProtocol(objectSpace.asESSymbol(protocol));
				return receiver;
			}

			public Object _removeFromProtocol_(Object receiver, Object protocol) {
				((ESMethod)receiver).removeFromProtocol(objectSpace.asESSymbol(protocol));
				return receiver;
			}

			public static Object _removeFromAllProtocols_(Object receiver) {
				((ESMethod)receiver).removeFromAllProtocols();
				return receiver;
			}

			public static Object _messagesSent_(Object receiver) {
				return ((ESMethod)receiver).MessagesSent;
			}

			public static Object _messagesSentToSelf_(Object receiver) {
				return ((ESMethod)receiver).MessagesSentToSelf;
			}

			public static Object _messagesSentToSuper_(Object receiver) {
				return ((ESMethod)receiver).MessagesSentToSuper;
			}

			public static Object _messagesSentToThisContext_(Object receiver) {
				return ((ESMethod)receiver).MessagesSentToThisContext;
			}

			public static Object _withUndeclaredVariablesDo_(Object receiver, Object enumerator1) {
				var functor1 = asFunctor1(enumerator1);
				((ESMethod)receiver).withUndeclaredVariablesDo(undeclaredVar => functor1(undeclaredVar));
				return receiver;
			}

			public Object _asMessageSend_(Object receiver) {
				return objectSpace.newMessageSend((ESMethod)receiver);
			}

			public Object _asMessageSendWithReceiver_(Object receiver, Object messageSendReceiver) {
				return objectSpace.newMessageSend(messageSendReceiver, (ESMethod)receiver);
			}

			public static Object _valueWithReceiverWithArguments_(Object receiver, Object self, Object arguments) {
				ESObject esObject = arguments as ESObject;
				Object[] argArray = esObject == null ? (Object[])arguments : esObject.asHostArray<Object>();
				return ((ESMethod)receiver).valueWithReceiverWithArguments(self, argArray);
			}

			public static Object _valueWithReceiver_(Object receiver, Object self) {
				FuncNs.Func<Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value0()", ((ESMethod)receiver).NumArgs, 0, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(0), invalidCastEx);
				}
				return functor(self);
			}

			public static Object _valueWithReceiver1_(Object receiver, Object self, Object a1) {
				FuncNs.Func<Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value1()", ((ESMethod)receiver).NumArgs, 1, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(1), invalidCastEx);
				}
				return functor(self, a1);
			}

			public static Object _valueWithReceiver2_(Object receiver, Object self, Object a1, Object a2) {
				FuncNs.Func<Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value2()", ((ESMethod)receiver).NumArgs, 2, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(2), invalidCastEx);
				}
				return functor(self, a1, a2);
			}

			public static Object _valueWithReceiver3_(Object receiver, Object self, Object a1, Object a2, Object a3) {
				FuncNs.Func<Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value3()", ((ESMethod)receiver).NumArgs, 3, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(3), invalidCastEx);
				}
				return functor(self, a1, a2, a3);
			}

			public static Object _valueWithReceiver4_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value4()", ((ESMethod)receiver).NumArgs, 4, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(4), invalidCastEx);
				}
				return functor(self, a1, a2, a3, a4);
			}

			public static Object _valueWithReceiver5_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value5()", ((ESMethod)receiver).NumArgs, 5, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(5), invalidCastEx);
				}
				return functor(self, a1, a2, a3, a4, a5);
			}

			public static Object _valueWithReceiver6_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value6()", ((ESMethod)receiver).NumArgs, 6, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(6), invalidCastEx);
				}
				return functor(self, a1, a2, a3, a4, a5, a6);
			}

			public static Object _valueWithReceiver7_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value7()", ((ESMethod)receiver).NumArgs, 7, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(7), invalidCastEx);
				}
				return functor(self, a1, a2, a3, a4, a5, a6, a7);
			}

			public static Object _valueWithReceiver8_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value8()", ((ESMethod)receiver).NumArgs, 8, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(8), invalidCastEx);
				}
				return functor(self, a1, a2, a3, a4, a5, a6, a7, a8);
			}

			public static Object _valueWithReceiver9_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value9()", ((ESMethod)receiver).NumArgs, 9, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(9), invalidCastEx);
				}
				return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9);
			}

			public static Object _valueWithReceiver10_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value10()", ((ESMethod)receiver).NumArgs, 10, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(10), invalidCastEx);
				}
				return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10);
			}

			public static Object _valueWithReceiver11_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value11()", ((ESMethod)receiver).NumArgs, 11, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(11), invalidCastEx);
				}
				return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11);
			}

			public static Object _valueWithReceiver12_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value12()", ((ESMethod)receiver).NumArgs, 12, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(12), invalidCastEx);
				}
				return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12);
			}

			public static Object _valueWithReceiver13_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value13()", ((ESMethod)receiver).NumArgs, 13, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(13), invalidCastEx);
				}
				return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13);
			}

			public static Object _valueWithReceiver14_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value14()", ((ESMethod)receiver).NumArgs, 14, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(14), invalidCastEx);
				}
				return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14);
			}

			public static Object _valueWithReceiver15_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value15()", ((ESMethod)receiver).NumArgs, 15, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(15), invalidCastEx);
				}
				return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15);
			}

			public static Object _valueWithReceiver16_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value16()", ((ESMethod)receiver).NumArgs, 16, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(16), invalidCastEx);
				}
				return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16);
			}

			public static Object _valueWithReceiver17_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value17()", ((ESMethod)receiver).NumArgs, 17, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(17), invalidCastEx);
				}
				return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17);
			}

			public static Object _valueWithReceiver18_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value18()", ((ESMethod)receiver).NumArgs, 18, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(18), invalidCastEx);
				}
				return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18);
			}

			public static Object _valueWithReceiver19_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value19()", ((ESMethod)receiver).NumArgs, 19, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(19), invalidCastEx);
				}
				return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19);
			}

			public static Object _valueWithReceiver20_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value20()", ((ESMethod)receiver).NumArgs, 20, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(20), invalidCastEx);
				}
				return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20);
			}

			public static Object _valueWithReceiver21_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value21()", ((ESMethod)receiver).NumArgs, 21, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(21), invalidCastEx);
				}
				return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21);
			}

			public static Object _valueWithReceiver22_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value22()", ((ESMethod)receiver).NumArgs, 22, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(22), invalidCastEx);
				}
				return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22);
			}

			public static Object _valueWithReceiver23_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value23()", ((ESMethod)receiver).NumArgs, 23, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(23), invalidCastEx);
				}
				return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23);
			}

			public static Object _valueWithReceiver24_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value24()", ((ESMethod)receiver).NumArgs, 24, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(24), invalidCastEx);
				}
				return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24);
			}

			public static Object _valueWithReceiver25_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value25()", ((ESMethod)receiver).NumArgs, 25, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(25), invalidCastEx);
				}
				return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25);
			}

			public static Object _valueWithReceiver26_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value26()", ((ESMethod)receiver).NumArgs, 26, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(26), invalidCastEx);
				}
				return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26);
			}

			public static Object _valueWithReceiver27_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value27()", ((ESMethod)receiver).NumArgs, 27, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(27), invalidCastEx);
				}
				return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27);
			}

			public static Object _valueWithReceiver28_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value28()", ((ESMethod)receiver).NumArgs, 28, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(28), invalidCastEx);
				}
				return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28);
			}

			public static Object _valueWithReceiver29_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value29()", ((ESMethod)receiver).NumArgs, 29, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(29), invalidCastEx);
				}
				return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29);
			}

			public static Object _valueWithReceiver30_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value30()", ((ESMethod)receiver).NumArgs, 30, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(30), invalidCastEx);
				}
				return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30);
			}

			public static Object _valueWithReceiver31_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30, Object a31) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value31()", ((ESMethod)receiver).NumArgs, 31, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(31), invalidCastEx);
				}
				return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31);
			}

			public static Object _valueWithReceiver32_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30, Object a31, Object a32) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMethod)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("Method.value32()", ((ESMethod)receiver).NumArgs, 32, ((ESMethod)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(32), invalidCastEx);
				}
				return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32);
			}

			#endregion

			public override void publishCanonicalPrimitives() {

				publishPrimitive("environment",					new FuncNs.Func<Object, Object>(_environment_));
				publishPrimitive("rootContext",					new FuncNs.Func<Object, Object>(_rootContext_));
				publishPrimitive("hasHomeClass",				new FuncNs.Func<Object, Object>(_hasHomeClass_));
				publishPrimitive("homeClass",					new FuncNs.Func<Object, Object>(_homeClass_));
	
				publishPrimitive("numArgs",					new FuncNs.Func<Object, Object>(_numArgs_));
				publishPrimitive("selector",					new FuncNs.Func<Object, Object>(_selector_));

				publishPrimitive("protocolCount",				new FuncNs.Func<Object, Object>(_protocolCount_));
				publishPrimitive("protocolsDo:",				new FuncNs.Func<Object, Object, Object>(_protocolsDo_));
				publishPrimitive("addToProtocol:",				new FuncNs.Func<Object, Object, Object>(_addToProtocol_));
				publishPrimitive("removeFromProtocol:",				new FuncNs.Func<Object, Object, Object>(_removeFromProtocol_));
				publishPrimitive("removeFromAllProtocols",			new FuncNs.Func<Object, Object>(_removeFromAllProtocols_));

				publishPrimitive("messagesSent",				new FuncNs.Func<Object, Object>(_messagesSent_));
				publishPrimitive("messagesSentToSelf",				new FuncNs.Func<Object, Object>(_messagesSentToSelf_));
				publishPrimitive("messagesSentToSuper",				new FuncNs.Func<Object, Object>(_messagesSentToSuper_));
				publishPrimitive("messagesSentToThisContext",			new FuncNs.Func<Object, Object>(_messagesSentToThisContext_));

				publishPrimitive("withUndeclaredVariablesDo:",			new FuncNs.Func<Object, Object, Object>(_withUndeclaredVariablesDo_));

				publishPrimitive("asMessageSend",				new FuncNs.Func<Object, Object>(_asMessageSend_));
				publishPrimitive("asMessageSendWithReceiver:",			new FuncNs.Func<Object, Object, Object>(_asMessageSendWithReceiver_));
				
				publishPrimitive("valueWithReceiver:withArguments:",		new FuncNs.Func<Object, Object, Object, Object>(_valueWithReceiverWithArguments_));

				publishPrimitive("valueWithReceiver:",				new FuncNs.Func<Object, Object, Object>(_valueWithReceiver_));
				publishPrimitive("valueWithReceiver:with:",			new FuncNs.Func<Object, Object, Object, Object>(_valueWithReceiver1_));
				publishPrimitive("valueWithReceiver:with:with:",		new FuncNs.Func<Object, Object, Object, Object, Object>(_valueWithReceiver2_));
				publishPrimitive("valueWithReceiver:with:with:with:",		new FuncNs.Func<Object, Object, Object, Object, Object, Object>(_valueWithReceiver3_));
				publishPrimitive("valueWithReceiver:with:with:with:with:",	new FuncNs.Func<Object, Object, Object, Object, Object, Object, Object>(_valueWithReceiver4_));
				publishPrimitive("valueWithReceiver:with:with:with:with:with:",	new FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object>(_valueWithReceiver5_));

			}

		}
		
	}

}
