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

		protected Delegate											function					= null;
		protected long 												numArgs 					= 0;
		
		protected ESCompiledCode(ESBehavior esClass) : base(esClass) {
			setFunctionToDefault(out function);
		}
		
		public ESCompiledCode(ESBehavior esClass, Delegate function, long numArgs) : base(esClass) {
			this.numArgs = numArgs;
			Function = function;
		}
		
		public abstract void setFunctionToDefault(out Delegate function);
		
		public virtual long NumArgs {
			get {return numArgs;}
			set {numArgs = value;}
		}
		
		public Delegate Function {
			get {return function;}
			set {function = value;
				if (function == null) setFunctionToDefault(out function); }
		}
		
		public abstract ESMethod HomeMethod {
			get;
		}
		
		public abstract bool HasHomeClass {
			get;
		}

		public abstract ESBehavior HomeClass {
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

			protected override void bindToKernel() {
				domainClass = kernel.CompiledCodeClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.CompiledCode;}
			}

			#region Primitive Definitions
		
			public Object _numArgs_(Object receiver) {
				return ((ESCompiledCode)receiver).NumArgs;
			}
		
			public Object _homeMethod_(Object receiver) {
				return ((ESCompiledCode)receiver).HomeMethod;
			}
		
			public Object _homeClass_(Object receiver) {
				return ((ESCompiledCode)receiver).HomeClass;
			}

			#endregion

			public override void publishCanonicalPrimitives() {

				publishPrimitive("numArgs",					new FuncNs.Func<Object, Object>(_numArgs_));
				publishPrimitive("homeMethod",					new FuncNs.Func<Object, Object>(_homeMethod_));
				publishPrimitive("homeClass",					new FuncNs.Func<Object, Object>(_homeClass_));

			}

		}
		
	}
	
	public class ESBlock : ESCompiledCode {
		
		protected ESCompiledCode 									lexicalContext 				= null;
		
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
		
		public ESCompiledCode LexicalContext {
			get {return lexicalContext;}
			set {lexicalContext = value;}
		}
		
		public override ESMethod HomeMethod {
			get {return lexicalContext == null ? null : lexicalContext.HomeMethod;}
		}
		
		public override ESBehavior HomeClass {
			get {return lexicalContext == null ? null : lexicalContext.HomeClass;}
		}
		
		public override bool HasHomeClass {
			get {return lexicalContext == null ? false : lexicalContext.HasHomeClass;}
		}
		
		public override ESBlock asBlock() {
			return this;
		}
		
		public bool asBoolean() {
			return asBoolean(value0());
		}
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			append("numArgs: ");
			append(NumArgs.ToString());
		}

		public override DynamicMetaObject GetMetaObject(Expression parameter) {
			return new ESBlockDynamicMetaObject(parameter, BindingRestrictions.Empty, this, Class);
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
				ESKernel.throwInvalidFunctionCallException("Block.value0()", NumArgs, 0, function.GetType(), blockFunctionTypeForNumArgs(0), invalidCastEx);
				functor = null;
			}
			return functor();
		}

		public Object value1(Object a1) {
			FuncNs.Func<Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value1()", NumArgs, 1, function.GetType(), blockFunctionTypeForNumArgs(1), invalidCastEx);
				functor = null;
			}
			return functor(a1);
		}

		public Object value2(Object a1, Object a2) {
			FuncNs.Func<Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value2()", NumArgs, 2, function.GetType(), blockFunctionTypeForNumArgs(2), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2);
		}

		public Object value3(Object a1, Object a2, Object a3) {
			FuncNs.Func<Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value3()", NumArgs, 3, function.GetType(), blockFunctionTypeForNumArgs(3), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3);
		}

		public Object value4(Object a1, Object a2, Object a3, Object a4) {
			FuncNs.Func<Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value4()", NumArgs, 4, function.GetType(), blockFunctionTypeForNumArgs(4), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4);
		}

		public Object value5(Object a1, Object a2, Object a3, Object a4, Object a5) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value5()", NumArgs, 5, function.GetType(), blockFunctionTypeForNumArgs(5), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5);
		}

		public Object value6(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value6()", NumArgs, 6, function.GetType(), blockFunctionTypeForNumArgs(6), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6);
		}

		public Object value7(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value7()", NumArgs, 7, function.GetType(), blockFunctionTypeForNumArgs(7), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7);
		}

		public Object value8(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value8()", NumArgs, 8, function.GetType(), blockFunctionTypeForNumArgs(8), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8);
		}

		public Object value9(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value9()", NumArgs, 9, function.GetType(), blockFunctionTypeForNumArgs(9), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9);
		}

		public Object value10(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value10()", NumArgs, 10, function.GetType(), blockFunctionTypeForNumArgs(10), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10);
		}

		public Object value11(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value11()", NumArgs, 11, function.GetType(), blockFunctionTypeForNumArgs(11), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11);
		}

		public Object value12(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value12()", NumArgs, 12, function.GetType(), blockFunctionTypeForNumArgs(12), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12);
		}

		public Object value13(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value13()", NumArgs, 13, function.GetType(), blockFunctionTypeForNumArgs(13), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13);
		}

		public Object value14(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value14()", NumArgs, 14, function.GetType(), blockFunctionTypeForNumArgs(14), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14);
		}

		public Object value15(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value15()", NumArgs, 15, function.GetType(), blockFunctionTypeForNumArgs(15), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15);
		}

		public Object value16(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value16()", NumArgs, 16, function.GetType(), blockFunctionTypeForNumArgs(16), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16);
		}

		public Object value17(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value17()", NumArgs, 17, function.GetType(), blockFunctionTypeForNumArgs(17), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17);
		}

		public Object value18(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value18()", NumArgs, 18, function.GetType(), blockFunctionTypeForNumArgs(18), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18);
		}

		public Object value19(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value19()", NumArgs, 19, function.GetType(), blockFunctionTypeForNumArgs(19), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19);
		}

		public Object value20(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value20()", NumArgs, 20, function.GetType(), blockFunctionTypeForNumArgs(20), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20);
		}

		public Object value21(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value21()", NumArgs, 21, function.GetType(), blockFunctionTypeForNumArgs(21), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21);
		}

		public Object value22(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value22()", NumArgs, 22, function.GetType(), blockFunctionTypeForNumArgs(22), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22);
		}

		public Object value23(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value23()", NumArgs, 23, function.GetType(), blockFunctionTypeForNumArgs(23), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23);
		}

		public Object value24(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value24()", NumArgs, 24, function.GetType(), blockFunctionTypeForNumArgs(24), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24);
		}

		public Object value25(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value25()", NumArgs, 25, function.GetType(), blockFunctionTypeForNumArgs(25), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25);
		}

		public Object value26(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value26()", NumArgs, 26, function.GetType(), blockFunctionTypeForNumArgs(26), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26);
		}

		public Object value27(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value27()", NumArgs, 27, function.GetType(), blockFunctionTypeForNumArgs(27), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27);
		}

		public Object value28(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value28()", NumArgs, 28, function.GetType(), blockFunctionTypeForNumArgs(28), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28);
		}

		public Object value29(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value29()", NumArgs, 29, function.GetType(), blockFunctionTypeForNumArgs(29), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29);
		}

		public Object value30(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value30()", NumArgs, 30, function.GetType(), blockFunctionTypeForNumArgs(30), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30);
		}

		public Object value31(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30, Object a31) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value31()", NumArgs, 31, function.GetType(), blockFunctionTypeForNumArgs(31), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31);
		}

		public Object value32(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30, Object a31, Object a32) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value32()", NumArgs, 32, function.GetType(), blockFunctionTypeForNumArgs(32), invalidCastEx);
				functor = null;
			}
			return functor(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32);
		}
		
		#endregion

		public new class Primitives : PrimitiveDomain {

			protected override void bindToKernel() {
				domainClass = kernel.BlockClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.Block;}
			}
		
			#region Primitive Definitions
		
			public static Object _lexicalContext_(Object receiver) {
				return ((ESBlock)receiver).LexicalContext;
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
				return ((ESBlock)receiver).value0();
			}

			public static Object _value1_(Object receiver, Object a1) {
				return ((ESBlock)receiver).value1(a1);
			}

			public static Object _value2_(Object receiver, Object a1, Object a2) {
				return ((ESBlock)receiver).value2(a1, a2);
			}

			public static Object _value3_(Object receiver, Object a1, Object a2, Object a3) {
				return ((ESBlock)receiver).value3(a1, a2, a3);
			}

			public static Object _value4_(Object receiver, Object a1, Object a2, Object a3, Object a4) {
				return ((ESBlock)receiver).value4(a1, a2, a3, a4);
			}

			public static Object _value5_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5) {
				return ((ESBlock)receiver).value5(a1, a2, a3, a4, a5);
			}

			public static Object _value6_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6) {
				return ((ESBlock)receiver).value6(a1, a2, a3, a4, a5, a6);
			}

			public static Object _value7_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7) {
				return ((ESBlock)receiver).value7(a1, a2, a3, a4, a5, a6, a7);
			}

			public static Object _value8_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8) {
				return ((ESBlock)receiver).value8(a1, a2, a3, a4, a5, a6, a7, a8);
			}

			public static Object _value9_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9) {
				return ((ESBlock)receiver).value9(a1, a2, a3, a4, a5, a6, a7, a8, a9);
			}

			public static Object _value10_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10) {
				return ((ESBlock)receiver).value10(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10);
			}

			public static Object _value11_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11) {
				return ((ESBlock)receiver).value11(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11);
			}

			public static Object _value12_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12) {
				return ((ESBlock)receiver).value12(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12);
			}

			public static Object _value13_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13) {
				return ((ESBlock)receiver).value13(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13);
			}

			public static Object _value14_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14) {
				return ((ESBlock)receiver).value14(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14);
			}

			public static Object _value15_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15) {
				return ((ESBlock)receiver).value15(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15);
			}

			public static Object _value16_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16) {
				return ((ESBlock)receiver).value16(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16);
			}

			public static Object _value17_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17) {
				return ((ESBlock)receiver).value17(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17);
			}

			public static Object _value18_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18) {
				return ((ESBlock)receiver).value18(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18);
			}

			public static Object _value19_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19) {
				return ((ESBlock)receiver).value19(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19);
			}

			public static Object _value20_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20) {
				return ((ESBlock)receiver).value20(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20);
			}

			public static Object _value21_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21) {
				return ((ESBlock)receiver).value21(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21);
			}

			public static Object _value22_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22) {
				return ((ESBlock)receiver).value22(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22);
			}

			public static Object _value23_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23) {
				return ((ESBlock)receiver).value23(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23);
			}

			public static Object _value24_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24) {
				return ((ESBlock)receiver).value24(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24);
			}

			public static Object _value25_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25) {
				return ((ESBlock)receiver).value25(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25);
			}

			public static Object _value26_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26) {
				return ((ESBlock)receiver).value26(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26);
			}

			public static Object _value27_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27) {
				return ((ESBlock)receiver).value27(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27);
			}

			public static Object _value28_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28) {
				return ((ESBlock)receiver).value28(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28);
			}

			public static Object _value29_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29) {
				return ((ESBlock)receiver).value29(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29);
			}

			public static Object _value30_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30) {
				return ((ESBlock)receiver).value30(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30);
			}

			public static Object _value31_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30, Object a31) {
				return ((ESBlock)receiver).value31(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31);
			}

			public static Object _value32_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30, Object a31, Object a32) {
				return ((ESBlock)receiver).value32(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32);
			}

			#endregion

			public override void publishCanonicalPrimitives() {

				publishPrimitive("lexicalContext",				new FuncNs.Func<Object, Object>(_lexicalContext_));

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
	
	public enum MethodOperationType {
		Function,
		Convert,
		GetField,
		SetField,
		InvokeField,
		GetProperty,
		SetProperty,
		InvokeProperty,
		InvokeMethod,
		CreateInstance
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

	public class ESMethod : ESCompiledCode {
		
		protected ESSymbol										selector				= null;
		protected InlineOperation									inlineOperation;
		protected ESBehavior 										lexicalContext 				= null;
		protected System.Collections.Generic.HashSet<ESSymbol>						protocols				= null;
		protected MethodDeclarationNode									methodDeclarationNode			= null;
		
		internal ESMethod(ESBehavior esClass) : base(esClass) {
		}
		
		public ESMethod(ESBehavior esClass, ESSymbol selector, Delegate function) : this(esClass, selector, function, null, null) {
		}
		
		public ESMethod(ESBehavior esClass, ESSymbol selector, Delegate function, ESBehavior lexicalContext) : this(esClass, selector, function, lexicalContext, null) {
		}
		
		public ESMethod(ESBehavior esClass, ESSymbol selector, Delegate function, ESSymbol protocol) : this(esClass, selector, function, null, protocol) {
		}
		
		public ESMethod(ESBehavior esClass, ESSymbol selector, Delegate function, ESBehavior lexicalContext, ESSymbol protocol) : this(esClass, selector, null, function, lexicalContext, protocol) {
		}
		
		public ESMethod(ESBehavior esClass, ESSymbol selector, InlineOperation inlineOperation, Delegate function) : this(esClass, selector, inlineOperation, function, null, null) {
		}
		
		public ESMethod(ESBehavior esClass, ESSymbol selector, InlineOperation inlineOperation, Delegate function, ESBehavior lexicalContext) : this(esClass, selector, inlineOperation, function, lexicalContext, null) {
		}

		public ESMethod(ESBehavior esClass, ESSymbol selector, InlineOperation inlineOperation, Delegate function, ESSymbol protocol) : this(esClass, selector, inlineOperation, function, null, protocol) {
		}
		
		public ESMethod(ESBehavior esClass, ESSymbol selector, InlineOperation inlineOperation, Delegate function, ESBehavior lexicalContext, ESSymbol protocol) : base(esClass) {
			Selector		= selector;
			this.inlineOperation	= inlineOperation;
			Function		= function;
			LexicalContext		= lexicalContext;
			addToProtocol(protocol);
		}
		
		public ESMethod(ESBehavior esClass, MethodDeclarationNode methodDeclarationNode) : this(esClass, methodDeclarationNode, null) {
		}
		
		public ESMethod(ESBehavior esClass, MethodDeclarationNode methodDeclarationNode, ESSymbol protocol) 
			: this(esClass, 
				methodDeclarationNode.Selector,
				methodDeclarationNode.InlineOperation,
				methodDeclarationNode.Function,
				methodDeclarationNode.HomeClass,
				protocol) {
			this.methodDeclarationNode = methodDeclarationNode;
		}

		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.Method;}
		}
		
		public MethodDeclarationNode MethodDeclarationNode {
			get {return methodDeclarationNode;}
		}

		public override bool IsMethod {
			get {return true;}
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

		public ESBehavior LexicalContext {
			get {return lexicalContext;}
			set {	bool isNewContext = lexicalContext != value;
				lexicalContext = value;
				if (isNewContext) recompile();}
		}
		
		public override long NumArgs {
			get {return numArgs;}
			set {}
		}
		
		public ESSymbol Selector {
			get {return selector;}
			set {	selector = value;
				numArgs = selector == null ? 0 : selector.NumArgs;}
		}
		
		public long ProtocolCount {
			get {return protocols == null ? 0 : protocols.Count;}
		}

		public System.Collections.Generic.HashSet<ESSymbol> Protocols {
 			get {return protocols;}
		}
		
		public override ESBehavior HomeClass {
			get {return lexicalContext;}
		}

		public override ESMethod HomeMethod {
			get {return this;}
		}
		
		public override bool HasHomeClass {
			get {return lexicalContext != null;}
		}
		
		public void recompile() {
			if (methodDeclarationNode == null) return;
			methodDeclarationNode.HomeClass = HomeClass;
			Function = methodDeclarationNode.Function;
		}

		internal void become(ESMethod other) {
			methodDeclarationNode = other.MethodDeclarationNode;
			inlineOperation = other.InlineOperation;
			Function = other.Function;
			if (other.ProtocolCount > 0) foreach (var protocol in other.Protocols) addToProtocol(protocol);
		}
		
		public override ESMethod asESMethod() {
			return this;
		}
		
		public ESMethod newCopyIn(ESBehavior newLexicalContext) {
			var newCopy = (ESMethod)copy();
			newCopy.LexicalContext = newLexicalContext;
			return newCopy;
		}

		public void protocolsDo(FuncNs.Func<Object, Object> enumerator1) {
			if (protocols == null) return;
			foreach (var protocol in protocols) enumerator1(protocol);
		}

		public void addToProtocol(ESSymbol protocol) {
			if (protocol == null) return;
			if (protocols == null) protocols = new System.Collections.Generic.HashSet<ESSymbol>();
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

		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			if (HomeClass == null) {
				append(" ??");
			} else {
				append(HomeClass.Name.PrimitiveValue);
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
				ESKernel.throwInvalidFunctionCallException("Block.value0()", NumArgs, 0, function.GetType(), blockFunctionTypeForNumArgs(0), invalidCastEx);
				functor = null;
			}
			return functor(self);
		}

		public Object value1(Object self, Object a1) {
			FuncNs.Func<Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value1()", NumArgs, 1, function.GetType(), blockFunctionTypeForNumArgs(1), invalidCastEx);
				functor = null;
			}
			return functor(self, a1);
		}

		public Object value2(Object self, Object a1, Object a2) {
			FuncNs.Func<Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value2()", NumArgs, 2, function.GetType(), blockFunctionTypeForNumArgs(2), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2);
		}

		public Object value3(Object self, Object a1, Object a2, Object a3) {
			FuncNs.Func<Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value3()", NumArgs, 3, function.GetType(), blockFunctionTypeForNumArgs(3), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3);
		}

		public Object value4(Object self, Object a1, Object a2, Object a3, Object a4) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value4()", NumArgs, 4, function.GetType(), blockFunctionTypeForNumArgs(4), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4);
		}

		public Object value5(Object self, Object a1, Object a2, Object a3, Object a4, Object a5) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value5()", NumArgs, 5, function.GetType(), blockFunctionTypeForNumArgs(5), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5);
		}

		public Object value6(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value6()", NumArgs, 6, function.GetType(), blockFunctionTypeForNumArgs(6), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6);
		}

		public Object value7(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value7()", NumArgs, 7, function.GetType(), blockFunctionTypeForNumArgs(7), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7);
		}

		public Object value8(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value8()", NumArgs, 8, function.GetType(), blockFunctionTypeForNumArgs(8), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8);
		}

		public Object value9(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value9()", NumArgs, 9, function.GetType(), blockFunctionTypeForNumArgs(9), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9);
		}

		public Object value10(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value10()", NumArgs, 10, function.GetType(), blockFunctionTypeForNumArgs(10), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10);
		}

		public Object value11(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value11()", NumArgs, 11, function.GetType(), blockFunctionTypeForNumArgs(11), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11);
		}

		public Object value12(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value12()", NumArgs, 12, function.GetType(), blockFunctionTypeForNumArgs(12), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12);
		}

		public Object value13(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value13()", NumArgs, 13, function.GetType(), blockFunctionTypeForNumArgs(13), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13);
		}

		public Object value14(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value14()", NumArgs, 14, function.GetType(), blockFunctionTypeForNumArgs(14), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14);
		}

		public Object value15(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value15()", NumArgs, 15, function.GetType(), blockFunctionTypeForNumArgs(15), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15);
		}

		public Object value16(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value16()", NumArgs, 16, function.GetType(), blockFunctionTypeForNumArgs(16), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16);
		}

		public Object value17(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value17()", NumArgs, 17, function.GetType(), blockFunctionTypeForNumArgs(17), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17);
		}

		public Object value18(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value18()", NumArgs, 18, function.GetType(), blockFunctionTypeForNumArgs(18), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18);
		}

		public Object value19(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value19()", NumArgs, 19, function.GetType(), blockFunctionTypeForNumArgs(19), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19);
		}

		public Object value20(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value20()", NumArgs, 20, function.GetType(), blockFunctionTypeForNumArgs(20), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20);
		}

		public Object value21(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value21()", NumArgs, 21, function.GetType(), blockFunctionTypeForNumArgs(21), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21);
		}

		public Object value22(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value22()", NumArgs, 22, function.GetType(), blockFunctionTypeForNumArgs(22), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22);
		}

		public Object value23(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value23()", NumArgs, 23, function.GetType(), blockFunctionTypeForNumArgs(23), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23);
		}

		public Object value24(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value24()", NumArgs, 24, function.GetType(), blockFunctionTypeForNumArgs(24), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24);
		}

		public Object value25(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value25()", NumArgs, 25, function.GetType(), blockFunctionTypeForNumArgs(25), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25);
		}

		public Object value26(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value26()", NumArgs, 26, function.GetType(), blockFunctionTypeForNumArgs(26), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26);
		}

		public Object value27(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value27()", NumArgs, 27, function.GetType(), blockFunctionTypeForNumArgs(27), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27);
		}

		public Object value28(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value28()", NumArgs, 28, function.GetType(), blockFunctionTypeForNumArgs(28), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28);
		}

		public Object value29(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value29()", NumArgs, 29, function.GetType(), blockFunctionTypeForNumArgs(29), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29);
		}

		public Object value30(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value30()", NumArgs, 30, function.GetType(), blockFunctionTypeForNumArgs(30), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30);
		}

		public Object value31(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30, Object a31) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value31()", NumArgs, 31, function.GetType(), blockFunctionTypeForNumArgs(31), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31);
		}

		public Object value32(Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30, Object a31, Object a32) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESKernel.throwInvalidFunctionCallException("Block.value32()", NumArgs, 32, function.GetType(), blockFunctionTypeForNumArgs(32), invalidCastEx);
				functor = null;
			}
			return functor(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32);
		}

		#endregion

		public new class Primitives : PrimitiveDomain {

			protected override void bindToKernel() {
				domainClass = kernel.MethodClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.Method;}
			}
		
			#region Primitive Definitions
		
			public Object _selector_(Object receiver) {
				return ((ESMethod)receiver).Selector;
			}
		
			public Object _protocolCount_(Object receiver) {
				return ((ESMethod)receiver).ProtocolCount;
			}

			public Object _protocolsDo_(Object receiver, Object enumerator1) {
				((ESMethod)receiver).protocolsDo(asFunctor1(enumerator1));
				return receiver;
			}

			public Object _addToProtocol_(Object receiver, Object protocol) {
				((ESMethod)receiver).addToProtocol(kernel.asESSymbol(protocol));
				return receiver;
			}

			public Object _removeFromProtocol_(Object receiver, Object protocol) {
				((ESMethod)receiver).removeFromProtocol(kernel.asESSymbol(protocol));
				return receiver;
			}

			public Object _removeFromAllProtocols_(Object receiver) {
				((ESMethod)receiver).removeFromAllProtocols();
				return receiver;
			}

			public Object _valueWithReceiverWithArguments_(Object receiver, Object self, Object arguments) {
				ESObject esObject = arguments as ESObject;
				Object[] argArray = esObject == null ? (Object[])arguments : esObject.asHostArray<Object>();
				return ((ESMethod)receiver).valueWithReceiverWithArguments(self, argArray);
			}

			public Object _valueWithReceiver_(Object receiver, Object self) {
				return ((ESMethod)receiver).value0(self);
			}

			public Object _valueWithReceiver1_(Object receiver, Object self, Object a1) {
				return ((ESMethod)receiver).value1(self, a1);
			}

			public Object _valueWithReceiver2_(Object receiver, Object self, Object a1, Object a2) {
				return ((ESMethod)receiver).value2(self, a1, a2);
			}

			public Object _valueWithReceiver3_(Object receiver, Object self, Object a1, Object a2, Object a3) {
				return ((ESMethod)receiver).value3(self, a1, a2, a3);
			}

			public Object _valueWithReceiver4_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4) {
				return ((ESMethod)receiver).value4(self, a1, a2, a3, a4);
			}

			public Object _valueWithReceiver5_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5) {
				return ((ESMethod)receiver).value5(self, a1, a2, a3, a4, a5);
			}

			public Object _valueWithReceiver6_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6) {
				return ((ESMethod)receiver).value6(self, a1, a2, a3, a4, a5, a6);
			}

			public Object _valueWithReceiver7_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7) {
				return ((ESMethod)receiver).value7(self, a1, a2, a3, a4, a5, a6, a7);
			}

			public Object _valueWithReceiver8_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8) {
				return ((ESMethod)receiver).value8(self, a1, a2, a3, a4, a5, a6, a7, a8);
			}

			public Object _valueWithReceiver9_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9) {
				return ((ESMethod)receiver).value9(self, a1, a2, a3, a4, a5, a6, a7, a8, a9);
			}

			public Object _valueWithReceiver10_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10) {
				return ((ESMethod)receiver).value10(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10);
			}

			public Object _valueWithReceiver11_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11) {
				return ((ESMethod)receiver).value11(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11);
			}

			public Object _valueWithReceiver12_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12) {
				return ((ESMethod)receiver).value12(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12);
			}

			public Object _valueWithReceiver13_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13) {
				return ((ESMethod)receiver).value13(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13);
			}

			public Object _valueWithReceiver14_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14) {
				return ((ESMethod)receiver).value14(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14);
			}

			public Object _valueWithReceiver15_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15) {
				return ((ESMethod)receiver).value15(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15);
			}

			public Object _valueWithReceiver16_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16) {
				return ((ESMethod)receiver).value16(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16);
			}

			public Object _valueWithReceiver17_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17) {
				return ((ESMethod)receiver).value17(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17);
			}

			public Object _valueWithReceiver18_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18) {
				return ((ESMethod)receiver).value18(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18);
			}

			public Object _valueWithReceiver19_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19) {
				return ((ESMethod)receiver).value19(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19);
			}

			public Object _valueWithReceiver20_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20) {
				return ((ESMethod)receiver).value20(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20);
			}

			public Object _valueWithReceiver21_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21) {
				return ((ESMethod)receiver).value21(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21);
			}

			public Object _valueWithReceiver22_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22) {
				return ((ESMethod)receiver).value22(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22);
			}

			public Object _valueWithReceiver23_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23) {
				return ((ESMethod)receiver).value23(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23);
			}

			public Object _valueWithReceiver24_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24) {
				return ((ESMethod)receiver).value24(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24);
			}

			public Object _valueWithReceiver25_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25) {
				return ((ESMethod)receiver).value25(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25);
			}

			public Object _valueWithReceiver26_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26) {
				return ((ESMethod)receiver).value26(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26);
			}

			public Object _valueWithReceiver27_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27) {
				return ((ESMethod)receiver).value27(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27);
			}

			public Object _valueWithReceiver28_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28) {
				return ((ESMethod)receiver).value28(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28);
			}

			public Object _valueWithReceiver29_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29) {
				return ((ESMethod)receiver).value29(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29);
			}

			public Object _valueWithReceiver30_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30) {
				return ((ESMethod)receiver).value30(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30);
			}

			public Object _valueWithReceiver31_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30, Object a31) {
				return ((ESMethod)receiver).value31(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31);
			}

			public Object _valueWithReceiver32_(Object receiver, Object self, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30, Object a31, Object a32) {
				return ((ESMethod)receiver).value32(self, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32);
			}

			#endregion

			public override void publishCanonicalPrimitives() {

				publishPrimitive("selector",					new FuncNs.Func<Object, Object>(_selector_));

				publishPrimitive("protocolCount",				new FuncNs.Func<Object, Object>(_protocolCount_));
				publishPrimitive("protocolsDo:",				new FuncNs.Func<Object, Object, Object>(_protocolsDo_));
				publishPrimitive("addToProtocol:",				new FuncNs.Func<Object, Object, Object>(_addToProtocol_));
				publishPrimitive("removeFromProtocol:",				new FuncNs.Func<Object, Object, Object>(_removeFromProtocol_));
				publishPrimitive("removeFromAllProtocols",			new FuncNs.Func<Object, Object>(_removeFromAllProtocols_));

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
