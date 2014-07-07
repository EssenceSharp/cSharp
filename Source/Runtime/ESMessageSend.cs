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
#if CLR2
using FuncNs = Microsoft.Scripting.Utils;
#else
using FuncNs = System;
#endif
using EssenceSharp.Exceptions.System;
using EssenceSharp.Exceptions.System.PrimitiveFailures;
using EssenceSharp.UtilityServices;
#endregion

namespace EssenceSharp.Runtime {

	public class ESMessageSend : ESMessage {
		
		protected static readonly	Func<Object, Object>	defaultFunction	= (Object receiver) => {return receiver;}; 

		protected			Delegate		function;
		protected			Object			receiver;

		public ESMessageSend(ESBehavior esClass) : base(esClass) {
		}
		
		public ESMessageSend(ESBehavior esClass, ESMethod method) : this(esClass, method, null) {
		}
		
		public ESMessageSend(ESBehavior esClass, ESMethod method, Object[] arguments) : this(esClass, null, method, arguments) {
		}
		
		public ESMessageSend(ESBehavior esClass, Object receiver, ESMethod method) : this(esClass, receiver, method, null) {
		}
		
		public ESMessageSend(ESBehavior esClass, Object receiver, ESMethod method, Object[] arguments) : base(esClass, null, arguments) {
			this.receiver = receiver;
			setMethod(method);
		}
		
		public ESMessageSend(ESBehavior esClass, ESSymbol selector) : base(esClass, selector, null) {
		}
		
		public ESMessageSend(ESBehavior esClass, ESSymbol selector, Object[] arguments) : base(esClass, selector, arguments) {
		}

		public ESMessageSend(ESBehavior esClass, Object receiver, ESSymbol selector) : this(esClass, receiver, selector, null) {
		}

		public ESMessageSend(ESBehavior esClass, Object receiver, ESSymbol selector, Object[] arguments) : base(esClass, selector, arguments) {
			this.receiver = receiver;
		}

		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.MessageSend;}
		}

		public Object Receiver {
			get {return receiver;}
			set {receiver = value;}
		}

		public override void setSelector(ESSymbol newSelector) {
			if (selector == newSelector) return;
			selector = newSelector;
			var os = Class.ObjectSpace;
			ESBlock block;
			if (selector == null) {
				numArgs = 0;
				function = defaultFunction;
			} else { 
				numArgs = selector.NumArgs;
				block = os.newBlockToSend(selector);
				function = block.Function;
			}
		}

		public void setMethod(ESMethod method) {
			if (method == null) return;
			numArgs = method.NumArgs;
			function = method.Function;
		}

		public override Object sendTo(Object receiver) {
			var providedArgCount = arguments == null ? 0 : arguments.Length;
			if (providedArgCount < numArgs) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.sendTo()", NumArgs, 0, ESCompiledCode.methodFunctionTypeForNumArgs(numArgs), ESCompiledCode.methodFunctionTypeForNumArgs(providedArgCount), null);
			}
 			switch (numArgs) {
				case 0:
					return ((FuncNs.Func<Object, Object>)function)(receiver);
				case 1:
					return ((FuncNs.Func<Object, Object, Object>)function)(receiver, arguments[0]);
				case 2:
					return ((FuncNs.Func<Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1]);
				case 3:
					return ((FuncNs.Func<Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2]);
				case 4:
					return ((FuncNs.Func<Object, Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2], arguments[3]);
				case 5:
					return ((FuncNs.Func<Object, Object, Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2], arguments[3], arguments[4]);
				case 6:
					return ((FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2], arguments[3], arguments[4], arguments[5]);
				case 7:
					return ((FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2], arguments[3], arguments[4], arguments[5], arguments[6]);
				case 8:
					return ((FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2], arguments[3], arguments[4], arguments[5], arguments[6], arguments[7]);
				case 9:
					return ((FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2], arguments[3], arguments[4], arguments[5], arguments[6], arguments[7], arguments[8]);
				case 10:
					return ((FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2], arguments[3], arguments[4], arguments[5], arguments[6], arguments[7], arguments[8], arguments[9]);
				case 11:
					return ((FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2], arguments[3], arguments[4], arguments[5], arguments[6], arguments[7], arguments[8], arguments[9], arguments[10]);
				case 12:
					return ((FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2], arguments[3], arguments[4], arguments[5], arguments[6], arguments[7], arguments[8], arguments[9], arguments[10], arguments[11]);
				case 13:
					return ((FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2], arguments[3], arguments[4], arguments[5], arguments[6], arguments[7], arguments[8], arguments[9], arguments[10], arguments[11], arguments[12]);
				case 14:
					return ((FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2], arguments[3], arguments[4], arguments[5], arguments[6], arguments[7], arguments[8], arguments[9], arguments[10], arguments[11], arguments[12], arguments[13]);
				case 15:
					return ((FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2], arguments[3], arguments[4], arguments[5], arguments[6], arguments[7], arguments[8], arguments[9], arguments[10], arguments[11], arguments[12], arguments[13], arguments[14]);
				case 16:
					return ((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2], arguments[3], arguments[4], arguments[5], arguments[6], arguments[7], arguments[8], arguments[9], arguments[10], arguments[11], arguments[12], arguments[13], arguments[14], arguments[15]);
				case 17:
					return ((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2], arguments[3], arguments[4], arguments[5], arguments[6], arguments[7], arguments[8], arguments[9], arguments[10], arguments[11], arguments[12], arguments[13], arguments[14], arguments[15], arguments[16]);
				case 18:
					return ((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2], arguments[3], arguments[4], arguments[5], arguments[6], arguments[7], arguments[8], arguments[9], arguments[10], arguments[11], arguments[12], arguments[13], arguments[14], arguments[15], arguments[16], arguments[17]);
				case 19:
					return ((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2], arguments[3], arguments[4], arguments[5], arguments[6], arguments[7], arguments[8], arguments[9], arguments[10], arguments[11], arguments[12], arguments[13], arguments[14], arguments[15], arguments[16], arguments[17], arguments[18]);
				case 20:
					return ((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2], arguments[3], arguments[4], arguments[5], arguments[6], arguments[7], arguments[8], arguments[9], arguments[10], arguments[11], arguments[12], arguments[13], arguments[14], arguments[15], arguments[16], arguments[17], arguments[18], arguments[19]);
				case 21:
					return ((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2], arguments[3], arguments[4], arguments[5], arguments[6], arguments[7], arguments[8], arguments[9], arguments[10], arguments[11], arguments[12], arguments[13], arguments[14], arguments[15], arguments[16], arguments[17], arguments[18], arguments[19], arguments[20]);
				case 22:
					return ((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2], arguments[3], arguments[4], arguments[5], arguments[6], arguments[7], arguments[8], arguments[9], arguments[10], arguments[11], arguments[12], arguments[13], arguments[14], arguments[15], arguments[16], arguments[17], arguments[18], arguments[19], arguments[20], arguments[21]);
				case 23:
					return ((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2], arguments[3], arguments[4], arguments[5], arguments[6], arguments[7], arguments[8], arguments[9], arguments[10], arguments[11], arguments[12], arguments[13], arguments[14], arguments[15], arguments[16], arguments[17], arguments[18], arguments[19], arguments[20], arguments[21], arguments[22]);
				case 24:
					return ((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2], arguments[3], arguments[4], arguments[5], arguments[6], arguments[7], arguments[8], arguments[9], arguments[10], arguments[11], arguments[12], arguments[13], arguments[14], arguments[15], arguments[16], arguments[17], arguments[18], arguments[19], arguments[20], arguments[21], arguments[22], arguments[23]);
				case 25:
					return ((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2], arguments[3], arguments[4], arguments[5], arguments[6], arguments[7], arguments[8], arguments[9], arguments[10], arguments[11], arguments[12], arguments[13], arguments[14], arguments[15], arguments[16], arguments[17], arguments[18], arguments[19], arguments[20], arguments[21], arguments[22], arguments[23], arguments[24]);
				case 26:
					return ((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2], arguments[3], arguments[4], arguments[5], arguments[6], arguments[7], arguments[8], arguments[9], arguments[10], arguments[11], arguments[12], arguments[13], arguments[14], arguments[15], arguments[16], arguments[17], arguments[18], arguments[19], arguments[20], arguments[21], arguments[22], arguments[23], arguments[24], arguments[25]);
				case 27:
					return ((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2], arguments[3], arguments[4], arguments[5], arguments[6], arguments[7], arguments[8], arguments[9], arguments[10], arguments[11], arguments[12], arguments[13], arguments[14], arguments[15], arguments[16], arguments[17], arguments[18], arguments[19], arguments[20], arguments[21], arguments[22], arguments[23], arguments[24], arguments[25], arguments[26]);
				case 28:
					return ((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2], arguments[3], arguments[4], arguments[5], arguments[6], arguments[7], arguments[8], arguments[9], arguments[10], arguments[11], arguments[12], arguments[13], arguments[14], arguments[15], arguments[16], arguments[17], arguments[18], arguments[19], arguments[20], arguments[21], arguments[22], arguments[23], arguments[24], arguments[25], arguments[26], arguments[27]);
				case 29:
					return ((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2], arguments[3], arguments[4], arguments[5], arguments[6], arguments[7], arguments[8], arguments[9], arguments[10], arguments[11], arguments[12], arguments[13], arguments[14], arguments[15], arguments[16], arguments[17], arguments[18], arguments[19], arguments[20], arguments[21], arguments[22], arguments[23], arguments[24], arguments[25], arguments[26], arguments[27], arguments[28]);
				case 30:
					return ((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2], arguments[3], arguments[4], arguments[5], arguments[6], arguments[7], arguments[8], arguments[9], arguments[10], arguments[11], arguments[12], arguments[13], arguments[14], arguments[15], arguments[16], arguments[17], arguments[18], arguments[19], arguments[20], arguments[21], arguments[22], arguments[23], arguments[24], arguments[25], arguments[26], arguments[27], arguments[28], arguments[29]);
				case 31:
					return ((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2], arguments[3], arguments[4], arguments[5], arguments[6], arguments[7], arguments[8], arguments[9], arguments[10], arguments[11], arguments[12], arguments[13], arguments[14], arguments[15], arguments[16], arguments[17], arguments[18], arguments[19], arguments[20], arguments[21], arguments[22], arguments[23], arguments[24], arguments[25], arguments[26], arguments[27], arguments[28], arguments[29], arguments[30]);
				case 32:
					return ((Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function)(receiver, arguments[0], arguments[1], arguments[2], arguments[3], arguments[4], arguments[5], arguments[6], arguments[7], arguments[8], arguments[9], arguments[10], arguments[11], arguments[12], arguments[13], arguments[14], arguments[15], arguments[16], arguments[17], arguments[18], arguments[19], arguments[20], arguments[21], arguments[22], arguments[23], arguments[24], arguments[25], arguments[26], arguments[27], arguments[28], arguments[29], arguments[30], arguments[31]);
				default:
					throw new PrimitiveFailException("Too many arguments");
			}

		}

		public Object value() {
			if (numArgs == 0 || arguments == null) return value0();
			return valueWithArguments(arguments);
		}

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
			FuncNs.Func<Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value0()", NumArgs, 0, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(0), invalidCastEx);
				functor = null;
			}
			return functor(receiver);
		}

		public Object value1(Object a1) {
			FuncNs.Func<Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value1()", NumArgs, 1, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(1), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1);
		}

		public Object value2(Object a1, Object a2) {
			FuncNs.Func<Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value2()", NumArgs, 2, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(2), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2);
		}

		public Object value3(Object a1, Object a2, Object a3) {
			FuncNs.Func<Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value3()", NumArgs, 3, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(3), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3);
		}

		public Object value4(Object a1, Object a2, Object a3, Object a4) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value4()", NumArgs, 4, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(4), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3, a4);
		}

		public Object value5(Object a1, Object a2, Object a3, Object a4, Object a5) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value5()", NumArgs, 5, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(5), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3, a4, a5);
		}

		public Object value6(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value6()", NumArgs, 6, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(6), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3, a4, a5, a6);
		}

		public Object value7(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value7()", NumArgs, 7, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(7), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3, a4, a5, a6, a7);
		}

		public Object value8(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value8()", NumArgs, 8, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(8), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3, a4, a5, a6, a7, a8);
		}

		public Object value9(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value9()", NumArgs, 9, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(9), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9);
		}

		public Object value10(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value10()", NumArgs, 10, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(10), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10);
		}

		public Object value11(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value11()", NumArgs, 11, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(11), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11);
		}

		public Object value12(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value12()", NumArgs, 12, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(12), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12);
		}

		public Object value13(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value13()", NumArgs, 13, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(13), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13);
		}

		public Object value14(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value14()", NumArgs, 14, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(14), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14);
		}

		public Object value15(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15) {
			FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value15()", NumArgs, 15, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(15), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15);
		}

		public Object value16(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value16()", NumArgs, 16, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(16), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16);
		}

		public Object value17(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value17()", NumArgs, 17, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(17), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17);
		}

		public Object value18(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value18()", NumArgs, 18, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(18), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18);
		}

		public Object value19(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value19()", NumArgs, 19, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(19), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19);
		}

		public Object value20(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value20()", NumArgs, 20, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(20), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20);
		}

		public Object value21(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value21()", NumArgs, 21, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(21), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21);
		}

		public Object value22(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value22()", NumArgs, 22, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(22), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22);
		}

		public Object value23(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value23()", NumArgs, 23, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(23), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23);
		}

		public Object value24(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value24()", NumArgs, 24, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(24), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24);
		}

		public Object value25(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value25()", NumArgs, 25, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(25), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25);
		}

		public Object value26(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value26()", NumArgs, 26, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(26), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26);
		}

		public Object value27(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value27()", NumArgs, 27, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(27), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27);
		}

		public Object value28(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value28()", NumArgs, 28, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(28), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28);
		}

		public Object value29(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value29()", NumArgs, 29, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(29), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29);
		}

		public Object value30(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value30()", NumArgs, 30, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(30), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30);
		}

		public Object value31(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30, Object a31) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value31()", NumArgs, 31, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(31), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31);
		}

		public Object value32(Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30, Object a31, Object a32) {
			Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
			try {
				functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)function;
			} catch (InvalidCastException invalidCastEx) {
				ESObjectSpace.throwInvalidFunctionCallException("MessageSend.value32()", NumArgs, 32, function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(32), invalidCastEx);
				functor = null;
			}
			return functor(receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32);
		}

		#endregion

		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			newLine(depth);
			append("receiver: ");
			if (receiver == null) {
				append("nil");
			} else {
				var esObject = receiver as ESObject;
				if (esObject == null) {
					append(receiver.ToString());
				} else { 
					esObject.printUsing(depth, append, newLine);
				}
			}
			base.printElementsUsing(depth, append, newLine);
		}

		public override T valueBy<T>(Operation<T> operation) {
		    return operation.applyToMessageSend(this);
		}

		public new class Primitives : PrimitiveDomain {

			protected override void bindToObjectSpace() {
				domainClass = objectSpace.MessageSendClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.MessageSend;}
			}
		
			#region Primitive Definitions

			public static Object _receiver_(Object receiver) {
				return ((ESMessageSend)receiver).Receiver;
			}

			public static Object _setReceiver_(Object receiver, Object newMessageSendReceiver) {
				((ESMessageSend)receiver).Receiver = newMessageSendReceiver;
				return receiver;
			}

			public static Object _setMethod_(Object receiver, Object compiledMethod) {
				((ESMessageSend)receiver).setMethod((ESMethod)compiledMethod);
				return receiver;
			}

			public static Object _valueWithArguments_(Object receiver, Object arguments) {
				ESObject esObject = arguments as ESObject;
				Object[] argArray = esObject == null ? (Object[])arguments : esObject.asHostArray<Object>();
				return ((ESMessageSend)receiver).valueWithArguments(argArray);
			}

			public static Object _value_(Object receiver) {
				return ((ESMessageSend)receiver).value();
			}

			public static Object _value1_(Object receiver, Object a1) {
				FuncNs.Func<Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value1()", ((ESMessageSend)receiver).NumArgs, 1, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(1), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1);
			}

			public static Object _value2_(Object receiver, Object a1, Object a2) {
				FuncNs.Func<Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value2()", ((ESMessageSend)receiver).NumArgs, 2, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(2), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2);
			}

			public static Object _value3_(Object receiver, Object a1, Object a2, Object a3) {
				FuncNs.Func<Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value3()", ((ESMessageSend)receiver).NumArgs, 3, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(3), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3);
			}

			public static Object _value4_(Object receiver, Object a1, Object a2, Object a3, Object a4) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value4()", ((ESMessageSend)receiver).NumArgs, 4, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(4), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3, a4);
			}

			public static Object _value5_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value5()", ((ESMessageSend)receiver).NumArgs, 5, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(5), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3, a4, a5);
			}

			public static Object _value6_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value6()", ((ESMessageSend)receiver).NumArgs, 6, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(6), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3, a4, a5, a6);
			}

			public static Object _value7_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value7()", ((ESMessageSend)receiver).NumArgs, 7, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(7), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3, a4, a5, a6, a7);
			}

			public static Object _value8_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value8()", ((ESMessageSend)receiver).NumArgs, 8, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(8), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3, a4, a5, a6, a7, a8);
			}

			public static Object _value9_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value9()", ((ESMessageSend)receiver).NumArgs, 9, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(9), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9);
			}

			public static Object _value10_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value10()", ((ESMessageSend)receiver).NumArgs, 10, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(10), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10);
			}

			public static Object _value11_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value11()", ((ESMessageSend)receiver).NumArgs, 11, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(11), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11);
			}

			public static Object _value12_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value12()", ((ESMessageSend)receiver).NumArgs, 12, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(12), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12);
			}

			public static Object _value13_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value13()", ((ESMessageSend)receiver).NumArgs, 13, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(13), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13);
			}

			public static Object _value14_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value14()", ((ESMessageSend)receiver).NumArgs, 14, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(14), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14);
			}

			public static Object _value15_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15) {
				FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (FuncNs.Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value15()", ((ESMessageSend)receiver).NumArgs, 15, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(15), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15);
			}

			public static Object _value16_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value16()", ((ESMessageSend)receiver).NumArgs, 16, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(16), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16);
			}

			public static Object _value17_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value17()", ((ESMessageSend)receiver).NumArgs, 17, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(17), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17);
			}

			public static Object _value18_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value18()", ((ESMessageSend)receiver).NumArgs, 18, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(18), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18);
			}

			public static Object _value19_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value19()", ((ESMessageSend)receiver).NumArgs, 19, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(19), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19);
			}

			public static Object _value20_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value20()", ((ESMessageSend)receiver).NumArgs, 20, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(20), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20);
			}

			public static Object _value21_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value21()", ((ESMessageSend)receiver).NumArgs, 21, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(21), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21);
			}

			public static Object _value22_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value22()", ((ESMessageSend)receiver).NumArgs, 22, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(22), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22);
			}

			public static Object _value23_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value23()", ((ESMessageSend)receiver).NumArgs, 23, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(23), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23);
			}

			public static Object _value24_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value24()", ((ESMessageSend)receiver).NumArgs, 24, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(24), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24);
			}

			public static Object _value25_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value25()", ((ESMessageSend)receiver).NumArgs, 25, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(25), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25);
			}

			public static Object _value26_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value26()", ((ESMessageSend)receiver).NumArgs, 26, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(26), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26);
			}

			public static Object _value27_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value27()", ((ESMessageSend)receiver).NumArgs, 27, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(27), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27);
			}

			public static Object _value28_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value28()", ((ESMessageSend)receiver).NumArgs, 28, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(28), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28);
			}

			public static Object _value29_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value29()", ((ESMessageSend)receiver).NumArgs, 29, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(29), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29);
			}

			public static Object _value30_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value30()", ((ESMessageSend)receiver).NumArgs, 30, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(30), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30);
			}

			public static Object _value31_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30, Object a31) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value31()", ((ESMessageSend)receiver).NumArgs, 31, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(31), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31);
			}

			public static Object _value32_(Object receiver, Object a1, Object a2, Object a3, Object a4, Object a5, Object a6, Object a7, Object a8, Object a9, Object a10, Object a11, Object a12, Object a13, Object a14, Object a15, Object a16, Object a17, Object a18, Object a19, Object a20, Object a21, Object a22, Object a23, Object a24, Object a25, Object a26, Object a27, Object a28, Object a29, Object a30, Object a31, Object a32) {
				Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object> functor;
				try {
					functor = (Func<Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object, Object>)((ESMessageSend)receiver).function;
				} catch (InvalidCastException invalidCastEx) {
					throw new InvalidFunctionCallException("MessageSend.value32()", ((ESMessageSend)receiver).NumArgs, 32, ((ESMessageSend)receiver).function.GetType(), ESCompiledCode.methodFunctionTypeForNumArgs(32), invalidCastEx);
				}
				return functor(((ESMessageSend)receiver).receiver, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25, a26, a27, a28, a29, a30, a31, a32);
			}

			#endregion

			public override void publishCanonicalPrimitives() {

				publishPrimitive("receiver",					new FuncNs.Func<Object, Object>(_receiver_));
				publishPrimitive("receiver:",					new FuncNs.Func<Object, Object, Object>(_setReceiver_));

				publishPrimitive("method:",					new FuncNs.Func<Object, Object, Object>(_setMethod_));


				publishPrimitive("valueWithArguments:",				new FuncNs.Func<Object, Object, Object>(_valueWithArguments_));

				publishPrimitive("value",					new FuncNs.Func<Object, Object>(_value_));
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

}
