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
using EssenceSharp.UtilityServices;
using EssenceSharp.Exceptions.System.PrimitiveFailures;
#endregion

namespace EssenceSharp.Runtime {
			
	public class ESMessage : ESObject {

		protected ESSymbol	selector;
		protected long 		numArgs;
		protected Object[]	arguments;
		
		public ESMessage(ESBehavior esClass) : base(esClass) {
		}
		
		public ESMessage(ESBehavior esClass, ESSymbol selector, Object[] arguments) : base(esClass) {
			setSelector(selector);
			this.arguments = arguments;
		}

		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.Message;}
		}
		
		public ESSymbol Selector {
			get {return selector;}
			set {setSelector(value);}
		}
		
		public long NumArgs {
			get {return numArgs;}
		}

		public virtual void setSelector(ESSymbol newSelector) {
			selector = newSelector;
			numArgs = selector == null ? 0 : selector.NumArgs;
		}

		public Object[] Arguments {
			get {return arguments;}
			set {arguments = value;}
		}
		
		public virtual Object sendTo(Object receiver) {
			var esReceiver = receiver as ESObject;
			if (esReceiver != null) {
				return esReceiver.performWithArguments(selector, arguments);
			}
			var providedArgCount = arguments == null ? 0 : arguments.Length;
			if (providedArgCount < numArgs) {
				ESObjectSpace.throwInvalidFunctionCallException("Message.sendTo()", NumArgs, 0, ESCompiledCode.methodFunctionTypeForNumArgs(numArgs), ESCompiledCode.methodFunctionTypeForNumArgs(providedArgCount), null);
			}
			var os = Class.ObjectSpace;
			var block = os.newBlockToSend(selector);
			var function = block.Function;
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
		
		public override void printElementsUsing(uint depth, Action<String> append, Action<uint> newLine) {
			newLine(depth);
			append("selector: ");
			if (selector == null) {
				append("nil");
			} else {
				selector.printUsing(depth, append, newLine);
			}
			newLine(depth);
			append("arguments: ");
			if (arguments == null) {
				append("nil");
			} else {
				print(arguments, depth, append, newLine);
			}
		}

		public override T valueBy<T>(Operation<T> operation) {
		    return operation.applyToMessage(this);
		}

		public new class Primitives : PrimitiveDomain {

			protected override void bindToObjectSpace() {
				domainClass = objectSpace.MessageClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.Message;}
			}
		
			#region Primitive Definitions
		
			public Object _selector_(Object receiver) {
				return ((ESMessage)receiver).Selector;
			}
		
			public Object _setSelector_(Object receiver, Object selector) {
				((ESMessage)receiver).setSelector(objectSpace.asESSymbol(selector));
				return receiver;
			}
		
			public Object _numArgs_(Object receiver) {
				return ((ESMessage)receiver).NumArgs;
			}
		
			public Object _arguments_(Object receiver) {
				return objectSpace.newArray(((ESMessage)receiver).Arguments);
			}
		
			public Object _setArguments_(Object receiver, Object arguments) {
				((ESMessage)receiver).Arguments = asHostArray<Object>(arguments);
				return receiver;
			}
		
			public Object _sendTo_(Object receiver, Object metaReceiver) {
				Object value = (ESObject)((ESMessage)receiver).sendTo((ESObject)metaReceiver);
				return value;
			}
		
			#endregion

			public override void publishCanonicalPrimitives() {

				publishPrimitive("selector",						new FuncNs.Func<Object, Object>(_selector_));
				publishPrimitive("selector:",						new FuncNs.Func<Object, Object, Object>(_setSelector_));
				publishPrimitive("numArgs",						new FuncNs.Func<Object, Object>(_numArgs_));
				publishPrimitive("arguments",						new FuncNs.Func<Object, Object>(_arguments_));
				publishPrimitive("arguments:",						new FuncNs.Func<Object, Object, Object>(_setArguments_));
				publishPrimitive("sendTo:",						new FuncNs.Func<Object, Object, Object>(_sendTo_));

			}

		}
		
	}

}
