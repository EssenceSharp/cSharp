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
#endregion

namespace EssenceSharp.Runtime {
			
	public class ESMessage : ESObject {

		protected ESSymbol 										selector 					= null;
		protected Object[]										arguments					= null;
		
		public ESMessage(ESBehavior esClass) : base(esClass) {
		}
		
		public ESMessage(ESBehavior esClass, ESSymbol selector, Object[] arguments) : base(esClass) {
			this.selector = selector;
			this.arguments = arguments;
		}

		public override ObjectStateArchitecture Architecture {
			get {return ObjectStateArchitecture.Message;}
		}
		
		public ESSymbol Selector {
			get {return selector;}
			set {selector = value;}
		}
		
		public Object[] Arguments {
			get {return arguments;}
			set {arguments = value;}
		}
		
		public override bool hasSameValueAs(ESObject other) {
			if (ReferenceEquals(this, other)) return true;
			ESMessage messageComparand = other as ESMessage;
			if (messageComparand == null) return false;
			if (selector == null) return false;
			Object[] otherArguments = messageComparand.Arguments;
			if (arguments == null) {
				if (otherArguments != null) return false;
				return selector.hasSameValueAs(messageComparand.Selector);
			} else if (otherArguments == null) return false;
			if (selector.hasSameValueAs(messageComparand.Selector)) {
				return elementsAreIdentical(arguments, otherArguments);
			} else {
				return false;
			}
		}
		
		public Object sendTo(ESObject receiver) {
			return receiver.performWithArguments(Selector, Arguments);
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

			protected override void bindToKernel() {
				domainClass = kernel.MessageClass;
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.Message;}
			}
		
			#region Primitive Definitions
		
			public Object _selector_(Object receiver) {
				return ((ESMessage)receiver).Selector;
			}
		
			public Object _setSelector_(Object receiver, Object selector) {
				((ESMessage)receiver).Selector = kernel.asESSymbol(selector);
				return receiver;
			}
		
			public Object _arguments_(Object receiver) {
				return kernel.newArray(((ESMessage)receiver).Arguments);
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
				publishPrimitive("arguments",						new FuncNs.Func<Object, Object>(_arguments_));
				publishPrimitive("arguments:",						new FuncNs.Func<Object, Object, Object>(_setArguments_));
				publishPrimitive("sendTo:",						new FuncNs.Func<Object, Object, Object>(_sendTo_));

			}

		}
		
	}

}
