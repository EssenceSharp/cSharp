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
using System.Collections.Generic;
using System.Dynamic;
#endregion

namespace EssenceSharp.Runtime.Binding {

	public class MessageSendBinder : DynamicMetaObjectBinder {

		protected DynamicBindingGuru	dynamicBindingGuru	= null;
		protected MessageReceiverKind	receiverKind		= MessageReceiverKind.General;
		protected ESSymbol		selector		= null;

		protected MessageSendBinder(DynamicBindingGuru dynamicBindingGuru, MessageReceiverKind receiverKind, ESSymbol selector) {
			this.dynamicBindingGuru	= dynamicBindingGuru;
			this.receiverKind	= receiverKind;
			this.selector		= selector;
		}

		public DynamicBindingGuru DynamicBindingGuru {
			get {return dynamicBindingGuru;}
		}

		public MessageReceiverKind ReceiverKind {
			get {return receiverKind;}
		}

		public ESSymbol Selector {
			get {return selector;}
			set {selector = value;}
		}

		public override DynamicMetaObject Bind(DynamicMetaObject target, DynamicMetaObject[] args) {

			switch (receiverKind) {
				case MessageReceiverKind.General:
					return dynamicBindingGuru.metaObjectToSendMessageToObject(target, Selector, args);
				case MessageReceiverKind.Self:
					return dynamicBindingGuru.metaObjectToSendMessageToSelf(target, Selector, args);
				case MessageReceiverKind.Super:
					return dynamicBindingGuru.metaObjectToSendMessageToSuper(target, Selector, args);
				case MessageReceiverKind.ThisContext:
					return dynamicBindingGuru.metaObjectToSendMessageToThisContext(target, Selector, args);
			}

			return null; // Yes, the C# compiler really IS that dumb....

		}

		public class Registry : BinderRegistry {

			protected Dictionary<ESSymbol, MessageSendBinder>	generalReceiverRegistry		= new Dictionary<ESSymbol, MessageSendBinder>();
			// protected Dictionary<ESSymbol, MessageSendBinder>	selfReceiverRegistry		= new Dictionary<ESSymbol, MessageSendBinder>();
			protected Dictionary<ESSymbol, MessageSendBinder>	superReceiverRegistry		= new Dictionary<ESSymbol, MessageSendBinder>();
			protected Dictionary<ESSymbol, MessageSendBinder>	thisContextReceiverRegistry	= new Dictionary<ESSymbol, MessageSendBinder>();

			public Registry(DynamicBindingGuru dynamicBindingGuru) : base(dynamicBindingGuru) {
			}

			public MessageSendBinder canonicalBinderFor(MessageReceiverKind receiverKind, ESSymbol selector) {
				Dictionary<ESSymbol, MessageSendBinder> registry = null;
				switch (receiverKind) {
					case MessageReceiverKind.General:
					case MessageReceiverKind.Self:
						registry = generalReceiverRegistry;
						break;
					case MessageReceiverKind.Super:
						registry = superReceiverRegistry;
						break;
					case MessageReceiverKind.ThisContext:
						registry = thisContextReceiverRegistry;
						break;
				}
				MessageSendBinder binder;
				if (!registry.TryGetValue(selector, out binder)) {
					binder = new MessageSendBinder(DynamicBindingGuru, receiverKind, selector);
					registry[selector] = binder;
				}
				return binder;
			}

		}

	}

}