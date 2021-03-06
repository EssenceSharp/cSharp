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
using EssenceSharp.Runtime;
#endregion

namespace EssenceSharp.Exceptions {

	public class MessageNotUnderstood : MissingMethodException {

		protected String messageText				= "";
		protected Object tag;
		protected ESObjectSpace objectSpace;
		protected Object receiver;
		protected ESMessage message;

		public MessageNotUnderstood(Object receiver, ESMessage message) : base() {
			setReceiverAndMessage(receiver, message);
		}

		protected void setReceiverAndMessage(Object receiver, ESMessage message) {
			this.receiver = receiver;
			this.message = message;
			if (message == null) {
				ClassName = receiver == null ? "UndefinedObject" : receiver.GetType().AssemblyQualifiedName;
				tag = "MessageNotUnderstood";
				messageText = ClassName + " instances do not know how to respond to an unknown message.";
			} else {
				var messageClass = message.Class;
				if (messageClass == null) {
					ClassName = receiver == null ? "UndefinedObject" : receiver.GetType().AssemblyQualifiedName;
					MemberName = "<nil selector>";
					tag = "MessageNotUnderstood";
					messageText = ClassName + " instances do not know how to respond to an unknown message.";
				} else { 
					objectSpace = messageClass.ObjectSpace;
					var selector = message.Selector;
					MemberName = selector == null ? "<nil selector>" : selector.PrimitiveValue;
					ClassName = objectSpace.classOf(receiver).QualifiedName;
					tag = objectSpace.symbolFor("MessageNotUnderstood");
					messageText = ClassName + " instances do not know how to respond to the message '" + MemberName + "'";
				}
			}
		}

		public virtual string MessageText { 
			get {return messageText;}
			set {messageText = value;}
		}

		public override string Message { 
			get {return messageText;}
		}

		public virtual Object Tag { 
			get {return tag;}
			set {tag = value;}
		}

		public bool IsResumable { 
			get {return false;}
		}

		public virtual String Description {
			get { return ToString();}
		}

		public virtual Object defaultAction() {                      
			return null;
		}

		public virtual ESMessage MessageObject { 
			get {return message;}
		}

		public virtual Object Receiver { 
			get {return receiver;}
		}

		public virtual void signal() {
			throw this;
		}

		public virtual void signal(String message) {
			MessageText = message;
			throw this;
		}

		public virtual void pass() {
			throw this;
		}

	}

}
