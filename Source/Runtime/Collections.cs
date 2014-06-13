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
using System.Collections;
using System.Collections.Generic;
#if CLR2
using FuncNs = Microsoft.Scripting.Utils;
#else
using FuncNs = System;
#endif
using EssenceSharp.UtilityServices;
using EssenceSharp.Exceptions.System;
using EssenceSharp.Exceptions.System.PrimitiveFailures;
#endregion


namespace EssenceSharp.Runtime {

	public class OrderedCollection : List<Object> {

		public OrderedCollection() : base() {
		}

		public OrderedCollection(int capacity) : base(capacity) {
		}

		public OrderedCollection(long capacity) : base((int)capacity) {
		}

		public OrderedCollection(IEnumerable<Object> source) : base(source) {
		}

		public bool contains(ESBlock predicate) {
			var predf = predicate.F1;
			return Exists(element => (bool)predf(element));
		}

		public Object detect(ESBlock predicate, ESBlock ifAbsentAction) {
			int mySize = Count;
			var predf = predicate.F1;
			for (var i = 0; i < mySize; i++) {
				var element = this[i];
				if ((bool)predf(element)) return element;
			}
			return ifAbsentAction == null ? null : ifAbsentAction.value0();
		}

		public OrderedCollection select(ESBlock predicate) {
			int mySize = Count;
			var selection = new OrderedCollection(mySize);
			var predf = predicate.F1;
			for (var i = 0; i < mySize; i++) {
				var element = this[i];
				if ((bool)predf(element)) selection.Add(element);
			}
			return selection;
		}

	}
}
