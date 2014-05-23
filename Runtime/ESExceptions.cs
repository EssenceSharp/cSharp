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
#endregion

namespace EssenceSharp.Runtime {

	public abstract class EssenceSharpException : Exception {

		protected EssenceSharpException()
			: base() {
		}

		protected EssenceSharpException(String description)
			: base(description) {
		}

		protected EssenceSharpException(String description, Exception specificException)
			: base(description, specificException) {
		}

	}

	public class ESNonLocalReturn : EssenceSharpException {

		protected long		targetContextIdentity	= -1;
		protected Object	returnValue;

		public ESNonLocalReturn(long targetContextIdentity, Object returnValue) : base() {
			this.targetContextIdentity	= targetContextIdentity;
			this.returnValue		= returnValue;
		}

		public long TargetContextIdentity {
			get {return targetContextIdentity;}
		}

		public Object ReturnValue {
			get {return returnValue;}
		}

	}

	public class InvalidOperationException : EssenceSharpException {

		public InvalidOperationException()
			: base() {
		}

		public InvalidOperationException(String description)
			: base(description) {
		}

		public InvalidOperationException(String description, Exception specificException)
			: base(description, specificException) {
		}

	}

	public class UnsupportedOperationException : EssenceSharpException {

		public UnsupportedOperationException()
			: base() {
		}

		public UnsupportedOperationException(String description)
			: base(description) {
		}

		public UnsupportedOperationException(String description, Exception specificException)
			: base(description, specificException) {
		}

	}

	public class InvalidArgumentException : EssenceSharpException {

		public InvalidArgumentException()
			: base() {
		}

		public InvalidArgumentException(String description)
			: base(description) {
		}

		public InvalidArgumentException(String description, Exception specificException)
			: base(description, specificException) {
		}

	}

	public abstract class EssenceSharpRuntimeException : EssenceSharpException {
		
		public EssenceSharpRuntimeException() : base() {
		}
		
		public EssenceSharpRuntimeException(String description) : base(description) {
		}
		
		public EssenceSharpRuntimeException(String description, Exception specificException) : base(description, specificException) {
		}
		
	}
	
	public class ImmutableObjectException : EssenceSharpRuntimeException {
		
		public ImmutableObjectException() : base() {
		}
		
		public ImmutableObjectException(String description) : base(description) {
		}
		
		public ImmutableObjectException(String description, Exception specificException) : base(description, specificException) {
		}

	}
	
	public class ImmutableBindingException : EssenceSharpRuntimeException {
		
		public ImmutableBindingException() : base() {
		}
		
		public ImmutableBindingException(String description) : base(description) {
		}
		
		public ImmutableBindingException(String description, Exception specificException) : base(description, specificException) {
		}

	}
	
	public class InternalSystemException : EssenceSharpRuntimeException {
		
		public InternalSystemException() : base() {
		}
		
		public InternalSystemException(String description) : base(description) {
		}
		
		public InternalSystemException(String description, Exception specificException) : base(description, specificException) {
		}

	}
	
	public class AssemblyBindingFailure : InternalSystemException {
		
		public AssemblyBindingFailure(String description) : base(description) {
		}

		public AssemblyBindingFailure(String description, Exception specificException) : base(description, specificException) {
		}

	}
	
	public class NullReceiverException : InternalSystemException {
		
		public NullReceiverException() : base() {
		}
		
		public NullReceiverException(String description) : base(description) {
		}
		
		public NullReceiverException(String description, Exception specificException) : base(description, specificException) {
		}

	}
	
	public class InvalidFunctionCallException : InternalSystemException {
		
		protected long expectedArgCount = 0;
		protected long actualArgCount = 0;
		protected Type expectedFunctionType = null;
		protected Type actualFunctionType = null;
		
		public InvalidFunctionCallException(String description, long expectedArgCount, long actualArgCount, Type expectedFunctionType, Type actualFunctionType, Exception specificException) : base(description, specificException) {
			this.expectedArgCount = expectedArgCount;
			this.actualArgCount = actualArgCount;
			this.expectedFunctionType = expectedFunctionType;
			this.actualFunctionType = actualFunctionType;
		}

		public long ExpectedArgCount {
			get {return expectedArgCount;}
		}

		public long ActualArgCount {
			get {return actualArgCount;}
		}

		public Type ExpectedFunctionType {
			get {return expectedFunctionType;}
		}

		public Type ActualFunctionType {
			get {return actualFunctionType;}
		}

	}
	
	public class PrimitiveFailException : InternalSystemException {
		
		public PrimitiveFailException() : base() {
		}
		
		public PrimitiveFailException(String description) : base(description) {
		}
		
		public PrimitiveFailException(Exception specificException) : base("Invalid argument count", specificException) {
		}
		
		public PrimitiveFailException(String description, Exception specificException) : base(description, specificException) {
		}

	}
	
	public class PrimIllegalOperationException : PrimitiveFailException {
		
		public PrimIllegalOperationException() : base() {
		}
		
		public PrimIllegalOperationException(String description) : base(description) {
		}
		
		public PrimIllegalOperationException(String description, Exception specificException) : base(description, specificException) {
		}

	}

	public class PrimNonDeletableKeyException : PrimIllegalOperationException {
		
		public PrimNonDeletableKeyException() : base() {
		}
		
		public PrimNonDeletableKeyException(String description) : base(description) {
		}
		
		public PrimNonDeletableKeyException(String description, Exception specificException) : base(description, specificException) {
		}

	}
	
	public class PrimInvalidOperandException : PrimitiveFailException {
		
		public PrimInvalidOperandException() : base() {
		}
		
		public PrimInvalidOperandException(String description) : base(description) {
		}
		
		public PrimInvalidOperandException(String description, Exception specificException) : base(description, specificException) {
		}

	}
	
	public class PrimIndexBoundsExcessionException : PrimInvalidOperandException {

		protected long index;
		protected long minIndex;
		protected long maxIndex;
		
		public PrimIndexBoundsExcessionException() : base() {
		}
		
		public PrimIndexBoundsExcessionException(String description) : base(description) {
		}
		
		public PrimIndexBoundsExcessionException(String description, long index, long minIndex, long maxIndex) : base(description) {
			this.index = index;
			this.minIndex = minIndex;
			this.maxIndex = maxIndex;

		}

		public long Index {
			get {return index;}
		}

		public long MinIndex {
			get {return minIndex;}
		}

		public long MaxIndex {
			get {return maxIndex;}
		}


	}
	
	public class MustBeBoolean : PrimInvalidOperandException {
		
		public MustBeBoolean() : base() {
		}
		
		public MustBeBoolean(String description) : base(description) {
		}
		
		public MustBeBoolean(String description, Exception specificException) : base(description, specificException) {
		}

	}
	
	public class PrimInvalidKeyException : PrimInvalidOperandException {
		
		public PrimInvalidKeyException() : base() {
		}
		
		public PrimInvalidKeyException(String description) : base(description) {
		}
		
		public PrimInvalidKeyException(String description, Exception specificException) : base(description, specificException) {
		}

	}
	
	public class NamespaceEntryNameConflictException : PrimInvalidOperandException {
		
		public NamespaceEntryNameConflictException() : base() {
		}
		
		public NamespaceEntryNameConflictException(String description) : base(description) {
		}
		
		public NamespaceEntryNameConflictException(String description, Exception specificException) : base(description, specificException) {
		}

	}
	
	public class UnimplementedPrimitiveException : PrimitiveFailException {
		
		public UnimplementedPrimitiveException() : base() {
		}
		
		public UnimplementedPrimitiveException(String description) : base(description) {
		}
		
		public UnimplementedPrimitiveException(Exception specificException) : base("Invalid argument count", specificException) {
		}
		
		public UnimplementedPrimitiveException(String description, Exception specificException) : base(description, specificException) {
		}

	}

}
