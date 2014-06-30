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
using EssenceSharp.Exceptions;
#endregion

namespace EssenceSharp.Runtime {

	public interface Operation<T> {

		// Double dispatching root 
		T applyTo(ESObject operand);
        
		// Generic
		T applyToHostSystemObject(Object operand);
		T applyToESObject(ESObject operand);

		// Runtime objects
		T applyToObject(ESObject operand);
		T applyToNamedSlotsObject(ESNamedSlotsObject operand);
		T applyToIndexedObjectSlotsObject(ESArray operand);
		T applyToIndexedByteSlotsObject(ESByteArray operan);
		T applyToIndexedCharSlotsObject(ESString operand);
		T applyToIndexedHalfWordSlotsObject(ESHalfWordArray operand);
		T applyToIndexedWordSlotsObject(ESWordArray operand);
		T applyToIndexedLongWordSlotsObject(ESLongWordArray operand);
		T applyToIndexedSinglePrecisionSlotsObject(ESFloatArray operand);
		T applyToIndexedDoublePrecisionSlotsObject(ESDoubleArray operand);
		T applyToIndexedQuadPrecisionSlotsObject(ESQuadArray operand);
		T applyToSymbol(ESSymbol operand);
		T applyToMessage(Runtime.ESMessage operand);
		T applyToAssociation(ESAssociation operand);
		T applyToBindingReference(ESBindingReference operand);
		T applyToIdentityDictionary(ESIdentityDictionary operand);
		T applyToDictionary(ESDictionary operand);
		T applyToNamespace(ESNamespace operand);
		T applyToPathname(ESPathname operand);
		T applyToCompiledBlock(ESBlock operand);
		T applyToCompiledMethod(ESMethod operand);
		T applyToBehavior(ESBehavior operand);
		T applyToClass(ESClass operand);
		T applyToMetaclass(ESMetaclass operand);
 		T applyToBehavioralTrait(ESBehavioralTrait operand);
		T applyToInstanceTrait(ESInstanceTrait operand);
		T applyToClassTrait(ESClassTrait operand);
		T applyToTraitTransformation(ESTraitTransformation operand);
		T applyToTraitComposition(ESTraitComposition operand);

	}

	public abstract class AbstractOperation<T> : Operation<T> {

		// Double dispatching root 

		public virtual T applyTo(ESObject operand) {
			return operand.valueBy(this);
		}

		// Generic

		public virtual T applyToHostSystemObject(Object operand) {
			throw new UnsupportedOperationException();
		}

		public virtual T applyToESObject(ESObject operand) {
			throw new UnsupportedOperationException();
		}

		// Runtime objects

		public virtual T applyToObject(ESObject operand) {
			return applyToESObject(operand);
		}

		public virtual T applyToNamedSlotsObject(ESNamedSlotsObject operand) {
			return applyToESObject(operand);
		}

		public virtual T applyToIndexedObjectSlotsObject(ESArray operand) {
			return applyToESObject(operand);
		}

		public virtual T applyToIndexedByteSlotsObject(ESByteArray operand) {
			return applyToESObject(operand);
		}

		public virtual T applyToIndexedCharSlotsObject(ESString operand) {
			return applyToESObject(operand);
		}

		public virtual T applyToIndexedHalfWordSlotsObject(ESHalfWordArray operand) {
			return applyToESObject(operand);
		}

		public virtual T applyToIndexedWordSlotsObject(ESWordArray operand) {
			return applyToESObject(operand);
		}

		public virtual T applyToIndexedLongWordSlotsObject(ESLongWordArray operand) {
			return applyToESObject(operand);
		}

		public virtual T applyToIndexedSinglePrecisionSlotsObject(ESFloatArray operand) {
			return applyToESObject(operand);
		}

		public virtual T applyToIndexedDoublePrecisionSlotsObject(ESDoubleArray operand) {
			return applyToESObject(operand);
		}

		public virtual T applyToIndexedQuadPrecisionSlotsObject(ESQuadArray operand) {
			return applyToESObject(operand);
		}

		public virtual T applyToSymbol(ESSymbol operand) {
			return applyToESObject(operand);
		}

		public virtual T applyToMessage(ESMessage operand) {
			return applyToESObject(operand);
		}

		public virtual T applyToAssociation(ESAssociation operand) {
			return applyToESObject(operand);
		}

		public virtual T applyToBindingReference(ESBindingReference operand) {
			return applyToESObject(operand);
		}

		public virtual T applyToIdentityDictionary(ESIdentityDictionary operand) {
			return applyToESObject(operand);
		}

		public virtual T applyToDictionary(ESDictionary operand) {
			return applyToESObject(operand);
		}

		public virtual T applyToNamespace(ESNamespace operand) {
			return applyToESObject(operand);
		}

		public virtual T applyToPathname(ESPathname operand) {
			return applyToESObject(operand);
		}

		public virtual T applyToCompiledBlock(ESBlock operand) {
			return applyToESObject(operand);
		}

		public virtual T applyToCompiledMethod(ESMethod operand) {
			return applyToESObject(operand);
		}

		public virtual T applyToBehavior(ESBehavior operand) {
			return applyToESObject(operand);
		}

		public virtual T applyToClass(ESClass operand) {
			return applyToESObject(operand);
		}

		public virtual T applyToMetaclass(ESMetaclass operand) {
			return applyToESObject(operand);
		}

		public virtual T applyToBehavioralTrait(ESBehavioralTrait operand) {
			return applyToESObject(operand);
		}

		public virtual T applyToInstanceTrait(ESInstanceTrait operand) {
			return applyToESObject(operand);
		}

		public virtual T applyToClassTrait(ESClassTrait operand) {
			return applyToESObject(operand);
		}

		public virtual T applyToTraitTransformation(ESTraitTransformation operand) {
			return applyToESObject(operand);
		}

		public virtual T applyToTraitComposition(ESTraitComposition operand) {
			return applyToESObject(operand);
		}

	}

	public abstract class GeneralGraphNavigator<T> : AbstractOperation<T> {

		protected Operation<T> operation;

		protected GeneralGraphNavigator(Operation<T> operation) {
			this.operation = operation;
		}

		public override T applyToHostSystemObject(Object operand) {
			return operation.applyToHostSystemObject(operand);
		}

		public override T applyToESObject(ESObject operand) {
			return operation.applyToESObject(operand);
		}

		public override T applyToObject(ESObject operand) {
			return operation.applyToObject(operand);
		}

		public override T applyToNamedSlotsObject(ESNamedSlotsObject operand) {
			return operation.applyToNamedSlotsObject(operand);
		}

		public override T applyToIndexedObjectSlotsObject(ESArray operand) {
			return operation.applyToIndexedObjectSlotsObject(operand);
		}

		public override T applyToIndexedByteSlotsObject(ESByteArray operand) {
			return operation.applyToIndexedByteSlotsObject(operand);
		}

		public override T applyToIndexedCharSlotsObject(ESString operand) {
			return operation.applyToIndexedCharSlotsObject(operand);
		}

		public override T applyToIndexedHalfWordSlotsObject(ESHalfWordArray operand) {
			return operation.applyToIndexedHalfWordSlotsObject(operand);
		}

		public override T applyToIndexedWordSlotsObject(ESWordArray operand) {
			return operation.applyToIndexedWordSlotsObject(operand);
		}

		public override T applyToIndexedLongWordSlotsObject(ESLongWordArray operand) {
			return operation.applyToIndexedLongWordSlotsObject(operand);
		}

		public override T applyToIndexedSinglePrecisionSlotsObject(ESFloatArray operand) {
			return operation.applyToIndexedSinglePrecisionSlotsObject(operand);
		}

		public override T applyToIndexedDoublePrecisionSlotsObject(ESDoubleArray operand) {
			return operation.applyToIndexedDoublePrecisionSlotsObject(operand);
		}

		public override T applyToIndexedQuadPrecisionSlotsObject(ESQuadArray operand) {
			return operation.applyToIndexedQuadPrecisionSlotsObject(operand);
		}

		public override T applyToSymbol(ESSymbol operand) {
			return operation.applyToSymbol(operand);
		}

		public override T applyToMessage(ESMessage operand) {
			return operation.applyToMessage(operand);
		}

		public override T applyToAssociation(ESAssociation operand) {
			return operation.applyToAssociation(operand);
		}

		public override T applyToBindingReference(ESBindingReference operand) {
			return operation.applyToBindingReference(operand);
		}

		public override T applyToDictionary(ESDictionary operand) {
			return operation.applyToDictionary(operand);
		}

		public override T applyToNamespace(ESNamespace operand) {
			return operation.applyToNamespace(operand);
		}

		public override T applyToPathname(ESPathname operand) {
			return operation.applyToPathname(operand);
		}

		public override T applyToCompiledBlock(ESBlock operand) {
			return operation.applyToCompiledBlock(operand);
		}

		public override T applyToCompiledMethod(ESMethod operand) {
			return operation.applyToCompiledMethod(operand);
		}

		public override T applyToBehavior(ESBehavior operand) {
			return operation.applyToBehavior(operand);
		}

		public override T applyToClass(ESClass operand) {
			return operation.applyToClass(operand);
		}

		public override T applyToMetaclass(ESMetaclass operand) {
			return operation.applyToMetaclass(operand);
		}

		public override T applyToBehavioralTrait(ESBehavioralTrait operand) {
			return operation.applyToBehavioralTrait(operand);
		}

		public override T applyToInstanceTrait(ESInstanceTrait operand) {
			return operation.applyToInstanceTrait(operand);
		}

		public override T applyToClassTrait(ESClassTrait operand) {
			return operation.applyToClassTrait(operand);
		}

		public override T applyToTraitTransformation(ESTraitTransformation operand) {
			return operation.applyToTraitTransformation(operand);
		}

		public override T applyToTraitComposition(ESTraitComposition operand) {
			return operation.applyToTraitComposition(operand);
		}

	}

}
