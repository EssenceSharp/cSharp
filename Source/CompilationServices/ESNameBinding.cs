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
using System.Collections.Generic;
#if CLR2
using Microsoft.Scripting.Ast;
using Expression = Microsoft.Scripting.Ast.Expression;
#else
using System.Linq.Expressions;
using Expression = System.Linq.Expressions.Expression;
#endif
using EssenceSharp.Runtime;
using EssenceSharp.Runtime.Binding;
using EssenceSharp.Exceptions;
using EssenceSharp.Exceptions.System;
using InvalidOperationException = EssenceSharp.Exceptions.InvalidOperationException;
#endregion

namespace EssenceSharp.CompilationServices {

	public class NameBindingScope {

		protected CodeGenerationContext						context;
		protected NameBindingScope						outerScope;
		protected Dictionary<String, StackResidentDeclaration>			localBindings			= new Dictionary<String, StackResidentDeclaration>();
		protected List<ParameterDeclaration>					parameterDeclarations		= new List<ParameterDeclaration>();
		protected List<StackResidentVariableDeclaration>			localVariableDeclarations	= new List<StackResidentVariableDeclaration>();
		protected List<NamedValueOccurrence>					localOccurrences		= new List<NamedValueOccurrence>();
		protected int								freeVariableReferenceCount	= 0;

		protected Dictionary<String, NonLocalVariableDeclaration>		nonLocalBindings;
		protected Dictionary<String, InstanceVariableDeclaration>		instanceVariableBindings;
		protected Dictionary<String, NamespaceResidentVariableDeclaration>	namespaceResidentBindings;

		public NameBindingScope(CodeGenerationContext context) {
			this.context = context;
		}

		public NameBindingScope(CodeGenerationContext context, NameBindingScope outerScope) : this(context) {
			this.outerScope = outerScope;
		}

		public HashSet<ESSymbol> bindNonLocalVariablesToEnvironment(NamespaceObject environment, BehavioralObject methodHomeClass) {
			if (nonLocalBindings == null) {
				if (outerScope != null) return outerScope.bindNonLocalVariablesToEnvironment(environment, methodHomeClass);
				return null;
			}
			String nameContext;
			instanceVariableBindings = null;
			namespaceResidentBindings = null;
			HashSet<ESSymbol> instVarNames = null;			
			var methodName = Context.MethodSelector;
			if (methodName != null) {
				if (methodHomeClass == null) {
					nameContext = environment.PathnameString + " ## " + methodName.PrimitiveValue + " => ";
				} else {
					nameContext = methodHomeClass.PathnameString + ">>" + methodName.PrimitiveValue + " => ";
					methodHomeClass.allInstVarNamesAndIndexesDo((instVarName, index) => {
						if (instVarNames == null) instVarNames = new HashSet<ESSymbol>();
						instVarNames.Add(instVarName);
						NonLocalVariableDeclaration nonLocalVar;
						if (nonLocalBindings.TryGetValue(instVarName, out nonLocalVar)) {
							var instanceVar = declareInstanceVariable(instVarName.PrimitiveValue, null);
							instanceVar.Index = (int)index;
							nonLocalVar.occurrencesDo(occurrence => occurrence.Declaration = instanceVar);
						}});
				}
			} else {
				nameContext = environment.PathnameString + " => ";
			}
			HashSet<ESSymbol> undeclared = null;
			foreach (var kvp in nonLocalBindings) {
				var nonLocalVar = kvp.Value;
				var nonLocalName = nonLocalVar.NameSymbol;
				if (instVarNames == null || !instVarNames.Contains(nonLocalName)) {
					var nsResidentVar = declareNamespaceVariable(environment, nonLocalName, null);
					nonLocalVar.occurrencesDo(occurrence => occurrence.Declaration = nsResidentVar);
					var binding = nonLocalName.bindingInNamespaceIfAbsent(environment, AccessPrivilegeLevel.Local, ImportTransitivity.Transitive, null);
					if (binding == null) {
						if (undeclared == null) undeclared = new HashSet<ESSymbol>();
						var nameInContext = Context.symbolFor(nameContext + nonLocalName);
						undeclared.Add(nameInContext);
					}
				}
			}
			return undeclared;
		}
		
		public CodeGenerationContext Context {
			get {return context;}
		}

		public ParameterExpression SelfParameter {
			get {return Context.SelfParameter;}
		}

		public ParameterExpression ThisContextParameter {
			get {return Context.ThisContextParameter;}
		}

		public NameBindingScope OuterScope {
			get {return outerScope;}
		}

		public bool IsRoot {
			get {return outerScope == null;}
		}

		public NameBindingScope Root {
			get {return outerScope == null ? this : outerScope.Root;}
		}

		public int Count {
			get {return localBindings.Count;}
		}

		public int ParameterDeclarationCount {
			get {return parameterDeclarations.Count;}
		}

		public int LocalVariableDeclarationCount {
			get {return localVariableDeclarations.Count;}
		}

		public int LocalOccurrenceCount {
			get {return localOccurrences.Count;}
		}

		public int FreeVariableReferenceCount {
			get {return freeVariableReferenceCount;}
		}

		public ESSymbol symbolFor(String value) {
			return Context.symbolFor(value);
		}

		public void localDeclarationsDo(Functor1<NamedValueDeclaration> enumerator1) {
			foreach (var binding in localBindings) enumerator1(binding.Value);
		}

		public void localParametersDo(Action<ParameterDeclaration> enumerator1) {
			if (parameterDeclarations == null) return;
			foreach (var declaration in parameterDeclarations) enumerator1(declaration);
		}

		public void localVariablesDo(Action<StackResidentVariableDeclaration> enumerator1) {
			if (localVariableDeclarations == null) return;
			foreach (var declaration in localVariableDeclarations) enumerator1(declaration);
		}

		public void nonLocalVariablesDo(Action<NonLocalVariableDeclaration> enumerator1) {
			if (nonLocalBindings == null) return;
			foreach (var kvp in nonLocalBindings) enumerator1(kvp.Value);
		}

		public void instanceVariablesDo(Action<InstanceVariableDeclaration> enumerator1) {
			if (instanceVariableBindings == null) return;
			foreach (var kvp in instanceVariableBindings) enumerator1(kvp.Value);
		}

		public void namespaceResidentVariablesDo(Action<NamespaceResidentVariableDeclaration> enumerator1) {
			if (namespaceResidentBindings == null) return;
			foreach (var kvp in namespaceResidentBindings) enumerator1(kvp.Value);
		}

		public void allDeclarationsDo(Functor1<NamedValueDeclaration> enumerator1) {
			localDeclarationsDo(enumerator1);
			if (nonLocalBindings != null) foreach (var kvp in nonLocalBindings) enumerator1(kvp.Value);
			if (instanceVariableBindings != null) foreach (var kvp in instanceVariableBindings) enumerator1(kvp.Value);
			if (namespaceResidentBindings != null) foreach (var kvp in namespaceResidentBindings) enumerator1(kvp.Value);
			if (outerScope != null) outerScope.allDeclarationsDo(enumerator1);
		}

		public void localOccurrencesDo(Functor1<NamedValueOccurrence> enumerator1) {
			foreach (var occurence in localOccurrences) enumerator1(occurence);
		}

		public NamedValueDeclaration atIfAbsent(ESSymbol name, Functor0<NamedValueDeclaration> notFoundAction) {
			StackResidentDeclaration localDeclaration;
			if (localBindings.TryGetValue(name, out localDeclaration)) {
				return localDeclaration;
			}
			NonLocalVariableDeclaration nonLocalVariableDeclaration;
			if (nonLocalBindings != null && nonLocalBindings.TryGetValue(name, out nonLocalVariableDeclaration)) {
				return nonLocalVariableDeclaration;
			}
			if (outerScope == null) return notFoundAction == null ? null : notFoundAction();
			return outerScope.atIfAbsent(name, notFoundAction);
		}

		public ParameterDeclaration declareParameter(ParameterExpression rootParameter, Functor1<ParameterDeclaration, NamedValueDeclaration> collisionAction) {
			StackResidentDeclaration existingDeclaration;
			if (localBindings.TryGetValue(rootParameter.Name, out existingDeclaration)) if (collisionAction != null) return collisionAction(existingDeclaration);
			// Collisions must be conditionally permitted in order to enable the compiler to find subsequent errors wnen the same parameter or variable is declared more than twice.
			var declaration = new ParameterDeclaration(this, rootParameter);
			localBindings[rootParameter.Name] = declaration;
			parameterDeclarations.Add(declaration);
			return declaration;
		}

		public ParameterDeclaration declareParameter(String name, Functor1<ParameterDeclaration, StackResidentDeclaration> collisionAction) {
			StackResidentDeclaration existingDeclaration;
			if (localBindings.TryGetValue(name, out existingDeclaration)) if (collisionAction != null) return collisionAction(existingDeclaration);
			// Collisions must be conditionally permitted in order to enable the compiler to find subsequent errors wnen the same parameter or variable is declared more than twice.
			var declaration = new ParameterDeclaration(this, Context.symbolFor(name));
			localBindings[name] = declaration;
			parameterDeclarations.Add(declaration);
			return declaration;
		}

		public StackResidentVariableDeclaration declareLocalVariable(String name, Functor1<StackResidentVariableDeclaration, StackResidentDeclaration> collisionAction) {
			StackResidentDeclaration existingDeclaration;
			if (localBindings.TryGetValue(name, out existingDeclaration)) if (collisionAction != null) return collisionAction(existingDeclaration);
			// Collisions must be conditionally permitted in order to enable the compiler to find subsequent errors wnen the same parameter or variable is declared more than twice.
			var declaration = new StackResidentVariableDeclaration(this, Context.symbolFor(name));
			localBindings[name] = declaration;
			localVariableDeclarations.Add(declaration);
			return declaration;
		}

		public NonLocalVariableDeclaration declareNonLocalVariable(String name, Functor1<NonLocalVariableDeclaration, NonLocalVariableDeclaration> collisionAction) {
			if (outerScope == null) {
				if (nonLocalBindings == null) {
					nonLocalBindings = new Dictionary<String, NonLocalVariableDeclaration>();
				} else {
					NonLocalVariableDeclaration existingDeclaration;
					if (nonLocalBindings.TryGetValue(name, out existingDeclaration)) if (collisionAction != null) return collisionAction(existingDeclaration);
				}
				// Collisions must be permitted because subclasses can override inherited instance variables, and instance variables can override namespace-resident variables.
				var declaration = new NonLocalVariableDeclaration(this, Context.symbolFor(name));
				nonLocalBindings[name] = declaration;
				return declaration;
			} else {
				return outerScope.declareNonLocalVariable(name, collisionAction);
			}
		}

		public InstanceVariableDeclaration declareInstanceVariable(String name, Functor1<InstanceVariableDeclaration, InstanceVariableDeclaration> collisionAction) {
			if (instanceVariableBindings == null) {
				instanceVariableBindings	= new Dictionary<String, InstanceVariableDeclaration>();
			} else {
				InstanceVariableDeclaration existingDeclaration;
				if (instanceVariableBindings.TryGetValue(name, out existingDeclaration)) if (collisionAction != null) return collisionAction(existingDeclaration);
			}
			// Collisions must be permitted because subclasses can override inherited instance variables.
			var declaration = new InstanceVariableDeclaration(this, Context.symbolFor(name));
			instanceVariableBindings[name] = declaration;
			return declaration;
		}

		public PseudovariableSelf declareSelf() {
			StackResidentDeclaration self;
			if (!localBindings.TryGetValue(Context.SelfSymbol, out self)) {
				self  = new PseudovariableSelf(this);
				localBindings[Context.SelfSymbol] = self;
			}
			return (PseudovariableSelf)self;
		}

		public PseudovariableSuper declareSuper() {
			StackResidentDeclaration super;
			if (!localBindings.TryGetValue(Context.SuperSymbol, out super)) {
				super = new PseudovariableSuper(this);
				localBindings[Context.SuperSymbol] = super;
			}
			return (PseudovariableSuper)super;
		}

		public PseudovariableThisContext declareThisContext() {
			StackResidentDeclaration thisContext;
			if (!localBindings.TryGetValue(Context.ThisContextSymbol, out thisContext)) {
				thisContext = new PseudovariableThisContext(this);
				localBindings[Context.ThisContextSymbol] = thisContext;
			}
			return (PseudovariableThisContext)thisContext;
		}

		public NamespaceResidentVariableDeclaration declareNamespaceVariable(NamespaceObject environment, ESSymbol name, Functor1<NamespaceResidentVariableDeclaration, String> collisionAction) {
			if (namespaceResidentBindings == null) {
				namespaceResidentBindings = new Dictionary<String, NamespaceResidentVariableDeclaration>();
			} else if (namespaceResidentBindings.ContainsKey(name)) return collisionAction(name);
			var declaration = new NamespaceResidentVariableDeclaration(environment, this, name);
			namespaceResidentBindings[name] = declaration;
			return declaration;
		}

		public NamedValueOccurrence newOccurrenceOf(ESSymbol name) {
			var declaration = atIfAbsent(name, () => declareNonLocalVariable(name, null));
			var occurrence = declaration.newOccurrenceIn(this);
			localOccurrences.Add(occurrence);
			if (occurrence.IsFreeVariable && declaration.IsStackResident) freeVariableReferenceCount++;
			return occurrence;
		}

	}
	
	public abstract class NamedValueDeclaration {

		protected NameBindingScope scope;
		protected String name;
		protected ESSymbol nameSymbol;
		protected List<NamedValueOccurrence> occurrences = new List<NamedValueOccurrence>();
		protected int nonLocalOccurenceCount = 0;
 
		public NamedValueDeclaration(NameBindingScope scope, String name) {
			this.scope = scope;
			this.name = name;
			nameSymbol = scope.symbolFor(name);
		}

		public NameBindingScope Scope {
			get {return scope;}
		}

		public CodeGenerationContext Context {
			get {return scope.Context;}
		}

		public ESSymbol NameSymbol {
			get {return nameSymbol;}
		}

		public String NameString {
			get {return NameSymbol;}
		}

		public virtual bool IsParameter {
			get {return false;}
		}

		public virtual bool IsPseudovariable {
			get {return false;}
		}

		public virtual bool IsVariable {
			get {return false;}
		}

		public virtual bool IsStackResident {
			get {return false;}
		}

		public abstract bool IsAssignable {
			get;
		}

		public int OccurrenceCount {
			get {return occurrences.Count;}
		}

		public int NonLocalOccurrenceCount {
			get {return nonLocalOccurenceCount;}
		}

		public void occurrencesDo(Action<NamedValueOccurrence> enumerator1) {
			foreach (var occurence in occurrences) enumerator1(occurence);
		}

		public NamedValueOccurrence newOccurrenceIn(NameBindingScope referencingScope) {
			var occurrence = new NamedValueOccurrence(this);
			occurrences.Add(occurrence);
			if (occurrence.IsFreeVariable && IsStackResident) nonLocalOccurenceCount++;
			return occurrence;
		}

		public bool isLocalTo(NameBindingScope aScope) {
			return scope == aScope;
		}

		public abstract Expression asCLRGetValueExpression();

		public abstract Expression asCLRSetValueExpression(Expression newValue);

	}

	public abstract class StackResidentDeclaration : NamedValueDeclaration {

		public static ParameterExpression parameterExpressionFrom(ESSymbol name, Type parameterType) {
			return Expression.Parameter(parameterType, name.PrimitiveValue);
		}

		protected ParameterExpression parameter;

		protected StackResidentDeclaration(NameBindingScope scope, ParameterExpression parameter) : base(scope, parameter.Name) {
			this.parameter = parameter;
		}

		protected StackResidentDeclaration(NameBindingScope scope, ESSymbol name) : this(scope, name, TypeGuru.objectType) {
		}

		protected StackResidentDeclaration(NameBindingScope scope, ESSymbol name, Type parameterType) : this(scope, parameterExpressionFrom(name, parameterType)) {
		}

		public Type Type {
			get {return parameter.Type;}
		}

		public override bool IsStackResident {
			get {return true;}
		}

		public ParameterExpression asCLRDeclarationExpression() {
			return parameter;
		}

		public override Expression asCLRGetValueExpression() {
			return parameter;
		}

	}

	public class ParameterDeclaration : StackResidentDeclaration {

		public ParameterDeclaration(NameBindingScope scope, ParameterExpression parameter) : base(scope, parameter) {
		}

		public ParameterDeclaration(NameBindingScope scope, ESSymbol name) : base(scope, name) {
		}

		public ParameterDeclaration(NameBindingScope scope, ESSymbol name, Type parameterType) : base(scope, name, parameterType) {
		}

		public override bool IsParameter {
			get {return true;}
		}

		public override bool IsAssignable {
			get {return false;}
		}

		public override Expression asCLRSetValueExpression(Expression newValue) {
			return Expression.Throw(Expression.Constant("Method and block parameters are not assignable (" + NameString + ")"), typeof(ImmutableBindingException));
		}

	}

	public abstract class Pseudovariable : ParameterDeclaration {

		public Pseudovariable(NameBindingScope scope, ParameterExpression parameter) : base(scope, parameter) {
		}

		public override bool IsPseudovariable {
			get {return true;}
		}

	}

	public class PseudovariableSelf : Pseudovariable {

		public PseudovariableSelf(NameBindingScope scope) : base(scope, scope.SelfParameter) {
		}

	}

	public class PseudovariableSuper : Pseudovariable {

		public PseudovariableSuper(NameBindingScope scope) : base(scope, scope.SelfParameter) {
		}

	}

	public class PseudovariableThisContext : Pseudovariable {

		public PseudovariableThisContext(NameBindingScope scope) : base(scope, scope.ThisContextParameter) {
		}

	}

	public class StackResidentVariableDeclaration : StackResidentDeclaration {

		public StackResidentVariableDeclaration(NameBindingScope scope, ESSymbol name) : base(scope, name) {
		}

		public override bool IsVariable {
			get {return true;}
		}

		public override bool IsAssignable {
			get {return true;}
		}

		public override Expression asCLRSetValueExpression(Expression newValue) {
			return Expression.Assign(parameter, newValue);
		}

	}

	public abstract class AbstractNonStackResidentVariableDeclaration : NamedValueDeclaration {

		public AbstractNonStackResidentVariableDeclaration(NameBindingScope scope, ESSymbol name) : base(scope, name) {}

		public override bool IsAssignable {
			get {return true;}
		}

	}

	public class NonLocalVariableDeclaration : AbstractNonStackResidentVariableDeclaration {

		public NonLocalVariableDeclaration(NameBindingScope scope, ESSymbol name) : base(scope, name) {}

		public override Expression asCLRGetValueExpression() {
			throw new InvalidOperationException("A NonStackResidentVariableDeclaration cannot be used to generate code. Use either an InstanceVariableDeclaration or a NamespaceResidentVariableDeclaration instead.");
		}

		public override Expression asCLRSetValueExpression(Expression newValue) {
			throw new InvalidOperationException("A NonStackResidentVariableDeclaration cannot be used to generate code. Use either an InstanceVariableDeclaration or a NamespaceResidentVariableDeclaration instead.");
		}

	}

	public class InstanceVariableDeclaration : AbstractNonStackResidentVariableDeclaration {

		protected static readonly ConstantExpression mutabilityFlagBitConstant	= Expression.Constant(ESInitiallyMutableObject.mutabilityFlagBit);
		protected static readonly ConstantExpression zeroConstant		= Expression.Constant((byte)0);

		protected ConstantExpression slotIndex;
		protected Expression namedSlots;
		protected Expression isMutable;

		public InstanceVariableDeclaration(NameBindingScope scope, ESSymbol name) : base(scope, name) {
			var namedSlotsObject = Expression.Convert(Scope.SelfParameter, TypeGuru.esNamedSlotsObjectType);
			namedSlots = Expression.Field(namedSlotsObject, "namedSlots");
			var statusFlags = Expression.Field(namedSlotsObject, "statusFlags");
			isMutable = Expression.Equal(Expression.And(statusFlags, mutabilityFlagBitConstant), zeroConstant);
		}

		public override Expression asCLRGetValueExpression() {
			return Expression.ArrayAccess(namedSlots, slotIndex);
		}

		public override Expression asCLRSetValueExpression(Expression newValue) {
			return Expression.Condition(
						isMutable,
							Expression.Assign(Expression.ArrayAccess(namedSlots, slotIndex), newValue),
							Expression.Block(
								TypeGuru.objectType,
								Expression.Throw(Expression.Constant(new ImmutableObjectException())),
								newValue));
		}

		public int Index {
			set {slotIndex = Expression.Constant(value);}
		}

	}

	public class NamespaceResidentVariableDeclaration : AbstractNonStackResidentVariableDeclaration {

		protected ConstantExpression getVariableCallSite;
		protected ConstantExpression setVariableCallSite;

		public NamespaceResidentVariableDeclaration(NamespaceObject environment, NameBindingScope scope, ESSymbol name) : base(scope, name) {
			getVariableCallSite = Context.getVariableValueCallSiteConstantFor(environment, name);
			setVariableCallSite = Context.setVariableValueCallSiteConstantFor(environment, name);
		}

		public override Expression asCLRGetValueExpression() {
			return Expression.Invoke(Expression.Field(getVariableCallSite, CodeGenerationContext.callSiteType[0], "Target"), getVariableCallSite, Scope.SelfParameter);
		}

		public override Expression asCLRSetValueExpression(Expression newValue) {
			return Expression.Invoke(Expression.Field(setVariableCallSite, CodeGenerationContext.callSiteType[1], "Target"), setVariableCallSite, Scope.SelfParameter, newValue);
		}

	}

	public class NamedValueOccurrence {

		protected NamedValueDeclaration declaration = null;
		protected NameBindingScope referencingScope = null;
		protected bool isAssignmentTarget = false;

		public NamedValueOccurrence(NamedValueDeclaration declaration) {
			this.declaration = declaration;
		}

		public NamedValueDeclaration Declaration {
			get {return declaration;}
			set {declaration = value;}
		}

		public NameBindingScope DeclarationScope {
			get {return declaration.Scope;}
		}

		public NameBindingScope ReferencingScope {
			get {return referencingScope;}
		}

		public bool IsAssignmentTarget {
			get {return isAssignmentTarget;}
			set {isAssignmentTarget = value;}
		}

		public bool IsBoundVariable {
			get {return declaration.isLocalTo(ReferencingScope);}
		}

		public bool IsFreeVariable {
			get {return !IsBoundVariable;}
		}

		public ESSymbol Name {
			get {return declaration.NameSymbol;}
		}

		public String NameString {
			get {return declaration.NameString;}
		}

		public bool IsParameter {
			get {return declaration.IsParameter;}
		}

		public bool IsVariable {
			get {return declaration.IsVariable;}
		}

		public bool IsStackResident {
			get {return declaration.IsStackResident;}
		}

		public bool IsAssignable {
			get {return declaration.IsAssignable;}
		}

		public Expression asCLRGetValueExpression() {
			return declaration.asCLRGetValueExpression();
		}

		public Expression asCLRSetValueExpression(Expression newValue) {
			return declaration.asCLRSetValueExpression(newValue);
		}

	}

}
