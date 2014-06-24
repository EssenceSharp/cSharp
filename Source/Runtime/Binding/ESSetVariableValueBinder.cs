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
using System.Collections.ObjectModel;
#if CLR2
using Microsoft.Scripting.Ast;
using Expression = Microsoft.Scripting.Ast.Expression;
#else
using System.Linq.Expressions;
using Expression = System.Linq.Expressions.Expression;
#endif
using EssenceSharp.Exceptions.System;
#endregion

namespace EssenceSharp.Runtime.Binding {

	public  class SetVariableValueBinder : NamedVariableBinder {

		protected static readonly ConstantExpression mutabilityFlagBitConstant	= Expression.Constant(ESInitiallyMutableObject.mutabilityFlagBit);
		protected static readonly ConstantExpression zeroConstant		= Expression.Constant((byte)0);

		protected SetVariableValueBinder(DynamicBindingGuru dynamicBindingGuru, ESSymbol name) : base(dynamicBindingGuru, name) {
		}

		protected SetVariableValueBinder(DynamicBindingGuru dynamicBindingGuru, ESSymbol name, NamespaceObject environment) : base(dynamicBindingGuru, name, environment) {
		}

		public override Expression Bind(Object[] args, ReadOnlyCollection<ParameterExpression> parameters, LabelTarget returnLabel) {
			Expression testRuleValidityExpression = null;
			Expression setVariableValueExpression = null;
			ParameterExpression value = parameters[1];
			doAllButFinalBinding(
				args, 
				(BindingHandle handle) => {
					testRuleValidityExpression = Expression.Constant(true);
					setVariableValueExpression = 
						Expression.Assign(
							Expression.Property(Expression.Constant(handle), "Value"),
							value);
				});

			return Expression
				.IfThen(
					testRuleValidityExpression,
						Expression.Return(returnLabel, setVariableValueExpression));
		}

		public new class Registry : NamedVariableBinder.Registry {

			protected readonly Dictionary<NamespaceObject, Dictionary<ESSymbol, SetVariableValueBinder>> registry = new Dictionary<NamespaceObject, Dictionary<ESSymbol, SetVariableValueBinder>>();

			public  Registry(DynamicBindingGuru dynamicBindingGuru) : base(dynamicBindingGuru) {
			}

			public SetVariableValueBinder canonicalBinderFor(ESSymbol name) {
				return canonicalBinderFor(name, defaultNamespace);
			}

			public SetVariableValueBinder canonicalBinderFor(ESSymbol name, NamespaceObject environment) {
				if (environment == null) environment = defaultNamespace;
				Dictionary<ESSymbol, SetVariableValueBinder> nameRegistry;
				SetVariableValueBinder binder;
				if (!registry.TryGetValue(environment, out nameRegistry)) {
					nameRegistry = new Dictionary<ESSymbol, SetVariableValueBinder>();
					binder = new SetVariableValueBinder(DynamicBindingGuru, name, environment);
					nameRegistry[name] = binder;
					return binder;
				}
				if (!nameRegistry.TryGetValue(name, out binder)) {
					binder = new SetVariableValueBinder(DynamicBindingGuru, name, environment);
					nameRegistry[name] = binder;
				}
				return binder;
			}

		}

	}

}
