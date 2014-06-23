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
using System.Runtime.CompilerServices;
#endregion

namespace EssenceSharp.Runtime.Binding {

	public abstract class NamedVariableBinder : CallSiteBinder {
		// A concrete subclass of ESNamedVariableBinder is responsible for fetching/caching/providing a readable, writable reference to a named variable, 
		// along with the code for using the reference to actually get or set the value of the variable. There are, of course, only two types of references 
		// to a named variable, and only two ways such a reference can be used (get or set the value of the variable,) resulting in four possible cases. 
		// All four cases are handled by one of two classes, as shown below:
		//
		//				Named Instance Variable			Namespace-Resident Variable (Note: A Smalltalk class is also a namespace.)
		//
		//	Get variable value:	GetVariableValueBinder			GetVariableValueBinder			
		//	Set variable value:	SetVariableValueBinder			SetVariableValueBinder
		//
		// NamedVariableBinders are NOT used for local ("temporary") variables, block parameters or method parameters. They're used for all the other 
		// cases, which are a) named instance variables, b) local and inherited class variables (somewhat analogous to static variables,) c) sharedPool 
		// variables (which are sort of analogous to struct/class/interface names imported from a namespace into a specific class, to use C# terminology,) 
		// and d) variable names defined in, imported into, or inherited by the local namespace. (Namepaces in Smalltalk are live objects at run time.)
		//
		// The first task of a NamedVariableBinder is to determine whether the named variable is an instance variable or a namespace-resident variable.
		// It delegates that to the Smalltalk class of the binder's operand, which is the first element of the <args> array. (Note that the object may
		// not actually be a Smalltlak object, but the runtime system nevertheless will have or dynamically create a Smalltallk class for it.)
		//
		// If the named variable is an instance variable, the second task is to retrieve the variable's index, and use that index to generate the code that 
		// will get or set the value of the instance variable. The Expression to generate that code is returned as the value of the Bind method.
		//
		// If the named variable is a namespace-resident variable, the second task is to retrieve the ESNamespaceReference object which can be used to get
		// or set the value of the variable (even should the name be dynamically changed at run time,) and then to generate the code that will get or set
		// the value of the namespace-resident variable. The Expression to generate that code is returned as the value of the Bind method.

		// Purpose of the Selector: If not null, the <selector> identifies the name of the method in which the variable reference was compiled. It is
		// used in the case where the named variable is not an instance variable in order to look up the class in which the method is defined, which
		// may not be the same as the class of the object in arg[0], because the method could be executed with an instance of a subclass as the receiver.

		protected static readonly AccessPrivilegeLevel		accessPrivilege		= AccessPrivilegeLevel.Local;

		protected DynamicBindingGuru				dynamicBindingGuru;
		protected ESKernel					kernel;
		protected ESNamespace					environment;
		protected ESSymbol					name;			// Name of the referenced variable (may be a qualified ("dotted") reference)
		protected String					nameString;
		protected readonly Functor0<ESBindingReference>		addToUndeclared;


		public NamedVariableBinder(DynamicBindingGuru dynamicBindingGuru, ESSymbol name) : this(dynamicBindingGuru, name, null) {
		}

		public NamedVariableBinder(DynamicBindingGuru dynamicBindingGuru, ESSymbol name, ESNamespace environment) {
			this.dynamicBindingGuru	= dynamicBindingGuru;
			kernel			= dynamicBindingGuru.Kernel;
			this.name		= name;
			this.environment	= environment;

			if (name != null)	nameString = name.PrimitiveValue;
			addToUndeclared = 
				() => 
					{var bindingRef = kernel.newBindingReference(NameString, new DirectBindingHandle((Object)null));
					kernel.UndeclaredNamespace.add(bindingRef);
					return bindingRef;};
		}

		public ESNamespace DefaultEnvironment {
			get { return environment;}
			set {environment = value;}
		}

		public ESSymbol Name {
			get { return name;}
			set {name = value;}
		}

		public String NameString {
			get { return nameString;}
			set {nameString = value;}
		}

		protected void doAllButFinalBinding(Object[] args, System.Action<BindingHandle> bindNSResidentVariable) {

			var bindingRef = Name.bindingInNamespaceIfAbsent(environment, accessPrivilege, ImportTransitivity.Transitive, addToUndeclared);
			if (bindingRef == null) kernel.UndeclaredNamespace.add(bindingRef = kernel.newBindingReference(NameString, new DirectBindingHandle(null)));
			bindNSResidentVariable(bindingRef.Value);

		}

		public abstract class Registry : BinderRegistry {

			protected readonly ESNamespace defaultNamespace;

			protected Registry(DynamicBindingGuru dynamicBindingGuru) : base(dynamicBindingGuru) {
				defaultNamespace = kernel.SmalltalkNamespace;
			}

			public ESNamespace DefaultNamespace {
				get { return defaultNamespace;}
			}

		}

	}

}
