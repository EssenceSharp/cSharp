using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using EssenceSharp.UtilityServices;
using EssenceSharp.Runtime;
#if CLR2
using FuncNs = Microsoft.Scripting.Utils;
#else
using FuncNs = System;
#endif

namespace EssenceSharp.Source.Examples {

	class ExamplePrimitives {

		public static long fortyTwo() {
			return 42;
		}

		public String name() {
			return "Craig";
		}

		public class Domain : PrimitiveDomain {

			protected override void bindToObjectSpace() {
				var typeName = new TypeName(typeof(ExamplePrimitives));
				domainClass = objectSpace.classForHostSystemType(typeName);
			}

			public override PrimitiveDomainType Type {
				get {return PrimitiveDomainType.Examples;}
			}

			#region Primitive Definitions

			public static Object _fortyTwo_(Object receiver) {
				return ExamplePrimitives.fortyTwo();
			}

			#endregion

			public override void publishCanonicalPrimitives() {
				publishPrimitive("fortyTwo",					new FuncNs.Func<Object, Object>(_fortyTwo_));
			}

		}

	}

}
