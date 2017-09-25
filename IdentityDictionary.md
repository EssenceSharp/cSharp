# Class IdentityDictionary

superclass: [KeyedCollection](KeyedCollection);
instanceArchitecture: #IdentityDictionary

_Page still under construction; sorry about that._

## Instance Methods

	{{
        protocol: #'system primitives' method:
	[## size

		<primitive: size domain: Dictionary>
	];

	protocol: #'system primitives' method:
	[## isEmpty

		<primitive: isEmpty domain: Dictionary>
	];

	protocol: #'system primitives' method:
	[## associationAt: key 

		<primitive: #associationAt: domain: Dictionary>
	];

	protocol: #'system primitives' method:
	[## associationAt: key ifAbsent: absentAction 

		<primitive: #associationAt:ifAbsent: domain: Dictionary>
	];

	protocol: #'system primitives' method:
	[## at: key 

		<primitive: #at: domain: Dictionary>
	];

	protocol: #'system primitives' method:
	[## at: key ifAbsent: absentAction 

		<primitive: #at:ifAbsent: domain: Dictionary>
	];

	protocol: #'system primitives' method:
	[## at: key ifAbsentPut: computeValueToBeAdded 

		<primitive: #at:ifAbsentPut: domain: Dictionary>
	];

	protocol: #'system primitives' method:
	[## add: newAssociation 

		<primitive: #add: domain: Dictionary>
	];

	protocol: #'system primitives' method:
	[## at: key put: newValue 

		<primitive: #at:put: domain: Dictionary>
	];

	protocol: #'system primitives' method:
	[## at: key immutablyPut: newValue 

		<primitive: #at:immutablyPut: domain: Dictionary>
	];

	protocol: #'system primitives' method:
	[## includesKey: key 

		<primitive: #includesKey: domain: Dictionary>
	];

	protocol: #'system primitives' method:
	[## removeKey: key 

		<primitive: #removeKey: domain: Dictionary>
	];

	protocol: #'system primitives' method:
	[## removeKey: key ifAbsent: notFoundAction 

		<primitive: #removeKey:ifAbsent: domain: Dictionary>
	];

	protocol: #'system primitives' method:
	[## associationsDo: enumerator1 

		<primitive: #associationsDo: domain: Dictionary>
	];

	protocol: #'system primitives' method:
	[## keysDo: enumerator1 

		<primitive: #keysDo: domain: Dictionary>
	];

	protocol: #'system primitives' method:
	[## valuesDo: enumerator1 

		<primitive: #valuesDo: domain: Dictionary>
	];

	protocol: #'system primitives' method:
	[## keysAndValuesDo: enumerator2 

		<primitive: #keysAndValuesDo: domain: Dictionary>
	]}}
