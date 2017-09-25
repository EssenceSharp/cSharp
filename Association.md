# Class Association

superclass: [Object](Object);
instanceArchitecture: #Association

_Page still under construction; sorry about that._

## Instance Methods
{{

	protocol: #'system primitives' method:
	[## isDeletable

		<primitive: isDeletable domain: Association>
	];

	protocol: #'system primitives' method:
	[## isKeyImmutable

		<primitive: isKeyImmutable domain: Association>
	];

	protocol: #'system primitives' method:
	[## keyBeImmutable

		<primitive: keyBeImmutable domain: Association>
	];

	protocol: #'system primitives' method:
	[## key

		<primitive: key domain: Association>
	];

	protocol: #'system primitives' method:
	[## value

		<primitive: value domain: Association>
	];

	protocol: #'system primitives' method:
	[## key: key value: value 

		<primitive: #key:value: domain: Association>
	];

	protocol: #'system primitives' method:
	[## key: key 

		<primitive: #key: domain: Association>
	];

	protocol: #'system primitives' method:
	[## immutableKey: key 

		<primitive: #immutableKey: domain: Association>
	];

	protocol: #'system primitives' method:
	[## value: value 

		<primitive: #value: domain: Association>
	];

	protocol: #'system primitives' method:
	[## immutableValue: value 

		<primitive: #immutableValue: domain: Association>
	];

	protocol: #'system primitives' method:
	[## withKey: newKey 

		<primitive: #withKey: domain: Association>
	];

	protocol: #'system primitives' method:
	[## withValue: newValue 

		<primitive: #withValue: domain: Association>
	];

	protocol: #'system primitives' method:
	[## withKey:value

		<primitive: #withKey:value domain: Association>
	]}}
