# Class True

superclass: [Boolean](Boolean);
instanceArchitecture: #True

_Page still under construction; sorry about that._

## Instance Methods
{{
	protocol: #'system primitives' method:
	[## class

		<primitive: class domain: True>
	];

	protocol: #'system primitives' method:
	[## isMemberOf: aBehavior 

		<primitive: #isMemberOf: domain: True>
	];

	protocol: #'system primitives' method:
	[## isKindOf: aBehavior 

		<primitive: #isKindOf: domain: True>
	];

	protocol: #'system primitives' method:
	[## = comparand

		<primitive: #= domain: True>
	];

	protocol: #'system primitives' method:
	[## hash

		<primitive: hash domain: True>
	];

	protocol: #'system primitives' method:
	[## not

		<primitive: not domain: True>
	];

	protocol: #'system primitives' method:
	[## & operand

		<primitive: #& domain: True>
	];

	protocol: #'system primitives' method:
	[## and: operand 

		<primitive: #and: domain: True>
	];

	protocol: #'system primitives' method:
	[## | operand

		<primitive: #| domain: True>
	];

	protocol: #'system primitives' method:
	[## or: operand 

		<primitive: #or: domain: True>
	];

	protocol: #'system primitives' method:
	[## xor: operand 

		<primitive: #xor: domain: True>
	];

	protocol: #'system primitives' method:
	[## ifFalse: operand 

		<primitive: #ifFalse: domain: True>
	];

	protocol: #'system primitives' method:
	[## ifTrue: operand 

		<primitive: #ifTrue: domain: True>
	];

	protocol: #'system primitives' method:
	[## ifFalse: falseAction ifTrue: trueAction 

		<primitive: #ifFalse:ifTrue: domain: True>
	];

	protocol: #'system primitives' method:
	[## ifTrue: trueAction ifFalse: falseAction 

		<primitive: #ifTrue:ifFalse: domain: True>
	]}}
