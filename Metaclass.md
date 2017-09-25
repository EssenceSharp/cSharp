# Class Metaclass

superclass: [Behavior](Behavior);
instanceArchitecture: #Metaclass

_Page still under construction; sorry about that._

## Instance Methods
{{
	protocol: #'Inter-Smalltalk compatibility' method: 
	[## soleInstance
		"Answer the canonical instance of the receiver"
		
		<primitive: canonicalInstance domain: Metaclass>
	];

	protocol: #'system primitives' method:
	[## canonicalInstance

		<primitive: canonicalInstance domain: Metaclass>
	]}}