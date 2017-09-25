# Class Namespace

superclass: [KeyedCollection](KeyedCollection);
instanceArchitecture: #Namespace

_Page still under construction; sorry about that._

## Instance Methods
{{
	protocol: #'system primitives' method:
	[## size

		<primitive: size domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## isEmpty

		<primitive: isEmpty domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## associationAt: key 

		<primitive: #associationAt: domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## associationAt: key ifAbsent: absentAction 

		<primitive: #associationAt:ifAbsent: domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## at: key 

		<primitive: #at: domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## at: key ifAbsent: absentAction 

		<primitive: #at:ifAbsent: domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## at: key ifAbsentPut: computeValueToBeAdded 

		<primitive: #at:ifAbsentPut: domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## add: newAssociation 

		<primitive: #add: domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## at: key put: newValue 

		<primitive: #at:put: domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## at: key immutablyPut: newValue 

		<primitive: #at:immutablyPut: domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## includesKey: key 

		<primitive: #includesKey: domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## removeKey: key 

		<primitive: #removeKey: domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## removeKey: key ifAbsent: notFoundAction 

		<primitive: #removeKey:ifAbsent: domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## associationsDo: enumerator1 

		<primitive: #associationsDo: domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## keysDo: enumerator1 

		<primitive: #keysDo: domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## valuesDo: enumerator1 

		<primitive: #valuesDo: domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## keysAndValuesDo: enumerator2 

		<primitive: #keysAndValuesDo: domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## asNamespace

		<primitive: asNamespace domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## name

		<primitive: name domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## name: name 

		<primitive: #name: domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## renameFrom: prevName to: newName 

		<primitive: #renameFrom:to: domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## hostSystemName

		<primitive: hostSystemName domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## hostSystemName: hostSystemName 

		<primitive: #hostSystemName: domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## hostSystemNamespace

		<primitive: hostSystemNamespace domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## hostSystemNamespace: hostSystemNamespace 

		<primitive: #hostSystemNamespace: domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## assemblyPathname

		<primitive: assemblyPathname domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## assemblyPathname: assemblyPathname 

		<primitive: #assemblyPathname: domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## assemblyName

		<primitive: assemblyName domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## assemblyName: assemblyName 

		<primitive: #assemblyName: domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## pathname

		<primitive: pathname domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## qualifiedHostSystemName

		<primitive: qualifiedHostSystemName domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## environment

		<primitive: environment domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## environment: environment 

		<primitive: #environment: domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## declareVariable: key withAccess: accessPrivilegeLevel onCollision: onCollisionAction 

		<primitive: #declareVariable:withAccess:onCollision: domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## declareConstant: key withValue: constantValue access: accessPrivilegeLevel onCollision: onCollisionAction 

		<primitive: #declareConstant:withValue:access:onCollision: domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## defineNamespace: nsName withAccess: accessPrivilegeLevel configure: configureClass 

		<primitive: #defineNamespace:withAccess:configure: domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## defineClass: className withAccess: accessPrivilegeLevel configure: configureClass 

		<primitive: #defineClass:withAccess:configure: domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## initializeSpecificImports

		<primitive: initializeSpecificImports domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## initializeGeneralImports

		<primitive: initializeGeneralImports domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## import: sourceNsSpecification withAccess: accessPrivilegeLevel 

		<primitive: #import:withAccess: domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## importTransitive: sourceNsSpecification withAccess: accessPrivilegeLevel 

		<primitive: #importTransitive:withAccess: domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## import: nameInSource from: sourceNsSpecification as: localName withAccess: accessPrivilegeLevel 

		<primitive: #import:from:as:withAccess: domain: Namespace>
	];

	protocol: #'system primitives' method:
	[## importTransitive: nameInSource from: sourceNsSpecification as: localName withAccess: accessPrivilegeLevel 

		<primitive: #importTransitive:from:as:withAccess: domain: Namespace>
	]}}
