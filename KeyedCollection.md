# Class KeyedCollection

superclass: [Collection](Collection);
instanceArchitecture: #Abstract

_Page still under construction; sorry about that._

## Instance Methods
{{
	protocol: #binding method: 
	[## metaValueAt: key ifAbsent: absentBlock
			^(self at: key ifAbsent: absentBlock) value
	];
	
	protocol: #binding method:
	[## metaValueAtIndex: index ifAbsent: absentBlock
			^(self at: index ifAbsent: absentBlock) value
	]}}
