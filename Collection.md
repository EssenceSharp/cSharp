!Class Collection

superclass: [Object](Object);
instanceArchitecture: #Abstract

_Page still under construction; sorry about that._

## Instance Methods
{{
protocol: #binding method:
[## metaValueAt: key ifAbsent: absentBlock
        self do: [:each ](-key-=-each-key-ifTrue_-[^each-value-value)].
        ^absentBlock value
];

protocol: #binding method:
[## metaValueAtIndex: index ifAbsent: absentBlock
        index > self size ifTrue: [^absentBlock value](^absentBlock-value).
        index < 1 ifTrue: [^absentBlock value](^absentBlock-value).
        ^(self at: index) value
]}}