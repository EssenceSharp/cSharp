# Class Behavior

superclass: [Namespace](Namespace);
instanceArchitecture: #Behavior

_Page still under construction; sorry about that._

## Instance Methods
{{
protocol: #'class initialization' method:
[## initialize

        "Initialize the receiver's state"

        ^self
];

protocol: #converting method:
[## asBehavior 
	^self
];

protocol: #'instance creation' method:
[## basicNew

        "Answer a new instance of the receiver"

        <primitive: new domain: Behavior>

        ^self newWithSize: 0
];

protocol: #'instance creation' method:
[## basicNew: size

        "Answer a new instance of the receiver with the specified <size> (number of indexed slots)"

        <primitive: #newWithSize: domain: Behavior>

        ^self newWithValue: size
];

protocol: #'instance creation' method:
[## basicValue: value

        "Answer a new instance of the receiver initialized by or representing the specified <value>"

        <primitive: #newWithValue: domain: Behavior>

        ^self new
];

protocol: #'instance creation' method:
[## new

        "Answer a new instance of the receiver"

        <primitive: new domain: Behavior>

        ^self newWithSize: 0
];

protocol: #'instance creation' method:
[## new: size

        "Answer a new instance of the receiver with the specified <size> (number of indexed slots)"

        <primitive: #newWithSize: domain: Behavior>

        ^self newWithValue: size
];

protocol: #'instance creation' method:
[## value: value

        "Answer a new instance of the receiver initialized by or representing the specified <value>"

        <primitive: #newWithValue: domain: Behavior>

        ^self new
];

protocol: #'system primitives' method:
[## instanceType

        <primitive: instanceType domain: Behavior>
];

protocol: #'system primitives' method:
[## instanceType: instanceTypeObject

        <primitive: #instanceType: domain: Behavior>
];

protocol: #'system primitives' method:
[## superclass

        <primitive: superclass domain: Behavior>
];

protocol: #'system primitives' method:
[## superclass: superclass

        <primitive: #superclass: domain: Behavior>
];

protocol: #'system primitives' method:
[## addSubclass: subclass

        <primitive: #addSubclass: domain: Behavior>
];

protocol: #'system primitives' method:
[## removeSubclass: subclass

        <primitive: #removeSubclass: domain: Behavior>
];

protocol: #'system primitives' method:
[## includesBehavior: aBehavior

        <primitive: #includesBehavior: domain: Behavior>
];

protocol: #'system primitives' method:
[## inheritsFrom: aBehavior

        <primitive: #inheritsFrom: domain: Behavior>
];

protocol: #'system primitives' method:
[## basicCompiledMethodAt: selector

        <primitive: #basicCompiledMethodAt: domain: Behavior>
];

protocol: #'system primitives' method:
[## compiledMethodAt: selector

        <primitive: #compiledMethodAt: domain: Behavior>
];

protocol: #'system primitives' method:
[## addMethod: method

        <primitive: #addMethod: domain: Behavior>
];

protocol: #'system primitives' method:
[## protocol: protocol method: method

        <primitive: #protocol:method: domain: Behavior>
];

protocol: #'system primitives' method:
[## compileMethodInProtocol: protocol fromString: methodText

        <primitive: #compileMethodInProtocol:fromString: domain: Behavior>
];

protocol: #'system primitives' method:
[## removeSelector: selector

        <primitive: #removeSelector: domain: Behavior>
];

protocol: #'system primitives' method:
[## includesSelector: selector

        <primitive: #includesSelector: domain: Behavior>
];

protocol: #'system primitives' method:
[## canUnderstand: selector

        <primitive: #canUnderstand: domain: Behavior>
];

protocol: #'system primitives' method:
[## selectorCount

        <primitive: selectorCount domain: Behavior>
];

protocol: #'system primitives' method:
[## selectors

        <primitive: selectors domain: Behavior>
];

protocol: #'system primitives' method:
[## selectorsDo: enumerator1

        <primitive: #selectorsDo: domain: Behavior>
];

protocol: #'system primitives' method:
[## selectorsAndMethodsDo: enumerator2

        <primitive: #selectorsAndMethodsDo: domain: Behavior>
];

protocol: #'system primitives' method:
[## compiledMethodAtSystemSelector: systemSelector numArgs: numArgs

        <primitive: #compiledMethodAtSystemSelector:numArgs: domain: Behavior>
];

protocol: #'system primitives' method:
[## bindMethodAt: essenceSelector toSystemSelector: systemSelector

        <primitive: #bindMethodAt:toSystemSelector: domain: Behavior>
];

protocol: #'system primitives' method:
[## unbindindMethodFromSystemSelector: systemSelector numArgs: numArgs

        <primitive: #unbindindMethodFromSystemSelector:numArgs: domain: Behavior>
];

protocol: #'system primitives' method:
[## includesSystemSelector: systemSelector numArgs: numArgs

        <primitive: #includesSystemSelector:numArgs: domain: Behavior>
];

protocol: #'system primitives' method:
[## systemSelectorCount

        <primitive: systemSelectorCount domain: Behavior>
];

protocol: #'system primitives' method:
[## systemSelectorsDo: enumerator2

        <primitive: #systemSelectorsDo: domain: Behavior>
];

protocol: #'system primitives' method:
[## systemSelectorsAndMethodsDo: enumerator3

        <primitive: #systemSelectorsAndMethodsDo: domain: Behavior>
];

protocol: #'system primitives' method:
[## instanceArchitecture

        <primitive: instanceArchitecture domain: Behavior>
];

protocol: #'system primitives' method:
[## instanceArchitecture: instanceArchitecture

        <primitive: #instanceArchitecture: domain: Behavior>
];

protocol: #'system primitives' method:
[## instanceVariableNames

        <primitive: instanceVariableNames domain: Behavior>
];

protocol: #'system primitives' method:
[## instanceVariableNames: instanceVariableNames

        <primitive: #instanceVariableNames: domain: Behavior>
];

protocol: #'system primitives' method:
[## basictInstVarNameAt: index

        <primitive: #basictInstVarNameAt: domain: Behavior>
];

protocol: #'system primitives' method:
[## instVarNameAt: index

        <primitive: #instVarNameAt: domain: Behavior>
];

protocol: #'system primitives' method:
[## instVarIndexFor: instanceVariableName

        <primitive: #instVarIndexFor: domain: Behavior>
];

protocol: #'system primitives' method:
[## basicInstSize

        <primitive: basicInstSize domain: Behavior>
];

protocol: #'system primitives' method:
[## instSize

        <primitive: instSize domain: Behavior>
];

protocol: #'system primitives' method:
[## new

        <primitive: new domain: Behavior>
];

protocol: #'system primitives' method:
[## newWithSize: size

        <primitive: #newWithSize: domain: Behavior>
];

protocol: #'system primitives' method:
[## newWithValue: initializingValue

        <primitive: #newWithValue: domain: Behavior>
]}}